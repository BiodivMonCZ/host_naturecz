# -------------------------
# Pomocná funkce: získá cestu k aktuálnímu souboru (pokud není explicitně zadán)
# -------------------------
detect_current_script <- function() {
  # 1) rstudio editor (pokud běží v RStudio)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (!is.null(p) && nzchar(p)) return(p)
  }
  # 2) commandArgs (při spuštění přes Rscript --file=...)
  args <- commandArgs(trailingOnly = FALSE)
  file_args <- grep("^--file=", args, value = TRUE)
  if (length(file_args)) {
    return(sub("^--file=", "", file_args[1]))
  }
  # Nezjistili jsme skript
  return(NULL)
}

# -------------------------
# Hlavní parser: projde R soubor a vybere všechny pojmenované přiřazení
# v rámci volání mutate/reframe/summarise/transmute - tj. to co typicky
# vytváří indikátory: ID = <výraz>
# -------------------------
extract_mutate_like_defs <- function(path) {
  if (!file.exists(path)) stop("Soubor nenalezen: ", path)
  exprs <- parse(file = path)
  defs <- list()
  
  walk <- function(x) {
    # rekurzivní průchod AST
    if (is.call(x)) {
      # zjisti jméno funkce (string)
      fn <- paste(deparse(x[[1]]), collapse = " ")
      # hledáme volání mutate/reframe/summarise/transmute (i namespaced: dplyr::mutate)
      if (grepl("\\b(mutate|reframe|summarise|transmute)\\b", fn)) {
        # argumenty volání (prvek 1 je funkce)
        arg_names <- names(x)
        # projít pojmenované argumenty - to jsou přiřazení typu NOVY = expr
        for (i in seq_along(x)[-1]) {
          nm <- arg_names[i]
          if (is.null(nm) || nm == "") next
          # vyjmout RHS a převést na jeden string
          rhs <- x[[i]]
          rhs_str <- paste(deparse(rhs), collapse = " ")
          rhs_str <- gsub("\\s+", " ", rhs_str)
          rhs_str <- trimws(rhs_str)
          # odstraníme zpětné uvozovky kolem názvů (pokud tam jsou)
          nm_clean <- gsub("^`|`$", "", nm)
          # poslední definice přepíše předchozí (což odpovídá pořadí v souboru)
          defs[[nm_clean]] <<- rhs_str
        }
      }
      # rekurzivně pro všechny podprvky volání
      for (y in as.list(x)) walk(y)
    } else if (is.expression(x) || is.list(x)) {
      for (y in x) walk(y)
    }
  }
  
  walk(exprs)
  
  if (length(defs) == 0) {
    return(tibble(ID_IND = character(0), KOMPILACE = character(0)))
  } else {
    df <- tibble(ID_IND = names(defs), KOMPILACE = unname(unlist(defs)))
    # zachovat pořadí tak, že poslední definice ve skriptu přepisují
    df <- df %>% distinct(ID_IND, .keep_all = TRUE)
    return(df)
  }
}

# -------------------------
# Sestavení finální mapovací tabulky
# - n2k_druhy: objekt v paměti (měl by už existovat)
# - limity: tabulka obsahující sloupce DRUH, ID_IND, UROVEN, LIM_IND, TYP_IND ...
# - script: cesta ke skriptu (pokud NULL, zkusíme autodetekci)
# -------------------------
build_indicator_map <- function(n2k_druhy, limity, script = NULL, write_csv = FALSE, out_file = "indicator_map.csv") {
  if (is.null(script)) script <- detect_current_script()
  if (is.null(script)) stop("Nezadán 'script' a nebylo možné automaticky zjistit cestu k souboru. Předat parameter 'script'.")
  if (!file.exists(script)) stop("Soubor nenalezen: ", script)
  
  # 1) parsuj definice z .R souboru
  defs <- extract_mutate_like_defs(script)
  
  # 2) zjisti, které sloupce v n2k_druhy považovat za indikátory
  # preferujeme proměnnou ncol_orig pokud existuje ve workspace; fallback na průnik názvů
  indicator_cols <- NULL
  if (exists("ncol_orig", envir = parent.frame())) {
    ncol_orig_val <- get("ncol_orig", envir = parent.frame())
    if (is.numeric(ncol_orig_val) && ncol_orig_val >= 1 && ncol_orig_val <= ncol(n2k_druhy)) {
      indicator_cols <- names(n2k_druhy)[ncol_orig_val:ncol(n2k_druhy)]
    }
  }
  if (is.null(indicator_cols)) {
    # fallback: vezmeme průnik s vyextrahovanými definicemi; pokud žádný, vezmeme všechny defs$ID_IND
    intr <- intersect(names(n2k_druhy), defs$ID_IND)
    if (length(intr) > 0) {
      indicator_cols <- intr
    } else {
      indicator_cols <- defs$ID_IND
    }
  }
  
  # 3) vyber z limity pouze relevantní kombinace (úroveň "lok", nelimitované NA, a jen druhy z n2k_druhy)
  druhu_in_data <- unique(n2k_druhy$DRUH)
  lim_relevant <- limity %>%
    filter(UROVEN == "lok", !is.na(LIM_IND), DRUH %in% druhu_in_data) %>%
    # jen indikátory, které máme ve zdrojových definicích / v n2k_druhy
    filter(ID_IND %in% indicator_cols)
  
  # 4) spojit: limity (obsahují DRUH x ID_IND) + defs (obsahují ID_IND -> KOMPILACE)
  map <- lim_relevant %>%
    left_join(defs, by = "ID_IND") %>%
    # pořadí a čistota
    distinct(DRUH, ID_IND, .keep_all = TRUE) %>%
    select(DRUH, ID_IND, everything())
  
  # 5) upozornění na chybějící definice (pokud nějaký ID_IND v lim_relevant nemá přiřazenou kompilaci)
  missing_defs <- map %>% filter(is.na(KOMPILACE) | KOMPILACE == "") %>% pull(ID_IND) %>% unique()
  if (length(missing_defs) > 0) {
    warning("Následující ID_IND nemají vyextrahovanou KOMPILACI (pravděpodobně dynamicky generované v kódu nebo nejsou v mutate/reframe):\n",
            paste0(missing_defs, collapse = ", "))
  }
  
  # 6) volitelný zápis
  if (write_csv) {
    readr::write_csv(map, out_file)
    message("Mapovací tabulka zapsána do: ", out_file)
  }
  
  return(map)
}
build_indicator_map()
