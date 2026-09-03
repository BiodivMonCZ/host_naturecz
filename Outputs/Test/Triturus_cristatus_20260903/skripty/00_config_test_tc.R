#----------------------------------------------------------#
# Orezany config pro testovaci beh - Triturus cristatus -----
#----------------------------------------------------------#
# Prebira R/00_config/00_n2k_config.R VYRAZ PO VYRAZU, doslovne. Nic
# neprepisuje ani neprepocitava - jen vynechava bloky, ktere na teto
# pracovni stanici nelze spustit, a omezuje vstupni data na jediny druh.
#
# Vynechane bloky a duvod:
#   vodstvo                  - WFS CUZK (sit, dlouhy beh); druhova cast
#                              pipeline (R/02_druhy) ho nepouziva
#   biotop_zvld / BiotopZvld - chybi Data/Input/BiotopZvld.shp, fallback by
#                              stahoval z WFS; v R/02_druhy se nepouziva
#   n2k_union                - drahy prostorovy join evl x po; nepouziva se
#   akt_okrsky               - chybi Data/Input/AktualizacniOkrsky.shp
#                              (viz harmonizace_registr.md, "Co zbyva" c. 6)
#   redlist / invaze /       - chybi export_redlist.csv, export_invaze.csv
#   expanze                    a export_expanze.csv v host_data
#   rn2kcz z GitHubu         - balicek se v R/02_druhy nikde nepouziva
#
# Omezeni na jeden druh: aplikuje se az POTE, co je znam `ncol_orig`
# (pocet sloupcu suroveho exportu), tedy pred blokem `n2k_load`. Vsechny
# transformace v tomto bloku jsou radkove (mutate / distinct / rename),
# predfiltrovani je proto vuci plnemu behu ekvivalentni - stejny postup
# a stejne oduvodneni jako u testovaciho behu 2026-08-25 zaznamenaneho
# v Metodiky/Obojzivelnici/harmonizace_registr.md.
#----------------------------------------------------------#

TEST_SPECIES <- "Triturus cristatus"

# Config je rozdeleny na obecnou cast a data druhu - n2k_load, n2k_export
# i ncol_orig vznikaji az v druhem souboru. Testovaci beh proto musi projit
# oba, ve stejnem poradi jako R/02_druhy/20_n2k_druhy_run.R.
cfg_paths <- c(
  "R/00_config/00_n2k_config.R",
  "R/00_config/02_n2k_data_druhy.R"
)

# Bloky, ktere se maji preskocit - hleda se doslovny vyskyt tokenu
# v deparsovanem textu vyrazu nejvyssi urovne.
skip_tokens <- c(
  "vodstvo",
  "BiotopZvld",
  "biotop_zvld",
  "n2k_union",
  "akt_okrsky",
  "AktualizacniOkrsky",
  "export_redlist",
  "export_invaze",
  "export_expanze",
  "redlist_species",
  "invasive_species",
  "expansive_species",
  "rn2kcz"
)

cfg_exprs <- unlist(
  lapply(cfg_paths, function(p) as.list(parse(p, encoding = "UTF-8"))),
  recursive = FALSE
)

message("=== ORIZNUTY CONFIG: ", length(cfg_exprs), " vyrazu nejvyssi urovne z ",
        length(cfg_paths), " souboru ===")

cfg_skipped <- character(0)
cfg_filtered <- FALSE

for (i in seq_along(cfg_exprs)) {

  e <- cfg_exprs[[i]]
  txt <- paste(deparse(e), collapse = "\n")

  hit <- vapply(skip_tokens, grepl, logical(1), x = txt, fixed = TRUE)

  if (any(hit)) {
    lbl <- sub("\n.*$", "", txt)
    cfg_skipped <- c(cfg_skipped, lbl)
    message("  [skip ", i, "] ", substr(lbl, 1, 70),
            "   <- ", paste(skip_tokens[hit], collapse = ", "))
    next
  }

  eval(e, envir = globalenv())

  # Jakmile je znam pocet sloupcu suroveho exportu, omez surova data na
  # jediny hodnoceny druh (viz komentar v hlavicce).
  if (!cfg_filtered &&
      exists("ncol_orig", envir = globalenv()) &&
      exists("n2k_export", envir = globalenv())) {

    n_all <- nrow(get("n2k_export", envir = globalenv()))

    assign(
      "n2k_export",
      dplyr::filter(get("n2k_export", envir = globalenv()), DRUH == TEST_SPECIES),
      envir = globalenv()
    )
    assign(
      "volna_export",
      dplyr::filter(get("volna_export", envir = globalenv()), DRUH == TEST_SPECIES),
      envir = globalenv()
    )

    cfg_filtered <- TRUE

    message(
      "  [filtr] ", TEST_SPECIES, ": export_data_evl ", n_all, " -> ",
      nrow(get("n2k_export", envir = globalenv())), " radku; ",
      "ncol_orig = ", get("ncol_orig", envir = globalenv())
    )

    gc(verbose = FALSE)
  }
}

# Pojistka: kdyby se filtr z jakehokoli duvodu neuplatnil vyse.
n2k_load <- dplyr::filter(n2k_load, DRUH == TEST_SPECIES) %>%
  dplyr::mutate(DRUH = droplevels(as.factor(DRUH)))

message("=== CONFIG HOTOV: n2k_load = ", nrow(n2k_load), " zaznamu, ",
        ncol(n2k_load), " sloupcu; preskoceno ", length(cfg_skipped), " vyrazu ===")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
