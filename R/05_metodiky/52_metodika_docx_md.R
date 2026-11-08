#----------------------------------------------------------#
# Prevod metodiky obojzivelniku z .docx do Markdownu -----
#----------------------------------------------------------#
# Vstup:  Metodiky/Obojzivelnici/met_ssEVL_SLOUCENE_teren_a_vyhodnocovani_obojziv_2024_zmeny.docx
# Vystup: Metodiky/Obojzivelnici/met_obojzivelnici_2026.md
#
# PROC NESTACI SAMO PANDOC:
# Puvodni .docx nema nadpisy oznacene stylem Nadpis 1/2/3. Kapitoly jsou
# rucne formatovane polozky cislovanych seznamu (tucne, vetsi pismo) a cast
# odstavcu ma navic styl "Nadpis_2", i kdyz jde o bezny text. Pandoc proto
# vyrobi dokument bez jedineho nadpisu - plochou radu odstavcu.
#
# Skript proto dela dva kroky:
#   1. z XML dokumentu odvodi skutecnou strukturu nadpisu (podle cislovani
#      seznamu a formatovani odstavce),
#   2. text prevede pandokem a odvozene nadpisy do vysledneho Markdownu
#      doplni.
#
# Nadpis se rozpoznava podle techto pravidel (v tomto poradi):
#   - text zacinajici "Priloha <cislo>"        -> uroven 1
#   - odstavec se stylem Nadpis_1 (Nadpis10)   -> uroven 1
#   - polozka cislovaneho seznamu numId 3/14   -> uroven ilvl + 1
#   - kratky odstavec, kde jsou VSECHNY useky
#     tucne (napr. "Stav populace")            -> uroven nadrazeneho + 1
# Styl "Nadpis_2" (Nadpis20) se zamerne NEPOUZIVA - v dokumentu ho nesou
# i bezne odstavce, takze jako priznak nadpisu je nepouzitelny.
#
# CISLOVANI:
# Cislovani v .docx je nekonzistentni (kapitoly jsou ze dvou ruznych
# cislovanych seznamu, takze Word u prvni kapitoly zobrazi jine cislo nez
# u druhe). Skript proto cislovani prepocitava znovu podle urovne nadpisu -
# urovne 1 a 2 dostanou cislo (1, 1.1, 1.2, 2, 2.1), hlubsi urovne zustavaji
# bez cisla, stejne jako v puvodnim dokumentu.
#
# CO SE DO MARKDOWNU DOPLNUJE NAVIC:
#   - hlavicka YAML (titul, autori, verze) z uvodnich odstavcu dokumentu,
#   - obrazky rozhodovacich stromu se odkazuji na diagramy generovane
#     skriptem 51_metodika_diagramy.R (misto rastru vlozenych do .docx),
#   - neuzavrene komentare z .docx se vypisuji na konec dokumentu, aby se
#     pri prevodu nezahodily.
#
# ZAVISLOSTI: R (xml2), pandoc (samostatny, nebo ten z instalace Quarta)
#----------------------------------------------------------#

#----------------------------------------------------------#
# Nacteni knihoven -----
#----------------------------------------------------------#
packages <- c("xml2")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

#----------------------------------------------------------#
# Nastaveni prevodu -----
#----------------------------------------------------------#

# Nadpisy, ktere se necisluji (uvod a prilohy)
MD_NECISLOVANE <- "^(Cíl sledování|Příloha\\s+\\d+)"

# Do ktere urovne se nadpisy cisluji
MD_CISLOVAT_DO <- 2

# Prirazeni obrazku v .docx k predloham v Diagrams/.
# Pridruzeni je dohledatelne: prvni obrazek ma v .docx alt text
# "...\strom_obojzivelnici_lok_long.png" a pomer stran oboru obrazku
# odpovida rozmerum prislusnych predloh.
MD_DIAGRAMY <- list(
  list(
    media   = "image1",
    soubor  = "diagramy/strom_obojzivelnici_lok_long.png",
    popisek = "Rozhodovací strom pro hodnocení stavu druhu na úrovni dílčí plochy"
  ),
  list(
    media   = "image2",
    soubor  = "diagramy/strom_obojzivelnici_long.png",
    popisek = "Rozhodovací strom pro hodnocení stavu druhu na úrovni sledovaného území"
  )
)

#----------------------------------------------------------#
# Pomocne funkce -----
#----------------------------------------------------------#

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

# Pandoc: nejdriv samostatna instalace, jinak ten, ktery s sebou nese Quarto.
# Vraci funkci, ktera prijme argumenty pandoku a spusti ho.
md_pandoc <- function() {
  exe <- Sys.which("pandoc")
  if (nzchar(exe)) {
    return(list(
      popis = paste0("pandoc (", exe, ")"),
      run = function(args) system2(exe, args)
    ))
  }

  quarto <- Sys.which("quarto")
  if (nzchar(quarto)) {
    return(list(
      popis = paste0("pandoc z Quarta (", quarto, ")"),
      run = function(args) system2(quarto, c("pandoc", args))
    ))
  }

  if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
    return(list(
      popis = "pandoc z balicku rmarkdown",
      run = function(args) system2(rmarkdown::pandoc_exec(), args)
    ))
  }

  stop(
    "Pandoc nenalezen. Nainstalujte pandoc (https://pandoc.org/installing.html), ",
    "nebo Quarto (https://quarto.org) - skript pouzije pandoc z nej.",
    call. = FALSE
  )
}

# Normalizace textu pro porovnavani odstavce v .docx s radkem v Markdownu.
# Odstranuje znacky Markdownu, HTML tagy, odrazky cislovanych seznamu
# a sjednocuje mezery (vcetne nezlomitelnych).
md_norm <- function(x) {
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("^[[:space:]]*([0-9]+[.)]|[-*+])[[:space:]]+", "", x)
  x <- gsub("[*_`\\\\]", "", x)
  x <- gsub(" ", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

#----------------------------------------------------------#
# Cteni struktury z .docx -----
#----------------------------------------------------------#

# Rozbaleni casti .docx do docasne slozky (bez externich nastroju)
md_docx_part <- function(docx, part, exdir) {
  utils::unzip(docx, files = part, exdir = exdir, overwrite = TRUE)
  p <- file.path(exdir, part)
  if (!file.exists(p)) stop("V .docx chybi cast '", part, "': ", docx, call. = FALSE)
  xml2::read_xml(p)
}

# Odvozeni nadpisu z formatovani odstavcu.
# Vraci data.frame v poradi dokumentu: text nadpisu a jeho uroven.
md_docx_headings <- function(doc) {
  ns <- xml2::xml_ns(doc)
  ps <- xml2::xml_find_all(doc, "/w:document/w:body/w:p", ns)

  val <- function(node, path) {
    n <- xml2::xml_find_first(node, path, ns)
    if (inherits(n, "xml_missing")) NA_character_ else xml2::xml_attr(n, "w:val", ns)
  }

  info <- lapply(ps, function(p) {
    runs <- xml2::xml_find_all(p, "./w:r", ns)
    n_bold <- sum(vapply(runs, function(r) {
      !inherits(xml2::xml_find_first(r, "./w:rPr/w:b", ns), "xml_missing")
    }, logical(1)))

    list(
      text  = trimws(paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns)), collapse = "")),
      style = val(p, "./w:pPr/w:pStyle"),
      numid = val(p, "./w:pPr/w:numPr/w:numId"),
      ilvl  = val(p, "./w:pPr/w:numPr/w:ilvl"),
      n_run = length(runs),
      n_bold = n_bold
    )
  })

  # Uvodni odstavce (titul, autori, afiliace) nadpisy nejsou - struktura
  # zacina prvnim odstavcem se stylem Nadpis_1 nebo s cislovanim kapitol.
  je_strukturni <- vapply(info, function(x) {
    identical(x$style, "Nadpis10") || (!is.na(x$numid) && x$numid %in% c("3", "14"))
  }, logical(1))
  if (!any(je_strukturni)) {
    stop("V dokumentu nebyl nalezen zadny nadpis - zkontrolujte vstupni .docx.", call. = FALSE)
  }
  zacatek <- min(which(je_strukturni))

  out <- list()
  uroven_nadrazeneho <- 1L

  for (i in seq_along(info)) {
    x <- info[[i]]
    if (i < zacatek || !nzchar(x$text)) next

    uroven <- NA_integer_

    if (grepl("^Příloha\\s+\\d+", x$text)) {
      uroven <- 1L

    } else if (identical(x$style, "Nadpis10") && nchar(x$text) < 120) {
      uroven <- 1L

    } else if (!is.na(x$numid) && x$numid %in% c("3", "14")) {
      uroven <- as.integer(x$ilvl %||% "0") + 1L

    } else if (x$n_run > 0 && x$n_bold == x$n_run && nchar(x$text) < 120) {
      # Odstavec cely tucne = mezinadpis bez cislovani ("Stav populace")
      uroven <- min(uroven_nadrazeneho + 1L, 4L)
    }

    if (is.na(uroven)) next

    # Uroven nadrazeneho nadpisu se posouva jen podle cislovanych nadpisu,
    # aby dva tucne mezinadpisy za sebou neskakaly do stale vetsi hloubky
    if (!is.na(x$numid) || identical(x$style, "Nadpis10")) uroven_nadrazeneho <- uroven

    out[[length(out) + 1]] <- data.frame(
      text = x$text, uroven = uroven, stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}

# Uvodni odstavce dokumentu - pouziji se pro hlavicku YAML
md_docx_front <- function(doc) {
  ns <- xml2::xml_ns(doc)
  ps <- xml2::xml_find_all(doc, "/w:document/w:body/w:p", ns)

  txt <- vapply(ps, function(p) {
    trimws(paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns)), collapse = ""))
  }, "")

  styl <- vapply(ps, function(p) {
    n <- xml2::xml_find_first(p, "./w:pPr/w:pStyle", ns)
    if (inherits(n, "xml_missing")) "" else xml2::xml_attr(n, "w:val", ns)
  }, "")

  konec <- min(which(styl == "Nadpis10")) - 1L
  txt[seq_len(konec)][nzchar(txt[seq_len(konec)])]
}

# Neuzavrene komentare z .docx vcetne odstavce, ke kteremu patri
md_docx_comments <- function(docx, exdir) {
  if (!"word/comments.xml" %in% utils::unzip(docx, list = TRUE)$Name) {
    return(NULL)
  }

  com <- md_docx_part(docx, "word/comments.xml", exdir)
  doc <- md_docx_part(docx, "word/document.xml", exdir)
  ns <- xml2::xml_ns(com)
  nsd <- xml2::xml_ns(doc)

  nodes <- xml2::xml_find_all(com, "//w:comment", ns)
  if (length(nodes) == 0) return(NULL)

  out <- lapply(nodes, function(n) {
    id <- xml2::xml_attr(n, "w:id", ns)

    # Odstavec, ve kterem komentar zacina - dava komentari kontext
    start <- xml2::xml_find_first(
      doc, paste0("//w:commentRangeStart[@w:id='", id, "']"), nsd
    )
    kontext <- if (inherits(start, "xml_missing")) NA_character_ else {
      p <- xml2::xml_find_first(start, "./ancestor::w:p", nsd)
      if (inherits(p, "xml_missing")) NA_character_ else {
        trimws(paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", nsd)), collapse = ""))
      }
    }

    data.frame(
      autor = xml2::xml_attr(n, "w:author", ns),
      datum = substr(xml2::xml_attr(n, "w:date", ns) %||% "", 1, 10),
      text = trimws(gsub("[[:space:]]+", " ", xml2::xml_text(n))),
      kontext = kontext,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

#----------------------------------------------------------#
# Uprava Markdownu -----
#----------------------------------------------------------#

# Nahrazeni radku odpovidajicich nadpisum za nadpisy Markdownu.
# Prochazi se dopredu a nadpisy se "odebiraji" v poradi dokumentu - nektere
# nadpisy se v dokumentu opakuji (nazvy druhu jsou jednou ve vyberu ploch
# a znovu v terenni metodice), takze hledat je podle textu bez ohledu na
# poradi by je zamenilo.
md_apply_headings <- function(lines, headings) {
  cil <- md_norm(headings$text)
  i_head <- 1L
  citace <- rep(FALSE, length(lines))

  # Radky v blocich kodu se neupravuji
  v_kodu <- FALSE
  for (i in seq_along(lines)) {
    if (grepl("^\\s*```", lines[i])) v_kodu <- !v_kodu
    citace[i] <- v_kodu
  }

  cisla <- integer(MD_CISLOVAT_DO)
  nenalezene <- integer(0)

  for (i in seq_along(lines)) {
    if (i_head > nrow(headings)) break
    if (citace[i] || !nzchar(trimws(lines[i]))) next

    if (!identical(md_norm(lines[i]), cil[i_head])) next

    uroven <- headings$uroven[i_head]
    text <- md_norm(lines[i])

    # Cislovani urovni 1 a 2; uvod a prilohy zustavaji bez cisla
    predpona <- ""
    if (uroven <= MD_CISLOVAT_DO && !grepl(MD_NECISLOVANE, text)) {
      cisla[uroven] <- cisla[uroven] + 1L
      if (uroven < MD_CISLOVAT_DO) cisla[(uroven + 1L):MD_CISLOVAT_DO] <- 0L
      predpona <- paste0(paste(cisla[seq_len(uroven)], collapse = "."), " ")
    }

    lines[i] <- paste0(strrep("#", uroven + 1L), " ", predpona, text)
    i_head <- i_head + 1L
  }

  # Nenalezene nadpisy jsou priznak, ze se vstupni .docx zmenil natolik,
  # ze uz mu odvozovaci pravidla neodpovidaji - je lepsi to ohlasit,
  # nez tise vyrobit dokument s chybejici strukturou.
  if (i_head <= nrow(headings)) {
    nenalezene <- seq(i_head, nrow(headings))
    warning(
      "V prevedenem Markdownu se nepodarilo najit ", length(nenalezene),
      " nadpis(u):\n  - ",
      paste(headings$text[nenalezene], collapse = "\n  - "),
      call. = FALSE
    )
  }

  attr(lines, "nenalezene") <- headings$text[nenalezene]
  lines
}

# Nahrazeni odkazu na rastry vlozene v .docx odkazy na generovane diagramy
md_replace_images <- function(lines, diagramy = MD_DIAGRAMY) {
  for (d in diagramy) {
    idx <- grep(paste0("media/", d$media, "\\."), lines)

    if (length(idx) == 0) {
      warning(
        "Obrazek '", d$media, "' nebyl v prevedenem dokumentu nalezen - ",
        "zkontrolujte prirazeni v MD_DIAGRAMY.",
        call. = FALSE
      )
      next
    }
    if (length(idx) > 1) {
      warning("Obrazek '", d$media, "' je v dokumentu ", length(idx),
              "x; nahrazuje se prvni vyskyt.", call. = FALSE)
      idx <- idx[1]
    }

    lines[idx] <- sprintf("![%s](%s)", d$popisek, d$soubor)
  }
  lines
}

# Hlavicka YAML z uvodnich odstavcu .docx.
# Autori se rozdeluji podle carky; horni indexy odkazuji na afiliace,
# ktere zustavaji prvnim odstavcem textu.
md_front_matter <- function(front, zdroj) {
  titul <- front[1]
  podtitul <- if (length(front) > 1) front[2] else ""
  verze <- if (length(front) > 2) front[3] else ""
  autori <- if (length(front) > 3) front[4] else ""

  autori <- trimws(strsplit(autori, ",")[[1]])
  autori <- autori[nzchar(autori)]
  # "Jiri Vojar1" -> "Jiri Vojar<sup>1</sup>" (horni index projde v Markdownu
  # na GitHubu i v sazbe pres pandoc)
  autori <- sub("([0-9]+)$", "<sup>\\1</sup>", autori)

  yml <- c(
    "---",
    sprintf('title: "%s"', titul),
    if (nzchar(podtitul)) sprintf('subtitle: "%s"', podtitul),
    if (nzchar(verze)) sprintf('date: "%s"', verze),
    if (length(autori) > 0) "author:",
    if (length(autori) > 0) sprintf('  - "%s"', autori),
    "lang: cs",
    "---",
    "",
    paste0(
      "<!-- Tento soubor je GENEROVANY z ", basename(zdroj),
      " skriptem R/05_metodiky/52_metodika_docx_md.R. -->"
    ),
    "<!-- Rucni zmeny se pri dalsim prevodu prepisi - upravujte zdrojovy .docx,",
    "     nebo prevod zastavte a dal udrzujte jen tento Markdown. -->",
    ""
  )

  yml[!vapply(yml, is.null, logical(1))]
}

# Vypis neuzavrenych komentaru z .docx na konec dokumentu
md_comments_section <- function(comments) {
  if (is.null(comments) || nrow(comments) == 0) return(character(0))

  out <- c(
    "",
    "## Otevřené komentáře z původního dokumentu",
    "",
    paste0(
      "Komentáře, které byly v době převodu nevyřešené v souboru .docx. ",
      "Jsou zde uvedeny, aby se převodem neztratily."
    ),
    ""
  )

  for (i in seq_len(nrow(comments))) {
    r <- comments[i, ]
    out <- c(
      out,
      sprintf("- **%s**%s: %s", r$autor,
              if (nzchar(r$datum)) paste0(" (", r$datum, ")") else "", r$text),
      if (!is.na(r$kontext) && nzchar(r$kontext)) {
        sprintf("  <br>*k textu:* „%s“", substr(r$kontext, 1, 200))
      }
    )
  }

  c(out, "")
}

#----------------------------------------------------------#
# Prevod -----
#----------------------------------------------------------#

metodika_docx_md <- function(
    docx = file.path(
      "Metodiky", "Obojzivelnici",
      "met_ssEVL_SLOUCENE_teren_a_vyhodnocovani_obojziv_2024_zmeny.docx"
    ),
    md = file.path("Metodiky", "Obojzivelnici", "met_obojzivelnici_2026.md")
) {
  if (!file.exists(docx)) stop("Vstupni .docx nenalezen: ", docx, call. = FALSE)

  pandoc <- md_pandoc()
  message("Prevod metodiky: ", basename(docx))
  message("  pandoc: ", pandoc$popis)

  exdir <- file.path(tempdir(), paste0("metodika_docx_", Sys.getpid()))
  dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)

  #--------------------------------------------------#
  ## 1. Struktura ze zdrojoveho .docx ----
  #--------------------------------------------------#
  doc <- md_docx_part(docx, "word/document.xml", exdir)
  headings <- md_docx_headings(doc)
  front <- md_docx_front(doc)
  comments <- md_docx_comments(docx, exdir)

  message("  nalezeno nadpisu: ", nrow(headings),
          " (urovne: ", paste(sort(unique(headings$uroven)), collapse = ", "), ")")

  #--------------------------------------------------#
  ## 2. Prevod textu pandokem ----
  #--------------------------------------------------#
  # gfm (GitHub Flavored Markdown) proto, aby byl vysledek citelny i na
  # GitHubu; podtrzeni prochazi jako <u>, tabulky jako roury.
  # --wrap=none drzi jeden odstavec na jednom radku, takze rozdily ve verzich
  # jsou citelne po odstavcich.
  raw_md <- file.path(exdir, "raw.md")
  status <- pandoc$run(c(
    shQuote(normalizePath(docx, winslash = "/")),
    "--from=docx",
    "--to=gfm",
    "--wrap=none",
    "--track-changes=accept",
    "--extract-media", shQuote(file.path(exdir, "media")),
    "--output", shQuote(raw_md)
  ))
  if (!identical(status, 0L) || !file.exists(raw_md)) {
    stop("Pandoc prevod neprosel (status ", status, ").", call. = FALSE)
  }

  lines <- readLines(raw_md, encoding = "UTF-8", warn = FALSE)

  #--------------------------------------------------#
  ## 3. Doplneni struktury a obrazku ----
  #--------------------------------------------------#
  # Uvodni odstavce (titul, podtitul, verze, autori) se z textu odeberou -
  # prenaseji se do hlavicky YAML
  front_head <- front[seq_len(min(4, length(front)))]
  for (f in front_head) {
    idx <- which(md_norm(lines) == md_norm(f))
    if (length(idx) > 0) lines[idx[1]] <- NA_character_
  }
  lines <- lines[!is.na(lines)]

  lines <- md_apply_headings(lines, headings)
  nenalezene <- attr(lines, "nenalezene")
  lines <- md_replace_images(lines)

  #--------------------------------------------------#
  ## 4. Slozeni vysledku ----
  #--------------------------------------------------#
  out <- c(
    md_front_matter(front, docx),
    lines,
    md_comments_section(comments)
  )

  # Vice prazdnych radku za sebou se slucuje do jednoho
  prazdny <- !nzchar(trimws(out))
  out <- out[!(prazdny & c(FALSE, prazdny[-length(prazdny)]))]

  dir.create(dirname(md), showWarnings = FALSE, recursive = TRUE)
  con <- file(md, open = "wb")
  writeLines(enc2utf8(out), con, useBytes = TRUE)
  close(con)

  message("  -> ", md, " (", length(out), " radku)")
  invisible(list(md = md, headings = headings, nenalezene = nenalezene))
}

if (sys.nframe() == 0) metodika_docx_md()
