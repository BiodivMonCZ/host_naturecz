#----------------------------------------------------------#
# Sazba metodiky z Markdownu do PDF s cislovanymi radky -----
#----------------------------------------------------------#
# Vstup:  Metodiky/Obojzivelnici/met_obojzivelnici_2026.md
# Vystup: Metodiky/Obojzivelnici/met_obojzivelnici_2026.pdf
#
# K CEMU TO JE:
# PDF je urcene k pripominkovani. Kazdy radek textu ma v levem okraji cislo,
# takze pripominka muze byt adresna ("str. 6, r. 214") a nezavisi na tom, jak
# kdo dokument posouva nebo zoomuje. Cislovani je pruubezne celym dokumentem.
#
# CELY POSTUP (reprodukovatelny od predloh po PDF):
#   Diagrams/*.drawio                          zdroj diagramu (verzovane XML)
#     -> 51_metodika_diagramy.R                TikZ -> PDF + PNG
#   met_...2024_zmeny.docx                     zdroj textu
#     -> 52_metodika_docx_md.R                 -> met_obojzivelnici_2026.md
#     -> 53_metodika_md_pdf.R (tento skript)   -> met_obojzivelnici_2026.pdf
# Vse najednou spusti R/05_metodiky/50_metodika_run.R.
#
# CO SE POUZIVA PRI SAZBE:
#   sazba/hlavicka.tex     - doplnky preambule (lineno, ulem, float)
#   sazba/html_inline.lua  - prevod <u>, <sup>, <br> z Markdownu na LaTeX
# Oba soubory jsou verzovane a urcene k rucnim upravam - meni se v nich
# vzhled recenzniho PDF.
#
# PROC PANDOC A NE rmarkdown::render:
# Metodika je cisty Markdown bez kodu v R, takze knitr nema co vyhodnocovat
# a mezikrok by jen pridal zavislost. Kdyby v budoucnu bylo potreba generovat
# tabulky z dat (napr. limity z Data/), staci soubor prejmenovat na .Rmd
# a pred timto krokem nad nim spustit knitr::knit().
#
# ZAVISLOSTI:
#   - pandoc (samostatny, nebo ten z instalace Quarta)
#   - LaTeX (TinyTeX) s balicky lineno, ulem, etoolbox, float, babel-czech;
#     chybejici balicky skript doinstaluje pres tinytex
#----------------------------------------------------------#

#----------------------------------------------------------#
# Nacteni knihoven a pomucek -----
#----------------------------------------------------------#
packages <- c("tinytex")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

util <- file.path("R", "05_metodiky", "00_metodika_util.R")
if (!file.exists(util)) {
  stop(
    "Nenalezen soubor '", util, "' - spustte skript z korene repozitare.",
    call. = FALSE
  )
}
source(util, encoding = "UTF-8")

#----------------------------------------------------------#
# Nastaveni sazby -----
#----------------------------------------------------------#

# Okraje. Levy okraj je sirsi, aby se do nej vesla cisla radku, pravy proto,
# aby zbylo misto na pripominky v PDF citarne.
PDF_GEOMETRY <- "left=3cm,right=3cm,top=2.5cm,bottom=2.5cm"

# LaTeX balicky, ktere sazba potrebuje nad zaklad TinyTeXu
PDF_TEX_PACKAGES <- c(
  "lineno", "ulem", "etoolbox", "float",
  "babel-czech", "hyphen-czech",
  # vyzaduje sablona pandoku
  "booktabs", "longtable", "caption", "fancyvrb", "xcolor", "geometry",
  "hyperref", "microtype", "upquote", "bookmark"
)

#----------------------------------------------------------#
# Doplneni chybejicich LaTeX balicku -----
#----------------------------------------------------------#
# tlmgr se vola jen pro balicky, ktere skutecne chybi - jinak by sazba
# na pripojeni k siti cekala zbytecne.

pdf_zajisti_tex <- function(packages = PDF_TEX_PACKAGES) {
  if (!nzchar(Sys.which("kpsewhich"))) {
    message("  kpsewhich nenalezen - kontrola LaTeX balicku se preskakuje")
    return(invisible(NULL))
  }

  chybi <- packages[vapply(packages, function(p) {
    # Nektere balicky se jmenuji jinak nez jejich .sty (hyphen-czech nema .sty)
    out <- suppressWarnings(system2(
      "kpsewhich", paste0(p, ".sty"), stdout = TRUE, stderr = FALSE
    ))
    length(out) == 0 || !nzchar(out[1])
  }, logical(1))]

  # babel-czech a hyphen-czech nemaji .sty; poznaji se podle czech.ldf
  if (any(c("babel-czech", "hyphen-czech") %in% chybi)) {
    ldf <- suppressWarnings(system2("kpsewhich", "czech.ldf", stdout = TRUE, stderr = FALSE))
    if (length(ldf) > 0 && nzchar(ldf[1])) {
      chybi <- setdiff(chybi, c("babel-czech", "hyphen-czech"))
    }
  }

  if (length(chybi) == 0) return(invisible(NULL))

  message("  doplnuji LaTeX balicky: ", paste(chybi, collapse = ", "))
  tinytex::tlmgr_install(chybi)
  invisible(chybi)
}

#----------------------------------------------------------#
# Sazba -----
#----------------------------------------------------------#

metodika_md_pdf <- function(
    md = file.path("Metodiky", "Obojzivelnici", "met_obojzivelnici_2026.md"),
    pdf = NULL,
    sazba_dir = file.path("Metodiky", "Obojzivelnici", "sazba"),
    toc = TRUE,
    engine = "pdflatex"
) {
  if (!file.exists(md)) {
    stop(
      "Vstupni Markdown nenalezen: ", md,
      "\nNejdriv spustte R/05_metodiky/52_metodika_docx_md.R.",
      call. = FALSE
    )
  }

  if (is.null(pdf)) pdf <- sub("\\.md$", ".pdf", md)

  hlavicka <- file.path(sazba_dir, "hlavicka.tex")
  filtry <- file.path(sazba_dir, c("html_inline.lua", "tabulky.lua"))
  for (f in c(hlavicka, filtry)) {
    if (!file.exists(f)) stop("Nenalezen soubor pro sazbu: ", f, call. = FALSE)
  }

  pandoc <- md_pandoc()
  message("Sazba metodiky: ", basename(md))
  message("  pandoc: ", pandoc$popis)

  pdf_zajisti_tex()

  # Obrazky se v Markdownu odkazuji relativne (diagramy/*.png), takze pandoc
  # musi hledat v adresari dokumentu.
  md_dir <- dirname(normalizePath(md, winslash = "/"))

  # Do sazby jde vektorove PDF diagramu misto PNG, ktere je v Markdownu kvuli
  # nahledu na GitHubu. Obsah je stejny (oboji vznika z jedne predlohy
  # skriptem 51), vektor se ale v PDF nerozostri pri priblizeni.
  zdroj <- readLines(md, encoding = "UTF-8", warn = FALSE)
  pro_sazbu <- gsub("(\\(diagramy/[^)]+)\\.png\\)", "\\1.pdf)", zdroj)

  tmp_md <- file.path(tempdir(), paste0("metodika_sazba_", Sys.getpid(), ".md"))
  on.exit(unlink(tmp_md), add = TRUE)
  con <- file(tmp_md, open = "wb")
  writeLines(enc2utf8(pro_sazbu), con, useBytes = TRUE)
  close(con)

  args <- c(
    shQuote(tmp_md),
    # implicit_figures: samostatny obrazek v odstavci se sazi jako obrazek
    # s cislovanym popiskem ("Obrazek 1: ..."). Gfm tohle sam nedela a obrazek
    # by zustal vlozeny v textu bez popisku, na ktery se nelze odvolat.
    "--from=gfm+implicit_figures",
    paste0("--pdf-engine=", engine),
    "--include-in-header", shQuote(normalizePath(hlavicka, winslash = "/")),
    as.vector(rbind("--lua-filter", shQuote(normalizePath(filtry, winslash = "/")))),
    "--resource-path", shQuote(md_dir),
    "--variable", shQuote(paste0("geometry:", PDF_GEOMETRY)),
    "--variable", "papersize:a4",
    "--variable", "fontsize:11pt",
    "--variable", "linkcolor:blue",
    "--variable", "lang:cs",
    if (toc) "--toc",
    if (toc) "--toc-depth=3",
    "--output", shQuote(normalizePath(pdf, winslash = "/", mustWork = FALSE))
  )

  status <- pandoc$run(args)

  if (!identical(status, 0L) || !file.exists(pdf)) {
    stop("Sazba do PDF neprosla (status ", status, ").", call. = FALSE)
  }

  message("  -> ", pdf, " (", round(file.size(pdf) / 1024), " kB)")
  invisible(pdf)
}

if (sys.nframe() == 0) metodika_md_pdf()
