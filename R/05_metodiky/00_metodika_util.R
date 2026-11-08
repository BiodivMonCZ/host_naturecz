#----------------------------------------------------------#
# Sdilene pomucky pro prevod a sazbu metodik -----
#----------------------------------------------------------#
# Nacitaji skripty R/05_metodiky/52_metodika_docx_md.R (prevod .docx -> .md)
# a R/05_metodiky/53_metodika_md_pdf.R (sazba .md -> .pdf).
#
# Skript pouze definuje funkce, nic sam nedela. Spousti se z korene
# repozitare, stejne jako ostatni skripty projektu.
#----------------------------------------------------------#

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

#----------------------------------------------------------#
# Nalezeni pandoku -----
#----------------------------------------------------------#
# Pandoc se hleda ve trech mistech, v tomto poradi:
#   1. samostatna instalace na PATH,
#   2. instalace Quarta (Quarto si vlastni pandoc nese s sebou),
#   3. pandoc dodavany s balickem rmarkdown.
# Diky tomu prevod funguje i na stanici, kde je jen RStudio nebo jen Quarto.
#
# Vraci seznam: $popis (pro vypis do logu) a $run(args) pro spusteni.

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
