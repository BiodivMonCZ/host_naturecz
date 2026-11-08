#----------------------------------------------------------#
# Kaskadove spusteni skriptu ve slozce 05_metodiky -----
#----------------------------------------------------------#
# Prevede metodiku obojzivelniku ze zdroju (.docx + predlohy diagramu)
# na Markdown a na PDF s cislovanymi radky pro pripominkovani.
#
# Postupne spousti:
#   51_metodika_diagramy.R  predlohy Diagrams/*.drawio -> PDF + PNG diagramu
#   52_metodika_docx_md.R   .docx -> met_obojzivelnici_2026.md
#   53_metodika_md_pdf.R    .md   -> met_obojzivelnici_2026.pdf
#
# Poradi je dane zavislostmi: Markdown se odkazuje na obrazky z prvniho kroku
# a sazba potrebuje hotovy Markdown.
#
# Spousti se z KORENE repozitare:
#   Rscript R/05_metodiky/50_metodika_run.R
# nebo v R:
#   source("R/05_metodiky/50_metodika_run.R", encoding = "UTF-8")
#
# VSTUPY (verzovane v repozitari):
#   Diagrams/strom_obojzivelnici_lok_long.drawio
#   Diagrams/strom_obojzivelnici_long.drawio
#   Metodiky/Obojzivelnici/met_ssEVL_SLOUCENE_teren_a_vyhodnocovani_obojziv_2024_zmeny.docx
#   Metodiky/Obojzivelnici/sazba/hlavicka.tex
#   Metodiky/Obojzivelnici/sazba/html_inline.lua
#   Metodiky/Obojzivelnici/sazba/tabulky.lua
#
# VYSTUPY (generovane, prepisuji se):
#   Metodiky/Obojzivelnici/diagramy/*.tex, *.pdf, *.png
#   Metodiky/Obojzivelnici/met_obojzivelnici_2026.md
#   Metodiky/Obojzivelnici/met_obojzivelnici_2026.pdf
#
# VNEJSI ZAVISLOSTI:
#   pandoc (samostatny, nebo ten z instalace Quarta)
#   LaTeX  (TinyTeX; chybejici balicky si skripty doinstaluji)
#----------------------------------------------------------#

#----------------------------------------------------------#
# Kontrola, ze se spousti z korene repozitare -----
#----------------------------------------------------------#
# Vsechny cesty ve skriptech jsou relativni ke koreni repozitare, aby byl beh
# stejny na kazde stanici. Bez teto kontroly by skripty spadly az na prvnim
# chybejicim souboru, s mnohem mene srozumitelnou chybou.

skripty <- file.path(
  "R", "05_metodiky",
  c("51_metodika_diagramy.R", "52_metodika_docx_md.R", "53_metodika_md_pdf.R")
)

chybi <- skripty[!file.exists(skripty)]
if (length(chybi) > 0) {
  stop(
    "Nenalezeny skripty:\n  ", paste(chybi, collapse = "\n  "),
    "\nSpustte kaskadu z korene repozitare (tam, kde je host_naturecz.Rproj).",
    call. = FALSE
  )
}

#----------------------------------------------------------#
# Beh -----
#----------------------------------------------------------#

# Skripty se jen nactou - kazdy z nich se sam nespousti, kdyz je zdrojovany
# (viz podminka sys.nframe() na jejich konci), takze o poradi kroku rozhoduje
# az tento skript.
for (s in skripty) source(s, encoding = "UTF-8")

#--------------------------------------------------#
## 1. Diagramy ----
#--------------------------------------------------#
message("\n==== 1/3 diagramy z predloh .drawio ====")
metodika_diagramy()

#--------------------------------------------------#
## 2. Prevod textu ----
#--------------------------------------------------#
message("\n==== 2/3 prevod .docx -> Markdown ====")
prevod <- metodika_docx_md()

#--------------------------------------------------#
## 3. Sazba ----
#--------------------------------------------------#
message("\n==== 3/3 sazba Markdown -> PDF ====")
pdf <- metodika_md_pdf(md = prevod$md)

message("\nHotovo. Vysledky:")
message("  ", prevod$md)
message("  ", pdf)
