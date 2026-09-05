#----------------------------------------------------------#
# Kaskadove spusteni skriptu ve slozce 02_druhy -----
#----------------------------------------------------------#
# Postupne zdrojuje vsechny skripty ve slozce R/02_druhy v poradi
# danem jejich nazvem souboru (numericky prefix urcuje poradi kroku
# vyhodnoceni: 21 akce, 22 obdobi, 23 posledni nalez, 24 lokality,
# 25 uzemi, 27 zapis/export).
#
# Nacita si jen to, co potrebuje vetev druhu:
#   R/00_config/00_n2k_config.R      - obecny config (knihovny, limity,
#                                      sdilene ciselniky, vrstvy EVL a PO)
#   R/00_config/02_n2k_data_druhy.R  - data druhu (export z NDOP -> n2k_load,
#                                      ciselniky druhu, cilove stavy)
#
# Data stanovist (R/00_config/03_n2k_data_stanoviste.R) se ZAMERNE nenacitaji -
# vetev druhu je nepotrebuje a jejich zdroje (AktualizacniOkrsky.shp, exporty
# redlist/invaze/expanze) na bezne stanici casto chybi.
#
# Oba skripty se spousti jen tehdy, kdyz jeste nebezely; uz nactena data se
# znovu necetou (nacteni exportu z NDOP trva radove minuty).
#----------------------------------------------------------#

nacti_config <- function(path, objekt, popis) {

  if (exists(objekt)) {
    return(invisible(FALSE))
  }

  if (!file.exists(path)) {
    stop(
      "Objekt '", objekt, "' neexistuje a skript '", path, "' nebyl nalezen",
      " - spustte kaskadu z korene repozitare, nebo nactete ", popis, " rucne."
    )
  }

  message("Objekt '", objekt, "' neexistuje - spoustim ", path)
  source(path, encoding = "UTF-8")

  # Skript mohl probehnout, aniz by objekt vznikl (napr. chybejici zdrojovy
  # soubor osetreny uvnitr) - bez nej by kaskada spadla az hloubeji, s daleko
  # mene srozumitelnou chybou.
  if (!exists(objekt)) {
    stop(
      "Skript '", path, "' probehl, ale objekt '", objekt, "' stale neexistuje",
      " - zkontrolujte nacteni zdrojovych dat (", popis, ")."
    )
  }

  invisible(TRUE)
}

# Poradi je zavazne: data druhu stoji na objektech z obecneho configu.
nacti_config(
  "R/00_config/00_n2k_config.R",
  "limity",
  "obecny config"
)
nacti_config(
  "R/00_config/02_n2k_data_druhy.R",
  "n2k_load",
  "export z NDOP"
)

#----------------------------------------------------------#
# Seznam skriptu ke spusteni -----
#----------------------------------------------------------#
druhy_folder <- "R/02_druhy"

druhy_scripts <- list.files(
  path = druhy_folder,
  pattern = "\\.R$",
  full.names = TRUE
)

# Vyradime sami sebe, kdyby byl skript nekdy volan primo z teto slozky
druhy_scripts <- druhy_scripts[basename(druhy_scripts) != "20_n2k_druhy_run.R"]

# Serazeni podle nazvu souboru (numericky prefix zajistuje spravne poradi)
druhy_scripts <- sort(druhy_scripts)

#----------------------------------------------------------#
# Postupne spusteni skriptu -----
#----------------------------------------------------------#
for (script in druhy_scripts) {
  message(paste0("=== Spoustim: ", basename(script), " ==="))
  source(script, encoding = "UTF-8")
}

message("=== HOTOVO: vsechny skripty ve slozce 02_druhy byly spusteny ===")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
