#----------------------------------------------------------#
# Kaskadove spusteni skriptu ve slozce 02_druhy -----
#----------------------------------------------------------#
# Postupne zdrojuje vsechny skripty ve slozce R/02_druhy v poradi
# danem jejich nazvem souboru (numericky prefix urcuje poradi kroku
# vyhodnoceni: 21 akce, 22 obdobi, 23 posledni nalez, 24 lokality,
# 25 uzemi, 27 zapis/export).
#
# Stoji na R/00_config/00_n2k_config.R, ktery pripravuje vstupni objekty
# (n2k_load, sites_subjects, limity, evl, rp_code, n2k_oop, biotop_evd,
# indikatory_id, cis_ryby_delky, cis_pocet_kat). Pokud config jeste nebezel,
# spusti se automaticky; uz nactena data se znovu necetou.
#----------------------------------------------------------#

config_script <- "R/00_config/00_n2k_config.R"

if (!exists("n2k_load")) {

  if (!file.exists(config_script)) {
    stop(
      "Objekt 'n2k_load' neexistuje a config '", config_script, "' nebyl nalezen",
      " - spustte skript z korene repozitare, nebo nejprve nactete config rucne."
    )
  }

  message("Objekt 'n2k_load' neexistuje - spoustim ", config_script)
  source(config_script, encoding = "UTF-8")

  # Config mohl probehnout, aniz by vstupni data vznikla (napr. chybejici
  # export z NDOP) - bez 'n2k_load' by kaskada spadla az uvnitr 21_1.
  if (!exists("n2k_load")) {
    stop(
      "Config '", config_script, "' probehl, ale objekt 'n2k_load' stale",
      " neexistuje - zkontrolujte nacteni zdrojovych dat z NDOP."
    )
  }
}

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
