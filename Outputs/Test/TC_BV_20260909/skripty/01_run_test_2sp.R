#----------------------------------------------------------#
# Testovaci beh kaskady hodnoceni - 2 druhy -----------------
#----------------------------------------------------------#
# Spousti kompletni kaskadu R/02_druhy pro Triturus cristatus a
# Bombina variegata:
#   21_1  uroven nalezu/akce  (extrakce indikatoru ze STRUKT_POZN)
#   21_2  uroven nalezu       (porovnani s limity -> STAV_IND)
#   22    hodnocena obdobi    (lok / pole / chu)
#   23    posledni nalez      (lok / pole / chu)
#   24    uroven dilci plochy (Tabulka 1 metodiky)
#   25    uroven uzemi / EVL  (Tabulka 2 metodiky)
#   27    export vsech trech urovni (nal / lok / chu)
#
# Kaskadu spousti puvodni R/02_druhy/20_n2k_druhy_run.R, tedy presne v tom
# poradi a tim kodem, ktery je ve vetvi main.
#----------------------------------------------------------#

t_start <- Sys.time()

options(warn = 1)

cache_path <- Sys.getenv("TC_CACHE", unset = "Test/.cache_config_2sp.rds")

if (file.exists(cache_path)) {

  for (pkg in c("tidyverse", "sf", "sp", "proj4", "openxlsx", "fuzzyjoin",
                "remotes", "ggplot2", "progress", "fs", "data.table")) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, quietly = TRUE)
    )
  }

  message("=== Nacitam cache konfigurace: ", cache_path, " ===")
  cached <- readRDS(cache_path)
  list2env(cached, envir = globalenv())
  rm(cached)
  gc(verbose = FALSE)
  message("=== Cache nactena: n2k_load = ", nrow(n2k_load), " zaznamu ===")

} else {

  source("Test/00_config_test_2sp.R", encoding = "UTF-8")

  message("=== Ukladam cache konfigurace: ", cache_path, " ===")
  keep <- setdiff(ls(envir = globalenv()), c("cfg_exprs", "e", "txt", "hit"))
  saveRDS(
    mget(keep, envir = globalenv()),
    cache_path,
    compress = FALSE
  )
}

#----------------------------------------------------------#
# Kaskada 21 -> 27 ----
#----------------------------------------------------------#
source("R/02_druhy/20_n2k_druhy_run.R", encoding = "UTF-8")

message("=== CELKOVY CAS: ",
        round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1),
        " min ===")

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
