# Harness: faze 3 (24) + 4 (25) z Data/Temp, bez drahe faze 1-2.
suppressPackageStartupMessages({ library(tidyverse); library(data.table) })
options(warn = 1)

cached <- readRDS("Test/.cache_config_2sp.rds")
for (o in c("n2k_load", "n2k_export", "volna_export")) cached[[o]] <- NULL
list2env(cached, envir = globalenv())
rm(cached); invisible(gc(verbose = FALSE))

# Konstanta zije v 21_1 (nalez H-50); 21_1 se tu nespousti, je drahe.
DRUHY_METODIKY_OBOJ <- c(
  "Bombina bombina", "Bombina variegata", "Lissotriton montandoni",
  "Triturus cristatus", "Triturus carnifex", "Triturus dobrogicus"
)

SRC <- Sys.getenv("SRC_DIR")
for (f in c("21_2.R", "24.R", "25.R"))
  source(file.path(SRC, f), encoding = "UTF-8")

n2k_druhy_lim        <- readr::read_csv("Data/Temp/n2k_druhy_lim.csv", show_col_types = FALSE)
n2k_druhy_obdobi_chu <- readr::read_csv("Data/Temp/n2k_druhy_obdobi_chu.csv", show_col_types = FALSE)
pocetnost_uzemi      <- readr::read_csv("Data/Temp/n2k_druhy_pocetnost.csv", show_col_types = FALSE)
species_list         <- sort(unique(as.character(n2k_druhy_lim$DRUH)))
cat("druhy:", paste(species_list, collapse = " + "), "\n")

bezi <- function(tag) {
  lok <- purrr::map_dfr(species_list, function(sp)
    run_n2k_druhy_lok(dplyr::filter(n2k_druhy_lim, DRUH == sp), sp,
                      sites_subjects, limity, current_year = 2025))
  cat("[", tag, "] lok hotovo:", nrow(lok), "\n")
  chu <- purrr::map_dfr(species_list, function(sp)
    run_n2k_druhy_uzemi(n2k_druhy_lok = dplyr::filter(lok, DRUH == sp),
                        species_name = sp, sites_subjects = sites_subjects,
                        limity = limity, biotop_evd = biotop_evd,
                        n2k_druhy_obdobi_chu = n2k_druhy_obdobi_chu,
                        n2k_druhy_posledni_chu = NULL,
                        cilove_stavy = cilove_stavy,
                        pocetnost_uzemi = pocetnost_uzemi,
                        current_year = 2025))
  saveRDS(list(lok = lok, chu = chu), file.path(OUT, paste0(tag, ".rds")))
  cat("[", tag, "] chu hotovo:", nrow(chu), "\n")
  invisible(list(lok = lok, chu = chu))
}
