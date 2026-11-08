#----------------------------------------------------------#
# Beh kaskady druhu v izolovane kopii repozitare a ulozeni vystupu
#----------------------------------------------------------#
# Pouziti (z libovolne slozky):
#   Rscript beh_kaskady.R <kopie_repozitare> <vystupni_slozka> <druhy> [cache_dir]
#
#   <druhy>  "ALL" = vsechny druhy (bez filtru), jinak nazvy oddelene "|"
#
# Config (00_n2k_config.R + 02_n2k_data_druhy.R) se nacita VYRAZ PO VYRAZU
# stejne jako Test/00_config_test_2sp.R - vynechane jsou jen bloky, ktere
# druhova kaskada nepouziva a ktere na stanici nelze spustit (skip_tokens).
# Config se nacita VZDY CELY (vsechny druhy) a cachuje; podmnozina druhu se
# vybira az z hotoveho n2k_load. Je to vernejsi plnemu behu nez filtr pred
# vznikem n2k_load: sjednoceni nazvu lokalit ryb (02_n2k_data_druhy.R) pocita
# nejcastejsi zapis pres vsechny druhy ryb v EVL.
#
# Cache je klicovana md5 obou configu a vsech vstupu v Data/Input - zmena
# limitu (napr. transplantace radku) ji zneplatni.
#
# Ulozi:  <out>/temp/    vsechny Data/Temp/*.csv vznikle behem
#         <out>/export/  Outputs/Data/druhy/* vznikle behem (bez datumu v nazvu)
#         <out>/meta.txt commit, druhy, datum behu (pro maskovani datum_hodnoceni)
#----------------------------------------------------------#

args <- commandArgs(trailingOnly = TRUE)
repo      <- normalizePath(args[1], winslash = "/")
out_dir   <- args[2]
druhy_arg <- args[3]
cache_dir <- if (length(args) >= 4) args[4] else file.path(dirname(out_dir), "_cache")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir, winslash = "/")
cache_dir <- normalizePath(cache_dir, winslash = "/")

setwd(repo)
options(warn = 1)
t_start_beh <- Sys.time()

TEST_SPECIES <- if (identical(druhy_arg, "ALL")) NULL else strsplit(druhy_arg, "|", fixed = TRUE)[[1]]

cfg_paths <- c("R/00_config/00_n2k_config.R", "R/00_config/02_n2k_data_druhy.R")
vstupy <- c(cfg_paths, sort(list.files("Data/Input", pattern = "\\.(csv|xlsx|shp|dbf)$", full.names = TRUE)))
klic_f <- tempfile()
writeLines(unname(tools::md5sum(vstupy)), klic_f)
cache_path <- file.path(cache_dir, paste0("cfg_", substr(unname(tools::md5sum(klic_f)), 1, 12), ".rds"))

pkgs <- c("tidyverse", "sf", "sp", "proj4", "openxlsx", "fuzzyjoin", "remotes",
          "ggplot2", "progress", "fs", "data.table")
lokalni <- c("args", "repo", "out_dir", "druhy_arg", "cache_dir", "t_start_beh", "TEST_SPECIES",
             "cfg_paths", "vstupy", "klic_f", "cache_path", "pkgs", "lokalni")

if (file.exists(cache_path)) {
  for (pkg in pkgs) suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
  message("=== Cache configu: ", cache_path, " ===")
  cached <- readRDS(cache_path)
  list2env(cached, envir = globalenv())
  rm(cached)
} else {
  skip_tokens <- c("vodstvo", "BiotopZvld", "biotop_zvld", "n2k_union", "akt_okrsky",
                   "AktualizacniOkrsky", "export_redlist", "export_invaze", "export_expanze",
                   "redlist_species", "invasive_species", "expansive_species", "rn2kcz")
  cfg_exprs <- unlist(lapply(cfg_paths, function(p) as.list(parse(p, encoding = "UTF-8"))), recursive = FALSE)
  for (i in seq_along(cfg_exprs)) {
    txt <- paste(deparse(cfg_exprs[[i]]), collapse = "\n")
    if (any(vapply(skip_tokens, grepl, logical(1), x = txt, fixed = TRUE))) next
    eval(cfg_exprs[[i]], envir = globalenv())
  }
  # Surove exporty se po vzniku n2k_load v kaskade nepouzivaji.
  rm(list = intersect(c("n2k_export", "volna_export"), ls(globalenv())), envir = globalenv())
  keep <- setdiff(ls(envir = globalenv()), c(lokalni, "cfg_exprs", "txt", "i", "skip_tokens"))
  saveRDS(mget(keep, envir = globalenv()), cache_path, compress = FALSE)
  message("=== Cache ulozena: ", cache_path, " ===")
}

if (!is.null(TEST_SPECIES)) {
  chybi <- setdiff(TEST_SPECIES, as.character(n2k_load$DRUH))
  if (length(chybi) > 0) warning("Druhy bez zaznamu v n2k_load: ", paste(chybi, collapse = ", "))
  n2k_load <- dplyr::filter(n2k_load, DRUH %in% TEST_SPECIES) %>%
    dplyr::mutate(DRUH = droplevels(as.factor(DRUH)))
}
gc(verbose = FALSE)
message("=== n2k_load: ", nrow(n2k_load), " zaznamu, ", dplyr::n_distinct(n2k_load$DRUH), " druhu ===")

# Data/Temp kopie se vycisti, aby se do vystupu nedostaly soubory z drivejska.
dir.create("Data/Temp", showWarnings = FALSE)
unlink(list.files("Data/Temp", full.names = TRUE))
t0 <- Sys.time() - 1
datum_behu <- as.character(Sys.Date())

source("R/02_druhy/20_n2k_druhy_run.R", encoding = "UTF-8")

datum_behu_konec <- as.character(Sys.Date())

dir.create(file.path(out_dir, "temp"), showWarnings = FALSE)
dir.create(file.path(out_dir, "export"), showWarnings = FALSE)
file.copy(list.files("Data/Temp", full.names = TRUE), file.path(out_dir, "temp"), overwrite = TRUE)
ex <- list.files("Outputs/Data/druhy", full.names = TRUE)
ex <- ex[file.mtime(ex) >= t0]
for (f in ex) {
  file.copy(f, file.path(out_dir, "export", sub("_20[0-9]{6}_", "_", basename(f))), overwrite = TRUE)
}
# Novy export z kopie repozitare pryc, aby v ni nezustal neverzovany soubor.
file.remove(ex)

writeLines(c(
  paste0("commit: ", system("git rev-parse HEAD", intern = TRUE)),
  paste0("dirty: ", paste(system("git status --porcelain -uno", intern = TRUE), collapse = " | ")),
  paste0("druhy: ", druhy_arg),
  paste0("n_druhu_species_list: ", length(species_list)),
  paste0("species_list: ", paste(species_list, collapse = "|")),
  paste0("datum_behu: ", datum_behu),
  paste0("datum_behu_konec: ", datum_behu_konec),
  paste0("cas_min: ", round(as.numeric(difftime(Sys.time(), t_start_beh, units = "mins")), 1))
), file.path(out_dir, "meta.txt"))

message("=== HOTOVO za ", round(as.numeric(difftime(Sys.time(), t_start_beh, units = "mins")), 1), " min ===")
