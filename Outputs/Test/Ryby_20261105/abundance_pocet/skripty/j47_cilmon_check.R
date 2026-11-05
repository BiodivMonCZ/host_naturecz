suppressPackageStartupMessages({library(dplyr); library(readr)})
setwd("C:/Users/jonas.gaigr/Documents/host_naturecz")
sp <- "<scratchpad>/"
all <- readRDS(paste0(sp, "all_slim.rds"))
f <- tempfile(); system2("git", c("-c", "core.autocrlf=false", "show", "HEAD:Data/Input/cil_mon_zdroj.csv"), stdout = f)
stary <- read_csv(f, locale = locale(encoding = "Windows-1250"), show_col_types = FALSE)$ZDROJ_NDOP
novy <- read_csv("Data/Input/cil_mon_zdroj.csv", locale = locale(encoding = "Windows-1250"), show_col_types = FALSE)$ZDROJ_NDOP
cil <- function(l) all$ZDROJ %in% l | all$PROJEKT %in% "Monitoring druhů ČR" | all$DRUH %in% c("Carabus menetriesi pacholei", "Bolbelasmus unicornis")
a <- cil(stary); b <- cil(novy)
cat("zaznamu:", nrow(all), " CILMON pred:", sum(a), " po:", sum(b), " zmenenych:", sum(a != b), "\n")
cat("zaznamu, jejichz ZDROJ je v novych radcich seznamu:", sum(all$ZDROJ %in% setdiff(novy, stary)),
    "; z toho bez PROJEKT Monitoring druhu CR:", sum(all$ZDROJ %in% setdiff(novy, stary) & !all$PROJEKT %in% "Monitoring druhů ČR"), "\n")
