args <- commandArgs(TRUE); d <- args[1]
suppressMessages(library(dplyr))
lok <- readr::read_csv(file.path(d, "temp/n2k_druhy_lok.csv"), show_col_types = FALSE,
                       col_types = readr::cols(.default = "c"))
dp <- lok %>% filter(ID_IND == "CELKOVE_HODNOCENI") %>% count(SKUPINA, DRUH, name = "DP")
chu <- readr::read_csv(file.path(d, "temp/n2k_druhy_chu.csv"), show_col_types = FALSE,
                       col_types = readr::cols(.default = "c")) %>%
  filter(ID_IND == "CELKOVE_HODNOCENI") %>% count(DRUH, name = "EVL")
m <- readLines(file.path(d, "meta.txt"), encoding = "UTF-8")
sl <- strsplit(sub("^species_list: ", "", grep("^species_list", m, value = TRUE)), "|", fixed = TRUE)[[1]]
t <- tibble(DRUH = sl) %>% left_join(dp, by = "DRUH") %>% left_join(chu, by = "DRUH")
print(t %>% group_by(SKUPINA) %>% summarise(druhu = n(), DP = sum(as.numeric(DP), na.rm = TRUE), EVL = sum(EVL, na.rm = TRUE)), n = 50)
cat("bez DP:", paste(t$DRUH[is.na(t$DP)], collapse = ", "), "\n")
readr::write_csv(t, file.path(d, "pokryti.csv"))
