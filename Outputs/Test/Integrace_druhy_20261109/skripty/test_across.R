suppressMessages(library(dplyr))
META_CHU <- c("ROK", "POLE", "NAZEV_LOK", "ID_ND_AKCE", "ID_ND_LOK")
set.seed(1)
x <- tibble(kod_chu = rep(c("A", "B"), each = 6), DRUH = "d", ID_IND = rep(c("X", "Y", "X"), 4),
            ROK = sample(c(2020, 2021, NA), 12, TRUE), POLE = sample(c("p1", "p2", NA), 12, TRUE),
            NAZEV_LOK = sample(c("n1", "n2"), 12, TRUE), ID_ND_AKCE = sample(c("a", "b", NA), 12, TRUE),
            ID_ND_LOK = sample(1:3, 12, TRUE), HOD_IND = sample(c("1", NA), 12, TRUE), CILMON = 1)
old <- x %>% group_by(kod_chu, DRUH, ID_IND) %>% reframe(ROK = toString(unique(ROK)), POLE = toString(unique(POLE)),
  NAZEV_LOK = toString(unique(NAZEV_LOK)), ID_ND_AKCE = toString(unique(ID_ND_AKCE)), ID_ND_LOK = toString(unique(ID_ND_LOK)),
  HOD_IND = toString(stats::na.omit(unique(HOD_IND))), CILMON_CHU = max(CILMON))
new <- x %>% group_by(kod_chu, DRUH, ID_IND) %>% reframe(across(all_of(META_CHU), ~ toString(unique(.x))),
  HOD_IND = toString(stats::na.omit(unique(HOD_IND))), CILMON_CHU = max(CILMON))
stopifnot(identical(old, new))
old2 <- x %>% group_by(kod_chu, DRUH) %>% summarise(ROK = toString(unique(ROK)), POLE = toString(unique(POLE)),
  NAZEV_LOK = toString(unique(NAZEV_LOK)), ID_ND_AKCE = toString(unique(ID_ND_AKCE)), ID_ND_LOK = toString(unique(ID_ND_LOK)), UROVEN = "chu")
new2 <- x %>% group_by(kod_chu, DRUH) %>% summarise(across(all_of(META_CHU), ~ toString(unique(.x))), UROVEN = "chu")
stopifnot(identical(old2, new2))
l1 <- old %>% tidyr::pivot_longer(cols = -c(kod_chu, DRUH, ROK, POLE, NAZEV_LOK, ID_ND_AKCE, ID_ND_LOK, CILMON_CHU), names_to = "I", values_to = "V")
l2 <- old %>% tidyr::pivot_longer(cols = -c(kod_chu, DRUH, all_of(META_CHU), CILMON_CHU), names_to = "I", values_to = "V")
stopifnot(identical(l1, l2))
f1 <- l1 %>% group_by(kod_chu) %>% tidyr::fill(ROK, POLE, NAZEV_LOK, ID_ND_AKCE, ID_ND_LOK, CILMON_CHU, .direction = "downup")
f2 <- l1 %>% group_by(kod_chu) %>% tidyr::fill(all_of(c(META_CHU, "CILMON_CHU")), .direction = "downup")
stopifnot(identical(f1, f2))
cat("across: shodne\n")
