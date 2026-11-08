#----------------------------------------------------------#
# Citelny vypis srovnavacich tabulek do konzole -------------
#----------------------------------------------------------#
suppressPackageStartupMessages(library(dplyr))

OUT <- "Outputs/Test/TC_BV_20260909/srovnani"

fmt <- function(a, b, c, d) {
  ifelse(
    is.na(a),
    "-",
    paste0(a, "/", b, "/", c, "  [NA ", d, "]")
  )
}

dp <- readr::read_csv(file.path(OUT, "srovnani_indikatoru_DP.csv"),
                      show_col_types = FALSE)

dp_v <- dp %>%
  dplyr::transmute(
    ID_IND,
    JEN_U,
    KLIC   = dplyr::coalesce(KLIC_TC, KLIC_BV),
    LIM_TC = substr(dplyr::coalesce(LIM_TC, ""), 1, 26),
    LIM_BV = substr(dplyr::coalesce(LIM_BV, ""), 1, 26),
    TC_1_05_0 = fmt(TC_SPLNENO, TC_CASTECNE, TC_NESPLNENO, TC_NEHODNOCEN),
    BV_1_05_0 = fmt(BV_SPLNENO, BV_CASTECNE, BV_NESPLNENO, BV_NEHODNOCEN)
  )

cat("\n===== UROVEN DP (Tabulka 1) - splneno/castecne/nesplneno [NA] =====\n")
cat("    TC = Triturus cristatus (724 DP-rok) | BV = Bombina variegata (368 DP-rok)\n\n")
print(as.data.frame(dp_v), right = FALSE)

evl <- readr::read_csv(file.path(OUT, "srovnani_indikatoru_EVL.csv"),
                       show_col_types = FALSE)

evl_v <- evl %>%
  dplyr::transmute(
    ID_IND,
    # Na urovni EVL je LIM_* nacteno jako cislo (jediny limit je 70) -
    # pred coalesce je nutne sjednotit typ na text.
    LIM_TC = substr(dplyr::coalesce(as.character(LIM_TC), ""), 1, 12),
    LIM_BV = substr(dplyr::coalesce(as.character(LIM_BV), ""), 1, 12),
    TC = paste0(TC_DOBRY, "/", TC_ZHORSENY, "/", TC_SPATNY, "/", TC_NEZNAMY,
                "  [neh ", TC_NEHODNOCEN, "]"),
    BV = paste0(BV_DOBRY, "/", BV_ZHORSENY, "/", BV_SPATNY, "/", BV_NEZNAMY,
                "  [neh ", BV_NEHODNOCEN, "]")
  )

cat("\n===== UROVEN EVL (Tabulka 2) - dobry/zhorseny/spatny/neznamy =====\n")
cat("    TC = 191 EVL | BV = 63 EVL\n\n")
print(as.data.frame(evl_v), right = FALSE)

#----------------------------------------------------------#
# Pokryti: jak casto ma indikator vubec hodnotu ----
#----------------------------------------------------------#
cat("\n===== POKRYTI na urovni DP: podil radku s HOD_IND =====\n\n")
pokryti <- dp %>%
  dplyr::transmute(
    ID_IND,
    TC_pct = ifelse(is.na(TC_ZAZNAMU), NA,
                    round(100 * TC_S_HODNOTOU / TC_ZAZNAMU, 1)),
    BV_pct = ifelse(is.na(BV_ZAZNAMU), NA,
                    round(100 * BV_S_HODNOTOU / BV_ZAZNAMU, 1))
  ) %>%
  dplyr::arrange(TC_pct)
print(as.data.frame(pokryti), right = FALSE)

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
