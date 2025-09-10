library(dplyr)
library(tidyr)
library(stringr)
library(readr)

build_indicator_map <- function(n2k_druhy, limity, script = NULL,
                                out_file = NULL) {
  
  # 1. Převést všechny sloupce za původními na character (pro bezpečný pivot)
  n2k_druhy <- n2k_druhy %>%
    mutate(across(where(is.numeric), as.character))
  
  # 2. Vytvořit seznam všech indikátorů podle názvů sloupců
  all_inds <- setdiff(names(n2k_druhy), "DRUH")
  
  # 3. Vytvořit lookup tabulku DRUH -> STRUKT_POZN
  lookup <- n2k_druhy %>%
    select(DRUH, STRUKT_POZN)
  
  # 4. Extrahovat kompilace indikátorů vektorově
  kompilace_df <- purrr::map_dfr(all_inds, function(ind) {
    pattern <- paste0("(?<=<", ind, ">).*?(?=</", ind, ">)")
    lookup %>%
      mutate(
        ID_IND = ind,
        KOMPILACE = str_extract(STRUKT_POZN, pattern)
      ) %>%
      select(DRUH, ID_IND, KOMPILACE)
  })
  
  # 5. Pivot původní n2k_druhy do long form pro join
  n2k_long <- n2k_druhy %>%
    pivot_longer(cols = all_of(all_inds),
                 names_to = "ID_IND",
                 values_to = "HODNOTA") %>%
    mutate(HODNOTA = as.character(HODNOTA))
  
  # 6. Vybrat pouze relevantní indikátory dle limity
  limity_lok <- limity %>%
    filter(UROVEN == "lok" & !is.na(LIM_IND)) %>%
    select(DRUH, ID_IND, everything())
  
  n2k_map <- n2k_long %>%
    right_join(limity_lok, by = c("DRUH", "ID_IND")) %>%
    left_join(kompilace_df, by = c("DRUH", "ID_IND"))
  
  # 7. Pokud zadaný script, lze ho evaluovat pro případné conditional mutates
  if(!is.null(script) && file.exists(script)) {
    source(script, local = TRUE)
  }
  
  # 8. Zápis CSV s Windows-1250
  if(!is.null(out_file)) {
    write.csv(n2k_map,
              file = out_file,
              row.names = FALSE,
              fileEncoding = "Windows-1250")
  }
  
  return(n2k_map)
}
