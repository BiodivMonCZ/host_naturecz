#' @title Načtení a zpracování vrstev mapování biotopů (VMB)
#'
#' @description
#' Tato funkce načítá prostorová (`.shp`) a atributová (`.dbf`, `.csv`) data 
#' pro různé etapy mapování biotopů (Základní, Aktualizace 1, Aktuální).
#' Provádí spojení tabulek, výpočet hodnocení (`FSB_EVAL`) a čištění dat.
#'
#' @details
#' Funkce pracuje s pevně danými síťovými cestami (`//bali.nature.cz/...`) a 
#' specifickými soubory na disku S:/.
#' 
#' **Upozornění:** Funkce nevrací hodnotu standardním způsobem, ale vytváří 
#' objekty přímo v globálním prostředí (`.GlobalEnv`) pomocí funkce `assign()`.
#'
#' @param vmb_x Integer. Určuje verzi dat, která se má načíst:
#' \itemize{
#'   \item \code{0} - Aktuální vrstva (CR_AKTUALNI)
#'   \item \code{1} - Základní mapování (CR_20060501)
#'   \item \code{2} - Aktualizace 1 (CR_Aktualizace1)
#' }
#' @param clean Logical. Pokud je \code{TRUE} (výchozí), odstraní z prostředí 
#' pomocné objekty vzniklé během načítání a ponechá pouze výsledné datasety.
#'
#' @return Funkce nevrací žádnou hodnotu (return invisible). 
#' Místo toho vytváří v globálním prostředí následující objekty v závislosti na parametru \code{vmb_x}:
#' \itemize{
#'   \item \code{vmb_shp_sjtsk_orig} (pro vmb_x = 1)
#'   \item \code{vmb_shp_sjtsk_a1}, \code{paseky_a1} (pro vmb_x = 2)
#'   \item \code{vmb_shp_sjtsk_akt}, \code{vmb_pb_x_akt}, \code{paseky} (pro vmb_x = 0)
#' }
#'
#' @importFrom sf st_read
#' @importFrom dplyr filter bind_rows group_by mutate ungroup select distinct left_join inner_join case_when rename n
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' # Načtení základní vrstvy (2006)
#' load_vmb(vmb_x = 1)
#' 
#' # Načtení aktuální vrstvy bez promazání pomocných proměnných
#' load_vmb(vmb_x = 0, clean = FALSE)
#' }
load_vmb <- function(vmb_x = 1, clean = TRUE) {
  
  if(vmb_x == 0) {
    
    # VMB1 (zakladni) ----
    vmb_shp_sjtsk_orig_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/20060501_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_hab_dbf_orig <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/HAB20060501_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_pb_dbf_orig <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_20060501/Biotop/PB20060501_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      ) %>%
      dplyr::filter(
        !OBJECTID %in% vmb_hab_dbf_orig$OBJECTID
      )
    
    vmb_hab_pb_dbf_orig <- 
      dplyr::bind_rows(
        vmb_hab_dbf_orig, 
        vmb_pb_dbf_orig
      ) %>%
      dplyr::group_by(
        SEGMENT_ID
      ) %>%
      dplyr::mutate(
        moz_num = dplyr::n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
      ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_orig <- 
      vmb_shp_sjtsk_orig_read %>%
      dplyr::left_join(
        vmb_hab_dbf_orig, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::left_join(
        vmb_hab_pb_dbf_orig,
        by = "SEGMENT_ID"
      ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
        ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT
        )
      )
    
    assign(
      "vmb_shp_sjtsk_orig", 
      vmb_shp_sjtsk_orig, 
      envir = .GlobalEnv
    )
    
  } else if(vmb_x == 2) {
    
    # VMB2 (VMBa1) ----
    vmb_shp_sjtsk_a1_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Aktualizace1_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_hab_dbf_a1 <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Hab_biotop.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_pb_dbf_a1 <-
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_Aktualizace1/Biotop/Aktualizace1_Biotop.dbf",
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_x_dbf_a1 <-
      vmb_pb_dbf_a1 %>%
      dplyr::filter(
        BIOTOP == "X"
      )
    
    # POZOR: v původním kódu zde bylo voláno vmb_pb_x_dbf_a1, které nebylo definováno.
    # Předpokládám, že šlo o překlep a mělo být použito vmb_x_dbf_a1 nebo chybí definice.
    # Ponechávám původní název proměnné, ale toto pravděpodobně skončí chybou:
    # vmb_pb_x_a1 <- 
    #   dplyr::inner_join(
    #     vmb_shp_sjtsk_a1_read, 
    #     vmb_pb_x_dbf_a1,  <-- ZDE JE PRAVDĚPODOBNĚ CHYBA V NÁZVU PROMĚNNÉ
    #     by = "SEGMENT_ID"
    #   )
    
    vmb_hab_pb_dbf_a1 <- 
      dplyr::bind_rows(
        vmb_hab_dbf_a1, 
        vmb_pb_dbf_a1 %>%
          dplyr::filter(
            !OBJECTID_1 %in% vmb_hab_dbf_a1$OBJECTID_1
          )
      ) %>%
      dplyr::group_by(SEGMENT_ID
      ) %>%
      dplyr::mutate(
        moz_num = dplyr::n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
        )
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
      ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_a1 <- 
      vmb_shp_sjtsk_a1_read %>%
      dplyr::left_join(
        vmb_hab_dbf_a1, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::left_join(
        vmb_hab_pb_dbf_a1, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
        ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT
        )
      )
    
    paseky_a1 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_a1_results_20240814.csv")
    
    assign(
      "vmb_shp_sjtsk_a1",
      vmb_shp_sjtsk_a1, 
      envir = .GlobalEnv
    )
    
    assign(
      "paseky_a1", 
      paseky_a1, 
      envir = .GlobalEnv
    )
    
  } else if(vmb_x == 0) {
    # VMBX  (aktualni) ----
    vmb_shp_sjtsk_akt_read <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Aktualni_Segment.shp", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_hab_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/HAB_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    vmb_pb_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/PB_BIOTOP.dbf", 
        options = "ENCODING=WINDOWS-1250"
      ) 
    vmb_x_dbf_akt <- 
      sf::st_read(
        "//bali.nature.cz/du/Mapovani/Biotopy/CR_AKTUALNI/Biotop/X_biotop.dbf", 
        options = "ENCODING=WINDOWS-1250"
      )
    
    vmb_pb_x_dbf_akt <-
      dplyr::bind_rows(
        vmb_pb_dbf_akt,
        vmb_x_dbf_akt
      ) %>%
      dplyr::distinct()
    
    vmb_pb_x_akt <- 
      dplyr::inner_join(
        vmb_shp_sjtsk_akt_read, 
        vmb_pb_x_dbf_akt,
        by = "SEGMENT_ID"
      )
    
    vmb_hab_pb_dbf_akt <- 
      dplyr::bind_rows(
        vmb_hab_dbf_akt,
        vmb_pb_dbf_akt %>%
          dplyr::filter(
            !OBJECTID %in% vmb_hab_dbf_akt$OBJECTID
          )
      ) %>%
      dplyr::group_by(
        SEGMENT_ID
      ) %>%
      dplyr::mutate(
        moz_num = dplyr::n(),
        FSB_EVAL_prep = dplyr::case_when(
          sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
          sum(STEJ_PR, na.rm = TRUE) >= 50 & sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
          sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_
        )
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::select(
        SEGMENT_ID,
        FSB_EVAL_prep
      ) %>%
      dplyr::distinct()
    
    vmb_shp_sjtsk_akt <- 
      vmb_shp_sjtsk_akt_read %>%
      dplyr::left_join(
        ., 
        vmb_hab_dbf_akt, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::left_join(
        ., 
        vmb_hab_pb_dbf_akt, 
        by = "SEGMENT_ID"
      ) %>%
      dplyr::mutate(
        FSB_EVAL = dplyr::case_when(
          FSB_EVAL_prep == "X" ~ "X",
          TRUE ~ FSB
        ),
        HABITAT = dplyr::case_when(
          HABITAT == 6210 & HABIT_TYP == "p" ~ "6210p",
          TRUE ~ HABITAT),
        REGION_ID = REGION_ID.x
      ) %>%
      dplyr::rename(
        DATUM = DATUM.x
      )
    
    paseky_23 <- read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv")
    
    
    assign(
      "vmb_shp_sjtsk_akt", 
      vmb_shp_sjtsk_akt, 
      envir = .GlobalEnv
    )
    assign(
      "vmb_pb_x_akt", 
      vmb_pb_x_akt, 
      envir = .GlobalEnv
    )
    assign(
      "paseky", 
      paseky_23, 
      envir = .GlobalEnv
    )
    
  } 
  
  # CLEANUP SECTION
  if(vmb_x == 1 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_orig_read, 
      vmb_hab_dbf_orig, 
      vmb_pb_dbf_orig,
      vmb_hab_pb_dbf_orig
    )
    
  } else if(vmb_x == 2 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_a1_read, 
      vmb_hab_dbf_a1, 
      vmb_pb_dbf_a1, 
      vmb_hab_pb_dbf_a1
    )
    
  } else if(vmb_x == 0 & clean == TRUE) {
    
    rm(
      vmb_shp_sjtsk_akt_read, 
      vmb_hab_dbf_akt, 
      vmb_pb_dbf_akt, 
      vmb_hab_pb_dbf_akt,
      vmb_x_dbf_akt,
      vmb_pb_x_dbf_akt
    )
    
  }
  
}