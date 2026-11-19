#' Načtení a zpracování vrstev mapování biotopů (VMB)
#'
#' Tato funkce načítá prostorová (`.shp`) a atributová (`.dbf`, `.csv`) data 
#' pro různé etapy mapování biotopů v ČR. Provádí spojení geometrie s atributy,
#' výpočet hodnocení (`FSB_EVAL`), čištění dat a jejich přípravu pro analýzu.
#'
#' @details
#' Funkce je specificky navržena pro interní infrastrukturu AOPK ČR.
#' Pracuje s pevně danými síťovými cestami (`//bali.nature.cz/...`) a 
#' specifickými soubory na disku `S:/`. Mimo tuto síť nebude funkce fungovat.
#'
#' Proces zpracování zahrnuje:
#' \enumerate{
#'   \item Načtení shapefilů a příslušných DBF tabulek.
#'   \item Filtraci a spojování tabulek (join).
#'   \item Výpočet pomocných statistik (např. počet segmentů `moz_num`).
#'   \item Kategorizaci hodnocení (`FSB_EVAL`) na základě pokryvnosti (`STEJ_PR`).
#'   \item Volitelné pročištění paměti od dočasných objektů.
#' }
#'
#' @param vmb_x Integer. Určuje verzi dat, která se má načíst:
#' \itemize{
#'   \item \code{0} - Aktuální vrstva (CR_AKTUALNI).
#'   \item \code{1} - Základní mapování (CR_20060501).
#'   \item \code{2} - Aktualizace 1 (CR_Aktualizace1).
#' }
#' @param clean Logical. Pokud je \code{TRUE} (výchozí), funkce odstraní z paměti 
#' pomocné objekty vzniklé během načítání a vrátí pouze finální vyčištěný list.
#'
#' @return Named list (seznam) obsahující `sf` objekty a dataframy. 
#' Obsah listu se liší podle zvoleného parametru \code{vmb_x}:
#' 
#' \strong{Pro vmb_x = 0 (Aktuální):}
#' \itemize{
#'   \item \code{vmb_shp_sjtsk_akt} - Hlavní sf objekt s geometrií a atributy.
#'   \item \code{vmb_pb_x_akt} - Tabulka/sf objekt pro specifické biotopy.
#'   \item \code{paseky} - Načtená CSV tabulka s výsledky pro paseky.
#' }
#' 
#' \strong{Pro vmb_x = 1 (Základní):}
#' \itemize{
#'   \item \code{vmb_shp_sjtsk_orig} - Hlavní sf objekt základního mapování.
#' }
#' 
#' \strong{Pro vmb_x = 2 (Aktualizace 1):}
#' \itemize{
#'   \item \code{vmb_shp_sjtsk_a1} - Hlavní sf objekt aktualizace.
#'   \item \code{vmb_pb_x_a1} - Spojená data pro biotopy typu X.
#'   \item \code{paseky_a1} - CSV tabulka s výsledky pro paseky (aktualizace).
#' }
#'
#' @importFrom sf st_read
#' @importFrom dplyr filter bind_rows group_by mutate ungroup select distinct left_join inner_join case_when rename n
#' @importFrom magrittr %>%
#' @importFrom utils read.csv2
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Příklad 1: Načtení základního mapování (2006)
#' data_orig <- load_vmb(vmb_x = 1)
#' shp_2006 <- data_orig$vmb_shp_sjtsk_orig
#' 
#' # Příklad 2: Načtení aktuální vrstvy bez čištění paměti
#' data_akt <- load_vmb(vmb_x = 0, clean = FALSE)
#' }
load_vmb <- function(vmb_x = 1, clean = TRUE) {
  
  # Inicializace výstupního listu
  output <- list()
  
  # -------------------------------------------------------------------------#
  # VMBX = 0: Aktuální vrstva
  # -------------------------------------------------------------------------#
  if(vmb_x == 0) {
    
    # Načtení dat
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
    
    # Zpracování PB a X biotopů
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
    
    # Příprava HAB a PB tabulek pro join a výpočet hodnocení
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
    
    # Finální spojení do shapefilu
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
    
    paseky_23 <- utils::read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_results_20220927.csv")
    
    # Uložení do výstupu
    output <- list(
      vmb_shp_sjtsk_akt = vmb_shp_sjtsk_akt,
      vmb_pb_x_akt = vmb_pb_x_akt,
      paseky = paseky_23
    )
    
    # -------------------------------------------------------------------------#
    # VMBX = 1: Základní mapování (VMB1)
    # -------------------------------------------------------------------------#
  } else if(vmb_x == 1) {
    
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
    
    output <- list(
      vmb_shp_sjtsk_orig = vmb_shp_sjtsk_orig
    )
    
    # -------------------------------------------------------------------------#
    # VMBX = 2: Aktualizace 1 (VMBa1)
    # -------------------------------------------------------------------------#
  } else if(vmb_x == 2) {
    
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
    
    # Spojení PB a X (Opraveno: definice chybějící proměnné)
    vmb_pb_x_dbf_a1 <- 
      dplyr::bind_rows(
        vmb_pb_dbf_a1,
        vmb_x_dbf_a1
      ) %>% 
      dplyr::distinct()
    
    vmb_pb_x_a1 <- 
      dplyr::inner_join(
        vmb_shp_sjtsk_a1_read, 
        vmb_pb_x_dbf_a1,
        by = "SEGMENT_ID"
      )
    
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
    
    paseky_a1 <- utils::read.csv2("S:/Složky uživatelů/Gaigr/hodnoceni_stanovist_grafy/paseky_a1_results_20240814.csv")
    
    output <- list(
      vmb_shp_sjtsk_a1 = vmb_shp_sjtsk_a1,
      vmb_pb_x_a1 = vmb_pb_x_a1,
      paseky_a1 = paseky_a1
    )
    
  } 
  
  # -------------------------------------------------------------------------#
  # Čištění paměti (Cleanup)
  # -------------------------------------------------------------------------#
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
      vmb_hab_pb_dbf_a1,
      vmb_x_dbf_a1,      # Přidáno pro úplnost
      vmb_pb_x_dbf_a1    # Přidáno pro úplnost
    )
    
  } else if(vmb_x == 0 & clean == TRUE) {
    
    # Opravený rm() bez zdvojené čárky
    rm(
      vmb_shp_sjtsk_akt_read, 
      vmb_hab_dbf_akt, 
      vmb_pb_dbf_akt, 
      vmb_hab_pb_dbf_akt,
      vmb_x_dbf_akt,
      vmb_pb_x_dbf_akt
    )
    
  }
  
  return(output)
}
