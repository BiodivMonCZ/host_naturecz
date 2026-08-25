run_n2k_druhy_uzemi <- function(
    n2k_druhy_lok,
    species_name,
    sites_subjects,
    limity,
    biotop_evd,
    n2k_druhy_obdobi_chu,
    n2k_druhy_posledni_chu,
    cilove_stavy = NULL,
    pocetnost_uzemi = NULL,
    current_year = 2025
) {
  
  #----------------------------------------------------------#
  # 1. Validace a nacteni skupiny ----
  #----------------------------------------------------------#
  skupina_druhu <- n2k_druhy_lok %>% 
    dplyr::filter(DRUH == species_name) %>% 
    dplyr::pull(SKUPINA) %>% 
    unique() %>% 
    stats::na.omit() %>% 
    dplyr::first()
  
  pole_skupiny <- c("Brouci", "Motýli", "Vážky", "Rovnokřídlí")
  is_pole_druh <- species_name %in% sites_subjects$DRUH[sites_subjects$SKUPINA %in% pole_skupiny]
  if(species_name %in% c("Eriogaster catax", "Euphydryas aurinia", "Euphydryas maturna")) {
    is_pole_druh <- FALSE
  }
  
  #----------------------------------------------------------#
  # 2. Priprava kontextu (CELKOVE jako sloupec) ----
  #----------------------------------------------------------#
  # Vytvorime mapovaci tabulku: Lokalita+Rok -> Vysledek
  
  hodnoceni_lokalit <- n2k_druhy_lok %>%
    dplyr::filter(ID_IND == "CELKOVE_HODNOCENI") %>%
    dplyr::select(
      kod_chu, 
      DRUH, 
      KOD_LOKAL, 
      ROK, 
      POLE, # Pole je dulezite pro spravne napojeni u hmyzu
      CELKOVE_CTX = STAV_IND # Prejmenujeme pro jednoznacnost (CTX = Context)
    ) %>%
    dplyr::distinct()
  
  # Pripojime informaci o celkovem hodnoceni ke vsem radkum
  # Takze kazdy radek (treba POP_POCET) ted "vi", jak dopadla jeho lokalita
  n2k_druhy_aug <- n2k_druhy_lok %>%
    dplyr::left_join(
      hodnoceni_lokalit,
      by = c("kod_chu", "DRUH", "KOD_LOKAL", "ROK", "POLE")
    )
  
  #----------------------------------------------------------#
  # 3. Vypocet agragovanych statistik ----
  #----------------------------------------------------------#
  
  if (is_pole_druh) {
    # ----------------------------------------------#
    ## Logika pro POLE (HMYZ) ----
    # ----------------------------------------------#
    n2k_druhy_chu_temp <- n2k_druhy_aug %>%
      dplyr::filter(
        DRUH == species_name
      ) %>%
      dplyr::group_by(
        kod_chu, 
        DRUH
      ) %>%
      dplyr::reframe(
        ROK = toString(unique(ROK)),
        POLE = toString(unique(POLE)),
        NAZEV_LOK = toString(unique(NAZEV_LOK)),
        ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
        ID_ND_LOK = toString(unique(ID_ND_LOK)),
        CILMON_CHU = max(CILMON, na.rm = TRUE),
        
        # Zde staci pocitat radky s hodnocenim
        POP_POCETPOLE1 = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1, na.rm = TRUE),
        
        POP_POCETPOLE1D = sum(
          ID_IND == "CELKOVE_HODNOCENI" & 
            !HOD_IND %in% c("neznámý", "zhoršený", "špatný") & 
            !is.na(STAV_IND) & STAV_IND == 1 & # Zjednodusena logika
            CILMON == 1, 
          na.rm = TRUE
        ),
        STA_HABPOKRYVPRE = {
          k_chu <- unique(kod_chu)
          x <- biotop_evd$BIOTOP_PROCENTO[biotop_evd$SITECODE == k_chu & biotop_evd$DRUH == species_name]
          if (length(x) == 0) NA_real_ else unique(x)
        }
      ) %>%
      dplyr::mutate(
        POP_PROCPOLE1D = dplyr::case_when(
          POP_POCETPOLE1 == 0 ~ 0,
          TRUE ~ round(POP_POCETPOLE1D/POP_POCETPOLE1*100, 3)
        ),
        STA_HABPOKRYV = ifelse(is.na(STA_HABPOKRYVPRE) == TRUE, NA, STA_HABPOKRYVPRE*100)
      ) %>%
      dplyr::select(-STA_HABPOKRYVPRE)
    
  } else { 
    # ----------------------------------------------#
    ## Logika pro LOKALITY / OSTATNI (HMYZ NE) ----
    # ----------------------------------------------#
    n2k_druhy_chu_temp <- n2k_druhy_aug %>%
      dplyr::filter(DRUH == species_name) %>%
      dplyr::group_by(
        kod_chu, 
        DRUH
      ) %>%
      dplyr::reframe(
        ROK = toString(unique(ROK)), 
        POLE = toString(unique(POLE)), 
        NAZEV_LOK = toString(unique(NAZEV_LOK)), 
        ID_ND_AKCE = toString(unique(ID_ND_AKCE)), 
        ID_ND_LOK = toString(unique(ID_ND_LOK)),
        CILMON_CHU = max(CILMON, na.rm = TRUE),
        
        POP_PRESENCE = dplyr::case_when(
          any(ID_IND == "POP_PRESENCE" & STAV_IND == 1, na.rm = TRUE) ~ "ano",
          any(ID_IND == "POP_PRESENCE" & STAV_IND == 0, na.rm = TRUE) & !any(ID_IND == "POP_PRESENCE" & STAV_IND == 1, na.rm = TRUE) ~ "ne",
          TRUE ~ NA_character_
        ), 
        
        POP_POCETMAX = sum(dplyr::case_when(ID_IND == "POP_POCETMAX" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETMIN = sum(dplyr::case_when(ID_IND == "POP_POCETMIN" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETSUM = sum(dplyr::case_when(ID_IND == "POP_POCET" & CILMON == 1 ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE) %>% max(na.rm = TRUE),
        
        # Nyni pouzijeme sloupec "CELKOVE_CTX", ktery jsme pripojili na zacatku
        POP_POCETDOB = sum(dplyr::case_when(ID_IND == "POP_POCET" & CELKOVE_CTX == 1 & CILMON == 1 ~ as.numeric(HOD_IND), TRUE ~ 0), na.rm = TRUE) %>% max(na.rm = TRUE),
        POP_POCETOST = sum(dplyr::case_when(ID_IND == "POP_POCET" & CELKOVE_CTX != 1 & CILMON == 1 ~ as.numeric(HOD_IND), TRUE ~ 0), na.rm = TRUE),
        
        POP_PROCDOB = dplyr::case_when(is.na(POP_POCETDOB) | is.na(POP_POCETSUM) ~ NA_real_, POP_POCETSUM == 0 ~ 0, TRUE ~ round(POP_POCETDOB / POP_POCETSUM * 100, 3)),
        
        POP_POCETZIM = sum(dplyr::case_when(ID_IND == "POP_POCETZIM" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETZIM1 = sum(dplyr::case_when(ID_IND == "POP_POCETZIM1" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETZIM2 = sum(dplyr::case_when(ID_IND == "POP_POCETZIM2" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETZIM3 = sum(dplyr::case_when(ID_IND == "POP_POCETZIM3" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETZIMREF = mean(c(POP_POCETZIM1, POP_POCETZIM2, POP_POCETZIM3), na.rm = TRUE),
        POP_VITALZIM = ifelse(POP_POCETZIMREF == 0 | is.na(POP_POCETZIMREF), NA_real_, round(POP_POCETZIM/POP_POCETZIMREF, 3)),
        
        POP_POCETLETS1 = sum(dplyr::case_when(ID_IND == "POP_POCETLETS1" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETLETS2 = sum(dplyr::case_when(ID_IND == "POP_POCETLETS2" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE),
        POP_POCETLET = sum(dplyr::case_when(ID_IND == "POP_POCETLET" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLET1 = sum(dplyr::case_when(ID_IND == "POP_POCETLET1" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLET2 = sum(dplyr::case_when(ID_IND == "POP_POCETLET2" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLET3 = sum(dplyr::case_when(ID_IND == "POP_POCETLET3" ~ as.numeric(HOD_IND), TRUE ~ NA), na.rm = TRUE), 
        POP_POCETLETREF = mean(c(POP_POCETLET1, POP_POCETLET2, POP_POCETLET3), na.rm = TRUE),
        POP_VITALLET = ifelse(POP_POCETLETREF == 0 | is.na(POP_POCETLETREF), NA_real_, round(POP_POCETLET/POP_POCETLETREF, 3)),
        
        POP_REPROCHI = dplyr::case_when(
          POP_POCETLETS1 == 0 ~ NA_real_, 
          TRUE ~ round(POP_POCETLETS2/POP_POCETLETS1, 3)
        ),
        
        # Metodika: "Procento dobre hodnocenych dilcich ploch" = pocet DP v dobrem stavu /
        # pocet DP v dobrem, zhorsenem ci spatnem stavu * 100. DP s neznamym stavem se do
        # vypoctu (citatele ani jmenovatele) nezapocitavaji.
        LOK_POCETSUM = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1 & HOD_IND %in% c("dobrý", "zhoršený", "špatný"), na.rm = TRUE),
        LOK_POCETDOB = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1 & HOD_IND == "dobrý", na.rm = TRUE),
        LOK_PROCDOBR = dplyr::case_when(is.na(LOK_POCETDOB) | is.na(LOK_POCETSUM) ~ NA_real_, LOK_POCETSUM == 0 ~ NA_real_, TRUE ~ round(LOK_POCETDOB / LOK_POCETSUM * 100, 3)),
        # Stejne pocty pod nazvy z ciselniku cis_indikatory_popis.csv (ind_id 130 / 131),
        # aby se pri exportu do ISOP (chu_export, viz 27_n2k_druhy_zapis.R) spravne
        # napojily na oficialni kod indikatoru misto surove textove hodnoty ID_IND.
        LOK_DILCDOBRE = LOK_POCETDOB,
        LOK_DILCPOCET = LOK_POCETSUM,

        STA_HABPOKRYVPRE = {
          k_chu <- unique(kod_chu)
          x <- biotop_evd$BIOTOP_PROCENTO[biotop_evd$SITECODE == k_chu & biotop_evd$DRUH == species_name]
          if (length(x) == 0) NA_real_ else unique(x)
        }
      ) %>%
      dplyr::mutate(
        STA_HABPOKRYV = ifelse(is.na(STA_HABPOKRYVPRE) == TRUE, NA, STA_HABPOKRYVPRE*100)
      ) %>%
      dplyr::select(-STA_HABPOKRYVPRE)
  }
  
  #--------------------------------------------------#
  # 4. Prevod na long format a napojeni limitu ----
  #--------------------------------------------------#
  n2k_druhy_chu_komb_long <- n2k_druhy_chu_temp %>%
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric), 
        .fns = as.character
      )
    ) %>%
    tidyr::pivot_longer(
      cols = -c(kod_chu, DRUH, ROK, POLE, NAZEV_LOK, ID_ND_AKCE, ID_ND_LOK, CILMON_CHU), 
      names_to = "ID_IND",
      values_to = "HOD_IND"
    ) %>%
    dplyr::mutate(
      HOD_IND = as.character(HOD_IND)
    )
  
  n2k_druhy_chu_pre <- n2k_druhy_chu_komb_long %>%
    dplyr::right_join(
      .,
      limity %>%
        dplyr::filter(UROVEN == "chu"),
      by = c("DRUH" = "DRUH", "ID_IND" = "ID_IND")
    ) %>%
    dplyr::group_by(kod_chu, DRUH) %>%
    tidyr::fill(ROK, POLE, NAZEV_LOK, ID_ND_AKCE, ID_ND_LOK, CILMON_CHU, .direction = "downup") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(kod_chu, DRUH, ID_IND) %>%
    dplyr::arrange(dplyr::desc(!is.na(HOD_IND)), HOD_IND) %>% 
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        is.na(HOD_IND) == TRUE ~ NA_real_,
        TYP_IND == "min" & as.numeric(HOD_IND) < as.numeric(LIM_IND) ~ 0,
        TYP_IND == "min" & as.numeric(HOD_IND) >= as.numeric(LIM_IND) ~ 1,
        TYP_IND == "max" & as.numeric(HOD_IND) > as.numeric(LIM_IND) ~ 0,
        TYP_IND == "max" & as.numeric(HOD_IND) <= as.numeric(LIM_IND) ~ 1,
        TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
        TYP_IND == "val" & HOD_IND == LIM_IND ~ 1
      )
    ) %>%
    dplyr::mutate(
      IND_GRP = dplyr::case_when(
        TYP_IND %in% c("min", "max") ~ "minmax",
        TRUE ~ TYP_IND),
      KLIC = dplyr::case_when(
        is.na(HOD_IND) == TRUE ~ "ne",
        TRUE ~ KLIC
      )
    ) %>%
    dplyr::distinct() %>%
    dplyr::select(-dplyr::starts_with("..."))
  
  #----------------------------------------------------------#
  # 5. Konsolidace a finalni hodnoceni uzemi (CHU) -----
  #----------------------------------------------------------#
  # POZNAMKA (obojzivelnici, Tabulka 2 metodiky): hodnoceni stavu druhu na urovni EVL
  # kombinuje DVA indikatory - (1) "% dobre hodnocenych DP" (LOK_PROCDOBR, limit min 70,
  # viz limity_vse.csv) a (2) "pocetnost populace" jako klouzavy prumer za posledni 3 roky
  # porovnany s revidovanym cilovym stavem evidovanym v ISOP (u EVL) resp. planu pece (u MZCHU).
  # Indikator (1) je nize implementovan pres generickou minmax logiku. Indikator (2) NENI
  # implementovan - repozitar aktualne neobsahuje zdroj dat s per-lokalitnim cilovym stavem
  # (viz Data/Input/). Bez nej CELKOVE hodnoceni uzemi vychazi jen z indikatoru (1), coz
  # neodpovida plne Tabulce 2 metodiky.
  #----------------------------------------------------------#
  # 4b. Druhy indikator Tabulky 2 - pocetnost vs. cilovy stav -----
  #----------------------------------------------------------#
  # Metodika, Tabulka 2: "Limitni hodnotou je v pripade hodnoceni stavu predmetu
  # ochrany EVL revidovany cilovy stav pro dane uzemi, ktery je evidovan v ISOP."
  # Limitni hodnoty jsou tedy SPECIFICKE PRO KAZDE UZEMI.
  #
  # PROC MIMO OBECNOU MINMAX VETEV: limity_vse.csv je klicovany DRUH x ID_IND
  # a NEMA rozmer uzemi, takze limit specificky pro kazdou EVL v nem nelze
  # vyjadrit. Ze dvou moznosti (rozsirit limity o KOD_CHU vs. napojit cilovou
  # hodnotu primo zde) je zvolena druha - rozsireni limitu by zasahlo genericke
  # napojeni limitu ve 21_2 i 25, ktere obsluhuje i ryby, hmyz, savce a rostliny,
  # a nese tedy riziko regrese mimo obojzivelniky.
  #
  # POZOR NA JEDNOTKY (nalez S-4): cilovy stav je v SDO veden v jednotkach
  # "jedinci"/"adulti", zatimco u Bombina bombina je hodnocenou jednotkou
  # VOKALIZUJICI SAMCI. Rozhodnuti autoru metodiky 2026-08-20: porovnavat
  # BEZ PREPOCTU, nesoulad se dokoncí v pristi expertne revidovane verzi
  # cilovych stavu. Rozdil je proto propsan do vystupu ve sloupci JEDNOTKA.
  cil_chu <- NULL
  if (!is.null(cilove_stavy) && !is.null(pocetnost_uzemi)) {
    cil_chu <- pocetnost_uzemi %>%
      dplyr::filter(DRUH == species_name) %>%
      dplyr::left_join(
        cilove_stavy %>% dplyr::filter(DRUH == species_name),
        by = c("kod_chu", "DRUH")
      ) %>%
      dplyr::mutate(
        # STAV_IND: 1 = cilovy stav splnen, 0 = nesplnen, NA = neznamo
        # (uzemi bez evidovaneho ciloveho stavu - napr. cely Lissotriton
        # montandoni, ktery ma v SDO vsechny hodnoty prazdne).
        STAV_CIL = dplyr::case_when(
          is.na(POP_CILSTAV) | is.na(POP_POCETPRUM3) ~ NA_real_,
          POP_POCETPRUM3 >= POP_CILSTAV ~ 1,
          TRUE ~ 0
        )
      )
  }

  n2k_druhy_chu_vypocet <- n2k_druhy_chu_pre %>%
    dplyr::group_by(kod_chu, DRUH, ID_IND, KLIC) %>%
    dplyr::reframe(
      ROK = toString(unique(ROK)),
      POLE = toString(unique(POLE)),
      NAZEV_LOK = toString(unique(NAZEV_LOK)),
      ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
      ID_ND_LOK = toString(unique(ID_ND_LOK)),
      HOD_IND = toString(stats::na.omit(unique(HOD_IND))),        
      TYP_IND = unique(TYP_IND),
      LIM_IND = unique(LIM_IND),
      JEDNOTKA = unique(JEDNOTKA),
      LIM_INDLIST = unique(LIM_INDLIST),
      
      # Vypocet STAV_IND
      STAV_IND = dplyr::case_when(
        is.na(HOD_IND) == TRUE ~ NA_real_,
        IND_GRP == "minmax" & grepl("POP_POSK", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
        IND_GRP == "val" ~ max(as.numeric(STAV_IND), na.rm = TRUE)
      ),
      KLIC = unique(KLIC),
      UROVEN = unique(UROVEN),
      IND_GRP = unique(IND_GRP),
      CILMON_CHU = max(CILMON_CHU, na.rm = TRUE)
    ) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Inf vznika z min()/max() nad prazdnym vektorem, tj. kdyz pro dany indikator
      # neexistuje zadna nenulova (nechybejici) hodnota STAV_IND ke shrnuti (indikator
      # nebyl zjisten/vyhodnocen). Puvodne se toto tise mapovalo na 0 (spatny), coz
      # nespravne oznacovalo "nemame data" jako "nepriznivy stav" - opraveno na NA
      # ("neznamy"), v souladu s existujicim osetrenim is.na(STAV_IND) nize.
      STAV_IND = ifelse(is.infinite(STAV_IND), NA_real_, STAV_IND)
    ) %>%
    dplyr::group_by(kod_chu, DRUH) %>%
    dplyr::mutate(
      IND_SUMKLIC = sum(STAV_IND[KLIC == "ano" & UROVEN == "chu" & !is.na(LIM_IND)], na.rm = TRUE),
      LENIND_SUMKLIC = length(unique(stats::na.omit(ID_IND[KLIC == "ano" & UROVEN == "chu" & !is.na(LIM_IND)]))),
      LENIND_NAKLIC = sum(KLIC == "ano" & UROVEN == "chu" & is.na(HOD_IND), na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      CELKOVE = dplyr::case_when(
        IND_SUMKLIC < (LENIND_SUMKLIC - 1 - LENIND_NAKLIC) ~ 0,
        IND_SUMKLIC < (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 0.5,
        IND_SUMKLIC >= (LENIND_SUMKLIC - LENIND_NAKLIC) ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::ungroup()

  #----------------------------------------------------------#
  # 5b. Tabulka 2 metodiky - kombinace obou indikatoru uzemi -----
  #----------------------------------------------------------#
  # | % dobre hodnocenych DP | pocet jedincu (klouzavy prumer 3 roky) | stav    |
  # | min 70 %               | cilovy stav splnen                     | dobry   |
  # | min 70 %               | cilovy stav nesplnen                   | zhorseny|
  # | mene nez 70 %          | cilovy stav splnen                     | zhorseny|
  # | mene nez 70 %          | cilovy stav nesplnen                   | spatny  |
  #
  # PROC SE NAHRAZUJE PUVODNI ARITMETIKA (nalez H-05): puvodni vzorec vyse
  # odvozoval CELKOVE z POCTU splnenych klicovych indikatoru. Na urovni chu je
  # ale klicovy indikator jediny (LOK_PROCDOBR), takze LENIND_SUMKLIC = 1 a prvni
  # podminka "IND_SUMKLIC < 1 - 1 - 0", tj. "0 < 0", nemohla nikdy nastat -
  # uroven EVL se NIKDY nemohla dostat do stavu "spatny". Tabulka 2 pritom
  # spatny stav definuje prave pro pripad, kdy jsou OBA indikatory spatne.
  if (!is.null(cil_chu) && nrow(cil_chu) > 0) {
    n2k_druhy_chu_vypocet <- n2k_druhy_chu_vypocet %>%
      dplyr::left_join(
        cil_chu %>% dplyr::select(kod_chu, DRUH, STAV_CIL),
        by = c("kod_chu", "DRUH")
      ) %>%
      dplyr::group_by(kod_chu, DRUH) %>%
      dplyr::mutate(
        # STAV_PROCDOBR: splnil indikator "% dobre hodnocenych DP" limit min 70?
        STAV_PROCDOBR = {
          v <- STAV_IND[ID_IND == "LOK_PROCDOBR"]
          v <- suppressWarnings(as.numeric(v))
          if (length(v) == 0 || all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
        },
        CELKOVE = dplyr::case_when(
          # oba indikatory zname -> Tabulka 2
          !is.na(STAV_PROCDOBR) & !is.na(STAV_CIL) &
            STAV_PROCDOBR == 1 & STAV_CIL == 1 ~ 1,
          !is.na(STAV_PROCDOBR) & !is.na(STAV_CIL) &
            STAV_PROCDOBR == 1 & STAV_CIL == 0 ~ 0.5,
          !is.na(STAV_PROCDOBR) & !is.na(STAV_CIL) &
            STAV_PROCDOBR == 0 & STAV_CIL == 1 ~ 0.5,
          !is.na(STAV_PROCDOBR) & !is.na(STAV_CIL) &
            STAV_PROCDOBR == 0 & STAV_CIL == 0 ~ 0,
          # Uzemi BEZ evidovaneho ciloveho stavu (napr. cely Lissotriton
          # montandoni): hodnoti se jen podle znameho indikatoru a stav
          # "spatny" nemuze nastat, protoze ten Tabulka 2 vyhrazuje pro
          # selhani OBOU indikatoru. Odpovida to chovani pred harmonizaci.
          # PREDPOKLAD K POTVRZENI - Tabulka 2 pripad chybejiciho ciloveho
          # stavu neresi (viz nalez H-20 v harmonizace_registr.md).
          !is.na(STAV_PROCDOBR) & is.na(STAV_CIL) & STAV_PROCDOBR == 1 ~ 1,
          !is.na(STAV_PROCDOBR) & is.na(STAV_CIL) & STAV_PROCDOBR == 0 ~ 0.5,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-STAV_PROCDOBR)
  }

  # Pomocny sloupec STAV_CIL uz neni potreba - vysledek je v CELKOVE.
  if ("STAV_CIL" %in% names(n2k_druhy_chu_vypocet)) {
    n2k_druhy_chu_vypocet <- n2k_druhy_chu_vypocet %>%
      dplyr::select(-STAV_CIL)
  }
  
  metadata_chu <- n2k_druhy_chu_vypocet %>%
    dplyr::group_by(kod_chu, DRUH) %>%
    dplyr::summarise(
      ROK = toString(unique(ROK)),
      POLE = toString(unique(POLE)),
      NAZEV_LOK = toString(unique(NAZEV_LOK)),
      ID_ND_AKCE = toString(unique(ID_ND_AKCE)),
      ID_ND_LOK = toString(unique(ID_ND_LOK)),
      UROVEN = "chu",
      CILMON_CHU = max(CILMON_CHU, na.rm = TRUE),
      STAV_IND = unique(CELKOVE) %>% max(na.rm = TRUE), 
      HOD_IND = dplyr::case_when(
        STAV_IND == 0 ~ "špatný",
        STAV_IND == 0.5 ~ "zhoršený",
        STAV_IND == 1 ~ "dobrý",
        is.na(STAV_IND) ~ "neznámý"
      )
    ) %>%
    dplyr::mutate(
      ID_IND = "CELKOVE_HODNOCENI",
      KLIC = NA_character_,
      TYP_IND = NA_character_,
      LIM_IND = NA_character_,
      JEDNOTKA = NA_character_,
      LIM_INDLIST = NA_character_
    ) %>%
    dplyr::select(kod_chu, DRUH, ROK, POLE, NAZEV_LOK, ID_ND_AKCE, ID_ND_LOK, ID_IND, HOD_IND, KLIC, UROVEN, TYP_IND, LIM_IND, JEDNOTKA, LIM_INDLIST, STAV_IND, CILMON_CHU)
  
  # Radky s hodnotami druheho indikatoru Tabulky 2, aby byl vysledek dohledatelny
  # ve vystupu a nezustal jen skryty uvnitr CELKOVE. JEDNOTKA nese upozorneni na
  # nesoulad jednotek (nalez S-4) a priznak varovani ze zdroje SDO.
  #
  # PROC SE PRIPOJUJI METADATA UZEMI (nalez H-22): zaver funkce filtruje
  # `is.na(ROK) == FALSE & ROK != "NA"`. Puvodni `transmute()` zadny ROK
  # nevytvarel, takze vsem radkum POP_POCETPRUM3 vysel po `bind_rows()` ROK = NA
  # a tento filtr je bezezbytku zahodil - druhy indikator Tabulky 2 sice
  # ovlivnoval CELKOVE, ale ve vystupu po nem nezustala ani stopa. Stejne by je
  # pozdeji zahodil i `dplyr::filter(CILMON_CHU == 1)` v chu_export()
  # (27_n2k_druhy_zapis.R), ktery na chybejicim CILMON_CHU take vraci NA.
  #
  # Metadata se berou z `metadata_chu`, aby radek nesl TYZ hodnocene obdobi i
  # tytez identifikatory akci jako ostatni radky daneho uzemi. ROK je tedy
  # obdobi hodnoceni uzemi jako celku, NIKOLI vycet let, ze kterych je spocitan
  # klouzavy prumer za posledni 3 roky (ten se odvozuje ve faze 1b v
  # 27_n2k_druhy_zapis.R). `inner_join` je zameny: uzemi, ktere nema v tomto
  # bloku zadny radek, by nemelo k cemu pripojit vysledek a bylo by stejne
  # zahozeno dale v toku dat.
  radky_cil <- NULL
  if (!is.null(cil_chu) && nrow(cil_chu) > 0) {
    radky_cil <- cil_chu %>%
      dplyr::transmute(
        kod_chu, DRUH,
        ID_IND = "POP_POCETPRUM3",
        HOD_IND = as.character(POP_POCETPRUM3),
        LIM_IND = as.character(POP_CILSTAV),
        TYP_IND = "min",
        KLIC = "ano",
        UROVEN = "chu",
        JEDNOTKA = dplyr::case_when(
          is.na(POP_CILSTAV) ~ NA_character_,
          CIL_VAROVANI %in% TRUE ~ "jedinci (cilovy stav SDO - VAROVANI zdroje)",
          TRUE ~ "jedinci (cilovy stav SDO)"
        ),
        LIM_INDLIST = dplyr::if_else(
          is.na(POP_CILSTAV), NA_character_,
          paste("alespoň", POP_CILSTAV, "jedinců (cílový stav území)")
        ),
        STAV_IND = STAV_CIL
      ) %>%
      dplyr::inner_join(
        metadata_chu %>%
          dplyr::select(
            kod_chu, DRUH, ROK, POLE, NAZEV_LOK,
            ID_ND_AKCE, ID_ND_LOK, CILMON_CHU
          ),
        by = c("kod_chu", "DRUH")
      )
  }

  n2k_druhy_chu_final <- dplyr::bind_rows(
    n2k_druhy_chu_vypocet %>%
      dplyr::select(-c(IND_SUMKLIC, LENIND_SUMKLIC, LENIND_NAKLIC, CELKOVE)),
    radky_cil,
    metadata_chu
  )
  
  n2k_druhy_chu_final <- n2k_druhy_chu_final %>%
    dplyr::mutate(
      HOD_IND = dplyr::case_when(
        HOD_IND == "NaN" ~ NA_character_,
        is.na(HOD_IND) == TRUE & ID_IND != "CELKOVE_HODNOCENI" ~ NA_character_,
        TRUE ~ HOD_IND
      )
    ) %>%
    dplyr::mutate(
      STAV_IND = dplyr::case_when(
        UROVEN != "chu" ~ "nehodnocen",
        ID_IND != "CELKOVE_HODNOCENI" & is.na(UROVEN) == TRUE ~ "nehodnocen",
        ID_IND != "CELKOVE_HODNOCENI" & is.na(LIM_IND) == TRUE ~ "nehodnocen",
        is.na(STAV_IND) == TRUE ~ "neznámý",
        is.infinite(STAV_IND) == TRUE ~ "neznámý",
        HOD_IND == " " ~ "neznámý",
        ID_IND == "STA_HABPOKRYV" & is.na(HOD_IND) == TRUE ~ "neznámý",
        is.na(HOD_IND) == TRUE & ID_IND != "CELKOVE_HODNOCENI" ~ "neznámý",
        STAV_IND == 0 ~ "špatný",
        STAV_IND == 0.5 ~ "zhoršený",
        STAV_IND == 1 ~ "dobrý",
        STAV_IND == "0" ~ "špatný",
        STAV_IND == "0.5" ~ "zhoršený",
        STAV_IND == "1" ~ "dobrý",
        TRUE ~ as.character(STAV_IND)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::arrange(DRUH, kod_chu, POLE) %>%
    dplyr::filter(is.na(ROK) == FALSE & ROK != "NA")
  
  return(n2k_druhy_chu_final)
  
}