#----------------------------------------------------------#
# Nalez - priprava indikatoru na urovni nalezu ----- 
#----------------------------------------------------------#
n2k_druhy_pre <- n2k_export %>%
  #dplyr::filter(DRUH == "Epidalea calamita") %>%
  dplyr::rename(
    POLE = POLE_1_RAD
    ) %>% 
  dplyr::mutate(
    # Převedení DRUHu na kategorickou veličinu
    DRUH = as.factor(DRUH),
    # Převedení datumu do vhodného formátu
    DATUM = as.Date(as.character(DATUM_OD), format = '%d.%m.%Y'),
    # Redukce data na den
    DEN = as.numeric(substring(DATUM_OD, 1, 2)),
    # Redukce data na měsíc
    MESIC = as.numeric(substring(DATUM_OD, 4, 5)),
    # Redukce data na rok
    ROK = as.numeric(
      substring(
        DATUM_OD, 
        7, 
        11
        )
      ),
    # Izolace kódu EVL
    kod_chu = substr(
      EVL, 
      1, 
      9
      ),
    # Izolace názvu lokality
    nazev_chu = substr(
      as.character(
        EVL
        ),
      12,
      nchar(
        as.character(
          EVL
          )
        )
      ),
    KOD_LOKRYB = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN,
        "(?<=<naz_tok>).*(?=</naz_tok>)"
        )
      ),
    KOD_LOKAL = dplyr::case_when(
      SKUPINA == "Letouni" ~ LOKALITA,
      SKUPINA == "Mechy" ~ LOKALITA,
      SKUPINA == "Motýli" ~ substring(
        KOD_LOKALITY, 
        1, 
        10
        ),
      SKUPINA == "Ryby a mihule" &
        is.na(KOD_LOKRYB) == FALSE 
      ~ KOD_LOKRYB,
      KOD_LOKALITY == "amp216" ~ "CZ0723412",
      KOD_LOKALITY == "amp222" ~ "CZ0724089_9",
      KOD_LOKALITY == "amp231" ~ "CZ0724089_19",
      KOD_LOKALITY == "amp185" ~ "CZ0623345",
      KOD_LOKALITY == "amp101" ~ "CZ0423006",
      KOD_LOKALITY == "amp71" ~ "CZ0323158",
      KOD_LOKALITY == "amp59" ~ "CZ0323144",
      KOD_LOKALITY == "amp15" ~ "CZ0213790",
      KOD_LOKALITY == "amp102" ~ "CZ0423215",
      KOD_LOKALITY == "amp254" ~ "CZ0813455",
      KOD_LOKALITY == "amp227" ~ "CZ0723410",
      KOD_LOKALITY == "amp336 (CZ_5)" ~ "CZ0714073_5",
      KOD_LOKALITY == "amp205 (CZ_3)" ~ "CZ0714073_3",
      KOD_LOKALITY == "amp337" ~ "CZ0713383",
      KOD_LOKALITY == "amp129" ~ "CZ0523011",
      KOD_LOKALITY == "amp334 (CZ_3)" ~ "CZ0523010_3",
      KOD_LOKALITY == "amp138 (CZ_2)" ~ "CZ0523010_2",
      KOD_LOKALITY == "amp335 (cz_1)" ~ "CZ0523010_1",
      KOD_LOKALITY == "amp102" ~ "CZ0423215",
      KOD_LOKALITY == "amp116" ~ "CZ0513249",
      KOD_LOKALITY == "amp101" ~ "CZ0423006",
      KOD_LOKALITY == "amp15" ~ "CZ0213790",
      KOD_LOKALITY == "amp30" ~ "CZ0213077",
      KOD_LOKALITY == "amp314 (cz_2)" ~ "CZ0213064_2",
      KOD_LOKALITY == "amp316 (cz_3)" ~ "CZ0213064_3",
      KOD_LOKALITY == "amp315 (cz_1)" ~ "CZ0213064_1",
      KOD_LOKALITY == "CZ0213008" ~ "CZ0213008_1",
      KOD_LOKALITY == "amp244" ~ "CZ0813457",
      KOD_LOKALITY == "amp207" ~ "CZ0713385",
      KOD_LOKALITY == "amp64" ~ "CZ0323143",
      KOD_LOKALITY == "amp24" ~ "CZ0213787",
      KOD_LOKALITY == "amp279" ~ "CZ0613335_03",
      KOD_LOKALITY == "amp110" ~ "CZ0513244",
      KOD_LOKALITY == "amp339" ~ "CZ0213066_2",
      KOD_LOKALITY == "amp340" ~ "CZ0213066_1",
      KOD_LOKALITY == "amp27" ~ "CZ0213058",
      KOD_LOKALITY == "amp226" ~ "CZ0724429_3",
      KOD_LOKALITY %in% c("amp211", "amp211" , "amp211", "amp107", "amp99",
                          "amp53", "amp252", "amp281", "amp280", "amp22",
                          "amp81", "amp25", "CZ0813450", "CZ0713397") ~ NA_character_,
      kod_chu == "CZ0623367" ~ "CZ0623367",
      is.na(KOD_LOKALITY) == TRUE ~ kod_chu,
      TRUE ~ NA_character_)
    ) %>%
  dplyr::mutate(
    KOD_LOKAL = dplyr::case_when(
      is.na(KOD_LOKAL) == TRUE ~ KOD_LOKALITY,
      TRUE ~ KOD_LOKAL
      )
    ) %>%
  dplyr::select(
    -KOD_LOKRYB
    ) %>%
  dplyr::filter(
    ROK >= current_year - 12
    ) %>%
  dplyr::mutate(
    KOD_LOKAL = dplyr::case_when(
      is.na(KOD_LOKAL) == TRUE ~ LOKALITA,
      TRUE ~ KOD_LOKAL
      )
    ) %>%
  # identifikace dat cileneho monitoringu
  dplyr::mutate(
    CILMON = dplyr::case_when(
      ZDROJ %in% CIS_CILMON ~ 1,
      PROJEKT == "Monitoring druhů ČR" ~ 1,
      DRUH %in% c(
        "Carabus menetriesi pacholei", 
        "Bolbelasmus unicornis"
        ) 
      ~ 1,
      TRUE ~ 0)
    ) %>%
  dplyr::filter(
    DRUH %in% sites_subjects$nazev_lat & 
      kod_chu %in% sites_subjects$site_code
  ) %>%
  #dplyr::filter(SKUPINA == "Cévnaté rostliny") %>%
  #dplyr::filter(SKUPINA %in% c("Motýli", "Brouci", "Vážky")) %>%
  #dplyr::filter(SKUPINA == "Obojživelníci") %>%
  #dplyr::filter(SKUPINA == "Ryby a mihule") %>%
  filter(SKUPINA %in% c("Letouni", "Savci")) %>%
#--------------------------------------------------#
## Spolecne indikatory ----- 
#--------------------------------------------------#
  dplyr::mutate(
    POP_PRESENCE_N = dplyr::case_when(
      NEGATIVNI == 1 ~ 0,
      POCET == 0 ~ 0,
      NEGATIVNI == 0 ~ 1,
      POCET > 0 ~ 1
      ),
    POP_PRESENCE = dplyr::case_when(
      POP_PRESENCE_N == 1 ~ "ano",
      POP_PRESENCE_N == 0 ~ "ne"
      ),
    POP_POCET = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCET"] ~ POCET,
      TRUE ~ NA_real_
      ),
    POP_POCETSUM = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETSUM"] ~ POCET,
      TRUE ~ NA_real_
      ),
    POP_PLOCHA = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_PLOCHA"] ~ POCET,
      TRUE ~ NA_real_
      ),
    POP_REPRO = dplyr::case_when(
      POP_PRESENCE == "ne" ~ "ne",
      POP_PRESENCE == "ano" & POCITANO %in% limity$JEDNOTKA[limity$ID_IND == "POP_REPRO" & limity$druh == DRUH] ~ "ano",
      POP_PRESENCE == "ano" & POCITANO %in% !limity$JEDNOTKA[limity$ID_IND == "POP_REPRO" & limity$druh == DRUH] ~ "ano",
      TRUE ~ NA_character_
      ),
    POP_POCETNOSTNAL = dplyr::case_when(
      POP_PRESENCE == 0 ~ 0,
      POP_POCET > 1000000 ~ 8,
      REL_POC == "100 001-1 000 000" ~ 8,
      POP_POCET > 100000 ~ 7,
      REL_POC == "10 001-100 000" ~ 7,
      POP_POCET > 10000 ~ 6,
      REL_POC == "1001-10 000" ~ 6,
      POP_POCET > 1000 ~ 5,
      REL_POC == "řádově tisíce" ~ 5,
      REL_POC == "1001-10 000" ~ 5,
      grepl("počet samců: řádově tisíce", POZN_TAX) ~ 5,
      POP_POCET > 100 & POP_POCET <= 1000 ~ 4,
      REL_POC == "řádově stovky" ~ 4,
      REL_POC == "101-1000" ~ 4,
      grepl("počet samců: řádově stovky", POZN_TAX) ~ 4,
      REL_POC == "cca 100" ~ 4,    
      grepl("počet samců: cca 100", POZN_TAX) ~ 4,
      grepl("počet samců: řádově vyšší desítky", POZN_TAX) ~ 3,
      REL_POC == "řádově vyšší desítky" ~ 3,
      POP_POCET > 51 & POP_POCET <= 100 ~ 3,
      REL_POC == "řádově nižší desítky" ~ 2,
      grepl("počet samců: řádově nižší desítky", POZN_TAX) ~ 2,
      REL_POC == "11-100" ~ 3,
      POP_POCET > 10 & POP_POCET < 50 ~ 2,
      POP_POCET > 0 & POP_POCET <= 10 ~ 1,
      REL_POC == "do 10" ~ 1,
      REL_POC == "1-10" ~ 1,
      grepl("počet samců: do 10", POZN_TAX) ~ 1),
    POP_PASTIPOCET = readr::parse_number(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<POP_PASTIPOCET>).*(?=</POP_PASTIPOCET>)"
        )
      ),
    # K DOŘEŠENÍ START !!!!!
    POP_MONPLOCHA = NA, # ze strukturovane poznamky
    POP_ABUNDANCE = POP_POCET/POP_MONPLOCHA,
    # K DOŘEŠENÍ END !!!!!
    # cilova jednotka, k nacteni z ciselniku, k doplneni Martinem
    POP_CILJEDNOTKA = NA,
    POP_KOEFICIENT = dplyr::case_when(
      POP_CILJEDNOTKA == POCITANO ~ 1,
      POP_CILJEDNOTKA == "cm2" & POCITANO == "dm2" ~ 100,
      POP_CILJEDNOTKA == "cm2" & POCITANO == "m2" ~ 10000,
      POP_CILJEDNOTKA == "dm2" & POCITANO == "cm2" ~ 0.01,
      POP_CILJEDNOTKA == "dm2" & POCITANO == "m2" ~ 100,
      POP_CILJEDNOTKA == "m2" & POCITANO == "cm2" ~ 0.0001,
      POP_CILJEDNOTKA == "m2" & POCITANO == "dm2" ~ 0.01),
    vliv = stringr::str_extract(
      STRUKT_POZN,
      "(?<=<vliv>).*(?=</vliv>)"
      )
    ) %>%
  dplyr::mutate(
    VLV_VLIVY = dplyr::case_when(
      is.na(vliv) == FALSE ~ vliv,
      TRUE ~ stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<VLV_VLIVY>).*(?=</VLV_VLIVY>)"
        )
      ),
    VLV_VLIVY_NUM = stringr::str_count(
      STRUKT_POZN,
      ","
      )
    ) %>%
  dplyr::mutate(
    # ------------------------------------------#
    ### Hmyz ----- 
    # ------------------------------------------#
      STA_SECCAS = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<sec_nacasovani>).*(?=</sec_nacasovani>)"
          )
        ),
      STA_SECMET = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<sec_celoplosna>).*(?=</sec_celoplosna>)"
          )
        ),
      STA_PRITOMNOSTROSTLIN = dplyr::case_when(
        ROK < 2021 ~ readr::parse_character(
          stringr::str_extract(
            STRUKT_POZN, 
            "(?<=<sta_pritomnostrostlin>).*(?=</sta_pritomnostrostlin>)"
            )
          ),
        TRUE ~ readr::parse_character(
          stringr::str_extract(
            STRUKT_POZN, 
            "(?<=<STA_PRITOMNOSTROSTLIN>).*(?=</STA_PRITOMNOSTROSTLIN>)"
            )
          )
        ),
      STA_JASANOKOLI = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_JASANOKOLI>).*(?=</STA_JASANOKOLI>)"
          )
        ),
      STA_MIKRPROSTLINA = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_MIKRPROSTLINA>).*(?=</STA_MIKRPROSTLINA>)"
          )
        ),
      STA_VHSTROMN = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_VHSTROMN>).*(?=</STA_VHSTROMN>)"
          )
        ),
      STA_VHSTROMK = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_VHSTROMK>).*(?=</STA_VHSTROMK>)"
          )
        ),
      STA_PERSPEKTIVA = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_PERSPEKTIVA>).*(?=</STA_PERSPEKTIVA>)"
          )
        ),
      STA_MRTDREVO = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_MRTDREVO>).*(?=</STA_MRTDREVO>)"
          )
        ),
      STA_TEKVODA = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_TEKVODA>).*(?=</STA_TEKVODA>)"
          )
        ),
      STA_POKRYVNOSTDREVIN = readr::parse_number(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_POKRYVNOSTDREVIN>).*(?=</STA_POKRYVNOSTDREVIN>)"
          )
        ),
      STA_ZAZEMNENI = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_ZAZEMNENI>).*(?=</STA_ZAZEMNENI>)"
          )
        ),
      STA_SKLADREVO = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_SKLADREVO>).*(?=</STA_SKLADREVO>)"
          )
        ),
      STA_LITVEGET = readr::parse_number(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_LITVEGET>).*(?=</STA_LITVEGET>)"
          )
        ),
      STA_SIRKALIT = readr::parse_number(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_SIRKALIT>).*(?=</STA_SIRKALIT>)"
          )
        ),
      STA_ZASTIN = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN,
          "(?<=<STA_ZASTIN>).*(?=</STA_ZASTIN>)"
          )
        ),
      STA_MAN = dplyr::case_when(STA_SECCAS == 1 &
                                   STA_SECMET == 1 ~ 1,
                                 STA_SECCAS == 0 |
                                   STA_SECMET == 0 ~ 0),
      STA_LIKVIDACE = dplyr::case_when(grepl(paste(c("vysazování lesů", 
                                                     "odvodňování, meliorace",
                                                     "zalesňování bezlesí",
                                                     "změna zemědělského využívání půdy"), 
                                               collapse = "|"), 
                                         STRUKT_POZN, 
                                         ignore.case = TRUE) ~ "zaznamenána",
                                       STA_SECCAS == 0 ~ "zaznamenána",
                                       STA_MAN == 0 & DRUH == "Phengaris teleius" ~ "nezaznamenána",
                                      CILMON == 1 ~ "nezaznamenána"),
      # ------------------------------------------#
      ### Ostatní bezobratlí ----- 
      # ------------------------------------------#
      # ------------------------------------------#
      ### Obojživelníci a plazi ----- 
      # ------------------------------------------#
      STA_STAVVODARYBNIK = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_STAVVODARYBNIK>).*(?=</STA_STAVVODARYBNIK>)"
          )
        ),
      STA_STAVVODALITORAL = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_STAVVODALITORAL>).*(?=</STA_STAVVODALITORAL>)"
          )
        ),
      STA_STAVVODATUNE = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_STAVVODATUNE>).*(?=</STA_STAVVODATUNE>)"
          )
        ),
      STA_STAVVODA = dplyr::case_when(
        is.na(STA_STAVVODATUNE) == FALSE ~ STA_STAVVODATUNE,
        is.na(STA_STAVVODALITORAL) == FALSE ~ STA_STAVVODALITORAL,
        is.na(STA_STAVVODARYBNIK) == FALSE ~ STA_STAVVODARYBNIK),
      STA_ZTRATABIO = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_ZOOPLANKTON>).*(?=</STA_ZOOPLANKTON>)"
          )
        ),
      STA_KACHNAPRITOMNOST = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_KACHNAPRITOMNOST>).*(?=</STA_KACHNAPRITOMNOST>)"
          )
        ),
      STA_RYBY = dplyr::case_when(
        grepl(
          "akvakultur", 
          STRUKT_POZN, 
          ignore.case = TRUE) | 
          grepl("rybolov", 
                STRUKT_POZN, 
                ignore.case = TRUE
                ) 
        ~ "ano",
        TRUE ~ "ne"),
      STA_ZOOPLANKTON = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_ZOOPLANKTON>).*(?=</STA_ZOOPLANKTON>)"
          )
        ),
      STA_MANIPULACE = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_MANIPULACE>).*(?=</STA_MANIPULACE>)"
          )
        ),
      STA_POKRVEGETACE = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_POKRVEGETACE>).*(?=</STA_POKRVEGETACE>)"
          )
        ),
      STA_PRUHLEDNOSTVODA = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_PRUHLEDNOSTVODA>).*(?=</STA_PRUHLEDNOSTVODA>)"
          )
        ),
      STA_PRUHLEDNOSTVODAT = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_PRUHLEDNOSTVODAT>).*(?=</STA_PRUHLEDNOSTVODAT>)"
          )
        ),
      STA_PRUHLEDNOSTVODA = dplyr::case_when(
        is.na(STA_PRUHLEDNOSTVODAT) == FALSE ~ STA_PRUHLEDNOSTVODAT,
        TRUE ~ STA_PRUHLEDNOSTVODA),
      STA_UHYNOBOJZIVELNIK = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_UHYNOBOJZIVELNIK>).*(?=</STA_UHYNOBOJZIVELNIK>)"
          )
        ),
      STA_ZASTINENIHLADINA = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_ZASTINENIHLADINA>).*(?=</STA_ZASTINENIHLADINA>)"
          )
        ),
      STA_ZASTINENILITORAL = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_ZASTINENILITORAL>).*(?=</STA_ZASTINENILITORAL>)"
          )
        ),
      STA_ZASTINENIHLADINA = dplyr::case_when(
        STA_ZASTINENIHLADINA <= STA_ZASTINENILITORAL ~ STA_ZASTINENILITORAL,
        TRUE ~ STA_ZASTINENIHLADINA),
      # ------------------------------------------#
      ### Ryby a mihule ----- 
      # ------------------------------------------#
      POP_DELKYJEDINCI = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<velikosti>).*(?=</velikosti>)"
        )
      ),
      # ------------------------------------------#
      ### Savci ----- 
      # ------------------------------------------#
      POP_PRESENCE_ZIMNI = max(
        POP_PRESENCE[(ROK == ROK & 
                        MESIC < 5) | 
                       (ROK == ROK - 1 & 
                          MESIC > 9)],
        na.rm = TRUE
        ),
      POP_PRESENCE_LETNI = max(
        POP_PRESENCE[(ROK == ROK &
                        MESIC >= 5 &
                        MESIC <= 9)],
        na.rm = TRUE),
      # ------------------------------------------#
      ### Mechorosty ----- 
      # ------------------------------------------#
      POP_POCETMIKROLOK = readr::parse_number(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<pocet_ml>).*(?=</pocet_ml>)"
          )
        ),
      POP_TRENDBRY = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<trend_vyvoj>).*(?=</trend_vyvoj>)"
          )
        ),
      POP_ZMENABRY1 = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<SP_POSKOZENI_ROSTLIN>).*(?=</SP_POSKOZENI_ROSTLIN>)"
          )
        ),
      POP_ZMENABRY2 = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<str_strom>).*(?=</str_strom>)"
          )
        ),
      POP_PLOCHAPOP = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<str_strom>).*(?=</str_strom>)"
          )
        ),
      STA_DREVOBRY = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<SUB>).*(?=</SUB>)"
          )
        ),
      STA_STRUKTVEKBRY = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<str_strom>).*(?=</str_strom>)"
          )
        ),
      STA_STRUKTDRUBRY = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<druh_strom>).*(?=</druh_strom>)"
          )
        ),
      # ------------------------------------------#
      ### Cévnaté rostliny ----- 
      # ------------------------------------------#
      POP_POCETLODYH = dplyr::case_when(
        POP_PRESENCE == "ne" ~ 0,
        POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETSUMLOD"] ~ POCET,
        TRUE ~ NA_real_),
      POP_POCETVITAL = dplyr::case_when(
        POP_PRESENCE == "ne" ~ 0,
        POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_VITAL"] ~ POCET,
        TRUE ~ NA_real_),
      STA_MAN = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<MAN>).*(?=</MAN>)"
          )
        ),
      STA_MANPOTREBAVLIV = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<MAN_POTREBAVLIV>).*(?=</MAN_POTREBAVLIV>)"
          )
        ),
      STA_MANPOTREBA = NA,
      STA_MANVLIV = NA,
      STA_POKRYVNOSTINVAZNI = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_POKRYVNOSTINVAZNI>).*(?=</STA_POKRYVNOSTINVAZNI>)"
          )
        ),
      STA_POKRYVNOSTEXPANZNI = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<STA_POKRYVNOSTEXPANZNI>).*(?=</STA_POKRYVNOSTEXPANZNI>)"
          )
        ),
      STA_POKRSTAR = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTSTARINA>).*(?=</STA_POKRYVNOSTSTARINA>)")),
      STA_POKRDREV = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTDREVIN>).*(?=</STA_POKRYVNOSTDREVIN>)")),
      STA_POKRDREVNIZ = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTDREVINNIZ>).*(?=</STA_POKRYVNOSTDREVINNIZ>)")),
      STA_POKRYVNOSTE2E3 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTE2E3>).*(?=</STA_POKRYVNOSTE2E3>)")),
      STA_POKRYVNOSTE1 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTE1>).*(?=</STA_POKRYVNOSTE1>)")),
      STA_POKRYVNOSTE0 = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTE0>).*(?=</STA_POKRYVNOSTE0>)")),
      STA_POKRYVNOSTVOLNAPUDA = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_POKRYVNOSTVOLNAPUDA>).*(?=</STA_POKRYVNOSTVOLNAPUDA>)")),
      STA_ZAPOJENICELK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZAPOJENICELK>).*(?=</STA_ZAPOJENICELK>)")),
      STA_ZAPOJENIKAT = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZAPOJENICELK>).*(?=:)")),
      STA_STUPENZACH = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<SP_STUPEN_ZACH_STAN>).*(?=</SP_STUPEN_ZACH_STAN>)")),
      STA_HYDRPOMERY = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_HYDRPOMERY>).*(?=</STA_HYDRPOMERY>)")),
      STA_VLHPOM = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_VLHKOPOMERY>).*(?=</STA_VLHKOPOMERY>)")),
      STA_ZASTINENICELK = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZASTINENICELK>).*(?=</STA_ZASTINENICELK>)")),
      SP_POSKOZENI_ROSTLIN = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<SP_POSKOZENI_ROSTLIN>).*(?=</SP_POSKOZENI_ROSTLIN>)")),
      POSKOZENI_ROSTLIN = dplyr::case_when(
        is.na(SP_POSKOZENI_ROSTLIN) ~ NA_character_,
        TRUE ~ paste0(
          SP_POSKOZENI_ROSTLIN, ";"
          )
        ),
      POSKOZENI_ROSTLIN_KAT = POSKOZENI_ROSTLIN %>%
        str_replace_all(., "Nezjištěno", "0") %>%
        str_replace_all(., "Bez poškození", "0") %>%
        str_replace_all(., "Bez poškození.", "0") %>%
        str_replace_all(., "1-10 %", "1") %>%
        str_replace_all(., "10 %", "1") %>%
        str_replace_all(., "11-50 %", "2") %>%
        str_replace_all(., "10-50 %", "2") %>%
        str_replace_all(., "51-100 %", "3") %>%
        str_replace_all(., "50-100 %", "3") %>%
        str_replace_all(., "100 %", "3")
      ) %>%
  dplyr::distinct() %>%
  dplyr::group_by(
    IDX_ND_AKCE, 
    DRUH) %>%
  dplyr::mutate(
    POP_POCETSUM = sum(
      POP_POCETSUM, 
      na.rm = TRUE
      ),
    POP_POCET = dplyr::case_when(
      is.na(POP_POCETSUM) == FALSE ~ POP_POCET,
      POP_POCETSUM > POP_POCET ~ POP_POCETSUM,
      NA ~ POP_POCET),
    POP_POCETKONCPAST = dplyr::case_when(
      is.na(POP_PASTIPOCET) == TRUE ~ NA_real_,
      is.infinite(POP_PASTIPOCET) == TRUE ~ NA_real_,
      TRUE ~ POP_POCET/POP_PASTIPOCET
      )
    ) %>%
  dplyr::mutate(
    POP_POCETMIN = dplyr::case_when(
      is.na(POP_POCET) == FALSE ~ POP_POCET,
      POP_POCETNOSTNAL == 5 ~ 1001,
      POP_POCETNOSTNAL == 4 ~ 100,
      POP_POCETNOSTNAL == 3 ~ 10000,
      POP_POCETNOSTNAL == 2 ~ 10000,
      POP_POCETNOSTNAL == 1 ~ 10000
      ),
    POP_POCETMAX = dplyr::case_when(
      is.na(POP_POCET) == FALSE ~ POP_POCET,
      POP_POCETNOSTNAL == 5 ~ 10000,
      POP_POCETNOSTNAL == 4 ~ 1000,
      POP_POCETNOSTNAL == 3 ~ 10000,
      POP_POCETNOSTNAL == 2 ~ 10000,
      POP_POCETNOSTNAL == 1 ~ 10000
      ),
    POP_POCETLODYHSUM = sum(
      POP_POCETLODYH, 
      na.rm = TRUE
      ),
    POP_POCETPLOD = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & 
                                      limity$ID_IND %in% "POP_POCETSUMLOD"] ~ POCET,
      TRUE ~ NA_real_
      ),
    POP_VITAL = POP_POCETVITAL/POP_POCET*100) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    ID_ND_NALEZ
    ) %>%
  tidyr::separate_rows(
    POSKOZENI_ROSTLIN, 
    sep = "; "
    ) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    POP_POSKPOC = dplyr::case_when(is.na(SP_POSKOZENI_ROSTLIN) == TRUE ~ NA,
                                   SP_POSKOZENI_ROSTLIN == "Nezjištěno" ~ 0,
                                   TRUE ~ sum(!is.na(POSKOZENI_ROSTLIN) & 
                                                POSKOZENI_ROSTLIN != "Nezjištěno")),
    POCET_POSKOZENYCH = readr::parse_number(POSKOZENI_ROSTLIN),
    POCET_POSKOZENYCHSUM = sum(POCET_POSKOZENYCH, na.rm = TRUE),
    PROCENTO_POSKOZENYCH = POCET_POSKOZENYCHSUM/unique(POP_POCETLODYHSUM),
    POSKOZENI_KAT = dplyr::case_when(
      grepl("Nezjištěno", POSKOZENI_ROSTLIN) ~ 0,
      grepl("1-10 %", POSKOZENI_ROSTLIN) ~ 1,
      grepl("10 %", POSKOZENI_ROSTLIN) ~ 1,
      grepl("11-50 %", POSKOZENI_ROSTLIN) ~ 2,
      grepl("10-50 %", POSKOZENI_ROSTLIN) ~ 2,
      grepl("51-100 %", POSKOZENI_ROSTLIN) ~ 3,
      grepl("50-100 %", POSKOZENI_ROSTLIN) ~ 3,
      grepl("100 %", POSKOZENI_ROSTLIN) ~ 3,
      TRUE ~ NA_integer_
      ),
    POSKOZENI_LODYHKAT = dplyr::case_when(
      PROCENTO_POSKOZENYCH == 0 ~ 0,
      PROCENTO_POSKOZENYCH > 0 & PROCENTO_POSKOZENYCH <= 0.1 ~ 1,
      PROCENTO_POSKOZENYCH > 0.1 & PROCENTO_POSKOZENYCH <= 0.5 ~ 2,
      PROCENTO_POSKOZENYCH > 0.5 ~ 3,
      TRUE ~ NA_integer_
      ),
    POP_POSKPERC = dplyr::case_when(
      DRUH == "Adenophora liliifolia" ~ unique(POSKOZENI_LODYHKAT),
      TRUE ~ sum(
        POSKOZENI_KAT, 
        na.rm = TRUE
        )
      )
    ) %>%
  dplyr::ungroup() %>%
  tidyr::separate_rows(
    STA_PRUHLEDNOSTVODA, 
    sep = ","
    ) %>%
  tidyr::separate_rows(
    STA_ZAPOJENICELK, 
    sep = ","
    ) %>%
  dplyr::mutate(
    STA_ZAPOJENIDRN = dplyr::case_when(
      nchar(STA_ZAPOJENICELK) > 15 ~ str_match(STA_ZAPOJENICELK, 
                                               "zapojený::\\s*(.*?)\\s*%")[,2],
      TRUE ~ "0"),
    STA_ZAPOJENIDRN = readr::parse_number(
      STA_ZAPOJENICELK
      )
    ) %>%
  tidyr::separate_rows(
    POP_DELKYJEDINCI,
    sep = ","
  ) %>%
  dplyr::mutate(
    POP_DELKYJEDINCINUM = as.numeric(
      POP_DELKYJEDINCI
      )
    ) %>%
  fuzzyjoin::fuzzy_left_join(
    .,
    cis_ryby_delky,
    by = c(
      "DRUH" = "DRUH",                  # exact
      "POP_DELKYJEDINCINUM" = "MIN",    # >=
      "POP_DELKYJEDINCINUM" = "MAX"     # <=
    ),
    match_fun = list(`==`, `>=`, `<=`)
  ) %>%
  # drop only the lookup columns, keep original POP_DELKYJEDINCI
  dplyr::select(
    -DRUH.y,
    -MIN, 
    -MAX
    ) %>%
  rename(
    DRUH = DRUH.x,
    POP_DELKYJEDINCIKAT = KAT
    ) %>%
  dplyr::distinct() 

#----------------------------------------------------------#
# Lokalita - priprava indikatoru na urovni lokality ----- 
#----------------------------------------------------------#
n2k_druhy_lokpop <- n2k_druhy_pre %>%
  dplyr::select(-c(ZDROJ:PRESNOST), SKUPINA) %>%
  dplyr::group_by(
    KOD_LOKAL, 
    ROK, 
    DRUH
    ) %>%
  dplyr::reframe(
    # ------------------------------------------#
    ### Společné indikátory ----- 
    # ------------------------------------------#
    CELKOVE = NA,
    POP_POCETSUMLOKAL = sum(
      POP_POCET, 
      na.rm = TRUE
      ),
    POP_POCETMIN = min(
      POP_POCET, 
      na.rm = TRUE
      ), 
    POP_POCETMAX = max(
      POP_POCET, 
      na.rm = TRUE
      ), 
    POP_POCETMAX = ifelse(is.infinite(POP_POCETMAX), 0, POP_POCETMAX),
    POP_POCETNOST = max(
      POP_POCETNOSTNAL,
      na.rm = TRUE
    ),
    POP_POCETNOSTMAX = NA,
    #POP_POCETSUM = sum(POP_POCET, na.rm = TRUE),
    #CILMON = max(CILMON, na.rm = TRUE),
    # ------------------------------------------#
    ### Hmyz ----- 
    # ------------------------------------------#
    # ------------------------------------------#
    ### Ostatní bezobratlí ----- 
    # ------------------------------------------#
    # ------------------------------------------#
    ### Obojživelníci a plazi ----- 
    # ------------------------------------------#
    # ------------------------------------------#
    ### Ryby a mihule ----- 
    # ------------------------------------------#
    POP_ABUNDANCE = NA,
    POP_ABUNDANCEREF = NA, # 
    POP_DYN = NA,
    POP_VITALITA = dplyr::n_distinct(
      POP_DELKYJEDINCIKAT, 
      na.rm = TRUE
      ),
    # ------------------------------------------#
    ### Savci ----- 
    # ------------------------------------------#
    POP_POCETZIM = max(
      POP_POCET[(ROK == current_year & 
                   MESIC < 5) |
                  (ROK == current_year - 1 & 
                     MESIC > 9)
                ],
      na.rm = TRUE
      ), 
    POP_POCETZIM1 = max(
      POP_POCET[(ROK == current_year - 1 & 
                   MESIC < 5) |
                  (ROK == current_year - 2 &
                     MESIC > 9)
                ],
      na.rm = TRUE
      ),
    POP_POCETZIM2 = max(
      POP_POCET[(ROK == current_year - 2 &
                   MESIC < 5) |
                  (ROK == current_year - 3 &
                     MESIC > 9)
                ],
      na.rm = TRUE
      ),
    POP_POCETZIM3 = max(
      POP_POCET[(ROK == current_year - 3 & 
                   MESIC < 5) |
                  (ROK == current_year - 4 &
                     MESIC > 9)], 
      na.rm = TRUE
      ),
    POP_POCETZIMREF = mean(
      POP_POCETZIM1, 
      POP_POCETZIM2, 
      POP_POCETZIM3, 
      na.rm = TRUE
      ),
    POP_VITALZIM = POP_POCETZIM/POP_POCETZIMREF,
    POP_POCETLETS1 = max(
      POP_POCET[(ROK == current_year & 
                   ((MESIC == 5 & 
                       DEN >= 15) |
                      (MESIC == 6 & 
                         DEN <= 15)
                    )
                 )
                ],
      na.rm = TRUE),
    POP_POCETLETS2 = max(
      POP_POCET[(ROK == current_year &
                   (
                     (MESIC == 6 & 
                       DEN > 15
                      ) | 
                       (
                         MESIC %in% c(7, 8, 9)
                        )
                     )
                 )
                ], 
      na.rm = TRUE),
    POP_POCETLET = max(
      POP_POCET[(ROK == current_year & 
                   MESIC >= 5 & 
                   MESIC <= 9)], 
      na.rm = TRUE
      ), 
    POP_POCETLET1 = max(
      POP_POCET[(ROK == current_year - 1 & 
                   MESIC >= 5 & 
                   MESIC <= 9)], 
      na.rm = TRUE
      ), 
    POP_POCETLET2 = max(POP_POCET[(ROK == current_year - 2 & 
                                 MESIC >= 5 & 
                                 MESIC <= 9)], 
                        na.rm = TRUE), 
    POP_POCETLET3 = max(POP_POCET[(ROK == current_year - 3 & 
                                 MESIC >= 5 & 
                                 MESIC <= 9)], 
                        na.rm = TRUE), 
    POP_POCETLETREF = mean(
      POP_POCETLET1, 
      POP_POCETLET2, 
      POP_POCETLET3, 
      na.rm = TRUE
      ),
    POP_VITALLET = POP_POCETLET/POP_POCETLETREF,
    POP_REPROCHI = POP_POCETLETS2/POP_POCETLETS1,
  ) %>%
  dplyr::ungroup()

# populacni trendy odvozene od posledniho pozorovani POP_POCETMAX[1]
n2k_druhy_lokpop_trend <- n2k_druhy_lokpop %>%
  dplyr::group_by(
    KOD_LOKAL, 
    DRUH
    ) %>%
  # serazeni sestupne podle roku
  dplyr::arrange(
    desc(ROK)
    ) %>%
  #dplyr::filter(CILMON == 1 & is.na(POP_POCETMAX) == FALSE & is.infinite(POP_POCETMAX) == FALSE) %>%
  dplyr::reframe(
    POP_POCETMAXREF = POP_POCETMAX[3],
    POP_TREND1 = dplyr::case_when(
      POP_POCETMAX[1] >= POP_POCETMAXREF ~ 1,
      POP_POCETMAX[1] < POP_POCETMAXREF ~ 0
      ),
    POP_TREND2 = dplyr::case_when(
      POP_POCETMAX[2] >= POP_POCETMAXREF ~ 1,
      POP_POCETMAX[2] < POP_POCETMAXREF ~ 0
      ),
    POP_TREND = sum(
      POP_TREND1, 
      POP_TREND2, 
      na.rm = TRUE
      ),
    POP_TRENDLM = coef(
      lm(
        POP_POCETMAX ~ ROK
        )
      )[2],
    POP_POCETNOSTMAX = max(
      POP_POCETNOST, 
      na.rm = TRUE
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    cis_pocet_kat,
    by = "POP_POCETNOSTMAX"
    ) %>%
  dplyr::distinct()
  

#--------------------------------------------------#
## Kompilace konecne tabulky vsech indikatoru ----- 
#--------------------------------------------------#
n2k_druhy <- n2k_druhy_pre %>%
  dplyr::left_join(
    ., 
    n2k_druhy_lokpop,
    by = join_by(
      ROK, 
      KOD_LOKAL,
      DRUH
      )
    ) %>%
  dplyr::left_join(
    ., 
    n2k_druhy_lokpop_trend, 
    by = join_by(
      KOD_LOKAL, 
      DRUH
      )
    ) %>%
  dplyr::distinct() 

#--------------------------------------------------#
## Odstraneni dilcich objektu z pameti ----- 
#--------------------------------------------------#
rm(
  n2k_druhy_pre, 
  n2k_druhy_lokpop, 
  n2k_druhy_lokpop_trend
  )

#----------------------------------------------------------#
# Prevod na long format a napojeni na limity ----- 
#----------------------------------------------------------#
n2k_druhy_long <- n2k_druhy %>%
  dplyr::mutate(
    across(
      .cols = ncol_orig:ncol(.),
      .fns = ~ as.character(.)
      )
    ) %>%
  tidyr::pivot_longer(
    .,
    cols = c(
      (ncol_orig+9):ncol(.)
      ),
    names_to = "ID_IND",
    values_to = "HOD_IND"
    ) %>%
  dplyr::select(
    -c(ZDROJ:PRESNOST)
    ) %>%
  dplyr::right_join(
    .,
    limity %>%
      dplyr::filter(
        UROVEN == "lok"
        ) %>%
      dplyr::filter(
        is.na(LIM_IND) == FALSE
        ),
    by = c("DRUH" = "DRUH",
           "ID_IND" = "ID_IND")
    ) %>%
  dplyr::mutate(
    IND_GRP = dplyr::case_when(
      TYP_IND %in% c(
        "min", 
        "max"
        )
      ~ "minmax",
      TRUE ~ TYP_IND
      )
    ) 

# ------------------------------------------#
# Porovnani s limity ----- 
# ------------------------------------------#
n2k_druhy_lim_pre <- n2k_druhy_long %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      TYP_IND == "min" & as.numeric(HOD_IND) < as.numeric(LIM_IND) ~ 0,
      TYP_IND == "min" & as.numeric(HOD_IND) >= as.numeric(LIM_IND) ~ 1,
      TYP_IND == "max" & as.numeric(HOD_IND) > as.numeric(LIM_IND) ~ 0,
      TYP_IND == "max" & as.numeric(HOD_IND) <= as.numeric(LIM_IND) ~ 1,
      TYP_IND == "val" & HOD_IND != LIM_IND ~ 0,
      TYP_IND == "val" & HOD_IND == LIM_IND ~ 1
      )
    ) %>%
  dplyr::group_by(
    ID_ND_NALEZ, 
    ID_IND, 
    IND_GRP
    ) %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      IND_GRP == "minmax" & grepl("POP_", ID_IND) == TRUE ~ max(as.numeric(STAV_IND), na.rm = TRUE),
      IND_GRP == "minmax" & grepl("POP_", ID_IND) == FALSE ~ min(as.numeric(STAV_IND), na.rm = TRUE),
      IND_GRP == "val" ~ max(as.numeric(STAV_IND), 
                             na.rm = TRUE)
      )
    ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    STAV_IND = dplyr::case_when(
      is.infinite(STAV_IND) ~ NA,
      TRUE ~ STAV_IND
      )
    ) %>%
  dplyr::group_by(
    ID_ND_NALEZ, 
    ID_IND
    ) %>%
  dplyr::arrange(
    dplyr::desc(
      STAV_IND
      )
    ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# ------------------------------------------#
# Hodnoceni nalezu ----- 
# ------------------------------------------#
n2k_druhy_lim <- n2k_druhy_lim_pre %>%
  dplyr::group_by(ID_ND_NALEZ) %>%
  dplyr::mutate(
    CELKOVE_HODNOCENI = as.character(
      sum(
        STAV_IND, 
        na.rm = TRUE)
      )
    ) %>%
  dplyr::select(-c(ID_IND:IND_GRP)) %>%
  tidyr::pivot_longer(
    .,
    cols = ncol(.),
    names_to = "ID_IND",
    values_to = "HOD_IND"
    ) %>%
  dplyr::distinct() %>%
  dplyr::bind_rows(
    ., 
    n2k_druhy_lim_pre
    ) %>%
  dplyr::arrange(ID_ND_NALEZ)

ncol_druhy_lim <- 
  ncol(
    n2k_druhy_lim
    )

#----------------------------------------------------------#
# Zapis dat -----
#----------------------------------------------------------#

nal_export <-
  function() {
    
    n2k_druhy_lim_write <-
      n2k_druhy_lim %>%
      dplyr::left_join(
        ., 
        evl %>%
          sf::st_drop_geometry() %>%
          dplyr::select(
            SITECODE, 
            NAZEV
          ),
        by = c(
          "kod_chu" = "SITECODE"
        )
      ) %>%
      dplyr::left_join(
        .,
        rp_code,
        by = join_by(
          "kod_chu"
        )
      ) %>%
      dplyr::left_join(
        .,
        n2k_oop,
        by = c("kod_chu" = "SITECODE")
      )
    
    sep_isop <- ";"
    quote_env_isop <- FALSE
    encoding_isop <- "UTF-8"
    
    sep <- ","
    quote_env <- TRUE
    encoding <- "Windows-1250"
    
    write.table(
      n2k_druhy_lim_write,
      paste0("Outputs/Data/",
             "n2k_druhy_nal",
             "_",
             current_year,
             "_",
             gsub(
               "-", 
               "", 
               Sys.Date()
             ),
             "_",
             encoding,
             ".csv"
      ),
      row.names = FALSE,
      sep = sep,
      quote = quote_env,
      fileEncoding = encoding
    )  
    
    write.table(
      n2k_druhy_lim_write,
      paste0("Outputs/Data/",
             "n2k_druhy_nal",
             "_",
             current_year,
             "_",
             gsub(
               "-", 
               "", 
               Sys.Date()
             ),
             "_",
             encoding_isop,
             ".csv"
      ),
      row.names = FALSE,
      sep = sep_isop,
      quote = quote_env_isop,
      fileEncoding = encoding_isop
    )  
    
  }

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#