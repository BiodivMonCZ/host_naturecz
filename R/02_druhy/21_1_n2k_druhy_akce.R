run_n2k_druhy <- function(
    n2k_load,
    species_name,
    sites_subjects,
    limity,
    current_year = 2024
) {
  
  # Kontrola limitu pro pocet (POP_POCET)
  # Pokud limit pro dany druh neexistuje, vypise se varovani a POP_POCET bude NA.
  # V opacnem pripade se vypise potvrzeni, ze limit byl nalezen.
  lim_pocet <- limity %>% filter(DRUH == species_name & ID_IND == "POP_POCET") %>% pull(JEDNOTKA) %>% unique() %>% stringr::str_squish()
  if (length(lim_pocet) == 0) {
    warning(glue::glue("No 'POP_POCET' limit for species — POP_POCET will be NA for all observations."))
  } else {
    warning(glue::glue("'POP_POCET' limit for species found"))
  }
  
  # Kontrola limitu pro sumu poctu (POP_POCETSUM)
  # Stejna logika jako vyse - kontrola existence limitu a varovani pri absenci.
  lim_pocetsum <- limity %>% filter(DRUH == species_name & ID_IND == "POP_POCETSUM") %>% pull(JEDNOTKA) %>% unique() %>% stringr::str_squish()
  if (length(lim_pocetsum) == 0) {
    warning(glue::glue("No 'POP_POCETSUM' limit for species — POP_POCETSUM will be NA for all observations."))
  } else {
    warning(glue::glue("'POP_POCETSUM' limit for species found"))
  }
  
  # Kontrola limitu pro reprodukci (POP_REPRO)
  # Stejna logika jako vyse - kontrola existence limitu a varovani pri absenci.
  lim_repro <- limity %>% filter(DRUH == species_name & ID_IND == "POP_REPRO") %>% pull(JEDNOTKA) %>% unique() %>% stringr::str_squish()
  if (length(lim_repro) == 0) {
    warning(glue::glue("No 'POP_REPRO' limit for species — POP_REPRO will be NA for all observations."))
  } else {
    warning(glue::glue("'POP_REPRO' limit for species found"))
  }
  
  #----------------------------------------------------------#
  # Nalez - priprava indikatoru na urovni nalezu ----- 
  #----------------------------------------------------------#
  n2k_druhy_pre <- n2k_load %>%
    # Filtrace dat: Ponechame pouze zaznamy z poslednich 12 let (vcetne aktualniho)
    dplyr::filter(
      ROK >= current_year - 12
    ) %>%
    # Filtrace dat: Ponechame pouze kombinace Druh a Lokalita, ktere jsou v seznamu sledovanych predmetu ochrany
    dplyr::filter(
      DRUH %in% sites_subjects$nazev_lat & 
        kod_chu %in% sites_subjects$site_code
    ) %>%
    #dplyr::filter(SKUPINA == "Cévnaté rostliny") %>%
    #dplyr::filter(SKUPINA %in% c("Motýli", "Brouci", "Vážky")) %>%
    # Filtrace dat: Ponechame pouze zaznamy pro konkretni druh, ktery aktualne zpracovavame
    dplyr::filter(DRUH == species_name) %>%
    #dplyr::filter(SKUPINA == "Ryby a mihule") %>%
    #filter(SKUPINA == "Savci") %>%
    #filter(SKUPINA == "Letouni") %>%
    #--------------------------------------------------#
    ## Populacni indikatory ----- 
  #--------------------------------------------------#
  dplyr::mutate(
    # POP_PRESENCE_N: Cislo reprezentujici pritomnost (1 = ano, 0 = ne)
    # Odvozuje se z priznaku NEGATIVNI a hodnoty POCET
    POCITANO_CLEAN = stringr::str_squish(as.character(POCITANO)),
    POP_PRESENCE_N = dplyr::case_when(
      NEGATIVNI == 1 ~ 0,
      POCET == 0 ~ 0,
      NEGATIVNI == 0 ~ 1,
      POCET > 0 ~ 1
    ),
    # POP_PRESENCE: Textova reprezentace pritomnosti ("ano"/"ne") na zaklade ciselne hodnoty
    POP_PRESENCE = dplyr::case_when(
      POP_PRESENCE_N == 1 ~ "ano",
      POP_PRESENCE_N == 0 ~ "ne"
    ),
    # POP_POCET: Hodnota poctu pro dany zaznam
    # Pokud neni pritomen -> 0.
    # Pokud jednotka (POCITANO_CLEAN) odpovida definovanym limitum pro POP_POCET -> hodnota POCET.
    # Jinak NA (pokud jednotka neodpovida metodice limitu).
    POP_POCET = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_pocet ~ POCET,
      TRUE ~ NA_real_
    ),
    POP_RELPOC = dplyr::case_when(
      POP_PRESENCE == "ne" ~ "0",
      POCITANO_CLEAN %in% lim_pocet ~ REL_POC,
      TRUE ~ NA_character_
    ),
    # POP_POCETSUM_PART: Castice pro scitani (napr. samci, samice), ktere se maji secist za celou akci
    # Pouzivame docasny nazev pro prehlednost, aby se nepletl s vyslednou sumou
    # Logika stejna jako u POP_POCET, ale kontroluje se shoda s jednotkami pro POP_POCETSUM.
    POP_POCETSUM_PART = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_pocetsum ~ POCET,
      TRUE ~ NA_real_
    ),
    # POP_POCETSUM: Inicializace promenne pro sumu
    # Zatim obsahuje jen hodnotu pro dany radek (pokud odpovida limitu), pozdeji bude agregovana.
    POP_POCETSUM = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_pocetsum ~ POCET,
      TRUE ~ NA_real_
    ),
    # POP_PLOCHA: Hodnota populace vyjadrena plochou
    # Pouze pokud jednotka (POCITANO_CLEAN) odpovida limitum pro POP_PLOCHA.
    POP_PLOCHA = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_PLOCHA"] ~ POCET,
      TRUE ~ NA_real_
    ),
    # POP_REPRO: Indikator reprodukce ("ano"/"ne")
    # Pokud je druh pritomen a jednotka odpovida limitum pro reprodukci -> "ano".
    POP_REPRO = dplyr::case_when(
      POP_PRESENCE == "ne" ~ "ne",
      POP_PRESENCE == "ano" & POCITANO_CLEAN %in% lim_repro ~ "ano",
      TRUE ~ NA_character_
    ),
    # POP_REPRONUM: Ciselna verze reprodukce (1 = ano, 0 = ne) pro snadnejsi vypocty
    POP_REPRONUM = dplyr::case_when(
      POP_REPRO == "ne" ~ 0L,
      POP_REPRO == "ano" ~ 1L,
      TRUE ~ NA_integer_
    ) %>% as.integer(),
    # POP_POCETNOSTNAL: Kategorizace pocetnosti do skaly 1-8
    # Prevadi ruzne textove (POP_RELPOC, POZN_TAX) a ciselne (POP_POCET) udaje na jednotnou skalu.
    # Napr. "radove stovky" -> kategorie 4.
    POP_POCETNOSTNAL = dplyr::case_when(
      POP_PRESENCE_N == 0 ~ 0,
      POP_POCET > 1000000 ~ 8,
      POP_RELPOC == "100 001-1 000 000" ~ 7,
      POP_POCET > 100000 ~ 7,
      POP_RELPOC == "10 001-100 000" ~ 6,
      POP_POCET > 10000 ~ 6,
      POP_POCET > 1000 ~ 5,
      POP_RELPOC == "řádově tisíce" ~ 5,
      POP_RELPOC == "1001-10 000" ~ 5,
      grepl("počet samců: řádově tisíce", POZN_TAX) ~ 5,
      POP_POCET > 100 & POP_POCET <= 1000 ~ 4,
      POP_RELPOC == "řádově stovky" ~ 4,
      POP_RELPOC == "101-1000" ~ 4,
      grepl("počet samců: řádově stovky", POZN_TAX) ~ 4,
      POP_RELPOC == "cca 100" ~ 4,    
      grepl("počet samců: cca 100", POZN_TAX) ~ 4,
      grepl("počet samců: řádově vyšší desítky", POZN_TAX) ~ 3,
      POP_RELPOC == "řádově vyšší desítky" ~ 3,
      POP_POCET > 51 & POP_POCET <= 100 ~ 3,
      POP_RELPOC == "řádově nižší desítky" ~ 2,
      grepl("počet samců: řádově nižší desítky", POZN_TAX) ~ 2,
      POP_RELPOC == "11-100" ~ 3,
      POP_POCET > 10 & POP_POCET < 50 ~ 2,
      POP_POCET > 0 & POP_POCET <= 10 ~ 1,
      POP_RELPOC == "do 10" ~ 1,
      POP_RELPOC == "1-10" ~ 1,
      grepl("počet samců: do 10", POZN_TAX) ~ 1
      ),
    # POP_PASTIPOCET: Extrakce poctu pasti ze strukturovane poznamky (XML tag)
    POP_PASTIPOCET = readr::parse_number(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<POP_PASTIPOCET>).*(?=</POP_PASTIPOCET>)"
      )
    ),
    # K DOŘEŠENÍ START !!!!!
    # POP_PLOCHALOV: Extrakce plochy odlovu ze strukturovane poznamky (XML tag)
    POP_PLOCHALOV = readr::parse_number(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<plocha_prolov_p>).*(?=</plocha_prolov_p>)"
      )
    ), # ze strukturovane poznamky
    # POP_ABUNDANCENAL: Vypocet abundance (pocet jedincu / plocha odlovu)
    POP_ABUNDANCENAL = POP_POCET/POP_PLOCHALOV,
    # cilova jednotka, k nacteni z ciselniku, k doplneni Martinem
    # POP_CILJEDNOTKA: Zatim NA, placeholder pro cilovou jednotku
    POP_CILJEDNOTKA = NA,
    # POP_KOEFICIENT: Koeficient pro prepocet jednotek (napr. m2 na cm2)
    # Zatim se odviji od POP_CILJEDNOTKA a POCITANO_CLEAN.
    POP_KOEFICIENT = dplyr::case_when(
      POP_CILJEDNOTKA == POCITANO_CLEAN ~ 1,
      POP_CILJEDNOTKA == "cm2" & POCITANO_CLEAN == "dm2" ~ 100,
      POP_CILJEDNOTKA == "cm2" & POCITANO_CLEAN == "m2" ~ 10000,
      POP_CILJEDNOTKA == "dm2" & POCITANO_CLEAN == "cm2" ~ 0.01,
      POP_CILJEDNOTKA == "dm2" & POCITANO_CLEAN == "m2" ~ 100,
      POP_CILJEDNOTKA == "m2" & POCITANO_CLEAN == "cm2" ~ 0.0001,
      POP_CILJEDNOTKA == "m2" & POCITANO_CLEAN == "dm2" ~ 0.01),
    ## Vlivy ----
    # Extrakce informaci o vlivech ze dvou ruznych XML tagu ve strukturovane poznamce
    vliv1 = stringr::str_extract(
      STRUKT_POZN,
      "(?<=<vliv>).*(?=</vliv>)"
    ),
    vliv2 = stringr::str_extract(
      STRUKT_POZN, 
      "(?<=<VLV_VLIVY>).*(?=</VLV_VLIVY>)")
  ) %>%
    dplyr::mutate(
      # VLV_VLIVY: Sjednoceni vlivu do jedne promenne (priorita vliv1, jinak vliv2)
      VLV_VLIVY = dplyr::case_when(
        is.na(vliv1) == FALSE ~ vliv1,
        TRUE ~ vliv2
      ),
      # VLV_VLIVY_NUM: Pocet uvedenych vlivu (POCITANO_CLEAN dle poctu carek)
      VLV_VLIVY_NUM = stringr::str_count(
        VLV_VLIVY,
        ","
      )
    ) %>%
    dplyr::mutate(
      # ------------------------------------------#
      ## Hmyz ----- 
      # ------------------------------------------#
      # STA_SECCAS: Extrakce nacasovani sece ze strukturovane poznamky
      STA_SECCAS = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<sec_nacasovani>).*(?=</sec_nacasovani>)"
        )
      ),
      # STA_SECMET: Extrakce metodiky sece (celoplosna?) ze strukturovane poznamky
      STA_SECMET = readr::parse_character(
        stringr::str_extract(
          STRUKT_POZN, 
          "(?<=<sec_celoplosna>).*(?=</sec_celoplosna>)"
        )
      ),
      # STA_PRITOMNOSTROSTLIN: Extrakce pritomnosti zivnych rostlin (lisí se tag dle roku)
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
      # Extrakce dalsich parametru stanoviste (zastineni, mrtve drevo, atd.)
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
      # STA_MAN: Hodnoceni managementu na zaklade nacasovani a metodiky sece
      # 1 = vhodny, 0 = nevhodny
      STA_MAN = dplyr::case_when(STA_SECCAS == 1 &
                                   STA_SECMET == 1 ~ 1,
                                 STA_SECCAS == 0 |
                                   STA_SECMET == 0 ~ 0),
      # STA_LIKVIDACE: Detekce klicovych slov indikujicich likvidaci stanoviste
      STA_LIKVIDACE = dplyr::case_when(grepl(paste(c("vysazování lesů", 
                                                     "odvodňování, meliorace",
                                                     "zalesňování bezlesí",
                                                     "změna zemědělského využívání půdy"), 
                                                   collapse = "|"), 
                                             STRUKT_POZN, 
                                             ignore.case = TRUE) ~ "zaznamenána",
                                       STA_SECCAS == 0 ~ "zaznamenána",
                                       STA_MAN == 0 & DRUH == "Phengaris teleius" ~ "nezaznamenána",
                                       CILMON == 1 ~ "nezaznamenána")
    ) %>%
    # ------------------------------------------#
    ## Ostatní bezobratlí ----- 
  # ------------------------------------------#
  # ------------------------------------------#
  ## Obojživelníci a plazi ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # Extrakce stavu vody z ruznych typu vodnich utvaru
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
    # STA_STAVVODA: Sjednoceni stavu vody (priorita: tune -> litoral -> rybnik)
    STA_STAVVODA = dplyr::case_when(
      is.na(STA_STAVVODATUNE) == FALSE ~ STA_STAVVODATUNE,
      is.na(STA_STAVVODALITORAL) == FALSE ~ STA_STAVVODALITORAL,
      is.na(STA_STAVVODARYBNIK) == FALSE ~ STA_STAVVODARYBNIK),
    # STA_STAVVODAKAT: Prevod slovniho hodnoceni stavu vody na ciselnou kategorii
    STA_STAVVODAKAT = dplyr::case_when(
      STA_STAVVODA == "zaniklá" ~ 0L,
      STA_STAVVODA == "vyschlá" ~ 0L,
      STA_STAVVODA == "1-25 %" ~ 1L,
      STA_STAVVODA == "26-50 %" ~ 2L,
      STA_STAVVODA == "51-75 %" ~ 3L,
      STA_STAVVODA == "76-90 %" ~ 4L,
      STA_STAVVODA == "91-100 %" ~ 5L,
      TRUE ~ NA_integer_
    ),
    # STA_VYSYCHANI: Indikator vysychani (1 = vysycha, 0 = nevysycha)
    STA_VYSYCHANI = dplyr::case_when(
      STA_STAVVODA == "zaniklá" ~ 1L,
      STA_STAVVODA == "vyschlá" ~ 1L,
      STA_STAVVODA == "1-25 %" ~ 1L,
      is.na(STA_STAVVODA) == FALSE ~ 0L,
      TRUE ~ NA_integer_
    ),
    # STA_VODAMANIPULACE: Detekce manipulace s hladinou z poznamky
    STA_VODAMANIPULACE = dplyr::case_when(
      grepl("manipulace s vodní hladinou", STRUKT_POZN, ignore.case = TRUE) ~ "ano",
      TRUE ~ "ne"
    ),
    # STA_ZTRATABIO: Indikator ztraty biotopu (zazemeni nebo zanik)
    STA_ZTRATABIO = dplyr::case_when(
      STA_STAVVODA == "zazeměná" ~ "ano",
      STA_STAVVODA == "zaniklá" ~ "ano",
      TRUE ~ "ne"
    ),
    # Extrakce dalsich parametru (kachny, ryby, zooplankton, vegetace, pruhlednost)
    STA_KACHNAPRITOMNOST = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<STA_KACHNAPRITOMNOST>).*(?=</STA_KACHNAPRITOMNOST>)"
      )
    ),
    STA_RYBY = dplyr::case_when(
      grepl("akvakultur", STRUKT_POZN, ignore.case = TRUE) | 
        grepl("rybolov", STRUKT_POZN, ignore.case = TRUE) 
      ~ "ano",
      TRUE ~ "ne"),
    STA_ZOOPLANKTON = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<STA_ZOOPLANKTON>).*(?=</STA_ZOOPLANKTON>)"
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
    # Sjednoceni pruhlednosti vody (priorita STA_PRUHLEDNOSTVODAT)
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
    # Sjednoceni zastineni hladiny (bere se mensi hodnota z hladiny/litoralu)
    STA_ZASTINENIHLADINA = dplyr::case_when(
      STA_ZASTINENIHLADINA <= STA_ZASTINENILITORAL ~ STA_ZASTINENILITORAL,
      TRUE ~ STA_ZASTINENIHLADINA)
  ) %>%
    # ------------------------------------------#
    ## Ryby a mihule ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # Extrakce delek jedincu a migracnich barier
    POP_DELKYJEDINCI = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<velikosti>).*(?=</velikosti>)"
      )
    ),
    STA_MIGBARPOCET = readr::parse_number(
      str_extract(
        STRUKT_POZN, 
        "(?<=<pocet_bar>)\\d+(?=</pocet_bar>)"
      )
    ),
    STA_MIGBARVYS = readr::parse_number(
      str_extract(
        STRUKT_POZN, 
        "(?<=<vyska_bar>)[^<]+(?=</vyska_bar>)"
      )
    )
  ) %>%
    # ------------------------------------------#
    ## Savci ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # Extrakce SCALP metodiky (rys ostrovid)
    POP_SCALP = readr::parse_character(
      stringr::str_extract(
        STRUKT_POZN, 
        "(?<=<SCALP>).*(?=</SCALP>)"
      )
    ), 
    # POP_POBYT: Prevod SCALP na binarni pobyt (1 pro C1/C2, jinak 0)
    POP_POBYT = dplyr::case_when(
      POP_SCALP == "C1" ~ 1,
      POP_SCALP == "C2" ~ 1,
      TRUE ~ 0
    )
  ) %>%
    # ------------------------------------------#
    ## Letouni ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # POP_PRESENCE_ZIMNI: Maximalni presence v zimnim obdobi (listopad-duben)
    POP_PRESENCE_ZIMNI = max(
      POP_PRESENCE[(ROK == ROK & MESIC < 5) | (ROK == ROK - 1 & MESIC > 9)],
      na.rm = TRUE
    ),
    # POP_PRESENCE_LETNI: Maximalni presence v letnim obdobi (kveten-zari)
    POP_PRESENCE_LETNI = max(
      POP_PRESENCE[(ROK == ROK & MESIC >= 5 & MESIC <= 9)],
      na.rm = TRUE)
  ) %>%
    # ------------------------------------------#
    ## Mechorosty ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # Extrakce specifickych parametru pro mechorosty (mikrolokality, trend, poskozeni, atd.)
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
    )
  ) %>%
    # ------------------------------------------#
    ## Cévnaté rostliny ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # POP_POCETLODYH: Pocet lodyh, pokud je druh pritomen a jednotka odpovida
    POP_POCETLODYH = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETSUMLOD"] ~ POCET,
      TRUE ~ NA_real_
    ),
    # POP_POCETVITAL: Pocet vitalnich casti, pokud jednotka odpovida
    POP_POCETVITAL = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & limity$ID_IND %in% "POP_POCETVITAL"] ~ POCET,
      TRUE ~ NA_real_
    ),
    # Extrakce parametru managementu a pokryvnosti (invazni, expanzni, dreviny, atd.)
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
    # STA_MANPOTREBA: Potreba managementu (z textu)
    STA_MANPOTREBA = dplyr::case_when(
      grepl("je zapotřebí", STA_MANPOTREBAVLIV) ~ "je zapotřebí",
      grepl("není zapotřebí", STA_MANPOTREBAVLIV) ~ "není zapotřebí"
    ),
    # STA_MANVLIV: Vliv managementu (neutralni/negativni/pozitivni)
    STA_MANVLIV = dplyr::case_when(
      grepl("neutrální", STA_MANPOTREBAVLIV) ~ "neutrální",
      grepl("negativní", STA_MANPOTREBAVLIV) ~ "negativní",
      grepl("pozitivní", STA_MANPOTREBAVLIV) ~ "pozitivní"
    ),
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
    # POSKOZENI_ROSTLIN: Uprava textu poskozeni (pridani stredniku pro separaci)
    POSKOZENI_ROSTLIN = dplyr::case_when(
      is.na(SP_POSKOZENI_ROSTLIN) ~ NA_character_,
      TRUE ~ paste0(
        SP_POSKOZENI_ROSTLIN, ";"
      )
    ),
    # POSKOZENI_ROSTLIN_KAT: Prevod slovniho popisu poskozeni na ciselne kategorie (0-3)
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
    # Agregace dat v ramci akce
    dplyr::distinct() %>%
    dplyr::group_by(
      IDX_ND_AKCE, 
      DRUH) %>%
    dplyr::mutate(
      # Secteme dily populace (napr. samci + samice) za celou akci
      POP_POCETSUM = sum(POP_POCETSUM_PART, na.rm = TRUE),
      # Aktualizujeme POP_POCET:
      POP_POCET = dplyr::case_when(
        # FIX: Ensure sum is positive (>0) before overwriting!
        # This prevents "sum of NAs = 0" from destroying your "missing unit" logic.
        !is.na(POP_POCETSUM) & POP_POCETSUM > 0 & (POP_POCETSUM > dplyr::coalesce(POP_POCET, -1)) ~ POP_POCETSUM,
        TRUE ~ POP_POCET
      ),
      # POP_POCETKONCPAST: Pocet jedincu na past
      POP_POCETKONCPAST = dplyr::case_when(
        is.na(POP_PASTIPOCET) == TRUE ~ NA_real_,
        is.infinite(POP_PASTIPOCET) == TRUE ~ NA_real_,
        TRUE ~ POP_POCET/POP_PASTIPOCET
      )
    ) %>%
    dplyr::mutate(
      # POP_POCETMIN: Minimalni odhad populace na zaklade kategorie pocetnosti
      POP_POCETMIN = dplyr::case_when(
        is.na(POP_POCET) == FALSE ~ as.numeric(POP_POCET),
        POP_POCETNOSTNAL == 8 ~ 1000000,
        POP_POCETNOSTNAL == 7 ~ 100001,
        POP_POCETNOSTNAL == 6 ~ 10001,
        POP_POCETNOSTNAL == 5 ~ 1001,
        POP_POCETNOSTNAL == 4 ~ 101,
        POP_POCETNOSTNAL == 3 ~ 50,
        POP_POCETNOSTNAL == 2 ~ 11,
        POP_POCETNOSTNAL == 1 ~ 1,
        TRUE ~ NA_real_
      ),
      # POP_POCETMAX: Maximalni odhad populace na zaklade kategorie pocetnosti
      POP_POCETMAX = dplyr::case_when(
        is.na(POP_POCET) == FALSE ~ as.numeric(POP_POCET),
        POP_POCETNOSTNAL == 5 ~ 10000,
        POP_POCETNOSTNAL == 4 ~ 1000,
        POP_POCETNOSTNAL == 3 ~ 10000,
        POP_POCETNOSTNAL == 2 ~ 10000,
        POP_POCETNOSTNAL == 1 ~ 10000,
        TRUE ~ NA_real_
      ),
      POP_POCETPRUM = 50,
      # POP_POCETLODYHSUM: Celkovy soucet lodyh
      POP_POCETLODYHSUM = sum(
        POP_POCETLODYH, 
        na.rm = TRUE
      ),
      # POP_POCETPLOD: Pocet plodnych casti, pokud jednotka odpovida
      POP_POCETPLOD = dplyr::case_when(
        POP_PRESENCE == "ne" ~ 0,
        POCITANO_CLEAN %in% limity$JEDNOTKA[limity$DRUH %in% DRUH & 
                                        limity$ID_IND %in% "POP_POCETSUMLOD"] ~ POCET,
        TRUE ~ NA_real_
      ),
      # POP_VITAL: Procento vitalnich casti populace
      POP_VITAL = POP_POCETVITAL/POP_POCET*100
      ) %>%
    dplyr::mutate(
      POP_POCETFIN = as.numeric(dplyr::coalesce(POP_POCET, POP_POCETMIN)),
      POP_POCET = POP_POCETFIN
    ) %>%
    dplyr::ungroup() %>%
    # Zpracovani poskozeni rostlin (rozpad retezce)
    dplyr::group_by(
      ID_ND_NALEZ
    ) %>%
    tidyr::separate_rows(
      POSKOZENI_ROSTLIN, 
      sep = "; "
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      # POP_POSKPOC: Pocet zaznamu o poskozeni
      POP_POSKPOC = dplyr::case_when(
        is.na(SP_POSKOZENI_ROSTLIN) == TRUE ~ 0,
        SP_POSKOZENI_ROSTLIN == "Nezjištěno" ~ 0,
        TRUE ~ sum(!is.na(POSKOZENI_ROSTLIN) & POSKOZENI_ROSTLIN != "Nezjištěno")
      ),
      # POCET_POSKOZENYCH: Parsrovani cisla z textu poskozeni
      POCET_POSKOZENYCH = readr::parse_number(as.character(POSKOZENI_ROSTLIN)),
      # POCET_POSKOZENYCHSUM: Soucet poskozenych jedincu
      POCET_POSKOZENYCHSUM = sum(POCET_POSKOZENYCH, na.rm = TRUE),
      # PROCENTO_POSKOZENYCH: Podil poskozenych z celkoveho poctu lodyh
      PROCENTO_POSKOZENYCH = POCET_POSKOZENYCHSUM/unique(POP_POCETLODYHSUM),
      # POSKOZENI_KAT: Kategorizace poskozeni jednotliveho zaznamu (0-3)
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
      # POSKOZENI_LODYHKAT: Kategorizace celkoveho procenta poskozeni (0-3)
      POSKOZENI_LODYHKAT = dplyr::case_when(
        PROCENTO_POSKOZENYCH == 0 ~ 0,
        PROCENTO_POSKOZENYCH > 0 & PROCENTO_POSKOZENYCH <= 0.1 ~ 1,
        PROCENTO_POSKOZENYCH > 0.1 & PROCENTO_POSKOZENYCH <= 0.5 ~ 2,
        PROCENTO_POSKOZENYCH > 0.5 ~ 3,
        TRUE ~ NA_integer_
      ),
      # POP_POSKPERC: Celkove procento poskozeni (pro specificke druhy jinak)
      POP_POSKPERC = dplyr::case_when(
        DRUH == "Adenophora liliifolia" ~ unique(POSKOZENI_LODYHKAT),
        TRUE ~ sum(
          POSKOZENI_KAT, 
          na.rm = TRUE
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    # Rozpad retezcu pro pruhlednost a zapojeni (vice hodnot oddelenych carkou)
    tidyr::separate_rows(
      STA_PRUHLEDNOSTVODA, 
      sep = ","
    ) %>%
    tidyr::separate_rows(
      STA_ZAPOJENICELK, 
      sep = ","
    ) %>%
    dplyr::mutate(
      # STA_ZAPOJENIDRN: Parsrovani zapojeni drnu z textu
      STA_ZAPOJENIDRN = dplyr::case_when(
        nchar(STA_ZAPOJENICELK) > 15 ~ str_match(STA_ZAPOJENICELK, 
                                                 "zapojený::\\s*(.*?)\\s*%")[,2],
        TRUE ~ "0"),
      STA_ZAPOJENIDRN = readr::parse_number(as.character(STA_ZAPOJENICELK))
    ) %>%
    # Rozpad retezce delek jedincu (ryby)
    tidyr::separate_rows(
      POP_DELKYJEDINCI,
      sep = ","
    ) %>%
    dplyr::mutate(
      POP_DELKYJEDINCINUM = as.numeric(
        POP_DELKYJEDINCI
      )
    ) %>%
    # Fuzzy join s ciselnikem delek ryb (intervalove prirazeni kategorii)
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
    # Razeni dat pro vyber reprezentativnich hodnot
    dplyr::arrange(
      desc(MESIC),
      desc(DEN)
    ) %>%
    dplyr::reframe(
      # ------------------------------------------#
      ### Společné indikátory ----- 
      # ------------------------------------------#
      CELKOVE = NA,
      # POP_POCETSUMLOKAL: Soucet populace za lokalitu
      # DULEZITE: Pouzivame !duplicated(IDX_ND_AKCE) pro zamezeni nasobeni stejnych akci
      POP_POCETSUMLOKAL = sum(POP_POCET[!duplicated(IDX_ND_AKCE)], na.rm = TRUE),
      # POP_POCETMIN: Minimalni hodnota populace
      POP_POCETMIN = min(
        POP_POCET, 
        na.rm = TRUE
      ), 
      # POP_POCETMAX: Maximalni hodnota populace
      POP_POCETMAX = max(
        POP_POCET, 
        na.rm = TRUE
      ), 
      # Osetreni nekonecnych hodnot u maxima
      POP_POCETMAX = ifelse(is.infinite(POP_POCETMAX), 0, POP_POCETMAX),
      # POP_POCETNOST: Maximalni kategorie pocetnosti
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
      # POP_REPROMAX: Maximalni reprodukce (1/0)
      POP_REPROMAX = max(POP_REPRONUM, na.rm = TRUE),
      POP_REPROMAX = ifelse(is.infinite(POP_REPROMAX), NA_real_, POP_REPROMAX),
      # STA_VYSYCHMAX: Maximalni hodnota vysychani
      STA_VYSYCHMAX  = max(STA_VYSYCHANI, na.rm = TRUE),
      STA_VYSYCHMAX = ifelse(is.infinite(STA_VYSYCHMAX), NA_real_, STA_VYSYCHMAX),
      # STA_STAVVODAKAT1/2: Prvni a druha hodnota kategorie stavu vody (z serazeni)
      STA_STAVVODAKAT1 = STA_STAVVODAKAT[1],
      STA_STAVVODAKAT2 = STA_STAVVODAKAT[2],
      # ------------------------------------------#
      ### Ryby a mihule ----- 
      # ------------------------------------------#
      # Spocitame pocet kategorii
      # POP_VITALITA_N_CATS: Pocet ruznych vekovych/velikostnich kategorii (vitalita)
      POP_VITALITA_N_CATS = dplyr::n_distinct(POP_DELKYJEDINCIKAT, na.rm = TRUE),
      # POP_VITALITA: Finalni hodnota vitality
      # 0 pokud druh chybi, NA pokud nejsou data, jinak pocet kategorii.
      POP_VITALITA = dplyr::case_when(
        # 1. Druh není přítomen -> Vitalita je 0 (Logické, populace nefunguje)
        POP_PRESENCE == "ne" ~ 0,
        # 2. Druh je přítomen, ale nemáme žádné kategorie (chybí data o velikostech) -> NA
        POP_PRESENCE == "ano" & POP_VITALITA_N_CATS == 0 ~ NA_integer_,
        # 3. Druh je přítomen a máme data -> Vracíme počet kategorií
        TRUE ~ as.integer(POP_VITALITA_N_CATS)
      ),
      # POP_ABUNDANCE: Maximalni abundance na lokalite
      POP_ABUNDANCE = max(POP_ABUNDANCENAL, na.rm = TRUE),
      POP_ABUNDANCE = dplyr::if_else(is.infinite(POP_ABUNDANCE), NA_real_, POP_ABUNDANCE),
      # ------------------------------------------#
      ### Savci ----- 
      # ------------------------------------------#
      # POP_POCETZIM: Maximalni zimni pocet (listopad-duben aktualniho roku)
      POP_POCETZIM = max(
        POP_POCET[(ROK == current_year & 
                     MESIC < 5) |
                    (ROK == current_year - 1 & 
                       MESIC > 9)
        ],
        na.rm = TRUE
      ), 
      # POP_POCETZIM1/2/3: Zimni pocty v predchozich letech
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
      # POP_POCETZIMREF: Prumer zimnich poctu z minulych 3 let (referencni)
      POP_POCETZIMREF = mean(
        POP_POCETZIM1, 
        POP_POCETZIM2, 
        POP_POCETZIM3, 
        na.rm = TRUE
      ),
      # POP_VITALZIM: Pomer aktualniho zimniho poctu k referencnimu
      POP_VITALZIM = POP_POCETZIM/POP_POCETZIMREF,
      # POP_POCETLETS1/2: Letni pocty v specifickych obdobich
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
      # POP_POCETLET: Maximalni letni pocet (kveten-zari)
      POP_POCETLET = max(
        POP_POCET[(ROK == current_year & 
                     MESIC >= 5 & 
                     MESIC <= 9)], 
        na.rm = TRUE
      ), 
      # POP_POCETLET1/2/3: Letni pocty v minulych letech
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
      # POP_POCETLETREF: Prumer letnich poctu z minulych 3 let
      POP_POCETLETREF = mean(
        POP_POCETLET1, 
        POP_POCETLET2, 
        POP_POCETLET3, 
        na.rm = TRUE
      ),
      # POP_VITALLET: Pomer aktualniho letniho poctu k referencnimu
      POP_VITALLET = POP_POCETLET/POP_POCETLETREF,
      # POP_REPROCHI: Reprodukcni index (pomer pozdniho a raneho leta)
      POP_REPROCHI = POP_POCETLETS2/POP_POCETLETS1
    ) %>%
    dplyr::ungroup()
  
  # Lokalita - trendy akualni----
  # populacni trendy odvozene od posledniho pozorovani POP_POCETMAX[1]
  n2k_druhy_lokpop_trend_desc <- n2k_druhy_lokpop %>%
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
      # POP_POCETMAXREF: Referencni maximum pred 3 lety
      POP_POCETMAXREF = POP_POCETMAX[3],
      # POP_TREND1/2: Porovnani aktualnich hodnot s referenci (1 = lepsi, 0 = horsi)
      POP_TREND1 = dplyr::case_when(
        POP_POCETMAX[1] >= POP_POCETMAXREF ~ 1,
        POP_POCETMAX[1] < POP_POCETMAXREF ~ 0
      ),
      POP_TREND2 = dplyr::case_when(
        POP_POCETMAX[2] >= POP_POCETMAXREF ~ 1,
        POP_POCETMAX[2] < POP_POCETMAXREF ~ 0
      ),
      # POP_TREND: Suma trendu (hodnoceni stability)
      POP_TREND = sum(
        POP_TREND1, 
        POP_TREND2, 
        na.rm = TRUE
      ),
      # POP_TRENDLM: Linearni trend (smernice regrese)
      POP_TRENDLM = if (sum(!is.na(POP_POCETMAX)) > 1) {
        coef(lm(POP_POCETMAX ~ ROK))[2]
      } else {
        NA_real_
      },
      # POP_ABUNDANCEMEAN: Prumerna abundance za posledni 3 roky
      POP_ABUNDANCEMEAN = mean(head(POP_ABUNDANCE, 3), na.rm = TRUE),
      # POP_POCETNOSTMAX: Maximalni pocetnost
      POP_POCETNOSTMAX = max(
        POP_POCETNOST, 
        na.rm = TRUE
      ),
      # POP_REPROPERIOD3: Suma reprodukce za 3 roky
      POP_REPROPERIOD3 = {
        v <- as.numeric(POP_REPROMAX[1:3])
        v[is.infinite(v)] <- NA_real_
        sum(v, na.rm = TRUE)
      },
      # STA_VYSYCHANIPERIOD3: Suma vysychani za 3 roky
      STA_VYSYCHANIPERIOD3 = {
        v <- as.numeric(STA_VYSYCHMAX[1:3])
        v[is.infinite(v)] <- NA_real_
        sum(v, na.rm = TRUE)
      }
    ) %>%
    dplyr::ungroup() 
  
  # Lokalita - trendy referencni----
  n2k_druhy_lokpop_trend_ascd <- n2k_druhy_lokpop %>%
    dplyr::group_by(
      KOD_LOKAL, 
      DRUH
    ) %>%
    # serazeni sestupne podle roku (pozor, v kodu je arrange(ROK), tedy vzestupne - od nejstarsich)
    dplyr::arrange(
      ROK
    ) %>%
    #dplyr::filter(CILMON == 1 & is.na(POP_POCETMAX) == FALSE & is.infinite(POP_POCETMAX) == FALSE) %>%
    dplyr::reframe(
      # POP_ABUNDANCEREF: Prumerna abundance na zacatku sledovaneho obdobi (referencni stav)
      POP_ABUNDANCEREF = mean(head(na.omit(POP_ABUNDANCE), 3), na.rm = TRUE)
    ) %>%
    dplyr::ungroup() 
  
  # Spojeni aktualnich a referencnich trendu
  n2k_druhy_lokpop_trend <-
    dplyr::left_join(
      n2k_druhy_lokpop_trend_desc,
      n2k_druhy_lokpop_trend_ascd,
      by = c("KOD_LOKAL", "DRUH")
    ) %>%
    dplyr::mutate(
      # POP_DYN: Dynamika populace (procentualni zmena oproti referenci)
      POP_DYN = dplyr::case_when(
        # 1. Pokud nemáme data pro výpočet (NaN vzniklé z mean(NA))
        is.na(POP_ABUNDANCEMEAN) | is.nan(POP_ABUNDANCEMEAN) | 
          is.na(POP_ABUNDANCEREF) | is.nan(POP_ABUNDANCEREF) ~ NA_real_,
        # 2. Stabilní absence: Začátek 0, Konec 0 -> 100% (beze změny)
        POP_ABUNDANCEREF == 0 & POP_ABUNDANCEMEAN == 0 ~ 100,
        # 3. Kolonizace: Začátek 0, Konec > 0 -> Nekonečný nárůst
        # R by vratilo Inf automaticky, ale explicitně je to čistší.
        # Pokud chcete strop, dejte sem třeba 9999. Jinak nechte Inf.
        POP_ABUNDANCEREF == 0 & POP_ABUNDANCEMEAN > 0 ~ Inf,
        # 4. Standardní výpočet
        TRUE ~ (POP_ABUNDANCEMEAN / POP_ABUNDANCEREF) * 100
      )
    ) %>%
    # Pripojeni kategorii poctu z ciselniku
    dplyr::left_join(
      cis_pocet_kat,
      by = "POP_POCETNOSTMAX"
    ) %>%
    dplyr::distinct()
  
  
  #--------------------------------------------------#
  # Kompilace konecne tabulky vsech indikatoru ----- 
  #--------------------------------------------------#
  n2k_druhy <- n2k_druhy_pre %>%
    # Pripojeni agregovanych dat za lokalitu a rok
    dplyr::left_join(
      ., 
      n2k_druhy_lokpop,
      by = join_by(
        ROK, 
        KOD_LOKAL,
        DRUH
      )
    ) %>%
    # Pripojeni trendu (jen za lokalitu, ne rok - trend je jeden pro lokalitu)
    dplyr::left_join(
      ., 
      n2k_druhy_lokpop_trend, 
      by = join_by(
        KOD_LOKAL, 
        DRUH
      )
    ) %>%
    dplyr::distinct()
  
  return(n2k_druhy)
  
}