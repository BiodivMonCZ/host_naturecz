#----------------------------------------------------------#
# Priprava prostredi -----
#----------------------------------------------------------#

# 1. Definice klicove promenne pro urceni sloupcu
ncol_orig <- ncol(n2k_load)

# 2. Zalozni kod metodiky pro export do ISOP.
# Pouzije se u druhu, ktere nemaji v Data/Input/cis_metodika.csv vyplneny
# sloupec 'metodika' - dnes vsechny skupiny krome obojzivelniku (22257)
# a cevnatych rostlin (19192). Drive byla tato hodnota natvrdo u vsech druhu.
METODIKA_VYCHOZI <- 15087

#----------------------------------------------------------#
# Druhovy seznam -----
#----------------------------------------------------------#
# Seznam druhu NENI napevno vazany na jednu SKUPINU (drive napr. natvrdo
# "Letouni" nebo "Obojživelníci"). Misto toho se odvozuje ze vsech druhu, pro
# ktere (a) existuje v tabulce limitu (limity$DRUH) definovana metodika
# hodnoceni A (b) existuje pro ne alespon jeden zaznam v nalezove databazi
# (n2k_load$DRUH). Clenstvi v sites_subjects (oficialni seznam predmetu
# ochrany dle lokality) se ZDE NEPOUZIVA jako filtr, protoze muze byt neuplne
# (napr. Lissotriton montandoni v nem chybi, ac ma definovanou metodiku i
# realne zaznamy) - skutecne omezeni na oficialni lokality se stale plne
# uplatnuje uvnitr run_n2k_druhy() pri praci s jednotlivymi zaznamy.
species_list <- intersect(
  unique(as.character(limity$DRUH)),
  unique(as.character(n2k_load$DRUH))
)
species_list <- species_list[!is.na(species_list)]

#----------------------------------------------------------#
# Pomocna funkce: sekvencni beh pres druhy -----
#----------------------------------------------------------#
# Druhy se pocitaji jeden po druhem (zadna paralelizace). `worker_fn` je
# funkce dvou argumentu (nazev druhu, jeho predfiltrovana data -
# `data_split[[sp]]`), ktera pro chybejici/prazdny podil dat vraci NULL
# (viz volajici funkce nize) - takove polozky `dplyr::bind_rows()` proste
# vynecha.
#
# PRUBEH VYPOCTU: prubeh se hlasi v jednotkach DRUH-LOKALITA (tj. kolik
# kombinaci druhu a EVL uz je zpracovano vuci celkovemu poctu), vcetne
# procent, uplynuleho casu a odhadu zbyvajiciho casu. Vypocet samotny ale
# probiha po DRUZICH, ne po jednotlivych lokalitach - viz poznamka nize.
#
# PROC NE PO JEDNOTLIVYCH LOKALITACH: run_n2k_druhy() (faze 1) agreguje
# populacni indikatory a trendy pres `KOD_LOKAL, ROK, DRUH`, tedy BEZ
# `kod_chu`. V datech existuje 224 kombinaci druh+KOD_LOKAL, kde se stejny
# KOD_LOKAL vyskytuje pod vice ruznymi EVL (casto neunikatni kody typu "1",
# "2", "Prameny"). Kdyby se faze 1 delila po lokalitach, tyto agregace
# (POP_POCETMAX, POP_ZMENARAD, POP_REPROPERIOD3, STA_VYSYCHANIPERIOD3 ...)
# by se rozpadly na dilci casti a vysledky by se zmenily. Deleni po druzich
# je proto z hlediska spravnosti vysledku zavazne.
run_over_species <- function(species_list, data_split, worker_fn, phase_label) {
  # Pocet lokalit (EVL) pro kazdy druh = jednotky druh-lokalita
  unit_counts <- vapply(species_list, function(sp) {
    ch <- data_split[[sp]]
    if (is.null(ch) || nrow(ch) == 0) return(0L)
    as.integer(dplyr::n_distinct(ch$kod_chu, na.rm = TRUE))
  }, integer(1))
  total_units <- sum(unit_counts)
  n_species <- length(species_list)

  t_start <- Sys.time()
  done_units <- 0L
  result <- vector("list", n_species)

  for (i in seq_along(species_list)) {
    sp <- species_list[[i]]
    chunk <- data_split[[sp]]
    n_units <- unit_counts[[i]]

    # Hlaseni se vypisuje PRED zpracovanim druhu: `done_units` je tedy pocet
    # jednotek DOKONCENYCH v predchozich krocich (aktualni druh se zapocita
    # az po dopocitani), takze procenta i odhad zbyvajiciho casu odpovidaji
    # skutecne dokoncene praci. Zaroven uzivatel v terminalu vidi, na kterem
    # druhu vypocet prave pracuje, i kdyz trva dele.
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    pct <- if (total_units > 0) done_units / total_units * 100 else 0
    eta <- if (done_units > 0) {
      elapsed / done_units * (total_units - done_units)
    } else NA_real_

    progress_line(sprintf(
      "[%s] druh %2d/%2d | hotovo %5d/%5d (%5.1f %%) | ubehlo %s | zbyva ~%s | -> %s (%d lokalit)",
      phase_label, i, n_species, done_units, total_units, pct,
      format_hms(elapsed), format_hms(eta), sp, n_units
    ))

    if (!is.null(chunk) && nrow(chunk) > 0) {
      result[[i]] <- worker_fn(sp, chunk)
    }

    done_units <- done_units + n_units
  }

  progress_line(sprintf(
    "[%s] HOTOVO: %d druhu / %d lokalit-druh za %s",
    phase_label, n_species, total_units,
    format_hms(as.numeric(difftime(Sys.time(), t_start, units = "secs")))
  ))

  dplyr::bind_rows(result)
}

# Vypis prubehu do konzole.
# Na Windows R konzole bufferuje vystup, takze bez flush.console() by se
# hlaseni objevila az na konci behu (nebo po velkych davkach) - proto se
# vystup po kazdem radku explicitne vyprazdni, aby byl prubeh videt zive
# v terminalu.
progress_line <- function(txt) {
  message(txt)
  utils::flush.console()
  invisible(NULL)
}

# Pomocny formatovac casu (sekundy -> HH:MM:SS)
format_hms <- function(secs) {
  if (!is.finite(secs) || secs < 0) return("--:--:--")
  secs <- round(secs)
  sprintf("%02d:%02d:%02d", secs %/% 3600, (secs %% 3600) %/% 60, secs %% 60)
}

# Zapis exportniho CSV komprimovaneho gzipem (.csv.gz) -----
# Vystupni tabulky jsou silne redundantni a gzip je zmensi cca 25x
# (napr. 264 MB -> 11 MB), cimz se vejdou pod limit GitHubu 100 MB na soubor.
# write.table() neumi gzip odvodit z pripony, proto se zapisuje pres gzfile().
# Pri predani connection se fileEncoding ignoruje, takze cilove kodovani musi
# byt nastaveno primo na connection - jinak by se Windows-1250 export ulozil
# v nativnim kodovani. Cteni zmenu nevyzaduje: readr::read_csv() i read.csv()
# ctou .gz transparentne.
write_export_gz <- function(x, path, encoding, sep, quote) {
  con <- gzfile(path, open = "wt", encoding = encoding)
  on.exit(close(con), add = TRUE)
  write.table(x, con, row.names = FALSE, sep = sep, quote = quote)
  invisible(path)
}

#----------------------------------------------------------#
# Zapis dat nalez -----
#----------------------------------------------------------#

nal_export <- function(
    n2k_load,
    species_list,
    sites_subjects,
    limity,
    evl,
    rp_code,
    n2k_oop,
    current_year = 2024
) {

  # Zajisteni existence slozky pro docasne soubory
  if (!dir.exists("Data/Temp/")) {
    dir.create("Data/Temp/", recursive = TRUE)
  }

  N_species <- length(species_list)

  #----------------------------------------------------------#
  # 1. Uroven akce (Vypocet indikatoru) -----
  #----------------------------------------------------------#
  progress_line(paste0("--- ZACINAM VYPOCET FAZE 1 (AKCE) PRO ", N_species, " DRUHU ---"))

  # Predfiltrujeme n2k_load JEDNOU (misto N-krat uvnitr run_n2k_druhy, jednou
  # pro kazdy druh) a rozdelime podle DRUH. Kazde volani run_n2k_druhy tak
  # dostane uz jen svuj (maly) podil dat namisto opakovaneho prohledavani cele
  # tabulky vsech druhu. run_n2k_druhy si stejne filtry aplikuje znovu uvnitr,
  # coz je na jiz vyfiltrovanych datech neskodne (idempotentni), takze vysledek
  # zustava identicky - jen se pocita jednou navic misto N-krat.
  n2k_load_valid <- n2k_load %>%
    dplyr::filter(ROK >= current_year - 12) %>%
    dplyr::filter(
      DRUH %in% c("Eriogaster catax", "Euphydryas aurinia") |
        (DRUH %in% sites_subjects$nazev_lat & kod_chu %in% sites_subjects$site_code)
    )
  n2k_load_split <- split(n2k_load_valid, as.character(n2k_load_valid$DRUH))

  run_one_akce <- function(sp, chunk) {
    run_n2k_druhy(chunk, sp, sites_subjects, limity, current_year = current_year)
  }

  n2k_druhy <- run_over_species(species_list, n2k_load_split, run_one_akce, "1/4 Akce")

  # Ulozeni mezivysledku 1
  progress_line("--- UKLADAM MEZIVYSLEDEK 1 DO TEMP ---")
  readr::write_csv(
    n2k_druhy,
    paste0("Data/Temp/n2k_druhy", ".csv")
  )

  #----------------------------------------------------------#
  # 1b. Rada pocetnosti pro uroven uzemi (Tabulka 2 metodiky) -----
  #----------------------------------------------------------#
  # Metodika, Tabulka 2, druhy indikator: "Predmetem hodnoceni je soucet
  # maximalnich pocetnosti zaznamenanych na kazde DP v danem roce", ktery se
  # porovnava jako KLOUZAVY PRUMER ZA POSLEDNI 3 ROKY s cilovym stavem uzemi.
  #
  # PROC ZDE, A NE AZ VE FAZI 4: faze 2 (run_n2k_druhy_lim) prevadi do dlouheho
  # formatu POUZE indikatory, ktere maji vyplneny LIM_IND. POP_POCET ma
  # v limity_vse.csv LIM_IND = NA (slouzi jen jako vycet jednotek), takze se
  # do faze 3 ani 4 vubec nedostane. Faze 3 navic vybira reprezentativni
  # navstevu a na kazde DP ponechava jediny rok, takze klouzavy prumer za tri
  # roky uz z jejiho vystupu spocitat nelze. Faze 1 je tedy posledni misto,
  # kde jsou k dispozici vsechny roky i surove pocty.
  #
  # Vysledek se uklada do samostatneho temp souboru, aby zustala zachovana
  # existujici architektura oddelenych fazi propojenych pres Data/Temp.
  # RELATIVNI POCETNOST SE PREVADI NA MEDIAN KATEGORIE, NE NA JEJI DOLNI MEZ
  # (nalez H-60). Metodika, par. Hodnoceni na urovni sledovaneho uzemi:
  #   "Predmetem hodnoceni je soucet maximalnich pocetnosti zaznamenanych na
  #    kazde DP v danem roce. V pripade, ze pro danou DP existuje zaznam
  #    relativni pocetnosti, prevadi se na odpovidajici hodnotu MEDIANU dane
  #    kategorie dle prevodni tabulky (napr. 500 jedincu pro kategorii stovky)."
  #
  # POZOR NA JIZ PROVEDENY PREVOD: 21_1_n2k_druhy_akce.R na konci faze 1 dela
  #   POP_POCETFIN = coalesce(POP_POCET, POP_POCETMIN); POP_POCET = POP_POCETFIN
  # tedy zaznam bez ciselneho poctu UZ MA POP_POCET doplneny z kategorie -
  # ale DOLNI MEZI (POP_POCETNMIN), ne medianem. Zaznamy se tedy neztraceji;
  # jsou jen prevedeny jinou statistikou, nez metodika predepisuje. Pro
  # kategorii "stovky" tak vychazi 101 misto 500, jak uvadi prima citace vyse.
  #
  # Rozlisit oba pripady lze podle SUROVEHO sloupce POCET z NDOP, ktery zustava
  # zachovan: je-li vyplnen, jde o skutecne zmereny pocet; je-li prazdny a
  # existuje kategorie 1-8, jde o dopocet z relativni pocetnosti.
  #
  # Tim se zaroven uzavira nalez H-26 ("dolni mez vs. median") - metodika
  # odpovida jednoznacne medianem. Zmena je zamerne omezena na TENTO indikator
  # urovne uzemi, protoze citace se tyka prave jeho; POP_POCET pouzivany jinde
  # (abundance, trendy, POP_VITAL) zustava nedotcen.
  if (!exists("cis_pocet_kat")) {
    stop("Objekt 'cis_pocet_kat' neexistuje - spustte R/00_config/02_n2k_data_druhy.R")
  }
  if (!"POP_POCETSTRED" %in% names(cis_pocet_kat)) {
    stop("Ciselnik 'cis_pocet_kat' nema sloupec POP_POCETSTRED (median kategorie).")
  }

  pocetnost_uzemi <- n2k_druhy %>%
    dplyr::mutate(
      POP_POCETMEDIAN = as.numeric(
        cis_pocet_kat$POP_POCETSTRED[
          match(POP_POCETNOSTNAL, cis_pocet_kat$POP_POCETNOSTMAX)
        ]
      ),
      POP_POCETEFEKT = dplyr::case_when(
        # skutecne zmereny pocet - bere se tak, jak byl zaznamenan
        !is.na(POCET) ~ as.numeric(POP_POCET),
        # jen relativni kategorie (1-8) -> median kategorie dle metodiky
        !is.na(POP_POCETMEDIAN) ~ POP_POCETMEDIAN,
        # nepritomnost druhu (kategorie 0) i ostatni pripady - beze zmeny
        TRUE ~ as.numeric(POP_POCET)
      )
    ) %>%
    dplyr::filter(CILMON == 1, !is.na(POP_POCETEFEKT)) %>%
    # maximum za dilci plochu a rok
    dplyr::group_by(kod_chu, DRUH, KOD_LOKAL, ROK) %>%
    dplyr::summarise(POP_POCETDP = max(POP_POCETEFEKT, na.rm = TRUE), .groups = "drop") %>%
    # soucet pres vsechny DP v ramci roku
    dplyr::group_by(kod_chu, DRUH, ROK) %>%
    dplyr::summarise(POP_POCETSUMROK = sum(POP_POCETDP, na.rm = TRUE), .groups = "drop") %>%
    # klouzavy prumer za posledni tri hodnocene roky
    dplyr::group_by(kod_chu, DRUH) %>%
    dplyr::slice_max(order_by = ROK, n = 3, with_ties = FALSE) %>%
    dplyr::summarise(
      POP_POCETPRUM3 = round(mean(POP_POCETSUMROK, na.rm = TRUE), 1),
      POP_POCETPRUM3_LET = dplyr::n(),
      .groups = "drop"
    )

  readr::write_csv(
    pocetnost_uzemi,
    paste0("Data/Temp/n2k_druhy_pocetnost", ".csv")
  )

  #----------------------------------------------------------#
  # 2. Porovnani s limity -----
  #----------------------------------------------------------#
  progress_line(paste0("--- ZACINAM VYPOCET FAZE 2 (LIMITY) ---"))

  if (nrow(n2k_druhy) == 0) {
    warning("Faze 1 nevygenerovala zadna data. Faze 2 a export budou preskoceny.")
    return(NULL)
  }

  n2k_druhy_split <- split(n2k_druhy, as.character(n2k_druhy$DRUH))

  run_one_lim <- function(sp, data_subset) {
    run_n2k_druhy_lim(data_subset, sp, sites_subjects, limity, current_year = current_year)
  }

  n2k_druhy_lim <- run_over_species(species_list, n2k_druhy_split, run_one_lim, "2/4 Limity")

  # Ulozeni mezivysledku 2 (take jako vstup pro lok_export, pokud se vola
  # samostatne / bez primeho predani objektu v pameti)
  progress_line("--- UKLADAM MEZIVYSLEDEK 2 DO TEMP ---")
  readr::write_csv(
    n2k_druhy_lim,
    paste0("Data/Temp/n2k_druhy_lim", ".csv")
  )

  #----------------------------------------------------------#
  # 3. Propojeni s metadaty a serazeni sloupcu -----
  #----------------------------------------------------------#

  progress_line("--- PRIPRAVA DAT PRO EXPORT (NALEZY) ---")

  if (nrow(n2k_druhy_lim) == 0) {
    warning("Faze 2 nevygenerovala zadna data. Export se neprovede.")
    return(NULL)
  }

  n2k_druhy_lim_write <- n2k_druhy_lim %>%
    # Pripojeni informaci o EVL
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
    # Pripojeni kodu RP
    dplyr::left_join(
      .,
      rp_code,
      by = dplyr::join_by(
        "kod_chu"
      )
    ) %>%
    # Pripojeni informaci o OOP
    dplyr::left_join(
      .,
      n2k_oop,
      by = c("kod_chu" = "SITECODE")
    ) %>%
    dplyr::distinct() %>%
    # Logicke serazeni sloupcu (stejna logika jako u lok_export)
    dplyr::select(
      # 1. Identifikace
      ROK,
      kod_chu,
      NAZEV,          # Nazev EVL
      DRUH,
      SKUPINA,

      # 2. Lokalita
      KOD_LOKAL,
      LOKALITA,
      pracoviste,
      oop,

      # 3. Akce a Nalez
      IDX_ND_AKCE,
      ID_ND_NALEZ,    # Specificke pro nal_export
      DATUM,
      AUTOR,          # Specificke pro nal_export

      # 4. Definice Indikatoru
      ID_IND,
      JEDNOTKA,
      TYP_IND,
      IND_GRP,
      KLIC,
      UROVEN,

      # 5. Vysledky
      LIM_IND,
      HOD_IND,
      STAV_IND,

      # Zbytek
      dplyr::everything()
    )

  #----------------------------------------------------------#
  # 4. Export dat -----
  #----------------------------------------------------------#

  sep_isop <- ";"
  quote_env_isop <- FALSE
  encoding_isop <- "UTF-8"

  sep <- ","
  quote_env <- TRUE
  encoding <- "Windows-1250"

  date_stamp <- gsub("-", "", Sys.Date())
  file_base <- paste0("Outputs/Data/druhy/n2k_druhy_nal_", current_year, "_", date_stamp)

  progress_line(paste0("--- EXPORTUJI SOUBORY DO: ", file_base, "... ---"))

  if (!dir.exists("Outputs/Data/druhy/")) {
    dir.create("Outputs/Data/druhy/", recursive = TRUE)
  }

  # Export 1: Windows-1250
  write_export_gz(
    n2k_druhy_lim_write,
    paste0(file_base, "_", encoding, ".csv.gz"),
    encoding = encoding,
    sep = sep,
    quote = quote_env
  )

  # Export 2: UTF-8
  write_export_gz(
    n2k_druhy_lim_write,
    paste0(file_base, "_", encoding_isop, ".csv.gz"),
    encoding = encoding_isop,
    sep = sep_isop,
    quote = quote_env_isop
  )

  progress_line("--- HOTOVO (NAL_EXPORT) ---")

  return(list(nal_write = n2k_druhy_lim_write, druhy_lim = n2k_druhy_lim))
}

kuknal_raw <- nal_export(n2k_load, species_list, sites_subjects, limity, evl, rp_code, n2k_oop, current_year = current_year)
kuknal <- kuknal_raw$nal_write

#----------------------------------------------------------#
# Zapis mapy indikatoru ----
#----------------------------------------------------------#
#source("R/0_Config/01_n2k_map_ind.R")
#map <- build_indicator_map(
#  n2k_druhy,
#  limity,
#  script = "R/2_druhy/21_n2k_druhy_akce.R",
#  out_file = "Outputs/indicator_map_n2k.csv"
#)

#----------------------------------------------------------#
# Zapis dat lok -----
#----------------------------------------------------------#
lok_export <- function(
    species_list,
    sites_subjects,
    limity,
    evl,
    n2k_druhy_obdobi_lok,
    rp_code,
    n2k_oop,
    current_year = 2024,
    input_path = "Data/Temp/n2k_druhy_lim.csv",
    n2k_druhy_lim_data = NULL
) {

  #--------------------------------------------------#
  # 1. Nacteni dat (prednostne z pameti, jinak z disku) ----
  #--------------------------------------------------#
  # Pokud volajici jiz ma vysledek predchozi faze v pameti (napr. primo z
  # nal_export ve stejnem behu), pouzije se rovnou - usetri se tim zapis a
  # opetovne cteni potencialne velkeho CSV z disku. CSV na disku (viz nal_export)
  # zustava k dispozici pro samostatne/opakovane spusteni teto faze.
  if (!is.null(n2k_druhy_lim_data)) {
    n2k_druhy_lim <- n2k_druhy_lim_data
  } else {
    if (!file.exists(input_path)) {
      stop(paste0("Input file not found: ", input_path))
    }
    n2k_druhy_lim <- readr::read_csv(input_path, show_col_types = FALSE)
  }

  progress_line("--- ZACINAM VYPOCET FAZE 3 (LOKALITY) ---")

  N_species <- length(species_list)

  #--------------------------------------------------#
  # 2. Vypocet - Agregace na uroven lokality ----
  #--------------------------------------------------#

  n2k_druhy_lim_split <- split(n2k_druhy_lim, as.character(n2k_druhy_lim$DRUH))

  run_one_lok <- function(sp, data_subset) {
    run_n2k_druhy_lok(data_subset, sp, sites_subjects, limity, current_year = current_year)
  }

  n2k_druhy_lok <- run_over_species(species_list, n2k_druhy_lim_split, run_one_lok, "3/4 Lokality")

  if (is.null(n2k_druhy_lok) || nrow(n2k_druhy_lok) == 0) {
    warning("Zadna data nebyla vygenerovana (n2k_druhy_lok je prazdne). Export se neprovede.")
    return(NULL)
  }

  # --- UKLADANI TEMP DAT ---
  # Ulozeni mezivysledku pro dalsi funkce
  progress_line("--- UKLADAM MEZIVYSLEDEK DO TEMP ---")

  # Ujistime se, ze existuje slozka (i kdyz pro cteni existovala)
  if (!dir.exists("Data/Temp/")) {
    dir.create("Data/Temp/", recursive = TRUE)
  }

  readr::write_csv(
    n2k_druhy_lok,
    paste0("Data/Temp/n2k_druhy_lok", ".csv")
  )

  #--------------------------------------------------#
  # 3. Propojeni s metadaty a serazeni sloupcu ----
  #--------------------------------------------------#

  progress_line("--- PRIPRAVA DAT PRO EXPORT ---")

  n2k_druhy_lok_write <- n2k_druhy_lok %>%
    # Pripojeni informaci o EVL
    dplyr::left_join(
      .,
      evl %>%
        sf::st_drop_geometry() %>%
        dplyr::select(SITECODE, NAZEV),
      by = c("kod_chu" = "SITECODE")
    ) %>%
    # Pripojeni informaci o obdobich
    dplyr::left_join(
      .,
      n2k_druhy_obdobi_lok,
      by = dplyr::join_by("kod_chu", "KOD_LOKAL", "POLE", "DRUH")
    ) %>%
    # Pripojeni kodu RP
    dplyr::left_join(
      .,
      rp_code,
      by = dplyr::join_by("kod_chu")
    ) %>%
    # Pripojeni informaci o OOP
    dplyr::left_join(
      .,
      n2k_oop,
      by = c("kod_chu" = "SITECODE")
    ) %>%
    dplyr::distinct() %>%
    # Logicke serazeni sloupcu
    dplyr::select(
      # 1. Identifikace (Kde, Kdo, Kdy)
      ROK,
      kod_chu,
      NAZEV,          # Nazev EVL
      DRUH,
      SKUPINA,

      # 2. Lokalita a Odpovednost
      KOD_LOKAL,
      NAZEV_LOK,
      POLE,
      pracoviste,
      oop,

      # 3. Obdobi a Akce
      HODNOCENE_OBDOBI_OD,
      HODNOCENE_OBDOBI_DO,
      ID_ND_AKCE,
      DATUM,
      CILMON,

      # 4. Definice Indikatoru
      ID_IND,
      JEDNOTKA,
      TYP_IND,
      IND_GRP,
      KLIC,
      UROVEN,

      # 5. Vysledky a Limity
      LIM_IND,
      LIM_INDLIST,
      HOD_IND,
      STAV_IND,

      # Zbytek
      dplyr::everything()
    )

  #--------------------------------------------------#
  # 4. Export dat ----
  #--------------------------------------------------#

  sep_isop <- ";"
  quote_env_isop <- FALSE
  encoding_isop <- "UTF-8"

  sep <- ","
  quote_env <- TRUE
  encoding <- "Windows-1250"

  date_stamp <- gsub("-", "", Sys.Date())
  file_base <- paste0("Outputs/Data/druhy/n2k_druhy_lok_", current_year, "_", date_stamp)

  progress_line(paste0("--- EXPORTUJI SOUBORY DO: ", file_base, "... ---"))

  if (!dir.exists("Outputs/Data/druhy/")) {
    dir.create("Outputs/Data/druhy/", recursive = TRUE)
  }

  # Export 1: Windows-1250
  write_export_gz(
    n2k_druhy_lok_write,
    paste0(file_base, "_", encoding, ".csv.gz"),
    encoding = encoding,
    sep = sep,
    quote = quote_env
  )

  # Export 2: UTF-8
  write_export_gz(
    n2k_druhy_lok_write,
    paste0(file_base, "_", encoding_isop, ".csv.gz"),
    encoding = encoding_isop,
    sep = sep_isop,
    quote = quote_env_isop
  )

  progress_line("--- HOTOVO ---")

  return(list(lok_write = n2k_druhy_lok_write, druhy_lok = n2k_druhy_lok))
}

kuklok_raw <- lok_export(species_list, sites_subjects, limity, evl, n2k_druhy_obdobi_lok, rp_code, n2k_oop, current_year = current_year, n2k_druhy_lim_data = kuknal_raw$druhy_lim)
kuklok <- kuklok_raw$lok_write

#----------------------------------------------------------#
# Zapis dat uzemi -----
#----------------------------------------------------------#
chu_export <- function(
    species_list,
    sites_subjects,
    limity,
    biotop_evd,
    evl,
    rp_code,
    n2k_oop,
    indikatory_id,
    n2k_druhy_obdobi_chu,
    # Ciselnik metodik (Data/Input/cis_metodika.csv, sloupce druh + metodika).
    # Neni-li predan, pouzije se pro vsechny druhy zalozni kod METODIKA_VYCHOZI.
    cis_metodika = NULL,
    cilove_stavy = NULL,
    current_year = 2025,
    input_path = "Data/Temp/n2k_druhy_lok.csv",
    n2k_druhy_lok_data = NULL,
    pocetnost_path = "Data/Temp/n2k_druhy_pocetnost.csv",
    pocetnost_uzemi_data = NULL
) {

  if (!is.null(n2k_druhy_lok_data)) {
    n2k_druhy_lok <- n2k_druhy_lok_data
  } else {
    if (!file.exists(input_path)) {
      stop(paste0("Input file not found: ", input_path))
    }
    n2k_druhy_lok <- readr::read_csv(input_path, show_col_types = FALSE)
  }

  # Rada pocetnosti pro Tabulku 2 (viz faze 1b). Neni-li k dispozici, druhy
  # indikator Tabulky 2 se nepocita a uroven uzemi vychazi jen z LOK_PROCDOBR -
  # to je stav pred harmonizaci, proto jen varovani, ne chyba.
  if (!is.null(pocetnost_uzemi_data)) {
    pocetnost_uzemi <- pocetnost_uzemi_data
  } else if (file.exists(pocetnost_path)) {
    pocetnost_uzemi <- readr::read_csv(pocetnost_path, show_col_types = FALSE)
  } else {
    warning(glue::glue(
      "Soubor {pocetnost_path} neexistuje - druhy indikator Tabulky 2 ",
      "(pocetnost vs. cilovy stav) nebude vyhodnocen."
    ))
    pocetnost_uzemi <- NULL
  }

  progress_line("--- ZACINAM VYPOCET FAZE 4 (UZEMI/CHU) ---")
  N_species <- length(species_list)

  n2k_druhy_lok_split <- split(n2k_druhy_lok, as.character(n2k_druhy_lok$DRUH))

  run_one_uzemi <- function(sp, data_subset) {
    run_n2k_druhy_uzemi(
      n2k_druhy_lok = data_subset,
      species_name = sp,
      sites_subjects = sites_subjects,
      limity = limity,
      biotop_evd = biotop_evd,
      n2k_druhy_obdobi_chu = n2k_druhy_obdobi_chu,
      cilove_stavy = cilove_stavy,
      pocetnost_uzemi = pocetnost_uzemi,
      current_year = current_year
    )
  }

  n2k_druhy_uzemi <- run_over_species(species_list, n2k_druhy_lok_split, run_one_uzemi, "4/4 Uzemi")

  if (is.null(n2k_druhy_uzemi) || nrow(n2k_druhy_uzemi) == 0) {
    warning("Zadna data nebyla vygenerovana. Export se neprovede.")
    return(NULL)
  }

  progress_line("--- UKLADAM MEZIVYSLEDEK DO TEMP ---")
  if (!dir.exists("Data/Temp/")) dir.create("Data/Temp/", recursive = TRUE)
  readr::write_csv(n2k_druhy_uzemi, paste0("Data/Temp/n2k_druhy_chu", ".csv"))

  progress_line("--- PRIPRAVA DAT PRO EXPORT (CHU) ---")

  n2k_druhy_chu_write <- n2k_druhy_uzemi %>%
    # Pripojeni seznamu predmetu ochrany. Krome omezeni na oficialni dvojice
    # uzemi x druh se odsud bere i sdf_code = kod druhu podle Natura 2000
    # (SDF), ktery jde do exportu jako 'feature_code' - viz nize.
    dplyr::inner_join(
      sites_subjects %>%
        dplyr::select(site_code, nazev_lat, sdf_code) %>%
        dplyr::distinct(),
      by = c("kod_chu" = "site_code", "DRUH" = "nazev_lat")
    ) %>%
    # Pripojeni informaci o obdobich
    dplyr::left_join(
      .,
      n2k_druhy_obdobi_chu,
      by = dplyr::join_by("kod_chu", "DRUH")
    ) %>%
    dplyr::left_join(evl %>% sf::st_drop_geometry() %>% dplyr::select(SITECODE, NAZEV), by = c("kod_chu" = "SITECODE")) %>%
    dplyr::left_join(rp_code, by = dplyr::join_by("kod_chu")) %>%
    dplyr::left_join(n2k_oop, by = c("kod_chu" = "SITECODE")) %>%
    # feature_code = kod druhu podle Natura 2000 (SDF), napr. 1166 pro
    # Triturus cristatus, 1188 pro Bombina bombina. Drive se sem natvrdo
    # zapisovalo NA, prestoze hodnota je k dispozici v seznamu predmetu
    # ochrany - importni sablona i soubor, ktery ISOP prijal
    # (amp_evl_2024_20250908), ji maji vyplnenou.
    #
    # POZOR: musi to byt 'sdf_code', NE 'feature_code' ze sites_subjects -
    # ten nese Kod.ISOP (pro Triturus cristatus 21), coz je jiny ciselnik
    # a do importu by sel spatny kod.
    dplyr::mutate(
      typ_predmetu_hodnoceni = "Druh",
      feature_code = sdf_code,
      trend = "neznámý",
      datum_hodnoceni = Sys.Date()
    ) %>%
    dplyr::filter(CILMON_CHU == 1) %>%
    dplyr::rename(
      nazev_chu = NAZEV,
      druh = DRUH,
      hodnocene_obdobi_od = HODNOCENE_OBDOBI_OD,
      hodnocene_obdobi_do = HODNOCENE_OBDOBI_DO,
      parametr_nazev = ID_IND,
      parametr_hodnota = HOD_IND,
      parametr_limit = LIM_IND,
      parametr_jednotka = JEDNOTKA,
      stav = STAV_IND
    ) %>%
    dplyr::select(typ_predmetu_hodnoceni, kod_chu, nazev_chu, druh, feature_code, hodnocene_obdobi_od, hodnocene_obdobi_do, oop, parametr_nazev, parametr_hodnota, parametr_limit, parametr_jednotka, stav, trend, datum_hodnoceni, pracoviste, ID_ND_AKCE) %>%
    dplyr::left_join(indikatory_id, by = c("parametr_nazev" = "ind_r")) %>%
    # Pripojeni kodu metodiky z ciselniku (druh -> metodika).
    # Drive zde bylo natvrdo metodika = 15087 pro VSECHNY druhy, prestoze
    # Data/Input/cis_metodika.csv prirazeni druh -> metodika obsahuje a config
    # ho nacita. U obojzivelniku tak sla do ISOP cizi metodika.
    #
    # Ciselnik ma vyplnenou metodiku jen u obojzivelniku (22257) a cevnatych
    # rostlin (19192); u zbylych 12 skupin je sloupec prazdny. Proto coalesce
    # na METODIKA_VYCHOZI - jinak by ostatnim skupinam metodika zmizela.
    dplyr::left_join(
      if (is.null(cis_metodika)) {
        tibble::tibble(druh = character(0), metodika_cis = numeric(0))
      } else {
        cis_metodika %>%
          dplyr::select(druh, metodika_cis = metodika) %>%
          dplyr::filter(!is.na(metodika_cis)) %>%
          dplyr::distinct()
      },
      by = "druh"
    ) %>%
    # --- ZDE BYLA CHYBA: Pridano as.character() pro sjednoceni typu ---
    dplyr::mutate(
      parametr_nazev = dplyr::coalesce(as.character(ind_id), as.character(parametr_nazev)),
      pracoviste = gsub(",", "", pracoviste),
      metodika = dplyr::coalesce(metodika_cis, METODIKA_VYCHOZI)
    ) %>%
    dplyr::select(-c(ind_id, ind_popis, ID_ND_AKCE, metodika_cis)) %>%
    dplyr::distinct()

  sep_isop <- ";"
  quote_env_isop <- FALSE
  encoding_isop <- "UTF-8"
  sep <- ","
  quote_env <- TRUE
  encoding <- "Windows-1250"
  date_stamp <- gsub("-", "", Sys.Date())
  file_base <- paste0("Outputs/Data/druhy/n2k_druhy_chu_", current_year, "_", date_stamp)

  progress_line(paste0("--- EXPORTUJI SOUBORY DO: ", file_base, "... ---"))
  if (!dir.exists("Outputs/Data/druhy/")) dir.create("Outputs/Data/druhy/", recursive = TRUE)
  write_export_gz(n2k_druhy_chu_write, paste0(file_base, "_", encoding, ".csv.gz"), encoding = encoding, sep = sep, quote = quote_env)
  write_export_gz(n2k_druhy_chu_write, paste0(file_base, "_", encoding_isop, ".csv.gz"), encoding = encoding_isop, sep = sep_isop, quote = quote_env_isop)

  progress_line("--- HOTOVO (CHU_EXPORT) ---")
  return(n2k_druhy_chu_write)
}

kukchu <-
  chu_export(
  species_list = species_list,
  sites_subjects = sites_subjects,
  limity = limity,
  biotop_evd = biotop_evd,
  evl = evl,
  rp_code = rp_code,
  n2k_oop = n2k_oop,
  indikatory_id = indikatory_id,
  n2k_druhy_obdobi_chu = n2k_druhy_obdobi_chu, # Zde predavame vypoctena obdobi
  # Ciselnik metodik (viz 00_n2k_config.R). Bez nej by vsem druhum zustal
  # zalozni kod METODIKA_VYCHOZI.
  cis_metodika = if (exists("cis_metodika")) cis_metodika else NULL,
  # Cilove stavy pro druhy indikator Tabulky 2 (viz 00_n2k_config.R).
  # Bez nich se druhy indikator nevyhodnocuje a uroven uzemi vychazi jen
  # z LOK_PROCDOBR - proto se predava i zde, nejen v run_one_uzemi().
  cilove_stavy = if (exists("cilove_stavy")) cilove_stavy else NULL,
  current_year = current_year,
  n2k_druhy_lok_data = kuklok_raw$druhy_lok
)
