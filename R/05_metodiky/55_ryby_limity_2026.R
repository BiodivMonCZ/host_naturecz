#----------------------------------------------------------#
# Navrh limitu ryb a mihuli - metodika EVL 2026 -----
#----------------------------------------------------------#
# Prevadi pravidla metodiky EVL 2026 (L. Hanel, M. Muska) pro tri druhy
# - Cottus gobio, Lampetra planeri, Misgurnus fossilis - do formatu, ktery
# cte pipeline (limity_ryby.csv), a zapisuje novy ciselnik delkovych skupin.
#
# Spousti se z KORENE repozitare:
#   Rscript R/05_metodiky/55_ryby_limity_2026.R
#
# VSTUPY (verzovane, jen cteni):
#   git <ZAKLAD_DIFF>:Data/Input/limity_ryby.csv            limity pred metodikou 2026
#   git <ZAKLAD_DIFF>:Data/Input/cis_ryby_delky_strukt.csv  delky pred metodikou 2026
#   Data/Input/cis_indikatory_popis.csv     kontrola pokryti ciselnikem ISOP
# VSTUP VOLITELNY (neverejny):
#   <slozka_lokal>/export_data_evl.csv      slovnik hodnot a citlivost prevodu
#                                           pasem; bez nej se kontrola preskoci
#
# VYSTUPY (generovane, prepisuji se):
#   Data/Input/limity_ryby.csv                            ZIVE limity ryb
#   Data/Input/cis_ryby_delky_strukt.csv                  ZIVY ciselnik delek
#   Metodiky/Ryby a mihule/limity_ryby_2026_diff.csv      rozdil proti limitum pred 2026
#   Metodiky/Ryby a mihule/limity_ryby_2026_kontrola.md   protokol prevodu
#
# Skript PREPISUJE zive soubory (rozhodnuti Z-13, 2026-11-05; obdoba
# 54_cevnate_limity_2026.R). Zaklad - radky ostatnich druhu a diff - se cte
# z gitu (ZAKLAD_DIFF), takze opakovany beh dava tentyz vysledek. Nalezy
# a rozhodnuti, ze kterych prevod vychazi, jsou v
# Metodiky/Ryby a mihule/harmonizace_registr.md (H-71 ...) a
# Metodiky/Ryby a mihule/revize_metodika_ryby.md (PR-xx, R-xx, Z-xx).
#
# FORMAT (shodny s limity_ryby.csv):
#   Windows-1250, oddelovac ";", CRLF, bez uvozovek, bez BOM, sloupce
#   DRUH;ID_IND;TYP_IND;LIM_IND;JEDNOTKA;KLIC;UROVEN, chybejici hodnota "NA".
#   Radky ostatnich druhu se kopiruji BAJT PO BAJTU (vcetne puvodniho
#   michani "NA" a prazdnych poli); blok tri druhu se nahrazuje na miste
#   prvniho puvodniho radku techto druhu.
#   TYP_IND min = hodnota >= limit, max = hodnota <= limit, val = jeden radek
#   na kazdou pripustnou hodnotu, info = bez limitu (jen zobrazeni).
#   Ostre nerovnosti format nezna (viz protokol, prah "> 50 %").
#----------------------------------------------------------#

#----------------------------------------------------------#
# Nacteni knihoven -----
#----------------------------------------------------------#
packages <- c("dplyr", "tidyr", "stringr", "readr", "tibble")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

if (!file.exists("R/05_metodiky/55_ryby_limity_2026.R")) {
  stop("Skript se spousti z korene repozitare.")
}

#----------------------------------------------------------#
# Nastaveni -----
#----------------------------------------------------------#
ENC <- "CP1250"
METODIKA <- "metodika EVL 2026 (Hanel, Muška)"
ROZHODNUTI <- "rozhodnutí 2026-11-05"
DRUHY <- c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis")

# Posledni commit se zivymi soubory pred metodikou 2026 - zaklad radku
# ostatnich druhu i diffu.
ZAKLAD_DIFF <- "35a3d99"
z_gitu <- function(cesta) {
  f <- tempfile(fileext = ".csv")
  stav <- system2("git", c("-c", "core.autocrlf=false", "show", paste0(ZAKLAD_DIFF, ":", cesta)),
                  stdout = f, stderr = FALSE)
  if (!identical(as.integer(stav), 0L) || !file.exists(f) || file.size(f) == 0) {
    stop("Soubor nelze nacist z gitu: ", ZAKLAD_DIFF, ":", cesta)
  }
  f
}

VSTUP <- list(
  limity = z_gitu("Data/Input/limity_ryby.csv"),
  delky  = z_gitu("Data/Input/cis_ryby_delky_strukt.csv"),
  popis  = "Data/Input/cis_indikatory_popis.csv",
  export = "C:/Users/jonas.gaigr/Documents/host_data/export_data_evl.csv"
)
VYSTUP <- list(
  limity   = "Data/Input/limity_ryby.csv",
  delky    = "Data/Input/cis_ryby_delky_strukt.csv",
  diff     = "Metodiky/Ryby a mihule/limity_ryby_2026_diff.csv",
  kontrola = "Metodiky/Ryby a mihule/limity_ryby_2026_kontrola.md"
)

#----------------------------------------------------------#
# Navrh radku tri druhu -----
#----------------------------------------------------------#
# Sloupec `nalez` jde jen do diffu a protokolu, do limitu ne.
# POP_DYN zustava "min 50" (smer potvrzen textem, H-47); meni se vypocet
# v 21_1 (H-72, u Lampetra varianta C z H-73).
# EXPORTUJI SE JEN INDIKATORY METODIKY + PRITOMNOST (pokyn zadavatele
# 2026-11-05: "the only indicators that shall be exported are the ones
# described by the protocol + presence for info"). Vypusteny informativni
# radky POP_POCET, POP_VITALITA, POP_POCETODLOV, POP_POCETODLOVZDROJ,
# STA_PODMINKYODLOVU a STA_MIGBARPOCET. STA_MIGBARVYS zustava: je to vyska
# bariery na lokalite, ze ktere 25 pocita parametr metodiky STA_MIGPROSTUP
# (metodika 4.1 "sledovani pritomnosti migracnich barier na monitorovanych
# lokalitach"). Jednotky POCET pro abundanci jsou nove v 21_1
# (JEDNOTKY_POCET_RYBY_2026).
VEG_PASMA <- c("běžný", "hojný", "velmi hojný", "převažující", "dominantní")

radky_druh <- function(druh) {
  zaklad <- tibble::tribble(
    ~ID_IND,               ~TYP_IND, ~LIM_IND, ~JEDNOTKA,                                  ~KLIC, ~UROVEN, ~nalez,
    "POP_DYN",             "min",    "50",     "%",                                        "ano", "lok",   "H-72, H-73, H-47",
    "POP_VITALITA3",       "min",    "2",      "průzkumy se všemi délkovými skupinami",    "ano", "lok",   "H-79",
    "POP_PRESENCE",        "info",   "NA",     "přítomnost druhu",                         "NA",  "lok",   "H-80",
    "STA_MIGBARVYS",       "info",   "NA",     "cm",                                       "NA",  "lok",   "H-83",
    "LOK_PROCDOBR",        "min",    "75",     "% lokalit v dobrém stavu",                 "ano", "chu",   "H-81",
    "STA_MIGPROSTUP",      "max",    if (druh == "Lampetra planeri") "5" else "10", "cm",  "ano", "chu",   "H-83"
  )
  stan <- switch(druh,
    "Cottus gobio" = tibble::tibble(ID_IND = "STA_DNOTYPSOUCETPROCENT", TYP_IND = "min", LIM_IND = "50",
                                    JEDNOTKA = "%", KLIC = "ne", UROVEN = "lok", nalez = "H-76, H-77"),
    "Lampetra planeri" = tibble::tibble(
      ID_IND = c("STA_ODHADCELKOVEPLOCHYJEMNYCHNAPLAVUVHODNYCHPROVYVOJLAREV",
                 "STA_ODHADCELKOVEPLOCHYPROUDNYCHUSEKUSESTERKOPISKOVYMDNEMVHODNYCHPROTRENI"),
      TYP_IND = "min", LIM_IND = "10", JEDNOTKA = "m2 na 100 m toku", KLIC = "ne", UROVEN = "lok",
      nalez = "H-76, H-77, H-54"),
    "Misgurnus fossilis" = dplyr::bind_rows(
      tibble::tibble(ID_IND = "STA_DNOTYPSOUCETPROCENT", TYP_IND = "min", LIM_IND = "50",
                     JEDNOTKA = "%", KLIC = "ne", UROVEN = "lok", nalez = "H-76"),
      tibble::tibble(ID_IND = "STA_POKRVEGSUBMERZ", TYP_IND = "val", LIM_IND = VEG_PASMA,
                     JEDNOTKA = "pásmo pokryvnosti", KLIC = "ne", UROVEN = "lok", nalez = "H-76")))
  dplyr::bind_rows(zaklad[1:4, ], stan, zaklad[5:6, ]) %>%
    dplyr::mutate(DRUH = druh, .before = 1)
}
nove3 <- dplyr::bind_rows(lapply(DRUHY, radky_druh))

#----------------------------------------------------------#
# Cteni zivych limitu bajt po bajtu -----
#----------------------------------------------------------#
cti_radky <- function(cesta, enc) {
  b <- readBin(cesta, "raw", file.size(cesta))
  if (length(b) >= 3 && identical(b[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) stop("Necekany BOM: ", cesta)
  txt <- iconv(rawToChar(b), from = enc, to = "UTF-8")
  if (!endsWith(txt, "\r\n")) stop("Soubor nekonci CRLF: ", cesta)
  r <- strsplit(sub("\r\n$", "", txt), "\r\n", fixed = TRUE)[[1]]
  if (any(grepl("\n", r, fixed = TRUE))) stop("Smisene konce radku: ", cesta)
  list(hlavicka = r[1], telo = r[-1], bajty = b)
}
zapis_radky <- function(hlavicka, telo, cesta, enc) {
  txt <- paste0(paste(c(hlavicka, telo), collapse = "\r\n"), "\r\n")
  b <- iconv(txt, from = "UTF-8", to = enc, toRaw = TRUE)[[1]]
  if (is.null(b)) stop("Text nelze zakodovat do ", enc, ": ", cesta)
  writeBin(b, cesta)
}
# Nahradi blok druhu na miste prvniho jeho radku, ostatni radky zachova.
nahrad_blok <- function(telo, sep, druhy, novy_blok) {
  druh_radku <- sub(paste0(sep, ".*$"), "", telo)
  cil <- druh_radku %in% druhy
  if (!any(cil)) stop("V souboru nejsou radky druhu: ", paste(druhy, collapse = ", "))
  prvni <- which(cil)[1]
  c(telo[seq_len(prvni - 1)][!cil[seq_len(prvni - 1)]], novy_blok,
    telo[-seq_len(prvni)][!cil[-seq_len(prvni)]])
}

lim <- cti_radky(VSTUP$limity, ENC)
stopifnot(lim$hlavicka == "DRUH;ID_IND;TYP_IND;LIM_IND;JEDNOTKA;KLIC;UROVEN")
blok_lim <- do.call(paste, c(nove3 %>% dplyr::select(DRUH:UROVEN), sep = ";"))
if (any(grepl(";", nove3$JEDNOTKA, fixed = TRUE))) stop("Strednik v JEDNOTKA")
telo_nove <- nahrad_blok(lim$telo, ";", DRUHY, blok_lim)

del <- cti_radky(VSTUP$delky, "ASCII")
stopifnot(del$hlavicka == "DRUH,KAT,LIMIT,MIN,MAX,JEDNOTKA")
# Delkove skupiny podle metodiky (Tab. 1, pozn. a): "< 5 cm" je ostra nerovnost,
# delky jsou v celych mm, proto MAX 1. skupiny = 49.99 (21_1 paruje >= MIN, <= MAX).
DELKY_2026 <- tibble::tribble(
  ~DRUH,                ~KAT, ~MIN,     ~MAX,
  "Cottus gobio",       1,    "0.01",   "49.99",
  "Cottus gobio",       2,    "50",     "90",
  "Cottus gobio",       3,    "90.01",  "Inf",
  "Lampetra planeri",   1,    "0.01",   "49.99",
  "Lampetra planeri",   2,    "50",     "120",
  "Lampetra planeri",   3,    "120.01", "Inf",
  "Misgurnus fossilis", 1,    "0.01",   "49.99",
  "Misgurnus fossilis", 2,    "50",     "150",
  "Misgurnus fossilis", 3,    "150.01", "Inf"
)
blok_del <- paste(DELKY_2026$DRUH, DELKY_2026$KAT, "min", DELKY_2026$MIN, DELKY_2026$MAX, "mm", sep = ",")
del_nove <- nahrad_blok(del$telo, ",", DRUHY, blok_del)

#----------------------------------------------------------#
# Zapis a kontrola formatu -----
#----------------------------------------------------------#
zapis_radky(lim$hlavicka, telo_nove, VYSTUP$limity, ENC)
zapis_radky(del$hlavicka, del_nove, VYSTUP$delky, "ASCII")

kontrola_fmt <- list()
lim_nove <- cti_radky(VYSTUP$limity, ENC)
ost_stare <- lim$telo[!sub(";.*$", "", lim$telo) %in% DRUHY]
ost_nove <- lim_nove$telo[!sub(";.*$", "", lim_nove$telo) %in% DRUHY]
kontrola_fmt$ostatni_shodne <- identical(ost_stare, ost_nove)
# bajtova shoda radku ostatnich druhu (po zpetnem zakodovani)
kontrola_fmt$bajty_ostatnich <- identical(
  iconv(paste(ost_stare, collapse = "\r\n"), "UTF-8", ENC, toRaw = TRUE)[[1]],
  iconv(paste(ost_nove, collapse = "\r\n"), "UTF-8", ENC, toRaw = TRUE)[[1]])
kontrola_fmt$crlf <- all(!grepl("\n", lim_nove$telo, fixed = TRUE))
stopifnot(kontrola_fmt$ostatni_shodne, kontrola_fmt$bajty_ostatnich)

# Zpetne nacteni stejnym volanim jako 00_n2k_config.R
zpet <- readr::read_csv2(VYSTUP$limity, locale = readr::locale(encoding = "Windows-1250"), show_col_types = FALSE)
puv <- readr::read_csv2(VSTUP$limity, locale = readr::locale(encoding = "Windows-1250"), show_col_types = FALSE)
stopifnot(identical(names(zpet), names(puv)), nrow(zpet) == length(telo_nove))
# Kontrola H-53: vyplneny LIM_IND a prazdny KLIC nebo UROVEN
kontrola_fmt$h53 <- zpet %>% dplyr::filter(DRUH %in% DRUHY, !is.na(LIM_IND), is.na(KLIC) | is.na(UROVEN))
stopifnot(nrow(kontrola_fmt$h53) == 0)
zpet_del <- readr::read_csv(VYSTUP$delky, show_col_types = FALSE)
stopifnot(identical(names(zpet_del), names(readr::read_csv(VSTUP$delky, show_col_types = FALSE))))

#----------------------------------------------------------#
# Diff -----
#----------------------------------------------------------#
klic_radku <- function(df) df %>% dplyr::mutate(
  radek = paste(TYP_IND, dplyr::coalesce(as.character(LIM_IND), "NA"),
                dplyr::coalesce(JEDNOTKA, "NA"), dplyr::coalesce(KLIC, "NA"), sep = " | "))
stare3 <- puv %>% dplyr::filter(DRUH %in% DRUHY) %>% klic_radku()
nove3k <- zpet %>% dplyr::filter(DRUH %in% DRUHY) %>% klic_radku()
nalezy <- nove3 %>% dplyr::distinct(DRUH, ID_IND, UROVEN, nalez)
ODEBRANO_PROC <- c(
  STA_DNO = "H-76 - metodika nezna (C: soucet pokryvnosti; M: podil bahna a pisku)",
  STA_DNOPOCET = "H-77 - sirotek bez opory v metodice",
  STA_DNOPREF = "H-76 - metodika nezna",
  STA_PROUD = "H-76 - metodika nezna",
  STA_PROUDPOCETTYPU = "H-76 - metodika nezna",
  STA_DNOTYP = "H-45, H-76 - metodika nezna",
  STA_DNOPOCETTYPU = "H-76 - metodika nezna",
  STA_VEGETACE = "H-76 - nahrazeno STA_POKRVEGSUBMERZ",
  POP_POCET = "pokyn 2026-11-05 - export jen indikatoru metodiky; jednotky POCET v 21_1",
  POP_VITALITA = "H-79 - nahrazeno POP_VITALITA3; pokyn 2026-11-05 - export jen indikatoru metodiky",
  STA_MIGBARPOCET = "H-83 - pokyn 2026-11-05 - export jen indikatoru metodiky"
)
diff <- dplyr::full_join(
  stare3 %>% dplyr::group_by(DRUH, ID_IND, UROVEN) %>% dplyr::summarise(puvodni = paste(sort(unique(radek)), collapse = " || "), .groups = "drop"),
  nove3k %>% dplyr::group_by(DRUH, ID_IND, UROVEN) %>% dplyr::summarise(novy = paste(sort(unique(radek)), collapse = " || "), .groups = "drop"),
  by = c("DRUH", "ID_IND", "UROVEN")) %>%
  dplyr::left_join(nalezy, by = c("DRUH", "ID_IND", "UROVEN")) %>%
  dplyr::mutate(
    zmena = dplyr::case_when(is.na(puvodni) ~ "přidáno", is.na(novy) ~ "odebráno",
                             puvodni == novy ~ "beze změny", TRUE ~ "změněno"),
    nalez = dplyr::coalesce(nalez, unname(ODEBRANO_PROC[ID_IND])),
    pozn = dplyr::case_when(ID_IND == "POP_DYN" ~ "limit beze změny; mění se výpočet v 21_1",
                            ID_IND == "STA_DNOTYPSOUCETPROCENT" & DRUH == "Cottus gobio" ~ "dosud sirotek; výpočet nově v 21_1",
                            grepl("^STA_ODHAD", ID_IND) ~ "dosud sirotek; výpočet nově v 21_1 (na 100 m toku)",
                            TRUE ~ "")) %>%
  dplyr::arrange(DRUH, UROVEN, ID_IND) %>%
  dplyr::select(DRUH, UROVEN, ID_IND, zmena, puvodni, novy, nalez, pozn)
diff_del <- dplyr::full_join(
  readr::read_csv(VSTUP$delky, show_col_types = FALSE) %>% dplyr::filter(DRUH %in% DRUHY) %>% dplyr::transmute(DRUH, KAT, puvodni = paste0(MIN, " - ", MAX, " mm")),
  zpet_del %>% dplyr::filter(DRUH %in% DRUHY) %>% dplyr::transmute(DRUH, KAT, novy = paste0(MIN, " - ", MAX, " mm")),
  by = c("DRUH", "KAT"))

# diff ve stejne konvenci jako limity_cevky_2026_diff.csv (UTF-8 pro cteni v repozitari)
readr::write_csv(diff, VYSTUP$diff, na = "")

#----------------------------------------------------------#
# Kontrola proti datum NDOP (volitelne) -----
#----------------------------------------------------------#
PASMA <- tibble::tibble(
  klic = c("vzácný", "běžný", "hojný", "velmi hojný", "převažující", "dominantní"),
  stitek = c("Vzácný (0-10 %)", "Běžný (11-25 %)", "Hojný (26-50 %)", "Velmi hojný (51-75 %)", "Převažující (> 75 %)", "Dominantní (100%)"),
  dolni = c(0, 11, 26, 51, 76, 100), horni = c(10, 25, 50, 75, 99, 100), stred = c(5, 18, 38, 63, 87.5, 100))
data_k <- NULL
if (file.exists(VSTUP$export)) {
  ex <- readr::read_csv(VSTUP$export, locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE, guess_max = 1e5) %>%
    dplyr::filter(DRUH %in% DRUHY, PROJEKT == "Monitoring druhů ČR", !is.na(STRUKT_POZN)) %>%
    dplyr::mutate(ROK = as.numeric(substr(DATUM_OD, 7, 10))) %>% dplyr::filter(ROK >= 2013)
  tag <- function(x, t) { v <- stringr::str_squish(stringr::str_match(x, paste0("<", t, ">([^<]*)</", t, ">"))[, 2]); v[!is.na(v) & v == ""] <- NA; v }
  klic_p <- function(v) stringr::str_extract(tolower(v), "^(velmi hojný|vzácný|běžný|hojný|převažující|dominantní)")
  tagy_p <- c("rsub_kam", "rsub_bal", "rsub_st", "rsub_pis", "rsub_bah", "veg_tok_sub")
  data_k$stitky <- dplyr::bind_rows(lapply(tagy_p, function(t) tibble::tibble(tag = t, hodnota = tag(ex$STRUKT_POZN, t)))) %>%
    dplyr::filter(!is.na(hodnota)) %>% dplyr::count(tag, hodnota) %>%
    dplyr::mutate(pasmo = klic_p(hodnota), rozpoznano = !is.na(pasmo))
  # citlivost souctu: substrat neuvedeny v <sub_dno> = 0, uvedeny bez pasma -> NA
  sub_dno <- tolower(dplyr::coalesce(tag(ex$STRUKT_POZN, "sub_dno"), ""))
  souc <- function(tags, slova, co) {
    out <- 0
    for (i in seq_along(tags)) {
      p <- PASMA[[co]][match(klic_p(tag(ex$STRUKT_POZN, tags[i])), PASMA$klic)]
      p <- ifelse(is.na(p) & !grepl(slova[i], sub_dno, fixed = TRUE), 0, p)
      out <- out + p
    }
    ifelse(sub_dno == "", NA, pmin(out, 100))
  }
  cit <- function(druh, tags, slova, prah) {
    i <- ex$DRUH == druh
    s <- souc(tags, slova, "stred")[i]; d <- souc(tags, slova, "dolni")[i]; h <- souc(tags, slova, "horni")[i]
    tibble::tibble(druh = druh, zaznamu = sum(!is.na(s)), splneno_stredy = sum(s >= prah, na.rm = TRUE),
                   jiste_splneno = sum(d >= prah, na.rm = TRUE), jiste_nesplneno = sum(h < prah, na.rm = TRUE),
                   nejiste = sum(!is.na(s)) - sum(d >= prah, na.rm = TRUE) - sum(h < prah, na.rm = TRUE),
                   presne_prah = sum(s == prah, na.rm = TRUE))
  }
  data_k$citlivost <- dplyr::bind_rows(
    cit("Cottus gobio", c("rsub_kam", "rsub_bal", "rsub_st"), c("kameny", "balvany", "štěrk"), 50),
    cit("Misgurnus fossilis", c("rsub_bah", "rsub_pis"), c("bahno", "písek"), 50))
  vt <- tag(ex$STRUKT_POZN, "veg_tok"); vs <- klic_p(tag(ex$STRUKT_POZN, "veg_tok_sub"))
  data_k$vegetace <- tibble::tibble(DRUH = ex$DRUH, v = dplyr::case_when(!is.na(vs) ~ vs,
      !is.na(vt) & !grepl("submerz", vt, ignore.case = TRUE) ~ "bez vegetace", TRUE ~ NA_character_)) %>%
    dplyr::filter(DRUH == "Misgurnus fossilis") %>% dplyr::count(hodnota = v) %>%
    dplyr::mutate(splneno = dplyr::if_else(is.na(hodnota), "neznámý", as.character(hodnota %in% VEG_PASMA)),
                  hodnota = dplyr::coalesce(hodnota, "(tag chybí)"))
}

#----------------------------------------------------------#
# Protokol -----
#----------------------------------------------------------#
md_tab <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("_(žádné)_")
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), ~ stringr::str_replace_all(dplyr::coalesce(as.character(.), ""), "\\|", "\\\\|")))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    paste0("| ", do.call(paste, c(df, sep = " | ")), " |"))
}
souhrn <- diff %>% dplyr::count(DRUH, zmena) %>% tidyr::pivot_wider(names_from = zmena, values_from = n, values_fill = 0)

prot <- c(
  "# Protokol převodu limitů ryb a mihulí — metodika EVL 2026",
  "",
  paste0("**Generováno:** `R/05_metodiky/55_ryby_limity_2026.R` · ", format(Sys.Date()), " · ", METODIKA, ", ", ROZHODNUTI, "."),
  "**Registr:** [harmonizace_registr.md](harmonizace_registr.md) · **Revize:** [revize_metodika_ryby.md](revize_metodika_ryby.md)",
  "",
  "Skript zapisuje **živé** `limity_ryby.csv` a `cis_ryby_delky_strukt.csv` (rozhodnutí Z-13,",
  "2026-11-05). Řádky ostatních druhů a diff vycházejí ze stavu před metodikou 2026",
  paste0("(`", ZAKLAD_DIFF, "`). Nové indikátory počítá kód z patche"),
  "[patche/ryby_metodika_2026.patch](patche/ryby_metodika_2026.patch), aplikovaného týmž rozhodnutím.",
  "",
  "## Rozhodnutí zadavatele, ze kterých převod vychází (2026-11-05)",
  "",
  md_tab(tibble::tribble(
    ~`#`, ~Otázka, ~Rozhodnutí, ~`Dopad na limity`,
    "Z-2", "vzorec dynamiky *Lampetra*", "C — oba poslední odlovy pod polovinou výchozí hodnoty", "`POP_DYN min 50` beze změny; hodnota = 100 × max(dva poslední) / výchozí",
    "Z-3", "počty odlovů z `POZNAMKA`", "ano, přechodně", "info `POP_POCETODLOV`, `POP_POCETODLOVZDROJ`",
    "Z-4", "`POP_PRESENCE`", "`info`", "`POP_PRESENCE;info`",
    "Z-5", "prázdný tag bariér", "neznámo", "`STA_MIGPROSTUP` (chu) neznámý, není-li výška známa u všech lokalit",
    "Z-6", "EVL s 1–2 lokalitami", "doslovně 75 %", "`LOK_PROCDOBR min 75`; výjimka „při třech dvě“ v `25`",
    "Z-7", "sezóna a podmínky", "jen příznak", "info `STA_PODMINKYODLOVU`",
    "Z-11", "převod pásem", "potvrzeno, zapsat do protokolu", "viz níže",
    "Z-13", "aplikace návrhu", "ano", "zapsáno do živých souborů; samostatné `*_2026.csv` zrušeny",
    "KB3", "které indikátory exportovat", "jen indikátory metodiky + přítomnost (info)", "vypuštěny info řádky `POP_POCET`, `POP_VITALITA`, `POP_POCETODLOV`, `POP_POCETODLOVZDROJ`, `STA_PODMINKYODLOVU`, `STA_MIGBARPOCET`; `STA_MIGBARVYS` zůstává jako vstup `STA_MIGPROSTUP`"
  )),
  "",
  "## Převod pásem pokryvnosti na číslo (rozhodnutí Z-11)",
  "",
  "Data nesou pokryvnost substrátů (`<rsub_*>`) a vodní vegetace (`<veg_tok_sub>`) v šesti pásmech.",
  "Převod je sladěný s ostatními skupinami:",
  "",
  "- **obojživelníci (H-60):** kategorie relativní početnosti se převádí na **medián kategorie**;",
  "- **cévnaté rostliny (PR-12):** *součet* měr poškození přes typy se počítá ze **středů pásem**;",
  "  **(PR-16):** *jedno* pásmo stanovištního indikátoru se hodnotí **výčtem přípustných kategorií** (`val`).",
  "",
  "Pro ryby tedy:",
  "",
  md_tab(PASMA %>% dplyr::transmute(`pásmo v datech` = stitek, `klíč v kódu` = klic, `dolní mez` = dolni, `horní mez` = horni, `střed pro součet` = stred)),
  "",
  "1. **Součtový parametr** — *Cottus*: kameny + balvany + štěrk; *Misgurnus*: bahno + písek",
  "   (`STA_DNOTYPSOUCETPROCENT`). Hodnota = součet **středů** pásem; substrát, který v `<sub_dno>`",
  "   není uveden, = 0; substrát uvedený bez pásma ⇒ hodnota `NA` (neznámá); strop 100 %.",
  "   Limit `min 50`. Metodika *Cottus* §5.1 má „> 50 %“, Tab. 1 „minimálně 50 %“ (R-02); součet",
  "   středů tří pásem z {0, 5, 18, 38, 63, 87,5, 100} hodnotu přesně 50 dát nemůže, takže `min 50`",
  "   a „> 50“ dávají totéž a ostrá nerovnost, kterou formát nezná, není potřeba.",
  "2. **Jednopásmový parametr** — *Misgurnus* submerzní vegetace (`STA_POKRVEGSUBMERZ`). Hodnota =",
  "   klíč pásma; `<veg_tok>` bez submerzní vegetace = `bez vegetace`. Limit `val` = výčet pásem od",
  "   *Běžný (11-25 %)* výše. Práh 11 % (§5.1) leží **přesně na hranici pásem**, takže výčet je",
  "   přesný; práh 10 % (Tab. 1) by padl dovnitř pásma 0–10 % a rozhodnout jej nelze (R-02).",
  "3. **Plochy *Lampetra*** nejsou pásma: `odhad / délka úseku × 100` (m² na 100 m toku), limit `min 10`;",
  "   bez délky úseku ⇒ `NA`.",
  "",
  "### Kontrola slovníku a citlivost (Monitoring druhů ČR od 2013)",
  "",
  if (is.null(data_k)) "_Export NDOP nebyl k dispozici — kontrola přeskočena._" else c(
    "Rozpoznání štítků pásem (každý štítek v datech musí dát klíč):", "",
    md_tab(data_k$stitky %>% dplyr::select(tag, hodnota, pasmo, rozpoznano, zaznamu = n)), "",
    "Citlivost součtového parametru na volbu bodu uvnitř pásma (práh 50 %):", "",
    md_tab(data_k$citlivost), "",
    "„Nejisté“ = součet dolních mezí < 50 a součet horních mezí ≥ 50 — o těchto záznamech rozhoduje volba středu.", "",
    "*Misgurnus* — submerzní vegetace:", "",
    md_tab(data_k$vegetace)),
  "",
  "## Řádky tří druhů (metodika 2026)",
  "",
  md_tab(nove3 %>% dplyr::select(DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN, nalez)),
  "",
  paste0("## Souhrn rozdílu proti `limity_ryby.csv` před metodikou 2026 (`", ZAKLAD_DIFF, "`)"),
  "",
  md_tab(souhrn),
  "",
  md_tab(diff),
  "",
  "## Délkové skupiny (`cis_ryby_delky_strukt.csv`)",
  "",
  "Metodika Tab. 1, pozn. a. Hranice „< 5 cm“ je ostrá; délky jsou v celých mm, proto horní mez",
  "1. skupiny 49,99. Typ délky (SL × TL u *Cottus*) zůstává otázkou na autory (R-06).",
  "",
  md_tab(diff_del),
  "",
  "## Kontrola formátu",
  "",
  md_tab(tibble::tibble(
    kontrola = c("řádky ostatních druhů shodné (text)", "řádky ostatních druhů shodné (bajty CP1250)", "konce řádků CRLF",
                 "zpětné načtení `read_csv2` jako v `00_n2k_config.R`", "řádků nově / před metodikou 2026",
                 "H-53: vyplněný limit bez `KLIC`/`UROVEN`", "`ind_id` v `cis_indikatory_popis.csv` pro nové `ID_IND`"),
    vysledek = c(kontrola_fmt$ostatni_shodne, kontrola_fmt$bajty_ostatnich, kontrola_fmt$crlf, "OK (stejné sloupce)",
                 paste(length(telo_nove), "/", length(lim$telo)), paste(nrow(kontrola_fmt$h53), "řádků"),
                 { p <- readr::read_csv(VSTUP$popis, locale = readr::locale(encoding = "Windows-1250"), show_col_types = FALSE)
                   paste(sum(unique(nove3$ID_IND) %in% p$ind_r[!is.na(p$ind_id)]), "z", length(unique(nove3$ID_IND)), "má kód (H-87)") })
  ))
)
con <- file(VYSTUP$kontrola, open = "wb")
writeLines(enc2utf8(prot), con, sep = "\n", useBytes = TRUE)
close(con)

message("Hotovo: ", VYSTUP$limity, " (", length(telo_nove), " radku), ", VYSTUP$delky, ", ", VYSTUP$diff, ", ", VYSTUP$kontrola)
print(souhrn)
