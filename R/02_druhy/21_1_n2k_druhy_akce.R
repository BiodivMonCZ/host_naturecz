#----------------------------------------------------------#
# Pomocne funkce - stav vody (obojzivelnici) ----
#----------------------------------------------------------#
# Metodika (par. Sledovane indikatory, "Stav vody"): "Zaznamenava se mira
# zaplaveni dna DP v procentech, kde 100 % odpovida plne zaplavenemu dnu,
# tj. zaplaveni cele panve v jejim maximalnim rozsahu; 0 % odpovida zcela
# vyschle plose."
#
# NDOP ale tentyz udaj nese ve TRECH ruznych tvarech soucasne:
#   - holym cislem v procentech ...... "100", "80", "0"
#   - procentnim pasmem .............. "26-50 %", "0-25 %", "76-100 %"
#   - slovnim stavem ................. "vyschle", "zanikla", "zazemnena"
#
# Puvodni case_when znal jen pasma a slovni tvar "vyschla" (zensky rod), ktery
# se v datech NEVYSKYTUJE ANI JEDNOU - data pouzivaji tvar "vyschle". Vyschle
# plochy proto propadaly na vetev is.na(...) == FALSE ~ 0L, tedy "nevysycha".
# Mereno na exportu z NDOP (31 733 zaznamu 6 druhu metodiky) rozpoznaval
# puvodni prevod: STA_STAVVODATUNE 82,3 %, STA_STAVVODARYBNIK 56,5 %,
# STA_STAVVODAPERTUNE 29,8 %, STA_STAVVODALITORAL 0,0 % hodnot.
#
# norm_stavvody() prevadi vsechny tri tvary na jedno cislo = procento zaplaveni.
# U pasma se bere HORNI mez, aby zustalo zachovano puvodni chovani ("1-25 %"
# znamenalo vysychani, horni mez 25 <= 25 dava totez). Nerozpoznana hodnota
# vraci NA - NIKDY ne 0 ani 100, aby chybejici udaj nebyl vydavan za mereni.
# PRAH_VYSYCHANI: horni mez zaplaveni dna (v procentech), pri ktere se plocha
# jeste povazuje za vysychajici.
#
# Metodika (par. Sledovane indikatory, "Stav vody"): "0 % odpovida zcela
# vyschle plose." Prah je proto 0 - hodnoti se jen skutecne vyschla plocha.
#
# Drive zde bylo 25, prevzate ze stareho pasma "1-25 %", ktere puvodni prevod
# povazoval za vysychani. Rozhodnuti zadavatele 2026-08-31: rovnat se
# doslovnemu zneni metodiky (nalez H-30).
#
# POZOR na interakci s norm_stavvody(): u procentniho pasma se bere HORNI mez,
# takze zaznam "0-25 %" da 25 a pri prahu 0 se za vysychani NEPOVAZUJE,
# prestoze jeho dolni konec je nula. Tvary, ktere prah 0 zachyti, jsou slovni
# stavy (vyschle / zanikla / zazemnena -> 0) a hole cislo 0.
PRAH_VYSYCHANI <- 0

# Druhy resene metodikou obojzivelniku (Priloha 1: BBOM, BVAR, LMON, TRITURUS).
#
# Konstanta je na urovni SOUBORU, protoze ji potrebuje i 25_n2k_druhy_uzemi.R
# k omezeni Tabulky 2 (nalez H-50). 21_1 se ve 20_n2k_druhy_run.R sourcuje
# driv nez 25, takze je v okamziku pouziti dostupna.
DRUHY_METODIKY_OBOJ <- c(
  "Bombina bombina", "Bombina variegata", "Lissotriton montandoni",
  "Triturus cristatus", "Triturus carnifex", "Triturus dobrogicus"
)

#----------------------------------------------------------#
# Ryby a mihule - metodika EVL 2026 (Hanel, Muska) ----
#----------------------------------------------------------#
# Druhy, pro ktere plati metodika EVL 2026. Konstanta je na urovni SOUBORU,
# protoze ji pouziva i 24 (prah "zhorseny", nalez H-75) a 25 (uroven EVL,
# H-81, H-83). Ostatni druhy ryb zustavaji u dosavadniho vypoctu. Nalezy
# H-71 az H-85 jsou v Metodiky/Ryby a mihule/harmonizace_registr.md,
# rozhodnuti zadavatele 2026-11-05 (Z-1 az Z-11) v revize_metodika_ryby.md.
DRUHY_RYBY_2026 <- c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis")

# Jednotky POCET, ze kterych se u techto druhu pocita abundance (POP_POCET /
# plocha odlovu). Drive se braly z informativnich radku POP_POCET v limitech;
# ty byly odstraneny, protoze se exportuji jen indikatory metodiky a
# pritomnost (pokyn zadavatele 2026-11-05). Hodnoty = drivejsi radky limitu.
JEDNOTKY_POCET_RYBY_2026 <- list(
  "Cottus gobio"       = "jedinci",
  "Lampetra planeri"   = c("jedinci", "larvy"),
  "Misgurnus fossilis" = "jedinci"
)

# Pasma pokryvnosti v datech ("Vzacny (0-10 %)", "Bezny (11-25 %)" ...).
# Klic = slovni cast stitku, hodnota = STRED pasma pro soucty pres substraty
# (rozhodnuti 2026-11-05, Z-11; obdoba medianu kategorie u obojzivelniku,
# nalez H-60, a souctu stredu pasem poskozeni u cevnatych, PR-12).
PASMA_POKRYVNOSTI <- c(
  "vzácný" = 5, "běžný" = 18, "hojný" = 38, "velmi hojný" = 63,
  "převažující" = 87.5, "dominantní" = 100
)
pasmo_klic <- function(x) {
  stringr::str_extract(
    tolower(stringr::str_squish(as.character(x))),
    "^(velmi hojný|vzácný|běžný|hojný|převažující|dominantní)"
  )
}

# Substraty, jejichz relativni pokryvnost se scita (metodika, kap. 5.1):
#   Cottus gobio ....... kameny + balvany + sterk > 50 %
#   Misgurnus fossilis . bahno + pisek >= 50 % plochy dna
# Jmeno = tag <rsub_*>, hodnota = tvar v <sub_dno> po prevodu SLOVNIK_DNO.
SUBSTRATY_RYBY_2026 <- list(
  "Cottus gobio"       = c(rsub_kam = "kameny", rsub_bal = "balvany", rsub_st = "štěrk"),
  "Misgurnus fossilis" = c(rsub_bah = "bahno", rsub_pis = "písek")
)

# Soucet stredu pasem vybranych substratu (STA_DNOTYPSOUCETPROCENT, H-76):
#   substrat v <sub_dno> neuvedeny ..... 0
#   uvedeny, ale bez pasma <rsub_*> .... NA (soucet neznamy)
#   <sub_dno> chybi .................... NA
# Strop 100 %. Soucet stredu nikdy nevyjde presne 50, takze limit "min 50"
# odpovida i ostre nerovnosti "> 50 %" z textu metodiky Cottus (R-02).
soucet_pasem <- function(strukt, dno_mnozina, substraty) {
  n <- length(strukt)
  if (is.null(substraty)) return(rep(NA_real_, n))
  dno <- paste0(", ", ifelse(is.na(dno_mnozina), "", dno_mnozina), ", ")
  out <- rep(0, n)
  for (tg in names(substraty)) {
    p <- unname(PASMA_POKRYVNOSTI[pasmo_klic(tag_hodnota(strukt, tg))])
    uveden <- stringr::str_detect(dno, stringr::fixed(paste0(", ", substraty[[tg]], ", ")))
    out <- out + dplyr::case_when(!is.na(p) ~ p, !uveden ~ 0, TRUE ~ NA_real_)
  }
  out <- pmin(out, 100)
  out[is.na(dno_mnozina)] <- NA_real_
  out
}

# Submerzni vegetace (Misgurnus, STA_POKRVEGSUBMERZ, H-76): klic pasma
# <veg_tok_sub>; <veg_tok> bez submerzni vegetace -> "bez vegetace"; jinak NA.
# Limit je vycet pasem od "bezny (11-25 %)" vyse - prah 11 % z kap. 5.1 lezi
# presne na hranici pasem (R-02).
vegetace_submerzni <- function(strukt) {
  sub <- pasmo_klic(tag_hodnota(strukt, "veg_tok_sub"))
  veg <- tag_hodnota(strukt, "veg_tok")
  dplyr::case_when(
    !is.na(sub) ~ sub,
    !is.na(veg) & !grepl("submerz", veg, ignore.case = TRUE) ~ "bez vegetace",
    TRUE ~ NA_character_
  )
}

# Plocha v m2 na 100 m toku (Lampetra: jemne naplavy <odhad_j_n>, trdliste
# <odhad_p_u>; delka useku <delka_prolov_u>). Bez delky useku NA - v datech
# 91 z 419 zaznamu s odhadem, z plochy a sirky ji dopocitat nelze (H-77).
plocha_na_100m <- function(strukt, tag) {
  cislo <- function(v) suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE)))
  pl <- cislo(tag_hodnota(strukt, tag))
  de <- cislo(tag_hodnota(strukt, "delka_prolov_u"))
  dplyr::if_else(!is.na(pl) & !is.na(de) & de > 0, round(pl / de * 100, 2), NA_real_)
}

# Pocty ulovenych v 1. a 2. odlovu. Survey je do NDOP prenasi JEN TEXTEM
# v POZNAMKA ("Pocet ulovenych jedincu v prvnim lovu: 34; ..."), mezi tagy
# nejsou (nalez H-74). Rozhodnuti 2026-11-05 (Z-3): cist text prechodne, do
# zavedeni tagu. Mihule: minohy (larvy, metodika hodnoti "pocet larev"),
# ostatni: jedinci; spatreni neuloveni jedinci se nezapocitavaji.
odlov_z_poznamky <- function(poznamka, druh, lov) {
  co <- if (identical(as.character(druh), "Lampetra planeri")) "minoh" else "jedinců"
  kde <- if (lov == 1) "prvním" else "druhém"
  as.numeric(stringr::str_match(
    as.character(poznamka),
    paste0("ulovených ", co, " v ", kde, " lovu: *([0-9]+)")
  )[, 2])
}

# Podminky odlovu (informativni priznak STA_PODMINKYODLOVU, rozhodnuti
# 2026-11-05, Z-7 - nic se NEFILTRUJE; nalez H-84). Sezona orientacne podle
# kap. 3.1: Cottus a Misgurnus "koncem srpna az do zacatku listopadu"
# = 15. 8. - 15. 11., Lampetra "v zari a rijnu". Mesic*100 + den.
SEZONA_RYBY_2026 <- list(
  "Cottus gobio"       = c(od = 815, do = 1115),
  "Lampetra planeri"   = c(od = 901, do = 1031),
  "Misgurnus fossilis" = c(od = 815, do = 1115)
)
podminky_odlovu <- function(mesic, den, strukt, druh) {
  n <- length(mesic)
  sez <- SEZONA_RYBY_2026[[as.character(druh)]]
  if (is.null(sez) || n == 0) return(rep(NA_character_, n))
  cislo <- function(v) suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE)))
  md <- mesic * 100 + den
  tep <- dplyr::coalesce(cislo(tag_hodnota(strukt, "tep_vody")), cislo(tag_hodnota(strukt, "Teplota")))
  duvody <- cbind(
    ifelse(!is.na(md) & (md < sez[["od"]] | md > sez[["do"]]), "mimo sezónu", NA),
    ifelse(!is.na(tep) & tep < 5, "voda pod 5 °C", NA),
    ifelse(tag_hodnota(strukt, "vod_stav") %in% "Nadprůměrný", "zvýšený průtok", NA),
    ifelse(tag_hodnota(strukt, "typ_pruh") %in% "Dno není viditelné", "zákal", NA)
  )
  out <- vapply(seq_len(n), function(i) {
    r <- duvody[i, ]; r <- r[!is.na(r)]
    if (length(r)) paste(r, collapse = "; ") else "v pořádku"
  }, character(1))
  out[is.na(md) & is.na(strukt)] <- NA_character_
  out
}

# Dynamika populace podle metodiky EVL 2026 (H-72, H-73). Pocita se za
# lokalitu a ROK z rady ROCNICH hodnot (nejvyssi abundance v roce; u Misgurnus
# to metodika rika vyslovne), okno konci v hodnocenem roce.
#   Cottus, Misgurnus .. >= 6 let: prumer 3 poslednich / 3 prvnich;
#                        2-5 let: 2 posledni / 2 prvni (doslovne, R-11);
#                        1 rok: NA
#   Lampetra (Z-2 = C) . vychozi = 1. pruzkum, od 5. pruzkumu prumer prvnich
#                        ctyr; spatny, jsou-li OBA posledni pod polovinou
#                        vychozi -> hodnota = max(dva posledni) / vychozi;
#                        < 3 pruzkumy: NA
# Hodnota je procento vychozi hodnoty, limit "min 50" (H-47). Vychozi 0: je-li
# i aktualni 0 -> 100 (beze zmeny), jinak DYN_STROP - narust z nuly neni
# pokles; Inf by 21_2 jako cislo neprecetl (obdoba VITAL_STROP).
DYN_STROP <- 999999
dyn_rok <- function(a, druh) {
  n <- length(a)
  if (identical(as.character(druh), "Lampetra planeri")) {
    if (n < 3) return(NA_real_)
    ref <- if (n >= 5) mean(a[1:4]) else a[1]
    cur <- max(utils::tail(a, 2))
  } else {
    if (n < 2) return(NA_real_)
    k <- if (n >= 6) 3 else 2
    ref <- mean(utils::head(a, k))
    cur <- mean(utils::tail(a, k))
  }
  if (ref == 0) return(if (cur == 0) 100 else DYN_STROP)
  round(cur / ref * 100, 3)
}

# Vitalita podle metodiky EVL 2026 (H-79): pocet pruzkumu se VSEMI TREMI
# delkovymi skupinami mezi tremi poslednimi pruzkumy v ruznych letech (okno
# konci v hodnocenem roce); limit "min 2". Pruzkum = rok, kdy jsou z lokality
# delky, nebo kdy druh nebyl zjisten (0 skupin). Mene nez dva pruzkumy v okne
# -> NA (neznamy) - PREDBEZNE, metodika pripad neresi (otazka R-13 / A-4).
vitalita3 <- function(ok3, mereno) {
  vapply(seq_along(ok3), function(i) {
    j <- utils::tail(which(mereno[seq_len(i)]), 3)
    if (length(j) < 2) NA_real_ else sum(ok3[j])
  }, numeric(1))
}

#----------------------------------------------------------#
# Pomocne funkce - cevnate rostliny (metodika 2026-10-21) ----
#----------------------------------------------------------#
# HODNOCENE OBDOBI. Metodika cevnatych rostlin nezna pevne okno 12 let:
#   kap. 6   "u neaktualizovanych lokalit vstupuji do hodnoceni puvodni udaje"
#   kap. 5.1 zanikle lokality se zapocitavaji 10 let od posledniho
#            prokazatelneho vyskytu druhu (puchyrka nejmene 12 let)
#   kap. 7.1 cilova pocetnost = prumer PRVNICH tri monitoringu lokality,
#            vyvoj pocetnosti = tri posledni udaje
# Okno se proto u rostlin neuplatnuje na zaznamy, ale na LOKALITY: lokalita
# zustava v hodnoceni i s celou historii, dokud jeji posledni prokazatelny
# vyskyt neni starsi nez OKNO_LET z ciselniku cis_cevky_frekvence.csv.
# Rozhodnuti zadavatele 2026-10-28 (otazka 14), nalezy H-09, H-15.
#
# "Prokazatelny vyskyt" = pozitivni zaznam druhu (NEGATIVNI = 0 a POCET neni
# 0) z projektu Monitoring druhu CR, nebo z jineho projektu, je-li garantovan
# jako verohodny a ma presnost lokalizace do 1 km (definice indikatoru
# "Posledni dolozeny vyskyt", kap. 7.1). Nalez se s lokalitou paruje pres
# KOD_LOKAL; prostorovy prunik zakresu (kap. 7.1) tu neni - nalezy mimo
# monitoring kod lokality zpravidla nemaji (nalez H-01).
#
# Druhy mimo ciselnik (vsechny ostatni skupiny) maji dal pevne okno 12 let.
#
# Sezeni, ktere nacetlo config pred zavedenim ciselniku, ho nema - proto
# zalozni nacteni zde.
if (!exists("cis_cevky_frekvence")) {
  cis_cevky_frekvence <- readr::read_csv(
    "Data/Input/cis_cevky_frekvence.csv",
    locale = readr::locale(encoding = "Windows-1250"),
    show_col_types = FALSE
  )
}

# OKNO_LET druhu, NA pro druhy mimo ciselnik.
okno_cevky <- function(species_name) {
  i <- match(species_name, cis_cevky_frekvence$DRUH)
  if (is.na(i)) NA_integer_ else as.integer(cis_cevky_frekvence$OKNO_LET[i])
}

# Ponecha zaznamy lokalit, jejichz posledni prokazatelny vyskyt je nejvyse
# `okno` let stary ("po dobu deseti let od posledniho prokazatelneho vyskytu":
# vyskyt v roce 2015 se pri hodnoceni roku 2025 jeste zapocita).
vyber_obdobi_cevky <- function(df, okno, current_year) {
  presnost <- suppressWarnings(as.numeric(df$PRESNOST))
  posledni <- df %>%
    dplyr::mutate(PRESNOST_M = presnost) %>%
    dplyr::filter(
      NEGATIVNI == 0,
      is.na(POCET) | POCET != 0,
      PROJEKT == "Monitoring druhů ČR" |
        (GARANCE == "garantováno - věrohodný záznam" &
           !is.na(PRESNOST_M) & PRESNOST_M <= 1000)
    ) %>%
    dplyr::group_by(DRUH, KOD_LOKAL) %>%
    dplyr::summarise(ROK_POSLEDNI_VYSKYT = max(ROK, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(current_year - ROK_POSLEDNI_VYSKYT <= okno)
  df %>%
    dplyr::semi_join(posledni, by = c("DRUH", "KOD_LOKAL"))
}

# VITALITA (kap. 7.1): "podil kvetoucich a plodnych populacnich jednotek
# a sterilnich populacnich jednotek vyjadreny v procentech", tedy
# kvetouci a plodne / STERILNI * 100 - drive kvetouci a plodne / CELKEM
# (rozhodnuti zadavatele 2026-10-28, otazka 13).
#
# Kvetouci a sterilni jednotky nese NDOP v SAMOSTATNYCH zaznamech tehoz
# monitoringu, proto se vitalita pocita za celou akci. Ktere hodnoty POCITANO
# jsou kvetouci a ktere sterilni, urcuji info radky POP_POCETVITAL
# a POP_POCETSTERIL v limitech. Negativni zaznam dane kategorie = 0.
#
#   cela akce negativni ............ 0 (nulova pocetnost, beze zmeny)
#   chybi kvetouci nebo sterilni ... NA (indikator neznamy, kap. 7)
#   kvetouci i sterilni = 0 ........ NA (podil neni definovan)
#   sterilni = 0, kvetouci > 0 ..... VITAL_STROP - podil je nekonecny, tedy
#                                    splni kazdy limit typu min. Nekonecno
#                                    (Inf) ani zapis 1e+06 by 21_2 jako cislo
#                                    neprecetl (regex ^-?\d+(\.\d+)?$).
VITAL_STROP <- 999999

vitalita_cevky <- function(presence_n, pocitano, pocet, jedn_plod, jedn_steril) {
  n <- length(presence_n)
  if (n > 0 && all(!is.na(presence_n) & presence_n == 0)) {
    return(rep(0, n))
  }
  hodnota <- ifelse(!is.na(presence_n) & presence_n == 0, 0, as.numeric(pocet))
  soucet <- function(jedn) {
    i <- pocitano %in% jedn
    if (!any(i) || all(is.na(hodnota[i]))) NA_real_ else sum(hodnota[i], na.rm = TRUE)
  }
  p <- soucet(jedn_plod)
  s <- soucet(jedn_steril)
  v <- if (is.na(p) || is.na(s) || (p == 0 && s == 0)) {
    NA_real_
  } else if (s == 0) {
    VITAL_STROP
  } else {
    p / s * 100
  }
  rep(v, n)
}

# PLOSNE JEDNOTKY (Priloha 1; rozhodnuti zadavatele 2026-10-28, otazka 7):
# je-li populacni jednotkou druhu plocha (JEDNOTKA radku POP_POCET v limitech
# je "cm2", "dm2" nebo "m2"), prevedou se na ni zaznamy v jine plosne
# jednotce - napr. Cerastium alsinifolium cm2 a dm2 -> m2. "plocha populace
# (cm2)" je starsi zapis cm2. Neplosne jednotky (jedinci, ruzice...) -> NA.
PLOCHA_M2 <- c("cm2" = 1e-4, "dm2" = 1e-2, "m2" = 1)

koef_plochy <- function(pocitano, cil) {
  if (is.na(cil)) {
    return(rep(NA_real_, length(pocitano)))
  }
  zdroj <- stringr::str_match(pocitano, "^(?:plocha populace \\()?([cd]?m2)\\)?$")[, 2]
  unname(PLOCHA_M2[zdroj] / PLOCHA_M2[cil])
}

norm_stavvody <- function(x) {
  v <- stringr::str_squish(as.character(x))
  v[v == ""] <- NA_character_
  vl <- tolower(v)
  out <- rep(NA_real_, length(v))
  # slovni stavy = zcela bez vody
  out[!is.na(vl) & grepl("^(vyschl|zanikl|zazem)", vl)] <- 0
  # procentni pasmo "A-B" / "A-B %" -> horni mez
  i <- is.na(out) & !is.na(vl) & grepl("^[0-9]+ *- *[0-9]+ *%?$", vl)
  out[i] <- as.numeric(sub("^[0-9]+ *- *([0-9]+) *%?$", "\\1", vl[i]))
  # hole cislo, volitelne s procentem
  i <- is.na(out) & !is.na(vl) & grepl("^[0-9]+([.,][0-9]+)? *%?$", vl)
  out[i] <- as.numeric(sub(",", ".", sub(" *%$", "", vl[i]), fixed = TRUE))
  out[!is.na(out) & (out < 0 | out > 100)] <- NA_real_
  out
}

# Slovni stav plochy - odlisuje ZANIK/ZAZEMNENI (ztrata biotopu) od pouheho
# VYSCHNUTI (docasny stav). Osetruje oba rodove tvary, ktere se v datech
# vyskytuji ("vyschla" i "vyschle").
stav_vody_slovni <- function(x) {
  vl <- tolower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    !is.na(vl) & grepl("^vyschl", vl) ~ "vyschla",
    !is.na(vl) & grepl("^zanikl", vl) ~ "zanikla",
    !is.na(vl) & grepl("^zazem",  vl) ~ "zazemnena",
    TRUE ~ NA_character_
  )
}

# Klouzavy soucet pres POSLEDNI TRI MONITOROVANE SEZONY VCETNE aktualniho radku.
# Vstupem je vektor serazeny vzestupne podle roku v ramci jedne DP a druhu.
# Metodika pracuje se "tremi poslednimi sezonami s monitoringem dane DP", nikoli
# se tremi kalendarnimi roky - okno se proto pocita pres radky, ne pres roky.
# Vraci NA, pokud v okne neni ani jedna nechybejici hodnota.
roll3_sum <- function(x) {
  x <- as.numeric(x)
  x[is.infinite(x)] <- NA_real_
  vapply(seq_along(x), function(i) {
    v <- x[max(1L, i - 2L):i]
    if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
  }, numeric(1))
}

#----------------------------------------------------------#
# Pomocne funkce - stanovistni indikatory ryb a mihuli ----
#----------------------------------------------------------#
# Data ryb nesou strukturovane poznamky pod JINOU konvenci nez obojzivelnici:
# male zkracene tagy (<sub_dno>, <char_prou>, <tr_tok_char> ...) misto nazvu
# ID_IND. Kod je proto dlouho vubec necetl a 19 z 26 indikatoru v
# limity_ryby.csv zustavalo sirotky - viz nalez H-38 v harmonizace_registr.md.
#
# Samotna extrakce ale nestaci. 21_2_n2k_druhy_akce_lim.R paruje limity pres
# intersect(nazvy sloupcu, ID_IND limitu), takze VYTVORENIM SLOUPCE SE
# INDIKATOR ZAPNE - a kdyby se hodnoty netrefily do limitu, vratil by
# STAV_IND = 0, tedy nepriznivy stav, u vsech zaznamu. To je presne mechanismus
# nalezu H-01. Extrakce proto resi tri veci najednou (nalez H-42):
#
#   1. SLOVNIK. Limity pouzivaji kratke tvary ("kameny", "submerzní"), data
#      plne ("Kameny (6-25 cm)", "Submerzní"). Prevod je rizeny slovnikem nize
#      a jde smerem DATA -> SLOVNIK LIMITU; limity zustavaji nedotcene,
#      protoze jsou normativni artefakt.
#   2. VICEHODNOTOVOST. <sub_dno> a spol. jsou vyber z vice moznosti oddeleny
#      carkou a poradi NENI stabilni - <char_prou> ma v datech 101 ruznych
#      retezcu ze sesti kategorii. Hodnota se proto sklada jako SERAZENA
#      MNOZINA a porovnani typu "val" v 21_2 i 25 se rozsirilo na prislusnost.
#   3. PASMA MISTO PROCENT. STA_UPRAVABREHU a STA_UPRAVADNA maji limit
#      "max 49 %", data ale nesou pasma - viz uprava_procent() nize.
#
# Kategorie se NEHLEDAJI rozdelenim retezce podle carky, ale detekci znamych
# tvaru. Duvod: hodnota "Umělý substrát (dlažba, beton)" obsahuje carku uvnitr
# (45 zaznamu) a rozdeleni by ji roztrhlo na dve neexistujici kategorie, cimz
# by se nafoukl i pocet typu pro STA_DNOPOCETTYPU.

# Slovniky: jmeno prvku = tvar hledany v datech (mala pismena),
# hodnota = tvar pouzity v limity_ryby.csv.
#
# Dvojice s TYMZ cilem osetruji preklepy primo ve zdrojovych datech:
# "Mírny proud" (634 zaznamu) vedle spravneho "Mírný proud" (1 399)
# a "vodopad" vedle "Vodopád".
SLOVNIK_DNO <- c(
  "balvany"                 = "balvany",
  "kameny"                  = "kameny",
  "štěrk"                   = "štěrk",
  "písek"                   = "písek",
  "bahno"                   = "bahno",
  "kompaktní jílovité dno"  = "kompaktní jílové dno",
  "skalní podloží"          = "skalní podloží",
  "umělý substrát"          = "umělý substrát",
  "jiný"                    = "jiný"
)

SLOVNIK_PROUD <- c(
  "peřejnatý úsek"    = "peřeje",
  "mírný proud"       = "mírný",
  "mírny proud"       = "mírný",
  "tůně"              = "tůně",
  "stupně a kaskády"  = "kaskády",
  "vzdutí"            = "vzdutí",
  "vodopád"           = "vodopád",
  "vodopad"           = "vodopád"
)

SLOVNIK_VEGETACE <- c(
  "bez vegetace" = "bez vegetace",
  "submerzní"    = "submerzní",
  "emerzní"      = "emerzní",
  "plovoucí"     = "plovoucí"
)

# <zahl_kor> je jednohodnotovy; limit zna jen "přirozeně nízký", ostatni
# kategorie se prevadeji na kratky tvar, aby bylo ve vystupu videt, co bylo
# zaznamenano.
SLOVNIK_ZAHLOUBENI <- c(
  "přirozené nízké zahloubení" = "přirozeně nízký",
  "umělé střední zahloubení"   = "uměle střední",
  "umělé značné zahloubení"    = "uměle značné",
  "střední zahloubení"         = "střední",
  "značné zahloubení"          = "značné"
)

# Nejvyssi cislo v retezci (nalez H-52).
#
# <vyska_bar> uvadi u vice barier vsechny vysky oddelene carkou - 103 z 521
# hodnot, napr. "100, 300", "50, 50, 50" nebo "0, 5". Puvodni parse_number()
# vracel PRVNI cislo, takze se u "0, 5" vyhodnotila nulova bariera a
# peticentimetrova se ztratila, u "100, 300" naopak zmizela ta vyssi. Pro limit
# typu "max N cm" je rozhodujici bariera NEJVYSSI - ta urcuje pruchodnost useku.
#
# Desetinna carka tu nehrozi: v <vyska_bar> se carka mezi cislicemi nevyskytuje
# ANI JEDNOU (overeno na vsech 521 hodnotach), vzdy oddeluje jednotlive bariery.
# Rozsahy typu "20-30" nebo "151-200cm" davaji horni mez, coz je u vysky
# bariery konzervativni odhad spravnym smerem.
#
# ZNAME OMEZENI: jedina hodnota v datech uvadi metry ("více než 1 m") a vyjde
# z ni 1, tedy jako by slo o 1 cm. Prevod jednotek se kvuli jedinemu zaznamu
# nezavadi, ale je to duvod, proc se ma vyska barier zapisovat cislem v cm.
max_cislo <- function(x) {
  x <- as.character(x)
  vapply(seq_along(x), function(i) {
    if (is.na(x[i])) return(NA_real_)
    v <- stringr::str_extract_all(x[i], "[0-9]+")[[1]]
    if (length(v) == 0) NA_real_ else max(as.numeric(v))
  }, numeric(1))
}

# Vytazeni hodnoty tagu ze STRUKT_POZN. Prazdny retezec -> NA, aby se
# nevyplneny udaj nevydaval za mereni (tataz zasada jako u nalezu H-12).
tag_hodnota <- function(x, tag) {
  v <- stringr::str_match(
    as.character(x),
    paste0("<", tag, ">([^<]*)</", tag, ">")
  )[, 2]
  v <- stringr::str_squish(v)
  v[!is.na(v) & v == ""] <- NA_character_
  v
}

# Puvodni extrakce tagu, kterou pouziva vetsina indikatoru: hladovy vyber
# ".*" (pri dvou vyskytech tagu az po posledni uzaviraci tag), bez str_squish.
# tag_text() prevadi pres readr::parse_character() ("" i "NA" -> NA),
# tag_cislo() pres readr::parse_number() (prvni cislo v textu).
# Od tag_hodnota() se lisi vyberem i osetrenim mezer - sjednoceni by zmenilo
# hodnoty, proto zustavaji obe varianty.
tag_vyber <- function(x, tag) {
  stringr::str_extract(x, paste0("(?<=<", tag, ">).*(?=</", tag, ">)"))
}
tag_text  <- function(x, tag) readr::parse_character(tag_vyber(x, tag))
tag_cislo <- function(x, tag) readr::parse_number(tag_vyber(x, tag))

# Mnozina kategorii pritomnych v hodnote tagu, prevedena do slovniku limitu.
# Vraci retezec "a, b, c" se SERAZENYMI a odduplikovanymi polozkami, aby na
# poradi zapisu v datech nezalezelo. Nenajde-li se zadna znama kategorie,
# vraci NA (ne prazdny retezec), aby indikator zustal nehodnoceny.
#
# Kategorie se hledaji od NEJDELSIHO tvaru k nejkratsimu a kazdy nalezeny tvar
# se ze vstupu ODEBERE. Bez toho by se kratsi tvar trefil do tehoz useku textu
# jako delsi: "Umělé střední zahloubení (1-2 m)" obsahuje jako podretezec
# i "střední zahloubení" a vysledkem by byly DVE kategorie misto jedne.
# Odebiranim se zaroven nepokazi vicehodnotove tagy - odstraneni "kameny"
# z "Kameny (6-25 cm), Štěrk (0,2-6 cm)" ostatni polozky nijak nezasahne.
kat_mnozina <- function(x, slovnik) {
  # Zkratka pro skupiny bez techto tagu (obojzivelnici, hmyz, rostliny...):
  # kdyz je vstup cely NA, je NA i vysledek, takze se nemusi nic prochazet.
  # Kaskada bezi pres cca sto druhu, z nichz tyto tagy ma jen hrstka ryb.
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  vl <- tolower(ifelse(is.na(x), "", as.character(x)))
  klice <- names(slovnik)[order(nchar(names(slovnik)), decreasing = TRUE)]
  hodnoty <- unname(slovnik[klice])
  nalez <- matrix(FALSE, nrow = length(vl), ncol = length(klice))
  for (j in seq_along(klice)) {
    m <- stringr::str_detect(vl, stringr::fixed(klice[j]))
    nalez[, j] <- m
    vl[m] <- stringr::str_remove_all(vl[m], stringr::fixed(klice[j]))
  }
  vapply(seq_along(x), function(i) {
    if (is.na(x[i])) return(NA_character_)
    k <- sort(unique(hodnoty[nalez[i, ]]))
    if (length(k) == 0L) NA_character_ else paste(k, collapse = ", ")
  }, character(1))
}

# Pocet ruznych kategorii v mnozine vracene funkci kat_mnozina().
kat_pocet <- function(x) {
  ifelse(is.na(x), NA_real_, stringr::str_count(x, ", ") + 1)
}

# Dolni mez procentniho pasma. Data pouzivaji dve stupnice:
#   uprava brehu / dna ... "0-10%", "10-25%", "26-50%", "51-75%", ">75%",
#                          "Dominantní 100%"
#   substrat / proud ..... "Vzácný (0-10 %)", "Běžný (11-25 %)", ...
pasmo_dolni_mez <- function(x) {
  vl <- tolower(stringr::str_squish(as.character(x)))
  out <- rep(NA_real_, length(vl))
  out[!is.na(vl) & grepl("dominantn", vl)] <- 100
  i <- is.na(out) & !is.na(vl) & grepl("^>", vl)
  out[i] <- as.numeric(sub("^>[^0-9]*([0-9]+).*$", "\\1", vl[i]))
  i <- is.na(out) & !is.na(vl) & grepl("[0-9]+ *- *[0-9]+", vl)
  out[i] <- as.numeric(sub("^[^0-9]*([0-9]+) *- *[0-9]+.*$", "\\1", vl[i]))
  out
}

# Procento UPRAVENE casti brehu / dna, odvozene z pasma NEUPRAVENE casti.
#
#   souhrn ...... <breh_upr> / <upr_dno>, vycet typu uprav
#   pasmo ....... <breh_upr_bu> / <upr_dno_r_b_u>, pasmo podilu casti bez uprav
#   bez_uprav ... text, kterym se v souhrnu pozna kategorie "bez uprav"
#
# Overeno na datech (2 507 zaznamu se souhrnnym tagem): pasmo je vyplneno
# PRAVE TEHDY, kdyz souhrn kategorii "bez uprav" obsahuje - jinak je prazdne
# (267 zaznamu u brehu, 62 u dna) a nikdy nenese pasmo. Chybejici pasmo pri
# chybejici kategorii tedy znamena "neupraveno 0 %", ne "neznamo".
#
# Vraci HORNI mez upraveneho podilu, tj. 100 - dolni mez neupraveneho. Prah
# metodiky je "max 49 %" a lezi PRESNE na hranici pasem:
#   neupraveno 51-75 % -> upraveno nejvyse 49 % -> splneno
#   neupraveno 26-50 % -> upraveno nejmene 50 % -> nesplneno
# Volba bodu uvnitr pasma proto vysledek nemeni a nepredjima nerozhodnuty
# nalez H-26 (dolni mez vs. median kategorie).
uprava_procent <- function(souhrn, pasmo, bez_uprav) {
  ma_souhrn <- !is.na(souhrn)
  ma_bez <- ma_souhrn &
    stringr::str_detect(tolower(ifelse(is.na(souhrn), "", souhrn)),
                        stringr::fixed(bez_uprav))
  mez <- pasmo_dolni_mez(pasmo)
  dplyr::case_when(
    !ma_souhrn  ~ NA_real_,     # tag vubec nezaznamenan
    !ma_bez     ~ 100,          # zadna cast bez uprav -> upraveno cele
    !is.na(mez) ~ 100 - mez,    # zname pasmo neupravene casti
    TRUE        ~ NA_real_      # kategorie uvedena, ale pasmo nevyplneno
  )
}

run_n2k_druhy <- function(
    n2k_load,
    species_name,
    sites_subjects,
    limity,
    current_year = 2025
) {
  
  # Nektera pravidla nize plati POUZE pro druhy metodiky obojzivelniku -
  # sdileny kod obsluhuje i ryby, hmyz, savce a rostliny, kde se skala
  # pocetnosti i zdroje indikatoru lisi. Seznam je konstanta
  # DRUHY_METODIKY_OBOJ na zacatku souboru.
  je_obojzivelnik <- species_name %in% DRUHY_METODIKY_OBOJ

  # Pocitat populacni trendy? Ridi se VYHRADNE tabulkou limitu - trendovy blok
  # (POP_POCETMAXREF, POP_TREND1, POP_TREND2, POP_TREND, POP_TRENDLM) se
  # vyhodnocuje jen u druhu, ktere maji nektery z indikatoru POP_TREND* uvedeny
  # v limitech. Dnes je to 34 druhu cevnatych rostlin (limity_cevky.csv,
  # POP_TREND: max 1, KLIC = ano, UROVEN = lok).
  #
  # PROC: u obojzivelniku metodika zadny populacni trend nezna a v limitech pro
  # ne zadny radek POP_TREND* neni - hodnoty se pocitaly, prosly celou fazi 1
  # a teprve ve fazi 2 je zahodil right_join na limity. Slo tedy o praci navic
  # bez vlivu na vysledek, ktera navic svadela k tomu cist POP_TRENDLM jako
  # platny udaj. Regrese se u zaznamu bez ciselneho poctu pocitala z dosazenych
  # mezi kategorii, takze cislo bylo i vecne zavadejici.
  #
  # Podminka je zamerne datova, ne "neni obojzivelnik" - pribude-li trendovy
  # limit dalsi skupine, zacne se pocitat sam od sebe.
  pocitat_trend <- any(
    limity$DRUH == species_name &
      grepl("^POP_TREND", limity$ID_IND),
    na.rm = TRUE
  )
  if (!pocitat_trend) {
    message(glue::glue(
      "Druh {species_name}: zadny limit POP_TREND* - trendovy blok se nepocita."
    ))
  }

  # Kontrola ciselniku kategorii pocetnosti.
  # POP_POCETMIN a POP_POCETMAX se z nej odvozuji primo (viz nize), takze
  # chybejici sloupec by se jinak projevil az nesrozumitelnou chybou delky
  # uvnitr dplyr::if_else().
  if (!exists("cis_pocet_kat")) {
    stop("Objekt 'cis_pocet_kat' neexistuje - spustte R/00_config/02_n2k_data_druhy.R")
  }
  cis_pocet_kat_sloupce <- c("POP_POCETNOSTMAX", "POP_POCETNMIN", "POP_POCETNMAX")
  if (!all(cis_pocet_kat_sloupce %in% names(cis_pocet_kat))) {
    stop(
      "Ciselnik 'cis_pocet_kat' nema ocekavane sloupce (",
      paste(setdiff(cis_pocet_kat_sloupce, names(cis_pocet_kat)), collapse = ", "),
      ") - zkontrolujte Data/Input/cis_pocet_kat.csv."
    )
  }

  # Jednotky (POCITANO), ktere radky limitu druhu uznavaji pro dany indikator.
  # Radky bez limitu (info) urcuji, co se do indikatoru vubec zapocita.
  jednotky_limitu <- function(id) {
    limity %>%
      dplyr::filter(DRUH == species_name & ID_IND == id) %>%
      dplyr::pull(JEDNOTKA) %>%
      unique() %>%
      stringr::str_squish()
  }
  lim_pocet <- jednotky_limitu("POP_POCET")
  # Ryby metodiky EVL 2026: POP_POCET neni v limitech (neni indikatorem
  # metodiky), jednotky poctu jsou v JEDNOTKY_POCET_RYBY_2026.
  if (species_name %in% DRUHY_RYBY_2026 && length(lim_pocet) == 0) {
    lim_pocet <- JEDNOTKY_POCET_RYBY_2026[[species_name]]
  }
  # plosna populacni jednotka druhu (NA, neni-li jednotkou plocha) - viz koef_plochy
  cil_plocha <- intersect(lim_pocet, names(PLOCHA_M2))[1]
  lim_pocetsum <- jednotky_limitu("POP_POCETSUM")
  lim_repro <- jednotky_limitu("POP_REPRO")
  # kvetouci / sterilni casti pro vitalitu (vitalita_cevky)
  lim_vital <- jednotky_limitu("POP_POCETVITAL")
  lim_steril <- jednotky_limitu("POP_POCETSTERIL")
  lim_plocha <- jednotky_limitu("POP_PLOCHA")
  lim_lodyhy <- jednotky_limitu("POP_POCETSUMLOD")

  # Bez jednotek limitu zustane indikator NA u vsech zaznamu druhu.
  bez_jednotek <- lengths(list(
    POP_POCET = lim_pocet, POP_POCETSUM = lim_pocetsum, POP_REPRO = lim_repro
  )) == 0
  for (id in names(bez_jednotek)[bez_jednotek]) {
    warning(glue::glue("Druh {species_name}: limity neuvadeji jednotky {id} - indikator bude NA."))
  }

  # Hodnocene obdobi: cevnate rostliny podle lokalit (vyber_obdobi_cevky),
  # ostatni skupiny pevne 12 let.
  okno_druhu <- okno_cevky(species_name)
  
  #----------------------------------------------------------#
  # Nalez - priprava indikatoru na urovni nalezu ----- 
  #----------------------------------------------------------#
  n2k_druhy_pre <- n2k_load %>%
    # Filtrace dat: hodnocene obdobi - u cevnatych rostlin lokality
    # s prokazatelnym vyskytem v poslednich OKNO_LET letech i s celou
    # historii, jinak zaznamy z poslednich 12 let (vcetne aktualniho)
    {
      if (is.na(okno_druhu)) {
        dplyr::filter(., ROK >= current_year - 12)
      } else {
        vyber_obdobi_cevky(., okno_druhu, current_year)
      }
    } %>%
    # Filtrace dat: Ponechame pouze kombinace Druh a Lokalita, ktere jsou v seznamu sledovanych predmetu ochrany
    #
    # PAROVE (nalez H-85): drive se testoval druh a kod EVL ZVLAST, takze
    # prosly i zaznamy z EVL, kde druh predmetem ochrany neni (od 2013 napr.
    # 5 119 zaznamu letounu, 4 180 obojzivelniku, 1 265 ryb). Na urovni EVL
    # je odstranil az export (27), na urovni DP zustaly. Rozhodnuti zadavatele
    # 2026-11-05: opravit pro vsechny skupiny. Vyjimky beze zmeny.
    dplyr::filter(
      DRUH %in% c("Eriogaster catax", "Euphydryas aurinia") |
      paste(DRUH, kod_chu) %in% paste(sites_subjects$nazev_lat, sites_subjects$site_code)
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
    # Plocha v jine plosne jednotce -> POCET * POP_KOEFICIENT (napr. cm2 -> m2).
    # Jinak NA (pokud jednotka neodpovida metodice limitu).
    # POP_CILJEDNOTKA: plosna jednotka POP_POCET z limitu (NA u neplosnych)
    POP_CILJEDNOTKA = cil_plocha,
    # POP_KOEFICIENT: koeficient prepoctu POCITANO_CLEAN na POP_CILJEDNOTKA
    POP_KOEFICIENT = koef_plochy(POCITANO_CLEAN, cil_plocha),
    POP_POCET = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_pocet ~ POCET,
      !is.na(POP_KOEFICIENT) ~ POCET * POP_KOEFICIENT,
      TRUE ~ NA_real_
    ),
    # Ryby metodiky EVL 2026: pocet ulovenych v 1. a 2. odlovu z textu
    # POZNAMKA (H-74, Z-3), u mihule jen larvy. Kde text neni (pred 2019),
    # zustava POP_POCET; zdroj se nese v POP_POCETODLOVZDROJ.
    POP_POCETLOV1 = if (species_name %in% DRUHY_RYBY_2026) {
      odlov_z_poznamky(POZNAMKA, species_name, 1)
    } else NA_real_,
    POP_POCETLOV2 = if (species_name %in% DRUHY_RYBY_2026) {
      odlov_z_poznamky(POZNAMKA, species_name, 2)
    } else NA_real_,
    POP_POCETODLOV = dplyr::case_when(
      !(species_name %in% DRUHY_RYBY_2026) ~ NA_real_,
      POP_PRESENCE == "ne" ~ 0,
      !is.na(POP_POCETLOV1) & !is.na(POP_POCETLOV2) ~ POP_POCETLOV1 + POP_POCETLOV2,
      TRUE ~ POP_POCET
    ),
    POP_POCETODLOVZDROJ = dplyr::case_when(
      !(species_name %in% DRUHY_RYBY_2026) ~ NA_character_,
      POP_PRESENCE == "ne" ~ "negativní záznam",
      !is.na(POP_POCETLOV1) & !is.na(POP_POCETLOV2) ~ "POZNAMKA (1. + 2. odlov)",
      !is.na(POP_POCET) ~ "POCET",
      TRUE ~ NA_character_
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
      POCITANO_CLEAN %in% lim_plocha ~ POCET,
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
      # Metodika obojzivelniku: vyssi desitky = 51-100. Puvodne "> 51", takze
      # hodnota POCET == 51 nespadala nikam a koncila jako NA.
      POP_POCET >= 51 & POP_POCET <= 100 ~ 3,
      POP_RELPOC == "řádově nižší desítky" ~ 2,
      grepl("počet samců: řádově nižší desítky", POZN_TAX) ~ 2,
      # Metodika obojzivelniku definuje sestistupnovou skalu, kde nizsi desitky
      # jsou 11-50 a vyssi desitky 51-100. Kategorie NDOP "11-100" tuto hranici
      # PRESAHUJE - v exportu jde o 1 630 zaznamu. Rozhodnuti autoru metodiky
      # (2026-08-20): "bere nizsi kategorie, konzervativni predbezna opatrnost",
      # tedy 2. U ostatnich skupin (ryby, hmyz, savci, rostliny) zustava puvodni
      # zarazeni 3, aby se nezmenilo jejich hodnoceni.
      je_obojzivelnik & POP_RELPOC == "11-100" ~ 2,
      POP_RELPOC == "11-100" ~ 3,
      # Metodika: nizsi desitky = 11-50. Puvodne "> 10 & < 50", takze hodnota
      # POCET == 50 nespadala nikam a koncila jako NA.
      POP_POCET >= 11 & POP_POCET <= 50 ~ 2,
      POP_POCET > 0 & POP_POCET <= 10 ~ 1,
      POP_RELPOC == "do 10" ~ 1,
      POP_RELPOC == "1-10" ~ 1,
      # Mezerove varianty zapisu z NDOP - drive nerozpoznane, koncily jako NA
      # ("1 - 10" 79 zaznamu, "101 - 1000" 31, "1001 - 10 000" 1).
      POP_RELPOC == "1 - 10" ~ 1,
      POP_RELPOC == "101 - 1000" ~ 4,
      POP_RELPOC == "1001 - 10 000" ~ 5,
      grepl("počet samců: do 10", POZN_TAX) ~ 1
      ),
    # POP_PASTIPOCET: Extrakce poctu pasti ze strukturovane poznamky (XML tag)
    POP_PASTIPOCET = tag_cislo(STRUKT_POZN, "POP_PASTIPOCET"),
    # K DOŘEŠENÍ START !!!!!
    # POP_PLOCHALOV: Extrakce plochy odlovu ze strukturovane poznamky (XML tag)
    # POP_PLOCHALOV: plocha odlovu, jmenovatel abundance.
    #
    # KAZDA METODA ODLOVU MA VLASTNI TAG (nalez H-48). Vazba je v datech
    # naprosto tesna:
    #   <metod_lov> = "Kontinuální lov"     -> <plocha_prolov_p>    1 064 zaznamu
    #   <metod_lov> = "Lov bodovou metodou" -> <plocha_prolov_pbm>    625 zaznamu
    # Puvodni kod cetl jen prvni z nich, takze u vsech 625 pruzkumu bodovou
    # metodou zustala plocha NA -> abundance NA -> POP_DYN (KLICOVY indikator)
    # i cely trendovy blok NA. Chyba byla ticha, indikator se proste nevyhodnotil.
    #
    # PRIORITA: prednost ma <plocha_prolov_p>, <plocha_prolov_pbm> se pouzije jen
    # tam, kde prvni chybi. Oba tagy se v jednom zaznamu vyskytnou zaroven jen
    # jednou z 2 458, takze na poradi prakticky nezalezi - pravidlo je uvedeno
    # explicitne, aby bylo dohledatelne (obdoba pravidla priority u H-10).
    #
    # POZOR NA SROVNATELNOST: plocha prolovena bodovou metodou nemusi byt
    # metodicky srovnatelna s kontinualnim prolovem, takze trend slozeny
    # z obou metod muze byt zkresleny. Metoda se proto propisuje do sloupce
    # POP_METODALOV, aby to bylo ve vystupu videt.
    POP_METODALOV = tag_hodnota(STRUKT_POZN, "metod_lov"),
    POP_PLOCHALOV = {
      p   <- tag_hodnota(STRUKT_POZN, "plocha_prolov_p")
      pbm <- tag_hodnota(STRUKT_POZN, "plocha_prolov_pbm")
      v <- dplyr::coalesce(p, pbm)
      # desetinna carka: <plocha_prolov_pbm> ji pouziva u 81 hodnot
      v <- suppressWarnings(as.numeric(gsub(",", ".", v, fixed = TRUE)))
      # nulova plocha je chyba zapisu, ne mereni - delenim nulou by vznikl Inf
      dplyr::if_else(!is.na(v) & v > 0, v, NA_real_)
    },
    # POP_ABUNDANCENAL: Vypocet abundance (pocet jedincu / plocha odlovu)
    # U ryb metodiky EVL 2026 take z POCET (POP_POCET) prepocteneho na plochu -
    # rozhodnuti zadavatele 2026-11-05 ("for population metrics, use abundance -
    # POCET adjusted by the area"); pocty 1. a 2. odlovu z POZNAMKA (H-74, Z-3)
    # zustavaji jen informativne v POP_POCETODLOV.
    # Nulovy ulovek = nulova abundance i bez udaje o plose (nalez H-93):
    # 0 / NA dava NA, takze negativni pruzkum bez plochy z rady dynamiky
    # vypadl - u Misgurnus fossilis 65 zaznamu ciloveho monitoringu.
    POP_ABUNDANCENAL = if (species_name %in% DRUHY_RYBY_2026) {
      dplyr::if_else(POP_POCET %in% 0, 0, POP_POCET / POP_PLOCHALOV)
    } else {
      POP_POCET / POP_PLOCHALOV
    },
    ## Vlivy ----
    # Extrakce informaci o vlivech ze dvou ruznych XML tagu ve strukturovane poznamce
    vliv1 = tag_vyber(STRUKT_POZN, "vliv"),
    vliv2 = tag_vyber(STRUKT_POZN, "VLV_VLIVY")
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
      STA_SECCAS = tag_text(STRUKT_POZN, "sec_nacasovani"),
      # STA_SECMET: Extrakce metodiky sece (celoplosna?) ze strukturovane poznamky
      STA_SECMET = tag_text(STRUKT_POZN, "sec_celoplosna"),
      # STA_PRITOMNOSTROSTLIN: Extrakce pritomnosti zivnych rostlin (lisí se tag dle roku)
      STA_PRITOMNOSTROSTLIN = dplyr::case_when(
        ROK < 2021 ~ tag_text(STRUKT_POZN, "sta_pritomnostrostlin"),
        TRUE ~ tag_text(STRUKT_POZN, "STA_PRITOMNOSTROSTLIN")
      ),
      # Extrakce dalsich parametru stanoviste (zastineni, mrtve drevo, atd.)
      STA_JASANOKOLI = tag_text(STRUKT_POZN, "STA_JASANOKOLI"),
      STA_MIKRPROSTLINA = tag_text(STRUKT_POZN, "STA_MIKRPROSTLINA"),
      STA_VHSTROMN = tag_text(STRUKT_POZN, "STA_VHSTROMN"),
      STA_VHSTROMK = tag_text(STRUKT_POZN, "STA_VHSTROMK"),
      STA_PERSPEKTIVA = tag_text(STRUKT_POZN, "STA_PERSPEKTIVA"),
      STA_MRTDREVO = tag_text(STRUKT_POZN, "STA_MRTDREVO"),
      STA_TEKVODA = tag_text(STRUKT_POZN, "STA_TEKVODA"),
      STA_POKRYVNOSTDREVIN = tag_cislo(STRUKT_POZN, "STA_POKRYVNOSTDREVIN"),
      STA_ZAZEMNENI = tag_text(STRUKT_POZN, "STA_ZAZEMNENI"),
      STA_SKLADREVO = tag_text(STRUKT_POZN, "STA_SKLADREVO"),
      STA_LITVEGET = tag_cislo(STRUKT_POZN, "STA_LITVEGET"),
      STA_SIRKALIT = tag_cislo(STRUKT_POZN, "STA_SIRKALIT"),
      STA_ZASTIN = tag_text(STRUKT_POZN, "STA_ZASTIN"),
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
    STA_STAVVODARYBNIK = tag_text(STRUKT_POZN, "STA_STAVVODARYBNIK"),
    STA_STAVVODALITORAL = tag_text(STRUKT_POZN, "STA_STAVVODALITORAL"),
    STA_STAVVODATUNE = tag_text(STRUKT_POZN, "STA_STAVVODATUNE"),
    # STA_STAVVODAPERTUNE: periodicke tune. Puvodne se tento tag VUBEC NECETL,
    # pritom jde o 2 411 zaznamu - a zrovna o plochy, u nichz je vysychani
    # sledovanym jevem. Rozhodnuti autoru metodiky (2026-08-20): periodicke tune
    # se do hodnoceni vysychani zapocitavaji STEJNOU VAHOU jako trvale.
    STA_STAVVODAPERTUNE = tag_text(STRUKT_POZN, "STA_STAVVODAPERTUNE"),
    # STA_STAVVODA: Sjednoceni stavu vody
    # (priorita: tune -> periodicke tune -> litoral -> rybnik)
    STA_STAVVODA = dplyr::coalesce(
      STA_STAVVODATUNE,
      STA_STAVVODAPERTUNE,
      STA_STAVVODALITORAL,
      STA_STAVVODARYBNIK
    ),
    # STA_STAVVODAPROC: zaplaveni dna v procentech, sjednocene ze vsech tvaru
    # zapisu (holé cislo / pasmo / slovni stav) - viz norm_stavvody() nahore.
    STA_STAVVODAPROC = norm_stavvody(STA_STAVVODA),
    # STA_STAVVODASLOVNI: odlisuje zanik a zazemneni od pouheho vyschnuti
    STA_STAVVODASLOVNI = stav_vody_slovni(STA_STAVVODA),
    # STA_STAVVODAKAT: Prevod na ciselnou kategorii 0-5 dle procenta zaplaveni.
    # Prahy odpovidaji puvodnim pasmum (25 / 50 / 75 / 90 / 100).
    STA_STAVVODAKAT = dplyr::case_when(
      is.na(STA_STAVVODAPROC) ~ NA_integer_,
      STA_STAVVODAPROC <= 0   ~ 0L,
      STA_STAVVODAPROC <= 25  ~ 1L,
      STA_STAVVODAPROC <= 50  ~ 2L,
      STA_STAVVODAPROC <= 75  ~ 3L,
      STA_STAVVODAPROC <= 90  ~ 4L,
      TRUE                    ~ 5L
    ),
    # STA_VYSYCHANI: Indikator vysychani (1 = vysycha, 0 = nevysycha, NA = neznamo)
    # Prah viz konstanta PRAH_VYSYCHANI na zacatku souboru (dnes 0 % dle
    # doslovneho zneni metodiky).
    # NEROZPOZNANA HODNOTA ZUSTAVA NA - nikdy se nemapuje na "nevysycha".
    #
    # Indikator sam se NEHODNOTI proti limitu (nalezy H-01 a H-02) - slouzi
    # jako vstup pro tribety STA_VYSYCHANIPERIOD3 a jako informativni radek
    # ve vystupu, aby bylo videt, ktere roky byly suche (radek TYP_IND = "info"
    # v limity_vse.csv, viz H-31).
    STA_VYSYCHANI = dplyr::case_when(
      is.na(STA_STAVVODAPROC)             ~ NA_integer_,
      STA_STAVVODAPROC <= PRAH_VYSYCHANI  ~ 1L,
      TRUE                                ~ 0L
    ),
    # STA_MANIPULACE: Manipulace s vodni hladinou
    # Nazev sjednocen s ciselnikem cis_indikatory_popis.csv (ind_r = STA_MANIPULACE,
    # ind_id = 33). Metodika (par. Vyhodnoceni): "Hodnoti se tedy manipulace
    # od dubna do cervence."
    #
    # ZDROJ (nalez H-18): terenni cast metodiky uvadi "Zaznamenava se VE VLIVECH
    # v casti Voda", nikoli jako samostatny tag. Rozhodnuti autoru 2026-08-20:
    # "STA_MANIPULACE odvozovat podle metodiky", tj. z VLV_VLIVY.
    # Tag <STA_MANIPULACE> je ale PONECHAN jako druhy zdroj, protoze v exportu
    # je 25 z 57 jeho zaznamu "ano", ktere se ve VLV_VLIVY neobjevuji - vyrazenim
    # tagu bychom o ne prisli. Metodika (par. Vyhodnoceni) navic u stanovistnich
    # indikatoru rika, ze "do celkoveho hodnoceni vstupuje NEJHORSI pozorovana
    # hodnota", takze zaznam manipulace v kterémkoli ze zdroju je manipulace.
    #
    # VLV_VLIVY je seznam, jehoz nazvy kategorii samy obsahuji carky
    # ("abiotické přírodní procesy (eroze, zanášení, vysychání apod.)"), proto se
    # NIKDY nedeli podle carky, ale paruje se vzorem nad celym retezcem.
    STA_MANIPULACE_TAG = dplyr::na_if(
      stringr::str_squish(
        tag_text(STRUKT_POZN, "STA_MANIPULACE")
      ),
      ""
    ),
    STA_MANIPULACE_VLV = dplyr::case_when(
      is.na(stringr::str_squish(VLV_VLIVY)) |
        stringr::str_squish(VLV_VLIVY) == "" ~ NA_character_,
      grepl(
        "manipulace s vodní hladinou|regulování vodní hladiny|regulace vodní hladiny",
        VLV_VLIVY,
        ignore.case = TRUE
      ) ~ "ano",
      TRUE ~ "ne"
    ),
    # Sjednoceni obou zdroju + sezonni omezeni dle metodiky (duben az cervenec).
    # Prazdny retezec v tagu se diky na_if() vyse stal NA (nalez H-12) - drive
    # se 402 nevyplnenych zaznamu porovnavalo s limitem val "ne" a vychazelo
    # jako ZJISTENA MANIPULACE, tedy nepriznivy stav.
    STA_MANIPULACE = dplyr::case_when(
      !(MESIC >= 4 & MESIC <= 7) ~ NA_character_,
      STA_MANIPULACE_TAG %in% "ano" | STA_MANIPULACE_VLV %in% "ano" ~ "ano",
      !is.na(STA_MANIPULACE_TAG) | !is.na(STA_MANIPULACE_VLV) ~ "ne",
      TRUE ~ NA_character_
    ),
    # STA_ZTRATABIO: Indikator ztraty biotopu (zazemeni nebo zanik)
    # Nove pres stav_vody_slovni(), ktera osetruje oba rodove tvary
    # ("zanikla"/"zanikle", "zazemnena"/"zazemnene"). Zaverecna vetev
    # TRUE ~ "ne" je PONECHANA zamerne - indikator pouziva i Epidalea calamita
    # (limit val "ne") a zmena teto vetve na NA by zmenila jeji hodnoceni.
    STA_ZTRATABIO = dplyr::case_when(
      STA_STAVVODASLOVNI %in% c("zazemnena", "zanikla") ~ "ano",
      TRUE ~ "ne"
    ),
    # Extrakce dalsich parametru (kachny, ryby, zooplankton, vegetace, pruhlednost)
    STA_KACHNAPRITOMNOST = tag_text(STRUKT_POZN, "STA_KACHNAPRITOMNOST"),
    # STA_RYBY: Nadmerny tlak ryb.
    # POZOR: ve skutecnych datech (STRUKT_POZN) neexistuje tag <STA_RYBY> - overeno na
    # exportu z NDOP, kde je pole ulozeno pod tagem <STA_INVDRUHRYBA> (ano/ne), ktery se
    # u obojzivelniku tyka prakticky vyhradne tohoto indikatoru. Nazev promenne v kodu
    # (STA_RYBY) je zachovan, aby odpovidal ciselniku cis_indikatory_popis.csv (ind_id 32)
    # a tabulce limitu (limity_vse.csv), zdrojovy tag je ale STA_INVDRUHRYBA.
    # Domena dle metodiky (par. Sledovane indikatory): ano / ne / nelze vyloucit
    # / nehodnoceno. "nehodnoceno" znamena, ze plochu nebylo mozne metodicky
    # proverit (zejmena rybniky a velke tune) - jde tedy o NEZNAMY stav, ne
    # o nepriznivy, a normalizuje se na NA, aby indikator do hodnoceni nevstoupil.
    # "nelze vyloucit" se dle rozhodnuti zadavatele 2026-08-20 hodnoti jako
    # PRIZNIVY stav a je explicitne uvedeno v limity_vse.csv vedle "ne".
    # V dosavadnim exportu se vyskytuji jen hodnoty "ano" (427) a "ne" (3 018);
    # osetreni je tedy pripravou na prechod Survey123 na novou skalu.
    STA_RYBY = dplyr::na_if(
      dplyr::na_if(
        stringr::str_squish(
          tag_text(STRUKT_POZN, "STA_INVDRUHRYBA")
        ),
        ""
      ),
      "nehodnoceno"
    ),
    STA_ZOOPLANKTON = tag_text(STRUKT_POZN, "STA_ZOOPLANKTON"),
    STA_POKRVEGETACE = tag_text(STRUKT_POZN, "STA_POKRVEGETACE"),
    STA_PRUHLEDNOSTVODA = tag_text(STRUKT_POZN, "STA_PRUHLEDNOSTVODA"),
    STA_PRUHLEDNOSTVODAT = tag_text(STRUKT_POZN, "STA_PRUHLEDNOSTVODAT"),
    # STA_PRUHLEDNOSTVODAR: varianta pro rybniky. Puvodne se tento tag VUBEC
    # NECETL, pritom jde o 1 931 zaznamu - u rybnicnich DP tak pruhlednost
    # chybela, ackoli data existovala.
    STA_PRUHLEDNOSTVODAR = tag_text(STRUKT_POZN, "STA_PRUHLEDNOSTVODAR"),
    # Sjednoceni pruhlednosti vody.
    # Poradi priority: nejprve typove varianty (tune, rybnik), ktere nesou
    # kategorie ("nad 50 cm", "do 30 cm", "az na dno"), az pak obecny tag,
    # ktery nese hodnotu v centimetrech. Duvod: obe varianty se v jednom
    # zaznamu prakticky nekombinuji a puvodni kod uz davel prednost variante
    # STA_PRUHLEDNOSTVODAT pred obecnou - toto poradi je zachovano a jen
    # doplneno o rybnicni variantu.
    STA_PRUHLEDNOSTVODA = dplyr::coalesce(
      STA_PRUHLEDNOSTVODAT,
      STA_PRUHLEDNOSTVODAR,
      STA_PRUHLEDNOSTVODA
    ),
    # ZRUSENO 2026-08-20 (nalez H-15): puvodne zde bylo sezonni omezeni
    #   MESIC == 5 | (MESIC == 6 & DEN <= 15) ~ STA_PRUHLEDNOSTVODA,
    #   TRUE ~ NA_character_
    # ktere zahazovalo vsechny zaznamy mimo kveten az 15. cervna. Toto pravidlo
    # NEMA oporu v textu metodiky - par. Vyhodnoceni zadne casove omezeni
    # pruhlednosti neuvadi a terenni cast obsahuje pouze doporuceni k poradi
    # navstev ("vhodne predevsim pro zaznamenani pruhlednosti v reprezentativ-
    # nejsim obdobi"), nikoli limit pro vyhodnoceni. Rozhodnuti autoru metodiky
    # (2026-08-20): "zrus casove omezeni, ale popis zmenu".
    STA_UHYNOBOJZIVELNIK = tag_text(STRUKT_POZN, "STA_UHYNOBOJZIVELNIK"),
    STA_ZASTINENIHLADINA = tag_text(STRUKT_POZN, "STA_ZASTINENIHLADINA"),
    # STA_ZASTINENILITORAL: hodnoceny indikator dle Prilohy 1 revize metodiky
    # 2026-08-20 ("zastineni litoralu okolni vegetaci", spatne nad 75 % u BBOM
    # a BVAR). Pouziva se PRIMO, v surove podobe.
    #
    # ZRUSENO (nalez H-13): puvodne se zde STA_ZASTINENIHLADINA prepisovala horsi
    # z dvojice hladina/litoral. Po revizi metodiky je hodnocenym indikatorem
    # litoral a STA_ZASTINENIHLADINA uz nema radek v Priloze 1 ani limit - slo
    # tedy o mrtvy kod, ktery vyrabel sloupec, jejz uz nikdo nekonzumuje.
    # Zastineni vodni hladiny zustava terenne zaznamenavanym, ale NEHODNOCENYM
    # udajem (viz par. Sledovane indikatory).
    STA_ZASTINENILITORAL = tag_text(STRUKT_POZN, "STA_ZASTINENILITORAL"),
    # STA_PLOCHA50CM: Plocha s hloubkou mensi nez 50 cm (% aktualne zaplavene
    # plochy DP) - indikator Prilohy 1, hodnoceny u vsech 6 druhu.
    #
    # POZOR (nalez H-04): tento tag se v exportu z NDOP zatim NEVYSKYTUJE ANI
    # JEDNOU - overeno inventurou vsech tagu ve STRUKT_POZN u 31 733 zaznamu
    # 6 druhu metodiky. Sdeleni autoru metodiky 2026-08-20: tag bude
    # STA_PLOCHA50CM a indikator se bude hodnotit AZ OD ROKU 2027.
    # Do te doby zustava hodnota NA, takze indikator do poctu hodnocenych
    # indikatoru nevstupuje (par. Vyhodnoceni: "Indikator se hodnoti pouze,
    # jsou-li dostupne informace k jeho hodnoceni.").
    # Puvodni nazev byl STA_HLOUBKAMENSI20 (prah 20 cm), metodika ale prah
    # zmenila na 50 cm.
    STA_PLOCHA50CM = tag_cislo(STRUKT_POZN, "STA_PLOCHA50CM")
  ) %>%
    # ------------------------------------------#
    ## Ryby a mihule ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # Extrakce delek jedincu a migracnich barier
    POP_DELKYJEDINCI = tag_text(STRUKT_POZN, "velikosti"),
    STA_MIGBARPOCET = readr::parse_number(
      str_extract(
        STRUKT_POZN, 
        "(?<=<pocet_bar>)\\d+(?=</pocet_bar>)"
      )
    ),
    # STA_MIGBARVYS: vyska migracni bariery. Bere se NEJVYSSI z uvedenych
    # barier, ne prvni v poradi - viz max_cislo() a nalez H-52.
    STA_MIGBARVYS = max_cislo(
      str_extract(
        STRUKT_POZN,
        "(?<=<vyska_bar>)[^<]+(?=</vyska_bar>)"
      )
    ),
    # ----- Stanovistni indikatory ze zkracenych tagu (nalez H-42) -----
    # Pomocne funkce a slovniky viz zacatek souboru. Sloupce vznikaji pro
    # vsechny druhy, ale u skupin bez techto tagu zustanou NA a bez radku
    # v limitech je 21_2 stejne zahodi, takze mimo ryby a mihule nemaji efekt.

    # Substrat dna. Tentyz zdroj obsluhuje tri ID_IND, ktere se lisi jen tim,
    # ktere typy dna jsou pro dany druh prijatelne (STA_DNO, STA_DNOTYP a
    # STA_DNOPREF maji v limitech ruzne vycty, samotna namerena hodnota je
    # ale jedna a tataz).
    STA_DNO = kat_mnozina(tag_hodnota(STRUKT_POZN, "sub_dno"), SLOVNIK_DNO),
    STA_DNOTYP = STA_DNO,
    STA_DNOPREF = STA_DNO,
    STA_DNOPOCETTYPU = kat_pocet(STA_DNO),

    # Charakter proudeni.
    STA_PROUD = kat_mnozina(tag_hodnota(STRUKT_POZN, "char_prou"), SLOVNIK_PROUD),
    STA_PROUDPOCETTYPU = kat_pocet(STA_PROUD),

    # Vodni vegetace v toku.
    STA_VEGETACE = kat_mnozina(tag_hodnota(STRUKT_POZN, "veg_tok"), SLOVNIK_VEGETACE),

    # Trasa toku a variabilita hloubek jsou jednohodnotove a jejich limity uz
    # byly srovnany s domenou dat pri reseni nalezu H-34, takze se predavaji
    # beze zmeny.
    STA_TRASATOKU = tag_hodnota(STRUKT_POZN, "tr_tok_char"),
    STA_VARIABILITAHLOUBEK = tag_hodnota(STRUKT_POZN, "var_hl_pr"),

    # Zahloubeni koryta - jednohodnotove, prevedeno na kratky tvar limitu.
    STA_ZAHLOUBENIKORYTA = kat_mnozina(
      tag_hodnota(STRUKT_POZN, "zahl_kor"),
      SLOVNIK_ZAHLOUBENI
    ),

    # Podil upravene casti brehu a dna v procentech.
    STA_UPRAVABREHU = uprava_procent(
      tag_hodnota(STRUKT_POZN, "breh_upr"),
      tag_hodnota(STRUKT_POZN, "breh_upr_bu"),
      "bez známek úprav"
    ),
    STA_UPRAVADNA = uprava_procent(
      tag_hodnota(STRUKT_POZN, "upr_dno"),
      tag_hodnota(STRUKT_POZN, "upr_dno_r_b_u"),
      "bez úprav"
    ),

    # ----- Stanovistni parametry metodiky EVL 2026 (H-76, H-77) -----
    # Mimo DRUHY_RYBY_2026 zustavaji NA (vcetne STA_DNOTYPSOUCETPROCENT,
    # ktery byl dosud sirotkem bez vypoctu).
    STA_DNOTYPSOUCETPROCENT = soucet_pasem(
      STRUKT_POZN, STA_DNO, SUBSTRATY_RYBY_2026[[species_name]]
    ),
    STA_POKRVEGSUBMERZ = if (species_name %in% DRUHY_RYBY_2026) {
      vegetace_submerzni(STRUKT_POZN)
    } else NA_character_,
    STA_ODHADCELKOVEPLOCHYJEMNYCHNAPLAVUVHODNYCHPROVYVOJLAREV = if (species_name %in% DRUHY_RYBY_2026) {
      plocha_na_100m(STRUKT_POZN, "odhad_j_n")
    } else NA_real_,
    STA_ODHADCELKOVEPLOCHYPROUDNYCHUSEKUSESTERKOPISKOVYMDNEMVHODNYCHPROTRENI = if (species_name %in% DRUHY_RYBY_2026) {
      plocha_na_100m(STRUKT_POZN, "odhad_p_u")
    } else NA_real_,
    # Informativni priznak podminek odlovu (H-84, Z-7).
    STA_PODMINKYODLOVU = podminky_odlovu(MESIC, DEN, STRUKT_POZN, species_name)
  ) %>%
    # ------------------------------------------#
    ## Savci ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # Extrakce SCALP metodiky (rys ostrovid)
    POP_SCALP = tag_text(STRUKT_POZN, "SCALP"), 
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
    POP_POCETMIKROLOK = tag_cislo(STRUKT_POZN, "pocet_ml"),
    POP_TRENDBRY = tag_text(STRUKT_POZN, "trend_vyvoj"),
    POP_ZMENABRY1 = tag_text(STRUKT_POZN, "SP_POSKOZENI_ROSTLIN"),
    POP_ZMENABRY2 = tag_text(STRUKT_POZN, "str_strom"),
    POP_PLOCHAPOP = tag_text(STRUKT_POZN, "str_strom"),
    STA_DREVOBRY = tag_text(STRUKT_POZN, "SUB"),
    STA_STRUKTVEKBRY = tag_text(STRUKT_POZN, "str_strom"),
    STA_STRUKTDRUBRY = tag_text(STRUKT_POZN, "druh_strom")
  ) %>%
    # ------------------------------------------#
    ## Cévnaté rostliny ----- 
  # ------------------------------------------#
  dplyr::mutate(
    # POP_POCETLODYH: Pocet lodyh, pokud je druh pritomen a jednotka odpovida
    POP_POCETLODYH = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_lodyhy ~ POCET,
      TRUE ~ NA_real_
    ),
    # POP_POCETVITAL: Pocet vitalnich casti, pokud jednotka odpovida
    POP_POCETVITAL = dplyr::case_when(
      POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_vital ~ POCET,
      TRUE ~ NA_real_
    ),
    # POP_POCETSTERIL: Pocet sterilnich casti (jmenovatel vitality). Jen
    # zaznamy sterilni kategorie - negativni zaznam jine kategorie o poctu
    # sterilnich casti nic nerika (srov. nalez H-20).
    POP_POCETSTERIL = dplyr::case_when(
      POCITANO_CLEAN %in% lim_steril & POP_PRESENCE == "ne" ~ 0,
      POCITANO_CLEAN %in% lim_steril ~ as.numeric(POCET),
      TRUE ~ NA_real_
    ),
    # Extrakce parametru managementu a pokryvnosti (invazni, expanzni, dreviny, atd.)
    STA_MAN = tag_text(STRUKT_POZN, "MAN"),
    STA_MANPOTREBAVLIV = tag_text(STRUKT_POZN, "MAN_POTREBAVLIV"),
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
    STA_POKRYVNOSTINVAZNI = tag_text(STRUKT_POZN, "STA_POKRYVNOSTINVAZNI"),
    STA_POKRYVNOSTEXPANZNI = tag_text(STRUKT_POZN, "STA_POKRYVNOSTEXPANZNI"),
    STA_POKRSTAR = tag_text(STRUKT_POZN, "STA_POKRYVNOSTSTARINA"),
    STA_POKRDREV = tag_text(STRUKT_POZN, "STA_POKRYVNOSTDREVIN"),
    STA_POKRDREVNIZ = tag_text(STRUKT_POZN, "STA_POKRYVNOSTDREVINNIZ"),
    STA_POKRYVNOSTE2E3 = tag_text(STRUKT_POZN, "STA_POKRYVNOSTE2E3"),
    STA_POKRYVNOSTE1 = tag_text(STRUKT_POZN, "STA_POKRYVNOSTE1"),
    STA_POKRYVNOSTE0 = tag_text(STRUKT_POZN, "STA_POKRYVNOSTE0"),
    STA_POKRYVNOSTVOLNAPUDA = tag_text(STRUKT_POZN, "STA_POKRYVNOSTVOLNAPUDA"),
    STA_ZAPOJENICELK = tag_text(STRUKT_POZN, "STA_ZAPOJENICELK"),
    STA_ZAPOJENIKAT = readr::parse_character(stringr::str_extract(STRUKT_POZN, "(?<=<STA_ZAPOJENICELK>).*(?=:)")),
    STA_STUPENZACH = tag_text(STRUKT_POZN, "SP_STUPEN_ZACH_STAN"),
    STA_HYDRPOMERY = tag_text(STRUKT_POZN, "STA_HYDRPOMERY"),
    STA_VLHPOM = tag_text(STRUKT_POZN, "STA_VLHKOPOMERY"),
    STA_ZASTINENICELK = tag_text(STRUKT_POZN, "STA_ZASTINENICELK"),
    SP_POSKOZENI_ROSTLIN = tag_text(STRUKT_POZN, "SP_POSKOZENI_ROSTLIN"),
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
      # POP_POCETMIN / POP_POCETMAX: hranice kategorie pocetnosti
      #
      # Obe meze se berou z ciselniku Data/Input/cis_pocet_kat.csv (sloupce
      # POP_POCETNMIN a POP_POCETNMAX), klicem je kategorie POP_POCETNOSTNAL.
      # Drive byly obe skaly natvrdo vypsane v case_when a rozchazely se
      # s ciselnikem, ktery je definuje:
      #   - POP_POCETMIN mel u kategorie 3 hodnotu 50, ciselnik uvadi 51
      #     (50 patri jeste do kategorie 2)
      #   - POP_POCETMAX mel u kategorii 1, 2 i 3 shodne 10000 misto 10, 50
      #     a 100, a pro kategorie 6, 7 a 8 nemel vetev vubec, takze propadal
      #     na NA. POP_POCETMAX pritom vstupuje do POP_TRENDLM a POP_TREND1/2,
      #     ktere se tak u zaznamu bez ciselneho poctu pocitaly z konstanty.
      #
      # match() vraci pro neznamou nebo chybejici kategorii (vc. POP_POCETNOSTNAL
      # = 0, coz je nepritomnost druhu) NA, indexace pak da NA_real_ - tedy
      # stejny vysledek jako puvodni vetev TRUE ~ NA_real_.
      #
      # POZOR: semantika zustava zamerne NEZMENENA - dosazuje se DOLNI mez
      # kategorie, ne jeji median (POP_POCETSTRED). Zmena na median je
      # metodicke rozhodnuti, ne oprava chyby.
      POP_POCETMIN = dplyr::if_else(
        is.na(POP_POCET) == FALSE,
        as.numeric(POP_POCET),
        as.numeric(
          cis_pocet_kat$POP_POCETNMIN[
            match(POP_POCETNOSTNAL, cis_pocet_kat$POP_POCETNOSTMAX)
          ]
        )
      ),
      POP_POCETMAX = dplyr::if_else(
        is.na(POP_POCET) == FALSE,
        as.numeric(POP_POCET),
        as.numeric(
          cis_pocet_kat$POP_POCETNMAX[
            match(POP_POCETNOSTNAL, cis_pocet_kat$POP_POCETNOSTMAX)
          ]
        )
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
        POCITANO_CLEAN %in% lim_lodyhy ~ POCET,
        TRUE ~ NA_real_
      ),
      # POP_VITAL: kvetouci a plodne / sterilni * 100 za celou akci
      # (vitalita_cevky nahore; drive kvetouci a plodne / celkem po zaznamech).
      # POP_VITAL maji v limitech jen cevnate rostliny.
      POP_VITAL = vitalita_cevky(
        POP_PRESENCE_N, POCITANO_CLEAN, POCET, lim_vital, lim_steril
      )
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
      # CILMON_MAX: Byl v danem roce na teto DP cileny monitoring?
      # Pouzito jako referencni rok pro POP_ZMENARAD (viz nize)
      CILMON_MAX = max(CILMON, na.rm = TRUE),
      # POP_POCETSUMLOKAL: Soucet populace za lokalitu
      # DULEZITE: Pouzivame !duplicated(IDX_ND_AKCE) pro zamezeni nasobeni stejnych akci
      POP_POCETSUMLOKAL = sum(POP_POCET[!duplicated(IDX_ND_AKCE)], na.rm = TRUE),
      # POP_POCETMIN: Minimalni hodnota populace
      POP_POCETMIN = min(
        POP_POCET,
        na.rm = TRUE
      ),
      # Osetreni nekonecnych hodnot u minima.
      # min() nad samymi NA vraci Inf; bez tohoto radku se Inf propisovalo
      # do vystupu (73 ze 724 DP testovaciho behu). Bylo to videt az po
      # oprave H-35, drive byl cely indikator neviditelny.
      #
      # POZOR na asymetrii vuci maximu nize: tam se Inf prevadi na 0, zde na
      # NA. Nula by tvrdila "napocitano nula jedincu", zatimco skutecnost je
      # "pocet nebyl zaznamenan" - pro nove zviditelneny informativni radek
      # se proto pouziva NA = "neznamy". U maxima zustava puvodni prevod na 0,
      # protoze vstupuje do POP_TRENDLM a POP_TREND1/2 u 34 druhu cevnatych
      # rostlin - zmena by tam nebyla neutralni. Viz nalez H-36.
      POP_POCETMIN = ifelse(is.infinite(POP_POCETMIN), NA_real_, POP_POCETMIN),
      # POP_POCETMAX: Maximalni hodnota populace
      POP_POCETMAX = max(
        POP_POCET,
        na.rm = TRUE
      ),
      # Osetreni nekonecnych hodnot u maxima
      POP_POCETMAX = ifelse(is.infinite(POP_POCETMAX), 0, POP_POCETMAX),
      # POP_POCETNOST: Maximalni kategorie pocetnosti
      POP_POCETNOST = if (all(is.na(POP_POCETNOSTNAL))) {
        NA_real_ 
      } else {
        max(POP_POCETNOSTNAL, na.rm = TRUE)
      },
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
      # POP_VITALITAYOY: pritomnost letosniho pludku (nalez H-51).
      #
      # Dva druhy - Leuciscus aspius a Sabanejewia balcanica - maji limit
      # "min 1" s jednotkou "jedinci tohorocni", tedy doklad letosniho
      # rozmnozeni. POP_VITALITA ale vraci POCET DELKOVYCH KATEGORII, takze se
      # podminka "min 1" splnila vzdy, kdyz byla k dispozici jakakoli delka -
      # indikator meril neco jineho, nez tvrdila jeho jednotka.
      #
      # Zde se testuje to, co limit rika: pritomnost NEJMENSI delkove kategorie
      # (KAT = 1) z ciselniku cis_ryby_delky_strukt.csv. U vsech osmi druhu
      # v ciselniku je kategorie 1 nejnizsi velikostni trida, tedy letosni
      # pludek. Ostatnim druhum limit na tento indikator nevznika, takze se
      # u nich nevyhodnocuje.
      POP_VITALITAYOY = dplyr::case_when(
        POP_PRESENCE == "ne" ~ 0L,
        POP_VITALITA_N_CATS == 0 ~ NA_integer_,
        any(POP_DELKYJEDINCIKAT == 1, na.rm = TRUE) ~ 1L,
        TRUE ~ 0L
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

  # Lokalita - zmena kategorie pocetnosti (POP_ZMENARAD) -----
  # Metodika (obojzivelnici): "porovnani odhadovane pocetnosti" je klicovy populacni
  # indikator - nepriznivy stav je pokles o vice nez 1 kategorii pocetnosti (POP_POCETNOST,
  # skala 0-5) oproti referencni hodnote, kterou je POSLEDNI PREDCHOZI ROK S CILENYM
  # MONITORINGEM (CILMON == 1) na teze DP. Pokud takovy referencni rok neexistuje
  # (napr. prvni rok cileneho monitoringu na dane DP), indikator zustava NA (nelze hodnotit).
  n2k_druhy_lokpop_zmenarad_ref <- n2k_druhy_lokpop %>%
    dplyr::filter(CILMON_MAX == 1) %>%
    dplyr::select(KOD_LOKAL, DRUH, ROK_REF = ROK, POP_POCETNOST_REF = POP_POCETNOST) %>%
    dplyr::distinct()

  n2k_druhy_lokpop_zmenarad <- n2k_druhy_lokpop %>%
    dplyr::select(KOD_LOKAL, DRUH, ROK, POP_POCETNOST) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      n2k_druhy_lokpop_zmenarad_ref,
      by = c("KOD_LOKAL", "DRUH"),
      relationship = "many-to-many"
    ) %>%
    # Referencni rok musi predchazet hodnocenemu roku
    dplyr::filter(ROK_REF < ROK) %>%
    # Vezmeme nejblizsi (nejnovejsi) predchozi rok s cilenym monitoringem
    dplyr::group_by(KOD_LOKAL, DRUH, ROK) %>%
    dplyr::slice_max(order_by = ROK_REF, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # POP_ZMENARAD: zmena kategorie pocetnosti oproti referencnimu roku
      # (zaporne cislo = pokles o X kategorii; limit v limity_vse.csv je min -1,
      # tj. pokles o vice nez 1 kategorii = nepriznivy stav)
      POP_ZMENARAD = POP_POCETNOST - POP_POCETNOST_REF
    ) %>%
    dplyr::select(KOD_LOKAL, DRUH, ROK, POP_ZMENARAD)

  n2k_druhy_lokpop <- n2k_druhy_lokpop %>%
    dplyr::left_join(
      n2k_druhy_lokpop_zmenarad,
      by = c("KOD_LOKAL", "DRUH", "ROK")
    )

  # Lokalita - trilete indikatory POCITANE PRO KAZDY HODNOCENY ROK ZVLAST -----
  # Metodika:
  #   reprodukce - "spatne, neni-li reprodukce dolozena ani jednou ze TRI
  #                 POSLEDNICH SEZON S MONITORINGEM dane DP",
  #   vysychani  - "spatne, pokud vodni plocha vyschla v KAZDEM ZE TRI
  #                 POSLEDNICH HODNOCENYCH LET".
  # Obojí je okno koncici v hodnocenem roce. Puvodne se obe hodnoty pocitaly
  # jednou za celou DP a pripojovaly se bez ROKU, takze historicke rocniky
  # dedily hodnotu odvozenou z pozdejsich dat a zpetne hodnoceni nebylo
  # reprodukovatelne.
  #
  # POZOR na semantiku prazdneho okna: roll3_sum() vraci NA, kdyz v okne neni
  # ani jedna nechybejici hodnota. Puvodni sum(na.rm = TRUE) vracel v takovem
  # pripade 0, coz u POP_REPROPERIOD3 (limit min 1) znamenalo NESPLNENY KLICOVY
  # indikator jen proto, ze reprodukce nebyla vubec zjistovana. To odporuje vete
  # metodiky "Indikator se hodnoti pouze, jsou-li dostupne informace k jeho
  # hodnoceni." (viz nalez H-19 v harmonizace_registr.md).
  n2k_druhy_lokpop_period3 <- n2k_druhy_lokpop %>%
    dplyr::select(KOD_LOKAL, DRUH, ROK, POP_REPROMAX, STA_VYSYCHMAX) %>%
    dplyr::distinct() %>%
    dplyr::arrange(KOD_LOKAL, DRUH, ROK) %>%
    dplyr::group_by(KOD_LOKAL, DRUH) %>%
    dplyr::mutate(
      POP_REPROPERIOD3     = roll3_sum(POP_REPROMAX),
      STA_VYSYCHANIPERIOD3 = roll3_sum(STA_VYSYCHMAX)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(KOD_LOKAL, DRUH, ROK, POP_REPROPERIOD3, STA_VYSYCHANIPERIOD3)

  n2k_druhy_lokpop <- n2k_druhy_lokpop %>%
    dplyr::left_join(
      n2k_druhy_lokpop_period3,
      by = c("KOD_LOKAL", "DRUH", "ROK")
    )

  # Lokalita - dynamika a vitalita podle metodiky EVL 2026, ZA KAZDY ROK -----
  # (nalezy H-72, H-73, H-79). Puvodni POP_DYN nize bere head(..., 3) pres
  # RADKY n2k_druhy_lokpop, kterych je na lokalitu-rok tolik, kolik bylo
  # zmerenych delek - u 69 % lokalit Cottus slo fakticky o 1 rok proti 1 roku
  # - a pripojuje se bez ROKu. Zde je jedna hodnota na rok a okno konci
  # v hodnocenem roce (vzor n2k_druhy_lokpop_period3). Ostatni druhy ryb
  # zustavaji u puvodniho vypoctu.
  n2k_druhy_lokpop_ryby2026 <- NULL
  if (species_name %in% DRUHY_RYBY_2026) {
    n2k_druhy_lokpop_ryby2026 <- n2k_druhy_lokpop %>%
      dplyr::group_by(KOD_LOKAL, DRUH, ROK) %>%
      dplyr::summarise(
        ABUND_ROK = {
          a <- POP_ABUNDANCE[!is.na(POP_ABUNDANCE)]
          if (length(a) > 0) max(a) else NA_real_
        },
        VIT_OK3 = any(!is.na(POP_VITALITA) & POP_VITALITA >= 3),
        VIT_MERENO = any(!is.na(POP_VITALITA)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(KOD_LOKAL, DRUH, ROK) %>%
      dplyr::group_by(KOD_LOKAL, DRUH) %>%
      dplyr::mutate(
        POP_DYN = vapply(seq_along(ROK), function(i) {
          a <- ABUND_ROK[seq_len(i)]
          dyn_rok(a[!is.na(a)], species_name)
        }, numeric(1)),
        POP_VITALITA3 = vitalita3(VIT_OK3, VIT_MERENO)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(KOD_LOKAL, DRUH, ROK, POP_DYN, POP_VITALITA3)
  }

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
      # Trendovy blok se pocita jen u druhu, ktere maji limit POP_TREND*
      # (viz 'pocitat_trend' na zacatku funkce). U ostatnich druhu zustavaji
      # sloupce prazdne - NEsmi se ale vypustit uplne, protoze faze 2
      # pivotuje pevny rozsah sloupcu (POP_PRESENCE_N az po posledni) a
      # zmena sirky tabulky by rozhodila 'ncol_orig'.
      #
      # POP_POCETMAXREF: Referencni maximum pred 3 lety (3. nejnovejsi rok)
      POP_POCETMAXREF = if (pocitat_trend) {
        POP_POCETMAX[3]
      } else {
        NA_real_
      },
      # POP_TREND1/2: Porovnani dvou nejnovejsich roku s referenci
      # (1 = stejne nebo lepsi, 0 = horsi)
      POP_TREND1 = if (pocitat_trend) {
        dplyr::case_when(
          POP_POCETMAX[1] >= POP_POCETMAXREF ~ 1,
          POP_POCETMAX[1] < POP_POCETMAXREF ~ 0
        )
      } else {
        NA_real_
      },
      POP_TREND2 = if (pocitat_trend) {
        dplyr::case_when(
          POP_POCETMAX[2] >= POP_POCETMAXREF ~ 1,
          POP_POCETMAX[2] < POP_POCETMAXREF ~ 0
        )
      } else {
        NA_real_
      },
      # POP_TREND: Suma trendu (hodnoceni stability), limit max 1
      POP_TREND = if (pocitat_trend) {
        sum(
          POP_TREND1,
          POP_TREND2,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      # POP_TRENDLM: Linearni trend (smernice regrese POP_POCETMAX na ROK)
      POP_TRENDLM = if (pocitat_trend && sum(!is.na(POP_POCETMAX)) > 1) {
        coef(lm(POP_POCETMAX ~ ROK))[2]
      } else {
        NA_real_
      },
      # POP_ABUNDANCEMEAN: Prumerna abundance za posledni 3 roky
      POP_ABUNDANCEMEAN = mean(head(POP_ABUNDANCE, 3), na.rm = TRUE),
      # POP_POCETNOSTMAX: Maximalni pocetnost
      # POZN.: POP_REPROPERIOD3 a STA_VYSYCHANIPERIOD3 se odsud PRESUNULY do
      # samostatne tabulky n2k_druhy_lokpop_period3 (viz nize). Puvodne se
      # pocitaly zde, tj. jednou za KOD_LOKAL + DRUH ze tri nejnovejsich radku,
      # a pripojovaly se BEZ ROKU - vsechny rocniky dane DP tak dostaly tutez
      # hodnotu odvozenou z nejnovejsich dat (hodnoceni roku 2019 mohlo byt
      # ovlivneno pozorovanim z roku 2025). Metodika pritom mluvi o "trech
      # poslednich sezonach s monitoringem dane DP", tedy o oknu koncicim
      # v hodnocenem roce.
      POP_POCETNOSTMAX = max(
        POP_POCETNOST,
        na.rm = TRUE
      )
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
    #
    # SUFFIX (nalez H-35): tri sloupce existuji na obou stranach joinu -
    # POP_POCETMIN, POP_POCETMAX a POP_POCETNOSTMAX. Vychozi suffixy ".x"/".y"
    # zpusobily, ze sloupec s PRESNYM nazvem indikatoru v tabulce vubec nebyl,
    # takze se nesparoval s tabulkou limitu (21_2 paruje pres
    # intersect(nazvy sloupcu, ID_IND limitu)) a oba indikatory byly ve VSECH
    # vystupech neviditelne. Zaroven to znamenalo, ze soucet
    # sum(ID_IND == "POP_POCETMIN") na urovni uzemi v 25 vzdy vracel 0.
    #
    # Indikatorem je hodnota za DILCI PLOCHU a rok (limity maji UROVEN = lok),
    # tedy strana `y` = agregace z n2k_druhy_lokpop - ta si proto nechava holy
    # nazev. Hodnota za jednotlivy nalez (strana `x`) zustava zachovana pod
    # priponou _NAL, aby se nic neztratilo a bylo poznat, o kterou uroven jde.
    dplyr::left_join(
      .,
      n2k_druhy_lokpop,
      by = join_by(
        ROK,
        KOD_LOKAL,
        DRUH
      ),
      suffix = c("_NAL", "")
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

  # Ryby metodiky EVL 2026: POP_DYN a POP_VITALITA3 za lokalitu a ROK (viz
  # n2k_druhy_lokpop_ryby2026 vyse) nahrazuji POP_DYN z trendove tabulky.
  if (!is.null(n2k_druhy_lokpop_ryby2026)) {
    n2k_druhy <- n2k_druhy %>%
      dplyr::select(-POP_DYN) %>%
      dplyr::left_join(
        n2k_druhy_lokpop_ryby2026,
        by = c("KOD_LOKAL", "DRUH", "ROK")
      ) %>%
      dplyr::distinct()
  }

  return(n2k_druhy)
  
}
