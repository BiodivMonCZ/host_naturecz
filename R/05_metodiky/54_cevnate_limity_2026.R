#----------------------------------------------------------#
# Prevod limitu cevnatych rostlin - metodika 2026-10-21 -----
#----------------------------------------------------------#
# Prevadi tabulky limitu z metodiky hodnoceni stavu cevnatych rostlin
# (Popelka et al., verze 2026-10-21) do formatu, ktery cte pipeline
# (limity_cevky.csv), a zaklada ciselniky, ktere metodika nove vyzaduje.
#
# Spousti se z KORENE repozitare:
#   Rscript R/05_metodiky/54_cevnate_limity_2026.R
#
# VSTUPY (verzovane):
#   Metodiky/Cevnate/Tabulky fin.xlsx            Prilohy 1-4 metodiky
#   git <ZAKLAD_DIFF>:Data/Input/limity_cevky.csv
#                                                dosavadni limity (jen pro diff;
#                                                vyzaduje git)
#   Data/Input/cis_indikatory_popis.csv          kontrola pokryti ciselnikem
#   Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx
#                                                nazvy podle evropskeho seznamu
# VSTUP VOLITELNY (neverejny):
#   <slozka_lokal>/export_data_evl.csv           slovnik hodnot v datech NDOP;
#                                                bez nej se kontrola slovniku
#                                                preskoci a v protokolu je to
#                                                uvedeno
#
# VYSTUPY (generovane, prepisuji se):
#   Data/Input/limity_cevky.csv                  ZIVE limity cevnatych rostlin
#   Data/Input/cis_cevky_frekvence.csv           druh -> populacni jednotka,
#                                                frekvence monitoringu, okno
#   Data/Input/cis_druhy_nazvy.csv               nazev evropskeho seznamu <-> NDOP
#   Data/Input/cevky_vyber_lokalit.csv           vyber lokalit (Priloha 4)
#   Metodiky/Cevnate/limity_cevky_2026_diff.csv  rozdil proti dosavadnim limitum
#   Metodiky/Cevnate/limity_cevky_2026_kontrola.md
#                                                protokol validace a souhrn diffu
#
# Skript PREPISUJE zivy soubor Data/Input/limity_cevky.csv - jediny soubor
# limitu rostlin (rozhodnuti zadavatele 2026-10-28, otazky 7 a 11; drive
# samostatny limity_cevky_2026.csv). Diff se pocita proti posledni verzi pred
# metodikou 2026 z gitu (ZAKLAD_DIFF). Vsechna rozhodnuti a vyklady, ze
# kterych prevod vychazi, jsou v Metodiky/Cevnate/harmonizace_registr.md
# (Faze C).
#
# FORMAT (shodny s limity_cevky.csv, viz registr, Faze C):
#   Windows-1250, oddelovac ",", CRLF, uvozovky jen kde je treba,
#   sloupce DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN, pozn,
#   chybejici hodnota "NA" (pozn prazdny).
#   TYP_IND min = hodnota >= limit, max = hodnota <= limit, val = jeden radek
#   na kazdou pripustnou hodnotu, info = bez limitu (jen zobrazeni / vycet
#   jednotek pro extrakci). Ostre nerovnosti format nezna.
#   KLIC ano = klicovy, ne = neklicovy populacni, info = informativni
#   (hodnoti se a zobrazuje, ale do verdiktu lokality nevstupuje - 24 pocita
#   jen "ano" a "ne"; rozhodnuti zadavatele 2026-10-28).
#   Kazdy indikator ma NEJVYSE JEDEN prah min a jeden max (rozhodnuti
#   2026-10-28); mirnejsi prahy rozhodovacich tabulek se vypousteji
#   a vyznacuji ve sloupci pozn.
#----------------------------------------------------------#

#----------------------------------------------------------#
# Nacteni knihoven -----
#----------------------------------------------------------#
packages <- c("readxl", "openxlsx", "dplyr", "tidyr", "stringr", "readr", "tibble", "purrr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

if (!file.exists("R/05_metodiky/54_cevnate_limity_2026.R")) {
  stop("Skript se spousti z korene repozitare.")
}

#----------------------------------------------------------#
# Nastaveni -----
#----------------------------------------------------------#
ENC <- "Windows-1250"
ZDROJ_XLSX <- "Metodiky/Cevnate/Tabulky fin.xlsx"
METODIKA <- "metodika 2026-10-21"
ROZHODNUTI <- "rozhodnutí 2026-10-28"

if (!exists("slozka_lokal")) slozka_lokal <- "../host_data/"
EXPORT_NDOP <- paste0(slozka_lokal, c("export_data_evl.csv", "export_data_zprap.csv"))

# Posledni commit, ktery menil limity_cevky.csv pred metodikou 2026 - zaklad diffu.
ZAKLAD_DIFF <- "1994c08"

VYSTUP <- list(
  limity    = "Data/Input/limity_cevky.csv",
  frekvence = "Data/Input/cis_cevky_frekvence.csv",
  nazvy     = "Data/Input/cis_druhy_nazvy.csv",
  vyber     = "Data/Input/cevky_vyber_lokalit.csv",
  diff      = "Metodiky/Cevnate/limity_cevky_2026_diff.csv",
  kontrola  = "Metodiky/Cevnate/limity_cevky_2026_kontrola.md"
)

#----------------------------------------------------------#
# Nazvy druhu -----
#----------------------------------------------------------#
# DRUH v limitech MUSI byt nazev z NDOP: limity se na data napojuji
# pres DRUH (21_2) a seznam hodnocenych druhu je prunik limitu a NDOP
# (27_n2k_druhy_zapis.R). Listy xlsx i text metodiky pouzivaji ruzne
# tvary - kazdy se zde prevadi explicitne, nic se neodhaduje.
NAZVY_ZDROJ <- tibble::tribble(
  ~nazev_zdroj,                              ~DRUH,
  "Aconitum* moravicum",                     "Aconitum firmum subsp. moravicum",
  "Aconitum firmum",                         "Aconitum firmum subsp. moravicum",
  "Aconitum firmum subsp. moravicum",        "Aconitum firmum subsp. moravicum",
  "Adenophora liliifolia",                   "Adenophora liliifolia",
  "Angelica palustris",                      "Angelica palustris",
  "Artemisia pancicii",                      "Artemisia pancicii",
  "Asplenium adulterinum",                   "Asplenium adulterinum",
  "Campanula bohemica",                      "Campanula bohemica",
  "Campanula bohemika",                      "Campanula bohemica",
  "Campanula gelida",                        "Campanula gelida",
  "Cerastium alsinifolium",                  "Cerastium alsinifolium",
  "Cirsium brachycephalum",                  "Cirsium brachycephalum",
  "Coleanthus subtilis",                     "Coleanthus subtilis",
  "Crambe tataria",                          "Crambe tataria",
  "Cypripedium calceolus",                   "Cypripedium calceolus",
  "Dianthus arenarius",                      "Dianthus arenarius subsp. bohemicus",
  "Dianthus lumnitzerii",                    "Dianthus lumnitzeri",
  "Dianthus moravicus",                      "Dianthus moravicus",
  "Dracocephalum austriacum",                "Dracocephalum austriacum",
  "Echium maculatum",                        "Echium maculatum",
  "Galium sudeticum",                        "Galium sudeticum",
  "Gentianella* bohemica",                   "Gentianella praecox subsp. bohemica",
  "Gentianella bohemica",                    "Gentianella praecox subsp. bohemica",
  "Gentianella praecox subsp. bohemica",     "Gentianella praecox subsp. bohemica",
  "Gladiolus palustris",                     "Gladiolus palustris",
  "Himantoglossum adriaticum",               "Himantoglossum adriaticum",
  "Iris arenaria",                           "Iris arenaria",
  "Jurinea cyanoides",                       "Jurinea cyanoides",
  "Klasea lycopifolia",                      "Klasea lycopifolia",
  "Ligularia sibirica",                      "Ligularia sibirica",
  "Liparis loeselii",                        "Liparis loeselii",
  "Luronium natans",                         "Luronium natans",
  "Minuartia smejkalii",                     "Minuartia smejkalii",
  "Pedicularis sudetica",                    "Pedicularis sudetica",
  "Poa riphaea",                             "Poa riphaea",
  "Pulsatilla grandis",                      "Pulsatilla grandis",
  "Pulsatilla patens",                       "Pulsatilla patens",
  "Stipa zalesskii",                         "Stipa glabrata",
  "Tephroseris* moravica",                   "Tephroseris longifolia subsp. moravica",
  "Tephroseris longifolia subsp. moravica",  "Tephroseris longifolia subsp. moravica",
  "Thesium ebracteatum",                     "Thesium ebracteatum",
  "Trichomanes speciosum",                   "Trichomanes speciosum"
)

# Nazvy, pod kterymi druh vede evropsky seznam (seznam predmetolokalit),
# pokud se lisi od NDOP. Bez prevodu 21_1 zahodi vsechny zaznamy druhu
# (filtr DRUH %in% sites_subjects$nazev_lat). Stipa: NDOP vede "kavyl
# olysaly" pod jmenem Stipa glabrata (CESKE_JME v exportu), SDF 4095 tentyz
# druh pod Stipa zalesskii.
NAZVY_N2K <- tibble::tribble(
  ~DRUH,                                  ~nazev_n2k,
  "Adenophora liliifolia",                "Adenophora lilifolia",
  "Echium maculatum",                     "Pontechium maculatum subsp. maculatum",
  "Gentianella praecox subsp. bohemica",  "Gentianella bohemica",
  "Iris arenaria",                        "Iris humilis subsp. arenaria",
  "Stipa glabrata",                       "Stipa zalesskii",
  "Trichomanes speciosum",                "Vandenboschia speciosa"
)

# Prevod DRUH dosavadnich limitu na nazev NDOP (jen pro diff).
NAZVY_STARE_LIMITY <- c(
  "Iris humilis subsp. arenaria" = "Iris arenaria",
  "Stipa zalesskii"              = "Stipa glabrata"
)

prevod_nazvu <- function(x) {
  x <- stringr::str_squish(x)
  out <- NAZVY_ZDROJ$DRUH[match(x, NAZVY_ZDROJ$nazev_zdroj)]
  if (any(is.na(out))) {
    stop("Neznamy nazev druhu ve zdroji: ", paste(unique(x[is.na(out)]), collapse = "; "))
  }
  out
}

#----------------------------------------------------------#
# Indikatory -----
#----------------------------------------------------------#
# Priloha 2: sloupec -> ID_IND. Sloupce se hledaji podle textu hlavicky,
# ne podle poradi, aby posun sloupcu ve zdroji nevedl k tichemu prohozeni.
P2_IND <- tibble::tribble(
  ~hlavicka,                                            ~ID_IND,             ~KLIC,  ~JEDNOTKA,
  "Úspěšnost reprodukce (klíčový indikátor)",           "POP_REPROUSP",      "ano",  NA,
  "Délka absence druhu (klíčový indikátor)",            "POP_DELKAABSENCE",  "ano",  "let",
  "Minimální přípustná početnost (klíčový indikátor)",  "POP_POCETMINPRIP",  "ano",  "% cílové početnosti",
  "Početnost",                                          "POP_POCETCIL",      "ne",   "% cílové početnosti",
  "Vývoj početnosti",                                   "POP_TREND",         "ne",   NA,
  "Vitalita",                                           "POP_VITAL",         "ne",   "% kvetoucích a plodných ze sterilních jednotek",
  "Poškození rostlin",                                  "POP_POSKPERC",      "ne",   "% populačních jednotek (součet typů poškození)"
)

# Vyjimky z KLIC podle textu metodiky (kap. 7.1): poskozeni ma u puchyrky
# jen informativni charakter, prestoze mu Priloha 2 limit uvadi.
KLIC_VYJIMKY <- tibble::tribble(
  ~DRUH,                  ~ID_IND,         ~KLIC,
  "Coleanthus subtilis",  "POP_POSKPERC",  "info"
)

# Priloha 3: radek -> ID_IND. Tam, kde dosavadni limity a kod pouzivaji
# ruzne nazvy, se bere nazev SLOUPCE, KTERY KOD VYTVARI (jinak by limit
# zustal sirotkem - nalez H-10). Ctyri indikatory nemaji v kodu extrakci
# vubec - drzi se dosavadni ID z limitu a vyznacuji se v protokolu.
P3_IND <- tibble::tribble(
  ~radek,                                                     ~ID_IND,                   ~tag_ndop,
  "Celkové zastínění lokality",                               "STA_ZASTINENICELK",       "STA_ZASTINENICELK",
  "Pokryvnost dřevin",                                        "STA_POKRDREV",            "STA_POKRYVNOSTDREVIN",
  "Pokryvnost dřevin vyšších než 1 m",                        "STA_POKRYVNOSTE2E3",      "STA_POKRYVNOSTE2E3",
  "Pokryvnost dřevin nižších než 1 m",                        "STA_POKRDREVNIZ",         "STA_POKRYVNOSTDREVINNIZ",
  "Pokryvnost E1 (včetně juvenilních dřevin)",                "STA_POKRYVNOSTE1",        "STA_POKRYVNOSTE1",
  "Pokryvnost E0",                                            "STA_POKRYVNOSTE0",        "STA_POKRYVNOSTE0",
  "Pokryvnost stařiny",                                       "STA_POKRSTAR",            "STA_POKRYVNOSTSTARINA",
  "Pokryvnost volné půdy",                                    "STA_POKRYVNOSTVOLNAPUDA", "STA_POKRYVNOSTVOLNAPUDA",
  "Pokryvnost invazních druhů",                               "STA_POKRYVNOSTINVAZNI",   "STA_POKRYVNOSTINVAZNI",
  "Pokryvnost expanzivních druhů",                            "STA_POKRYVNOSTEXPANZNI",  "STA_POKRYVNOSTEXPANZNI",
  "Pokryvnost rákosu",                                        "STA_POKRRAKOS",           NA,
  "Pokryvnost ostatních natantních a submerzních druhů",      "STA_POC_OST_NATASUB_DRH", "STA_POKRYVNOSTNATDRUH",
  "Stupeň zachování stanoviště",                              "STA_STUPENZACH",          "SP_STUPEN_ZACH_STAN",
  "Hydrologické poměry",                                      "STA_HYDRPOMERY",          "STA_HYDRPOMERY",
  "Vlhkostní poměry stanoviště",                              "STA_VLHPOM",              "STA_VLHKOPOMERY",
  "Podíl dna nad vodní hladinou (oproti normálnímu stavu)",   "STA_POKRDNO",             "STA_PODILDNONADHLADINA",
  "Termín a doba trvání poklesu vodní hladiny",               "STA_POKLESHLAD",          "STA_POKLESVODNIHLADINA"
)

# Indikatory, ktere kod 21_1 dnes NEPOCITA (limit by byl sirotkem, dokud
# nebude doplnena extrakce / vypocet). Jen pro protokol.
BEZ_VYPOCTU <- c(
  "POP_REPROUSP", "POP_DELKAABSENCE", "POP_POCETMINPRIP", "POP_POCETCIL",
  "POP_CILSTAVPROC", "STA_POKRRAKOS", "STA_POC_OST_NATASUB_DRH",
  "STA_POKRDNO", "STA_POKLESHLAD"
)
# Indikatory, ktere kod pocita, ale jinak, nez limit ocekava.
JINY_VYPOCET <- c(
  POP_TREND    = "kod pocita cislo 0-2 (obracena logika, H-03), limit ocekava kategorii",
  POP_POSKPERC = "kod scita kategorie 0-3 (H-12), limit ocekava soucet procent (medianu)"
)

# Prejmenovane ID proti dosavadnim limitum (jen pro diff).
PREJMENOVANI <- c(
  STA_HYDROPOMERY = "STA_HYDRPOMERY",
  STA_POKRPUDA    = "STA_POKRYVNOSTVOLNAPUDA",
  STA_POKRVOLPUD  = "STA_POKRYVNOSTVOLNAPUDA"
)

#----------------------------------------------------------#
# Uroven uzemi (Tabulka 1 a 2 metodiky) -----
#----------------------------------------------------------#
# Tabulky jsou jen v textu metodiky (.docx), ne v xlsx. Kazdy indikator
# dostane JEDINY prah - ten pro dobry stav. Mirnejsi prahy (40 %, 60 %,
# 80 %) se podle rozhodnuti zadavatele 2026-10-28 vypousteji a vyznacuji.
UZEMI <- tibble::tribble(
  ~ID_IND,            ~TYP_IND, ~LIM_IND, ~JEDNOTKA,                    ~coleanthus, ~ostatni, ~pozn,
  "LOK_PROCDOBR",     "min",    "50",     "% lokalit v dobrém stavu",   TRUE,        TRUE,     "Tab. 1 a 2; mírnější práh 40 % (Tab. 2) vypuštěn",
  "POP_PROCDOB",      "min",    "75",     "% populace v dobrém stavu",  FALSE,       TRUE,     "Tab. 2; mírnější práh 60 % vypuštěn",
  "POP_CILSTAVPROC",  "min",    "100",    "% cílového stavu území",     TRUE,        TRUE,     "Tab. 1 a 2; mírnější práh 80 % vypuštěn"
)

#----------------------------------------------------------#
# Populacni jednotky -> hodnoty POCITANO v NDOP -----
#----------------------------------------------------------#
# Funkcni JEDNOTKA (21_1 vybira zaznamy, jejichz POCITANO se shoduje):
#   POP_POCET        pocetnost v populacni jednotce Prilohy 1 primo
#   POP_POCETSUM     casti, ktere se v ramci akce scitaji na pocetnost
#                    (sterilni + kvetouci a plodne)
#   POP_POCETVITAL   kvetouci a plodne jednotky (citatel vitality)
#   POP_POCETSTERIL  sterilni jednotky (jmenovatel vitality, kap. 7.1)
# Priloha 1 je pro jednotky rozhodujici (rozhodnuti 2026-10-28).
# "kvetouci X" je starsi tvar "kvetouci a plodne X" (tentyz organ, do
# cca 2021) - zarazeno jako ekvivalent a vyznaceno. Jiny organ (jedinci
# misto trsu, lodyhy misto trsu) a semenacky se NEZARAZUJI - vycet
# vyrazenych hodnot je v protokolu. Plochu v jine plosne jednotce (cm2, dm2)
# prevadi 21_1 na plosnou jednotku Prilohy 1 (koef_plochy; rozhodnuti
# 2026-10-28, otazka 7), zde se proto neuvadi.
# Stav: shoda = hodnota je v datech monitoringu; potvrzeno = vyklad
# ekvivalence potvrzeny zadavatelem 2026-10-28 (otazka 7); chybi = jednotka
# Prilohy 1 v datech neni.
J <- function(druh, id, jedn, stav = "shoda", pozn = NA_character_) {
  tibble::tibble(DRUH = druh, ID_IND = id, JEDNOTKA = jedn, stav = stav, pozn = pozn)
}
pp <- c("sterilní prýty", "kvetoucí a plodné prýty")
pt <- c("sterilní trsy", "kvetoucí a plodné trsy")
pj <- c("sterilní jedinci", "kvetoucí a plodní jedinci")

JEDNOTKY <- dplyr::bind_rows(
  J("Aconitum firmum subsp. moravicum", "POP_POCET", "prýty"),
  J("Aconitum firmum subsp. moravicum", "POP_POCETSUM", c(pp, "plodné prýty")),
  J("Aconitum firmum subsp. moravicum", "POP_POCETVITAL", c("kvetoucí a plodné prýty", "plodné prýty")),
  J("Aconitum firmum subsp. moravicum", "POP_POCETSTERIL", "sterilní prýty"),
  J("Adenophora liliifolia", "POP_POCET", "trsy"),
  J("Adenophora liliifolia", "POP_POCETSUM", pt),
  J("Adenophora liliifolia", "POP_POCETVITAL", "kvetoucí a plodné trsy"),
  J("Adenophora liliifolia", "POP_POCETSTERIL", "sterilní trsy"),
  J("Angelica palustris", "POP_POCET", "prýty", "chybí", "v datech jen součet částí; 'jedinci' nezařazeno"),
  J("Angelica palustris", "POP_POCETSUM", pp),
  J("Angelica palustris", "POP_POCETVITAL", "kvetoucí a plodné prýty"),
  J("Angelica palustris", "POP_POCETSTERIL", "sterilní prýty"),
  J("Artemisia pancicii", "POP_POCET", "prýty"),
  J("Artemisia pancicii", "POP_POCETSUM", pp),
  J("Asplenium adulterinum", "POP_POCET", "trsy"),
  J("Campanula bohemica", "POP_POCET", "kvetoucí a plodné trsy", "shoda", "'kvetoucí jedinci' 2011-2016 nezařazeno (jiný orgán)"),
  J("Campanula gelida", "POP_POCET", "trsy"),
  J("Cerastium alsinifolium", "POP_POCET", "m2", "shoda", "Příloha 2 uvádí dm2 (platí Příloha 1); záznamy v cm2 a dm2 převádí 21_1 na m2"),
  J("Cirsium brachycephalum", "POP_POCET", "kvetoucí a plodní jedinci"),
  J("Cirsium brachycephalum", "POP_POCET", "kvetoucí jedinci", "potvrzeno", "starší tvar 2011-2021"),
  J("Coleanthus subtilis", "POP_POCET", "jedinci", "shoda", "relativní stupnice REL_POC se váže na tuto jednotku"),
  J("Crambe tataria", "POP_POCET", "jedinci"),
  J("Crambe tataria", "POP_POCETSUM", c(pj, "kvetoucí jedinci")),
  J("Crambe tataria", "POP_POCETVITAL", "kvetoucí a plodní jedinci"),
  J("Crambe tataria", "POP_POCETVITAL", "kvetoucí jedinci", "potvrzeno", "starší tvar"),
  J("Crambe tataria", "POP_POCETSTERIL", "sterilní jedinci"),
  J("Cypripedium calceolus", "POP_POCET", "trsy"),
  J("Cypripedium calceolus", "POP_POCETSUM", pt),
  J("Cypripedium calceolus", "POP_POCETVITAL", "kvetoucí a plodné trsy"),
  J("Cypripedium calceolus", "POP_POCETSTERIL", "sterilní trsy"),
  J("Dianthus arenarius subsp. bohemicus", "POP_POCET", "trsy"),
  J("Dianthus arenarius subsp. bohemicus", "POP_POCETSUM", c(pt, "kvetoucí trsy", "plodné trsy")),
  J("Dianthus arenarius subsp. bohemicus", "POP_POCETVITAL", c("kvetoucí a plodné trsy", "kvetoucí trsy", "plodné trsy"), "potvrzeno", "'kvetoucí' a 'plodné' trsy v téže akci se mohou překrývat"),
  J("Dianthus arenarius subsp. bohemicus", "POP_POCETSTERIL", "sterilní trsy"),
  J("Dianthus lumnitzeri", "POP_POCET", "kvetoucí a plodné trsy"),
  J("Dianthus moravicus", "POP_POCET", "kvetoucí a plodné trsy"),
  J("Dracocephalum austriacum", "POP_POCET", "trsy", "chybí", "Příloha 1 'trs/kvetoucí a plodný trs'; v datech jen součet částí; 'jedinci' nezařazeno"),
  J("Dracocephalum austriacum", "POP_POCETSUM", pt),
  J("Dracocephalum austriacum", "POP_POCETVITAL", "kvetoucí a plodné trsy"),
  J("Dracocephalum austriacum", "POP_POCETSTERIL", "sterilní trsy"),
  J("Echium maculatum", "POP_POCET", "kvetoucí a plodní jedinci", "shoda", "'jedinci' (246 záznamů 2006-2022) nezařazeno - celkový počet, ne kvetoucí"),
  J("Echium maculatum", "POP_POCET", "kvetoucí jedinci", "potvrzeno", "starší tvar"),
  J("Galium sudeticum", "POP_POCET", "m2", "shoda", "Příloha 2 uvádí dm2 (platí Příloha 1); záznamy v cm2 a dm2 převádí 21_1 na m2"),
  J("Gentianella praecox subsp. bohemica", "POP_POCET", "kvetoucí a plodní jedinci"),
  J("Gentianella praecox subsp. bohemica", "POP_POCET", "kvetoucí jedinci", "potvrzeno", "starší tvar 2005-2022"),
  J("Gladiolus palustris", "POP_POCET", "kvetoucí a plodní jedinci", "shoda", "'jedinci' (392 záznamů 2006-2022) nezařazeno - celkový počet"),
  J("Gladiolus palustris", "POP_POCET", "kvetoucí jedinci", "potvrzeno", "starší tvar"),
  J("Himantoglossum adriaticum", "POP_POCET", "jedinci"),
  J("Himantoglossum adriaticum", "POP_POCETSUM", c(pj, "kvetoucí jedinci")),
  J("Himantoglossum adriaticum", "POP_POCETVITAL", "kvetoucí a plodní jedinci"),
  J("Himantoglossum adriaticum", "POP_POCETVITAL", "kvetoucí jedinci", "potvrzeno", "starší tvar"),
  J("Himantoglossum adriaticum", "POP_POCETSTERIL", "sterilní jedinci"),
  J("Iris arenaria", "POP_POCET", "prýty"),
  J("Jurinea cyanoides", "POP_POCET", "prýty", "chybí", "v datech jen součet částí; 'ramety' nezařazeno"),
  J("Jurinea cyanoides", "POP_POCETSUM", pp),
  J("Jurinea cyanoides", "POP_POCETVITAL", "kvetoucí a plodné prýty"),
  J("Jurinea cyanoides", "POP_POCETSTERIL", "sterilní prýty"),
  J("Klasea lycopifolia", "POP_POCET", "m2", "shoda", "záznamy v 'cm2' a 'plocha populace (cm2)' převádí 21_1 na m2"),
  J("Klasea lycopifolia", "POP_POCETVITAL", "kvetoucí a plodné prýty"),
  J("Klasea lycopifolia", "POP_POCETSTERIL", "sterilní prýty"),
  J("Ligularia sibirica", "POP_POCET", "jedinci"),
  J("Ligularia sibirica", "POP_POCETSUM", c(pj, "kvetoucí jedinci")),
  J("Ligularia sibirica", "POP_POCETVITAL", "kvetoucí a plodní jedinci"),
  J("Ligularia sibirica", "POP_POCETVITAL", "kvetoucí jedinci", "potvrzeno", "starší tvar"),
  J("Ligularia sibirica", "POP_POCETSTERIL", "sterilní jedinci"),
  J("Liparis loeselii", "POP_POCET", "jedinci"),
  J("Liparis loeselii", "POP_POCETSUM", pj),
  J("Liparis loeselii", "POP_POCETVITAL", "kvetoucí a plodní jedinci"),
  J("Liparis loeselii", "POP_POCETSTERIL", "sterilní jedinci"),
  J("Luronium natans", "POP_POCET", "růžice", "shoda", "'cm2' a 'm2' nezařazeno - plochu nelze převést na počet růžic"),
  J("Minuartia smejkalii", "POP_POCET", "trsy"),
  J("Pedicularis sudetica", "POP_POCET", "kvetoucí a plodné lodyhy", "chybí", "v datech jen 'trsy'"),
  J("Poa riphaea", "POP_POCET", "trsy"),
  J("Poa riphaea", "POP_POCETVITAL", "kvetoucí a plodné trsy", "chybí", "v datech jen trsy, stébla, semenáčky - vitalitu nelze spočítat"),
  J("Poa riphaea", "POP_POCETSTERIL", "sterilní trsy", "chybí"),
  J("Pulsatilla grandis", "POP_POCET", "kvetoucí a plodné trsy"),
  J("Pulsatilla grandis", "POP_POCET", "kvetoucí trsy", "potvrzeno", "starší tvar 2005-2021"),
  J("Pulsatilla patens", "POP_POCET", "trsy"),
  J("Pulsatilla patens", "POP_POCETSUM", c(pt, "kvetoucí trsy")),
  J("Pulsatilla patens", "POP_POCETVITAL", "kvetoucí a plodné trsy"),
  J("Pulsatilla patens", "POP_POCETVITAL", "kvetoucí trsy", "potvrzeno", "starší tvar 2009-2022"),
  J("Pulsatilla patens", "POP_POCETSTERIL", "sterilní trsy"),
  J("Stipa glabrata", "POP_POCET", "trsy"),
  J("Tephroseris longifolia subsp. moravica", "POP_POCET", "jedinci"),
  J("Tephroseris longifolia subsp. moravica", "POP_POCETSUM", pj),
  J("Tephroseris longifolia subsp. moravica", "POP_POCETVITAL", "kvetoucí a plodní jedinci"),
  J("Tephroseris longifolia subsp. moravica", "POP_POCETSTERIL", "sterilní jedinci"),
  J("Thesium ebracteatum", "POP_POCET", "prýty"),
  J("Thesium ebracteatum", "POP_POCETSUM", pp),
  J("Trichomanes speciosum", "POP_POCET", "cm2", "shoda", "záznamy v dm2 a m2 by 21_1 převedl na cm2 (v datech nejsou)")
)

#----------------------------------------------------------#
# Pomocne funkce -----
#----------------------------------------------------------#
# Sloupce se pojmenuji podle pozice (x1, x2, ...) - hlavicky se ctou jako
# data a sloupce se pak hledaji podle textu hlavicky, ne "od oka".
cti_list <- function(list) {
  df <- readxl::read_excel(
    ZDROJ_XLSX, sheet = list, col_names = FALSE, col_types = "text",
    trim_ws = FALSE, .name_repair = "minimal"
  )
  stats::setNames(df, paste0("x", seq_len(ncol(df))))
}

# Index radku a sloupce hlavicky podle textu prvni bunky radku.
radek_hlavicky <- function(df, text) {
  i <- which(stringr::str_squish(df[[1]]) == text)
  if (length(i) != 1) stop("Hlavicka '", text, "' nenalezena prave jednou.")
  i
}

# Tvar kategorie, ktery prevazuje v datech: "75-100%" -> "75-100 %".
norm_kategorie <- function(x) {
  x <- stringr::str_squish(x)
  stringr::str_replace(x, "^(\\d+)-(\\d+)%$", "\\1-\\2 %")
}

# Bunka Prilohy 2 -> radky limitu. "." a prazdna bunka = indikator se
# nepouziva. Neznamy tvar zastavi prevod (nic se nedomysli).
parsuj_limit <- function(x) {
  x <- stringr::str_squish(x)
  if (is.na(x) || x == "" || x == ".") return(NULL)
  if (stringr::str_detect(x, "^≥ ?\\d+ ?%$")) {
    return(tibble::tibble(TYP_IND = "min", LIM_IND = stringr::str_extract(x, "\\d+"), poznamka = NA_character_))
  }
  if (stringr::str_detect(x, "^≤ ?\\d+ ?%$")) {
    return(tibble::tibble(TYP_IND = "max", LIM_IND = stringr::str_extract(x, "\\d+"), poznamka = NA_character_))
  }
  if (stringr::str_detect(x, "^< ?\\d+ ?let$")) {
    # Format zna jen <=. Delka absence se pocita v celych letech, takze
    # "< 6 let" = "<= 5 let" (rozhodnuti 2026-10-28).
    n <- as.integer(stringr::str_extract(x, "\\d+"))
    return(tibble::tibble(TYP_IND = "max", LIM_IND = as.character(n - 1L),
                          poznamka = paste0("zdroj '", x, "' (celé roky)")))
  }
  if (stringr::str_detect(x, "^= ?")) {
    v <- stringr::str_squish(unlist(stringr::str_split(stringr::str_remove(x, "^= ?"), ";")))
    v <- v[v != ""]
    return(tibble::tibble(TYP_IND = "val", LIM_IND = v, poznamka = NA_character_))
  }
  stop("Neznamy tvar limitu v Priloze 2: '", x, "'")
}

# Zapis CSV v konvenci repozitare (Windows-1250, CRLF, uvozovky jen kde je
# treba). na_text urcuje, jak se zapise chybejici hodnota v kazdem sloupci.
zapis_csv <- function(df, cesta, na_text) {
  pole <- function(x, nt) {
    x <- as.character(x)
    x[is.na(x)] <- nt
    q <- stringr::str_detect(x, "[\",\r\n]")
    x[q] <- paste0("\"", stringr::str_replace_all(x[q], "\"", "\"\""), "\"")
    x
  }
  nt <- rep_len(na_text, ncol(df))
  if (!is.null(names(na_text))) nt <- unname(na_text[names(df)])
  sl <- purrr::map2(df, nt, pole)
  radky <- c(paste(names(df), collapse = ","), do.call(paste, c(sl, sep = ",")))
  radky_enc <- iconv(radky, from = "UTF-8", to = "CP1250")
  if (any(is.na(radky_enc))) {
    stop("Znak, ktery nelze zapsat ve Windows-1250: ", radky[is.na(radky_enc)][1])
  }
  con <- file(cesta, open = "wb")
  on.exit(close(con))
  writeLines(radky_enc, con, sep = "\r\n", useBytes = TRUE)
}

cti_cp1250 <- function(cesta) {
  readr::read_csv(cesta, locale = readr::locale(encoding = ENC),
                  col_types = readr::cols(.default = "c"), na = c("", "NA"))
}

#----------------------------------------------------------#
# Priloha 1 - populacni jednotka a frekvence -----
#----------------------------------------------------------#
p1 <- cti_list("Příloha 1")
h1 <- radek_hlavicky(p1, "Druh")
if (!identical(stringr::str_squish(unname(unlist(p1[h1, 1:3]))),
               c("Druh", "Populační jednotka", "Frekvence monitoringu lokalit v letech"))) {
  stop("Priloha 1: neocekavana hlavicka.")
}
p1 <- p1[(h1 + 1):nrow(p1), 1:3] %>%
  stats::setNames(c("nazev", "jednotka", "frekvence")) %>%
  dplyr::filter(!is.na(nazev)) %>%
  dplyr::mutate(
    dplyr::across(dplyr::everything(), stringr::str_squish),
    DRUH = prevod_nazvu(nazev),
    FREKV_MIN = as.integer(stringr::str_extract(frekvence, "^\\d+")),
    FREKV_MAX = as.integer(dplyr::coalesce(stringr::str_extract(frekvence, "\\d+$"), stringr::str_extract(frekvence, "^\\d+")))
  )

#----------------------------------------------------------#
# Priloha 2 - populacni indikatory -----
#----------------------------------------------------------#
p2 <- cti_list("Příloha 2")
h2 <- radek_hlavicky(p2, "Druh")
hlav2 <- stringr::str_squish(unname(unlist(p2[h2, ])))
sl2 <- match(P2_IND$hlavicka, hlav2)
if (any(is.na(sl2))) stop("Priloha 2: chybi sloupec ", P2_IND$hlavicka[is.na(sl2)][1])
sl2_jedn <- match("Populační jednotka", hlav2)
sl2_frekv <- match("Frekvence monitoringu", hlav2)

p2_data <- p2[(h2 + 1):nrow(p2), ] %>% dplyr::filter(!is.na(.data[[names(p2)[1]]]))
p2_druh <- prevod_nazvu(p2_data[[1]])

# Mezery na zacatku bunek (zaznamenano do protokolu, pak orezano).
p2_mezery <- purrr::map_dfr(seq_len(nrow(P2_IND)), function(k) {
  v <- p2_data[[sl2[k]]]
  i <- which(!is.na(v) & stringr::str_detect(v, "^\\s|\\s$"))
  tibble::tibble(DRUH = p2_druh[i], sloupec = P2_IND$hlavicka[rep(k, length(i))], hodnota = v[i])
})

lim_pop <- purrr::map_dfr(seq_len(nrow(P2_IND)), function(k) {
  purrr::map_dfr(seq_along(p2_druh), function(i) {
    r <- parsuj_limit(p2_data[[sl2[k]]][i])
    if (is.null(r)) return(NULL)
    r %>% dplyr::mutate(DRUH = p2_druh[i], ID_IND = P2_IND$ID_IND[k],
                        KLIC = P2_IND$KLIC[k], JEDNOTKA = P2_IND$JEDNOTKA[k],
                        zdroj = paste0("Příloha 2: ", P2_IND$hlavicka[k], " = ", stringr::str_squish(p2_data[[sl2[k]]][i])))
  })
}) %>%
  dplyr::left_join(KLIC_VYJIMKY %>% dplyr::rename(KLIC_V = KLIC), by = c("DRUH", "ID_IND")) %>%
  dplyr::mutate(
    poznamka = dplyr::if_else(!is.na(KLIC_V),
                              paste0(dplyr::coalesce(poznamka, ""), "informativní dle textu kap. 7.1 (", ROZHODNUTI, ")"),
                              poznamka),
    KLIC = dplyr::coalesce(KLIC_V, KLIC),
    UROVEN = "lok"
  ) %>%
  dplyr::select(-KLIC_V)

p2_meta <- tibble::tibble(
  DRUH = p2_druh,
  jednotka_p2 = stringr::str_squish(p2_data[[sl2_jedn]]),
  frekvence_p2 = stringr::str_squish(p2_data[[sl2_frekv]])
)

#----------------------------------------------------------#
# Priloha 3 - stanovistni indikatory (siroky -> dlouhy format) -----
#----------------------------------------------------------#
p3 <- cti_list("Příloha 3")
h3 <- radek_hlavicky(p3, "Název položky")
p3_druh <- prevod_nazvu(unname(unlist(p3[h3, -1])))
p3_data <- p3[(h3 + 1):nrow(p3), ] %>% dplyr::filter(!is.na(.data[[names(p3)[1]]]))
p3_radek <- stringr::str_squish(p3_data[[1]])
if (any(!p3_radek %in% P3_IND$radek)) {
  stop("Priloha 3: neznamy radek ", p3_radek[!p3_radek %in% P3_IND$radek][1])
}

p3_long <- purrr::map_dfr(seq_along(p3_radek), function(r) {
  purrr::map_dfr(seq_along(p3_druh), function(s) {
    bunka <- p3_data[[s + 1]][r]
    if (is.na(bunka) || stringr::str_squish(bunka) == "") return(NULL)
    tibble::tibble(DRUH = p3_druh[s], radek = p3_radek[r], bunka = bunka,
                   hodnota_zdroj = stringr::str_squish(unlist(stringr::str_split(bunka, ";"))))
  })
}) %>%
  dplyr::filter(hodnota_zdroj != "") %>%
  dplyr::mutate(LIM_IND = norm_kategorie(hodnota_zdroj)) %>%
  dplyr::left_join(P3_IND, by = "radek")

lim_sta <- p3_long %>%
  dplyr::transmute(
    DRUH, ID_IND, TYP_IND = "val", LIM_IND, JEDNOTKA = NA_character_,
    KLIC = "info", UROVEN = "lok",
    poznamka = dplyr::if_else(LIM_IND != hodnota_zdroj,
                              paste0("zdroj '", hodnota_zdroj, "'"), NA_character_)
  )

#----------------------------------------------------------#
# Uroven uzemi -----
#----------------------------------------------------------#
druhy <- sort(unique(p1$DRUH))
lim_chu <- tidyr::crossing(DRUH = druhy, UZEMI) %>%
  dplyr::filter(dplyr::if_else(DRUH == "Coleanthus subtilis", coleanthus, ostatni)) %>%
  dplyr::transmute(DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC = "ano",
                   UROVEN = "chu", poznamka = paste0(pozn, " (", ROZHODNUTI, ")"))

#----------------------------------------------------------#
# Jednotky (info radky) -----
#----------------------------------------------------------#
# Vitalitu potrebuji jen druhy, ktere ji maji v Priloze 2.
druhy_vital <- lim_pop$DRUH[lim_pop$ID_IND == "POP_VITAL"]
lim_jedn <- JEDNOTKY %>%
  dplyr::filter(!(ID_IND %in% c("POP_POCETVITAL", "POP_POCETSTERIL") & !DRUH %in% druhy_vital)) %>%
  dplyr::transmute(
    DRUH, ID_IND, TYP_IND = "info", LIM_IND = NA_character_, JEDNOTKA,
    KLIC = NA_character_, UROVEN = "lok",
    poznamka = dplyr::case_when(
      stav == "shoda" ~ pozn,
      is.na(pozn) ~ paste0("jednotka: ", stav),
      TRUE ~ paste0("jednotka: ", stav, "; ", pozn)
    )
  )
chybi_jedn <- setdiff(druhy, JEDNOTKY$DRUH[JEDNOTKY$ID_IND == "POP_POCET"])
if (length(chybi_jedn) > 0) stop("Chybi jednotka POP_POCET pro: ", paste(chybi_jedn, collapse = ", "))

#----------------------------------------------------------#
# Slozeni limitu -----
#----------------------------------------------------------#
PORADI_ID <- c("POP_POCET", "POP_POCETSUM", "POP_POCETVITAL", "POP_POCETSTERIL",
               P2_IND$ID_IND, P3_IND$ID_IND, UZEMI$ID_IND)

limity_nove <- dplyr::bind_rows(
  lim_jedn,
  lim_pop %>% dplyr::select(DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN, poznamka),
  lim_sta,
  lim_chu
) %>%
  dplyr::mutate(
    pozn = dplyr::if_else(is.na(poznamka), METODIKA, paste0(METODIKA, "; ", poznamka)),
    .ord_id = match(ID_IND, PORADI_ID)
  ) %>%
  dplyr::arrange(DRUH, .ord_id) %>%
  dplyr::select(DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN, pozn)

#----------------------------------------------------------#
# Ciselnik druhu: jednotka, frekvence, okno -----
#----------------------------------------------------------#
# OKNO_LET: jak dlouho zustava lokalita v hodnoceni od posledniho
# prokazatelneho vyskytu druhu (kap. 5.1: 10 let, puchyrka nejmene 12 let).
# Cte ho 21_1_n2k_druhy_akce.R misto pevneho okna 12 let (rozhodnuti
# zadavatele 2026-10-28, otazka 14).
frekvence <- p1 %>%
  dplyr::left_join(p2_meta, by = "DRUH") %>%
  dplyr::transmute(
    DRUH,
    POP_JEDNOTKA = jednotka,
    FREKV_TEXT = frekvence,
    FREKV_MIN, FREKV_MAX,
    OKNO_LET = dplyr::if_else(DRUH == "Coleanthus subtilis", 12L, 10L),
    pozn = purrr::pmap_chr(list(jednotka, jednotka_p2, frekvence, frekvence_p2), function(j1, j2, f1, f2) {
      x <- c(
        if (!is.na(j2) && j2 != j1) paste0("Příloha 2 uvádí jednotku '", j2, "'"),
        if (!is.na(f2) && f2 != f1) paste0("Příloha 2 uvádí frekvenci '", f2, "'"),
        if (is.na(f2)) "Příloha 2 frekvenci neuvádí"
      )
      x <- c(paste0(METODIKA, ", Příloha 1 (rozhodující)"), x)
      paste(x, collapse = "; ")
    })
  ) %>%
  dplyr::arrange(DRUH)

#----------------------------------------------------------#
# Ciselnik nazvu: evropsky seznam <-> NDOP -----
#----------------------------------------------------------#
seznam <- openxlsx::read.xlsx("Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx", sheet = 1)
seznam_druhy <- seznam %>%
  dplyr::filter(`Typ.předmětu.ochrany` == "druh") %>%
  dplyr::distinct(nazev_n2k = `Název.latinsky.(druh)`, kod_sdf = `Kód.SDF`)

nazvy <- tibble::tibble(nazev_ndop = druhy) %>%
  dplyr::left_join(NAZVY_N2K, by = c("nazev_ndop" = "DRUH")) %>%
  dplyr::mutate(nazev_n2k = dplyr::coalesce(nazev_n2k, nazev_ndop)) %>%
  dplyr::left_join(seznam_druhy, by = "nazev_n2k") %>%
  dplyr::transmute(
    nazev_n2k, nazev_ndop, kod_sdf, skupina = "Cévnaté rostliny",
    pozn = dplyr::case_when(
      is.na(kod_sdf) ~ "druh není v seznamu předmětolokalit",
      nazev_n2k != nazev_ndop ~ "název se liší - bez převodu 21_1 záznamy druhu zahodí",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::arrange(nazev_ndop)

#----------------------------------------------------------#
# Vyber lokalit (Priloha 4; Priloha 5 dosud nedodana) -----
#----------------------------------------------------------#
p4 <- cti_list("Příloha 4")
h4 <- radek_hlavicky(p4, "Kód lokality")
vyber <- p4[(h4 + 1):nrow(p4), 1:2] %>%
  stats::setNames(c("KOD_LOKALITY", "NAZEV_LOKALITY")) %>%
  dplyr::filter(!is.na(KOD_LOKALITY)) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish)) %>%
  dplyr::transmute(DRUH = "Campanula bohemica", KOD_LOKALITY, NAZEV_LOKALITY,
                   ZDROJ = paste0(METODIKA, ", Příloha 4"))
p5 <- cti_list("Příloha 5")

#----------------------------------------------------------#
# Validace (strukturalni, bez hodnoceni) -----
#----------------------------------------------------------#
kontrola <- list()
# Dosavadni limity z gitu - zivy soubor uz je vystupem tohoto skriptu.
zaklad <- tempfile(fileext = ".csv")
stav_git <- system2("git", c("show", paste0(ZAKLAD_DIFF, ":Data/Input/limity_cevky.csv")),
                    stdout = zaklad, stderr = FALSE)
if (!identical(as.integer(stav_git), 0L) || !file.exists(zaklad) || file.size(zaklad) == 0) {
  stop("Dosavadni limity nelze nacist z gitu (", ZAKLAD_DIFF, ":Data/Input/limity_cevky.csv).")
}
limity_stare <- cti_cp1250(zaklad)
cis_ind <- cti_cp1250("Data/Input/cis_indikatory_popis.csv")

# 1. Sloupce a jejich poradi jako v dosavadnim souboru
kontrola$sloupce <- identical(names(limity_nove), names(limity_stare))
if (!kontrola$sloupce) stop("Sloupce se lisi od limity_cevky.csv.")

# 2. Domeny
stopifnot(
  all(limity_nove$TYP_IND %in% c("min", "max", "val", "info")),
  all(limity_nove$KLIC %in% c("ano", "ne", "info", NA)),
  all(limity_nove$UROVEN %in% c("lok", "chu")),
  all(!is.na(suppressWarnings(as.numeric(limity_nove$LIM_IND[limity_nove$TYP_IND %in% c("min", "max")])))),
  all(is.na(limity_nove$LIM_IND[limity_nove$TYP_IND == "info"])),
  all(!is.na(limity_nove$KLIC[limity_nove$TYP_IND != "info"]))
)

# 3. Kazdy druh Prilohy 1 je v limitech
kontrola$chybi_druh <- setdiff(druhy, limity_nove$DRUH)
if (length(kontrola$chybi_druh) > 0) stop("Chybi druhy: ", paste(kontrola$chybi_druh, collapse = ", "))

# 4. Duplicity a jediny prah min/max na indikator
kontrola$duplicity <- limity_nove %>% dplyr::count(DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, UROVEN) %>% dplyr::filter(n > 1)
kontrola$vice_prahu <- limity_nove %>% dplyr::filter(TYP_IND %in% c("min", "max")) %>%
  dplyr::count(DRUH, ID_IND, UROVEN, TYP_IND) %>% dplyr::filter(n > 1)
kontrola$smiseny_typ <- limity_nove %>% dplyr::filter(TYP_IND != "info") %>%
  dplyr::distinct(DRUH, ID_IND, UROVEN, TYP_IND) %>% dplyr::count(DRUH, ID_IND, UROVEN) %>%
  dplyr::filter(n > 1)
kontrola$smiseny_klic <- limity_nove %>% dplyr::distinct(DRUH, ID_IND, UROVEN, KLIC) %>%
  dplyr::count(DRUH, ID_IND, UROVEN) %>% dplyr::filter(n > 1)
if (nrow(kontrola$duplicity) + nrow(kontrola$vice_prahu) + nrow(kontrola$smiseny_klic) > 0) {
  stop("Duplicity, vice prahu nebo smiseny KLIC v limitech - viz objekt 'kontrola'.")
}

# 5. Pokryti ciselnikem indikatoru (ind_popis + ind_id)
kontrola$ciselnik <- tibble::tibble(ID_IND = unique(limity_nove$ID_IND)) %>%
  dplyr::left_join(cis_ind %>% dplyr::distinct(ind_r, .keep_all = TRUE), by = c("ID_IND" = "ind_r")) %>%
  dplyr::mutate(stav = dplyr::case_when(
    is.na(ind_popis) & is.na(ind_id) ~ "chybí v číselníku",
    is.na(ind_id) ~ "bez ind_id (přidělí ISOP)",
    TRUE ~ "OK"
  ))

# 6. Slovnik hodnot v datech NDOP (volitelne)
# Oba exporty jako v 02_n2k_data_druhy.R (bind_rows + distinct).
kontrola$slovnik_overen <- all(file.exists(EXPORT_NDOP))
if (kontrola$slovnik_overen) {
  ndop <- purrr::map_dfr(EXPORT_NDOP, function(f) {
    readr::read_csv(
      f, locale = readr::locale(encoding = "UTF-8"),
      col_types = readr::cols_only(ID_ND_NALEZ = "c", DRUH = "c", POCITANO = "c", PROJEKT = "c",
                                   STRUKT_POZN = "c", KOD_LOKALITY = "c", SKUPINA = "c")
    )
  }) %>%
    dplyr::distinct() %>%
    dplyr::filter(SKUPINA == "Cévnaté rostliny", PROJEKT == "Monitoring druhů ČR")

  tagy <- ndop %>%
    dplyr::filter(!is.na(STRUKT_POZN)) %>%
    dplyr::mutate(m = stringr::str_match_all(STRUKT_POZN, "<([A-Za-z][A-Za-z0-9_]*)>([^<]*)</([A-Za-z][A-Za-z0-9_]*)>")) %>%
    dplyr::select(DRUH, m) %>%
    dplyr::mutate(m = lapply(m, function(z) tibble::tibble(tag = z[, 2], hodnota = stringr::str_squish(z[, 3])))) %>%
    tidyr::unnest(m)

  # a) hodnoty limitu typu val proti hodnotam tagu (val_shoda v 21_2 squishuje)
  kontrola$slovnik <- lim_sta %>%
    dplyr::distinct(ID_IND, LIM_IND) %>%
    dplyr::left_join(P3_IND %>% dplyr::select(ID_IND, tag_ndop), by = "ID_IND") %>%
    dplyr::left_join(tagy %>% dplyr::count(tag, hodnota, name = "n_dat"),
                     by = c("tag_ndop" = "tag", "LIM_IND" = "hodnota")) %>%
    dplyr::mutate(n_dat = dplyr::coalesce(n_dat, 0L)) %>%
    dplyr::filter(n_dat == 0) %>%
    dplyr::left_join(
      tagy %>% dplyr::group_by(tag) %>%
        dplyr::summarise(hodnoty_v_datech = paste(sort(unique(hodnota)), collapse = " | "), .groups = "drop"),
      by = c("tag_ndop" = "tag")
    )

  # b) bunky s vyctem jen sesticlenne stupnice, kde data druhu nesou
  #    peticlennou tridu "do 25 %" - takovy zaznam limit nikdy nesplni
  kontrola$jen_6tridni <- lim_sta %>%
    dplyr::group_by(DRUH, ID_IND) %>%
    dplyr::summarise(ma_do5 = any(LIM_IND == "do 5 %"), ma_do25 = any(LIM_IND == "do 25 %"), .groups = "drop") %>%
    dplyr::filter(ma_do5, !ma_do25) %>%
    dplyr::left_join(P3_IND %>% dplyr::select(ID_IND, tag_ndop), by = "ID_IND") %>%
    dplyr::left_join(
      tagy %>% dplyr::filter(hodnota == "do 25 %") %>% dplyr::count(DRUH, tag, name = "n_do25"),
      by = c("DRUH", "tag_ndop" = "tag")
    ) %>%
    dplyr::mutate(n_do25 = dplyr::coalesce(n_do25, 0L))

  # c) funkcni jednotky proti POCITANO v datech druhu
  pocitano <- ndop %>% dplyr::mutate(POCITANO = stringr::str_squish(POCITANO)) %>%
    dplyr::count(DRUH, POCITANO, name = "n_dat")
  kontrola$jednotky <- JEDNOTKY %>%
    dplyr::left_join(pocitano, by = c("DRUH", "JEDNOTKA" = "POCITANO")) %>%
    dplyr::mutate(n_dat = dplyr::coalesce(n_dat, 0L))
  # plochy v jine plosne jednotce prevadi 21_1 (koef_plochy) - vyznacit
  plosne <- JEDNOTKY %>% dplyr::filter(ID_IND == "POP_POCET", JEDNOTKA %in% c("cm2", "dm2", "m2")) %>%
    dplyr::distinct(DRUH, cil = JEDNOTKA)
  kontrola$jednotky_nezarazene <- pocitano %>%
    dplyr::filter(DRUH %in% druhy, !is.na(POCITANO)) %>%
    dplyr::anti_join(JEDNOTKY, by = c("DRUH", "POCITANO" = "JEDNOTKA")) %>%
    dplyr::left_join(plosne, by = "DRUH") %>%
    dplyr::mutate(pozn = dplyr::if_else(
      !is.na(cil) & stringr::str_detect(POCITANO, "^(plocha populace [(])?[cd]?m2[)]?$"),
      paste0("21_1 převádí na ", cil), ""
    )) %>%
    dplyr::select(-cil) %>%
    dplyr::arrange(DRUH, dplyr::desc(n_dat))

  # d) kody lokalit Prilohy 4 v datech
  kontrola$vyber_bez_dat <- setdiff(vyber$KOD_LOKALITY, ndop$KOD_LOKALITY[ndop$DRUH == "Campanula bohemica"])
}

#----------------------------------------------------------#
# Diff proti limity_cevky.csv -----
#----------------------------------------------------------#
klic_radku <- function(df) {
  df %>% dplyr::mutate(radek = paste(TYP_IND, dplyr::coalesce(LIM_IND, "NA"),
                                     dplyr::coalesce(JEDNOTKA, "NA"), dplyr::coalesce(KLIC, "NA"), sep = " | "))
}
stare <- limity_stare %>%
  dplyr::mutate(
    DRUH = dplyr::coalesce(unname(NAZVY_STARE_LIMITY[DRUH]), DRUH),
    ID_IND_puv = ID_IND,
    ID_IND = dplyr::coalesce(unname(PREJMENOVANI[ID_IND]), ID_IND)
  ) %>%
  klic_radku()
nove <- limity_nove %>% klic_radku()

diff <- dplyr::full_join(
  stare %>% dplyr::group_by(DRUH, ID_IND, UROVEN) %>%
    dplyr::summarise(puvodni = paste(sort(unique(radek)), collapse = " || "),
                     ID_puvodni = paste(sort(unique(ID_IND_puv)), collapse = ", "), .groups = "drop"),
  nove %>% dplyr::group_by(DRUH, ID_IND, UROVEN) %>%
    dplyr::summarise(novy = paste(sort(unique(radek)), collapse = " || "), .groups = "drop"),
  by = c("DRUH", "ID_IND", "UROVEN")
) %>%
  dplyr::mutate(
    zmena = dplyr::case_when(
      is.na(puvodni) ~ "přidáno",
      is.na(novy) ~ "odebráno",
      puvodni == novy & ID_puvodni == ID_IND ~ "beze změny",
      puvodni == novy ~ "přejmenováno",
      TRUE ~ "změněno"
    ),
    ID_puvodni = dplyr::if_else(!is.na(ID_puvodni) & ID_puvodni != ID_IND, ID_puvodni, NA_character_)
  ) %>%
  dplyr::arrange(DRUH, UROVEN, ID_IND) %>%
  dplyr::select(DRUH, UROVEN, ID_IND, ID_puvodni, zmena, puvodni, novy)

#----------------------------------------------------------#
# Zapis vystupu -----
#----------------------------------------------------------#
zapis_csv(limity_nove, VYSTUP$limity,
          c(DRUH = "NA", ID_IND = "NA", TYP_IND = "NA", LIM_IND = "NA",
            JEDNOTKA = "NA", KLIC = "NA", UROVEN = "NA", pozn = ""))
zapis_csv(frekvence, VYSTUP$frekvence, "")
zapis_csv(nazvy, VYSTUP$nazvy, "")
zapis_csv(vyber, VYSTUP$vyber, "")
zapis_csv(diff, VYSTUP$diff, "")

# Kontrola zpetneho nacteni stejnym volanim jako 00_n2k_config.R
zpet <- readr::read_csv(VYSTUP$limity, locale = readr::locale(encoding = ENC), show_col_types = FALSE)
stopifnot(nrow(zpet) == nrow(limity_nove), identical(names(zpet), names(limity_stare)))

#----------------------------------------------------------#
# Protokol -----
#----------------------------------------------------------#
md_tab <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("_(žádné)_")
  df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), ~ stringr::str_replace_all(dplyr::coalesce(as.character(.), ""), "\\|", "\\\\|")))
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    do.call(paste, c(df, sep = " | ")) %>% paste0("| ", ., " |"))
}

souhrn_diff <- diff %>% dplyr::count(zmena) %>% dplyr::arrange(dplyr::desc(n))
souhrn_id <- diff %>% dplyr::count(ID_IND, zmena) %>%
  tidyr::pivot_wider(names_from = zmena, values_from = n, values_fill = 0) %>% dplyr::arrange(ID_IND)
souhrn_druh <- diff %>% dplyr::count(DRUH, zmena) %>%
  tidyr::pivot_wider(names_from = zmena, values_from = n, values_fill = 0) %>% dplyr::arrange(DRUH)

prot <- c(
  "# Převod limitů cévnatých rostlin — protokol validace a souhrn rozdílů",
  "",
  paste0("Generováno skriptem `R/05_metodiky/54_cevnate_limity_2026.R` (", Sys.Date(), "). ",
         "Vstup: `", ZDROJ_XLSX, "` (", METODIKA, "). Rozhodnutí a výklady: ",
         "[harmonizace_registr.md](harmonizace_registr.md), Fáze C."),
  "",
  "## Výstupy",
  "",
  md_tab(tibble::tibble(
    soubor = unlist(VYSTUP),
    radku = c(nrow(limity_nove), nrow(frekvence), nrow(nazvy), nrow(vyber), nrow(diff), NA)
  )),
  "",
  "## Strukturální kontroly",
  "",
  paste0("- Sloupce a jejich pořadí shodné s dosavadním `limity_cevky.csv` (git `", ZAKLAD_DIFF, "`): **", kontrola$sloupce, "**"),
  paste0("- Druhů v limitech: **", dplyr::n_distinct(limity_nove$DRUH), "** (Příloha 1: ", length(druhy), "; chybí: ",
         if (length(kontrola$chybi_druh)) paste(kontrola$chybi_druh, collapse = ", ") else "žádný", ")"),
  paste0("- Duplicitní řádky: **", nrow(kontrola$duplicity), "**; indikátory s více než jedním prahem min/max: **",
         nrow(kontrola$vice_prahu), "**; indikátory se smíšeným KLIC: **", nrow(kontrola$smiseny_klic), "**"),
  paste0("- Zpětné načtení stejným voláním jako `00_n2k_config.R` (Windows-1250): **", nrow(zpet), " řádků**"),
  paste0("- Řádků podle TYP_IND: ", paste(names(table(limity_nove$TYP_IND)), table(limity_nove$TYP_IND), sep = " ", collapse = ", ")),
  paste0("- Řádků podle KLIC: ", paste(names(table(limity_nove$KLIC, useNA = "ifany")), table(limity_nove$KLIC, useNA = "ifany"), sep = " ", collapse = ", ")),
  "",
  "### Pokrytí číselníkem `cis_indikatory_popis.csv`",
  "",
  md_tab(kontrola$ciselnik %>% dplyr::select(ID_IND, ind_popis, ind_id, stav) %>% dplyr::arrange(stav, ID_IND)),
  "",
  "### Indikátory bez výpočtu v kódu nebo s jiným výpočtem",
  "",
  md_tab(dplyr::bind_rows(
    tibble::tibble(ID_IND = BEZ_VYPOCTU, stav = "kód 21_1 indikátor nepočítá (limit je zatím sirotek)"),
    tibble::tibble(ID_IND = names(JINY_VYPOCET), stav = unname(JINY_VYPOCET))
  ) %>% dplyr::filter(ID_IND %in% limity_nove$ID_IND)),
  "",
  "### Mezery v buňkách Přílohy 2 (oříznuty)",
  "",
  md_tab(p2_mezery),
  "",
  "### Příloha 5",
  "",
  paste0("Obsah listu: „", stringr::str_squish(p5[[1]][1]), "“ — seznam lokalit *Cerastium alsinifolium* nedodán (blokuje výběr lokalit tohoto druhu)."),
  ""
)

if (kontrola$slovnik_overen) {
  prot <- c(prot,
    "## Kontrola proti slovníku hodnot v datech NDOP",
    "",
    paste0("Záznamy projektu Monitoring druhů ČR, cévnaté rostliny, oba exporty (", nrow(ndop), " záznamů)."),
    "",
    "### Kategorie limitů, které se v datech daného tagu nevyskytují",
    "",
    md_tab(kontrola$slovnik %>% dplyr::select(ID_IND, tag_ndop, LIM_IND, hodnoty_v_datech)),
    "",
    "### Buňky jen se šestičlennou stupnicí, kde data druhu nesou pětičlennou třídu „do 25 %“",
    "",
    "Záznam „do 25 %“ takový limit nikdy nesplní (Příloha 3 nevyjmenovává „do 25 %“).",
    "",
    md_tab(kontrola$jen_6tridni %>% dplyr::filter(n_do25 > 0) %>% dplyr::select(DRUH, ID_IND, n_do25)),
    "",
    "### Funkční jednotky (JEDNOTKA ↔ POCITANO)",
    "",
    md_tab(kontrola$jednotky %>% dplyr::select(DRUH, ID_IND, JEDNOTKA, stav, n_dat, pozn)),
    "",
    "### Hodnoty POCITANO v datech, které žádná jednotka nezařazuje",
    "",
    "Plošné jednotky, které `21_1` převádí na plošnou jednotku Přílohy 1, jsou vyznačeny ve sloupci `pozn`.",
    "",
    md_tab(kontrola$jednotky_nezarazene),
    "",
    paste0("### Kódy Přílohy 4 bez jediného záznamu v NDOP: ", length(kontrola$vyber_bez_dat), " ze ", nrow(vyber)),
    "",
    paste(kontrola$vyber_bez_dat, collapse = ", "),
    ""
  )
} else {
  prot <- c(prot, "## Kontrola proti slovníku hodnot v datech NDOP", "",
            paste0("**Neověřeno** — exporty `", paste(EXPORT_NDOP, collapse = "`, `"), "` nejsou k dispozici."), "")
}

prot <- c(prot,
  paste0("## Rozdíl proti dosavadním limitům (`limity_cevky.csv`, git `", ZAKLAD_DIFF, "`)"),
  "",
  paste0("Úplný rozdíl po řádcích: [`limity_cevky_2026_diff.csv`](limity_cevky_2026_diff.csv). ",
         "Klíč porovnání je druh × indikátor × úroveň; dosavadní názvy druhů a ID jsou před ",
         "porovnáním převedeny (", paste(names(NAZVY_STARE_LIMITY), NAZVY_STARE_LIMITY, sep = " → ", collapse = "; "),
         "; ", paste(names(PREJMENOVANI), PREJMENOVANI, sep = " → ", collapse = "; "), ")."),
  "",
  md_tab(souhrn_diff),
  "",
  "### Podle indikátoru",
  "",
  md_tab(souhrn_id),
  "",
  "### Podle druhu",
  "",
  md_tab(souhrn_druh),
  ""
)

con <- file(VYSTUP$kontrola, open = "wb")
writeLines(enc2utf8(prot), con, sep = "\n", useBytes = TRUE)
close(con)

message("Hotovo: ", nrow(limity_nove), " radku limitu, ", dplyr::n_distinct(limity_nove$DRUH), " druhu.")
