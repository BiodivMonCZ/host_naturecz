# Převod limitů cévnatých rostlin — protokol validace a souhrn rozdílů

Generováno skriptem `R/05_metodiky/54_cevnate_limity_2026.R` (2026-10-28). Vstup: `Metodiky/Cevnate/Tabulky fin.xlsx` (metodika 2026-10-21). Rozhodnutí a výklady: [harmonizace_registr.md](harmonizace_registr.md), Fáze C.

## Výstupy

| soubor | radku |
|---|---|
| Data/Input/limity_cevky_2026.csv | 941 |
| Data/Input/cis_cevky_frekvence.csv | 36 |
| Data/Input/cis_druhy_nazvy.csv | 36 |
| Data/Input/cevky_vyber_lokalit.csv | 120 |
| Metodiky/Cevnate/limity_cevky_2026_diff.csv | 549 |
| Metodiky/Cevnate/limity_cevky_2026_kontrola.md |  |

## Strukturální kontroly

- Sloupce a jejich pořadí shodné s `limity_cevky.csv`: **TRUE**
- Druhů v limitech: **36** (Příloha 1: 36; chybí: žádný)
- Duplicitní řádky: **0**; indikátory s více než jedním prahem min/max: **0**; indikátory se smíšeným KLIC: **0**
- Zpětné načtení stejným voláním jako `00_n2k_config.R` (Windows-1250): **941 řádků**
- Řádků podle TYP_IND: info 115, max 37, min 192, val 597
- Řádků podle KLIC: ano 144, info 525, ne 157, NA 115

### Pokrytí číselníkem `cis_indikatory_popis.csv`

| ID_IND | ind_popis | ind_id | stav |
|---|---|---|---|
| LOK_PROCDOBR | procento dílčích lokalit v dobrém stavu | 189 | OK |
| POP_TREND | vývoj početnosti populace | 80 | OK |
| POP_CILSTAVPROC | cílový stav (podíl celkové početnosti a cílového stavu území) |  | bez ind_id (přidělí ISOP) |
| POP_DELKAABSENCE | délka absence druhu |  | bez ind_id (přidělí ISOP) |
| POP_POCET | početnost populace (počet populačních jednotek) |  | bez ind_id (přidělí ISOP) |
| POP_POCETCIL | početnost (podíl cílové početnosti lokality) |  | bez ind_id (přidělí ISOP) |
| POP_POCETMINPRIP | minimální přípustná početnost |  | bez ind_id (přidělí ISOP) |
| POP_POCETSTERIL | počet sterilních populačních jednotek |  | bez ind_id (přidělí ISOP) |
| POP_POCETSUM | součet populačních jednotek v rámci návštěvy |  | bez ind_id (přidělí ISOP) |
| POP_POCETVITAL | počet kvetoucích a plodných populačních jednotek |  | bez ind_id (přidělí ISOP) |
| POP_POSKPERC | poškození rostlin |  | bez ind_id (přidělí ISOP) |
| POP_PROCDOB | podíl populace v lokalitách v dobrém stavu |  | bez ind_id (přidělí ISOP) |
| POP_REPROUSP | úspěšnost reprodukce |  | bez ind_id (přidělí ISOP) |
| POP_VITAL | vitalita populace |  | bez ind_id (přidělí ISOP) |
| STA_HYDRPOMERY | hydrologické poměry |  | bez ind_id (přidělí ISOP) |
| STA_POC_OST_NATASUB_DRH | pokryvnost ostatních natantních a submerzních druhů |  | bez ind_id (přidělí ISOP) |
| STA_POKLESHLAD | termín a doba trvání poklesu vodní hladiny |  | bez ind_id (přidělí ISOP) |
| STA_POKRDNO | podíl dna nad vodní hladinou |  | bez ind_id (přidělí ISOP) |
| STA_POKRDREV | pokryvnost dřevin |  | bez ind_id (přidělí ISOP) |
| STA_POKRDREVNIZ | pokryvnost dřevin nižších než 1 m |  | bez ind_id (přidělí ISOP) |
| STA_POKRRAKOS | pokryvnost rákosu |  | bez ind_id (přidělí ISOP) |
| STA_POKRSTAR | pokryvnost stařiny |  | bez ind_id (přidělí ISOP) |
| STA_POKRYVNOSTE0 | pokryvnost E0 |  | bez ind_id (přidělí ISOP) |
| STA_POKRYVNOSTE1 | pokryvnost E1 |  | bez ind_id (přidělí ISOP) |
| STA_POKRYVNOSTE2E3 | pokryvnost dřevin vyšších než 1 m |  | bez ind_id (přidělí ISOP) |
| STA_POKRYVNOSTEXPANZNI | pokryvnost expanzivních druhů |  | bez ind_id (přidělí ISOP) |
| STA_POKRYVNOSTINVAZNI | pokryvnost invazních druhů |  | bez ind_id (přidělí ISOP) |
| STA_POKRYVNOSTVOLNAPUDA | pokryvnost volné půdy |  | bez ind_id (přidělí ISOP) |
| STA_STUPENZACH | stupeň zachování stanoviště |  | bez ind_id (přidělí ISOP) |
| STA_VLHPOM | vlhkostní poměry stanoviště |  | bez ind_id (přidělí ISOP) |
| STA_ZASTINENICELK | celkové zastínění lokality |  | bez ind_id (přidělí ISOP) |

### Indikátory bez výpočtu v kódu nebo s jiným výpočtem

| ID_IND | stav |
|---|---|
| POP_REPROUSP | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| POP_DELKAABSENCE | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| POP_POCETMINPRIP | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| POP_POCETCIL | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| POP_CILSTAVPROC | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| STA_POKRRAKOS | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| STA_POC_OST_NATASUB_DRH | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| STA_POKRDNO | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| STA_POKLESHLAD | kód 21_1 indikátor nepočítá (limit je zatím sirotek) |
| POP_TREND | kod pocita cislo 0-2 (obracena logika, H-03), limit ocekava kategorii |
| POP_POSKPERC | kod scita kategorie 0-3 (H-12), limit ocekava soucet procent (medianu) |

### Mezery v buňkách Přílohy 2 (oříznuty)

| DRUH | sloupec | hodnota |
|---|---|---|
| Dianthus arenarius subsp. bohemicus | Vitalita |  ≥ 30 % |
| Poa riphaea | Vitalita |  ≥ 15 % |

### Příloha 5

Obsah listu: „Jednou určitě bude :-)“ — seznam lokalit *Cerastium alsinifolium* nedodán (blokuje výběr lokalit tohoto druhu).

## Kontrola proti slovníku hodnot v datech NDOP

Záznamy projektu Monitoring druhů ČR, cévnaté rostliny, oba exporty (8984 záznamů).

### Kategorie limitů, které se v datech daného tagu nevyskytují

| ID_IND | tag_ndop | LIM_IND | hodnoty_v_datech |
|---|---|---|---|
| STA_POKRRAKOS |  | 0 |  |
| STA_POKRRAKOS |  | do 5 % |  |
| STA_POKRRAKOS |  | do 25 % |  |
| STA_POKRRAKOS |  | 5-25 % |  |
| STA_POKRRAKOS |  | 25-50 % |  |
| STA_POKRRAKOS |  | 50-75 % |  |
| STA_POC_OST_NATASUB_DRH | STA_POKRYVNOSTNATDRUH | do 5 % | 0 \| 50-75 % \| 75-100 % \| do 25 % |
| STA_POC_OST_NATASUB_DRH | STA_POKRYVNOSTNATDRUH | 5-25 % | 0 \| 50-75 % \| 75-100 % \| do 25 % |
| STA_VLHPOM | STA_VLHKOPOMERY | zhoršené | dobré |
| STA_POKRDNO | STA_PODILDNONADHLADINA | 51-100 % | 11-50 % \| 51-100% \| do 10 % |

### Buňky jen se šestičlennou stupnicí, kde data druhu nesou pětičlennou třídu „do 25 %“

Záznam „do 25 %“ takový limit nikdy nesplní (Příloha 3 nevyjmenovává „do 25 %“).

_(žádné)_

### Funkční jednotky (JEDNOTKA ↔ POCITANO)

| DRUH | ID_IND | JEDNOTKA | stav | n_dat | pozn |
|---|---|---|---|---|---|
| Aconitum firmum subsp. moravicum | POP_POCET | prýty | shoda | 875 |  |
| Aconitum firmum subsp. moravicum | POP_POCETSUM | sterilní prýty | shoda | 271 |  |
| Aconitum firmum subsp. moravicum | POP_POCETSUM | kvetoucí a plodné prýty | shoda | 207 |  |
| Aconitum firmum subsp. moravicum | POP_POCETSUM | plodné prýty | shoda | 64 |  |
| Aconitum firmum subsp. moravicum | POP_POCETVITAL | kvetoucí a plodné prýty | shoda | 207 |  |
| Aconitum firmum subsp. moravicum | POP_POCETVITAL | plodné prýty | shoda | 64 |  |
| Aconitum firmum subsp. moravicum | POP_POCETSTERIL | sterilní prýty | shoda | 271 |  |
| Adenophora liliifolia | POP_POCET | trsy | shoda | 56 |  |
| Adenophora liliifolia | POP_POCETSUM | sterilní trsy | shoda | 30 |  |
| Adenophora liliifolia | POP_POCETSUM | kvetoucí a plodné trsy | shoda | 30 |  |
| Adenophora liliifolia | POP_POCETVITAL | kvetoucí a plodné trsy | shoda | 30 |  |
| Adenophora liliifolia | POP_POCETSTERIL | sterilní trsy | shoda | 30 |  |
| Angelica palustris | POP_POCET | prýty | chybí | 0 | v datech jen součet částí; 'jedinci' nezařazeno |
| Angelica palustris | POP_POCETSUM | sterilní prýty | shoda | 3 |  |
| Angelica palustris | POP_POCETSUM | kvetoucí a plodné prýty | shoda | 3 |  |
| Angelica palustris | POP_POCETVITAL | kvetoucí a plodné prýty | shoda | 3 |  |
| Angelica palustris | POP_POCETSTERIL | sterilní prýty | shoda | 3 |  |
| Artemisia pancicii | POP_POCET | prýty | shoda | 67 |  |
| Artemisia pancicii | POP_POCETSUM | sterilní prýty | shoda | 14 |  |
| Artemisia pancicii | POP_POCETSUM | kvetoucí a plodné prýty | shoda | 14 |  |
| Asplenium adulterinum | POP_POCET | trsy | shoda | 163 |  |
| Campanula bohemica | POP_POCET | kvetoucí a plodné trsy | shoda | 327 | 'kvetoucí jedinci' 2011-2016 nezařazeno (jiný orgán) |
| Campanula gelida | POP_POCET | trsy | shoda | 19 |  |
| Cerastium alsinifolium | POP_POCET | m2 | shoda | 4 | Příloha 2 uvádí dm2; data převážně cm2 - nezařazeno, převod jednotek format nezná |
| Cirsium brachycephalum | POP_POCET | kvetoucí a plodní jedinci | shoda | 15 |  |
| Cirsium brachycephalum | POP_POCET | kvetoucí jedinci | kandidát | 22 | starší tvar 2011-2021 |
| Coleanthus subtilis | POP_POCET | jedinci | shoda | 83 | relativní stupnice REL_POC se váže na tuto jednotku |
| Crambe tataria | POP_POCET | jedinci | shoda | 1 |  |
| Crambe tataria | POP_POCETSUM | sterilní jedinci | shoda | 28 |  |
| Crambe tataria | POP_POCETSUM | kvetoucí a plodní jedinci | shoda | 14 |  |
| Crambe tataria | POP_POCETSUM | kvetoucí jedinci | shoda | 63 |  |
| Crambe tataria | POP_POCETVITAL | kvetoucí a plodní jedinci | shoda | 14 |  |
| Crambe tataria | POP_POCETVITAL | kvetoucí jedinci | kandidát | 63 | starší tvar |
| Crambe tataria | POP_POCETSTERIL | sterilní jedinci | shoda | 28 |  |
| Cypripedium calceolus | POP_POCET | trsy | shoda | 567 |  |
| Cypripedium calceolus | POP_POCETSUM | sterilní trsy | shoda | 131 |  |
| Cypripedium calceolus | POP_POCETSUM | kvetoucí a plodné trsy | shoda | 130 |  |
| Cypripedium calceolus | POP_POCETVITAL | kvetoucí a plodné trsy | shoda | 130 |  |
| Cypripedium calceolus | POP_POCETSTERIL | sterilní trsy | shoda | 131 |  |
| Dianthus arenarius subsp. bohemicus | POP_POCET | trsy | shoda | 4 |  |
| Dianthus arenarius subsp. bohemicus | POP_POCETSUM | sterilní trsy | shoda | 2 |  |
| Dianthus arenarius subsp. bohemicus | POP_POCETSUM | kvetoucí a plodné trsy | shoda | 1 |  |
| Dianthus arenarius subsp. bohemicus | POP_POCETSUM | kvetoucí trsy | shoda | 1 |  |
| Dianthus arenarius subsp. bohemicus | POP_POCETSUM | plodné trsy | shoda | 1 |  |
| Dianthus arenarius subsp. bohemicus | POP_POCETVITAL | kvetoucí a plodné trsy | kandidát | 1 | 'kvetoucí' a 'plodné' trsy v téže akci se mohou překrývat |
| Dianthus arenarius subsp. bohemicus | POP_POCETVITAL | kvetoucí trsy | kandidát | 1 | 'kvetoucí' a 'plodné' trsy v téže akci se mohou překrývat |
| Dianthus arenarius subsp. bohemicus | POP_POCETVITAL | plodné trsy | kandidát | 1 | 'kvetoucí' a 'plodné' trsy v téže akci se mohou překrývat |
| Dianthus arenarius subsp. bohemicus | POP_POCETSTERIL | sterilní trsy | shoda | 2 |  |
| Dianthus lumnitzeri | POP_POCET | kvetoucí a plodné trsy | shoda | 5 |  |
| Dianthus moravicus | POP_POCET | kvetoucí a plodné trsy | shoda | 6 |  |
| Dracocephalum austriacum | POP_POCET | trsy | chybí | 0 | Příloha 1 'trs/kvetoucí a plodný trs'; v datech jen součet částí; 'jedinci' nezařazeno |
| Dracocephalum austriacum | POP_POCETSUM | sterilní trsy | shoda | 28 |  |
| Dracocephalum austriacum | POP_POCETSUM | kvetoucí a plodné trsy | shoda | 37 |  |
| Dracocephalum austriacum | POP_POCETVITAL | kvetoucí a plodné trsy | shoda | 37 |  |
| Dracocephalum austriacum | POP_POCETSTERIL | sterilní trsy | shoda | 28 |  |
| Echium maculatum | POP_POCET | kvetoucí a plodní jedinci | shoda | 39 | 'jedinci' (246 záznamů 2006-2022) nezařazeno - celkový počet, ne kvetoucí |
| Echium maculatum | POP_POCET | kvetoucí jedinci | kandidát | 2 | starší tvar |
| Galium sudeticum | POP_POCET | m2 | shoda | 3 | Příloha 2 uvádí dm2 |
| Gentianella praecox subsp. bohemica | POP_POCET | kvetoucí a plodní jedinci | shoda | 65 |  |
| Gentianella praecox subsp. bohemica | POP_POCET | kvetoucí jedinci | kandidát | 317 | starší tvar 2005-2022 |
| Gladiolus palustris | POP_POCET | kvetoucí a plodní jedinci | shoda | 143 | 'jedinci' (392 záznamů 2006-2022) nezařazeno - celkový počet |
| Gladiolus palustris | POP_POCET | kvetoucí jedinci | kandidát | 7 | starší tvar |
| Himantoglossum adriaticum | POP_POCET | jedinci | shoda | 16 |  |
| Himantoglossum adriaticum | POP_POCETSUM | sterilní jedinci | shoda | 3 |  |
| Himantoglossum adriaticum | POP_POCETSUM | kvetoucí a plodní jedinci | shoda | 2 |  |
| Himantoglossum adriaticum | POP_POCETSUM | kvetoucí jedinci | shoda | 1 |  |
| Himantoglossum adriaticum | POP_POCETVITAL | kvetoucí a plodní jedinci | shoda | 2 |  |
| Himantoglossum adriaticum | POP_POCETVITAL | kvetoucí jedinci | kandidát | 1 | starší tvar |
| Himantoglossum adriaticum | POP_POCETSTERIL | sterilní jedinci | shoda | 3 |  |
| Iris arenaria | POP_POCET | prýty | shoda | 85 |  |
| Jurinea cyanoides | POP_POCET | prýty | chybí | 0 | v datech jen součet částí; 'ramety' nezařazeno |
| Jurinea cyanoides | POP_POCETSUM | sterilní prýty | shoda | 16 |  |
| Jurinea cyanoides | POP_POCETSUM | kvetoucí a plodné prýty | shoda | 16 |  |
| Jurinea cyanoides | POP_POCETVITAL | kvetoucí a plodné prýty | shoda | 16 |  |
| Jurinea cyanoides | POP_POCETSTERIL | sterilní prýty | shoda | 16 |  |
| Klasea lycopifolia | POP_POCET | m2 | shoda | 47 | 'cm2' a 'plocha populace (cm2)' nezařazeno - převod jednotek format nezná |
| Klasea lycopifolia | POP_POCETVITAL | kvetoucí a plodné prýty | shoda | 17 |  |
| Klasea lycopifolia | POP_POCETSTERIL | sterilní prýty | shoda | 1 |  |
| Ligularia sibirica | POP_POCET | jedinci | shoda | 54 |  |
| Ligularia sibirica | POP_POCETSUM | sterilní jedinci | shoda | 11 |  |
| Ligularia sibirica | POP_POCETSUM | kvetoucí a plodní jedinci | shoda | 11 |  |
| Ligularia sibirica | POP_POCETSUM | kvetoucí jedinci | shoda | 1 |  |
| Ligularia sibirica | POP_POCETVITAL | kvetoucí a plodní jedinci | shoda | 11 |  |
| Ligularia sibirica | POP_POCETVITAL | kvetoucí jedinci | kandidát | 1 | starší tvar |
| Ligularia sibirica | POP_POCETSTERIL | sterilní jedinci | shoda | 11 |  |
| Liparis loeselii | POP_POCET | jedinci | shoda | 124 |  |
| Liparis loeselii | POP_POCETSUM | sterilní jedinci | shoda | 9 |  |
| Liparis loeselii | POP_POCETSUM | kvetoucí a plodní jedinci | shoda | 9 |  |
| Liparis loeselii | POP_POCETVITAL | kvetoucí a plodní jedinci | shoda | 9 |  |
| Liparis loeselii | POP_POCETSTERIL | sterilní jedinci | shoda | 9 |  |
| Luronium natans | POP_POCET | růžice | shoda | 8 | 'cm2' a 'm2' nezařazeno - převod jednotek format nezná |
| Minuartia smejkalii | POP_POCET | trsy | shoda | 117 |  |
| Pedicularis sudetica | POP_POCET | kvetoucí a plodné lodyhy | chybí | 0 | v datech jen 'trsy' |
| Poa riphaea | POP_POCET | trsy | shoda | 20 |  |
| Poa riphaea | POP_POCETVITAL | kvetoucí a plodné trsy | chybí | 0 | v datech jen trsy, stébla, semenáčky - vitalitu nelze spočítat |
| Poa riphaea | POP_POCETSTERIL | sterilní trsy | chybí | 0 |  |
| Pulsatilla grandis | POP_POCET | kvetoucí a plodné trsy | shoda | 278 |  |
| Pulsatilla grandis | POP_POCET | kvetoucí trsy | kandidát | 487 | starší tvar 2005-2021 |
| Pulsatilla patens | POP_POCET | trsy | shoda | 143 |  |
| Pulsatilla patens | POP_POCETSUM | sterilní trsy | shoda | 43 |  |
| Pulsatilla patens | POP_POCETSUM | kvetoucí a plodné trsy | shoda | 43 |  |
| Pulsatilla patens | POP_POCETSUM | kvetoucí trsy | shoda | 133 |  |
| Pulsatilla patens | POP_POCETVITAL | kvetoucí a plodné trsy | shoda | 43 |  |
| Pulsatilla patens | POP_POCETVITAL | kvetoucí trsy | kandidát | 133 | starší tvar 2009-2022 |
| Pulsatilla patens | POP_POCETSTERIL | sterilní trsy | shoda | 43 |  |
| Stipa glabrata | POP_POCET | trsy | shoda | 40 |  |
| Tephroseris longifolia subsp. moravica | POP_POCET | jedinci | shoda | 52 |  |
| Tephroseris longifolia subsp. moravica | POP_POCETSUM | sterilní jedinci | shoda | 4 |  |
| Tephroseris longifolia subsp. moravica | POP_POCETSUM | kvetoucí a plodní jedinci | shoda | 4 |  |
| Tephroseris longifolia subsp. moravica | POP_POCETVITAL | kvetoucí a plodní jedinci | shoda | 4 |  |
| Tephroseris longifolia subsp. moravica | POP_POCETSTERIL | sterilní jedinci | shoda | 4 |  |
| Thesium ebracteatum | POP_POCET | prýty | shoda | 21 |  |
| Thesium ebracteatum | POP_POCETSUM | sterilní prýty | shoda | 1 |  |
| Thesium ebracteatum | POP_POCETSUM | kvetoucí a plodné prýty | shoda | 1 |  |
| Trichomanes speciosum | POP_POCET | cm2 | shoda | 17 |  |

### Hodnoty POCITANO v datech, které žádná jednotka nezařazuje

| DRUH | POCITANO | n_dat |
|---|---|---|
| Adenophora liliifolia | kvetoucí a plodné lodyhy | 28 |
| Adenophora liliifolia | sterilní lodyhy | 28 |
| Adenophora liliifolia | jedinci | 25 |
| Adenophora liliifolia | semenáčky | 2 |
| Angelica palustris | jedinci | 4 |
| Artemisia pancicii | kvetoucí lodyhy | 14 |
| Campanula bohemica | kvetoucí jedinci | 86 |
| Campanula bohemica | cm2 | 2 |
| Campanula bohemica | jedinci | 1 |
| Campanula gelida | kvetoucí a plodné lodyhy | 4 |
| Campanula gelida | semenáčky | 4 |
| Cerastium alsinifolium | cm2 | 35 |
| Cerastium alsinifolium | dm2 | 3 |
| Cirsium brachycephalum | jedinci | 6 |
| Cirsium brachycephalum | sterilní jedinci | 2 |
| Coleanthus subtilis | m2 | 65 |
| Coleanthus subtilis | trsy | 1 |
| Crambe tataria | m2 | 10 |
| Cypripedium calceolus | sterilní lodyhy | 84 |
| Cypripedium calceolus | kvetoucí a plodné lodyhy | 76 |
| Cypripedium calceolus | semenáčky | 38 |
| Cypripedium calceolus | jedinci | 22 |
| Dianthus lumnitzeri | trsy | 31 |
| Dianthus lumnitzeri | sterilní trsy | 5 |
| Dianthus moravicus | trsy | 21 |
| Dianthus moravicus | sterilní trsy | 2 |
| Dracocephalum austriacum | jedinci | 118 |
| Dracocephalum austriacum | semenáčky | 29 |
| Echium maculatum | jedinci | 246 |
| Echium maculatum | sterilní jedinci | 26 |
| Gentianella praecox subsp. bohemica | jedinci | 2 |
| Gladiolus palustris | jedinci | 392 |
| Gladiolus palustris | sterilní jedinci | 61 |
| Iris arenaria | jedinci | 12 |
| Iris arenaria | kvetoucí a plodné prýty | 6 |
| Iris arenaria | trsy | 1 |
| Jurinea cyanoides | ramety | 24 |
| Jurinea cyanoides | semenáčky | 13 |
| Klasea lycopifolia | plocha populace (cm2) | 12 |
| Klasea lycopifolia | jedinci | 3 |
| Klasea lycopifolia | polykormony | 3 |
| Klasea lycopifolia | cm2 | 1 |
| Luronium natans | cm2 | 18 |
| Luronium natans | m2 | 9 |
| Minuartia smejkalii | semenáčky | 32 |
| Pedicularis sudetica | trsy | 23 |
| Poa riphaea | semenáčky | 4 |
| Poa riphaea | stébla | 4 |
| Pulsatilla grandis | trsy | 474 |
| Pulsatilla grandis | sterilní trsy | 147 |
| Pulsatilla grandis | semenáčky | 119 |
| Pulsatilla grandis | plodní jedinci | 2 |
| Pulsatilla grandis | kvetoucí jedinci | 1 |
| Pulsatilla patens | semenáčky | 31 |

### Kódy Přílohy 4 bez jediného záznamu v NDOP: 32 ze 120

Cboh269, Cboh332, Cboh342, Cboh343, Cboh345, Cboh349, Cboh358, Cboh362, Cboh365, Cboh367, Cboh373, Cboh378, Cboh385, Cboh390, Cboh392, Cboh397, Cboh401, Cboh402, Cboh403, Cboh404, Cboh405, Cboh406, Cboh407, Cboh410, Cboh413, Cboh415, Cboh421, Cboh427, Cboh429, Cboh432, Cboh435, Cboh439

## Rozdíl proti `limity_cevky.csv`

Úplný rozdíl po řádcích: [`limity_cevky_2026_diff.csv`](limity_cevky_2026_diff.csv). Klíč porovnání je druh × indikátor × úroveň; dosavadní názvy druhů a ID jsou před porovnáním převedeny (Iris humilis subsp. arenaria → Iris arenaria; Stipa zalesskii → Stipa glabrata; STA_HYDROPOMERY → STA_HYDRPOMERY; STA_POKRPUDA → STA_POKRYVNOSTVOLNAPUDA; STA_POKRVOLPUD → STA_POKRYVNOSTVOLNAPUDA).

| zmena | n |
|---|---|
| změněno | 307 |
| přidáno | 157 |
| odebráno | 44 |
| beze změny | 41 |

### Podle indikátoru

| ID_IND | beze změny | přidáno | odebráno | změněno |
|---|---|---|---|---|
| LOK_PROCDOBR | 34 | 2 | 0 | 0 |
| POP_CILSTAVPROC | 0 | 36 | 0 | 0 |
| POP_DELKAABSENCE | 0 | 1 | 0 | 0 |
| POP_PLOCHA | 0 | 0 | 3 | 0 |
| POP_POCET | 0 | 7 | 0 | 29 |
| POP_POCETCIL | 0 | 35 | 0 | 0 |
| POP_POCETMINPRIP | 0 | 35 | 0 | 0 |
| POP_POCETNOST | 0 | 0 | 1 | 0 |
| POP_POCETSPEC | 0 | 0 | 1 | 0 |
| POP_POCETSTERIL | 0 | 15 | 0 | 0 |
| POP_POCETSUM | 3 | 4 | 0 | 8 |
| POP_POCETSUMLOD | 0 | 0 | 1 | 0 |
| POP_POCETVITAL | 4 | 3 | 1 | 8 |
| POP_POPULACE | 0 | 0 | 1 | 0 |
| POP_POSKPERC | 0 | 3 | 0 | 33 |
| POP_POSKPOC | 0 | 0 | 31 | 0 |
| POP_PROCDOB | 0 | 2 | 1 | 33 |
| POP_REPROUSP | 0 | 0 | 0 | 1 |
| POP_TREND | 0 | 2 | 0 | 34 |
| POP_VITAL | 0 | 1 | 1 | 14 |
| STA_HYDRPOMERY | 0 | 0 | 0 | 3 |
| STA_POC_OST_NATASUB_DRH | 0 | 0 | 0 | 1 |
| STA_POKLESHLAD | 0 | 0 | 0 | 1 |
| STA_POKRDNO | 0 | 0 | 0 | 1 |
| STA_POKRDREV | 0 | 2 | 1 | 17 |
| STA_POKRDREVNIZ | 0 | 1 | 0 | 2 |
| STA_POKRRAKOS | 0 | 0 | 0 | 2 |
| STA_POKRSTAR | 0 | 2 | 0 | 25 |
| STA_POKRYVNOSTE0 | 0 | 0 | 0 | 3 |
| STA_POKRYVNOSTE1 | 0 | 1 | 1 | 12 |
| STA_POKRYVNOSTE2E3 | 0 | 0 | 0 | 8 |
| STA_POKRYVNOSTEXPANZNI | 0 | 2 | 0 | 31 |
| STA_POKRYVNOSTINVAZNI | 0 | 2 | 0 | 30 |
| STA_POKRYVNOSTVOLNAPUDA | 0 | 1 | 0 | 4 |
| STA_STUPENZACH | 0 | 0 | 0 | 4 |
| STA_VLHPOM | 0 | 0 | 0 | 1 |
| STA_ZAPOJENICELK | 0 | 0 | 1 | 0 |
| STA_ZASTINENICELK | 0 | 0 | 0 | 2 |

### Podle druhu

| DRUH | beze změny | odebráno | přidáno | změněno |
|---|---|---|---|---|
| Aconitum firmum subsp. moravicum | 1 | 1 | 5 | 11 |
| Adenophora liliifolia | 2 | 1 | 4 | 10 |
| Angelica palustris | 3 | 1 | 4 | 9 |
| Artemisia pancicii | 0 | 0 | 14 | 0 |
| Asplenium adulterinum | 1 | 1 | 3 | 9 |
| Campanula bohemica | 1 | 1 | 3 | 8 |
| Campanula gelida | 1 | 1 | 3 | 6 |
| Cerastium alsinifolium | 1 | 2 | 4 | 8 |
| Cirsium brachycephalum | 1 | 1 | 3 | 9 |
| Coleanthus subtilis | 1 | 2 | 4 | 6 |
| Crambe tataria | 0 | 0 | 16 | 0 |
| Cypripedium calceolus | 2 | 1 | 4 | 10 |
| Dianthus arenarius subsp. bohemicus | 1 | 3 | 5 | 12 |
| Dianthus lumnitzeri | 1 | 1 | 3 | 8 |
| Dianthus moravicus | 1 | 1 | 3 | 8 |
| Dracocephalum austriacum | 2 | 1 | 5 | 10 |
| Echium maculatum | 1 | 1 | 3 | 8 |
| Galium sudeticum | 1 | 2 | 4 | 6 |
| Gentianella praecox subsp. bohemica | 1 | 1 | 3 | 9 |
| Gladiolus palustris | 1 | 2 | 3 | 9 |
| Himantoglossum adriaticum | 1 | 1 | 4 | 11 |
| Iris arenaria | 1 | 1 | 3 | 9 |
| Jurinea cyanoides | 1 | 1 | 5 | 13 |
| Klasea lycopifolia | 1 | 2 | 5 | 9 |
| Ligularia sibirica | 1 | 1 | 4 | 13 |
| Liparis loeselii | 2 | 1 | 4 | 12 |
| Luronium natans | 1 | 1 | 3 | 7 |
| Minuartia smejkalii | 1 | 3 | 3 | 10 |
| Pedicularis sudetica | 1 | 1 | 3 | 8 |
| Poa riphaea | 1 | 1 | 4 | 7 |
| Pulsatilla grandis | 1 | 1 | 4 | 8 |
| Pulsatilla patens | 1 | 1 | 4 | 12 |
| Stipa glabrata | 1 | 1 | 3 | 9 |
| Tephroseris longifolia subsp. moravica | 2 | 1 | 4 | 10 |
| Thesium ebracteatum | 1 | 1 | 4 | 8 |
| Trichomanes speciosum | 1 | 2 | 4 | 5 |

