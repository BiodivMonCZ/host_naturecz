# Testovací běh — oprava `CIS_CILMON` u obojživelníků — 2026-11-05

Doklad k rozhodnutí **Z-14** revize ryb („změřit opravu `CIS_CILMON` na *T. cristatus*
+ *B. bombina* před aplikací"), nález **H-89** v
[../../../Metodiky/Ryby a mihule/harmonizace_registr.md](../../../Metodiky/Ryby%20a%20mihule/harmonizace_registr.md)
(u cévnatých jde o H-14). Kaskáda `21_1` → `27` (`Test/01_run_test_2sp.R`) pro *Triturus
cristatus* a *Bombina bombina*, export NDOP 2026-05-10, hodnocený rok 2025.

| Běh | Kód | Rozdíl | Doba |
|---|---|---|---|
| **pred** | `b7abb07` (`R/` = `35a3d99`) | — | 17,7 min |
| **cilmon** | totéž + jediný řádek `ZDROJ %in% CIS_CILMON$ZDROJ_NDOP` v `02_n2k_data_druhy.R` | jen oprava H-89 | 18,2 min |
| **aplikovano** | `06a849d` (patch ryb + H-90 + Z-16) | navíc párový filtr H-85 a pravidlo H-90 | běh [`../Ryby_20261105/aplikovano/`](../Ryby_20261105/aplikovano/) (5 druhů) |

Běhy proběhly v **izolovaných kopiích repozitáře**; `Data/Temp/` ani `Outputs/Data/`
repozitáře nebyly dotčeny. Seznam `cil_mon_zdroj.csv` zůstal beze změny — oprava jen
způsobí, že se poprvé uplatní; u obojživelníků přepne 2 106 záznamů zdroje
„Kolektiv autorů (2021) Sledování stavu EVL - obojživelníci." (*B. bombina* 1 245,
*T. cristatus* 687).

## Samotná oprava `CIS_CILMON` (pred → cilmon)

| | *B. bombina* | *T. cristatus* |
|---|---|---|
| DP s cíleným monitoringem (`CILMON = 1`) | 629 → **795** | 428 → **584** |
| verdikt z cíleného monitoringu d / z / š (`CELKOVE_CILMON`) | 273 / 86 / 274 → 285 / 88 / 426 | 153 / 31 / 246 → 183 / 31 / 372 |
| verdikt DP ze všech záznamů d / z / š | 473 / 88 / 433 → 472 / 88 / 434 | beze změny 318 / 32 / 374 |
| EVL d / z / š | 11 / 45 / 37 → **9 / 46 / 38** | beze změny 2 / 22 / 39 |
| medián `LOK_PROCDOBR` | 40,0 → 40,0 % | 33,3 → **25,0 %** |

EVL *B. bombina*: 2× dobrý → zhoršený, 1× zhoršený → špatný. Nově započtené DP jsou převážně
špatné (+12 / +2 / +152 u *B. bombina*, +30 / 0 / +126 u *T. cristatus*), takže podíl dobrých
DP v EVL klesá. Úroveň DP ze všech záznamů se téměř nemění (jedna DP).

## Konečný stav větve (cilmon → aplikovano)

Navíc párový filtr druh × EVL (H-85) a pravidlo „neznámý klíčový indikátor ⇒ neznámý celkový
stav" (H-90, rozhodnutí Z-12, napříč skupinami).

| | *B. bombina* | *T. cristatus* |
|---|---|---|
| DP | 994 → 783 (párový filtr) | 724 → 552 |
| verdikt DP d / z / š / neznámý | 472 / 88 / 434 / 0 → **136 / 49 / 224 / 374** | 318 / 32 / 374 / 0 → **54 / 18 / 176 / 304** |
| verdikt z cíleného monitoringu d / z / š / ? | 285 / 88 / 426 / 0 → 136 / 48 / 226 / 315 | 183 / 31 / 372 / 0 → 54 / 17 / 178 / 275 |
| EVL d / ? / z / š | 9 / 0 / 46 / 38 → **8 / 9 / 39 / 37** | 2 / 0 / 22 / 39 → **1 / 7 / 19 / 36** |
| neznámý klíčový indikátor u DP „neznámý" | `POP_ZMENARAD` 374 | `POP_ZMENARAD` 301, `POP_REPROPERIOD3` 28 |
| z DP „neznámý" s **nesplněným** klíčovým indikátorem | 178 | 180 |

Posledních 178 + 180 DP by při přednosti „špatný" před „neznámý" vyšlo špatně. Metodika
obojživelníků (kap. 2.1) přitom říká „Indikátor se hodnotí pouze, jsou-li dostupné informace
k jeho hodnocení" — pravidlo H-90 je s tímto zněním v rozporu (nález H-91 registru ryb).

## Obsah

| Co | Kde |
|---|---|
| exporty DP, EVL a nálezů běhů pred a cilmon | `pred/export/`, `cilmon/export/` (běh aplikovano: `../Ryby_20261105/aplikovano/export/`) |
| logy | `log/` |
| srovnání pred × cilmon | `srovnani_pred_cilmon/` |
| srovnání pred × cilmon × aplikovano | `srovnani_aplikovano/` |
| config (`TEST_SPECIES`) a skript srovnání | `skripty/` |
