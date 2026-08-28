# Testovací běh hodnocení — *Triturus cristatus*, 2026-08-28

Kompletní běh kaskády `R/02_druhy` pro jediný druh, všechny úrovně hodnocení,
a srovnání s posledním zakomitovaným během na větvi.

## Co se testovalo

| | |
|---|---|
| **Kód** | větev `202608-obojzivelnici`, HEAD `2a7e036` |
| **Druh** | *Triturus cristatus* |
| **Vstup** | `host_data/export_data_evl.csv` + `export_data_zprap.csv` (NDOP) |
| **Rozsah** | 8 322 záznamů → 724 dílčích ploch ve 191 EVL |
| **Doba běhu** | 7,5 min (fáze 1: 4:12, fáze 2: 0:03, fáze 3: 1:04, fáze 4: 0:05) |
| **Srovnáváno s** | commit `9a108a2` (2026-08-25), exporty `nal_20260820`, `lok_20260821`, `chu_20260821` |

Mezi porovnávanými commity se změnily **pouze** `24_n2k_druhy_lokality.R`
a `25_n2k_druhy_uzemi.R` (nálezy H-21 a H-22). Úroveň nálezu je proto
kontrolní vzorek — musí vyjít shodně.

## Provedené úrovně

| Skript | Úroveň | Výstup |
|---|---|---|
| `21_1` | nález / akce — extrakce indikátorů ze `STRUKT_POZN` | `mezivysledky/n2k_druhy.csv.gz` |
| `21_1` fáze 1b | řada početností pro Tabulku 2 | `mezivysledky/n2k_druhy_pocetnost.csv` |
| `21_2` | nález — porovnání s limity → `STAV_IND` | `mezivysledky/n2k_druhy_lim.csv.gz` |
| `22` | hodnocená období (lok / pole / chu) | `mezivysledky/n2k_druhy_obdobi_*.csv` |
| `23` | poslední nález (lok / pole / chu) | `mezivysledky/n2k_druhy_posledni_*.csv` |
| `24` | dílčí plocha — Tabulka 1 | `mezivysledky/n2k_druhy_lok.csv.gz` |
| `25` | území / EVL — Tabulka 2 | `mezivysledky/n2k_druhy_chu.csv` |
| `27` | exporty pro ISOP | `export/*.csv.gz` |

## Výsledek srovnání

### Úroveň nálezu — beze změny (kontrola)

81 891 řádků, **všechny shodné**. Setříděný export je s původním během
bajt po bajtu identický. Potvrzuje to, že ořezaný config reprodukuje
vstup původního běhu věrně.

### Úroveň dílčí plochy — 43 z 724 změněno

| stav | původní | nový |
|---|---|---|
| dobrý | 377 | **334** |
| zhoršený | **0** | **16** |
| špatný | 347 | **374** |

Všech 43 změn je na řádku `CELKOVE_HODNOCENI`; **žádný jednotlivý indikátor
se nezměnil**. Přechody: 27× dobrý → špatný, 16× dobrý → zhoršený.
Odpovídá nálezu H-21 (nevyhodnocený indikátor se počítal jako splněný).

Doložený příklad — DP `CZ0213790 / 2598`, dobrý → špatný:

| indikátor | hodnota | `STAV_IND` |
|---|---|---|
| `POP_PRESENCE` (klíčový) | ano | 1 |
| `POP_REPROPERIOD3` (klíčový) | 0 | **0** |
| `POP_ZMENARAD` (klíčový) | neznámý | NA |
| ostatní stanovištní | neznámý | NA |

Klíčový indikátor `POP_REPROPERIOD3` selhal, přesto původní běh vykázal
„dobrý": `NA` u `POP_ZMENARAD` se přes `n_distinct()` započítalo jako další
splněný indikátor, takže `N_KEY_PASSED` (2) nebylo menší než
`N_KEY_EXPECTED` (2). Nově `N_KEY_PASSED = 1 < 2` → „špatný", jak Tabulka 1
vyžaduje.

### Úroveň EVL — 7 z 63 změněno

| stav | původní | nový |
|---|---|---|
| dobrý | 6 | 5 |
| zhoršený | 26 | 21 |
| špatný | 31 | 37 |

Změněná území: `CZ0214006`, `CZ0323147`, `CZ0323149`, `CZ0424125`,
`CZ0813469`, `CZ0813472` (zhoršený → špatný) a `CZ0314639` (dobrý → zhoršený).

Export navíc obsahuje **63 nových řádků `POP_POCETPRUM3`** (252 → 315 řádků,
4 → 5 indikátorů), které původní běh ztrácel — nález H-22.

## Vzorek (10 EVL, 50 DP)

Losováno se `set.seed(20260828)`; seznamy v `srovnani/vzorek_evl.csv`
a `srovnani/vzorek_dp.csv`. EVL se losují z území, která mají hodnocení
na úrovni EVL (63), ne ze všech 191.

Ve vzorku **nevyšla žádná změna celkového hodnocení** — 50/50 DP i 10/10 EVL
shodně, všech 4 569 porovnaných řádků nálezů shodně. Při 43 změnách ze 724 DP
je pravděpodobnost takového losu ≈ 5 %, jde tedy o shodu náhody, ne o rozpor.
Jediné rozdíly ve vzorku: 10 nových řádků `POP_POCETPRUM3` a 4 změny hodnot
`LOK_DILCDOBRE` / `LOK_PROCDOBR`.

Úplné seznamy změn mimo vzorek jsou v `srovnani/zmenene_DP_vse.csv`
a `srovnani/zmenene_EVL_vse.csv`.

## Omezení běhu

Použit **ořezaný konfigurační skript** (`skripty/00_config_test_tc.R`), který
`R/00_config/00_n2k_config.R` vyhodnocuje výraz po výrazu doslovně a přeskakuje
11 výrazů nejvyšší úrovně, jež na této stanici nelze spustit:

- `vodstvo` — WFS ČÚZK (síť)
- `biotop_zvld` / `BiotopZvld.shp` — soubor chybí
- `n2k_union` — drahý prostorový join, nepoužívá se
- `akt_okrsky` / `AktualizacniOkrsky.shp` — soubor chybí (viz registr, „Co zbývá" č. 6)
- `redlist_species`, `invasive_species`, `expansive_species` — exporty chybí v `host_data`
- `rn2kcz` z GitHubu — balíček se v `R/02_druhy` nepoužívá

**Žádný z těchto objektů se v `R/02_druhy` nepoužívá**, běh druhové části
je tedy úplný. Omezení na jeden druh se aplikuje po zjištění `ncol_orig`;
všechny transformace v bloku `n2k_load` jsou řádkové, předfiltrování je proto
vůči plnému běhu ekvivalentní.

## Poznámka k datům

Při běhu `24` se hlásí varování:

```
Druh Triturus cristatus: Indikatory s nekonzistentnimi metadaty:
STA_POKRVEGETACE, STA_PRUHLEDNOSTVODA
```

Oba indikátory mají v `limity_vse.csv` pro tentýž `DRUH × ID_IND` současně
řádky `val` i `min`/`max`. Varování je přítomno v obou porovnávaných bězích,
nejde o regresi — ale stojí za prověření, protože `run_n2k_druhy_lok()`
u takového indikátoru bere `TYP_IND`, `KLIC` a `UROVEN` z prvního řádku
(`dplyr::first()`).

## Reprodukce

```r
# v pracovním stromu na větvi 202608-obojzivelnici
source("Test/01_run_test_tc.R")     # kaskáda 21 -> 27
source("Test/02_compare_test_tc.R") # srovnání s původním během
```

Cache konfigurace (`Test/.cache_config_tc.rds`) se vytvoří při prvním běhu,
aby se 142MB export NDOP nemusel číst znovu; smazáním se vynutí nové načtení.
