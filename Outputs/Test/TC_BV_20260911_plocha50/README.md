# Regresní běh — ztišení `STA_PLOCHA50CM` (2026-09-11)

Doklad k úpravě v [21_2_n2k_druhy_akce_lim.R](../../../R/02_druhy/21_2_n2k_druhy_akce_lim.R):
indikátory, které se začnou sbírat teprve v budoucnu, se za starší roky
nevykazují vůbec.

Srovnáváno proti běhu [`../TC_BV_20260909/`](../TC_BV_20260909/) — stejná data,
stejné druhy, jediná změna je tento filtr.

## Co se změnilo

Nová tabulka `IND_OD_ROKU <- c(STA_PLOCHA50CM = 2027)` a řádkový filtr za
napojením limitů. Filtr je **ročníkový, ne globální**: od roku 2027 se
indikátor začne vykazovat sám, bez dalšího zásahu do kódu.

Důvod: `STA_PLOCHA50CM` má v `limity_vse.csv` vyplněný limit (`min` 25 u TC,
`min` 75 u BV), ale tag `<STA_PLOCHA50CM>` se v exportu z NDOP nevyskytuje ani
jednou — dle sdělení autorů metodiky (2026-08-20) se indikátor hodnotí až od
roku 2027 (nález **H-04**). Do výstupu tak šel jediný možný řádek:
`HOD_IND = "neznámý"`, `STAV_IND = NA`, tedy 1 092 řádků bez informační
hodnoty, které navíc putovaly i do importu do ISOP.

## Výsledek: verdikty beze změny

| | |
|---|---|
| `verdikty.csv` | **bajtově shodné** s během 20260909 |
| `srovnani_indikatoru_EVL.csv` | **bajtově shodné** |
| `srovnani_indikatoru_DP.csv` | **jediný rozdíl** — ubyl řádek `STA_PLOCHA50CM` |
| `STA_PLOCHA50CM` v `n2k_druhy_lok.csv` | 724 + 368 řádků → **0** |
| `STA_PLOCHA50CM` v `n2k_druhy_lim.csv` | → **0** |

Verdikty na obou úrovních zůstaly na jednotku stejné:

| úroveň | druh | dobrý | zhoršený | špatný | neznámý |
|---|---|---|---|---|---|
| DP | *Triturus cristatus* | 318 | 32 | 374 | — |
| DP | *Bombina variegata* | 166 | 1 | 201 | — |
| EVL | *Triturus cristatus* | 35 | 29 | 39 | 88 |
| EVL | *Bombina variegata* | 6 | 12 | 12 | 33 |

**Proč to tak musí být:** [24_n2k_druhy_lokality.R:151](../../../R/02_druhy/24_n2k_druhy_lokality.R#L151)
počítá `N_KEY_EXPECTED` i `N_OTH_EXPECTED` výhradně z řádků, kde
`!is.na(STAV_IND)` (nález H-21). Nevyhodnocený indikátor tedy do počtu
očekávaných ani splněných indikátorů nevstupoval už před touto úpravou —
filtr jen **přestal vypisovat** to, co se stejně nehodnotilo.

## Obsah

| Co | Kde |
|---|---|
| výpis běhu | `log/run.log` |
| srovnání přes `ID_IND` | `srovnani/` |
| skripty běhu | `skripty/` |

Fáze: 1/4 Akce 04:24 · 2/4 Limity 00:05 · 3/4 Lokality 01:34 · 4/4 Území 00:05.
(Údaj „CELKOVY CAS" v logu je chybný — testovací harness si ho přebral
z cache předchozího běhu; opraveno po tomto běhu.)
