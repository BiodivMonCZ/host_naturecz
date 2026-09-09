# Testovací běh *Triturus cristatus* + *Bombina variegata* — 2026-09-09

Společný běh obou druhů jednou kaskadou `R/02_druhy`, výsledky spojené
**joinem přes názvy indikátorů** (`ID_IND`) k validaci.

*Triturus cristatus* zároveň slouží jako **regresní kontrola** proti běhu
[`../Triturus_cristatus_20260905/`](../Triturus_cristatus_20260905/).

## Jak byl běh proveden

Oba druhy v **jednom** běhu: fáze 1 v `27_n2k_druhy_zapis.R` dělí data přes
`split(DRUH)` a počítá po druzích, takže společný běh dává stejné výsledky
jako dva samostatné — a 142MB export z NDOP se čte jen jednou.

| | |
|---|---|
| vstup | `n2k_load` = 14 852 záznamů (TC 8 322 · BV 6 530) |
| jednotky druh–lokalita | 254 (TC 191 EVL · BV 63 EVL) |
| doba běhu | 8,8 min (fáze 1: 4:58, 2: 0:07, 3: 2:30, 4: 0:07) |
| oříznutý config | přeskočen 1 výraz (`rn2kcz`) — viz `skripty/00_config_test_2sp.R` |

## Verdikty

| úroveň | druh | dobrý | zhoršený | špatný | neznámý | celkem |
|---|---|---|---|---|---|---|
| DP | *Triturus cristatus* | 318 | 32 | 374 | — | 724 |
| DP | *Bombina variegata* | 166 | 1 | 201 | — | 368 |
| EVL | *Triturus cristatus* | 35 | 29 | 39 | 88 | 191 |
| EVL | *Bombina variegata* | 6 | 12 | 12 | 33 | 63 |

**Regrese TC je čistá:** DP 318/32/374 i EVL 35/29/39/88 jsou **shodné**
s během 20260905 na jednotku přesně.

## Co si při validaci ověřit

### 1. `STA_PLOCHA50CM` se nehodnotí u žádného druhu — očekávané (H-04)

Indikátor má plně vyplněný limit (`min` 25 u TC, `min` 75 u BV, `minmax`,
úroveň `lok`), přesto je `STAV_IND = NA` na **všech 1 092** řádcích DP-rok
a `HOD_IND = "neznámý"`.

Není to chyba: tag `<STA_PLOCHA50CM>` se v exportu z NDOP **nevyskytuje ani
jednou** a dle sdělení autorů metodiky (2026-08-20) se indikátor hodnotí
**až od roku 2027** — viz komentář u `R/02_druhy/21_1_n2k_druhy_akce.R:965`.
Do počtu hodnocených indikátorů proto nevstupuje.

### 2. Rozdíl 32 vs. 1 „zhoršený" na úrovni DP je důsledek Tabulky 1

„Zhoršený" vyžaduje **0** špatných klíčových (populačních) indikátorů
a zároveň **≥2** špatné stanovištní. Počty selhání stanovištních indikátorů:

| indikátor | TC nesplněno | BV nesplněno |
|---|---|---|
| `STA_PRUHLEDNOSTVODA` | **246** | — (druh nemá limit) |
| `STA_POKRVEGETACE` | 164 | 109 |
| `STA_RYBY` | 49 | 0 |
| `STA_ZASTINENILITORAL` | — (druh nemá limit) | 14 |
| `STA_MANIPULACE` | 16 | — (druh nemá limit) |
| `STA_VYSYCHANIPERIOD3` | 2 | 1 |
| `STA_UHYNOBOJZIVELNIK` | 0 | 0 |

TC má 6 použitelných stanovištních indikátorů včetně silného
`STA_PRUHLEDNOSTVODA`; BV jich má 5 a jejich selhání jsou vzácná, takže
souběh dvou na téže DP a roce prakticky nenastane. **Nízké číslo je
vysvětlené, ne podezřelé.**

### 3. Indikátory, které má jen jeden z druhů

| indikátor | jen u | pozn. |
|---|---|---|
| `POP_POCETSUM` | *Triturus cristatus* | `info`, bez limitu |
| `STA_MANIPULACE` | *Triturus cristatus* | `val` = „ne" |
| `STA_PRUHLEDNOSTVODA` | *Triturus cristatus* | `min` 50 cm |
| `STA_ZASTINENILITORAL` | *Bombina variegata* | `max` 75 % |

### 4. `STA_PLOCHA50CM` a `STA_POKRVEGETACE` mají u druhů **různý limit**

- `STA_PLOCHA50CM`: TC `min 25` · BV `min 75`
- `STA_POKRVEGETACE`: TC `min 1` / `max 75` · BV `max 50`, a navíc BV
  připouští hodnotu `0` a `0 %` jako **příznivou** — *B. variegata*
  vyhledává tůně v raných sukcesních stádiích (Příloha 1 metodiky).

Obojí je záměrné, ale patří to ke kontrole.

### 5. Otevřený nález H-64 se týká právě *B. variegata*

`POP_POCET` má u BVAR jednotky `samci` · `jedinci` · `adulti`, ačkoli
metodika připouští **dospělce** u všech druhů kromě *B. bombina*. Ovlivňuje
`POP_POCET`, početnost za EVL (`POP_POCETPRUM3`) a škálu vstupující do
`POP_ZMENARAD`. Viz `Metodiky/Obojzivelnici/harmonizace_registr.md`, H-64.

## Obsah

| Co | Kde |
|---|---|
| **srovnání přes `ID_IND`, úroveň DP** | `srovnani/srovnani_indikatoru_DP.csv` |
| **srovnání přes `ID_IND`, úroveň EVL** | `srovnani/srovnani_indikatoru_EVL.csv` |
| definice limitů obou druhů vedle sebe | `srovnani/limity_srovnani.csv` |
| verdikty | `srovnani/verdikty.csv` |
| výpis běhu | `log/run.log` |
| exporty všech tří úrovní | `export/` |
| mezivýsledky (DP, EVL, početnost) | `n2k_druhy_lok.csv.gz`, `n2k_druhy_chu.csv.gz`, `n2k_druhy_pocetnost.csv.gz` |
| skripty běhu | `skripty/` |

Join je záměrně **full** (`dplyr::full_join`) přes `ID_IND` — indikátor, který
má jen jeden z druhů, musí být vidět; právě to je častá chyba v tabulce limitů.
