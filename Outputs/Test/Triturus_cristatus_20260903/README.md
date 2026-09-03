# Testovací běh kaskády — *Triturus cristatus*, 2026-09-03

Podklad pro nález **H-37** (`POP_REPRO` → `info`) v
[harmonizace_registr.md](../../../Metodiky/Obojzivelnici/harmonizace_registr.md).
Kaskáda `21_1` → `27` proběhla nad jediným druhem **dvakrát** — před zásahem
do `limity_vse.csv` a po něm — aby se doložilo, že změna nemění žádný verdikt.

| | |
|---|---|
| druh | *Triturus cristatus* |
| rozsah | 724 dílčích ploch ve 191 EVL |
| doba běhu | 10 min (před) · 9 min (po) |
| jediná změna mezi běhy | 44 řádků `POP_REPRO` v `limity_vse.csv`, `TYP_IND` `val` → `info` |

## Výsledek

| Co | Před | Po |
|---|---|---|
| indikátorů ve výstupu DP | 18 | **19** |
| `POP_REPRO` | chybí | **724 řádků** (`ano` 189 · `ne` 399 · `neznámý` 136), `STAV_IND` vždy `NA` |
| `CELKOVE_HODNOCENI` (DP) | 336 / 14 / 374 | 336 / 14 / 374 |
| změněných verdiktů DP | — | **0** ze 724 |
| změněných `CELKOVE_SUM` | — | **0** |
| úroveň EVL | 38 / 29 / 36 / 88 | beze změny |

Podrobný výpis je v [srovnani/srovnani_pop_repro.txt](srovnani/srovnani_pop_repro.txt).

## Obsah

| Složka | Co |
|---|---|
| `skripty/` | ořezaný config, spouštěč kaskády, srovnávací skript |
| `log/` | `run_pred.log`, `run_po.log` — celé výpisy obou běhů |
| `export/` | exporty všech tří úrovní po zásahu (`nal`, `lok`, `chu`), UTF-8 i Windows-1250 |
| `srovnani/` | strojové srovnání obou běhů |

**Exporty jsou z běhu nad jediným druhem**, ne z plné kaskády — nemají se
použít jako podklad pro import do ISOP. Proto leží zde a ne v
`Outputs/Data/druhy/`.

## Poznámka k `skripty/00_config_test_tc.R`

Oproti verzi archivované u běhu `Triturus_cristatus_20260828` prochází
**oba** konfigurační soubory:

```r
cfg_paths <- c(
  "R/00_config/00_n2k_config.R",
  "R/00_config/02_n2k_data_druhy.R"
)
```

Původní verze četla jen `00_n2k_config.R` a od rozdělení configu padala na
`object 'n2k_load' not found` — `n2k_load`, `n2k_export` i `ncol_orig`
vznikají až v druhém souboru. Ostatní logika (přeskakování bloků podle
tokenů, omezení na jeden druh až po zjištění `ncol_orig`) je beze změny.

## Jak běh zopakovat

Skripty se spouští z kořene repozitáře a očekávají se ve složce `Test/`:

```sh
mkdir -p Test && cp Outputs/Test/Triturus_cristatus_20260903/skripty/*.R Test/
Rscript Test/01_run_test_tc.R
```

První běh si přečte export z NDOP (142 MB) a uloží `Test/.cache_config_tc.rds`.
**Cache obsahuje i tabulku `limity`** — po každé změně souborů limitů je nutné
ji smazat, jinak by další běh počítal se starými limity.
