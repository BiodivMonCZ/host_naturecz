# Integrace a harmonizace kódu druhů — 2026-11-09

Větev `druhy-integrace-202611` (z `main` @ `ea0e450`). Cíl: spojit veškerou práci na druzích
do jedné větve, kód projít a sjednotit — kratší, kde to jde, srozumitelnější všude —
a přitom zachovat **přesně tytéž výsledky hodnocení**, kromě změn podložených
zaznamenaným rozhodnutím. Priorita: **správnost > srozumitelnost > stručnost**.

> Stav: **plán (Fáze 0)** — tento soubor se po dokončení stane závěrečnou zprávou.

## Ověřený výchozí stav (2026-11-08)

| Větev | Tip | Vztah k `main` (`ea0e450`) | Naložení |
|---|---|---|---|
| `ryby-metodika-2026` | `fedd237` | +41 / −26, odbočka `f9540f9`; lineárně obsahuje `cevnate-metodika-2026` (`35a3d99`), `harmonizace-cevnate` (`83f3727`, jen lokální) a `druhy-202609` (`a172ba1`) | `merge --no-ff` (hashe citované v registrech zůstanou) |
| `ryby-indikatory` | `470c439` | +1 / −241 | nesloučit; přenést jen chybějící tagy použité limitem nebo exportem |
| `eriogaster-catax-2025-01` | `84a7106` | +4 / −241 | nesloučit; přenést jen řádky *Eriogaster catax* v `limity_vse.csv` |
| `202608-obojzivelnici`, `stanoviste-hotfix-20251016-2` | — | už v `main` | — |
| `stanoviste-revize-2025Q1-3` | — | stanoviště | mimo rozsah |

`main` od odbočky změnil jen `load_vmb.R` a skripty stanovišť — žádný soubor, který mění větev,
a `load_vmb()` větev druhů nepoužívá (sémantický konflikt nehrozí).

**Pozor:** `.gitattributes` v repozitáři **neexistuje** (zadání ho zmiňuje). Konce řádků řídí
`core.autocrlf = true` a stav indexu (`git ls-files --eol`): vstupní CSV jsou v indexu zčásti CRLF,
zčásti LF; `24_n2k_druhy_lokality.R` je git veden jako `-text`. Každý commit se kontroluje
na celosouborové přepisy.

## Plán

| Fáze | Obsah | Ověření |
|---|---|---|
| 0 | inventura, tento plán | — |
| 1 | **základní běh** kaskády `21_1` → `27` na `fedd237` pro **všechny druhy** (82) — ostrý export NDOP 2026-05-10 v `../host_data/`; izolovaná kopie (`git worktree`) mimo pracovní strom | uložené `Data/Temp/*.csv` a exporty `Outputs/Data/druhy/*` |
| 2 | merge `ryby-metodika-2026`; transplantace řádků *E. catax*; tagy ryb (pokud něco chybí) — každé zvlášť | plný běh, srovnání s Fází 1 |
| 3 | revize všech skriptů druhů, configů a `R/05_metodiky` proti metodikám a Z-rozhodnutím; nálezy H-96+ | — |
| 4 | sjednocení a zkrácení bez změny výsledků; po **každém** commitu plný běh a shoda | plný běh, srovnání |
| 5 | úklid `Outputs/Test` — jen návrh + mazání neodkazovaného a regenerovatelného | seznam smazaných cest |
| 6 | nezávislá kontrola, push, draft PR | — |

Srovnání běhů: [`skripty/porovnej_behy.R`](skripty/porovnej_behy.R) porovnává každý výstupní soubor
**řádek po řádku jako text** (pořadí řádků i sloupců), maskuje se jen datum běhu
(`datum_hodnoceni` v exportu úrovně EVL).
