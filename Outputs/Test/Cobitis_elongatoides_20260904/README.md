# Testovací běh kaskády — *Cobitis elongatoides*, 2026-09-04

Doklad k nálezu **H-39** (oprava názvu druhu) v
[harmonizace_registr.md](../../../Metodiky/Obojzivelnici/harmonizace_registr.md).
Kaskáda `21_1` → `27` proběhla dvakrát — s překlepem `Cobitis elangotoides`
v `limity_ryby.csv` a po jeho opravě.

| | |
|---|---|
| druh | *Cobitis elongatoides* (sekavec podunajský) |
| rozsah | 211 záznamů NDOP, 8 lokalit v seznamu předmětů ochrany, 19 DP |
| doba běhu | 0,3 min (oba běhy) |
| jediná změna mezi běhy | 10 řádků `limity_ryby.csv`, `Cobitis elangotoides` → `Cobitis elongatoides` |

## Výsledek

| Co | Před | Po |
|---|---|---|
| indikátorů ve výstupu DP | 4 | **6** |
| `POP_DYN` (`max 50`, `KLIC = ano`) | chybí | **19 řádků** |
| `POP_VITALITA` (`min 2`, `KLIC = ano`) | chybí | **19 řádků** |
| `CELKOVE_HODNOCENI` | 19× dobrý | **17× dobrý · 2× špatný** |
| změněných DP | — | **2 z 19**, obě k horšímu |

Změna **není neutrální a být nemá** — obě DP se dosud vykazovaly jako v dobrém
stavu jen proto, že se jejich klíčové indikátory kvůli názvu vůbec nepočítaly.
Bez nich bylo `N_KEY_EXPECTED = 0` a verdikt „špatný" byl nedosažitelný.

## Úroveň EVL zde chybí — a není to chybou běhu

Pro tento druh **nevzniká žádný výstup na úrovni EVL**: nemá v limitech jediný
řádek s `UROVEN = "chu"`, takže jej `25_n2k_druhy_uzemi.R` neprodukuje. Týká se
to 16 ze 17 druhů ryb — viz nález **H-41**.

**Pozor při srovnávání běhů:** `Data/Temp/n2k_druhy_chu.csv` se v takovém
případě nepřepíše a zůstane v něm výstup **předchozího běhu jiného druhu**.
Při prvním srovnání to vypadalo jako „úroveň EVL beze změny, 191 EVL", ve
skutečnosti šlo o zbytek po běhu *Triturus cristatus*. Vždy je nutné
zkontrolovat čas změny souboru.

## Obsah

| Složka | Co |
|---|---|
| `log/` | `run_pred.log`, `run_po.log` — celé výpisy obou běhů |
| `export/` | exporty úrovní `nal` a `lok` po opravě, UTF-8 i Windows-1250 |

Exporty jsou z běhu nad jediným druhem — nemají se použít jako podklad pro
import do ISOP. Skripty testovacího běhu jsou ve složce
[`../Triturus_cristatus_20260903/skripty/`](../Triturus_cristatus_20260903/skripty/);
liší se jen hodnotou `TEST_SPECIES`.
