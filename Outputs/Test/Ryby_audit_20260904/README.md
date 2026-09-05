# Řešení auditu ryb — 2026-09-04

Doklad k nálezům **H-47 … H-58** v
[harmonizace_registr.md](../../../Metodiky/Obojzivelnici/harmonizace_registr.md).
Kaskáda `21_1` → `27` nad třemi druhy, srovnáno proti stavu po H-42
(běh `Ryby_20260904`).

## Výsledek

| Běh | DP | Změněných verdiktů | K lepšímu | K horšímu |
|---|---|---|---|---|
| *Triturus cristatus* — **regrese** | 724 | **0** | — | — |
| *Cottus gobio* | 370 | **80** | 55 špatný → dobrý | 16 dobrý → špatný · 9 špatný → zhoršený |
| *Lampetra planeri* | 253 | **41** | 22 špatný → dobrý | 7 dobrý → špatný · 12 špatný → zhoršený |

## Čím jsou změny způsobeny (*Cottus gobio*)

| Indikátor | Změna `STAV_IND` | DP | Nález |
|---|---|---|---|
| `POP_DYN` | 0 → 1 | **177** | H-47 — opravená nepravdivá selhání |
| `POP_DYN` | `NA` → 1 / 0 | 44 / 11 | H-48 — získaná abundance u bodové metody |
| `POP_DYN` | 1 → 0 | 27 | populace skutečně klesla pod polovinu reference |
| `POP_PRESENCE` | `NA` → 1 / 0 | 292 / 78 | H-56 + H-58 — indikátor poprvé funguje |
| `STA_MIGBARVYS` | 1 → 0 | 3 | H-52 — nalezena vyšší bariéra než první v pořadí |

Převaha změn k lepšímu jsou **opravy nepravdivých selhání klíčového
indikátoru**, ne změkčení hodnocení. Změny k horšímu jsou převážně
nepřítomnost druhu, kterou `POP_PRESENCE` nově skutečně zachytí.

## ⚠ H-47 je prozatímní

Otočení limitu `POP_DYN` na `min 50` je **pracovní předpoklad zadavatele**,
ne rozhodnutí autorů metodiky ryb. Stojí na něm 177 z 370 změněných hodnocení
u *Cottus gobio*. Zpětný krok je změna sedmi řádků v `limity_ryby.csv`.

**Do rozhodnutí se výstupy ryb nemají použít pro import do ISOP.**

## Obsah

| Co | Kde |
|---|---|
| výpisy tří běhů | `log/` |
| exporty úrovní `nal`, `lok`, `chu` (*Lampetra planeri*, poslední běh) | `export/` |
| 11 jednotkových testů `max_cislo()` (H-52) | `test_max_cislo.R` |

Testy se spouští z kořene repozitáře:

```sh
Rscript Outputs/Test/Ryby_audit_20260904/test_max_cislo.R
```

Skripty testovacího běhu jsou v
[`../Triturus_cristatus_20260903/skripty/`](../Triturus_cristatus_20260903/skripty/);
`TEST_SPECIES` se čte z proměnné prostředí.
