# Testovací běh — ryby a mihule, metodika EVL 2026 — 2026-11-05

Doklad k Fázi 3 revize ([harmonizace_registr.md](../../../Metodiky/Ryby%20a%20mihule/harmonizace_registr.md),
[revize_metodika_ryby.md](../../../Metodiky/Ryby%20a%20mihule/revize_metodika_ryby.md)).
Kaskáda `21_1` → `27` (`Test/01_run_test_2sp.R`) pro *Cottus gobio*, *Lampetra planeri*
a *Misgurnus fossilis*, export NDOP 2026-05-10, hodnocený rok 2025.

| Běh | Kód | Limity a délkové skupiny | Doba |
|---|---|---|---|
| **pred** | větev `ryby-metodika-2026` @ `2f40b14` (kód `R/` = `35a3d99`) | živé `limity_ryby.csv`, `cis_ryby_delky_strukt.csv` | 9,4 min |
| **po** | totéž @ `24cfa29` + [patche/ryby_metodika_2026.patch](../../../Metodiky/Ryby%20a%20mihule/patche/ryby_metodika_2026.patch) | `limity_ryby_2026.csv`, `cis_ryby_delky_strukt_2026.csv` | 11,4 min |

Oba běhy proběhly v **izolovaných kopiích repozitáře** mimo pracovní strom; `Data/Temp/`
ani `Outputs/Data/` repozitáře nebyly dotčeny. Skript `27` zapisuje do `Outputs/Data/druhy/`
kopie — sem jsou převzaty jen exporty, logy a srovnání.

> ⚠ Běh „po" obsahuje vadu H-90 (lokalita bez hodnoceného klíčového parametru vychází jako
> „dobrá"). **Konečný stav větve po rozhodnutích Kontrolního bodu 2 je běh
> [„aplikovano"](#konečný-stav-větve--běh-aplikovano) na konci tohoto souboru.**

## Úroveň DP

| Druh | Běh | DP | dobrý | zhoršený | špatný | DP s cíleným monitoringem hodnocené |
|---|---|---|---|---|---|---|
| *Cottus gobio* | pred | 370 | 158 | 12 | 200 | 300 |
| | po | 874 | 659 | 42 | 173 | 494 |
| *Lampetra planeri* | pred | 253 | 97 | 19 | 137 | 200 |
| | po | 563 | 376 | 95 | 92 | 367 |
| *Misgurnus fossilis* | pred | 113 | 33 | 0 | 80 | 73 |
| | po | 390 | 276 | 79 | 35 | 179 |

DP je v běhu „po" víc, protože klíčem je `LOKALITA` (H-71): sloučené úseky se rozdělily
a každý náhodný nález s vlastním názvem lokality je samostatná DP. DP z EVL, kde druh není
předmětem ochrany, v běhu „po" nejsou (H-85; „pred" jich má 115 / 63 / 34).

### Proč tolik „dobrý" — vada H-90

| Druh | dobrý celkem | z toho **bez jediného hodnoceného klíčového parametru** | bez jakéhokoli hodnoceného indikátoru |
|---|---|---|---|
| *Cottus gobio* | 659 | **616** | 422 |
| *Lampetra planeri* | 376 | **371** | 327 |
| *Misgurnus fossilis* | 276 | **276** | 215 |

`POP_PRESENCE` je nově `info` (Z-4), `POP_DYN` potřebuje ≥ 2 roky (*Lampetra* ≥ 3)
a `POP_VITALITA3` ≥ 2 průzkumy — u lokality s jediným rokem dat tedy klíčový parametr
není hodnocen a `24` vrátí „dobrý" (nic nesplněno není). Varianty na DP s cíleným monitoringem
(`srovnani/po_dp_varianty_klicovych.csv`):

| Druh | varianta | dobrý | zhoršený | špatný | neznámý |
|---|---|---|---|---|---|
| *Cottus* | A — dnes (patch) | 279 | 42 | 173 | 0 |
| | B — žádný hodnocený klíčový ⇒ neznámý | 43 | 3 | 173 | 275 |
| | C — dobrý/zhoršený jen při obou hodnocených (kap. 5.1) | 42 | 3 | 173 | 276 |
| *Lampetra* | A | 180 | 95 | 92 | 0 |
| | B | 5 | 1 | 92 | 269 |
| | C | 5 | 1 | 92 | 269 |
| *Misgurnus* | A | 66 | 79 | 34 | 0 |
| | B | 0 | 2 | 34 | 143 |
| | C | 0 | 0 | 34 | 145 |

### Indikátory DP (splněno / nesplněno / nehodnoceno)

| Druh | Indikátor | pred | po |
|---|---|---|---|
| *Cottus* | `POP_DYN` | 230 / 41 / 99 | 206 / 10 / 658 |
| | `POP_VITALITA` → `POP_VITALITA3` | 103 / 182 / 85 | 46 / 172 / 656 |
| | `POP_PRESENCE` | 292 / 78 / 0 | `info` |
| | `STA_DNOTYPSOUCETPROCENT` (K + B + Š) | sirotek | 384 / 68 / 422 |
| *Lampetra* | `POP_DYN` | 123 / 33 / 97 | 36 / 10 / 517 |
| | `POP_VITALITA` → `POP_VITALITA3` | 45 / 129 / 79 | 6 / 92 / 465 |
| | `STA_DNOTYP` | 4 / 169 / 80 | vypuštěn |
| | náplavy ≥ 10 m²/100 m | sirotek | 75 / 132 / 356 |
| | trdliště ≥ 10 m²/100 m | sirotek | 163 / 43 / 357 |
| *Misgurnus* | `POP_DYN` | 35 / 1 / 77 | 15 / 0 / 375 |
| | `POP_VITALITA` → `POP_VITALITA3` | 0 / 80 / 33 | 0 / 35 / 355 |
| | bahno + písek ≥ 50 % | (`STA_DNO` 69 / 1) | 123 / 49 / 218 |
| | submerzní vegetace ≥ 11 % | (`STA_VEGETACE` 50 / 19) | 97 / 65 / 228 |

Informativní řádky „po": zdroj počtu (`POP_POCETODLOVZDROJ`) — *Cottus* 270× text `POZNAMKA`,
376× `POCET`, 177× negativní záznam; *Lampetra* 159 / 188 / 117; *Misgurnus* 41 / 138 / 205.
Podmínky odlovu (`STA_PODMINKYODLOVU`, všechny DP) — mimo sezónu *Cottus* 151, *Lampetra* 146,
*Misgurnus* 204 (s kombinacemi); zákal *Misgurnus* 33.

## Úroveň EVL (dvojice předmětů ochrany)

| Druh | pred → po | EVL |
|---|---|---|
| *Cottus gobio* | (bez výstupu) → dobrý | 8 |
| | (bez výstupu) → špatný | 22 |
| *Lampetra planeri* | dobrý → dobrý | 0 |
| | dobrý → zhoršený | 2 |
| | dobrý → špatný | 8 |
| | dobrý → neznámý | 1 (CZ0213050 — dřív „dobrý" bez hodnocené lokality, H-82) |
| | zhoršený → dobrý | 2 |
| | zhoršený → zhoršený | 2 |
| | zhoršený → špatný | 13 |
| *Misgurnus fossilis* | (bez výstupu) → špatný | 9 |

Stav „špatný" na úrovni EVL v běhu „pred" nevznikl ani jednou (H-81). S opravou H-90
(varianta B/C) by agregované lokality vyšly dobré jen ve **2** EVL *Cottus* (místo 8) a **1**
EVL *Lampetra* (místo 6).

## Migrační prostupnost — souhrnná statistika (rozhodnutí Z-5: prázdný tag = neznámo)

**Lokality** (reprezentativní návštěva, cílený monitoring):

| Druh | lokalit | výška bariéry známa | nad limitem | do limitu | neznámo |
|---|---|---|---|---|---|
| *Cottus gobio* (limit 10 cm) | 494 | 106 (21 %) | 95 | 11 | 388 |
| *Lampetra planeri* (5 cm) | 367 | 100 (27 %) | 97 | 3 | 267 |
| *Misgurnus fossilis* (10 cm) | 179 | 10 (6 %) | 10 | 0 | 169 |

**EVL** (`STA_MIGPROSTUP`):

| Druh | EVL | dobrá | špatná | neznámá |
|---|---|---|---|---|
| *Cottus gobio* | 30 | **0** | 17 | 13 |
| *Lampetra planeri* | 28 | **0** | 19 | 9 |
| *Misgurnus fossilis* | 9 | **0** | 4 | 5 |

Prostupnost **nemůže vyjít dobrá v žádné EVL**: formulář zapisuje `<pocet_bar>` jen při
nalezené bariéře, takže „bez bariéry" je od „nezjišťováno" nerozlišitelné, a v každé EVL je
alespoň jedna lokalita bez údaje. Dokud formulář nezapisuje `0` (datová mezera D-5), je
prostupnost buď špatná, nebo neznámá; neznámá do verdiktu EVL nevstupuje.

## Konečný stav větve — běh „aplikovano"

Kód `06a849d` (patch + pravidlo H-90 pro všechny skupiny, rozhodnutí Z-12 + sjednocení zápisu
`LOKALITA`, Z-16), živé limity 2026; jeden běh pro tři ryby a *T. cristatus* + *B. bombina*
(obojživelníci viz [`../TC_BB_20261105_cilmon/`](../TC_BB_20261105_cilmon/README.md)), 26,2 min.
Srovnání `aplikovano/srovnani/`.

| Druh | Běh | DP | dobrý | zhoršený | špatný | neznámý | DP s cíleným monitoringem |
|---|---|---|---|---|---|---|---|
| *Cottus gobio* | po | 874 | 659 | 42 | 173 | 0 | 494 |
| | **aplikovano** | 789 | **53** | 2 | 177 | **557** | 428 |
| *Lampetra planeri* | po | 563 | 376 | 95 | 92 | 0 | 367 |
| | **aplikovano** | 510 | **5** | 1 | 51 | **453** | 322 |
| *Misgurnus fossilis* | po | 390 | 276 | 79 | 35 | 0 | 179 |
| | **aplikovano** | 384 | **0** | 0 | 13 | **371** | 175 |

Verdikt z cíleného monitoringu (vstup EVL), aplikovano, d / z / š / neznámý: *Cottus* 53 / 2 / 177 / 196,
*Lampetra* 5 / 1 / 51 / 265, *Misgurnus* 0 / 0 / 13 / 162.

- **Sjednocení zápisu `LOKALITA` (Z-16):** DP s cíleným monitoringem 494 → 428, 367 → 322, 179 → 175 —
  shodně s odhadem revize (425 / 322 / 175).
- **Neznámé klíčové indikátory:** `POP_DYN` (*Cottus* 557, *Lampetra* 453, *Misgurnus* 368) a
  `POP_VITALITA3` (554 / 388 / 348) — většina lokalit má stále jen jeden nebo dva roky dat.
- **Přednost „neznámý" před „špatný":** z DP „neznámý" má nesplněný klíčový indikátor *Cottus* 3,
  *Lampetra* 64, *Misgurnus* 23 — při opačné přednosti by byly špatné.

**EVL (dvojice předmětů ochrany):**

| Druh | po: d / z / š / ? | **aplikovano**: d / z / š / ? | přechody po → aplikovano |
|---|---|---|---|
| *Cottus gobio* | 8 / 0 / 22 / 0 | **5 / 0 / 22 / 3** | dobrý → neznámý 3, dobrý → špatný 2, **špatný → dobrý 2** |
| *Lampetra planeri* | 2 / 4 / 21 / 1 | **1 / 0 / 14 / 13** | zhoršený → neznámý 4, špatný → neznámý 7, dobrý → neznámý 1 |
| *Misgurnus fossilis* | 0 / 0 / 9 / 0 | **0 / 0 / 6 / 3** | špatný → neznámý 3 |

Dvě EVL *Cottus* přešly ze „špatný" na „dobrý": jejich špatné lokality mají vedle nesplněného
i nehodnocený klíčový indikátor, staly se neznámými a do podílu dobrých lokalit nevstupují.
EVL „neznámý" = žádná hodnocená lokalita (`LOK_PROCDOBR` NA: *Cottus* 3, *Lampetra* 13, *Misgurnus* 3).

**Dopad pravidla H-90 na všechny skupiny** — export DP posledního plného běhu (2026-08-28,
starší kód a limity), reprezentativní návštěva DP (`aplikovano/srovnani/dopad_h90_vsechny_skupiny_beh20260828.csv`):

| Skupina | DP | dobrý před → po | zhoršený | špatný | neznámý po |
|---|---|---|---|---|---|
| Cévnaté rostliny | 5 275 | 3 473 → 125 | 172 → 32 | 1 630 → 900 | 4 218 |
| Obojživelníci | 2 168 | 1 006 → 299 | 91 → 51 | 1 071 → 494 | 1 324 |
| Ryby a mihule | 926 | 374 → 77 | 6 → 0 | 546 → 348 | 501 |
| Brouci | 863 | 809 → 768 | 6 → 6 | 48 → 47 | 42 |
| Letouni, motýli, vážky | 6 957 | beze změny | | | 0 |

## Obsah

| Co | Kde |
|---|---|
| exporty DP, EVL a nálezů obou běhů | `pred/export/`, `po/export/` |
| konečný stav (pět druhů): export, log, srovnání, skripty | `aplikovano/` |
| logy běhů | `log/` |
| srovnání (verdikty, indikátory, EVL, migrace, varianty H-90) | `srovnani/` |
| config testu (`TEST_SPECIES` = tři druhy), skripty srovnání | `skripty/` |
