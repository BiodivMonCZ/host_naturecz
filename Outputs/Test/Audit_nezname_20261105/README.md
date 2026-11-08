# Audit neznámých klíčových indikátorů v cíleném monitoringu — 2026-11-05

Podnět zadavatele: *„velmi pochybuji o vysokém podílu neznámých, protože všechny indikátory by
měly být pokryty v datech cíleného monitoringu."* Registr:
[../../../Metodiky/Ryby a mihule/harmonizace_registr.md](../../../Metodiky/Ryby%20a%20mihule/harmonizace_registr.md),
nález **H-95**.

**Vstup:** verdikty DP z cíleného monitoringu (`CELKOVE_CILMON`, vstup EVL) běhů
[`../Ryby_20261105/export_metodika/`](../Ryby_20261105/export_metodika/) (ryby, kód `982c999`) a
[`../Ryby_20261105/aplikovano/`](../Ryby_20261105/README.md#konečný-stav-větve--běh-aplikovano)
(*T. cristatus*, *B. bombina*); souřadnice záznamů z exportu NDOP 2026-05-10 (S-JTSK, přesnost 50 m).

**Pro každou DP a neznámý klíčový indikátor je určena příčina:**

| Kód | Příčina | Povaha |
|---|---|---|
| A1 | jediný (cílený) průzkum místa — žádný dřívější průzkum na DP ani do 100 m v téže EVL | skutečně chybí data: indikátor změny potřebuje dřívější průzkum |
| A2 | jediný průzkum pod tímto klíčem, totéž místo je **později** zkoumáno pod jiným klíčem | duplicita lokality |
| B | dřívější průzkum druhu do 100 m v téže EVL, ale pod **jiným klíčem** lokality | nepropojená řada (klíč / název) |
| C | dřívější průzkumy jsou, chybí vstup (ryby: abundance nebo délky; obojživelníci: kategorie početnosti) | chybí hodnota v záznamu |

## Výsledek

| Druh | DP s verdiktem z CILMON | z toho s neznámým klíčovým | A1 | A2 | B | C |
|---|---|---|---|---|---|---|
| *Bombina bombina* (`POP_ZMENARAD`) | 725 | 315 | 161 (51 %) | 29 (9 %) | 47 (15 %) | 78 (25 %) |
| *Triturus cristatus* (`POP_ZMENARAD`) | 524 | 275 | 155 (57 %) | 20 (7 %) | 39 (14 %) | 58 (21 %) |
| *Cottus gobio* (`POP_DYN`) | 428 | 193 | 114 (59 %) | 28 (15 %) | 42 (22 %) | 9 (5 %) |
| *Lampetra planeri* (`POP_DYN`) | 322 | 263 | 118 (45 %) | 28 (11 %) | 68 (26 %) | 49 (19 %) |
| *Misgurnus fossilis* (`POP_DYN`) | 175 | 152 | 66 (44 %) | 10 (7 %) | 52 (35 %) | 21 (14 %) |

`POP_VITALITA3` u ryb dává téměř stejné rozdělení; *T. cristatus* má navíc 15 DP s neznámým
`POP_REPROPERIOD3` (nerozebráno). Podrobně `srovnani/pricina_neznamych_klicovych.csv`.

## Co z toho plyne

1. **Největší část (44–59 %) je skutečně první průzkum.** Klíčové indikátory ryb (dynamika, vitalita) i
   obojživelníků (`POP_ZMENARAD`) jsou indikátory **změny** — z jednoho průzkumu je spočítat nelze.
   - Obojživelníci: 127 / 134 takových DP (*B. bombina* / *T. cristatus*) má jediný cílený průzkum
     **2021** — kampaň „Sledování stavu EVL – obojživelníci" (1 736 záznamů s kódem DP), jejíž DP
     pravidelný monitoring 2022–2025 znovu nenavštívil.
   - Ryby: jediný průzkum v letech 2019–2023; druhý by často přinesl monitoring 2024–2025, který
     v exportu chybí (H-94).
2. **21–42 % je nepropojená řada téhož místa (A2 + B).**
   - Ryby: jen varianty názvu, které sjednocení zápisu (Z-16) nespojí — překlepy
     („Blanice – Krečovice" × „Blanice - Krejčovice", „Volšovka – Sterzmühle" × „Starzmühle"),
     rozšířené názvy („Teplá Vltava I - Polka" × „Teplá Vltava a Zelenohorský potok I - Polka")
     a upřesnění („Spůlka - Stachy - hřiště" × „Spůlka – Stachy").
   - Obojživelníci: dřívější záznamy bez `KOD_LOKALITY` mají klíč = kód EVL (`CZ0213047`), pozdější
     kód DP (`CZ0213047_1`); dále přechod `amp…` → `CZ…_n` a přečíslování DP (`CZ0314023_10` × `_9`).
3. **5–25 % chybí hodnota v záznamu.** Obojživelníci: počet nebo relativní početnost je zapsána
   v jednotce, kterou limity pro kategorii početnosti nepřijímají (`adulti`, `subadulti`, `larvy` …,
   srov. H-64 registru obojživelníků). Ryby: chybí plocha úseku nebo délky.

## Obsah

| Co | Kde |
|---|---|
| příčiny po druzích a indikátorech; detail po DP | `srovnani/` |
| skripty auditu (zdroje obojživelníků, příčiny, detail, duplicity, souhrn) | `skripty/` |
