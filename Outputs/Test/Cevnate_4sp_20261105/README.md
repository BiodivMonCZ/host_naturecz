# Dopad pravidla H-90 na hodnocení od roku 2024 — 2026-11-05

Odpověď na pokyn zadavatele k Z-19 (revize ryb, nález **H-91**): *„Týká se to aktuálních dat?
Pokud to významně nemění hodnocení od roku 2024, není to problém."* Registr:
[../../../Metodiky/Ryby a mihule/harmonizace_registr.md](../../../Metodiky/Ryby%20a%20mihule/harmonizace_registr.md).

Pravidlo H-90 (`27b2510`): alespoň jeden neznámý klíčový indikátor ⇒ neznámý celkový stav DP.

## Postup

1. **Současný kód:** `24` bez pravidla (`433b610`) a s pravidlem (`27b2510`) nad týmž
   mezivýsledkem `21_2` testovacího běhu; rok = rok hodnocené (reprezentativní) návštěvy DP.
   - *Cottus*, *Lampetra*, *Misgurnus*, *T. cristatus*, *B. bombina* — běh
     [`../Ryby_20261105/aplikovano/`](../Ryby_20261105/README.md#konečný-stav-větve--běh-aplikovano);
   - cévnaté *Pulsatilla grandis*, *Campanula bohemica*, *Cypripedium calceolus*, *Klasea lycopifolia*
     — **tento běh** (kód `a59a271`, izolovaná kopie, 3,1 min; čtyři druhy nejvíce dotčené v exportu níže).
2. **Ostatní skupiny:** export DP posledního plného běhu (`Outputs/Data/druhy/n2k_druhy_lok_2025_20260828`,
   starší kód a limity) — jen orientačně.

## Výsledek — DP s hodnocenou návštěvou 2024 a později

| Druh / skupina | DP 2024+ | změněno | dobrý bez → s pravidlem | zdroj |
|---|---|---|---|---|
| *Bombina bombina* | 484 | **133 (27 %)** | 207 → 121 | současný kód |
| *Triturus cristatus* | 312 | **104 (33 %)** | 97 → 42 | současný kód |
| *Cottus* / *Lampetra* / *Misgurnus* | 38 / 20 / 12 | 35 / 20 / 12 | — | současný kód; monitoring 2024–2025 v NDOP chybí (H-94) |
| cévnaté (4 druhy) | 638 | **0** | beze změny | současný kód — klíčový indikátor `POP_POCETMINPRIP` se nepočítá (H-25 cévnatých, H-92) |
| brouci | 201 | 5 (2,5 %) | 195 → 190 | export 2026-08-28 |
| letouni, motýli, vážky | 1 678 | 0 | — | export 2026-08-28 |

Verdikt z cíleného monitoringu (vstup EVL), rok 2024+: *B. bombina* 109 z 457 změněno, *T. cristatus*
92 z 299.

**Cévnaté na současném kódu:** všech 2 432 DP čtyř druhů nemá žádný řádek klíčového indikátoru —
2 410 je „dobrý", 22 „zhoršený" (*Cypripedium*, jen podle neklíčových indikátorů). Klíčové
indikátory limitů 2026 (`POP_POCETMINPRIP`, `POP_REPROUSP`, `POP_DELKAABSENCE`) zatím žádný kód
nepočítá.

**Proč je `POP_ZMENARAD` u obojživelníků 2024+ neznámý** (DP-roky): bez předchozího roku s cíleným
monitoringem 89 / 74 (*B. bombina* / *T. cristatus*); v hodnoceném roce chybí kategorie početnosti
59 / 55; poslední předchozí rok kategorii nemá, starší ano 30 / 18 (volba kódu, otázka Z-23); žádný
předchozí rok s kategorií 14 / 9.

## Obsah

| Co | Kde |
|---|---|
| export DP, EVL a nálezů běhu cévnatých | `export/` |
| log běhu | `log/` |
| souhrny podle roku: současný kód (všechny DP / cílený monitoring), export 2026-08-28 | `srovnani/` |
| config (`TEST_SPECIES`) a skripty rozboru | `skripty/` |
