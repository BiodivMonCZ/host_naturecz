# Běh *Triturus cristatus* po opravách H-59 a H-60 — 2026-09-05

Doklad k nálezům **H-59** a **H-60** v
[harmonizace_registr.md](../../../Metodiky/Obojzivelnici/harmonizace_registr.md).
Srovnáváno proti běhu [`../Triturus_cristatus_20260904/`](../Triturus_cristatus_20260904/).

## Výsledek

| | před | po |
|---|---|---|
| DP (724) | 336 dobrý · 14 zhoršený · 374 špatný | **318 · 32 · 374** |
| EVL (191) | 38 dobrý · 29 zhoršený · 36 špatný · 88 neznámý | **35 · 29 · 39 · 88** |
| změněných DP | — | **18** (všechny dobrý → zhoršený) |
| změněných EVL | — | **6** (3× dobrý → zhoršený, 3× zhoršený → špatný) |

Do „špatný" se na úrovni DP neposunula žádná plocha — stanovištní indikátory
to podle Tabulky 1 samy způsobit nemohou. Veškerý posun jde za **H-59**.

## H-60 je verdiktově neutrální

Izolovaným srovnáním (běh jen s H-59 vs. běh s oběma) vychází:

| | |
|---|---|
| záznamů převedených mediánem místo dolní meze | **184** |
| EVL s vyšší hodnotou `POP_POCETPRUM3` | **15 z 97** (nejvíce +24,0 u `CZ0723423`) |
| změněných `STAV_IND` u `POP_POCETPRUM3` | **0** |
| změněných verdiktů EVL | **0** |

Nárůsty nestačily překročit cílový stav, takže oprava dnes žádný verdikt
nemění — je to srovnání s metodikou, ne změna hodnocení.

Převodní tabulka `cis_pocet_kat.csv`, kategorie 4 („stovky"): dolní mez **101**
→ medián **500**, přesně jak metodika uvádí v příkladu.

## Obsah

| Co | Kde |
|---|---|
| výpis běhu | `log/run.log` |
| exporty všech tří úrovní | `export/` |
| početnost za území (vstup Tabulky 2) | `n2k_druhy_pocetnost.csv.gz` |
| výstup úrovně EVL | `n2k_druhy_chu.csv.gz` |
| **normativní text NOVÉ verze metodiky** | `metodika_prijate_revize.txt` |

Text odpovídá dokumentu ve stavu k 2026-09-05 (390 549 B) s přijatými revizemi.
Předchozí znění je u běhu `Triturus_cristatus_20260904`; rozdíly obou znění
shrnuje registr v sekci *Nová verze metodiky*.
