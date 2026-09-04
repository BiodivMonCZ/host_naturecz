# Plný běh *Triturus cristatus* a srovnání s metodikou — 2026-09-04

Podklad k nálezům **H-59 … H-64** v
[harmonizace_registr.md](../../../Metodiky/Obojzivelnici/harmonizace_registr.md).
Kaskáda `21_1` → `27` na všech třech úrovních, 7,1 min, bez chyb.

## Výsledek běhu

| Úroveň | Rozsah | Hodnocení |
|---|---|---|
| nález | 127 775 řádků · 6 555 nálezů · 724 DP | 16 indikátorů |
| dílčí plocha | 724 DP × 19 indikátorů | 336 dobrý · 14 zhoršený · 374 špatný |
| území | 191 EVL | 38 dobrý · 29 zhoršený · 36 špatný · 88 neznámý |

Rozpad verdiktů DP potvrzuje **přesnou shodu s Tabulkou 1**: „dobrý" jen při
0–1 špatných stanovištních indikátorech, „zhoršený" právě při 2, „špatný" vždy
při ≥ 1 špatném klíčovém. Stejně tak všechny čtyři kombinace Tabulky 2.

## Shoda s metodikou

**Odpovídá:** Příloha 1 (všech 11 řádků × 4 sloupce druhů), Tabulka 1,
Tabulka 2, sezónní okno duben–červenec u manipulace, práh vysychání 0 %,
vyloučení neznámých DP z procenta, přítomnost druhu, agregace populačních
indikátorů na nejvyšší hodnotu.

**Neodpovídá — viz nálezy:**

| Nález | Věc | Rozsah |
|---|---|---|
| **H-59** | stanovištní `val` indikátory se agregují na nejlepší, ne nejhorší hodnotu | 249 dvojic DP × rok |
| **H-60** | početnost za EVL vynechává DP s pouze relativní početností | 131 z 1 780 dvojic |
| **H-61** | hodnocená množina neodpovídá Příloze 2 | 191 vs 66 EVL, 724 vs 192 DP |
| **H-62** | `CILMON` se uplatňuje nesouměrně mezi úrovněmi | 296 ze 724 DP |
| **H-63** | `amplexus` se počítá jako doklad reprodukce | 15 záznamů |
| **H-64** | jednotky `POP_POCET` jsou širší, než metodika připouští | 4 druhy |

## Obsah

| Co | Kde |
|---|---|
| výpis běhu | `log/run.log` |
| exporty všech tří úrovní (UTF-8 i Windows-1250) | `export/` |
| výstup úrovně EVL | `n2k_druhy_chu.csv.gz` |
| **normativní text metodiky s přijatými revizemi** | `metodika_prijate_revize.txt` |
| skript, kterým byl text z `.docx` získán | `extrakce_metodiky.R` |

`metodika_prijate_revize.txt` je strojově čitelná podoba
`met_ssEVL_SLOUCENE_…_zmeny.docx` po **přijetí** sledovaných změn (odstraněny
`<w:del>`, `<w:delText>`, `<w:moveFrom>`). Tabulky jsou zachovány značkami
`[TABULKA]`, `[RADEK]` a tabulátory mezi buňkami, takže Přílohu 1 i Přílohu 2
lze číst strojově. Uloženo proto, aby další srovnání nemuselo extrakci opakovat
a aby bylo dohledatelné, na jaké znění metodiky nálezy odkazují.
