# Regresní běh — `CILMON = 0` nesmí přispívat do `LOK_PROCDOBR` (2026-09-11)

Doklad k nálezům **H-69** a **H-70**. Srovnáváno proti běhu
[`../TC_BV_20260911_plocha50/`](../TC_BV_20260911_plocha50/) — stejná data,
stejné druhy, jediná změna je popsaná níže.

## Co se měřilo

Fáze 1–2 (`21_1`, `21_2`) se nespouštěly znovu — vstupem obou větví je týž
soubor `Data/Temp/n2k_druhy_lim.csv` z běhu 20260911 (206 352 záznamů,
*Triturus cristatus* + *Bombina variegata*). Přepočítány byly jen fáze 3 a 4:

| větev | `21_2` | `24` | `25` |
|---|---|---|---|
| **base** | pracovní strom | stav před opravou (`git HEAD`) | stav před opravou |
| **fix** | pracovní strom | s blokem 6 | s `CELKOVE_CILMON` |

Kontrola věrnosti harnessu: větev **base** dala verdikty EVL na jednotku
shodné s během 20260911 (TC 35 / 29 / 39 / 88, BV 6 / 12 / 12 / 33).

## Co se změnilo v kódu

Nový sloupec `CELKOVE_CILMON` v [24_n2k_druhy_lokality.R](../../../R/02_druhy/24_n2k_druhy_lokality.R)
(blok 6): týž verdikt Tabulky 1, ale spočtený **jen ze záznamů s `CILMON = 1`**.
Počítá se rekurzí přes tutéž funkci, takže se logika Tabulky 1 neduplikuje.
[25_n2k_druhy_uzemi.R:191](../../../R/02_druhy/25_n2k_druhy_uzemi.R#L191) z něj
staví `LOK_POCETSUM` a `LOK_POCETDOB` místo dřívějšího `CILMON == 1 & HOD_IND`.
Do exportu DP sloupec nevstupuje.

## Výsledek

| | base | fix |
|---|---|---|
| řádků úrovně DP | 18 920 | 18 920 |
| úroveň DP | — | **bajtově shodná** |
| změněné indikátory EVL | — | **9 řádků / 4 EVL** |
| změněné verdikty EVL | — | **1** |

Verdikty EVL:

| druh | dobrý | zhoršený | špatný | neznámý |
|---|---|---|---|---|
| *T. cristatus* — base | 35 | 29 | 39 | 88 |
| *T. cristatus* — fix | **34** | **30** | 39 | 88 |
| *B. variegata* — base i fix | 6 | 12 | 12 | 33 |

Jediný přesun: `CZ0624129` (*T. cristatus*) **dobrý → zhoršený**.

### Devět změněných řádků

| EVL | druh | indikátor | base | fix | proč |
|---|---|---|---|---|---|
| CZ0724089 | BV | `LOK_DILCPOCET` | 22 | 23 | DP přibyla (H-70) |
| CZ0724089 | BV | `LOK_DILCDOBRE` | 9 | 10 | — |
| CZ0724089 | BV | `LOK_PROCDOBR` | 40,909 | 43,478 | verdikt zůstal špatný |
| CZ0624129 | TC | `LOK_DILCPOCET` | 1 | 2 | DP přibyla (H-70) |
| CZ0624129 | TC | `LOK_PROCDOBR` | 100 | **50** | **dobrý → zhoršený** |
| CZ0714073 | TC | `LOK_DILCPOCET` | 9 | 10 | DP přibyla (H-70) |
| CZ0714073 | TC | `LOK_PROCDOBR` | 11,111 | 10 | verdikt zůstal špatný |
| CZ0724089 | TC | `LOK_DILCDOBRE` | 2 | **1** | DP přehodnocena (H-69) |
| CZ0724089 | TC | `LOK_PROCDOBR` | 66,667 | 33,333 | verdikt zůstal špatný |

Změna jde **oběma směry**: tři DP se do jmenovatele nově dostaly (H-70), u čtyř
DP se verdikt po omezení na cílený monitoring zhoršil (H-69).

### Rozdíly mezi plným a cíleným verdiktem DP

Ze 1 092 DP má cílený verdikt 430 (TC) + 326 (BV). Liší se u **čtyř**:

| EVL | DP | `CILMON` vítězné návštěvy | verdikt ze všech dat | verdikt z cíleného monitoringu |
|---|---|---|---|---|
| CZ0323149 | CZ0323149 | 1 | zhoršený | **špatný** |
| CZ0624129 | CZ0624129 | 0 | dobrý | **špatný** |
| CZ0714073 | CZ0714073 | 0 | dobrý | **špatný** |
| CZ0724089 | CZ0724089 | 1 | dobrý | **špatný** |

Všechny čtyři jdou směrem dolů — přesně jak předpovídá H-69: nahodilý nález
přidává pozorování, ale nikdy nepřidává důvod k horšímu hodnocení.

## Co se nezměnilo: „neznámý"

**88 (TC) a 33 (BV) — beze změny.** Zpřísnění `CILMON` nemohlo počet snížit,
protože jen odebírá příspěvky; teoreticky jej mohlo zvýšit, nestalo se to.

U všech 121 neznámých EVL platí i po opravě:

| kontrola | výsledek |
|---|---|
| `LOK_DILCPOCET` | **0** u všech 121 |
| `LOK_PROCDOBR` → `STAV_IND` | **„neznámý"** u všech 121 |
| DP zcela bez verdiktu | **0 EVL** |
| DP s `CILMON = 1` | **0** (TC i BV) |
| DP s cíleným verdiktem | **0** (TC i BV) |
| zahozených verdiktů DP | **92 TC** (87 dobrý + 5 špatný) · **36 BV** (34 dobrý + 2 špatný) |

Příčina je tedy nadále **jediná a beze zbytku tatáž**: tato území nemají ani
jednu DP z cíleného monitoringu. Nejde o vadu výpočtu — je to důsledek toho,
že metodika filtr na úrovni území předepisuje, zatímco na úrovni DP ne.
**Rozhodnutí patří autorům metodiky (H-62)**; kódem to řešit nelze.

## Obsah

| Co | Kde |
|---|---|
| verdikty EVL před/po | `srovnani/verdikty_EVL.csv` |
| změněné indikátory EVL | `srovnani/zmenene_indikatory_EVL.csv` |
| verdikt DP plný vs. cílený | `srovnani/dp_verdikt_cileny.csv` |
| rozbor neznámých EVL | `srovnani/nezname_EVL.csv` |
| skripty běhu a srovnání | `skripty/` |
