# Zprovoznění stanovištních indikátorů ryb — 2026-09-04

Doklad k nálezu **H-42** v
[harmonizace_registr.md](../../../Metodiky/Obojzivelnici/harmonizace_registr.md).
Kaskáda `21_1` → `27` proběhla nad třemi druhy, u dvou z nich před zásahem
i po něm. Jediným rozdílem mezi běhy je commit `2c605e5`.

## Proč tři druhy

| druh | proč právě on |
|---|---|
| *Triturus cristatus* | **regrese** — ověřuje, že zásah do sdíleného porovnání `val` nezměnil nic mimo ryby |
| *Cottus gobio* | největší datová sada ryb (3 045 záznamů, 30 lokalit) a čtyři nové indikátory |
| *Lampetra planeri* | jediný druh ryb s indikátorem na **úrovni EVL**, a zároveň druh s nejnepříznivějším limitem (viz H-45) |

## Výsledek

| Běh | DP | Nové indikátory | Změněné verdikty |
|---|---|---|---|
| *Triturus cristatus* | 724 | — | **0** — 336 / 14 / 374 před i po |
| *Cottus gobio* | 370 | 4 | **0** — 119 / 3 / 248; `CELKOVE_SUM` ↑ u 268 DP |
| *Lampetra planeri* | 253 | 1 | **5** z „dobrý" na „zhoršený"; úroveň EVL **0 z 80** |

*Cottus gobio* vyšel neutrálně proto, že jeho verdikty drží klíčové indikátory
(`POP_DYN` selhává 180×, `POP_VITALITA` 182×) a nové stanovištní indikátory
přidaly nejvýše jedno selhání na DP — pravidlo „min 2 špatné stanovištní
indikátory" se tedy neuplatnilo. `STA_PROUD` neselhává vůbec: jeho výčet
(`mírný | peřeje | tůně`) pokrývá prakticky všechny zaznamenané typy proudění,
takže přispívá stejně do očekávaných i splněných.

Pět posunutých DP u *Lampetra planeri* jde **výhradně** za limitem
`STA_DNOTYP = kompaktní jílové dno`, který se v datech druhu vyskytuje
u 2,6 % záznamů — viz **H-45**. Extrakce je ověřená, limit nedotčen.

## Obsah

| Co | Kde |
|---|---|
| výpisy pěti běhů | `log/` |
| 29 jednotkových testů pomocných funkcí | `test_pomocnych_funkci.R` |

Skripty testovacího běhu jsou v
[`../Triturus_cristatus_20260903/skripty/`](../Triturus_cristatus_20260903/skripty/);
zde se liší jen tím, že `TEST_SPECIES` čte z proměnné prostředí, aby šlo
spustit více druhů za sebou.

## Jak testy spustit

```sh
Rscript Outputs/Test/Ryby_20260904/test_pomocnych_funkci.R
```

Testy načtou pomocné funkce přímo z `R/02_druhy/21_1_n2k_druhy_akce.R`
a `21_2_n2k_druhy_akce_lim.R`, takže se spouští z kořene repozitáře a nemají
žádnou vlastní kopii kódu. Pokrývají slovníky, překryv prefixů
(`Umělé střední zahloubení` × `střední zahloubení`), čárku uvnitř kategorie
(`Umělý substrát (dlažba, beton)`), převod pásem a hraniční případy
`val_shoda()` včetně desetinné čárky.
