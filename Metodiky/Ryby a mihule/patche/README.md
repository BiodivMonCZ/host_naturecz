# Patch kódu — ryby a mihule, metodika EVL 2026

**Soubor:** [ryby_metodika_2026.patch](ryby_metodika_2026.patch) · **Základ:** větev
`ryby-metodika-2026` (pět patchovaných souborů beze změny od `35a3d99`) · **Stav:**
**aplikováno** na větev `ryby-metodika-2026` (rozhodnutí Z-13, 2026-11-05) spolu s limity
metodiky 2026 v živých `limity_ryby.csv` a `cis_ryby_delky_strukt.csv`. Patch zůstává jako
záznam ověřeného stavu.

Nad rámec patche byly týmž rozhodnutím zapsány přímo do kódu (nejsou v patchi):

| Změna | Soubor | Rozhodnutí | Nález |
|---|---|---|---|
| neznámý klíčový indikátor ⇒ neznámý celkový stav DP, **všechny skupiny** (neznámý má přednost před špatným) | `24` | Z-12 | H-90 |
| pole s neznámým stavem se nezapočítává do podílu dobrých polí; žádné hodnocené pole ⇒ `NA` | `25` (hmyz) | Z-12 | H-90 |
| sjednocení zápisu `LOKALITA` (pomlčky, mezery, velikost písmen) na nejčastější zápis v EVL | `02_n2k_data_druhy.R` | Z-16 | H-71 |

Patch byl ověřen testovacím během v izolované kopii repozitáře spolu s tehdejším návrhem
limitů (výsledky: `Outputs/Test/Ryby_20261105/`); konečný stav včetně změn v tabulce výše
ověřil běh `Outputs/Test/Ryby_20261105/aplikovano/`. Bez nových limitů nové indikátory nemají
řádek limitu a `21_2` je zahodí; bez patche naopak nové řádky limitů nemají sloupec a zůstanou
nehodnocené — proto se aplikovalo obojí současně.

## Co patch mění

| Soubor | Změna | Nález | Dotčené skupiny |
|---|---|---|---|
| `R/00_config/02_n2k_data_druhy.R` | `KOD_LOKAL` u ryb = `LOKALITA` (místo `<naz_tok>` / kódu EVL) | H-71, Z-1 | ryby |
| `R/00_config/02_n2k_data_druhy.R` | `ZDROJ %in% CIS_CILMON$ZDROJ_NDOP` (dosud se neshodoval nikdy) | H-89, Z-1 | **všechny** — 2 128 záznamů nově `CILMON = 1` (obojživelníci 2 106, motýli 18, brouci 4) |
| `R/02_druhy/21_1_n2k_druhy_akce.R` | konstanty a funkce `DRUHY_RYBY_2026`, `PASMA_POKRYVNOSTI`, `soucet_pasem()`, `vegetace_submerzni()`, `plocha_na_100m()`, `odlov_z_poznamky()`, `podminky_odlovu()`, `dyn_rok()`, `vitalita3()` | H-72 … H-84 | jen tři druhy |
| `21_1` | párový filtr druh × EVL | H-85, Z-8 | **všechny** — viz tabulka níže |
| `21_1` | počty 1. a 2. odlovu z `POZNAMKA` (u mihule larvy), abundance z nich; zdroj ve `POP_POCETODLOVZDROJ` | H-74, Z-3 | tři druhy |
| `21_1` | `STA_DNOTYPSOUCETPROCENT` (součet středů pásem), `STA_POKRVEGSUBMERZ`, oba `STA_ODHADCELKOVEPLOCHY…` na 100 m, příznak `STA_PODMINKYODLOVU` | H-76, H-77, H-84, Z-7, Z-11 | tři druhy |
| `21_1` | `POP_DYN` za lokalitu a rok (roční maxima, 3 : 3 / 2 : 2; *Lampetra* varianta C), `POP_VITALITA3` | H-72, H-73, H-79, Z-2 | tři druhy; ostatní ryby beze změny |
| `R/02_druhy/24_n2k_druhy_lokality.R` | „zhoršený“ od **jednoho** nesplněného stanovištního parametru | H-75 | tři druhy |
| `R/02_druhy/25_n2k_druhy_uzemi.R` | `STA_MIGPROSTUP` na úrovni EVL; rozhodovací tabulka lokality × prostupnost; 75 %, při třech dvě; žádná hodnocená lokalita ⇒ neznámý | H-81, H-82, H-83, Z-5, Z-6 | tři druhy |
| `R/02_druhy/27_n2k_druhy_zapis.R` | párový filtr druh × EVL v předfiltru | H-85, Z-8 | **všechny** |

### Dopad párového filtru na ostatní skupiny (H-85, záznamy od 2013)

Vyřazené záznamy pocházejí z EVL, kde druh **není** předmětem ochrany. Na úrovni EVL je
odstraňoval už export (`27`), na úrovni DP se vykazovaly.

| skupina | záznamů | vyřazeno | z toho monitoring | podíl | dvojic druh × EVL |
|---|---|---|---|---|---|
| Letouni | 11 760 | 5 119 | 56 | 43,5 % | 598 |
| Obojživelníci | 24 327 | 4 180 | 964 | 17,2 % | 322 |
| Brouci | 10 153 | 1 999 | 112 | 19,7 % | 344 |
| Motýli | 10 510 | 1 495 | 102 | 14,2 % | 313 |
| Cévnaté rostliny | 10 916 | 1 275 | 843 | 11,7 % | 98 |
| Ryby a mihule | 5 106 | 1 265 | 295 | 24,8 % | 198 |
| Vážky | 1 330 | 442 | 15 | 33,2 % | 128 |

Nejvíce záznamů monitoringu: *Bombina bombina* 474, *Pulsatilla grandis* 430, *Triturus
cristatus* 334, *Cypripedium calceolus* 217, *Cottus gobio* 196.

## Jak byl aplikován (2026-11-05)

```sh
# z korene repozitare, na vetvi ryby-metodika-2026
git apply --ignore-whitespace "Metodiky/Ryby a mihule/patche/ryby_metodika_2026.patch"
# limity 2026 do zivych souboru (obdoba 54_cevnate_limity_2026.R): skript nove zapisuje
# primo limity_ryby.csv a cis_ryby_delky_strukt.csv, zaklad cte z gitu (35a3d99);
# vystup je bajtove shodny s overenym navrhem *_2026.csv, ktery byl pak zrusen
Rscript R/05_metodiky/55_ryby_limity_2026.R
```

`--ignore-whitespace` je nutné: soubory `21_1`, `24`, `25`, `27` jsou v repozitáři uloženy
s CRLF, `02_n2k_data_druhy.R` s LF; patch je vůči obsahu `HEAD` bajtově přesný (ověřeno
aplikací na čistou kopii `HEAD` — výsledek bajtově shodný s testovanou kopií).
