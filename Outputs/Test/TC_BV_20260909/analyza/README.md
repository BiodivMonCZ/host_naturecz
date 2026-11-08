# Rozbor příčin — běh TC + BV 2026-09-09

Dvě otázky z validace běhu: **co žene „neznámý“** na úrovni EVL a **proč tak
často selhává `POP_POCETPRUM3`**.

Skripty: `../../../../Test/04_analyza_priciny.R`, `05_analyza_prum3_detail.R`,
`06_jednotky.R`. Data: `priciny_neznamy_EVL.csv`,
`priciny_pop_pocetprum3.csv`, `pop_pocetprum3_detail.csv`.

---

## A. „Neznámý“ má **jedinou** příčinu

| druh | neznámý | podíl EVL | příčina |
|---|---|---|---|
| *Triturus cristatus* | 88 / 191 | 46 % | DP existují, ale **ani jedna z cíleného monitoringu** |
| *Bombina variegata* | 33 / 63 | 52 % | totéž |

**Ani jedno** území není neznámé proto, že by v něm chyběla DP — ve všech
121 případech DP s verdiktem existují. Zahazuje se jich **92 (TC)** a
**36 (BV)**.

### Mechanismus

`LOK_POCETSUM` v [25_n2k_druhy_uzemi.R:162](../../../../R/02_druhy/25_n2k_druhy_uzemi.R#L162)
počítá jen DP s `CILMON == 1`:

```r
LOK_POCETSUM = sum(ID_IND == "CELKOVE_HODNOCENI" & CILMON == 1 & HOD_IND %in% c("dobrý", "zhoršený", "špatný"))
LOK_PROCDOBR = case_when(..., LOK_POCETSUM == 0 ~ NA_real_, ...)
```

Odtud dál: `LOK_PROCDOBR` = NA → `STAV_PROCDOBR` = NA → Tabulka 2 (blok 5b)
nemá na co sáhnout a spadne do poslední větve `TRUE ~ NA_real_` → **neznámý**.
Ověřeno: u všech 121 neznámých území je `LOK_DILCPOCET` = 0 a `LOK_PROCDOBR`
je NA. Druhý indikátor Tabulky 2 přitom může být klidně známý — sám verdikt
nezachrání.

### Co z toho plyne

Míra „neznámý“ **není vlastnost dat o druhu, ale důsledek filtru na cílený
monitoring.** Stojí za rozhodnutí, protože úrovně si navzájem odporují:

- **úroveň DP** verdikt z dat `CILMON = 0` spočítá a vykáže,
- **úroveň EVL** tentýž verdikt odmítne agregovat.

Uživatel tak v exportu vidí DP s verdiktem „dobrý“ v území, které je jako
celek „neznámý“. Buď je správné brát `CILMON = 0` na obou úrovních, nebo na
žádné — současný stav je nekonzistentní. Metodika filtr na úrovni území
předepisuje, takže jde o **otázku pro autory**, ne o chybu kódu.

---

## B. `POP_POCETPRUM3` — čtyři příčiny, seřazené podle váhy

Selhává u **51/62** hodnocených EVL u TC a **19/21** u BV.

### 1. Cílový stav nebyl za 12 let nikdy dosažen (dominantní)

| druh | selhává | z toho cíl **nikdy** nedosažen | medián „nejlepší rok / cíl“ |
|---|---|---|---|
| *Triturus cristatus* | 51 | **38** (75 %) | 0,40 |
| *Bombina variegata* | 19 | **16** (84 %) | 0,22 |

U tří čtvrtin selhání nemůže indikátor na současných datech projít **bez
ohledu na způsob výpočtu** — ani nejlepší rok z celého dvanáctiletého okna
cíle nedosáhl. U BV stačil medián nejlepšího roku na 22 % cíle.

### 2. Cíl je řádově jinde než měření

| druh | stav | medián cíle | medián hodnoty | medián DP (CILMON=1) |
|---|---|---|---|---|
| TC | dobrý | 6 | 19,7 | 2 |
| TC | špatný | **55** | 6,3 | 3 |
| BV | dobrý | 16 | 25,7 | 52 |
| BV | špatný | **64** | 7,0 | 2 |

Selhání koreluje s **velikostí cíle**, ne s intenzitou monitoringu — počty
monitorovaných DP jsou u „dobrý“ i „špatný“ prakticky stejné (2 vs 3 u TC).
Rozdíl mezi cílem a hodnotou je u selhávajících území ~9–10×.

### 3. Devatenáct území má průměr 0 — jsou to skutečné nepřítomnosti

Ne chybějící data. Příklad `CZ0413185` (TC): 54 záznamů z cíleného
monitoringu, **všech 54** má `POP_POCET` = 0 a **všech 54** kategorii
početnosti 0 (= nepřítomnost). Stejně `CZ0214015` (BV, 37/37),
`CZ0623365` (TC, 89/89), `CZ0814093` (BV, 32/32).

Druh byl hledán a nenalezen. Indikátor se chová správně; nula je nález.

### 4. Jednotky jsou nesourodé (H-64) — systematické vychýlení dolů

Jednotka (`POCITANO`) u záznamů vstupujících do průměru:

| druh | bez jednotky | adulti | samci | samice | jedinci | nedospělá stadia |
|---|---|---|---|---|---|---|
| *T. cristatus* | **61,4 %** | 18,3 % | 6,9 % | 7,9 % | 3,9 % | 1,7 % |
| *B. variegata* | **64,3 %** | 25,1 % | 1,8 % | — | 8,8 % | — |

Tři problémy v jednom:

1. **Dvě třetiny záznamů nemají jednotku vůbec** — nelze ověřit, co se sčítá.
2. Maximum za DP a rok se bere **přes smíšené jednotky**; `max(15 samců,
   20 adultů)` není definovaná veličina.
3. Výsledek se porovnává s cílem vedeným v **jedincích**. Počet jednoho
   pohlaví je z principu nižší než počet jedinců, takže vychýlení jde
   **vždy ve prospěch selhání**.

U TC navíc do součtu vstupují `larvy`, `subadulti`, `snůšky` a `juvenilové`
(dohromady 1,7 %), ačkoli metodika připouští jen dospělce.

Toto je kvantifikace otevřeného nálezu **H-64**.

### Co se vylučuje jako příčina

| hypotéza | výsledek |
|---|---|
| průměr stojí na méně než 3 letech | ne — 47/51 selhání u TC má plné 3 roky |
| vstupní roky jsou staré | ne — medián věku nejnovějšího roku **0 let**, maximum 1 rok |
| málo monitorovaných DP | ne — viz tabulka v bodě 2, počty DP se neliší |

---

## Závěr pro zadavatele

- **„Neznámý“** je rozhodnutí o rozsahu dat (`CILMON`), ne chyba. Vyžaduje
  rozhodnutí autorů metodiky, protože úrovně DP a EVL se dnes chovají
  rozdílně.
- **`POP_POCETPRUM3`** selhává převážně proto, že **cílové stavy z SDO jsou
  nastavené nad vším, co monitoring kdy zaznamenal**. Oprava jednotek (H-64)
  je potřebná a odstraní systematické vychýlení, ale sama verdikty neobrátí —
  u 75–84 % selhání chybí k cíli násobky, ne procenta.
