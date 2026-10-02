# Registr nálezů — audit kódu a dat cévnatých rostlin proti harmonizované metodice (Fáze B)

**Zadání:** [harmonizace_cevnate_prompt.md](harmonizace_cevnate_prompt.md), §5.
**Měřítko:** [met_cevnate_2026_harmonizovana.md](met_cevnate_2026_harmonizovana.md) po rozhodnutí
zadavatele 2026-10-02 (viz [revize_metodika_cevnate.md](revize_metodika_cevnate.md),
§Rozhodnutí zadavatele — souhrn). Body, které zůstaly otevřené (R-19, R-24, R-31,
R-34), jsou auditovány proti dnešnímu znění a varianty jsou uvedeny u nálezu.
**Datum:** 2026-10-02 · **Větev:** `harmonizace-cevnate` · **Stav kódu:** commit `c8712d7`
(beze změny `R/` i `Data/Input/` od `druhy-202609`).

Fáze B **nezměnila žádný kód ani data**. Implementace není součástí zadání — návrhy
řešení jsou podklad pro samostatné zadání.

## Jak byl audit proveden

| Krok | Zdroj | Rozsah / poznámka |
|---|---|---|
| Normativní text | harmonizované znění 2026-10-02 | Příloha 1A (9 indikátorů DP), Příloha 1B (2 indikátory EVL), Tabulky 1 a 2, oddíly 3.1–3.3 |
| Číselník ISOP | `Data/Input/cis_indikatory_popis.csv` (**Windows-1250**) | 48 řádků |
| Limity | `Data/Input/limity_cevky.csv` (**Windows-1250**) | 594 řádků, 34 druhů; *Aconitum* 26 řádků |
| Cílové stavy | `Data/Input/sdo_cilove_druhy_20260820.csv` (UTF-8) | řádky cévnatých rostlin; české sloupce poškozené, nečteny pro logiku |
| Číselník period | `Data/Input/cis_evd_perioda.csv` | config jej čte jako Windows-1250, soubor je převážně UTF-8 s neplatnou sekvencí na ř. 35 (H-09) |
| Kód | `00_n2k_config.R`, `02_n2k_data_druhy.R`, `21_1`, `21_2`, `22`, `23`, `24`, `25`, `27` | čtení kódu; čísla řádků k commitu `c8712d7` |
| **Ostrá data** | `host_data/export_data_evl.csv` (UTF-8) | **2 251 záznamů *Aconitum*** (1 EVL CZ0724089 Beskydy), z toho **1 521 z projektu Monitoring druhů ČR** (2010–2025) |
| **Plný běh kaskády** | `21_1` → `27` pro *Aconitum*, aktuální kód | ořezaný config podle `Test/00_config_test_2sp.R` (jen `TEST_SPECIES`), spuštěno v **izolované kopii repozitáře mimo pracovní strom** (`Data/Temp` ani `Outputs` repozitáře nedotčeny); 1,4 min, bez chyb |
| **Srovnávací výpočet** | jednorázový skript mimo `R/` a mimo commit | pravidla harmonizovaného znění na týchž ostrých datech, dvě varianty identity DP — viz §Srovnávací výpočet |

Všechna čísla v nálezech jsou z ostrých dat nebo z výstupů plného běhu, není-li uvedeno
jinak. Ověřeno **jen na *Aconitum***; ostatních 33 druhů pouze souhrnně (§Limity bez
normativního zdroje).

## Výsledek dnešního kódu pro *Aconitum* (plný běh)

| Úroveň | Výsledek |
|---|---|
| nálezy (fáze 1–2) | 2 063 řádků fáze 1, 1 580 nálezů, roky **2013–2025** |
| dílčí plochy | **740 DP**, z toho jen **256 s kódem AFM**; **389 dobrý · 4 zhoršený · 347 špatný · 0 neznámý** |
| EVL CZ0724089 | **špatný** (`LOK_PROCDOBR` 27,2 %, `POP_PROCDOB` 15,5 %, obojí `min 50`) |

## Souhrn

| Závažnost | Počet | Nálezy |
|---|---|---|
| **Kritická** — plošně mění výsledek | 8 | H-01 … H-08 |
| **Vysoká** — mění výsledek části DP | 5 | H-09 … H-13 |
| **Střední** — konzistence, dohledatelnost, export | 4 | H-14 … H-17 |
| **Nízká** | 2 | H-18, H-19 |
| **Potvrzeno — bez akce** | 2 | P-01, P-02 |

| Typ | Počet | Nálezy |
|---|---|---|
| BUG | 10 | H-01, H-02, H-03, H-05, H-09, H-10, H-11, H-14, H-18, H-19 |
| ROZPOR-METODIKY | 5 | H-04, H-06, H-07, H-12, H-13 |
| GAP | 2 | H-08, H-15 |
| STOPA-DO-ISOP | 1 | H-16 |
| NÁZVOSLOVÍ | 1 | H-17 |

(H-15 je veden jako GAP; část „poslední nález počítá i negativní záznamy" je BUG.)

**Dotčení sdíleného kódu:** H-06, H-07, H-09, H-13, H-14, H-15, H-18 se týkají kódu,
který obsluhuje i obojživelníky, ryby, hmyz a savce — u každého je uvedeno, zda musí
být náprava podmíněna skupinou.

## Potvrzeno — bez akce

| # | Zjištění |
|---|---|
| P-01 | **Směr agregace v DP × rok odpovídá rozhodnutí R-28** (seed K-06 tím přestává být rozporem). [`24:104-112`](../../R/02_druhy/24_n2k_druhy_lokality.R#L104-L112): `POP_*` → `max()` (nejlepší), `POP_POSK*` a stanovištní → `min()` (nejhorší). Verze 2024 (výběr celého záznamu s nejvyšší početností) by se od kódu lišila; harmonizované znění ji opustilo. |
| P-02 | **Zdroj „cíleného monitoringu" u rostlin odpovídá R-15.** `CILMON = 1` má právě 1 627 řádků fáze 1, všechny z projektu „Monitoring druhů ČR"; žádný jiný projekt `CILMON = 1` nedostal. (Větev přes `ZDROJ` je přitom nefunkční — H-14 — u rostlin se to neprojeví.) |

---

# Kritické nálezy

### H-01 — Identita DP: záznamy monitoringu 2010–2020 nemají kód DP, tatáž plocha se rozpadá na dvě
- **Závažnost:** kritická · **Typ:** BUG (data + kód)
- **Odkazy:** seed **K-11**; revize R-15, R-18, R-19, R-20
- **Metodika:** 2.2 — „Vymezení DP pro účely hodnocení a monitoringu se řídí aktuální metodikou monitoringu daného druhu"; 3.1.4 — „Za počáteční početnost druhu na DP se považuje průměr početností z prvních tří monitoringů DP"; 2.1 krátkodobý trend — „průměr početností ze tří předchozích hodnocených sezón téže DP".
- **Stav v datech:** z 1 521 záznamů monitoringu má kód DP (`KOD_LOKALITY`, tvar `AFMnnn`) jen **631 — všechny z let 2021–2025**. **890 záznamů z let 2010–2020 kód nemá** a DP nese jen textový název (`LOKALITA`). **202** z 253 starých názvů se vyskytuje i u záznamů s kódem; **200 lze jednoznačně namapovat** na kód AFM, 2 názvy mají kódy dva, 51 nemá protějšek.
- **Stav v kódu:** [`02_n2k_data_druhy.R:228`](../../R/00_config/02_n2k_data_druhy.R#L228) výslovně vylučuje rostliny z náhrady `kod_chu`, [`:242`](../../R/00_config/02_n2k_data_druhy.R#L242) pak dosadí `KOD_LOKAL = LOKALITA`. DP do roku 2020 a od roku 2021 jsou proto **různé DP**.
- **Důsledek:** plný běh vykazuje **740 DP**, z toho jen 256 skutečných (AFM); zbylých **484 jsou „DP" pojmenované textem** — staré záznamy monitoringu a nahodilé nálezy. Historie početnosti každé plochy je rozdělena v roce 2021, takže počáteční početnost (první tři monitoringy) ani krátkodobý trend (tři předchozí sezóny) nelze u AFM ploch spočítat: od roku 2021 má **160 z 256 AFM ploch jedinou sezónu**, 89 dvě a 7 tři.
- **Dotčené druhy:** *Aconitum*; pravděpodobně všechny rostliny s monitoringem před zavedením Survey123 (neověřeno).
- **Dotčené skupiny ve sdíleném kódu:** rostliny (řádek 228 je podmíněn skupinou; náhrada názvem na ř. 242 je sdílená, ale u ostatních skupin se uplatní jen tam, kde chybí i `kod_chu`).
- **Návrh řešení:** vendorovaná převodní tabulka „starý název lokality → kód AFM" v `Data/Input/` (200 jednoznačných dvojic lze odvodit z dat, 53 názvů vyžaduje ruční přiřazení), případně doplnění kódů přímo v NDOP. Mapování musí být explicitní a ověřené garantem, ne odvozované za běhu.
- **Otevřená otázka:** Kdo potvrdí převodní tabulku a dořeší 2 nejednoznačné a 51 nenamapovatelných názvů? Má se mapování provést v NDOP, nebo jako číselník v repozitáři?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-02 — Početnost: jednotka „počet prýtů" se netrefí do dat — počty 2013–2020 chybí úplně
- **Závažnost:** kritická · **Typ:** BUG
- **Odkazy:** seed **K-02** (část); revize R-04, R-21
- **Metodika:** Příloha 1A — velikost populace i krátkodobý trend se počítají „z počtu prýtů".
- **Stav v datech:** `limity_cevky.csv`: `Aconitum…,POP_POCET,min,50,počet prýtů`. V NDOP je `POCITANO` = **„prýty"** (záznamy do roku 2020), od roku 2021 **„sterilní prýty"** + **„kvetoucí a plodné prýty"**, u 64 akcí místo toho **„plodné prýty"** (obě označení se v jedné akci nikdy nepotkají — jde o dvě jména téže části).
- **Stav v kódu:** [`21_1:351`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L351) bere jednotky `POP_POCET` z limitu, [`:421`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L421) porovnává přesnou shodou. „prýty" ≠ „počet prýtů" ⇒ `POP_POCET = NA`. Od roku 2021 se počet skládá přes `POP_POCETSUM` (jednotky „kvetoucí a plodné prýty", „sterilní prýty") — **„plodné prýty" mezi nimi chybí**.
- **Důsledek (plný běh):** u přítomného druhu má `POP_POCET` hodnotu v **0 ze 769 záznamů let 2013–2020** a v 936 z 1 097 let 2021–2025. U 64 akcí s „plodnými prýty" se sčítají jen sterilní prýty (podhodnocení). Chybějící počet se v [`21_1:1462`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1462) převádí na `POP_POCETMAX = 0`, takže **staré roky vstupují do trendu jako nulové populace** (viz H-03, souvisí s H-36 registru obojživelníků).
- **Dotčené druhy:** *Aconitum*. Souhrnně: u dalších druhů nese `JEDNOTKA` u `POP_POCET` 16 řádků tvaru „počet …" („počet trsů" 6×, „počet jedinců" 2× …), tedy týž vzor — **jednotlivě neověřeno**.
- **Dotčené skupiny ve sdíleném kódu:** žádné — oprava je v datech limitů.
- **Návrh řešení:** jednotky v `limity_cevky.csv` psát tvarem z NDOP (`prýty`; k `POP_POCETSUM` doplnit `plodné prýty`); `JEDNOTKA` je u těchto řádků funkční, ne zobrazovací (srov. H-67 obojživelníků).
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-03 — Krátkodobý trend (`POP_TREND`): obrácená logika, jedna hodnota pro všechny roky, „bez dat = splněno"
- **Závažnost:** kritická · **Typ:** BUG + ROZPOR-METODIKY
- **Odkazy:** seed **K-01** (potvrzen a rozšířen); revize R-07, R-20
- **Metodika:** 2.1 / Příloha 1A — „Početnost druhu na DP v hodnocené sezóně se porovná s průměrem početností ze tří předchozích hodnocených sezón téže DP … Limitní hodnoty je dosaženo, je-li početnost v hodnocené sezóně alespoň rovna průměru"; 3.1.3 — neznámý klíčový indikátor ⇒ DP neznámá.
- **Stav v datech:** `limity_cevky.csv`: `POP_TREND,max,1,posledních dvou let s menším počtem jedinců než je referenční hodnota,ano,lok`.
- **Stav v kódu:** [`21_1:1709-1765`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1709-L1765):
  1. `POP_TREND1/2 = 1`, je-li rok **stejný nebo lepší** než reference; `POP_TREND` je jejich součet. Limit `max 1` s jednotkou „let s **menším** počtem" počítá opak ⇒ indikátor **selže právě tehdy, když jsou oba roky dobré**; [`21_2:269-286`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L269-L286) hodnotu neinvertuje.
  2. `sum(..., na.rm = TRUE)` vrací bez dat **0 ⇒ splněno**.
  3. Reference = 3. nejnovější řádek (`POP_POCETMAX[3]`) bez ohledu na cílený monitoring (filtr na ř. 1718 je zakomentovaný); roky bez počtu mají `POP_POCETMAX = 0` (H-02).
  4. Trend se počítá **jednou na DP** a připojuje bez roku ([`:1861-1867`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1861-L1867)) — všechny ročníky DP dostanou tutéž hodnotu (tatáž vada jako H-11 obojživelníků).
  5. Algoritmus (dvě poslední hodnoty proti třetí) neodpovídá schválené definici (sezóna proti průměru tří předchozích).
- **Důsledek (plný běh):** všech **219/219 DP s více roky** má tutéž hodnotu `POP_TREND` ve všech letech. Na úrovni DP: **147 DP je „špatný" jen kvůli `POP_TREND`**; naopak **374 z 389 „dobrých" DP stojí na jediném hodnoceném klíčovém indikátoru — a tím je `POP_TREND` = 0 vzniklý z prázdných dat**.
- **Minimální příklad:** početnost 100 – 100 – 100 ⇒ `TREND1 = 1`, `TREND2 = 1`, `POP_TREND = 2 > 1` ⇒ **nesplněno** (stabilní populace selže). DP bez jediného počtu ⇒ `POP_TREND = 0` ⇒ **splněno**.
- **Dotčené druhy:** **všech 34 druhů** s limitem `POP_TREND` (33× `max 1`, 1× `min 1` s jednotkou „let s vyšším počtem" — jediný řádek se správnou orientací; u jednoho druhu je jednotkou nesmyslně „počet kvetoucích a plodných lodyh").
- **Dotčené skupiny ve sdíleném kódu:** blok se počítá jen u druhů s limitem `POP_TREND*` (dnes výhradně rostliny, viz H-25 obojživelníků) — oprava je de facto podmíněná skupinou.
- **Návrh řešení:** přepsat indikátor podle schválené definice (sezóna ≥ průměr tří předchozích hodnocených sezón v 12letém okně, počítáno pro každý rok, NA při méně než třech předchozích sezónách); limit `min 100` v %; neopravovat jen znaménko.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-04 — Velikost populace: absolutní limit 50 prýtů místo 75 % počáteční početnosti; počáteční početnost se nepočítá
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY
- **Odkazy:** seed **K-02**; revize R-04, R-19, R-25
- **Metodika:** Příloha 1A — „velikost populace | ano | poslední monitoring | ≥ 75 % | % počáteční početnosti DP (počet prýtů)"; 3.1.4 — počáteční početnost = průměr prvních tří monitoringů DP.
- **Stav v datech:** `POP_POCET,min,50,počet prýtů` — absolutní počet, bez vztahu k počáteční početnosti.
- **Stav v kódu:** počáteční početnost DP se **nikde nepočítá**; `POP_POCET` se porovnává přímo s 50. Na [`21_1:1287`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1287) je navíc konstanta `POP_POCETPRUM = 50`, kterou nic nepoužívá.
- **Důsledek (plný běh):** `POP_POCET` na DP: 75 splněno, 200 nesplněno, 465 neznámo. **71 DP je „špatný" jen kvůli tomu, že má méně než 50 prýtů** — bez ohledu na to, zda je populace stabilní. Dalších 87 DP selhává na `POP_POCET` + `POP_VITAL` současně (nulové početnosti).
- **Dotčené druhy:** *Aconitum*; souhrnně 29 druhů má `POP_POCET` s absolutním `min` — pro ostatní druhy metodika neexistuje (§Limity bez normativního zdroje).
- **Dotčené skupiny ve sdíleném kódu:** žádné (limit je v `limity_cevky.csv`, výpočet je nový).
- **Návrh řešení:** nový indikátor „velikost populace" = poslední početnost / průměr prvních tří monitoringů DP × 100, limit `min 75`; počáteční početnost počítat až po vyřešení H-01, jinak se počítá z rozdělené historie.
- **Otevřená otázka:** viz revize R-19 — kdo stanoví arbitrární počáteční početnost a jak postupovat při počáteční početnosti 0.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-05 — Vitalita: u přítomných rostlin vždy neznámá, u nepřítomnosti 0 — indikátor může jen selhat
- **Závažnost:** kritická · **Typ:** BUG
- **Odkazy:** revize R-21, R-27
- **Metodika:** Příloha 1A — „vitalita populace (podíl kvetoucích a plodných prýtů) | ano | ≥ 30 % | % počtu prýtů".
- **Stav v datech:** *Aconitum* má `POP_VITAL,info,NA,kvetoucí a plodné prýty` a `POP_VITAL,min,30,% počet prýtů`, ale **žádný řádek `POP_POCETVITAL`**.
- **Stav v kódu:** [`21_1:1151-1155`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1151-L1155) plní čitatel `POP_POCETVITAL` jen z jednotek řádku `POP_POCETVITAL` ⇒ pro *Aconitum* `NA`; [`:1301-1304`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1301-L1304) pak `POP_VITAL = NA`, kromě nepřítomnosti (`0`). Dvojí řádek `info` + `min` u `POP_VITAL` navíc vyvolá many-to-many v `right_join` (varování v běhu).
- **Důsledek (plný běh):** `POP_VITAL` je u **všech 1 866 záznamů s přítomným druhem neznámá**; vyhodnoceno 148 nálezů, **všech 148 nesplněno, žádné splněno**. Data přitom podíl spočítat umožňují — od roku 2021 nesou „kvetoucí a plodné / plodné prýty" i „sterilní prýty" (srovnávací výpočet: 111 DP splňuje, 154 nesplňuje).
- **Dotčené druhy:** *Aconitum*, *Dracocephalum austriacum* (oba mají `POP_VITAL min` bez `POP_POCETVITAL`).
- **Dotčené skupiny ve sdíleném kódu:** žádné.
- **Návrh řešení:** doplnit `POP_POCETVITAL` s jednotkami `kvetoucí a plodné prýty`, `plodné prýty`; před rokem 2021 (jen „prýty") zůstane vitalita neznámá — což podle R-27 znamená neznámou DP (viz §Srovnávací výpočet).
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-06 — Tabulka 1 (DP): rostliny hodnotí obojživelnická tabulka; neznámý klíčový indikátor se přeskakuje
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY
- **Odkazy:** seedy **K-05**, **K-07** (úroveň DP); revize R-02, R-03, R-26, R-27
- **Metodika:** 3.2 — „Do celkového stavu DP vstupují pouze klíčové indikátory; poškození rostlin a stanovištní indikátory se hodnotí a zobrazují, do celkového stavu DP nevstupují. … Stav zhoršený na úrovni DP nevzniká." Tabulka 1: alespoň jeden klíčový nelze hodnotit ⇒ **neznámý**; všechny splněny ⇒ dobrý; alespoň jeden nesplněn ⇒ špatný.
- **Stav v kódu:** [`24:155-156, 185-186`](../../R/02_druhy/24_n2k_druhy_lokality.R#L155-L186) počítají jen indikátory s `!is.na(STAV_IND)` a [`:203-211`](../../R/02_druhy/24_n2k_druhy_lokality.R#L203-L211) uplatní Tabulku 1 obojživelníků pro **všechny skupiny**:
  1. ≥ 2 nesplněné neklíčové (stanovištní **i poškození** — `KLIC = ne`) ⇒ **zhoršený**;
  2. neznámý klíčový indikátor se **přeskočí** (princip obojživelníků „indikátor bez dat se nehodnotí"), takže DP se hodnotí ze zbylých;
  3. DP bez jediného hodnoceného klíčového indikátoru a s ≤ 1 nesplněným neklíčovým ⇒ **dobrý**; stav „neznámý" na DP nevzniká vůbec.
- **Důsledek (plný běh):** **4 DP „zhoršený"** jen ze stanovištních / poškození (metodika: nevzniká). **0 DP „neznámý"**, ačkoli 465 DP nemá hodnocenou velikost populace a 650 vitalitu. **374 z 389 „dobrých" DP má hodnocený jediný klíčový indikátor** (a tím je chybný `POP_TREND`, H-03). Podle pravidel harmonizovaného znění by na týchž datech byla neznámá většina DP (§Srovnávací výpočet).
- **Dotčené druhy:** všech 34 druhů rostlin.
- **Dotčené skupiny ve sdíleném kódu:** **obojživelníci, ryby, hmyz, savci, rostliny** — větev je společná a pro obojživelníky je správná (jejich Tabulka 1 i pravidlo „indikátor bez dat se nehodnotí" platí). Náprava **musí být podmíněna skupinou** (rostliny), jinak by se změnila hodnocení obojživelníků a ostatních skupin.
- **Návrh řešení:** samostatná rozhodovací větev pro cévnaté rostliny (příznak skupiny nebo druhu z číselníku metodik): `N_KEY_UNKNOWN > 0` ⇒ neznámý; jinak `N_KEY_FAIL > 0` ⇒ špatný; jinak dobrý; neklíčové indikátory jen zobrazit. Pro to je nutné znát i počet klíčových indikátorů **očekávaných** (z limitů), ne jen hodnocených.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-07 — Úroveň EVL: hodnotí se dva jiné indikátory, dlouhodobý trend chybí, neznámé DP mimo jmenovatel, pravidlo 50 % neznámé populace chybí
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY
- **Odkazy:** seedy **K-03**, **K-07** (úroveň EVL); revize R-08, R-09, R-29, R-30, R-31
- **Metodika:** 3.3 — dva indikátory: **podíl DP v dobrém stavu** („počet DP v dobrém stavu / počet DP v dobrém, špatném či neznámém stavu × 100", limit ≥ 50 %) a **dlouhodobý trend populace** (celková početnost ≥ 75 % cílového stavu); Tabulka 2 (2×2); „Stav druhu v EVL nemůže být hodnocen … v případě, že se 50 % nebo více populace druhu nachází v DP v neznámém stavu."
- **Stav v datech:** `limity_cevky.csv` (úroveň `chu`): `LOK_PROCDOBR,min,50` ✓ a **`POP_PROCDOB,min,50`** („% populace v dobrém stavu" — podmínka, kterou rozhodnutí R-09 vypouští, navíc s hranicí 50 místo 75 z verze 2024). Řádek pro dlouhodobý trend **neexistuje**.
- **Stav v kódu:** [`25:191-193`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L191-L193) — jmenovatel `LOK_PROCDOBR` jen DP v dobrém / zhoršeném / špatném stavu (**bez neznámých**, rozpor s R-30). [`25:314-333`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L314-L333) — porovnání s cílovým stavem jen pro `DRUHY_METODIKY_OBOJ`. Verdikt [`25:374-384`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L374-L384) aritmetikou nad klíčovými `chu` indikátory — při dvou indikátorech dává shodou okolností tabulku 2×2, ale nad nesprávnou dvojicí. Pravidlo „≥ 50 % populace v neznámých DP ⇒ EVL neznámá" **není implementováno nikde**.
- **Důsledek (plný běh):** EVL CZ0724089 = **špatný** z `LOK_PROCDOBR` 27,2 % (125 dobrých z 460 DP s cíleným verdiktem) a `POP_PROCDOB` 15,5 %. Podle harmonizovaného znění by byla **neznámá** (56–100 % populace v neznámých DP; §Srovnávací výpočet).
- **Dotčené druhy:** všech 34 druhů (každý má `LOK_PROCDOBR` i `POP_PROCDOB`).
- **Dotčené skupiny ve sdíleném kódu:** **obojživelníci** (blok cílového stavu a Tabulka 2), **ryby, hmyz, savci, rostliny** (aritmetika `CELKOVE`, jmenovatel `LOK_PROCDOBR`). Změna jmenovatele i pravidlo neznámé populace **musí být podmíněny skupinou** — metodika obojživelníků výslovně předepisuje opak („DP s neznámým stavem … nevstupují").
- **Návrh řešení:** pro rostliny: z limitů vypustit `POP_PROCDOB`, doplnit indikátor dlouhodobého trendu na úrovni `chu`; rozšířit blok cílového stavu o rostliny s vlastním výpočtem celkové početnosti (R-31); jmenovatel s neznámými DP a pravidlo 50 % podmínit skupinou.
- **Otevřená otázka:** revize R-31 — potvrzení součtu „poslední monitoring každé DP v 12letém okně".
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-08 — Cílový stav z digitalizace SDO: *Aconitum* se jménem nenapojí a jednotka je jiná
- **Závažnost:** kritická · **Typ:** GAP
- **Odkazy:** seed **K-03** (upraven — *Aconitum* ve snímku **je**); revize R-05, R-06, R-31
- **Metodika:** 3.1.4 — „Cílovým stavem je navržená hodnota z výsledků digitalizace souhrnů doporučených opatření (SDO) pro danou EVL"; Příloha 1B — dlouhodobý trend „≥ 75 % cílového stavu EVL".
- **Stav v datech:** snímek obsahuje *Aconitum* pod `sdf_code` **4109**, ale latinský název je při digitalizaci rozdělen: `nazev_lat` = **„subsp. moravicum"**. Totéž u **18 řádků 5 poddruhů** (4098 *Iris humilis* subsp. *arenaria* 14×, 4094, 4073, 4088, 4109). Hodnota: `pop_min = pop_max = navrzena_hodnota = 600`, `pop_jednotka = jedinci`, `varovani = TRUE`; text SDO: „Na lokalitě se v současnosti vyskytuje cca 30–35 000 prýtů. Cílem je toto množství udržet." Podle jména se napojí 28 z 34 druhů `limity_cevky.csv`.
- **Stav v kódu:** [`02_n2k_data_druhy.R:45-48`](../../R/00_config/02_n2k_data_druhy.R#L45-L48) klíčuje cílové stavy `DRUH = nazev_lat` (mapuje jen synonymum *Triturus montandoni*) — *Aconitum* by se nenapojil, ani kdyby blok v `25` rostliny zahrnoval (H-07).
- **Důsledek:** celková početnost *Aconitum* v prýtech (běh: klouzavý průměr 3 let **5 360**, srovnávací výpočet: součet posledních monitoringů **9 524**) proti cíli 600 „jedinců" ⇒ dlouhodobý trend **trvale splněn** (≈ 1 600 % cíle). Nesoulad jednotek je o dva řády — tatáž třída jako S-4 / H-68 u obojživelníků, ale v opačném směru.
- **Dotčené druhy:** *Aconitum* a další poddruhy (výše); jednotky ostatních druhů neověřeny.
- **Dotčené skupiny ve sdíleném kódu:** obojživelníci (týž objekt `cilove_stavy`) — přechod na klíč `sdf_code` je pro ně neutrální, pokud se ověří počet napojených řádků (S-1).
- **Návrh řešení:** napojovat cílové stavy přes `sdf_code` (+ `sitecode`), ne přes jméno; nesoulad jednotek zdokumentovat a propsat do výstupu (vzor H-06 obojživelníků).
- **Otevřená otázka:** Má se u *Aconitum* porovnávat s hodnotou 600 „jedinců", nebo je cílovým stavem 30–35 000 prýtů z textu SDO? (Rozhodnutí R-31 zní „výsledky digitalizace SDO", tj. 600 — prosím potvrdit s vědomím, že je indikátor pak vždy splněn.)
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Vysoká závažnost

### H-09 — Okno 13 sezón místo 12; číselník period se nepoužívá
- **Závažnost:** vysoká · **Typ:** BUG
- **Odkazy:** revize R-18, R-20
- **Metodika:** 3.1.1 — „Hodnoceným obdobím je hodnocený rok a 11 let předcházejících (12 let)".
- **Stav v kódu:** [`21_1:382`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L382) a [`27:173`](../../R/02_druhy/27_n2k_druhy_zapis.R#L173) — `ROK >= current_year - 12`, kde `current_year` = loňský rok ([`00_n2k_config.R:51`](../../R/00_config/00_n2k_config.R#L51)) ⇒ **2013–2025 = 13 sezón**. `cis_evd_perioda` se načte ([`00_n2k_config.R:239-246`](../../R/00_config/00_n2k_config.R#L239-L246)), ale **nikde nepoužije**.
- **Stav v datech:** `cis_evd_perioda.csv` uvádí u *Aconitum* `PERIODA = 6` (rozhodnutí: 12). Soubor je převážně UTF-8 s neplatnou sekvencí na ř. 35, config jej čte jako Windows-1250 (latinská jména jsou ASCII, české se poškodí).
- **Důsledek (plný běh):** **123 DP** má reprezentativní návštěvu z roku 2013, který podle metodiky do okna nepatří.
- **Dotčené druhy:** všechny.
- **Dotčené skupiny ve sdíleném kódu:** **obojživelníci, ryby, hmyz, savci, rostliny** — filtr je společný. Změna na 12 let mění vstupní data všech skupin; podmínit délku okna druhem přes `cis_evd_perioda` (pak je třeba ověřit hodnoty u všech druhů).
- **Návrh řešení:** délku okna brát z číselníku period podle druhu (*Aconitum*: 12) a filtr psát `ROK > current_year - PERIODA`; číselník převést na jednotné kódování.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-10 — Hydrologické poměry se nikdy nehodnotí: `STA_HYDRPOMERY` × `STA_HYDROPOMERY`
- **Závažnost:** vysoká · **Typ:** BUG
- **Odkazy:** revize R-23
- **Metodika:** Příloha 1A — „hydrologické poměry | stanoviště | ne | … | Dobré nebo Zhoršené | kategorie".
- **Stav v kódu:** [`21_1:1202`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1202) vytvoří sloupec **`STA_HYDRPOMERY`** (tag `<STA_HYDRPOMERY>`); limity mají **`STA_HYDROPOMERY`**. `21_2` páruje přes `intersect(názvy sloupců, ID_IND)` ⇒ limit je sirotek, sloupec osiřelý.
- **Stav v datech:** tag nese **dobré 562 · zhoršené 381 · špatné 103 · nelze určit 25** (malá písmena, shodně s limity `val dobré / zhoršené`). Hodnota „nelze určit" by po zapnutí vyšla jako nesplněná — má být neznámá.
- **Důsledek:** indikátor se na žádné DP nedostane. Podle metodiky jde o neklíčový indikátor (jen zobrazení), dnes by se ale po zapnutí počítal do „≥ 2 nesplněné neklíčové ⇒ zhoršený" (H-06).
- **Dotčené druhy:** *Aconitum*, *Ligularia sibirica*, *Liparis loeselii*.
- **Dotčené skupiny ve sdíleném kódu:** žádné.
- **Návrh řešení:** sjednotit název (doporučeno přejmenovat limit na tvar tagu, nebo sloupec na tvar limitu — ne obojí); „nelze určit" normalizovat na `NA`.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-11 — Pokryvnosti: třídy zapsané s mezerami („5 - 25 %") se hodnotí jako nesplněné
- **Závažnost:** vysoká · **Typ:** BUG
- **Odkazy:** seed **K-09** (upraven — hranice tříd limitům odpovídají, kolizi působí zápis); revize R-22, R-24
- **Metodika:** Příloha 1A — E1 ≤ 75 %, dřeviny > 1 m ≤ 75 %, invazní = 0 %, expanzivní ≤ 25 %; „zaznamenává se v procentních třídách".
- **Stav v datech:** limity vyjmenovávají třídy `0 / do 5 % / 5-25 % / 25-50 % / 50-75 %` (E1, dřeviny), `0 / do 5 % / 5-25 %` (expanzivní), `0` (invazní) — **hranice limitů leží na hranicích tříd** ✓. V datech se ale vedle nich vyskytují **tvary s mezerami**: E1 „75 - 100 %" 83, „50 - 75 %" 77, „25 - 50 %" 62, „5 - 25 %" 16; expanzivní „5 - 25 %" 40, „25 - 50 %" 37, „50 - 75 %" 16.
- **Stav v kódu:** [`21_2:283-285`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L283-L285) porovnává přesnou shodou ⇒ tvar s mezerou = nesplněno.
- **Důsledek (plný běh, nálezy):** **105 chybných nesplnění** — E1 „50 - 75 %" 47, „25 - 50 %" 30, „5 - 25 %" 8; expanzivní „5 - 25 %" 20. (Tvary nad limitem jsou nesplněné správně.) Dnes vstupují do počtu nesplněných neklíčových (H-06); po nápravě H-06 jen do zobrazení.
- **Dotčené druhy:** *Aconitum*; tytéž tagy používá 31 druhů (expanzivní) a 13 (E1) — jednotlivě neověřeno.
- **Dotčené skupiny ve sdíleném kódu:** žádné, je-li náprava v limitech nebo v extrakci rostlin.
- **Návrh řešení:** normalizovat mezery kolem pomlčky při extrakci (`"5 - 25 %"` → `"5-25 %"`), nikoli rozšiřovat výčty limitů.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-12 — Poškození rostlin: součet kategorií přes typy poškození místo podílu poškozených prýtů
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY
- **Odkazy:** seed **K-04** (potvrzen); revize R-03, R-24
- **Metodika:** Příloha 1A — „poškození rostlin | populace | **ne** | … | ≤ 50 % | % počtu prýtů (třída)"; 3.1.2 — u poškození nejhorší hodnota.
- **Stav v datech:** `POP_POSKPERC,max,2,procenta počtu prýtů,ne` a `POP_POSKPOC,max,3,počet typů poškození rostlin,ne` (bez opory v metodice). Tag `<SP_POSKOZENI_ROSTLIN>` nese **třídu za každý typ poškození zvlášť** („Poškození hmyzem: 1-10 %; Poškození padlím: 10-50 %; …"), starší tvar bez typů a s mezerami („1 - 10 %; 11 - 50 %"). Celkový podíl poškozených prýtů se nezaznamenává.
- **Stav v kódu:** [`21_1:1334-1360`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1334-L1360) — `POP_POSKPERC` = **součet kategorií 0–3 přes typy** (v datech 0–11) proti limitu „max 2 procenta"; tvar „11 - 50 %" se nerozpozná (`NA`) a ze součtu vypadne. `POP_POSKPOC` = počet typů.
- **Důsledek (plný běh, DP):** `POP_POSKPERC` nesplněno u 74 DP, `POP_POSKPOC` u 7 — obojí dnes vstupuje do počtu nesplněných neklíčových indikátorů, ze kterého vzniká stav „zhoršený" (H-06).
- **Dotčené druhy:** 33 druhů (`POP_POSKPERC`), 31 (`POP_POSKPOC`).
- **Dotčené skupiny ve sdíleném kódu:** žádné.
- **Návrh řešení:** po rozhodnutí autorů jeden indikátor „poškození rostlin" v % třídách; `POP_POSKPOC` odstranit nebo vést jako informativní.
- **Otevřená otázka pro autory:** Jak z tříd po typech poškození odvodit „podíl poškozených prýtů" — bere se nejvyšší třída ze všech typů, nebo se má v terénu zaznamenávat celkový podíl?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-13 — Zdroj dat: verdikty DP vznikají i z nahodilých záznamů mimo monitoring
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY
- **Odkazy:** revize R-15; registr obojživelníků H-62, H-69
- **Metodika:** 2.4 — „Přítomnost druhu na DP, včetně posledního prokazatelného záznamu druhu, se zjišťuje ze všech záznamů NDOP; všechny ostatní indikátory se počítají výhradně ze záznamů projektu Monitoring druhů ČR."
- **Stav v kódu:** `21_1` počítá indikátory ze **všech** záznamů bez ohledu na projekt; [`24:219-228`](../../R/02_druhy/24_n2k_druhy_lokality.R#L219-L228) vybírá reprezentativní návštěvu s preferencí `CILMON`, ale nemá-li DP žádný monitoring, vezme nahodilý záznam. Omezení na `CILMON == 1` existuje jen pro podíl DP na úrovni EVL (`CELKOVE_CILMON`, H-69 obojživelníků).
- **Důsledek (plný běh):** **280 ze 740 DP** má verdikt postavený na záznamu **mimo monitoring** (mapování biotopů, mobilní sběr, floristické nálezy …). U **všech 280** je jediným hodnoceným klíčovým indikátorem `POP_TREND` (z prázdných dat, H-03): **264 vychází „dobrý“, 16 „špatný“**. Do podílu DP na úrovni EVL nevstupují (díky `CELKOVE_CILMON`), ve výstupu DP ale figurují.
- **Dotčené druhy:** *Aconitum*; všechny rostliny.
- **Dotčené skupiny ve sdíleném kódu:** **obojživelníci, ryby, hmyz, savci, rostliny** — u obojživelníků je otázka cíleného monitoringu na úrovni DP otevřená (H-62) a rozhodnutí R-15 platí jen pro rostliny ⇒ náprava **podmíněná skupinou**.
- **Návrh řešení:** pro rostliny počítat indikátory DP (kromě přítomnosti) jen ze záznamů monitoringu; DP jen s nahodilými záznamy vést jako „přítomnost potvrzena, nehodnoceno".
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Střední závažnost

### H-14 — `ZDROJ %in% CIS_CILMON` porovnává s datovým rámcem — větev nikdy nesedne
- **Závažnost:** střední · **Typ:** BUG
- **Odkazy:** P-02
- **Stav v kódu:** [`02_n2k_data_druhy.R:249`](../../R/00_config/02_n2k_data_druhy.R#L249) — `CIS_CILMON` je tabulka (sloupec `ZDROJ_NDOP`, 32 zdrojů), `%in%` proti tabulce porovnává s celými sloupci, takže podmínka je vždy nepravdivá.
- **Důsledek:** u rostlin bez dopadu — `CILMON` se plně odvodí z `PROJEKT == "Monitoring druhů ČR"` (P-02). U skupin, jejichž cílený monitoring je rozpoznatelný jen podle `ZDROJ` (např. „Monitoring totenových modrásků" v číselníku), se cílený monitoring nepozná — **neověřeno, mimo rozsah**.
- **Dotčené skupiny ve sdíleném kódu:** **všechny**; oprava (`ZDROJ %in% CIS_CILMON$ZDROJ_NDOP`) mění `CILMON` u skupin mimo rostliny ⇒ změřit dopad před nasazením.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-15 — Zaniklé DP: pravidlo deseti let není implementováno; „poslední nález" počítá i negativní záznamy
- **Závažnost:** střední · **Typ:** GAP (+ BUG)
- **Odkazy:** seed **K-10**; revize R-32
- **Metodika:** 3.3 — „Zaniklé DP … jsou do celkového hodnocení na úrovni EVL zpravidla započítávány po dobu deseti let od posledního prokazatelného záznamu druhu (2.4). O započítávání delší dobu … rozhoduje regionální garant s potvrzením koordinátora monitoringu."
- **Stav v kódu:** [`23_n2k_posledni_nalez.R`](../../R/02_druhy/23_n2k_posledni_nalez.R) počítá `POSLEDNI_NALEZ = max(DATUM)` přes **všechny** záznamy včetně negativních (BUG proti „prokazatelnému záznamu"); výsledek se předává do `25` jako `n2k_druhy_posledni_chu`, ale **nepoužije se**. Jedinou časovou hranicí je 13leté okno (H-09). Rozhodnutí garanta o prodloužení nemá v datech kde být.
- **Důsledek:** DP s posledním pozitivním záznamem před 11–12 lety zůstává v hodnocení, DP s negativními záznamy nikdy „nezanikne"; nelze vyhodnotit automaticky.
- **Dotčené skupiny ve sdíleném kódu:** **všechny** (`23` je společný; použití ve `25` by bylo nové) ⇒ podmínit skupinou.
- **Návrh řešení:** „poslední prokazatelný záznam" = max datum záznamu s přítomností (všechny projekty, R-15); zaniklou DP započítávat 10 let od něj; výjimky vést ve vendorovaném seznamu (DP, do kdy, kdo rozhodl).
- **Otevřená otázka:** Kde a v jaké podobě budou evidována rozhodnutí regionálního garanta o prodloužení?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-16 — Export do ISOP: žádný indikátor rostlin nemá `ind_id`
- **Závažnost:** střední · **Typ:** STOPA-DO-ISOP
- **Odkazy:** revize R-04, R-07
- **Stav v datech:** `cis_indikatory_popis.csv` (48 řádků) nemá řádek pro `POP_POCET`, `POP_TREND`, `POP_VITAL`, `POP_POSKPERC`, `POP_POSKPOC`, `POP_POCETSUM`, `STA_POKRYVNOSTE1`, `STA_POKRYVNOSTE2E3`, `STA_POKRYVNOSTINVAZNI`, `STA_POKRYVNOSTEXPANZNI`, `STA_HYDROPOMERY` ani `POP_PROCDOB`. Má jen `LOK_PROCDOBR` (189), `LOK_DILCDOBRE` (130), `LOK_DILCPOCET` (131), `CELKOVE_HODNOCENI` (10). Kód **80 „vývoj početnosti populace"** patří `POP_ZMENARAD` (obojživelníci) — stejný název jako dosavadní indikátor rostlin. Kód 190 „počet jedinců (klouzavý průměr za poslední 3 roky)" patří indikátoru obojživelníků s jinou definicí, než má dlouhodobý trend rostlin.
- **Stav v kódu:** [`27:759`](../../R/02_druhy/27_n2k_druhy_zapis.R#L759) napojuje `ind_r → ind_id`, [`:781`](../../R/02_druhy/27_n2k_druhy_zapis.R#L781) bez shody propouští surový název.
- **Důsledek:** export rostlin by do ISOP šel pod názvy proměnných R. Nové indikátory harmonizovaného znění (velikost populace, krátkodobý trend, dlouhodobý trend) kódy nemají.
- **Dotčené skupiny ve sdíleném kódu:** žádné.
- **Návrh řešení:** požádat ISOP o `ind_id` pro indikátory Přílohy 1A a 1B; do přidělení vést v číselníku řádky bez `ind_id` (vzor H-04/H-14 obojživelníků).
- **Otevřená otázka:** Má krátkodobý trend rostlin sdílet kód 80 s „vývojem početnosti" obojživelníků, nebo dostat vlastní?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-17 — `limity_cevky.csv`: sloupec `pozn`, typ `info` a prázdný `TYP_IND`
- **Závažnost:** střední · **Typ:** NÁZVOSLOVÍ
- **Odkazy:** seed **K-08** (potvrzen, dopad nízký)
- **Stav v kódu/datech:** [`00_n2k_config.R:78-126`](../../R/00_config/00_n2k_config.R#L78-L126) spojuje limity přes `bind_rows()` — `pozn` zůstane jako sloupec (u ostatních souborů `NA`), nikde se nepoužije. Řádky `info` mají `LIM_IND = NA`, a proto do čítačů v `24` nevstupují ✓. U `POP_VITAL` stojí vedle sebe `info` (`KLIC = NA`) a `min` (`KLIC = ano`): `right_join` v `21_2` je many-to-many (varování v běhu) a `24` bere `KLIC = first(KLIC)` — v běhu má `POP_VITAL` na 650 DP `KLIC = NA` (vždy se `STAV_IND = NA`, takže bez vlivu). Config hlásí *Cypripedium calceolus* `POP_POCETVITAL` s limitem bez `TYP_IND` (H-53 obojživelníků).
- **Důsledek:** dnes bez vlivu na verdikt; při nápravě H-05 hrozí, že `KLIC` převezme hodnotu z řádku `info`.
- **Dotčené skupiny ve sdíleném kódu:** žádné (úprava dat limitů rostlin; mechanismus `info` je sdílený a beze změny).
- **Návrh řešení:** u indikátoru s hodnoceným limitem nevést souběžný řádek `info`; `pozn` buď vypustit, nebo z něj udělat dokumentační sloupec ve všech souborech limitů.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Nízká závažnost

### H-18 — Filtr předmětů ochrany není párový
- **Závažnost:** nízká · **Typ:** BUG
- **Odkazy:** — (nález auditu; souvisí s H-40 registru obojživelníků)
- **Stav v kódu:** [`21_1:385-388`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L385-L388) a [`27:174-177`](../../R/02_druhy/27_n2k_druhy_zapis.R#L174-L177) — `DRUH %in% nazev_lat & kod_chu %in% site_code` testuje druh a EVL **nezávisle**; projde záznam druhu z EVL, kde druh předmětem ochrany není, je-li tam předmětem jiný druh.
- **Důsledek:** u *Aconitum* bez vlivu (všech 2 251 záznamů leží v jediné EVL, kde je předmětem ochrany). U druhů s nálezy i mimo „své" EVL vznikají hodnocení EVL, které metodika nepředpokládá — **neověřeno**.
- **Dotčené skupiny ve sdíleném kódu:** **všechny** ⇒ změřit dopad.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-19 — Osiřelé sloupce a konstanta
- **Závažnost:** nízká · **Typ:** BUG
- **Stav v kódu:** `POP_POCETPRUM = 50` ([`21_1:1287`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1287)) se nikde nepoužije; `POP_TRENDLM`, `POP_POCETMAXREF`, `POP_TREND1/2` se u rostlin počítají bez limitu; `STA_HYDRPOMERY` viz H-10.
- **Odkazy:** H-03, H-10
- **Důsledek:** zavádějící hodnoty ve fázi 1; na verdikt bez vlivu.
- **Dotčené skupiny ve sdíleném kódu:** rostliny (trendový blok se počítá jen u druhů s limitem `POP_TREND*`); `POP_POCETPRUM` se počítá pro všechny skupiny a nikde nepoužije.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Srovnávací výpočet — pravidla harmonizovaného znění na ostrých datech

Jednorázový skript mimo `R/` a mimo commit (zadání §5.2; zápis N-5 „Po výpočtu hodnocení
bude nastavení ještě posouzeno"; rozhodnutí R-20 „namodelovat všechny tři varianty").

**Postup.** Záznamy *Aconitum* z projektu Monitoring druhů ČR (R-15) v okně **2014–2025**
(R-18). Početnost akce = „prýty", jinak součet „sterilní" + „kvetoucí a plodné" /
„plodné prýty"; negativní záznam = 0. DP × sezóna: nejlepší hodnota (R-28). Hodnotí se
poslední sezóna každé DP: velikost populace (≥ 75 % průměru prvních tří sezón s počtem),
krátkodobý trend (≥ průměr tří předchozích sezón), vitalita (≥ 30 %); nulová početnost ⇒
vše nesplněno (R-21); neznámý klíčový ⇒ DP neznámá (R-27); Tabulka 1 podle R-26.
EVL: podíl DP v dobrém stavu s neznámými ve jmenovateli (R-30), dlouhodobý trend =
součet poslední početnosti DP se známým stavem proti cíli SDO 600 (≥ 75 %), pravidlo
50 % neznámé populace. Dvě varianty identity DP: **I0** jako dnes (H-01), **I1** se starými
názvy jednoznačně namapovanými na kód AFM.

| | dnešní kód (plný běh) | harmonizované znění, I0 | harmonizované znění, I1 |
|---|---|---|---|
| DP | 740 | 438 | 281 |
| dobrý | 389 | **0** | **5** |
| zhoršený | 4 | — (nevzniká) | — |
| špatný | 347 | 90 | 130 |
| neznámý | 0 | **348** | **146** |
| — „neznámý": velikost + krátkodobý trend | — | 178 | 104 |
| — „neznámý": velikost + krátkodobý trend + vitalita | — | 105 | 9 |
| — „neznámý": jen krátkodobý trend | — | 7 | 26 |
| — „neznámý": jen vitalita | — | 45 | 6 |
| — „neznámý": krátkodobý trend + vitalita | — | 13 | 1 |
| — příčina „špatný": nulová početnost | — | 90 | 80 |
| podíl DP v dobrém stavu | 27,2 % (bez neznámých) | 0 % | 1,8 % |
| dlouhodobý trend (% cíle SDO) | — (nehodnocen) | 0 % (DP se známým stavem mají nulovou početnost) | **1 587 %** (9 524 prýtů / 600) |
| podíl populace v neznámých DP | — | 100 % | 56,1 % |
| **stav EVL** | **špatný** | **neznámý** | **neznámý** |

**Krátkodobý trend — čtyři pravidla** (I1, DP se třemi a více sezónami s početností):

| pravidlo | splněno | nesplněno | neznámé | shoda s R-07 |
|---|---|---|---|---|
| **R-07** (sezóna ≥ průměr tří předchozích) — závazné | 13 | 65 | 27 | — |
| (A) obě poslední hodnoty ≥ první | 23 | 82 | 0 | 65 / 78 |
| (B) nesplněno, jen jsou-li obě nižší | 57 | 48 | 0 | 45 / 78 |
| (T) sklon regrese tří let ≥ 0 | 25 | 80 | 0 | 70 / 78 |

**Závěry pro posouzení nastavení (N-5, R-20):**

1. Dnešní kód dává ***Aconitum* převážně „dobrý" DP, které jsou artefaktem** (H-03, H-13);
   pravidla harmonizovaného znění na týchž datech dávají **téměř žádnou dobrou DP**.
2. Stav „neznámý" je dominantní a vzniká kombinací R-27 (jediný neznámý klíčový ⇒ DP
   neznámá) s požadavkem tří sezón u velikosti populace i trendu a s 12letým oknem
   (R-18, R-20) — 160 z 256 AFM ploch má od roku 2021 jedinou sezónu.
3. Bez mapování identity DP (H-01) je hodnocení EVL nemožné (100 % populace v neznámých
   DP); s mapováním zůstává neznámé (56 %).
4. Dlouhodobý trend je při cíli 600 „jedinců" vůči ~9 500 prýtům splněn vždy (H-08).
5. Krátkodobý trend podle R-07 je nejpřísnější ze čtyř pravidel; nejblíže mu je regresní
   varianta (T), nejvzdálenější (B).

---

# Matice pokrytí — *Aconitum firmum* subsp. *moravicum*

`OK` = shoda · `H-nn` = nález · `N/A` = na této zastávce se indikátor nevyskytuje.
Průřezově se všech řádků týkají **H-01** (identita DP), **H-09** (okno) a **H-13** (zdroj dat).

| Indikátor (harmonizované znění) | ID v kódu | metodika | číselník | limity | `21_1` | `24` | `25` | `27` |
|---|---|---|---|---|---|---|---|---|
| velikost populace (1A, klíčový) | `POP_POCET` | OK | H-16 | H-04 | H-02, H-04 | H-06 | H-07 | H-16 |
| krátkodobý trend populace (1A, klíčový) | `POP_TREND` | OK | H-16 | H-03 | H-03 | H-06 | N/A | H-16 |
| vitalita populace (1A, klíčový) | `POP_VITAL` | OK | H-16 | H-05, H-17 | H-05 | H-06 | N/A | H-16 |
| poškození rostlin (1A, neklíčový) | `POP_POSKPERC`, `POP_POSKPOC` | H-12 | H-16 | H-12 | H-12 | H-06 | N/A | H-16 |
| hydrologické poměry (1A) | `STA_HYDROPOMERY` / `STA_HYDRPOMERY` | OK | H-16 | H-10 | H-10 | H-06 | N/A | H-16 |
| pokryvnost E1 (1A) | `STA_POKRYVNOSTE1` | OK | H-16 | OK | H-11 | H-06 | N/A | H-16 |
| pokryvnost dřevin > 1 m (1A) | `STA_POKRYVNOSTE2E3` | OK | H-16 | OK | OK | H-06 | N/A | H-16 |
| pokryvnost invazních druhů (1A) | `STA_POKRYVNOSTINVAZNI` | OK | H-16 | OK | OK | H-06 | N/A | H-16 |
| pokryvnost expanzivních druhů (1A) | `STA_POKRYVNOSTEXPANZNI` | R-24 (seznam) | H-16 | OK | H-11 | H-06 | N/A | H-16 |
| podíl DP v dobrém stavu (1B) | `LOK_PROCDOBR` | OK | OK (189) | OK | N/A | H-06, H-13 | H-07 | OK |
| dlouhodobý trend populace (1B) | — (neexistuje) | R-31 | H-16 | H-07 | N/A | N/A | H-07, H-08 | H-16 |
| *(navíc, mimo 1B)* % populace v DP v dobrém stavu | `POP_PROCDOB` | N/A (vypuštěno R-09) | H-16 | H-07 | N/A | N/A | H-07 | H-16 |
| Tabulka 1 (DP) | `24` | OK | N/A | N/A | N/A | H-06 | N/A | OK |
| Tabulka 2 (EVL) | `25` | OK | N/A | H-07 | N/A | N/A | H-07 | OK |
| zaniklé a nové DP | `23`, `25` | OK | N/A | N/A | N/A | N/A | H-15 | N/A |

**Čisté jsou jen dva indikátory** (dřeviny > 1 m, invazní druhy) a to jen do úrovně
extrakce — na úrovni DP je zasahuje H-06.

---

# Sdílená logika cévnatých rostlin

Nálezy, které se netýkají jen *Aconitum* (druhy podle `limity_cevky.csv`; mimo
*Aconitum* **jednotlivě neověřeno**):

| Nález | Dotčené druhy |
|---|---|
| H-03 `POP_TREND` | **všech 34**; 33× `max 1` (obrácená logika), 1× `min 1` |
| H-06 Tabulka 1 | všech 34 |
| H-07 úroveň EVL (`LOK_PROCDOBR` + `POP_PROCDOB`) | všech 34 |
| H-01 identita DP (větev rostlin) | všechny rostliny s monitoringem před zavedením kódů DP |
| H-12 poškození | 33 (`POP_POSKPERC`), 31 (`POP_POSKPOC`) |
| H-11 třídy s mezerami | stejné tagy u 31 (expanzivní), 30 (invazní), 13 (E1), 8 (E2E3) druhů |
| H-02 tvar jednotky „počet …" | 16 řádků `POP_POCET` u dalších druhů („počet trsů" 6×, „počet jedinců" 2× …) |
| H-05 vitalita bez `POP_POCETVITAL` | *Aconitum*, *Dracocephalum austriacum* |
| H-10 hydrologické poměry | *Aconitum*, *Ligularia sibirica*, *Liparis loeselii* |
| H-08 rozbité jméno v SDO | *Aconitum* (4109), *Iris humilis* subsp. *arenaria* (4098, 14 řádků), 4094, 4073, 4088 |

---

# Limity bez normativního zdroje — 33 dalších druhů

Pro tyto druhy metodika zatím neexistuje (rozhodnutí R-17: zatím jen *Aconitum*).
Neaudituje se jednotlivě; souhrn pro budoucí druhové listy:

| Ukazatel | Hodnota |
|---|---|
| řádků limitů mimo *Aconitum* | **568** z 594 |
| druhů | 33 |
| `ID_IND` bez výpočtu v kódu (sirotci) | **11**: `POP_POCETSPEC`, `POP_POCETSUMLOD`, `POP_POPULACE`, `POP_REPROUSP`, `STA_HYDROPOMERY`, `STA_POC_OST_NATASUB_DRH`, `STA_POKLESHLAD`, `STA_POKRDNO`, `STA_POKRPUDA`, `STA_POKRRAKOS`, `STA_POKRVOLPUD` |
| indikátory mimo strukturu Přílohy 1A | `STA_POKRSTAR` (25 druhů), `STA_POKRDREV` (18), `STA_STUPENZACH`, `STA_POKRYVNOSTE0`, `STA_POKRDREVNIZ`, `STA_ZASTINENICELK`, `STA_ZAPOJENICELK`, `STA_VLHPOM`, `POP_PLOCHA`, `POP_POCETNOST`, `POP_REPROUSP` |
| úroveň EVL | všech 33: `LOK_PROCDOBR` + `POP_PROCDOB` (`min 50`) — stejná dvojice jako u *Aconitum*, tedy mimo rozhodnutí R-09 |
| názvy druhů vůči seznamu předmětolokalit | 6 druhů v limitech pod jiným jménem (*Adenophora liliifolia*, *Echium maculatum*, *Gentianella praecox* subsp. *bohemica*, *Iris humilis* subsp. *arenaria*, *Stipa zalesskii*, *Trichomanes speciosum*); *Artemisia pancicii* a *Crambe tataria* jsou předměty ochrany bez limitů (revize R-11) — tatáž třída jako H-40 ryb |

---

# Otázky na autory metodiky a zadavatele

| # | Otázka | Nález |
|---|---|---|
| 1 | Kdo potvrdí převodní tabulku starých názvů lokalit na kódy AFM (200 jednoznačných, 2 nejednoznačné, 51 bez protějšku) a má být v NDOP, nebo jako číselník v repozitáři? | H-01 |
| 2 | Má se u *Aconitum* porovnávat celková početnost s cílem 600 „jedinců" z digitalizace SDO, když text SDO uvádí 30–35 000 prýtů a indikátor pak vychází vždy splněn? | H-08 |
| 3 | Jak z tříd po typech poškození odvodit „podíl poškozených prýtů" — nejvyšší třída ze všech typů, nebo zaznamenávat celkový podíl? | H-12 |
| 4 | Kde budou evidována rozhodnutí regionálního garanta o započítávání zaniklé DP déle než 10 let? | H-15 |
| 5 | Má krátkodobý trend rostlin sdílet ISOP kód 80 s „vývojem početnosti" obojživelníků, nebo dostat vlastní? Kdo požádá o kódy pro velikost populace a dlouhodobý trend? | H-16 |
| 6 | Srovnávací výpočet dává při pravidlech R-18 + R-20 + R-27 většinu DP v neznámém stavu a EVL neznámou. Platí nastavení, nebo se má přehodnotit (např. velikost populace a krátkodobý trend nevyžadovat u DP s méně než třemi sezónami)? | §Srovnávací výpočet |
| 7 | Revize R-19: kdo stanoví arbitrární počáteční početnost; postup při počáteční početnosti 0. | H-04 |
| 8 | Revize R-31: potvrzení součtu celkové početnosti (poslední monitoring každé DP v 12letém okně); hodnocení EVL bez cílového stavu. | H-07 |
| 9 | Revize R-24: seznam expanzivních druhů. | H-11 |

---

# Diagramy k aktualizaci

`Diagrams/hodnoceni_urovne.drawio` je koncepční a logiku úrovní nezobrazuje — beze změny.
Stromy `strom_obojzivelnici*.drawio` platí jen pro obojživelníky; pro rostliny je po
implementaci vhodné vytvořit **nový** rozhodovací strom (Tabulka 1 bez stanovištních
indikátorů a se stavem „neznámý"; Tabulka 2 jako u obojživelníků s jmenovatelem včetně
neznámých DP).

---

# Pokrytí seedů zadání

| Seed | Nález | Výsledek |
|---|---|---|
| K-01 | H-03 | **potvrzeno a rozšířeno** — obrácená logika, „bez dat = splněno", jedna hodnota na DP bez roku, reference bez cíleného monitoringu, neshoda s novou definicí |
| K-02 | H-02, H-04 | **potvrzeno** — absolutní `min 50`, počáteční početnost se nepočítá; navíc jednotka „počet prýtů" vyřazuje všechny počty 2013–2020 |
| K-03 | H-07, H-08 | **upraveno** — blok cílového stavu je omezen na obojživelníky (potvrzeno), ale *Aconitum* ve snímku **je** (sdf 4109, rozbitý název); jednotka 600 „jedinců" vs. prýty; ověření upstreamu `digitalizaceSDO` neprovedeno (nic se nestahovalo) |
| K-04 | H-12 | **potvrzeno** — součet kategorií proti „procentům"; `POP_POSKPOC` bez opory; nerozpoznané tvary s mezerami |
| K-05 | H-06 | **potvrzeno** — DP: 389 dobrý / 4 zhoršený / 347 špatný / 0 neznámý |
| K-06 | P-01 | **vyvráceno jako rozpor** — po rozhodnutí R-28 agregace kódu metodice odpovídá |
| K-07 | H-06, H-07 | **potvrzeno** — na DP se neznámý klíčový přeskakuje; pravidlo 50 % neznámé populace v EVL chybí |
| K-08 | H-17 | **potvrzeno**, dopad nízký |
| K-09 | H-11 | **upraveno** — pásma limitů odpovídají třídám; kolizi působí tvary s mezerami (105 chybných nesplnění) |
| K-10 | H-15 | **potvrzeno** — pravidlo 10 let neimplementováno; „poslední nález" počítá i negativní záznamy |
| K-11 | H-01 | **potvrzeno a rozšířeno** — 484 pseudo-DP; tatáž plocha před a po roce 2021 pod dvěma identitami |

# Co bylo ověřeno proti ostrým datům a co jen proti kódu

| Ověřeno proti ostrým datům a plnému běhu | Jen proti kódu / limitům |
|---|---|
| H-01 … H-08, H-10 … H-13, H-15 (část „poslední nález"), H-17, H-18 (u *Aconitum*) | H-09 (dopad na jiné druhy), H-14 (dopad na jiné skupiny), H-16, H-19; všech 33 dalších druhů rostlin |

Neověřeno: terénní metodika Popelářová et al. (2022) — není v repozitáři; upstream
`BiodivMonCZ/digitalizaceSDO` — zadání zakazuje cokoli stahovat; dopad H-09, H-14, H-18
mimo rostliny.
