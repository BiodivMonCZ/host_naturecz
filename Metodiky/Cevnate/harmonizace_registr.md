# Registr nálezů — audit kódu a dat cévnatých rostlin proti harmonizované metodice (Fáze B)

**Zadání:** [harmonizace_cevnate_prompt.md](harmonizace_cevnate_prompt.md), §5.
**Měřítko:** [met_cevnate_2026_harmonizovana.md](met_cevnate_2026_harmonizovana.md) po rozhodnutí
zadavatele 2026-10-02 (viz [revize_metodika_cevnate.md](revize_metodika_cevnate.md),
§Rozhodnutí zadavatele — souhrn). Body, které zůstaly otevřené (R-19, R-24, R-31,
R-34), jsou auditovány proti dnešnímu znění a varianty jsou uvedeny u nálezu.
**Datum:** 2026-10-02 · **Větev:** `harmonizace-cevnate` · **Stav kódu:** commit `c8712d7`
(beze změny `R/` i `Data/Input/` od `druhy-202609`).
**Doplněno 2026-10-02:** odpovědi zadavatele k otázkám registru, nález H-20, rozbor příčin srovnávacího výpočtu.
**Doplněno 2026-10-28:** [Fáze C](#fáze-c--metodika-2026-10-21-převod-limitů-a-audit-kódu) — metodika 2026-10-21: převod limitů, matice pravidel PR-01 až PR-27, nálezy H-21 až H-37, odpovědi na komentáře autora (na konci dokumentu).

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
| **Střední** — konzistence, dohledatelnost, export | 5 | H-14 … H-17, H-20 |
| **Nízká** | 2 | H-18, H-19 |
| **Potvrzeno — bez akce** | 2 | P-01, P-02 |

| Typ | Počet | Nálezy |
|---|---|---|
| BUG | 11 | H-01, H-02, H-03, H-05, H-09, H-10, H-11, H-14, H-18, H-19, H-20 |
| ROZPOR-METODIKY | 5 | H-04, H-06, H-07, H-12, H-13 |
| GAP | 2 | H-08, H-15 |
| STOPA-DO-ISOP | 1 | H-16 |
| NÁZVOSLOVÍ | 1 | H-17 |

(H-15 je veden jako GAP; část „poslední nález počítá i negativní záznamy" je BUG.)

**Dotčení sdíleného kódu:** H-06, H-07, H-09, H-13, H-14, H-15, H-18, H-20 se týkají kódu,
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
- **Rozhodnutí zadavatele:** ⏸ (2026-10-02) Převodní tabulku starých názvů na kódy AFM potvrdí **centrální koordinátor monitoringu**. Otevřené: forma (oprava v NDOP, nebo vendorovaný číselník).

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
- **Rozhodnutí zadavatele:** ⚠ (2026-10-02) **Ponechat** navrženou hodnotu z digitalizace SDO (600 „jedinců“) a nesoulad jednotek **vyznačit** v metodice i ve výstupu (vzor S-4 / H-06 obojživelníků). Do revize cílových stavů je dlouhodobý trend u *Aconitum* fakticky vždy splněn.

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
- **Rozhodnutí zadavatele:** ⏸ (2026-10-02) Navrhnout výpočet přes **střední hodnoty tříd** (obdoba dolní mez / horní mez / střed u kategorií početnosti obojživelníků) — návrh a jeho dopad viz níže; čeká na potvrzení.
- **Návrh výpočtu (2026-10-02, k potvrzení):**
  1. **Číselník tříd poškození** (obdoba číselníku kategorií početnosti — dolní mez / horní mez / střed):

     | třída v datech | dolní mez | horní mez | střed |
     |---|---|---|---|
     | Nezjištěno, Bez poškození | 0 | 0 | 0 |
     | 1-10 % | 1 | 10 | 5,5 |
     | 10-50 % | 10 | 50 | 30 |
     | 11-50 % | 11 | 50 | 30,5 |
     | 50-100 % | 50 | 100 | 75 |
     | 51-100 % | 51 | 100 | 75,5 |

     Mezery kolem pomlčky („11 - 50 %") se před převodem normalizují.
  2. **Podíl poškozených prýtů za návštěvu = nejvyšší střed ze všech zaznamenaných typů poškození.** Různé typy poškození postihují z velké části tytéž prýty, takže podíl poškozených prýtů je nejméně roven podílu u nejrozšířenějšího typu; součet přes typy by poškození nadhodnotil. Pro dohledatelnost se vedou i meze: dolní = nejvyšší dolní mez, horní = součet horních mezí (nejvýše 100 %).
  3. Limit ≤ 50 % se porovnává se středem; v DP × rok rozhoduje nejhorší návštěva (R-28).
- **Dopad návrhu (ostrá data 2014–2025, 262 návštěv se záznamem poškození):** limit splňuje **205** návštěv podle středu (podle dolní meze 252, podle horní 153). Typ poškození je uveden u 317 položek (nejčastěji padlí 111, hmyz 93, sešlap 32, okus 18); 179 položek je bez typu (starší tvar). Nerozpoznány zůstávají 4 slovní položky („pastva", „sesuv" …) → `NA`.

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
- **Rozhodnutí zadavatele:** ⚠ (2026-10-02) Rozhodnutí o prodloužení se **evidovat nebude**. Metodika to popisuje; automatické hodnocení proto započítává zaniklou DP právě 10 let od posledního prokazatelného záznamu a případné prodloužení se v něm neprojeví — vyznačeno v textu.

### H-16 — Export do ISOP: žádný indikátor rostlin nemá `ind_id`
- **Závažnost:** střední · **Typ:** STOPA-DO-ISOP
- **Odkazy:** revize R-04, R-07
- **Stav v datech:** `cis_indikatory_popis.csv` (48 řádků) nemá řádek pro `POP_POCET`, `POP_TREND`, `POP_VITAL`, `POP_POSKPERC`, `POP_POSKPOC`, `POP_POCETSUM`, `STA_POKRYVNOSTE1`, `STA_POKRYVNOSTE2E3`, `STA_POKRYVNOSTINVAZNI`, `STA_POKRYVNOSTEXPANZNI`, `STA_HYDROPOMERY` ani `POP_PROCDOB`. Má jen `LOK_PROCDOBR` (189), `LOK_DILCDOBRE` (130), `LOK_DILCPOCET` (131), `CELKOVE_HODNOCENI` (10). Kód **80 „vývoj početnosti populace"** patří `POP_ZMENARAD` (obojživelníci) — stejný název jako dosavadní indikátor rostlin. Kód 190 „počet jedinců (klouzavý průměr za poslední 3 roky)" patří indikátoru obojživelníků s jinou definicí, než má dlouhodobý trend rostlin.
- **Stav v kódu:** [`27:759`](../../R/02_druhy/27_n2k_druhy_zapis.R#L759) napojuje `ind_r → ind_id`, [`:781`](../../R/02_druhy/27_n2k_druhy_zapis.R#L781) bez shody propouští surový název.
- **Důsledek:** export rostlin by do ISOP šel pod názvy proměnných R. Nové indikátory harmonizovaného znění (velikost populace, krátkodobý trend, dlouhodobý trend) kódy nemají.
- **Dotčené skupiny ve sdíleném kódu:** žádné.
- **Návrh řešení:** požádat ISOP o `ind_id` pro indikátory Přílohy 1A a 1B; do přidělení vést v číselníku řádky bez `ind_id` (vzor H-04/H-14 obojživelníků).
- **Otevřená otázka:** Má krátkodobý trend rostlin sdílet kód 80 s „vývojem početnosti" obojživelníků, nebo dostat vlastní?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Krátkodobý trend rostlin **sdílí kód 80**, je-li to v ISOP možné. Kódy pro velikost populace a dlouhodobý trend vyžádat.

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

# Nálezy z rozboru srovnávacího výpočtu (2026-10-02)

### H-20 — Negativní příznak u dílčího záznamu se čte jako nepřítomnost druhu
- **Závažnost:** střední · **Typ:** BUG
- **Odkazy:** revize R-21; §Srovnávací výpočet
- **Metodika:** 3.1.3 — „Byla-li při monitoringu zjištěna nulová početnost druhu, jsou všechny indikátory stavu populace … hodnoceny jako nesplněné"; nulová je **početnost druhu**, ne jedné kategorie prýtů.
- **Stav v datech:** od roku 2021 se návštěva zapisuje jako několik záznamů po kategoriích prýtů. Je-li některá kategorie nulová (např. žádné kvetoucí prýty), nese její záznam `NEGATIVNI = 1`, ačkoli jiný záznam téže návštěvy rostliny má (příklad AFM012A, 2021: 60 sterilních prýtů + negativní záznam „plodné prýty"). **36 návštěv, 84 záznamů** (z toho 56 „kvetoucí a plodné prýty", 16 „sterilní prýty", 10 „plodné prýty").
- **Stav v kódu:** [`21_1:404-409`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L404-L409) — `POP_PRESENCE_N = 0`, je-li `NEGATIVNI == 1`, **po záznamech**; tyto záznamy pak dostanou `POP_POCET = 0` a `POP_VITAL = 0` (nepřítomnost).
- **Důsledek:** v plném běhu se na verdiktu DP **neprojevil** (0 z 90 nesplněných vitalit na DP pochází z takové návštěvy), protože na DP rozhoduje nejlepší hodnota. Ve výstupu na úrovni nálezů ale návštěva s rostlinami vystupuje i jako „nepřítomnost". První verze srovnávacího výpočtu tuto chybu převzala a vykazovala 110 místo 80 DP s nulovou početností — opraveno.
- **Dotčené skupiny ve sdíleném kódu:** **všechny** — vzor „jedna návštěva = více záznamů po kategoriích" mají i obojživelníci (dospělci / larvy / snůšky); neověřeno.
- **Návrh řešení:** nepřítomnost druhu odvozovat **za návštěvu** (všechny záznamy návštěvy negativní nebo s počtem 0); negativní dílčí záznam brát jako nulu jen pro svou kategorii.
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


## Rozbor příčin (doplněno 2026-10-02)

Zadavatel: *„Výsledek je velmi zvláštní — v dobrém stavu by mělo být víc DP. Je to dáno
hlavně chybějícími delšími časovými řadami?"* Výpočet byl zopakován s opravou dvou chyb
srovnávacího skriptu (součet samých chybějících hodnot dával 0; negativní dílčí záznam
se četl jako nepřítomnost — H-20). Varianta I1, okno 2014–2025:

| Příčina stavu DP | min. 3 sezóny | min. 2 sezóny („3 je ideál, 2 přípustné") |
|---|---|---|
| neznámý — krátká časová řada (velikost populace nebo krátkodobý trend) | **140** | **113** |
| neznámý — chybí vitalita (poslední sezóna před rokem 2021) | 6 | 7 |
| špatný — při poslední návštěvě nenalezen žádný prýt | **80** | **80** |
| špatný — velikost populace + krátkodobý trend | 22 | 29 |
| špatný — velikost + krátkodobý + vitalita | 16 | 25 |
| špatný — jiná kombinace | 12 | 19 |
| **dobrý** | **5** | **8** |
| celkem | 281 | 281 |
| podíl DP v dobrém stavu | 1,8 % | 2,8 % |
| podíl populace v neznámých DP | 56,1 % | 47,2 % |
| stav EVL | neznámý | neznámý (pod hranicí 50 % → podle Tabulky 2 by byl **zhoršený**: podíl DP nesplněn, dlouhodobý trend splněn) |

**Odpověď: krátké časové řady vysvětlují stav „neznámý" (asi polovinu DP), ale málo
„dobrých" DP mají na svědomí tři další, na délce řady nezávislé příčiny:**

1. **Skutečné nepřítomnosti.** U 80 DP (28 %) nebyl při poslední návštěvě nalezen ani
   jeden prýt (celá návštěva negativní). Řady tomu odpovídají — např. AFM015: 600 → 275 →
   95 → 30 (2010–2013) → 0 (2022). Zda jde o vymizení, nebo o přehlédnutí, z dat
   nepoznáme; metodika (R-21) je hodnotí jako špatný stav.
2. **Skok mezi obdobími záznamu.** U 121 DP s počty před i po roce 2021 je medián poměru
   „početnost od 2021 / do 2020" **0,70** a **56 % DP je pod 75 %**. Počáteční početnost se
   počítá ze starého období („prýty"), aktuální stav z nového („sterilní" + „kvetoucí
   a plodné"). Velikost populace proto selhává u většiny DP s úplnými daty (splněna jen
   u 15 z 55). Je-li změna způsobena změnou postupu sčítání, je to artefakt; je-li
   skutečná, jde o plošný pokles — **z dat to rozlišit nelze**.
3. **Přísnost kombinace tří indikátorů.** Krátkodobý trend („sezóna ≥ průměr tří
   předchozích") u kolísající, ale stabilní populace selže zhruba v polovině případů už
   náhodou; u DP s úplnými daty je splněn jen u 11 z 55. Vitalita (≥ 30 %) je splněna
   u 32 z 55 (medián vitality 36 %, pod 30 % u 74 ze 185 DP s prýty). Dobrý stav vyžaduje
   splnění všech tří bez tolerance — při nezávislosti by i u stabilní populace vyšel
   nejvýše asi u třetiny DP (≈ 0,5 za trend × 0,6 za vitalitu), při poklesu (bod 2) ještě méně.

**Povolení dvou sezón** zvýší počet hodnocených DP (neznámých 146 → 120), ale nově
hodnocené DP padají převážně do stavu „špatný" (130 → 153) — z důvodů 2 a 3.

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


## Odpovědi zadavatele (2026-10-02)

| # | Odpověď | Promítnuto |
|---|---|---|
| 1 | Převodní tabulku potvrdí **centrální koordinátor monitoringu**. | H-01 |
| 2 | Cíl 600 z digitalizace SDO **zatím ponechat a vyznačit**. | H-08; metodika 3.1.4 |
| 3 | **Navrhnout** výpočet přes střední hodnoty tříd (jako min / max / střed u obojživelníků). | H-12 (návrh); metodika 2.1 |
| 4 | Rozhodnutí o prodloužení se **evidovat nebude** — popsat a vyznačit. | H-15; metodika 3.3 |
| 5 | Krátkodobý trend **sdílí kód 80**, je-li to možné. | H-16 |
| 6 | **Tři sezóny jsou ideál, dvě se připouštějí.** | metodika 2.1, 3.1.4; §Rozbor příčin |
| 7 | Arbitrární počáteční početnost stanoví **samostatný skript pro všechny skupiny a všechny DP: maximální početnost kdy zaznamenaná pro druh na DP** (mimo toto zadání). Celková početnost EVL = **součet** za DP, porovnaný s cílem z digitalizace SDO (potvrzeno). Expanzivní druhy = **Axmanová et al. (2024)**, Katalog expanzivních rostlin ČR. | revize R-19, R-24, R-31; metodika 2.1, 3.1.4 |

**Zbývá otevřené:** H-01 (forma převodní tabulky), H-12 (potvrzení návrhu), H-20,
počáteční početnost 0 (R-19), hodnocení EVL bez cílového stavu (R-31 — u *Aconitum*
nenastává) a regionální platnost katalogu expanzivních druhů (katalog hodnotí
expanzivitu po regionech a biotopech).

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

---

# Fáze C — metodika 2026-10-21: převod limitů a audit kódu

**Zadání:** [prompt_host_n2kcz_cevnate_review.md](prompt_host_n2kcz_cevnate_review.md).
**Měřítko:** autorský text metodiky verze 2026-10-21 (Popelka et al.) a [Tabulky fin.xlsx](Tabulky%20fin.xlsx);
kontrolní seznam pravidel PR-01 … PR-27 a nálezy ve zdrojích R-35 … R-52 jsou v
[revize_metodika_cevnate.md](revize_metodika_cevnate.md), část „Verze 2026-10-21“.
**Datum:** 2026-10-28 · **Větev:** `cevnate-metodika-2026` (z `harmonizace-cevnate`).
**Rozhodnutí zadavatele:** 2026-10-28, otázky 1–15 Fáze 0 (souhrn v revizi).

**Co se změnilo v kódu:** jen to, co zadavatel výslovně schválil — otázky 13 a 14 Fáze 0
(commit `677102a`: vitalita H-21, hodnocené období H-22) a otázka 7 Fáze C (commit `f51ade7`:
převod plošných jednotek, H-36). **Ostatní návrhy oprav níže nejsou aplikovány** (otázka 11 Fáze C:
„bude rozhodnuto“). Pipeline ani hodnocení se nespouštěly.

**Limity (doplněno 2026-10-28, 2. kolo):** převedené limity jsou od commitu `19602b0` přímo v živém
`Data/Input/limity_cevky.csv` (formát beze změny); samostatný `limity_cevky_2026.csv` zrušen — jediný
soubor limitů. **Dokud nejsou aplikovány H-23, H-25 a H-26, nedávají výsledky rostlin smysl** (viz
[Pořadí nasazení](#pořadí-nasazení)). Rozhodnutí 2. kola: [níže](#rozhodnutí-zadavatele-k-otázkám-fáze-c-2026-10-28).

## Jak byl audit proveden

| Krok | Zdroj | Poznámka |
|---|---|---|
| Normativní text | `.docx` 2026-10-21 → Markdown (pandoc z Quarta, `--track-changes=all`) | 23 otevřených komentářů, vložené úseky |
| Limity | `Tabulky fin.xlsx` přes `readxl` po buňkách, sloupce podle textu hlavičky | Přílohy 1–5 |
| Převod | `R/05_metodiky/54_cevnate_limity_2026.R` | reprodukovatelný, píše i protokol [limity_cevky_2026_kontrola.md](limity_cevky_2026_kontrola.md) a diff [limity_cevky_2026_diff.csv](limity_cevky_2026_diff.csv) |
| Kód | `00_n2k_config.R`, `02_n2k_data_druhy.R`, `21_1`, `21_2`, `22`, `23`, `24`, `25`, `27` | čtení kódu; čísla řádků ke commitu `677102a` |
| Data | neveřejný export NDOP (`export_data_evl.csv` + `export_data_zprap.csv`) | jen slovník hodnot, jednotky, kódy lokalit, pole NDOP; **nic se nehodnotilo** |
| Ověření úprav kódu | parse tří souborů + 17 jednotkových testů pomocných funkcí na syntetických datech (2. kolo: + 7 testů převodu ploch) | jednorázový skript mimo repozitář; pipeline nespuštěna (zadání to zakazuje) |

## Formát limitů — specifikace převodu

| Vlastnost | `limity_cevky.csv` (vzor) |
|---|---|
| Kódování / BOM | Windows-1250, bez BOM; čte [`00_n2k_config.R:62-65`](../../R/00_config/00_n2k_config.R#L62-L65) |
| Oddělovač / desetinná značka / konce řádků | `,` / `.` / CRLF |
| Uvozovky | jen kde je třeba (čárka v poli) |
| Sloupce | `DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN, pozn`; `limity_vse.csv` a `limity_ryby.csv` (`;`) mají 7 sloupců bez `pozn`; slučuje `bind_rows` ([`:78-86`](../../R/00_config/00_n2k_config.R#L78-L86)), `pozn` kód nepoužívá |
| Chybějící hodnota | v dosavadním souboru smíšeně `NA` (222×) a prázdná (116×); převod píše `NA`, `pozn` prázdný |
| `DRUH` | název z NDOP (napojení v `21_2`, seznam druhů = průnik limitů a NDOP v [`27:26-29`](../../R/02_druhy/27_n2k_druhy_zapis.R#L26-L29)) |
| `TYP_IND` | `min` = hodnota ≥ limit, `max` = hodnota ≤ limit ([`21_2:271-275`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L271-L275)); ostré nerovnosti formát nezná |
| Výčet přípustných kategorií | `val`, **jeden řádek na hodnotu**; shoda po `str_squish` ([`21_2:99-116`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L99-L116)) |
| Informativní bez limitu | `info` + `LIM_IND = NA` ([`21_2:179-183`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L179-L183)) |
| `KLIC` | `ano` / `ne` (počítají [`24:155-186`](../../R/02_druhy/24_n2k_druhy_lokality.R#L155-L186)) / `NA` u `info`; **nově `info`** = hodnotí se, do verdiktu nevstupuje (rozhodnutí 2026-10-28) |
| `JEDNOTKA` | zobrazení v `LIM_INDLIST`; **funkční** u `POP_POCET`, `POP_POCETSUM`, `POP_POCETVITAL`, nově `POP_POCETSTERIL` (shoda s `POCITANO` z NDOP) |
| `UROVEN` | `lok` / `chu` |
| Pravidlo zadavatele | jediný práh `min` a/nebo `max` na indikátor; jediná tabulka limitů |
| `cis_indikatory_popis.csv` | Windows-1250, `,`, 4 sloupce `ind_popis, ind_id, ind_r, ind_nadr`, smíšené konce řádků; `ind_r` = `ID_IND`; export [`27:763-789`](../../R/02_druhy/27_n2k_druhy_zapis.R#L763-L789) bere `ind_id`, jinak surové `ID_IND` |

## Převod limitů

### Příloha 2 → ID_IND

| Sloupec | `ID_IND` | Původ ID | Kódování | `KLIC` | `ind_id` |
|---|---|---|---|---|---|
| Úspěšnost reprodukce | `POP_REPROUSP` | existující (3 staré řádky `val`, bez kódu) | `val` `dostatečná` | ano | — |
| Délka absence druhu | `POP_DELKAABSENCE` | *nový* | `max 5` „let“ (z „< 6 let“, R-41) | ano | — |
| Minimální přípustná početnost | `POP_POCETMINPRIP` | *nový* | `min` 50 / 60 „% cílové početnosti“ | ano | — |
| Početnost | `POP_POCETCIL` | *nový* | `min` 75 / 80 „% cílové početnosti“ | ne | — |
| Vývoj početnosti | `POP_TREND` | existující (jen rostliny) | `val` `pozitivní` + `val` `setrvalý` | ne | 80 (H-16) |
| Vitalita | `POP_VITAL` | existující | `min` 5–40 % | **ne** (dříve ano) | — |
| Poškození rostlin | `POP_POSKPERC` | existující | `max 40` | ne; *Coleanthus* `info` | — |
| Populační jednotka | `info` řádky `POP_POCET` / `POP_POCETSUM` / `POP_POCETVITAL` / `POP_POCETSTERIL` | — | funkční `JEDNOTKA` = hodnoty `POCITANO` | NA | — |
| Frekvence | `cis_cevky_frekvence.csv` | — | `FREKV_MIN`, `FREKV_MAX` | — | — |

„.“ a prázdná buňka ⇒ žádný řádek. `POP_POSKPOC` (počet typů poškození) Příloha 2 neuvádí ⇒ odebrán.

### Příloha 3 → ID_IND (všechny `val`, `KLIC = info`)

| Řádek | `ID_IND` | Tag v NDOP | Extrakce v `21_1` | Druhů |
|---|---|---|---|---|
| Celkové zastínění lokality | `STA_ZASTINENICELK` | `STA_ZASTINENICELK` | ano | 2 |
| Pokryvnost dřevin | `STA_POKRDREV` | `STA_POKRYVNOSTDREVIN` | ano | 19 |
| Pokryvnost dřevin vyšších než 1 m | `STA_POKRYVNOSTE2E3` | `STA_POKRYVNOSTE2E3` | ano | 8 |
| Pokryvnost dřevin nižších než 1 m | `STA_POKRDREVNIZ` | `STA_POKRYVNOSTDREVINNIZ` | ano | 3 |
| Pokryvnost E1 | `STA_POKRYVNOSTE1` | `STA_POKRYVNOSTE1` | ano | 13 |
| Pokryvnost E0 | `STA_POKRYVNOSTE0` | `STA_POKRYVNOSTE0` | ano | 3 |
| Pokryvnost stařiny | `STA_POKRSTAR` | `STA_POKRYVNOSTSTARINA` | ano | 27 |
| Pokryvnost volné půdy | `STA_POKRYVNOSTVOLNAPUDA` (dříve sirotci `STA_POKRPUDA`, `STA_POKRVOLPUD`) | `STA_POKRYVNOSTVOLNAPUDA` | ano | 5 |
| Pokryvnost invazních druhů | `STA_POKRYVNOSTINVAZNI` | týž | ano | 32 |
| Pokryvnost expanzivních druhů | `STA_POKRYVNOSTEXPANZNI` | týž | ano | 33 |
| Pokryvnost rákosu | `STA_POKRRAKOS` | **v datech není** | **ne** | 2 |
| Ostatní natantní a submerzní | `STA_POC_OST_NATASUB_DRH` | `STA_POKRYVNOSTNATDRUH` | **ne** | 1 |
| Stupeň zachování stanoviště | `STA_STUPENZACH` | `SP_STUPEN_ZACH_STAN` | ano | 4 |
| Hydrologické poměry | `STA_HYDRPOMERY` (dříve sirotek `STA_HYDROPOMERY`, H-10) | `STA_HYDRPOMERY` | ano | 3 |
| Vlhkostní poměry stanoviště | `STA_VLHPOM` | `STA_VLHKOPOMERY` | ano | 1 |
| Podíl dna nad vodní hladinou | `STA_POKRDNO` | `STA_PODILDNONADHLADINA` | **ne** | 1 |
| Termín a doba trvání poklesu hladiny | `STA_POKLESHLAD` | `STA_POKLESVODNIHLADINA` | **ne** | 1 |

### Úroveň území (`UROVEN = chu`, `KLIC = ano`)

| Indikátor | `ID_IND` | Limit | Druhy | Vypuštěný mírnější práh |
|---|---|---|---|---|
| Podíl lokalit v dobrém stavu | `LOK_PROCDOBR` (`ind_id` 189) | `min 50` | všech 36 | 40 % (Tab. 2) |
| Podíl populace v lokalitách v dobrém stavu | `POP_PROCDOB` | `min 75` | 35 (ne *Coleanthus*) | 60 % |
| Cílový stav | `POP_CILSTAVPROC` (*nový*) | `min 100` | všech 36 | 80 % |

### Normalizace a předpoklady (každý je vyznačen i ve sloupci `pozn`)

| # | Normalizace / předpoklad | Rozsah |
|---|---|---|
| N-01 | Názvy druhů převedeny na NDOP (`NAZVY_ZDROJ` ve skriptu); *Stipa zalesskii* → *Stipa glabrata* („kavyl olysalý“ v NDOP) | 36 druhů |
| N-02 | Mezery na začátku buněk P2 oříznuty | 2 buňky |
| N-03 | `75-100%` → `75-100 %`, `50-75%` → `50-75 %` (převažující tvar dat) | 6 hodnot |
| N-04 | „< 6 let“ → `max 5` (celé roky) | 1 |
| N-05 | *Coleanthus* poškození `KLIC = info` při zachování limitu | 1 |
| N-06 | Jednotky Přílohy 1 → hodnoty `POCITANO`; „kvetoucí X“ = starší tvar „kvetoucí a plodné X“ (*potvrzeno* 2026-10-28); jiný orgán a semenáčky nezařazeny; jinou plošnou jednotku převádí `21_1` (H-36) | 99 řádků `info`, 12 potvrzených ekvivalencí (dříve chybně uváděno 16), 5 jednotek bez dat |
| N-07 | Vitalita jen u druhů, které ji mají v P2 (15 druhů) — jen tam řádky `POP_POCETVITAL` / `POP_POCETSTERIL` | 15 |
| N-08 | Mírnější prahy Tab. 1 a 2 vypuštěny | 3 indikátory |
| N-09 | `OKNO_LET` = 10, *Coleanthus* 12 (kap. 5.1) | 36 |

### Validace a rozdíl (podrobně v protokolu)

- Sloupce, pořadí a zpětné načtení stejným voláním jako config: ✅; 36 druhů (všechny z Přílohy 1); 0 duplicit;
  0 indikátorů s více prahy; 941 řádků (info 115, min 192, max 37, val 597).
- Slovník: 10 hodnot limitů se v datech daného tagu nevyskytuje — rákos (tag neexistuje), natantní `do 5 %` a
  `5-25 %` (data jen pětičlenně), vlhkostní poměry `zhoršené` (data jen `dobré`), podíl dna `51-100 %` (data `51-100%`).
  Buňky jen se šestičlennou stupnicí dnes žádný pětičlenný záznam nezasáhnou.
- Číselník: všech 31 `ID_IND` má plný název; `ind_id` mají jen `LOK_PROCDOBR` (189) a `POP_TREND` (80); **29 kódů musí
  přidělit ISOP**.
- Diff (druh × indikátor × úroveň): změněno 307, přidáno 157, odebráno 44, beze změny 41. Odebráno mj. `POP_POSKPOC` (31×),
  `POP_PLOCHA` (3×), `POP_POCETNOST`, `POP_POCETSPEC`, `POP_POPULACE`, `POP_POCETSUMLOD`, `STA_ZAPOJENICELK`,
  `POP_PROCDOB` u *Coleanthus*. Nové druhy *Artemisia pancicii* a *Crambe tataria*.

### Nové soubory

| Soubor | Obsah | Čte kód |
|---|---|---|
| `Data/Input/limity_cevky.csv` (živý; dříve `limity_cevky_2026.csv`) | limity ve formátu dosavadního `limity_cevky.csv` | ano — od `19602b0` (rozhodnutí 2026-10-28, ot. 11) |
| `Data/Input/cis_cevky_frekvence.csv` | `DRUH, POP_JEDNOTKA, FREKV_TEXT, FREKV_MIN, FREKV_MAX, OKNO_LET, pozn` | ano — `00_n2k_config.R`, `21_1`, `27` (H-22) |
| `Data/Input/cis_druhy_nazvy.csv` | `nazev_n2k, nazev_ndop, kod_sdf, skupina, pozn` | zatím ne (H-32) |
| `Data/Input/cevky_vyber_lokalit.csv` | `DRUH, KOD_LOKALITY, NAZEV_LOKALITY, ZDROJ` (Příloha 4) | zatím ne (H-31) |
| `Data/Input/cis_indikatory_popis.csv` | +30 řádků (názvy všech indikátorů rostlin) | ano (export) |
| `Data/Input/cevky_cilova_pocetnost.csv` | expertní cílová početnost lokality: `DRUH, KOD_LOKALITY, CILOVA_POCETNOST, JEDNOTKA, DUVOD, OD_ROKU, ROZHODL, DATUM`; má-li lokalita řádek, platí jeho hodnota, jinak průměr prvních tří monitoringů (ot. 3) | zatím ne (H-25); založen prázdný |
| `Data/Input/cevky_lokality_stav.csv` | zaniklé / déle započítávané / vyřazené lokality (c10): `DRUH, KOD_LOKALITY, STAV, PLATNOST_DO, DUVOD, ROZHODL, DATUM`; `STAV` = zaniklá / prodloužena / vyřazena | zatím ne (H-22 výjimky); založen prázdný |
| `Data/Input/cevky_neplatne_zaznamy.csv` | nevalidní záznamy (c17): `IDX_ND_AKCE, ID_ND_NALEZ, DRUH, KOD_LOKALITY, DUVOD, ROZHODL, DATUM`; prázdné `ID_ND_NALEZ` = celá akce | zatím ne (H-34); založen prázdný |

---

## Matice pravidel — kód proti metodice 2026-10-21

`OK` · `liší se` · `chybí` · `nejasné`. Řádky se vztahují ke kódu po commitu `677102a`.

| # | Pravidlo | Verdikt | Kde v kódu | Nález |
|---|---|---|---|---|
| PR-01 | jen data monitoringu | **liší se** | indikátory ze všech záznamů; [`24:219-228`](../../R/02_druhy/24_n2k_druhy_lokality.R#L219-L228) bere nahodilý záznam, chybí-li monitoring; `CILMON` jen pro podíl lokalit a součet početnosti ([`25:149`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L149), [`:191-193`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L191-L193)) | H-33 (H-13) |
| PR-02 | poslední doložený výskyt (informativní) | **chybí** | [`23_n2k_posledni_nalez.R`](../../R/02_druhy/23_n2k_posledni_nalez.R): `max(DATUM)` i z negativních záznamů, bez `GARANCE`/`PRESNOST`, ve `25` nepoužit | H-33 (H-15) |
| PR-03 | jen validní záznamy | **chybí** | `VALIDACE`, `GARANCE` se zahazují jako metadata ([`21_1:1541`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1541)) | H-34 |
| PR-04 | více návštěv: celý záznam s nejvyšší početností, shoda → nejstarší | **liší se** | agregace po indikátorech [`24:104-112`](../../R/02_druhy/24_n2k_druhy_lokality.R#L104-L112); reprezentativní návštěva nejnovější ([`24:221-225`](../../R/02_druhy/24_n2k_druhy_lokality.R#L221-L225)) | H-24 |
| PR-05 | relativní stupnice → medián | **liší se** | dolní mez z `cis_pocet_kat.csv` (8 tříd) [`21_1:1388-1409`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1388-L1409) | H-27 |
| PR-06 | populační jednotka | **liší se** (data) | funkční `JEDNOTKA` ✓; 12 ekvivalencí potvrzeno, plochy převádí `21_1` (`f51ade7`); v datech chybí jednotka Přílohy 1 u 5 druhů | H-36 |
| PR-07 | cílová početnost (první tři monitoringy, expertní přepis) | **chybí** | nikde se nepočítá | H-25 |
| PR-08 | minimální přípustná početnost | **chybí** | `POP_POCETMINPRIP` sirotek | H-25 |
| PR-09 | početnost (% cílové) | **chybí** | `POP_POCETCIL` sirotek | H-25 |
| PR-10 | vývoj početnosti T1–T3 | **liší se** | [`21_1:1833-1935`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1833-L1935) — číslo 0–2, obrácená logika, jedna hodnota na lokalitu | H-26 (H-03) |
| PR-11 | vitalita = kvetoucí / sterilní | **OK** po `677102a` (s limity 2026, živé od `19602b0`) | [`21_1:130-150`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L130-L150), [`:1426`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1426) | H-21 |
| PR-12 | poškození = součet mediánů | **liší se** | součet kategorií 0–3 [`21_1:1458-1484`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1458-L1484) | H-28 (H-12) |
| PR-13 | úspěšnost reprodukce (*Coleanthus*) | **chybí** (kód i data) | `POP_REPROUSP` sirotek | H-29 |
| PR-14 | délka absence (*Coleanthus*) | **chybí** | `POP_DELKAABSENCE` sirotek | H-29 |
| PR-15 | neznámý indikátor = chybí vstup | **OK** na úrovni indikátoru; **liší se** v agregaci | `STAV_IND = NA` ✓ ([`21_2:283`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L283)); `24` neznámé přeskakuje | H-23 |
| PR-16 | stanovištní jen informativní | **OK** (`KLIC = info`; limity 2026 živé od `19602b0`) | `24` počítá jen `ano`/`ne` ([`24:155-156`](../../R/02_druhy/24_n2k_druhy_lokality.R#L155-L156), [`:185-186`](../../R/02_druhy/24_n2k_druhy_lokality.R#L185-L186)) | P-03 |
| PR-17 | verdikt lokality dobrý/špatný/neznámý, max 1 neklíčový špatný | **liší se** | [`24:203-211`](../../R/02_druhy/24_n2k_druhy_lokality.R#L203-L211) — ≥ 2 neklíčové ⇒ zhoršený; neznámý nevzniká | H-23 (H-06) |
| PR-18 | verdikt území podle Tab. 1 / 2 | **liší se** | aritmetika [`25:372-385`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L372-L385); Tab. 2 obojživelníků jen pro `DRUHY_METODIKY_OBOJ` ([`25:314-316`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L314-L316)) | H-30 |
| PR-19 | celková početnost, podíl populace | **liší se** (po rozhodnutí 1, 2026-10-28) | [`25:149-155`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L149-L155) — součet `POP_POCET` reprezentativních let lokalit ✓; jmenovatel podílu populace ale zahrnuje i lokality v neznámém stavu; závisí na H-24 a jednotkách (H-36) | H-30 |
| PR-20 | podíl lokalit; lokality v neznámém stavu se nezapočítávají (rozhodnutí 1, 2026-10-28) | **OK** | jmenovatel bez neznámých ([`25:191`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L191)) | H-30 (H-07) |
| PR-21 | cílový stav (ISOP / plán péče) | **chybí** pro rostliny | [`25:314-333`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L314-L333) jen obojživelníci; párování SDO jménem ([`02_n2k_data_druhy.R:45-48`](../../R/00_config/02_n2k_data_druhy.R#L45-L48)) | H-30 (H-08) |
| PR-22 | výběr lokalit (*Campanula*, *Cerastium*) | **chybí** | žádný filtr; seznam vendorován | H-31 |
| PR-23 | zaniklé lokality 10 / 12 let | **OK** po `677102a` (základ); **chybí** výjimky | [`21_1:93-109`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L93-L109), [`:489-498`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L489-L498) | H-22 |
| PR-24 | nové lokality se započítají | **OK** | každý nový `KOD_LOKAL` vstupuje automaticky | — |
| PR-25 | bez pevného okna, původní údaje | **OK** po `677102a` | tamtéž + [`27:172-177`](../../R/02_druhy/27_n2k_druhy_zapis.R#L172-L177) | H-22 |
| PR-26 | území EVL i MZCHÚ | **chybí** MZCHÚ | území jen z pole `EVL` exportu ([`02_n2k_data_druhy.R:153-157`](../../R/00_config/02_n2k_data_druhy.R#L153-L157)) | H-35 |
| PR-27 | lokalita = kód lokality monitoringu | **liší se** | bez kódu se dosadí `LOKALITA` ([`02_n2k_data_druhy.R:228`](../../R/00_config/02_n2k_data_druhy.R#L228), [`:242`](../../R/00_config/02_n2k_data_druhy.R#L242)) | H-36 (H-01) |

**Souhrn:** OK 5, částečně 2 (PR-15, PR-23), liší se 10, chybí 10, nejasné 0 (nejasnosti jsou ve zdroji —
R-44 až R-46). *Opraveno 2026-10-28:* původně uvedený souhrn „OK 4 (+ 2 částečně), liší se 13, chybí 9“ byl
chybně sečten (dával 28 pravidel). Po rozhodnutích 2. kola se počty nemění: PR-20 přešlo z „liší se“ na OK,
PR-19 z OK na „liší se“ (jmenovatel podílu populace).

---

## Nálezy Fáze C

### H-21 ✅ — Vitalita: kvetoucí a plodné / sterilní, za celou akci *(provedeno, `677102a`)*
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Odkazy:** PR-11, R-43, H-05
- **Metodika:** kap. 7.1 — „podíl kvetoucích a plodných populačních jednotek a sterilních populačních jednotek vyjádřený v procentech“.
- **Dřív:** `POP_POCETVITAL / POP_POCET * 100` po záznamech (kvetoucí / celkem); kvetoucí a sterilní jsou v NDOP v samostatných záznamech téže akce, takže u záznamů po roce 2021 podíl nevznikl (H-05).
- **Nyní:** [`vitalita_cevky()`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L130-L150) za akci; sterilní jednotky z nových `info` řádků `POP_POCETSTERIL`, kvetoucí z `POP_POCETVITAL`; celá akce negativní ⇒ 0; chybí kvetoucí nebo sterilní ⇒ NA; 0 sterilních ⇒ strop 999999 (splní každý limit `min`; `Inf` by `21_2` nepřečetl).
- **Důsledek:** se **živými** limity (`limity_cevky.csv` nemá `POP_POCETSTERIL`) je vitalita u všech rostlin **neznámá**; správně počítá až s `limity_cevky_2026.csv`. *Poa riphaea* nemá v datech kvetoucí ani sterilní trsy ⇒ vitalita trvale neznámá (R-51).
- **Dotčené skupiny ve sdíleném kódu:** žádné — `POP_VITAL` mají v limitech jen rostliny (ryby používají `POP_VITALITA`).

### H-22 ✅ — Hodnocené období po lokalitách (10 / 12 let) *(provedeno, `677102a`)*
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Odkazy:** PR-23, PR-25, R-48, H-09, H-15
- **Metodika:** kap. 6 „u neaktualizovaných lokalit vstupují do hodnocení původní údaje“; kap. 5.1 zaniklé lokality 10 let (*Coleanthus* ≥ 12) od posledního prokazatelného výskytu; kap. 7.1 cílová početnost z prvních tří monitoringů.
- **Dřív:** všechny skupiny `ROK >= current_year - 12` (13 sezón, H-09) v `21_1` i v předfiltru `27`.
- **Nyní:** u druhů z `cis_cevky_frekvence.csv` [`vyber_obdobi_cevky()`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L93-L109) ponechá lokality, jejichž poslední prokazatelný výskyt je nejvýše `OKNO_LET` let starý, **s celou historií**; „prokazatelný“ = pozitivní záznam monitoringu, nebo jiný záznam garantovaný jako věrohodný s přesností ≤ 1 km. Předfiltr [`27:172-177`](../../R/02_druhy/27_n2k_druhy_zapis.R#L172-L177) rostliny nekrátí. Ostatní skupiny beze změny.
- **Výklad k potvrzení:** hranice „deset let od“ — výskyt 2015 se při hodnocení roku 2025 ještě započítá.
- **Důsledek:** reprezentativní rok lokality v `24` je dál nejnovější (do 25 vstupuje jeden rok na lokalitu, takže se nesčítá historie); pseudolokality pojmenované textem (H-01) vstupují, mají-li pozitivní záznam v okně. Trendový blok a budoucí cílová početnost dostanou celou historii.
- **Nepokryto:** *Coleanthus* — vyřazení po 12 letech jen při doložených vhodných podmínkách ve ≥ 2 sezónách posledních 6 let (dnes se vyřadí vždy); prodloužení „lze započítávat delší dobu“ není kde evidovat (c10, níže).
- **Dotčené skupiny ve sdíleném kódu:** žádné — větev je podmíněna příslušností druhu k číselníku.

### H-23 — Verdikt lokality: max. 1 neklíčový špatný, stav neznámý
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY · **Odkazy:** PR-15, PR-17, R-45, H-06
- **Stav v kódu:** [`24:203-211`](../../R/02_druhy/24_n2k_druhy_lokality.R#L203-L211) — klíčový nesplněn ⇒ špatný; ≥ 2 neklíčové nesplněny ⇒ **zhoršený**; neznámé indikátory se nepočítají, takže **neznámý nevzniká**; popisek `NA` je „nehodnoceno“ ([`24:253-257`](../../R/02_druhy/24_n2k_druhy_lokality.R#L253-L257)).
- **Dotčené skupiny ve sdíleném kódu:** obojživelníci, ryby, hmyz, savci — **podmínit skupinou**.
- **Návrh opravy (neaplikováno):** v `n2k_eval` doplnit očekávané počty z řádků s limitem bez ohledu na `STAV_IND` a pro rostliny samostatnou větev:
  ```r
  N_KEY_ALL = dplyr::n_distinct(ID_IND[KLIC == "ano" & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != ""]),
  N_OTH_ALL = dplyr::n_distinct(ID_IND[KLIC == "ne"  & UROVEN == "lok" & !is.na(LIM_IND) & LIM_IND != ""]),
  # ...
  je_cevnata = DRUH %in% cis_cevky_frekvence$DRUH,
  CELKOVE = dplyr::case_when(
    je_cevnata & (N_KEY_ALL > N_KEY_EXPECTED | N_OTH_ALL - N_OTH_EXPECTED > 1) ~ NA_real_,  # neznámý (přednost)
    je_cevnata & (N_KEY_PASSED < N_KEY_EXPECTED | N_OTH_FAIL >= 2) ~ 0,       # špatný
    je_cevnata ~ 1,
    # ... dosavadní větve pro ostatní skupiny
  )
  ```
  a popisek `NA` → „neznámý“ u rostlin.
- **Rozhodnutí zadavatele (2026-10-28, ot. 1):** špatný **nemá** přednost — platí doslovně kap. 7: klíčový
  indikátor neznámý nebo více než jeden neklíčový neznámý ⇒ lokalita **neznámá** (i když je jiný indikátor
  špatný); lokality v neznámém stavu se do procentních hodnocení území nezapočítávají (H-30). Návrh výše je
  tomu přizpůsoben (původně byla větev „špatný“ první).

### H-24 — Více návštěv v roce: výběr celého záznamu
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Odkazy:** PR-04, R-47 (ruší R-28), P-01
- **Stav v kódu:** agregace po indikátorech ([`24:104-112`](../../R/02_druhy/24_n2k_druhy_lokality.R#L104-L112): populační max, stanovištní a poškození min) — metodika 2026 ji nahrazuje výběrem jednoho záznamu.
- **Návrh opravy (neaplikováno):** na konci `21_1`, jen pro druhy z číselníku: pro každou `KOD_LOKAL × ROK` ponechat akci (`IDX_ND_AKCE`) s nejvyšší `POP_POCET` (v populační jednotce), při shodě s nejstarším `DATUM`; ostatní akce roku zahodit. Agregace v `24` pak pracuje s jedinou akcí a výsledek nezávisí na směru `max`/`min`.
- **Dotčené skupiny:** žádné při podmínce na číselník rostlin.

### H-25 — Cílová početnost a indikátory početnosti
- **Závažnost:** kritická · **Typ:** GAP · **Odkazy:** PR-07 až PR-09, H-04, R-19
- **Stav v kódu:** cílová početnost se nepočítá; `POP_POCETMINPRIP` a `POP_POCETCIL` jsou sirotci.
- **Návrh opravy (neaplikováno):** v `21_1` (blok lokality × rok) pro rostliny: roční početnost lokality = početnost vybraného záznamu (H-24); `CILOVA` = průměr prvních tří let s monitoringem a známou početností; přepis expertní hodnotou z vendorovaného souboru `Data/Input/cevky_cilova_pocetnost.csv` (založen prázdný; `DRUH, KOD_LOKALITY, CILOVA_POCETNOST, JEDNOTKA, DUVOD, OD_ROKU, ROZHODL, DATUM`; `JEDNOTKA` se kontroluje proti jednotce `POP_POCET` druhu); `POP_POCETMINPRIP = POP_POCETCIL = aktuální / CILOVA * 100`; `CILOVA = 0` ⇒ NA; méně než tři monitoringy se známou početností a bez přepisu ⇒ NA.
- **Rozhodnutí zadavatele (2026-10-28, ot. 3):** ✅ přísně **první tři monitoringy**; má-li lokalita řádek v `cevky_cilova_pocetnost.csv`, platí jeho hodnota. Tím se pro rostliny ruší „dvě přípustné“ (odpověď 6, 2026-10-02) i „maximum kdy zaznamenané“ (odpověď 7).

### H-26 — Vývoj početnosti (`POP_TREND`) podle T1–T3
- **Závažnost:** kritická · **Typ:** BUG + ROZPOR-METODIKY · **Odkazy:** PR-10, R-44, H-03
- **Stav v kódu:** [`21_1:1833-1935`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1833-L1935) — součet dvou příznaků proti 3. nejnovějšímu roku (číslo 0–2, obrácená logika), jedna hodnota na lokalitu pro všechny roky. Nové limity očekávají kategorii ⇒ **s `limity_cevky_2026.csv` by bez opravy selhal u každé lokality.**
- **Návrh opravy (neaplikováno):** pro každý rok lokality T1–T3 = početnosti tří posledních let s monitoringem do daného roku včetně; `negativní` (T2 < T1 a T3 < T1), `pozitivní` (T2 > T1 a T3 > T1), jinak `setrvalý`; < 3 údaje ⇒ NA; připojit po roce.
- **Rozhodnutí zadavatele (2026-10-28, ot. 4):** ✅ výklad platí, pokud odpovídá textu. **Ověřeno proti textu kap. 7.1:** slovy je definován jen negativní trend („T2 i T3 jsou menší než T1“); pozitivní je dán jen příkladem 10; 12; 11 a setrvalý příklady 10; 10; 10 / 10; 12; 10 / 10; 2; 10 / 10; 10; 9 — všechny příklady výkladu odpovídají. T1–T3 = „tři poslední údaje o početnosti … ze tří po sobě jdoucích monitoringů“. Protože limit je „pozitivní; setrvalý“, rozhoduje o stavu indikátoru jen slovně definovaný negativní trend; hranice pozitivní/setrvalý výsledek nemění.
- **Dotčené skupiny:** žádné (blok jen u druhů s `POP_TREND*`).

### H-27 — Relativní stupnice: mediány metodiky
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Odkazy:** PR-05, R-38
- **Stav v kódu:** `REL_POC` → kategorie 1–8 ([`21_1:582`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L582)) → **dolní mez** z `cis_pocet_kat.csv` ([`21_1:1388-1409`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1388-L1409)); číselník má jiné třídy (11–50, 51–100) i středy než metodika.
- **Návrh opravy (neaplikováno):** pro rostliny převádět `REL_POC` přímo na mediány metodiky (`1-10` 5,5 · `11-100` 55,5 · `101-1000` 550,5 · `1001-10 000` 5 500,5 · `10 001-100 000` 55 000,5 · `100 001-1 000 000` 550 000,5 · `> 1 000 000` 1 000 001), slovní kategorie („hojně“…) ⇒ NA. Nový malý číselník, `cis_pocet_kat.csv` nesahat (obojživelníci).
- **Dotčené skupiny:** žádné při podmínce na rostliny.

### H-28 — Poškození: součet mediánů po typech
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Odkazy:** PR-12, H-12
- **Stav v kódu:** [`21_1:1458-1484`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1458-L1484) — součet kategorií 0–3; limit `max 40` by se porovnal se součtem kategorií.
- **Návrh opravy (neaplikováno):** po typech: `Nezjištěno`/`Bez poškození` 0; `1-10 %` 5,5; `11-50 %` (i starší `10-50 %`) 30,5; `51-100 %` (i `50-100 %`) 75,5; holé „N %“ = N; mezery kolem pomlčky normalizovat; „N lodyh“ a slovní položky ⇒ NA; `POP_POSKPERC` = součet. Rozhoduje otevřenou otázku H-12 (součet, ne maximum).
- **Rozhodnutí zadavatele (2026-10-28, ot. 8):** ✅ součet **omezit na 100 %** (`pmin(součet, 100)`). Mapování starších tříd `10-50 %` / `50-100 %` na 30,5 / 75,5 zůstává v návrhu.

### H-29 — *Coleanthus*: úspěšnost reprodukce a délka absence
- **Závažnost:** kritická (pro druh) · **Typ:** GAP (kód i data) · **Odkazy:** PR-13, PR-14, R-40, R-50
- **Stav:** oba indikátory sirotci; podíl plodných jedinců v NDOP jen `POZN_BIO` „Míra úspěšnosti reprodukce: N %“ (18 záznamů 2020–2022).
- **Návrh opravy (neaplikováno):** `POP_REPROUSP` = „dostatečná“, existuje-li v posledních 6 letech pozitivní záznam monitoringu s mediánem `REL_POC` > 100 a podílem ≥ 11 % (zdroj: `POZN_BIO`, příp. budoucí pole formuláře); jinak „nedostatečná“, je-li absence doložena (≥ 2 negativní záznamy monitoringu v 6 letech a žádný jiný výskyt), jinak NA. `POP_DELKAABSENCE` = roky od posledního prokazatelného výskytu (logika H-22), hodnotí se jen při doložené absenci.
- **Blokuje:** data od roku 2023.
- **Rozhodnutí zadavatele (2026-10-28, ot. 6):** pole ve formuláři je; najít v datech nejvhodnější údaj a dát ho k validaci.
- **Prohledání dat (2026-10-28; oba exporty NDOP, 685 záznamů *Coleanthus*, z toho 368 monitoring; prohledány všechny sloupce a všechny tagy `STRUKT_POZN`):**

  | Zdroj | Období | Záznamů (monitoring) | Povaha | Použitelnost |
  |---|---|---|---|---|
  | `POZN_BIO`, klíč „Míra úspěšnosti reprodukce: N %“ | 2020–2022 | 18 (10 lokalit, 9 EVL) | číslo v %; u 9 negativních záznamů vždy 0 %, u 8 pozitivních 40–100 %, 1 pozitivní bez hodnoty („%“) | **navržený zdroj** — přímo podíl odplozených jedinců; záznamy nemají `KOD_LOKALITY` (párování názvem, H-01) |
  | tagy `STRUKT_POZN` | 2023–2024 | 100 (15 pozitivních) | `MAN`, `MAN_POTREBAVLIV`, `SP_POSKOZENI_ROSTLIN`, `STA_PODILDNONADHLADINA`, `STA_POKLESVODNIHLADINA`, `STA_POKRYVNOSTEXPANZNI`, `STA_POKRYVNOSTINVAZNI`, `VLV_VLIVY` | **žádný tag s úspěšností reprodukce**; `STA_POKLESVODNIHLADINA` hodnotí dobu poklesu hladiny „z hlediska dozrání semen“ — podmínky, ne výsledek |
  | volný text `POZNAMKA` / `POZN_BIO` | 2023–2024 | 5 | např. „kvetlo méně než 50 % rostlin“ (Csub043, 2023), „většina puchýřky úspěšně odplodila“ (Csub248, 2024), „řada jedinců asi nestihne reprodukci dokončit“ (Csub026, 2023) | jen ruční přepis |
  | `SP_FERTILITA` | 2022–2023 | 0 (2 záznamy mimo monitoring) | „kvetoucí jedinci“, „sterilní a kvetoucí jedinci“ | ne — kvalitativní, mimo monitoring |

  Záznamy 2020–2022 s „Mírou úspěšnosti reprodukce“ (pozitivní / negativní): Hvožďanské háje (2022: 100 % / 2020–2021: 0 %), Kamenný rybník u Polničky (2020: bez hodnoty / 2021: 0 %), Lomnický Malý rybník (2020: 90 %), Lomnický Velký rybník (2020: 90 %), Malý Horusický rybník (2021: 90 %), Maňovický rybník (2020: 40 %), Velký Býkovický rybník (2020–2022: 0 %), Vidlák (2020, 2022: 0 %), Vosecký rybník (2020, 2022: 50 %), Černý Nadýmač (2020: 90 %).
- **K validaci zadavatelem:** (a) je „Míra úspěšnosti reprodukce“ v `POZN_BIO` správný údaj pro podíl odplozených jedinců (≥ 11 %)? (b) pod jakým názvem (tag / sloupec) exportuje formulář pole od roku 2023 — v exportu (monitoring rostlin do 2025-11) není u *Coleanthus* vyplněno nikde.

### H-30 — Úroveň území: Tabulky 1 a 2 s jediným prahem
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY + GAP · **Odkazy:** PR-18 až PR-21, R-46, H-07, H-08
- **Stav v kódu:** `CELKOVE` aritmetikou nad klíčovými `chu` indikátory ([`25:372-385`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L372-L385)): 0 nesplněných ⇒ dobrý, 1 ⇒ zhoršený, ≥ 2 ⇒ špatný (nezjištěné se odečítají). Cílový stav jen obojživelníci. `POP_CILSTAVPROC` se nepočítá ⇒ s novými limity se dnes rozhoduje jen z `LOK_PROCDOBR` a `POP_PROCDOB`. Jmenovatel podílu lokalit bez neznámých.
- **Tabulky po vypuštění mírnějších prahů** (p = podíl populace ≥ 75 %, l = podíl lokalit ≥ 50 %, c = cílový stav ≥ 100 %):

  | p | l | c | Tab. 2 (doslovně) | aritmetika `25` |
  |---|---|---|---|---|
  | ✓ | ✓ | ✓ | dobrý | dobrý |
  | ✓ | ✓ | ✗ | zhoršený (C 80–99) / špatný (C < 80) | zhoršený |
  | ✗ | ✓ | ✓ | zhoršený (P 60–74) / nepokryto (P < 60) | zhoršený |
  | ✓ | ✗ | ✓ | zhoršený (L 40–49) / nepokryto (L < 40) | zhoršený |
  | ✗ | ✗ | ✓ | zhoršený (P ≥ 60 a L ≥ 40) / špatný (oba níž) | špatný |
  | ✗ / ✓ | ✓ / ✗ | ✗ | špatný (ř. 5) / nepokryto | špatný |
  | ✗ | ✗ | ✗ | špatný | špatný |

  | l | c | Tab. 1 (*Coleanthus*) | aritmetika `25` |
  |---|---|---|---|
  | ✓ | ✓ | dobrý | dobrý |
  | ✓ | ✗ | zhoršený (C 80–99) / nepokryto (C < 80) | zhoršený |
  | ✗ | ✓ | **špatný** | **zhoršený** |
  | ✗ | ✗ | špatný | špatný |

- **Návrh opravy (neaplikováno):** Tab. 2 = aritmetika počtu nesplněných (shoduje se s převažujícím čtením všech jednoznačných buněk); Tab. 1 = podíl lokalit rozhoduje (nesplněn ⇒ špatný, jinak c rozhoduje dobrý/zhoršený) — větev podmíněná *Coleanthus*. Dále: `POP_CILSTAVPROC` = celková početnost / cílový stav × 100 s párováním cílových stavů přes `sdf_code` (`cis_druhy_nazvy.csv`); chybí-li cílový stav ⇒ území **neznámé**; jmenovatel podílu populace jen z lokalit se známým stavem (`CELKOVE_CTX` dobrý/špatný), jmenovatel podílu lokalit beze změny (podmínit skupinou).
- **Dotčené skupiny ve sdíleném kódu:** obojživelníci (blok cílového stavu, Tab. 2), ostatní skupiny (aritmetika, jmenovatel podílu populace) — podmínit.
- **Rozhodnutí zadavatele (2026-10-28, ot. 1 a 2):** ✅ lokality v neznámém stavu se do procentních hodnocení (podíl lokalit, podíl populace) **nezapočítávají vůbec** — ani do čitatele, ani do jmenovatele; podíl lokalit to už splňuje ([`25:191`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L191)), podíl populace ne (jmenovatel [`25:149`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L149) sčítá všechny lokality). ✅ Území **bez cílového stavu** ⇒ **neznámý**. *Výklad (k potvrzení):* cílový stav se počítá ze všech známých početností bez ohledu na stav lokality (text: „součet všech posledních zjištěných početností“); bez lokality se známým stavem nelze podíly spočítat ⇒ území neznámé.
- **Zbývá rozhodnout:** sloučená Tab. 1 pro *Coleanthus* (l ✗, c ✓: špatný podle tabulky, zhoršený podle aritmetiky); platí „bez cílového stavu ⇒ neznámý“ i tam, kde tabulka dává špatný pro libovolný cílový stav (řádek „∀ %“)?

### H-31 — Výběr lokalit (*Campanula bohemica*, *Cerastium alsinifolium*)
- **Závažnost:** vysoká · **Typ:** GAP · **Odkazy:** PR-22, R-49
- **Stav v kódu:** žádný filtr; `Data/Input/cevky_vyber_lokalit.csv` založen (120 lokalit), kód ho nečte.
- **Návrh opravy (neaplikováno):** v `21_1` za filtrem druhu: má-li druh v souboru řádky, ponechat jen `KOD_LOKAL %in% KOD_LOKALITY`. Pro *Cerastium* soubor řádky nemá ⇒ dnes by se hodnotily všechny lokality.
- **Rozhodnutí zadavatele (2026-10-28, ot. 5):** ✅ do dodání Přílohy 5 hodnotit u *Cerastium* **všechny lokality pod cíleným monitoringem** — tj. soubor výběru nemá pro druh řádky (všechny lokality) a platí omezení na data monitoringu (H-33).

### H-32 — Názvy druhů: filtr předmětů ochrany zahodí šest druhů
- **Závažnost:** kritická (pro 6 druhů) · **Typ:** BUG · **Odkazy:** R-35, H-18
- **Stav v kódu:** [`21_1:500-504`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L500-L504) a [`27:178-181`](../../R/02_druhy/27_n2k_druhy_zapis.R#L178-L181) — `DRUH %in% sites_subjects$nazev_lat`; *Adenophora liliifolia*, *Echium maculatum*, *Gentianella praecox* subsp. *bohemica*, *Iris arenaria*, *Stipa glabrata*, *Trichomanes speciosum* mají v seznamu předmětolokalit jiné jméno ⇒ **dnes se nehodnotí vůbec**.
- **Návrh opravy (neaplikováno):** v `00_n2k_config.R` doplnit `sites_subjects$nazev_lat` o NDOP jména z `cis_druhy_nazvy.csv` (řádky s `nazev_n2k != nazev_ndop`) — přidává jen řádky, ostatní skupiny beze změny. Zároveň napravit nepárový filtr (H-18).

### H-33 — Zdroj dat a „Poslední doložený výskyt“
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY + GAP · **Odkazy:** PR-01, PR-02, H-13, H-15
- **Stav v kódu:** verdikt lokality i z nahodilých záznamů (H-13); `23` počítá poslední nález i z negativních záznamů a ve `25` se nepoužije.
- **Návrh opravy (neaplikováno):** rostliny — indikátory lokality jen z `CILMON == 1`; nový informativní indikátor `POP_POSLVYSKYT` (*provizorní ID*) = rok posledního prokazatelného výskytu (logika `vyber_obdobi_cevky`), v limitech jako `info`; prostorové přiřazení nálezů mimo monitoring k lokalitě je mimo repozitář (c8, c31).

### H-34 — Nevalidní záznamy (c17)
- **Závažnost:** střední · **Typ:** GAP · **Odkazy:** PR-03
- **Stav:** žádný mechanismus; viz odpověď na c17.
- **Rozhodnutí zadavatele (2026-10-28, ot. 9):** ✅ evidence v repozitáři schválena — založeny prázdné `Data/Input/cevky_neplatne_zaznamy.csv` (c17) a `Data/Input/cevky_lokality_stav.csv` (c10); kód je zatím nečte (filtr v `21_1` a výjimky v `vyber_obdobi_cevky` jsou součástí H-34 / H-22, otázka 11).

### H-35 — MZCHÚ se nehodnotí
- **Závažnost:** střední · **Typ:** GAP · **Odkazy:** PR-26
- **Stav:** území = pole `EVL` exportu ([`02_n2k_data_druhy.R:153-157`](../../R/00_config/02_n2k_data_druhy.R#L153-L157)); pro MZCHÚ v exportu pole není a cílové stavy z plánů péče nejsou v repozitáři.
- **Návrh:** samostatné zadání (přiřazení záznamů k MZCHÚ, zdroj cílových stavů).

### H-36 — Jednotky a identita lokalit v datech *(jednotky provedeno, `f51ade7`, `19602b0`)*
- **Závažnost:** vysoká · **Typ:** GAP (data) · **Odkazy:** PR-06, PR-27, R-51, H-01, H-02
- **Stav:** úplná tabulka jednotek v protokolu převodu; 5 jednotek Přílohy 1 v datech chybí. Identita lokalit beze změny (H-01).
- **Rozhodnutí zadavatele (2026-10-28, ot. 7):** ✅ ekvivalence „kvetoucí X“ = „kvetoucí a plodné X“ potvrzeny (12 řádků limitů, stav `potvrzeno`; v otázce chybně uvedeno 16); ✅ plochy převádět podle Přílohy 1.
- **Provedeno:** [`koef_plochy()`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L152-L165) a větev `POP_POCET` v `21_1` — je-li `JEDNOTKA` řádku `POP_POCET` plocha, převede se na ni záznam v jiné plošné jednotce: *Cerastium alsinifolium* cm² (35) a dm² (3) → m², *Klasea lycopifolia* cm² (1) a „plocha populace (cm2)“ (12) → m²; *Galium sudeticum* (m²) a *Trichomanes speciosum* (cm²) jiné plošné jednotky v datech nemají. Plochy u druhů s neplošnou jednotkou (*Luronium* růžice, *Coleanthus* jedinci, *Crambe*, *Campanula bohemica*) se nepřevádějí — plochu nelze převést na počet. Nahrazuje nedokončený zástupný blok `POP_CILJEDNOTKA` / `POP_KOEFICIENT`. Ostatní skupiny nemají plošnou `POP_POCET` ⇒ beze změny. Ověřeno 7 jednotkovými testy.

### P-03 — Informativní stanovištní indikátory bez změny kódu *(potvrzeno)*
- `KLIC = info`: `21_2` je vyhodnotí (`STAV_IND`) a propíše, `24` je nezapočte (počítá jen `ano`/`ne`), kontrola úplnosti v configu je nevaruje (hlídá jen prázdný `KLIC`). Hodnota „nelze určit“ u hydrologických poměrů vyjde jako nesplněná (měla by být neznámá) — u informativního indikátoru bez vlivu na verdikt.

### H-37 — Export do ISOP: 29 indikátorů bez `ind_id`
- **Závažnost:** střední · **Typ:** STOPA-DO-ISOP · **Odkazy:** H-16
- **Stav:** číselník má plný název všech 31 indikátorů; `ind_id` jen 189 a 80. Bez kódu jde do exportu surové `ID_IND` ([`27:785`](../../R/02_druhy/27_n2k_druhy_zapis.R#L785)).
- **Návrh:** vyžádat kódy v ISOP (seznam v protokolu převodu). `POP_TREND` → 80 platí už nyní — s dnešním (chybným, H-26) výpočtem by se pod kódem 80 exportovala obrácená hodnota; nasazovat společně s H-26.

---

## Odpovědi na otázky autora metodiky

**c8 — přiřazení lokalit k hodnocenému území (překryv, buffery, procenta).**
Pipeline žádný prostorový překryv nedělá. Území se bere z pole `EVL` exportu NDOP, které NDOP
dopočítává sám: `kod_chu = substr(EVL, 1, 9)` ([`02_n2k_data_druhy.R:153-157`](../../R/00_config/02_n2k_data_druhy.R#L153-L157)).
Export nese i `PROC_EVL` (podíl zákresu nálezu v EVL) a `MAX_PROC`, ale **kód je nepoužívá** — není
žádný minimální podíl: u cévnatých rostlin má 829 záznamů `PROC_EVL` < 50 % a 30 < 10 %; 76 záznamů
leží ve dvou EVL a vstupuje do obou. Záznamy se k lokalitě přiřazují kódem lokality (`KOD_LOKALITY`),
bez kódu textovým názvem lokality (H-01). MZCHÚ v exportu nejsou (H-35). Způsob výpočtu `EVL`/`PROC_EVL`
(buffery, práh) je třeba ověřit u správce exportu NDOP (Z. Kučera).

**c10 — kde evidovat příznak zaniklé / déle započítávané lokality.**
Doporučení: vendorovaný soubor v repozitáři stejného typu jako výběr lokalit, např.
`Data/Input/cevky_lokality_stav.csv` (`DRUH, KOD_LOKALITY, STAV` = zaniklá / prodloužena / vyřazena,
`PLATNOST_DO, DUVOD, ROZHODL, DATUM`). Je verzovaný, dohledatelný a nezávisí na verzi garantské
aplikace; kód ho přečte ve `vyber_obdobi_cevky` (H-22). Druhá možnost je atribut lokality
v registru lokalit NDOP (`IDX_ND_LOK`), pokud jej NDOP umí nést. Pozor: rozhodnutí H-15
(2026-10-02) zní „evidovat se nebude“ — nový text („lze započítávat delší dobu“, *Coleanthus*)
ho fakticky vyžaduje; potřeba znovu rozhodnout.

**c17 — vyřazení nevalidních záznamů (např. po pokosení).**
Dnes nijak: `VALIDACE` (validováno — chybný záznam / záznam k opravě / méně věrohodný) i `GARANCE`
NDOP nese, ale pipeline je zahazuje jako metadata ([`21_1:1541`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1541)).
Doporučení: (a) pro nová data pole ve formuláři Survey123 „záznam nevstupuje do hodnocení“ + důvod,
uložené jako tag ve `STRUKT_POZN` a filtrované v `21_1`; (b) pro historii vendorovaný seznam
`IDX_ND_AKCE` s důvodem; (c) alternativně kategorie `VALIDACE` v NDOP, pokud ji validátoři mohou
pro tento účel použít. Je to tentýž mechanismus jako u c10 (evidence mimo garantskou aplikaci).

**c31 — buffer pro bodové a liniové zákresy.**
V repozitáři se **žádný buffer neuplatňuje** — párování nálezů NDOP s lokalitami prostorově
neprobíhá vůbec (viz c8). Buffer i práh 10 % je tedy otázka pro správce exportu NDOP (Z. Kučera).
Má-li se párování pro indikátor *Poslední doložený výskyt* dělat v repozitáři, je navrhovaný
buffer 5–10 m technicky snadný (`sf::st_buffer` v S-JTSK, jako u stanovišť), ale vyžaduje zákresy
lokalit a nálezů, které export dnes nenese.

---

## Pořadí nasazení

Převedené limity, úpravy `677102a` a navržené opravy na sobě závisí. **Stav po 2. kole (2026-10-28):**
limity 2026 jsou živé (`19602b0`, rozhodnutí 11), opravy H-23 … H-33 zatím ne („bude rozhodnuto“). Ve větvi
je tedy kombinace, ve které **výsledky cévnatých rostlin nedávají smysl** — hodnocení rostlin
nespouštět ani neexportovat do ISOP, dokud nejsou aplikovány alespoň H-25, H-26 a H-23:

1. **Vývoj početnosti (H-26):** limity čekají kategorii `pozitivní` / `setrvalý`, kód počítá číslo 0–2 ⇒
   indikátor je nesplněný u každé lokality, kde se spočítá.
2. **Klíčové indikátory početnosti (H-25):** `POP_POCETMINPRIP` a `POP_POCETCIL` se nepočítají; `21_2`
   bere jen indikátory, které kód spočítá, takže lokalita nemá žádný klíčový indikátor a verdikt stojí
   jen na neklíčových. *Coleanthus* totéž s `POP_REPROUSP` a `POP_DELKAABSENCE` (H-29).
3. **Verdikt lokality (H-23):** ≥ 2 neklíčové nesplněné ⇒ dosud „zhoršený“ místo „špatný“; neznámý nevzniká.
4. **Úroveň území (H-30):** `POP_CILSTAVPROC` se nepočítá; podíl populace má v jmenovateli i neznámé lokality.
5. Funguje podle metodiky: vitalita (H-21), hodnocené období (H-22), převod ploch (H-36), stanovištní
   indikátory jako informativní (P-03).

Doporučené pořadí zbývajících oprav: H-32 → H-24, H-25, H-26, H-27, H-28 → H-23 → H-30, H-31 → H-29, H-33
(data) → H-34 a výjimky H-22 (čtení `cevky_neplatne_zaznamy.csv`, `cevky_lokality_stav.csv`) → H-37 (kódy
ISOP; `POP_TREND` → 80 nasazovat až s H-26).

## Otázky pro zadavatele a autory (Fáze C)

| # | Otázka | Nález |
|---|---|---|
| 1 | Přednost „špatný“ před „neznámý“ ve verdiktu lokality (tři případy) | R-45, H-23 |
| 2 | Sloučené Tabulky 1 a 2 (aritmetika vs. *Coleanthus* podíl lokalit rozhoduje); stav neznámý území; území bez cílového stavu | R-46, H-30 |
| 3 | Cílová početnost: přísně první tři monitoringy? Evidence expertního přepisu (`cevky_cilova_pocetnost.csv`)? | H-25 |
| 4 | Výklad vývoje početnosti (pozitivní = T2 i T3 > T1) | R-44, H-26 |
| 5 | *Cerastium alsinifolium* do dodání Přílohy 5: všechny lokality, nebo žádná? | H-31 |
| 6 | *Coleanthus*: použít `POZN_BIO` „Míra úspěšnosti reprodukce“ (2020–2022) a zavést pole do formuláře? | R-50, H-29 |
| 7 | Potvrdit 16 kandidátů jednotek; zavést převod ploch cm² / dm² → m²? | R-51, H-36 |
| 8 | Poškození: omezit součet na 100 %? Mapovat starší třídy `10-50 %` / `50-100 %` na 30,5 / 75,5? | H-28 |
| 9 | Evidence zaniklých / prodloužených lokalit (c10) a nevalidních záznamů (c17) — schválit soubory v repozitáři? | H-22, H-34 |
| 10 | Kódy ISOP pro 29 indikátorů — kdo vyžádá? | H-37 |
| 11 | Které opravy H-23 … H-33 aplikovat (a kdy přepnout limity)? | — |

## Rozhodnutí zadavatele k otázkám Fáze C (2026-10-28)

| # | Rozhodnutí (2. kolo) | Provedeno / důsledek |
|---|---|---|
| 1 | Špatný **nemá** přednost před neznámým (platí doslovně kap. 7); lokality v neznámém stavu se do procentních hodnocení území nezapočítávají vůbec | návrh H-23 přeřazen (neznámý první); H-30: jmenovatel podílu populace jen ze známých lokalit; PR-20 → OK, PR-19 → liší se |
| 2 | Území bez cílového stavu ⇒ **neznámý** | doplněno do H-30; sloučená Tab. 1 a řádek „∀ %“ bez cílového stavu zůstávají otevřené |
| 3 | Cílová početnost = průměr **prvních tří** monitoringů; má-li lokalita hodnotu v novém souboru, platí ta | založen prázdný `Data/Input/cevky_cilova_pocetnost.csv`; výpočet je součástí H-25 (neaplikováno) |
| 4 | Výklad T1–T3 platí, odpovídá-li textu | ověřeno: slovy definován jen negativní trend, ostatní výklad odpovídá všem příkladům textu; o stavu rozhoduje jen negativní trend (H-26) |
| 5 | *Cerastium*: do dodání Přílohy 5 všechny lokality pod cíleným monitoringem | doplněno do H-31 (bez řádků ve výběru + H-33) |
| 6 | *Coleanthus*: pole je ve formuláři — najít nejvhodnější údaj v datech, dát k validaci | prohledány všechny sloupce a tagy; návrh `POZN_BIO` „Míra úspěšnosti reprodukce“ (2020–2022); od 2023 údaj v exportu není — **čeká na validaci** (H-29) |
| 7 | Potvrdit ekvivalence jednotek; plochy převádět podle Přílohy 1 | `f51ade7` (převod v `21_1`), `19602b0` (12 ekvivalencí `potvrzeno`) — H-36 |
| 8 | Poškození: součet omezit na 100 % | doplněno do H-28 (neaplikováno) |
| 9 | Evidence c10 a c17 v repozitáři schválena | založeny prázdné `cevky_lokality_stav.csv` a `cevky_neplatne_zaznamy.csv`; čtení v kódu je součástí H-22 / H-34 |
| 10 | Kódy ISOP — bude rozhodnuto | beze změny (H-37) |
| 11 | Opravy — bude rozhodnuto; limity aktualizovat novými hodnotami ve starém formátu | `19602b0`: skript píše přímo `limity_cevky.csv` (formát beze změny), `limity_cevky_2026.csv` zrušen; diff proti `1994c08` beze změny |

**Otevřené po 2. kole:** validace zdroje úspěšnosti reprodukce *Coleanthus* a název pole formuláře od 2023 (6);
sloučená Tab. 1 (*Coleanthus*, l ✗ c ✓) a „∀ %“ bez cílového stavu (2); výklad „cílový stav ze všech známých
početností“ (1); mapování tříd poškození `10-50 %` / `50-100 %` (8); kódy ISOP (10); výběr a pořadí oprav (11).

## Co bylo ověřeno proti datům a co jen proti kódu

| Proti datům NDOP | Jen proti kódu / limitům |
|---|---|
| slovník hodnot tagů a kategorií limitů; jednotky `POCITANO`; kódy Přílohy 4; pole pro reprodukci *Coleanthus*; názvy druhů (NDOP, seznam předmětolokalit, česká jména); `PROC_EVL`, `GARANCE`, `VALIDACE`, `PRESNOST` | všechny verdikty matice PR; dopad H-21 a H-22 na výsledky (pipeline se nespouštěla); H-23 až H-37 |

Neověřeno: dopad úprav `677102a`, `f51ade7` a přepnutí limitů `19602b0` na hodnocení (zadání zakazuje běh hodnocení);
způsob výpočtu `EVL` a `PROC_EVL` v NDOP. Úpravy kódu ověřeny jen parse a 24 jednotkovými testy pomocných
funkcí na syntetických datech (17 + 7 pro převod ploch).
