# Registr nálezů — revize modulu ryb a mihulí proti metodice EVL 2026

**Druhy:** *Cottus gobio* (vranka obecná), *Lampetra planeri* (mihule potoční),
*Misgurnus fossilis* (piskoř pruhovaný).
**Měřítko:** [Metodika_EVL_upravena_mihule_piskor.md](Metodika_EVL_upravena_mihule_piskor.md)
(L. Hanel, M. Muška; částečně upravené znění — viz [revize_metodika_ryby.md](revize_metodika_ryby.md), R-xx).
**Datum:** 2026-11-05 · **Větev:** `ryby-metodika-2026` (odbočena z `cevnate-metodika-2026`, `35a3d99`).
**Revize a kontrolní seznam pravidel:** [revize_metodika_ryby.md](revize_metodika_ryby.md) (PR-xx, R-xx, stav H-38 … H-58).

**Číslování.** Nálezy kódu a dat navazují na globální řadu: registr obojživelníků
([../Obojzivelnici/harmonizace_registr.md](../Obojzivelnici/harmonizace_registr.md)) končí
H-70 a obsahuje i dosavadní nálezy ryb H-38 … H-58. Tento registr začíná **H-71**, takže
číslo nálezu je jednoznačné napříč repozitářem. Nálezy v textu metodiky jsou `R-xx`
v revizi, pravidla `PR-<pravidlo>-<C|L|M>` (C = *Cottus*, L = *Lampetra*, M = *Misgurnus*).

**Rozhodnutí zadavatele ke Kontrolnímu bodu 0 (2026-11-05):**

| # | Otázka | Rozhodnutí |
|---|---|---|
| 1 | Samostatný registr ve složce ryb, H-71+, R-xx, PR-xx; odkaz z registru obojživelníků | **ano** |
| 2 | Verzovat text metodiky jako referenční vstup | **ano** (commit `c3c9397`) |
| 3 | Identita lokality | „lokalita se má řídit stejnými pravidly jako u ostatních skupin — seskupuje se podle `KOD_LOKAL`" — viz **H-71** (pravidlo je stejné, u ryb ale nemá vstup) |
| 4 | Počty v 1. a 2. odlovu z textu `POZNAMKA` | „analyzovat data, ale mají to být tagy" — viz **H-74** |
| 5 | Převod procentních pásem na číslo | „navrhnout řešení sladěné s obojživelníky a cévnatými rostlinami" — viz **H-76** |

**Rozhodnutí zadavatele ke Kontrolnímu bodu 1 (2026-11-05):**

| # | Otázka (revize, Z-xx) | Rozhodnutí | Provedení (Fáze 3) |
|---|---|---|---|
| Z-1 | zdroj kódu lokality ryb | „just LOKALITA — klíčové je, aby byla v CILMON; označit, chybí-li v seznamu CILMON významný nedávný `ZDROJ` s bohatou `STRUKT_POZN`, a případně doplnit" | `KOD_LOKAL` = `LOKALITA` (H-71); u ryb žádný takový zdroj nechybí; seznam se ale vůbec neuplatňoval — **H-89** |
| Z-2 | vzorec dynamiky *Lampetra* | **C** — oba poslední odlovy pod polovinou výchozí hodnoty | H-73 |
| Z-3 | počty odlovů z `POZNAMKA` | **ano** (přechodně, se zdrojem) | H-74 |
| Z-4 | `POP_PRESENCE` | **`info`** | H-80 |
| Z-5 | prázdný tag bariér | **neznámo**; souhrnnou statistiku na konci | H-83 |
| Z-6 | EVL s 1–2 lokalitami | **doslovně** 75 % | H-81 |
| Z-7 | sezóna a podmínky | **jen příznak** | H-84 |
| Z-8 | párový filtr předmětů ochrany | **opravit pro všechny skupiny, změřit dopad** | H-85 |
| Z-9 | volné soubory v kořeni | **přesunout** do `podklady/` (commit `9f894fe`) | H-88 |
| Z-10 | testovací běh | **ano** | `Outputs/Test/Ryby_20261105/` |
| Z-11 | převod pásem | **potvrzeno, zapsat do protokolu** | [limity_ryby_2026_kontrola.md](limity_ryby_2026_kontrola.md) |

**Rozhodnutí zadavatele ke Kontrolnímu bodu 2 (2026-11-05):**

| # | Otázka (revize, Z-xx) | Rozhodnutí | Provedení (Fáze 4) |
|---|---|---|---|
| Z-12 | lokalita bez hodnoceného klíčového parametru (H-90) | „minimálně jeden neznámý klíčový indikátor → neznámý celkový stav — implementovat napříč skupinami" | `24` pro **všechny skupiny**; neznámý má přednost před špatným (shodně s H-23 cévnatých); `25` (hmyz) nezapočítává neznámá pole — **H-90** |
| Z-13 | aplikovat patch a návrh limitů | „ano, aplikovat, zapsat do příslušných registrů" | patch aplikován; limity 2026 v živých `limity_ryby.csv` a `cis_ryby_delky_strukt.csv` (`55` zapisuje přímo, obdoba `54`); `*_2026.csv` zrušeny; zápisy v registrech obojživelníků a cévnatých |
| Z-14 | změřit opravu `CIS_CILMON` na obojživelnících před aplikací | **ano** | běh *T. cristatus* + *B. bombina* — **H-89** |
| Z-15 | doplnit zdroj „srpice karbincolisté" do `cil_mon_zdroj.csv` | **ne**, zapsat do registru cévnatých | nedoplněno; zápis v [../Cevnate/harmonizace_registr.md](../Cevnate/harmonizace_registr.md) |
| Z-16 | sjednotit zápis `LOKALITA` | **ano** | `02_n2k_data_druhy.R` — **H-71** |
| Z-17 | vitalita při méně než dvou průzkumech | **ano** — `NA` do odpovědi autorů | beze změny kódu (H-79) |
| Z-18 | otázky A-1 … A-10 autorům | „zapsat do markdownu metodiky na konec" | oddíl na konci [Metodika_EVL_upravena_mihule_piskor.md](Metodika_EVL_upravena_mihule_piskor.md), se sloupcem „Přechodně ve výpočtu" |

**Rozhodnutí a pokyny zadavatele ke Kontrolnímu bodu 3 (2026-11-05):**

| # | Pokyn | Provedení |
|---|---|---|
| Z-19 | „Týká se to aktuálních dat? Pokud to významně nemění hodnocení od 2024, není to problém." | změřeno — od 2024 mění jen obojživelníky (27–33 % DP); otázka zůstává otevřená — **H-91** |
| Z-20 | „ano, ale nejdřív vyřešit Z-19" | odloženo do rozhodnutí Z-19 |
| Z-21 | „nejdřív vyřešit piskoře a mihuli" | diagnóza; oprava **H-93** (`aa2d8bd`); hlavní příčinou je chybějící monitoring 2024–2025 — **H-94** |
| — | „`Monitoring druhů ČR` je `PROJEKT`, ne `ZDROJ` — doplnit zdroje ryb, např. Kolektiv autorů (2021) … (2025) Monitoring ryb. Monitoring druhů ČR. AOPK ČR." | řádek odstraněn, doplněno 8 zdrojů ryb — **H-89** |
| — | „pro populační metriky použít abundanci — `POCET` přepočtený na plochu" | `21_1` u tří druhů — **H-74**; tím zodpovězena i úprava B ze Z-21 (ne počet, ale hustota) |

**Odpovědi zadavatele — Kontrolní bod 3, 2. kolo (2026-11-05):**

| # | Odpověď | Provedení |
|---|---|---|
| Z-19 | „2023" | význam nejistý — změřeno i od roku 2023: obojživelníci *B. bombina* 163 z 530 DP (31 %), *T. cristatus* 113 z 332 (34 %); ryby 40–91 %; cévnaté 0 (H-25). **Doptat se** (H-91) |
| Z-20 | „follow the previous rule" | čte se jako dřívější pokyn „ano, ale nejdřív vyřešit Z-19" — **čeká na Z-19** |
| Z-22 | „export bude dodán později; teď brát poslední dostupné roky" | beze změny kódu: okno ryb je `ROK >= current_year - 12` (2013–2025), takže data do 2023 vstupují; dynamika a vitalita se počítají k poslednímu roku lokality a reprezentativní návštěva je nejnovější |
| Z-23 | „teď to není problém" | beze změny |
| Z-24 | „testovat jen piskoře a mihuli" | `24`: očekávané klíčové indikátory z limitů jen pro *Lampetra* a *Misgurnus* (`DRUHY_KLIC_Z_LIMITU`) — **H-92** |
| — | „exportovat jen indikátory popsané metodikou + přítomnost jako info" | limity tří druhů 46 → 27 řádků; jednotky `POCET` do `21_1` — viz §Fáze 4 |

Fáze 1 a 2 **nezměnily žádný kód ani vstupní data.** Fáze 3 vytvořila návrh limitů
(`Data/Input/limity_ryby_2026.csv`, `cis_ryby_delky_strukt_2026.csv`, skript
`R/05_metodiky/55_ryby_limity_2026.R`) a patch kódu
[patche/ryby_metodika_2026.patch](patche/ryby_metodika_2026.patch); obojí bylo ověřeno
testovacím během v izolované kopii repozitáře — viz [§Fáze 3](#fáze-3--návrh-patch-a-testovací-běh-2026-11-05).
**Fáze 4** (po Kontrolním bodu 2) patch i limity aplikovala a doplnila pravidlo H-90 —
viz [§Fáze 4](#fáze-4--aplikace-2026-11-05).

## Jak byl audit proveden

| Krok | Zdroj | Rozsah / poznámka |
|---|---|---|
| Normativní text | `Metodika_EVL_upravena_mihule_piskor.md` | tři kapitoly (564 řádků); text je částečně přepsán podle chování kódu (R-08) |
| Limity | `Data/Input/limity_ryby.csv` (Windows-1250, `;`, CRLF, bez BOM) | 272 řádků; tři druhy 46 řádků |
| Délkové skupiny | `Data/Input/cis_ryby_delky_strukt.csv` (ASCII, `,`, CRLF) | 3 řádky na druh |
| Kód | `00_n2k_config.R`, `02_n2k_data_druhy.R`, `21_1`, `21_2`, `22`, `24`, `25`, `27` | čtení kódu; čísla řádků k `35a3d99` |
| **Ostrá data** | `host_data/export_data_evl.csv` + `export_data_zprap.csv` (export 2026-05-10) | ryby 13 290 záznamů; *Cottus* 3 045, *Lampetra* 1 849, *Misgurnus* 790; projekt *Monitoring druhů ČR* od 2013: 1 063 / 701 / 246 |
| **Replikace verdiktů** | jednorázové skripty mimo repozitář (scratchpad) | pomocné funkce **převzaté `source()` přímo z `21_1` a `21_2`** (`kat_mnozina`, `val_shoda`, `max_cislo`, `tag_hodnota` …); pipeline se **nespouštěla**, `Outputs/` ani `Data/Temp/` nedotčeny |

**Pozor na povahu čísel.** Počty dotčených lokalit a EVL jsou z **replikace** logiky `21_1` → `24`
(`25` jen podíl lokalit) nad ostrými daty, ne z běhu kaskády. Replikace sleduje i vedlejší
mechanismy (násobení řádků `separate_rows()`, `reframe()` v `n2k_druhy_lokpop`, `slice(1)`
přes `POLE` v `24`), ale není s kaskádou ověřena. **Ověří ji testovací běh** (Kontrolní bod 1).
„Reprezentativní návštěva" = poslední rok cíleného monitoringu lokality (`KOD_LOKAL`),
tj. větev `CELKOVE_CILMON`, která vstupuje do `LOK_PROCDOBR`.

## Souhrn

| Závažnost | Počet | Nálezy |
|---|---|---|
| **Kritická** — plošně mění výsledek | 6 | H-71, H-72, H-73, H-75, H-81, H-90 |
| **Vysoká** — mění výsledek u části dat | 11 | H-74, H-76, H-77, H-78, H-79, H-80, H-83, H-85, H-89¹, H-91, H-94 |
| **Střední** | 4 | H-82, H-84, H-92, H-93 |
| **Nízká** | 3 | H-86, H-87, H-88 |

¹ H-89 bylo ve Fázi 2 vedeno jako nízké (mimo rozsah ryb); po rozboru ke Z-1 (2026-11-05)
přeřazeno — týká se 2 106 záznamů obojživelníků. Oddíl nálezu zůstává na původním místě.
| *Potvrzeno — bez akce* | 5 | P-01 … P-05 |

---

# Kritické nálezy

### H-71 ⚠⚠ — `KOD_LOKAL` u ryb nenese kód monitorované lokality: úseky se slévají do jedné „lokality"
- **Závažnost:** kritická · **Typ:** DATA + KÓD · **Stav:** ✅ **přechodně vyřešeno** — klíč `LOKALITA` (Z-1, `433b610`) se sjednoceným zápisem (Z-16, `06a849d`); trvale zbývá kód lokality (D-1) · **Odkazy:** PR-LOK-1, PR-EVL-1, H-70
- **Kontext:** metodika definuje lokalitu jako **prostorově fixovaný úsek** ≥ 100 m (≥ 800 m²)
  s GPS začátku a konce. Zadavatel (2026-11-05): *lokalita se má řídit stejnými pravidly jako
  u ostatních skupin — seskupuje se podle `KOD_LOKAL`.* **Pravidlo je správné a mění se nemá;
  u ryb ale nemá vstup.**
- **Stav v kódu:** [`02_n2k_data_druhy.R:170-186`](../../R/00_config/02_n2k_data_druhy.R#L170-L186)
  bere u ryb `KOD_LOKAL` z tagu `<naz_tok>`; bez něj platí obecná větev
  [`:228`](../../R/00_config/02_n2k_data_druhy.R#L228) `is.na(KOD_LOKALITY) ~ kod_chu`, tedy
  **kód EVL**. U ostatních skupin nese `KOD_LOKAL` kód lokality z `KOD_LOKALITY`.
- **Zjištění (Monitoring druhů ČR, od 2013):**

  | skupina | záznamů | s `KOD_LOKALITY` | příklad kódu |
  |---|---|---|---|
  | Obojživelníci | 13 276 | **78,6 %** (1 209 kódů) | `amp12`, `CZ0624119_25` |
  | Cévnaté rostliny | 6 858 | **46,7 %** (1 201 kódů) | `Jcya001`, `Alil009` |
  | **Ryby a mihule** | 2 788 | **0 %** | — |

  `<naz_tok>` („název toku") se u 94 % *Cottus* / 93 % *Lampetra* / 87 % *Misgurnus*
  shoduje s **názvem EVL nebo CHKO**, případně jej obsahuje („Šumava", „Blanský les",
  „CHKO Litovelské Pomoraví"). U EVL pojmenovaných po toku je to zároveň název toku — v obou
  případech je to **jeden klíč pro všechny úseky** v území. Zdroj klíče po filtru předmětů ochrany (od 2013): *Cottus* 975× `naz_tok` / 1 136× kód
  EVL; *Lampetra* 630 / 616; *Misgurnus* 236 / 399.
- **Důsledek — jedna „lokalita" = desítky úseků:**

  | | *Cottus* | *Lampetra* | *Misgurnus* |
  |---|---|---|---|
  | záznamů monitoringu sdílejících lokalitu-rok s jiným průzkumem | **465 z 1 063 (43,7 %)** | **236 z 701 (33,7 %)** | **131 z 246 (53,3 %)** |
  | max. průzkumů pod jedním klíčem v jednom roce | **68** | 51 | 19 |

  Příklad: *Cottus* „Šumava" (CZ0314024) — 133 záznamů ve 4 letech, 83 různých textů `LOKALITA`.
  Sloučené jsou všechny lokalitní ukazatele: `POP_ABUNDANCE` = maximum přes úseky, `POP_VITALITA`
  = sjednocení délkových skupin přes úseky, `POP_DYN` = poměr maxim různých úseků v různých letech
  a `LOK_PROCDOBR` počítá klíče, ne úseky. Stanovištní ukazatele bere `24` z prvního `POLE`
  (H-70), dopad je zde zanedbatelný (P-04).
- **Proč to nelze opravit seskupováním:** v exportu **neexistuje stabilní identita úseku** —
  `IDX_ND_LOK` je jedinečný pro každý záznam (133 záznamů „Šumava" = 133 hodnot) a `LOKALITA`
  má varianty zápisu (`Blanice - Krejčovice` / `Blanice – Krečovice`). Tag s kódem lokality
  mezi 101 tagy ryb není.
- **Řešení (návrh, neaplikováno):**
  1. **Trvalé — stejné jako u ostatních skupin:** kód monitorované lokality ryb přenášet ze
     Survey do NDOP v `KOD_LOKALITY` (správce formuláře / NDOP). Pak kód beze změny vezme
     `KOD_LOKALITY` jako u obojživelníků a větev `<naz_tok>` lze zrušit.
  2. **Přechodné:** odvozený klíč úseku (normalizovaný tok z `LOKALITA` + shluk souřadnic
     začátku úseku v toleranci, např. 200 m) zapsaný do `KOD_LOKAL` jen pro skupinu ryb.
     Návrh algoritmu a jeho ověření patří do Fáze 3, pokud jej zadavatel chce.
  3. Bez 1 nebo 2 jsou všechna pravidla PR-DYN, PR-VIT, PR-HAB a PR-EVL hodnocena nad
     sloučenými klíči — viz poznámka u každého nálezu.
- **Rozhodnutí zadavatele (2026-11-05):** *„just LOKALITA — the key is that it needs to be
  in CIL-MON."* `KOD_LOKAL` ryb = `LOKALITA` (bez normalizace zápisu); rozhoduje, aby
  záznamy lokality byly cíleným monitoringem. → patch F3-C, **ověřeno během**.
- **Co klíč `LOKALITA` dává** (monitoring od 2013, dvojice předmětů ochrany):

  | | *Cottus* | *Lampetra* | *Misgurnus* |
  |---|---|---|---|
  | lokalit dnes (`naz_tok` / kód EVL) | 235 | 176 | 69 |
  | lokalit podle `LOKALITA` | **492** | **367** | **179** |
  | — po sjednocení pomlček, mezer a velikosti písmen (jen informativně) | 425 | 322 | 175 |
  | tabulka metodiky (počet monitorovaných lokalit) | 221 | 174 | 69 |
  | lokalita-rok s více průzkumy | 83 → **9** | 56 → **4** | 26 → **4** |
  | lokalit s jediným rokem monitoringu | 272 (55 %) | 207 (56 %) | 139 (78 %) |

  Sloučení úseků zmizelo. Lokalit je ale zhruba **dvakrát víc**, než uvádí metodika: tentýž úsek
  se v různých letech jmenuje různě (`Blanice - Krejčovice` / `Blanice – Krečovice`), takže se
  řada rozpadá na jednoleté lokality — dynamika a vitalita jsou u nich nehodnotitelné.
  Sjednocení zápisu by spojilo 67 / 45 / 4 lokalit; trvalé řešení zůstává kód lokality
  v `KOD_LOKALITY` (varianta 1). Tytéž názvy ve dvou EVL: 3 lokality *Cottus* (12 záznamů)
  a 1 *Misgurnus* (2) — `21_1` je sloučí, protože seskupuje bez `kod_chu` (bez dopadu).
- **Rozhodnutí zadavatele ke Kontrolnímu bodu 2 (Z-16): sjednotit zápis — provedeno** (`06a849d`,
  [`02_n2k_data_druhy.R`](../../R/00_config/02_n2k_data_druhy.R), blok „Ryby - sjednoceni zapisu
  nazvu lokality"). Klíč = název po nahrazení en-dash / em-dash / minus pomlčkou, sjednocení mezer
  (i kolem pomlčky) a malých písmen; v rámci EVL se varianty nahradí **nejčastějším** zápisem (při
  shodě abecedně prvním), takže kód lokality zůstává čitelný název. Změřeno během „aplikovano":
  DP s cíleným monitoringem 494 → **428** (*Cottus*), 367 → **322** (*Lampetra*), 179 → **175**
  (*Misgurnus*). Překlepy (`Krejčovice` / `Krečovice`) se nesjednotí — to vyřeší až kód lokality.
- **CILMON u ryb:** všech **2 463** záznamů ryb od 2019 s ≥ 10 tagy ve `STRUKT_POZN` je
  cíleným monitoringem (`PROJEKT` = *Monitoring druhů ČR*); u ryb tedy v seznamu žádný zdroj
  nechybí. Seznam zdrojů se ale neuplatňuje u žádné skupiny — viz **H-89**.

### H-72 ⚠ — `POP_DYN` průměruje řádky, ne roky; jeden rok dat dává „stabilní" populaci
- **Závažnost:** kritická · **Typ:** BUG · **Stav:** **zaznamenáno** · **Odkazy:** PR-DYN-C, PR-DYN-M, H-47, H-11
- **Metodika** (*Cottus*, *Misgurnus*, §5.1): průměr posledních tří průzkumů **ze tří různých
  let** proti průměru prvních tří let; je-li méně než šest měření z různých let, dva první proti
  dvěma posledním; pokles o více než 50 % ⇒ špatný. *Misgurnus*: více měření v roce ⇒ nejvyšší.
- **Stav v kódu:**
  - [`21_1:1550-1765`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1550-L1765) staví
    `n2k_druhy_lokpop` přes `reframe()`; `POP_VITALITA` je v něm **řádkový vektor**, takže
    lokalita-rok má **tolik řádků, kolik řádků měl vstup** — a ten
    [`separate_rows()`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1515-L1518) rozložil na jeden
    řádek na **každou změřenou délku** (a na každou hodnotu `STA_PRUHLEDNOSTVODA`).
  - [`:1901`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1901) `mean(head(POP_ABUNDANCE, 3))` a
    [`:1931`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1931) `mean(head(na.omit(POP_ABUNDANCE), 3))`
    tedy berou **první tři řádky**, typicky z téhož roku.
  - [`:1995-2002`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1995-L2002) připojuje `POP_DYN`
    **bez `ROK`** — všechny ročníky lokality dostanou hodnotu z nejnovějších dat (táž třída jako
    H-11 u obojživelníků).
  - [`:1949`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1949) reference 0 a průměr 0 ⇒ 100
    (splněno); [`:1953`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1953) reference 0, průměr > 0
    ⇒ `Inf`, který `21_2` jako číslo nepřečte ⇒ nehodnoceno.
- **Měřený dopad (replikace):**

  | | *Cottus* | *Lampetra* | *Misgurnus* |
  |---|---|---|---|
  | řádků `lokpop` na lokalitu-rok: medián / max | 5 / 701 | 2 / 135 | 1 / 70 |
  | lokality s abundancí ve ≥ 2 letech | 156 | 122 | 4 |
  | z nich průměr i reference **z jediného roku** (fakticky 1 : 1) | **107 (68,6 %)** | **67 (54,9 %)** | 1 |
  | z nich skutečně 3 : 3 | 10 (6,4 %) | 0 | 0 |
  | `POP_DYN` = 100 z „0 / 0" (trvalá nepřítomnost ⇒ splněno) | 38 | 17 | 23 |

  Reprezentativní lokality mají abundanci nejvýše ve **4 letech** do hodnoceného roku —
  pravidlo „3 : 3" se tak neuplatní nikde a vždy platí větev „2 : 2". Srovnání verdiktu
  kódu a metodiky (2 : 2, roční maxima, okno do hodnoceného roku), *Cottus*:

  | kód \ metodika | splněno | nesplněno | nehodnotitelné |
  |---|---|---|---|
  | splněno | 122 | **5** | **103** |
  | nesplněno | **29** | 12 | — |

  **103 lokalit** *Cottus* (*Misgurnus* 25) kód hodnotí jako stabilní, ačkoli mají data
  z **jediného roku** — průměr a reference vzniknou z téhož čísla a poměr je 100 %.
- **Řešení (návrh, neaplikováno):** `POP_DYN` počítat v samostatné tabulce za **lokalitu a rok**
  (vzor `n2k_druhy_lokpop_period3`, [`21_1:1823-1839`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1823-L1839)):
  řada ročních maxim abundance (jeden řádek na rok), okno končící v hodnoceném roce,
  3 : 3 při ≥ 6 letech, jinak 2 : 2; méně let ⇒ `NA`. Připojit **přes `ROK`**. Hraniční
  případy (2–3 roky, kdy se okna 2 : 2 překrývají; „0 / 0"; „0 / > 0") jsou otázky na autory (R-11).

### H-73 ⚠ — *Lampetra*: dynamika má v metodice jiný algoritmus, kód jej nemá
- **Závažnost:** kritická · **Typ:** CHYBÍ · **Stav:** **zaznamenáno, výklad k rozhodnutí** · **Odkazy:** PR-DYN-L, R-12, H-49
- **Metodika** (§5.1, Tab. 1): výchozí hodnota = první odlov; „pakliže v následujících dvou
  odlovech dojde **při součtu relativních změn** početnosti (počítáno od výchozí hodnoty)
  k poklesu o více než 50 %" ⇒ špatný. Po čtyřech průzkumech v různých letech je výchozí
  hodnotou **průměr těchto čtyř**; dále se hodnotí součet relativních změn **posledních dvou**
  odlovů vůči němu. Hodnocenou jednotkou je **počet larev**.
- **Stav v kódu:** tentýž `POP_DYN` jako u ostatních druhů (H-72), počítaný z celkového
  `POCET` (H-74), limit `min 50`.
- **Výklad — tři čtení, která se v datech liší:** s výchozí hodnotou *N*₀ a posledními dvěma
  odlovy *N*₁, *N*₂ je stav špatný, když

  | | vzorec | čtení |
  |---|---|---|
  | **A** | (*N*₁ − *N*₀)/*N*₀ + (*N*₂ − *N*₀)/*N*₀ < −0,5 | doslovně „součet relativních změn" — dva poklesy po 30 % dají −60 % ⇒ špatný |
  | **B** | ½ · [(*N*₁ − *N*₀)/*N*₀ + (*N*₂ − *N*₀)/*N*₀] < −0,5 ⇔ (*N*₁ + *N*₂)/2 < 0,5 *N*₀ | průměrná změna — totéž co *Cottus* 2 : 2 s jedním výchozím údajem |
  | **C** | *N*₁ < 0,5 *N*₀ **a** *N*₂ < 0,5 *N*₀ | oba poslední odlovy pod polovinou |

  Na lokalitách *Lampetra* se ≥ 3 průzkumy v různých letech (67 klíčů) označí za špatné:
  **A 26, B 21, C 15.** Dalších **90 klíčů** má méně než tři průzkumy a podle žádného čtení
  hodnotitelné není (kód je dnes hodnotí 1 : 1, H-72).
- **Doporučení:** **B** — je to jediné čtení, u kterého práh „50 %" znamená totéž co
  u ostatních dvou druhů a nezávisí na počtu sčítaných odlovů. A je doslovnější, ale
  s každým dalším sčítancem se práh fakticky zpřísňuje. **Rozhoduje zadavatel, potvrdit
  autoři metodiky.**
- **Rozhodnutí zadavatele:** _(A, B, nebo C?)_

### H-75 ⚠ — Verdikt lokality ryb: zhoršený až od dvou nesplněných stanovištních parametrů (obojživelníci), metodika žádá jeden
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY · **Stav:** **zaznamenáno** · **Odkazy:** PR-DP-C/L/M, R-08, H-21
- **Metodika** (§5.3, všechny tři druhy): „Pokud vyhoví všechny klíčové populační parametry,
  ale **nevyhoví alespoň jeden** hodnocený stanovištní parametr, je stav hodnocen jako zhoršený."
  *Cottus* to dokládá i rozhodovací tabulkou.
- **Stav v kódu:** [`24:208`](../../R/02_druhy/24_n2k_druhy_lokality.R#L208)
  `N_OTH_FAIL >= 2 ~ 0.5` pro všechny skupiny (Tabulka 1 metodiky obojživelníků).
  Odstavec *Misgurnus* §5.1 „Kód hodnotí stanoviště piskoře…" popisuje právě tuto větev a odporuje
  §5.3 téže kapitoly — je to **text přepsaný z kódu**, ne metodika (R-08).
- **Měřený dopad (replikace, reprezentativní návštěvy, dnešní limity):**

  | | *Cottus* | *Lampetra* | *Misgurnus* |
  |---|---|---|---|
  | lokalit | 300 | 200 | 73 |
  | dobrý → **zhoršený** při pravidle „≥ 1" | **31** | **38** | **11** |
  | dobrý zůstane | 69 | 24 | 3 |

  Změna se dnešními limity sčítá s nevhodnou sadou stanovištních indikátorů (H-76) — po
  přechodu na sadu metodiky bude dopad jiný (Fáze 3, testovací běh).
- **Řešení (návrh, neaplikováno):** podmínit práh skupinou — `24` obsluhuje obojživelníky, hmyz,
  savce i rostliny. Vzor je návrh u cévnatých H-23:
  ```r
  # skupina_druhu je v 24 uz urcena (radek 35)
  prah_zhorseny <- if (identical(skupina_druhu, "Ryby a mihule")) 1L else 2L
  # ...
  CELKOVE = dplyr::case_when(
    is.na(MAX_CILMON) ~ NA_real_,
    N_KEY_EXPECTED > 0 & N_KEY_PASSED < N_KEY_EXPECTED ~ 0,
    N_OTH_FAIL >= prah_zhorseny ~ 0.5,
    TRUE ~ 1
  )
  ```

### H-81 ⚠ — Úroveň EVL: jen *Lampetra* má indikátor, s jiným prahem, a stav „špatný" nemůže vzniknout
- **Závažnost:** kritická · **Typ:** ROZPOR-METODIKY + BUG · **Stav:** **zaznamenáno** · **Odkazy:** PR-EVL-1/2/3, H-41, H-50, H-05, R-10
- **Metodika** (§5.4): podíl hodnocených lokalit v dobrém stavu (neznámé se nezapočítávají);
  agregovaný stav dobrý při **≥ 75 %**, při třech lokalitách stačí dvě. Pak tabulka:
  lokality dobré + prostupnost dobrá ⇒ dobrý; lokality dobré + prostupnost špatná ⇒ zhoršený;
  **lokality špatné ⇒ špatný** bez ohledu na prostupnost.
- **Stav v kódu a v limitech:**
  1. `UROVEN = chu` má jen `Lampetra planeri;LOK_PROCDOBR;min;50` — **50 % místo 75 %**.
     *Cottus* a *Misgurnus* nemají řádek `chu` (H-41), takže **výstup EVL nevzniká vůbec**.
  2. [`25:374-384`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L374-L384): s jediným klíčovým
     indikátorem `chu` je `LENIND_SUMKLIC = 1` a první podmínka `IND_SUMKLIC < 1 − 1` nemůže
     nastat — **EVL je dobrá, nebo zhoršená, nikdy špatná**. Totéž, co u obojživelníků řešilo
     H-05 Tabulkou 2; tu H-50 pro ryby (správně) vypnulo, takže vada se u ryb vrátila.
  3. Migrační prostupnost na úrovni EVL není (H-83), rozhodovací tabulka 2×2 také ne.
  4. `LOK_POCETDOB` počítá jen „dobrý" ([`25:192`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L192))
     — odpovídá *Lampetra* a *Misgurnus* („v dobrém stavu"); *Cottus* píše „v příznivém stavu
     na základě klíčových parametrů", což lze číst i jako dobrý + zhoršený (R-10).
- **Pravidlo „≥ 75 %, při třech dvě" pro 1–2 hodnocené lokality:** metodika neřeší. Dnešní
  kód s `min 50` dává pro *n* = 1: 1/1; *n* = 2: 1/2 stačí; *n* = 3: 2/3. Doslovné 75 %:
  *n* = 1: 1/1; *n* = 2: 2/2; *n* = 4: 3/4. Počet hodnocených lokalit (klíčů, H-71) na EVL:

  | hodnocených lokalit | 1 | 2 | 3 | 4 | 5 | ≥ 6 |
  |---|---|---|---|---|---|---|
  | *Cottus* (30 EVL) | 3 | 1 | 8 | 3 | 3 | 12 |
  | *Lampetra* (27 EVL) | 2 | 5 | 4 | 6 | 4 | 6 |
  | *Misgurnus* (9 EVL) | — | — | — | 1 | 2 | 6 |

- **Měřený dopad (replikace; agregované lokality dobré):**

  | | EVL | 50 %, DP „≥ 2" (dnes u *Lampetra*) | 75 %, DP „≥ 2" | 50 %, DP „≥ 1" | **75 %, DP „≥ 1" (metodika)** |
  |---|---|---|---|---|---|
  | *Cottus* | 30 | 8 | 4 | 5 | **3** |
  | *Lampetra* | 27 | 10 | 5 | 4 | **1** |
  | *Misgurnus* | 9 | 1 | 1 | 0 | **0** |

  Čísla jsou nad sloučenými klíči (H-71) a s dnešními lokálními indikátory — slouží k řádu
  velikosti, ne jako výsledek.
- **Řešení (návrh, neaplikováno):** `LOK_PROCDOBR;min;75;…;ano;chu` pro všechny tři druhy;
  výjimku „při třech dvě" a případ *n* < 3 řešit v `25` explicitně (podmíněně skupinou);
  verdikt EVL u ryb odvodit **rozhodovací tabulkou** (agregované lokality × prostupnost, H-83)
  místo aritmetiky nad počtem splněných indikátorů — týž přístup jako H-05/H-06 u obojživelníků.

---

# Vysoká závažnost

### H-74 ⚠ — Co je „počet": oba odlovy dohromady, u mihule i dospělci a spatření jedinci; počty po odlovech nejsou v tazích
- **Závažnost:** vysoká · **Typ:** DATA + ROZPOR-METODIKY · **Stav:** **zaznamenáno** · **Odkazy:** PR-ODL-C/L/M, PR-ABU, H-48, H-49, R-14
- **Metodika:** dva odlovy, **počty se uvádějí zvlášť**. *Lampetra*: 1. odlov kontinuálně celým
  profilem, 2. odlov **bodovou metodou jen na jemných náplavech** (s plochou prolovených
  a odhadem celkové plochy náplavů); počítají se i spatření neulovení jedinci; dynamika se
  hodnotí z **larev**. Pro odhad početnosti plocha celého úseku. Literatura (Cowx 1991,
  Raleigh & Short 1981) je o odhadu velikosti populace z odlovů s odstraňováním, metodika
  ale nikde neříká, že se odhad (depletion) počítá — hodnotí se tedy úlovek.
- **Stav v kódu:** `POP_POCET` = `POCET` záznamu, je-li `POCITANO` mezi jednotkami limitu
  ([`21_1:556-561`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L556-L561)); abundance =
  `POP_POCET / POP_PLOCHALOV` ([`:689`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L689)).
- **Zjištění — z čeho se `POCET` skládá** (text v `POZNAMKA`, viz níže):

  | | *Cottus* | *Misgurnus* | *Lampetra* |
  |---|---|---|---|
  | `POCET` = 1. + 2. odlov | 342 | 56 | — |
  | `POCET` = 1. + 2. odlov **+ spatření** | 131 | 6 | — |
  | `POCET` = minohy + **dospělci** (oba odlovy) **+ spatření** | — | — | **297 z 297** |

  Dospělci jsou v 136 záznamech *Lampetra*, spatřené minohy ve 102. U mihule se tedy do dynamiky
  sčítají larvy s dospělci a kontinuální 1. odlov s bodovým 2. odlovem na náplavech. Záznamy
  s `POCITANO` = `ks/ha`, `ks/km` nebo `mrtví jedinci` (*Cottus* 43, *Lampetra* 13, *Misgurnus* 4)
  kód tiše vynechá — správně, ale nevykazuje to.
- **Počty po odlovech nejsou v tazích.** Mezi **všemi 101 tagy všech 18 druhů ryb** není
  žádný s počty odlovů. Jsou jen jako text v `POZNAMKA`, ve tvaru
  *„Počet ulovených jedinců v prvním lovu: X; … v druhém lovu: Y; Počet spatřených
  (neulovených) jedinců: Z; Abundance: N"* (u mihule zvlášť minohy a adulti) — **1 844 záznamů
  ryb, roky 2019–2024**; u tří druhů 686 / 412 / 214. „Abundance" v textu je ind./ha: u *Cottus*
  (kontinuální lov) = (1. + 2. odlov) / plocha × 10⁴; u *Lampetra* žádnému jednoduchému vzorci
  neodpovídá (290 z 296).
- **Řešení (návrh):** zadavatel 2026-11-05: *„analyzovat data, ale mají to být tagy"* —
  1. **Trvalé:** přenášet počty z formuláře do `STRUKT_POZN` jako tagy (návrh názvů v revizi,
     oddíl Datové mezery): 1. a 2. odlov, spatření, u mihule minohy a adulti zvlášť.
  2. **Přechodné:** čtení textu `POZNAMKA` regulárním výrazem (výše uvedený tvar je strojový,
     2019–2024 bez odchylek). Pouze pokud to zadavatel povolí; jinak zůstává `POCET`.
  3. Kterou veličinu hodnotit (úlovek obou odlovů, jen 1. odlov, se spatřenými či bez; u mihule
     jen larvy) rozhodují autoři metodiky (R-14).
- **Rozhodnutí zadavatele (2026-11-05, po Kontrolním bodu 3):** *„pro populační metriky použít
  abundanci — `POCET` přepočtený na plochu."* Provedeno (`7722980`): u tří druhů
  `POP_ABUNDANCENAL = POP_POCET / POP_PLOCHALOV` (nulový úlovek = 0, H-93); počty 1. a 2. odlovu
  z `POZNAMKA` (Z-3) zůstávají jen jako informativní `POP_POCETODLOV`. `POCET` je u cíleného
  monitoringu vyplněn téměř vždy (jednotka „jedinci"); oproti součtu odlovů je vyšší u 71 %
  záznamů *Lampetra* (dospělci a spatření jedinci) a 20 % *Cottus*.
- **Ověřeno během** ([`Outputs/Test/Ryby_20261105/abundance_pocet/`](../../Outputs/Test/Ryby_20261105/abundance_pocet/)):
  abundance se změnila u 129 záznamů *Cottus*, 173 *Lampetra* a 2 *Misgurnus* (téměř vždy vyšší),
  hodnota `POP_DYN` u 61 a 37 DP; splnění limitu se změnilo u 2 DP *Lampetra*, obě jsou kvůli
  nehodnocené vitalitě neznámé. **Verdikty DP ani EVL se nezměnily.**

### H-76 ⚠ — Stanovištní indikátory neodpovídají metodice; zdroje pro parametry metodiky v datech jsou
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Stav:** **zaznamenáno, návrh k rozhodnutí** · **Odkazy:** PR-HAB-C, PR-HAB-L1/L2, PR-HAB-M1/M2, H-42, H-45, H-60
- **Metodika vs. limity:**

  | druh | metodika (§5.1–5.2) | `limity_ryby.csv` dnes |
  |---|---|---|
  | *Cottus* | jediný parametr: součet relativní pokryvnosti **kameny + balvany + štěrk > 50 %** (Tab. 1 „minimálně 50 %") | `STA_DNO` (balvany/kameny/štěrk), `STA_DNOPREF` (kameny/**písek**/štěrk), `STA_DNOPOCET` ≥ 3, `STA_DNOTYPSOUCETPROCENT` ≥ 50 %, `STA_PROUD`, `STA_PROUDPOCETTYPU` ≥ 2 |
  | *Lampetra* | jemné náplavy ≥ 10 m² na 100 m; štěrkopísčité proudné úseky ≥ 10 m² na 100 m | `STA_DNOTYP` = „kompaktní jílové dno" (H-45); oba `STA_ODHADCELKOVEPLOCHY…` jako `val 10 m2` |
  | *Misgurnus* | bahno + písek ≥ 50 % dna; submerzní makrofyta ≥ 11 % (Tab. 1 ≥ 10 %); dobrý = oba | `STA_DNO` (bahno/písek), `STA_DNOPOCETTYPU` ≥ 2, `STA_PROUD` (**peřeje, kaskády**, mírný, tůně), `STA_VEGETACE` = submerzní |

  Navíc `STA_MIGBARVYS` a `STA_MIGBARPOCET` u všech tří — patří na úroveň EVL (H-83).
  Dnes hodnocené stanovištní indikátory (bez bariér), které metodice neodpovídají: *Cottus* 4
  (`STA_DNO`, `STA_DNOPREF`, `STA_PROUD`, `STA_PROUDPOCETTYPU`), *Lampetra* 1 (`STA_DNOTYP`),
  *Misgurnus* 4 — z nich `STA_DNO` a `STA_VEGETACE` jsou hrubé zástupné ukazatele parametrů
  metodiky (přítomnost místo podílu).
- **Dnešní stav na záznamech monitoringu** (splněno / nesplněno): *Lampetra* `STA_DNOTYP` **16 / 603**;
  *Misgurnus* `STA_DNOPOCETTYPU` 126 / 110, `STA_VEGETACE` 134 / 95; *Cottus* `STA_PROUDPOCETTYPU`
  816 / 105. Ostatní téměř vždy splněny.
- **Zdroje parametrů metodiky v datech** (Monitoring od 2013, *Cottus* / *Lampetra* / *Misgurnus*):

  | parametr | tagy | záznamů | doména |
  |---|---|---|---|
  | pokryvnost substrátů | `rsub_kam`, `rsub_bal`, `rsub_st`, `rsub_pis`, `rsub_bah` (+ `jild`, `ksp`, `umel`, `jiny`) | 933 / 615 / 226 | pásma *Vzácný (0-10 %)*, *Běžný (11-25 %)*, *Hojný (26-50 %)*, *Velmi hojný (51-75 %)*, *Převažující (> 75 %)*, *Dominantní (100%)* |
  | submerzní vegetace | `veg_tok_sub` (+ `veg_tok` = „Bez vegetace") | 274 / 136 / 129 (+ 394 / 287 / 59 bez vegetace) | táž pásma |
  | jemné náplavy, trdliště | `odhad_j_n`, `odhad_p_u` | — / 446 / — | m² (čtení názvu tagu, k potvrzení) |
  | délka úseku | `delka_prolov_u` | 774 / 535 / 49 | m, převážně 100 |

  Substrát uvedený v `<sub_dno>` bez pásma v `rsub_*` je vzácný (≤ 7 záznamů na substrát).
- **Převod pásma na číslo — návrh sladěný s obojživelníky a cévnatými** (zadání bodu 5):
  - **Obojživelníci (H-60):** kategorie se převádí na **medián** („500 jedinců pro kategorii stovky").
  - **Cévnaté (PR-12, PR-16):** *součet* měr poškození přes typy se počítá z **mediánů**
    pásem (5,5 / 30,5 / 75,5); *jednotlivé* pásmo stanovištního indikátoru se hodnotí výčtem
    přípustných kategorií (`val`).
  - **Ryby — tedy:** **součtový parametr** (*Cottus* K + B + Š; *Misgurnus* bahno + písek) =
    součet **středů pásem** — 5 / 18 / 38 / 63 / 87,5 / 100, substrát neuvedený v `<sub_dno>` = 0,
    strop 100; **jednopásmový parametr** (*Misgurnus* submerzní vegetace) = `val` s výčtem
    pásem od *Běžný (11-25 %)* výše. Práh 11 % leží přesně na hranici pásem, takže výčet je
    přesný bez volby bodu; „Bez vegetace" = nesplněno. Práh 10 % z Tab. 1 by padl dovnitř pásma
    0–10 a rozhodnout jej nelze (R-02).
- **Citlivost (záznamy monitoringu):** *Cottus* — součet středů > 50 % u **771 z 931**;
  jistě splněno (součet dolních mezí > 50) 560, jistě nesplněno (součet horních mezí < 50) 89,
  **nejistých 282 (30 %)**. Hodnota přesně 50 nevznikla ani jednou, takže rozpor „> 50" vs. „≥ 50"
  (R-02) výsledek nemění. *Misgurnus* — bahno + písek ≥ 50 u 168 z 235 (nejistých 50),
  vegetace ≥ 11 % u 128 z 224, obojí 96 z 224. *Lampetra* — náplavy ≥ 10 m²/100 m u 153 z 351,
  trdliště 278 z 349; **95 záznamů** má odhad bez délky úseku (normalizace na 100 m nemožná).
- **Reprezentativní návštěvy (splněno / nesplněno):** *Cottus* 214 / 53; *Lampetra* náplavy
  45 / 77, trdliště 83 / 39; *Misgurnus* substrát 48 / 20, vegetace 48 / 20.
- **Řešení (Fáze 3):** v `limity_ryby_2026.csv` nahradit řádky sadou metodiky; v `21_1` doplnit
  výpočty (H-77). Rozhodnutí o převodu je na zadavateli.

### H-77 ⚠ — Čtyři sirotci a oprava H-38 / H-42: zdrojové tagy existují
- **Závažnost:** vysoká · **Typ:** GAP + OPRAVA DŘÍVĚJŠÍHO NÁLEZU · **Stav:** **zaznamenáno** · **Odkazy:** H-38, H-42, H-54, PR-HAB-*
- **Potvrzeno:** bez výpočtu v `R/` jsou u tří druhů právě **4 `ID_IND`**:
  `STA_DNOPOCET` a `STA_DNOTYPSOUCETPROCENT` (*Cottus*), oba `STA_ODHADCELKOVEPLOCHY…`
  (*Lampetra*). Kód počítá `STA_DNOPOCETTYPU` (používá jen *Misgurnus*), ne `STA_DNOPOCET`.
- **Oprava H-42:** H-42 uvádělo, že pro oba `STA_ODHADCELKOVEPLOCHY…` „v datech není
  odpovídající tag" a že `STA_DNOTYPSOUCETPROCENT` vyžaduje rozhodnutí H-26. **Obojí už neplatí:**
  `<odhad_j_n>` (446 záznamů) a `<odhad_p_u>` (444) jsou u *Lampetra* číselné plochy;
  `<rsub_*>` nese pokryvnost po substrátech a H-26 bylo uzavřeno mediánem (H-60).
- **Co s nimi:** `STA_DNOPOCET` nemá v nové metodice oporu ⇒ vypustit. `STA_DNOTYPSOUCETPROCENT`
  = parametr *Cottus* (H-76). Oba plošné indikátory *Lampetra* = parametry metodiky, ale
  **`TYP_IND = min`** (ne `val`, H-54) a hodnota **normalizovaná na 100 m** (`odhad / delka_prolov_u × 100`).
  Dlouhé `ID_IND` lze ponechat (jsou v limitech už dnes), nebo zkrátit — ani jedno nemá `ind_id` (H-88).

### H-78 ⚠ — Délkové skupiny: hranice 60 mm místo 50 mm; délkový typ neznámý
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY + DATA · **Stav:** **zaznamenáno** · **Odkazy:** PR-VIT-C/L/M, R-06, H-51
- **Metodika:** *Cottus* < 5, 5–9, > 9 cm; *Lampetra* (larvy) < 5, 5–12, > 12 cm; *Misgurnus*
  < 5, 5–15, > 15 cm.
- **Číselník** [`cis_ryby_delky_strukt.csv`](../../Data/Input/cis_ryby_delky_strukt.csv):
  *Cottus* 0,01–60 / 60,01–90 / > 90 mm; *Lampetra* 0,01–60 / 60,01–120 / > 120; *Misgurnus*
  0,01–50 / 50,01–150 / > 150. Párování [`21_1:1525-1534`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1525-L1534)
  `>= MIN` a `<= MAX` (horní mez patří do skupiny).
- **Měřený dopad:** metodika zařadí jinak **3 031 délek *Cottus*** a **590 *Lampetra*** (50–60 mm,
  skupina 1 → 2) a 3 délky *Misgurnus* (přesně 50 mm — „< 5 cm" je ostrá nerovnost). Splnění
  „tři skupiny v roce" na reprezentativních návštěvách klesne *Cottus* 103 → 87, *Lampetra* 45 → 29.
- **Délkový typ:** *Cottus* §3.1 SL (*longitudo corporis*), poznámka Tab. 1 celková délka
  (R-06). Data to neprozradí — `<velikosti>` je jen seznam čísel (medián *Cottus* 65 mm). SL je
  u vranky zhruba 0,85 TL, takže hranice 50 mm odpovídá ~ 59 mm TL — **volba typu posouvá tytéž
  ryby přes hranici.**
- **Kvalita dat:** odlehlé hodnoty (*Cottus* 6 570 mm, *Lampetra* 1 332 mm), nuly (51 / 17 / 85 —
  „0" u negativních záznamů, číselník je správně vynechá), 5 nečíselných tokenů (`110 90`),
  62 záznamů *Cottus* s více než 50 změřenými (max. 304), 26 záznamů s více délkami než `POCET`.
- **Řešení (Fáze 3):** `cis_ryby_delky_strukt_2026.csv` s hranicemi metodiky (*Cottus* 0,01–49,99 /
  50–90 / > 90; *Lampetra* 0,01–49,99 / 50–120 / > 120; *Misgurnus* 0,01–49,99 / 50–150 / > 150).
  Ostrá nerovnost „< 5 cm" se zapíše `MAX = 49.99`, protože délky jsou v celých mm (desetinnou
  má v datech jediná délka) a párování `<= MAX` se tím nemění. Typ délky rozhodnou autoři.

### H-79 ⚠ — Vitalita: hodnotí se v rámci roku, ne „alespoň ve dvou ze tří posledních průzkumů"
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Stav:** **zaznamenáno** · **Odkazy:** PR-VIT-C/L/M, R-13, H-51, H-71
- **Metodika:** všechny tři délkové skupiny **alespoň ve dvou** z posledních tří průzkumů
  v různých letech.
- **Stav v kódu:** `POP_VITALITA` = počet různých skupin za lokalitu-rok
  ([`21_1:1629-1639`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1629-L1639)), limit `min 3`. Víceleté
  okno nikde není. Se sloučenými úseky (H-71) se skupiny sjednocují přes úseky.
- **Měřený dopad (reprezentativní návštěvy, splněno):**

  | | *Cottus* | *Lampetra* | *Misgurnus* |
  |---|---|---|---|
  | kód (skupiny v roce, dnešní číselník) | 103 | 45 | **0** |
  | metodika (hranice 50 mm, ≥ 2 ze 3 let) | **50** | **13** | **0** |
  | v okně jen 1 průzkum / 2 / 3 | 71 / 56 / 133 | 30 / 64 / 63 | 32 / 22 / 14 |

  Lokalita s jediným průzkumem v okně podmínku „ve dvou průzkumech" splnit nemůže — zda je
  pak vitalita špatná, nebo neznámá, metodika neříká (R-13).
- ***Misgurnus* nesplní vitalitu nikde** (0 ze 73, ani v jednom roce). Protože je klíčová,
  je každá hodnocená lokalita piskoře „špatná" bez ohledu na ostatní data — otázka
  proveditelnosti limitu pro autory (R-13).
- ***Lampetra*: délky dospělců jsou v témž seznamu.** 135 ze 136 záznamů s dospělci má
  `<velikosti>`; dospělci (12–16 cm) tak naplní skupinu „> 12 cm", která má podle metodiky
  patřit **larvám**. Metodika navíc sama uvádí, že larvy < 5 cm se elektrolovem loví „jen
  zcela výjimečně" — pravidlo je pro mihuli obtížně splnitelné (R-13).
- **Řešení (Fáze 3):** vitalitu počítat v tabulce za lokalitu a rok jako klouzavé okno tří
  posledních průzkumů (vzor `roll3_sum`, [`21_1:202-209`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L202-L209)),
  hodnota = počet průzkumů se všemi třemi skupinami, limit `min 2`. Okno s méně průzkumy
  podle rozhodnutí (R-13).

### H-80 ⚠ — `POP_PRESENCE` je klíčový indikátor, metodika jej nezná
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Stav:** **otázka na zadavatele** · **Odkazy:** PR-POP, H-56, H-58
- **Stav:** `POP_PRESENCE;val;ano;…;ano;lok` u všech tří druhů (zavedeno H-56/H-58). Metodika
  zná jako klíčové jen **dynamiku a vitalitu**; nepřítomnost se v ní projeví nulovou vitalitou
  a poklesem početnosti, ale **ne v jediném roce** — vitalita se hodnotí přes tři průzkumy.
- **Dopad:** na reprezentativní návštěvě nesplní přítomnost 69 lokalit *Cottus*, 39 *Lampetra*,
  43 *Misgurnus* — ty jsou „špatné" už z tohoto důvodu.
- **Možnosti:** (a) ponechat jako klíčový (přísnější než metodika); (b) `info` (zobrazuje se,
  nehodnotí — vzor H-33 / cévnaté PR-16); (c) vypustit. **Doporučení: (b).**
- **Rozhodnutí zadavatele:** _(a / b / c?)_

### H-83 ⚠ — Migrační prostupnost: v kódu neklíčový indikátor lokality, metodika ji hodnotí na úrovni EVL
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY · **Stav:** **zaznamenáno, výklad k rozhodnutí** · **Odkazy:** PR-MIG-C/L/M, H-52, R-07
- **Metodika:** parametr **EVL**; dobrý jen při nepřítomnosti příčné překážky přes celý profil
  vyšší než 10 cm (*Cottus*, *Misgurnus*) / 5 cm (*Lampetra*) v monitorovaných lokalitách
  a jejich okolí (~ 500 m); existuje-li hodnocení prostupnosti celého toku v EVL, použije se
  (u *Cottus* text tuto přednost neuvádí, R-07).
- **Stav v kódu a limitech:** `STA_MIGBARVYS max 10 / 5 cm` a `STA_MIGBARPOCET max 1`, oba
  `KLIC = ne`, `UROVEN = lok` — bariéra může lokalitu jen srazit na zhoršený a na EVL
  nepůsobí. `max` je `<=` ([`21_2:274-275`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L274-L275)):
  výška 10 cm vyhoví (správně — „vyšší než 10 cm"), ale **`max 1` připouští jednu bariéru**.
- **Data:** tag `<pocet_bar>` je v 974 / 630 / 236 záznamech, **vyplněn jen v 224 / 164 / 16**;
  prázdný tag je dnes `NA` (nehodnoceno). Zda prázdný znamená „bez bariéry", nebo
  „nezjišťováno", data neříkají. O „celém profilu" ani o vzdálenosti od úseku informace není.
  [`Limity_migracnibariery .xlsx`](../../Limity_migracnibariery%20.xlsx) **není** hodnocení
  prostupnosti toku — je to návrh prahů výšky po druzích ve formátu limitů (H-88).
- **Měřený dopad (úroveň EVL, monitoring od 2013, dvojice předmětů ochrany):** alespoň jedna
  bariéra nad limitem v **19 z 30 EVL *Cottus*, 19 z 27 *Lampetra*, 4 z 9 *Misgurnus***
  (bez bariéry nad limitem, ale se zaznamenaným tagem: 10 / 8 / 5). Dnes na úrovni lokality:
  `STA_MIGBARVYS` nesplněn u 185 / 148 / 14 záznamů.
- **Řešení (návrh, neaplikováno):** nový indikátor `STA_MIGPROSTUP` s `UROVEN = chu`, `KLIC = ano`,
  hodnota = nejvyšší výška bariéry ze záznamů monitoringu EVL v hodnoceném období (podle
  rozhodnutí: poslední průzkum každé lokality, nebo celé okno), `max 10` / `max 5`; z lokální
  úrovně oba `STA_MIGBAR*` vypustit, nebo převést na `info`. Externí hodnocení celého toku
  zatím v repozitáři není — evidovat jako datovou mezeru.
- **Rozhodnutí zadavatele:** _(agregace v čase; význam prázdného tagu)_

### H-85 ⚠ — Filtr předmětů ochrany není párový; výstup DP vzniká i v EVL, kde druh předmětem ochrany není
- **Závažnost:** vysoká · **Typ:** BUG · **Stav:** **zaznamenáno** · **Odkazy:** PR-EVL-0
- **Stav v kódu:** [`21_1:517-521`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L517-L521) a
  [`27:178-181`](../../R/02_druhy/27_n2k_druhy_zapis.R#L178-L181) testují `DRUH %in% nazev_lat`
  a `kod_chu %in% site_code` **zvlášť**, ne dvojici. Export EVL páruje
  ([`27:720-725`](../../R/02_druhy/27_n2k_druhy_zapis.R#L720-L725)), export DP ne
  ([`27:515-587`](../../R/02_druhy/27_n2k_druhy_zapis.R#L515-L587)).
- **Měřený dopad (od 2013):** projde **585** záznamů *Cottus* (196 z monitoringu), **326**
  *Lampetra* (67) a **119** *Misgurnus* (13) z EVL, kde druh předmětem ochrany není (např.
  *Cottus* v CZ0424031 České Švýcarsko 46, *Lampetra* v CZ0524044 Krkonoše 42). Ve výstupu DP
  jsou, na úrovni EVL ne. Sdílený kód — týká se všech skupin.
- **Řešení (návrh):** párový `semi_join` se seznamem předmětů ochrany v `21_1` a `27`
  (výjimky *Eriogaster catax*, *Euphydryas aurinia* zachovat). Dopad mimo ryby změřit.

---

# Střední závažnost

### H-82 — Latentní „dobrý" na úrovni EVL, když není hodnocena žádná lokalita
- **Závažnost:** střední · **Typ:** BUG (latentní) · **Stav:** **zaznamenáno** · **Odkazy:** H-53, H-81
- **Mechanismus:** `LOK_PROCDOBR = NA`, když `LOK_POCETSUM = 0`
  ([`25:193`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L193)); [`25:265-268`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L265-L268)
  pak přepíše `KLIC` na „ne", `LENIND_SUMKLIC = 0` a podmínka se zvrhne na `0 >= 0` ⇒ **dobrý**.
  Táž past jako H-53, jen jiným vstupem (prázdná hodnota místo prázdného `KLIC`).
- **Výskyt:** *Lampetra* v CZ0213050 (Ohrazenický potok) — 2 záznamy, žádný monitoring ⇒
  v `Data/Temp/n2k_druhy_chu.csv` „dobrý". **Do exportu se nedostane**, protože
  [`27:750`](../../R/02_druhy/27_n2k_druhy_zapis.R#L750) filtruje `CILMON_CHU == 1`. Ožije, jakmile
  vznikne EVL s monitoringem, ale bez hodnocené lokality.
- **Řešení:** v `25` větev `LENIND_SUMKLIC == 0 ~ NA` (neznámý); řeší se spolu s H-81.

### H-84 — Sezóna, teplota a podmínky odlovu se nekontrolují
- **Závažnost:** střední · **Typ:** CHYBÍ (informativní) · **Stav:** **zaznamenáno, bez zásahu** · **Odkazy:** PR-SEZ-C/L/M
- **Metodika:** *Cottus*, *Misgurnus* konec srpna – začátek listopadu, *Lampetra* září–říjen;
  voda ≥ 5 °C; vzduch ≤ 25 °C (*Cottus*, *Misgurnus*); ne za zvýšených průtoků, zákalu a deště.
- **Stav:** kód nic nefiltruje ani neoznačuje. V datech jsou `<tep_vody>` / `<Teplota>`,
  `<vod_stav>`, `<typ_pruh>`, `<metod_lov>`; teplota vzduchu a déšť nejsou.
- **Data (monitoring od 2013, *C* / *L* / *M*):** mimo okno (orientačně 15. 8. – 15. 11.,
  resp. IX–X) **54 / 66 / 92**; voda < 5 °C 7 / 1 / 8 (teplota zapsána u 348 / 137 / 198);
  nadprůměrný stav vody 17 / 16 / 12; „dno není viditelné" 37 / 2 / 45; bodová metoda 186 / 94 / 85.
- **Řešení:** zadání nepovoluje filtry bez souhlasu. Návrh: informativní příznak
  `MIMO_PODMINKY` ve výstupu DP; filtr až po rozhodnutí. U *Misgurnus* je mimo okno 37 %
  záznamů monitoringu — filtr by výrazně zmenšil vstup.

---

# Nízká závažnost

### H-86 — Hodnocené období nemá horní mez
- **Závažnost:** nízká · **Typ:** BUG · **Stav:** **zaznamenáno**
- [`21_1:511`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L511) a [`27:177`](../../R/02_druhy/27_n2k_druhy_zapis.R#L177)
  filtrují jen `ROK >= current_year - 12`. Při hodnocení roku 2025 projdou 2 záznamy *Lampetra*
  z ledna 2026 (Mobilní sběr dat, CZ0424031). Mimo monitoring, takže reprezentativní návštěvu
  nevyhrají; sdílený kód.

### H-87 — Rybí indikátory nemají `ind_id` v číselníku ISOP
- **Závažnost:** nízká · **Typ:** STOPA-DO-ISOP · **Stav:** **zaznamenáno**
- V [`cis_indikatory_popis.csv`](../../Data/Input/cis_indikatory_popis.csv) mají kód jen `POP_PRESENCE` (27)
  a `LOK_PROCDOBR` (189). `POP_DYN`, `POP_VITALITA`, všechny `STA_*` ryb i nově navrhované
  indikátory jdou do exportu jako surový `ID_IND` ([`27:785`](../../R/02_druhy/27_n2k_druhy_zapis.R#L785)).

### H-88 — Volné soubory v kořeni repozitáře
- **Závažnost:** nízká · **Typ:** ÚKLID · **Stav:** **zaznamenáno, otázka na zadavatele**

  | soubor | role | stav |
  |---|---|---|
  | `Chemdata11.R` + `chemdata.Rproj` | samostatný prototyp: chemie vody z ARROW v bufferu 500 m kolem EVL ryb, mediány proti limitům | **pipeline jej nečte**; v repozitáři nespustitelný (čte `Data/Limity_ryby_Reshaped.csv` a `data/Fyz_chem_*.csv`, které nejsou; `sites_subjects` nedefinován; názvy druhů s vloženým koncem řádku; `minDATA >= minDATA` je vždy pravda; komentář 0,5 × kód 0,6) |
  | `Limity_ryby_Reshaped.csv` | limity chemie (15 druhů × 13 ukazatelů, min/med/max) | vstup jen pro prototyp |
  | `Hodnocenichemie.csv` | výstup prototypu (295 řádků, EVL × druh × rok) | statický |
  | `Limity_migracnibariery .xlsx` | návrh prahů výšky bariér po druzích ve formátu limitů (`STA_MIGBAR`, `val`, cm) | hodnoty odpovídají `STA_MIGBARVYS`; není hodnocením celého toku |

  Kvalita vody není podle metodiky hodnoceným parametrem (R-09) — prototyp je tedy podklad
  pro budoucí rozšíření, ne chybějící část hodnocení. Návrh: přesunout do
  `Metodiky/Ryby a mihule/podklady/`.

### H-89 ⚠ — Seznam zdrojů cíleného monitoringu (`CIS_CILMON`) se neuplatňuje u žádného záznamu
- **Závažnost:** vysoká (mimo ryby) · **Typ:** BUG · **Stav:** ✅ **opraveno** (F3-D, aplikováno ve Fázi 4; dopad na obojživelníky změřen, Z-14) · **Odkazy:** H-71, H-62, H-69, Z-1; cévnaté H-14
- **Stav v kódu:** [`02_n2k_data_druhy.R:249`](../../R/00_config/02_n2k_data_druhy.R#L249)
  `ZDROJ %in% CIS_CILMON` porovnává s **tabulkou** (tibble), ne se sloupcem `ZDROJ_NDOP`, takže
  neplatí nikdy; `CILMON` vzniká jen z `PROJEKT == "Monitoring druhů ČR"` (a dvou druhů brouků).
- **Kontext (Z-1):** zadavatel požadoval ověřit, zda v seznamu nechybí významný nedávný zdroj
  s bohatou `STRUKT_POZN`. Ukázalo se, že **seznam nepůsobí vůbec** — doplnit do něj cokoli by
  nemělo žádný účinek.
- **Co seznam obsahuje a co by oprava přepnula na `CILMON = 1`** (33 položek; mimo
  *Monitoring druhů ČR* se v exportu vyskytují jen tři):

  | položka seznamu | záznamů | skupina |
  |---|---|---|
  | Kolektiv autorů (2021) Sledování stavu EVL - obojživelníci. | **2 106** (*B. bombina* 1 245, *T. cristatus* 687, *B. variegata* 90, *L. montandoni* 45, *T. dobrogicus* 32, *T. carnifex* 7) | obojživelníci |
  | Kolektiv autorů (2022) Monitoring motýlů. | 18 | motýli |
  | Kolektiv autorů (2022) Monitoring brouků. | 5 | brouci |

  U obojživelníků přibude cílený monitoring jen v **jedné** dvojici druh × EVL, kde dosud žádný
  nebyl (*B. bombina*) — H-62 (121 neznámých EVL) to tedy nevysvětluje; mění se ale verdikty DP
  s těmito záznamy (`CELKOVE_CILMON`) a jmenovatel `LOK_PROCDOBR`. **Dopad na obojživelníky
  není změřen během** (mimo rozsah tohoto zadání).
- **Významné nedávné zdroje s bohatou `STRUKT_POZN`, které cíleným monitoringem nejsou
  a v seznamu chybí** (od 2019, ≥ 10 tagů):

  | zdroj | záznamů | roky | skupina | doporučení |
  |---|---|---|---|---|
  | Fajmon Karel (2025) Sledování stavu populací srpice karbincolisté v Bílých Karpatech v letech 2024 a 2025. | **303** | 2024–2025 | cévnaté | **nedoplněno** — metodika cévnatých (PR-01) připouští jen projekt *Monitoring druhů ČR*; rozhodnutí zadavatele |
  | několik zpráv o monitoringu mechorostů (Štechová, Pulkrabová, Musil) | 2–6 každá | 2021–2025 | mechorosty | jen evidovat |

  U ryb nic nechybí (H-71). Do `cil_mon_zdroj.csv` proto **nebylo nic doplněno**.
- **Řešení:** patch F3-D (`ZDROJ %in% CIS_CILMON$ZDROJ_NDOP`).
- **Rozhodnutí zadavatele (2026-11-05):** Z-13 aplikovat; Z-14 nejdřív změřit na obojživelnících;
  Z-15 zdroj „srpice karbincolisté" **nedoplňovat**, zapsat do registru cévnatých (provedeno).
- **Oprava seznamu (pokyn zadavatele po Kontrolním bodu 3, `7722980`):** řádek
  „Monitoring druhů ČR" byl v seznamu chybně — je to hodnota sloupce `PROJEKT`, ne `ZDROJ`
  (žádný záznam ji jako `ZDROJ` nemá; `PROJEKT` se v `02_n2k_data_druhy.R` kontroluje zvlášť).
  Odstraněn. Doplněny zdroje monitoringu ryb **přesně v zápisu NDOP**, který se mezi roky liší:

  | zdroj | záznamů |
  |---|---|
  | Kolektiv autorů (2018) / (2019) / (2020) / (2021) Monitoring ryb. Monitoring druhů ČR. AOPK ČR. | 83 / 248 / 431 / 567 |
  | Kolektiv autorů (2022) Monitoring ryb (Monitoring druhů ČR ). | 627 |
  | Kolektiv autorů (2023) / (2024) Monitoring ryb. Monitoring druhů ČR. | 612 / 7 |
  | Kolektiv autorů (2025) Monitoring ryb. Monitoring druhů ČR. AOPK ČR. (zápis podle zadavatele) | 0 |

  Všech 2 575 záznamů těchto zdrojů má `PROJEKT = Monitoring druhů ČR`, takže se `CILMON`
  nezměnil u žádného z 209 825 záznamů exportu (ověřeno). Seznam teď zachytí tytéž zdroje i při
  jiném `PROJEKT`. Nepřidány (bez strukturovaných dat, jen evidence): „Halačka Karel (2019)
  monitoring EVD ryb ve vybraných EVL." (141 záznamů, 0 tagů) a „Blábolil P., Kočvara R. (2024)
  Zpráva z monitoringu pro mihuli potoční a vranku obecnou v EVL …" (18 záznamů 2023, 0 tagů).
- **Změřený dopad samotné opravy** — běh *T. cristatus* + *B. bombina*, kód `b7abb07` (= `35a3d99`)
  ± jediný řádek F3-D, nic jiného ([`Outputs/Test/TC_BB_20261105_cilmon/`](../../Outputs/Test/TC_BB_20261105_cilmon/README.md)):

  | | *B. bombina* před → po | *T. cristatus* před → po |
  |---|---|---|
  | DP s cíleným monitoringem | 629 → **795** | 428 → **584** |
  | verdikt z cíleného monitoringu d / z / š (`CELKOVE_CILMON`) | 273 / 86 / 274 → 285 / 88 / 426 | 153 / 31 / 246 → 183 / 31 / 372 |
  | verdikt DP ze všech záznamů d / z / š | 473 / 88 / 433 → 472 / 88 / 434 | beze změny (318 / 32 / 374) |
  | EVL d / z / š | 11 / 45 / 37 → **9 / 46 / 38** | beze změny (2 / 22 / 39) |
  | medián `LOK_PROCDOBR` | 40,0 → 40,0 % | 33,3 → **25,0 %** |

  Změna EVL *B. bombina*: 2× dobrý → zhoršený, 1× zhoršený → špatný. Nově započtené DP jsou
  převážně špatné (*B. bombina* +12 dobrých, +2 zhoršené, +152 špatných; *T. cristatus* +30 / 0 / +126),
  proto podíl dobrých DP klesá.

---

# Nález z testovacího běhu (Fáze 3, 2026-11-05)

### H-90 ⚠ — Lokalita bez hodnoceného klíčového parametru vychází jako „dobrá"
- **Závažnost:** kritická · **Typ:** BUG (vzniká kombinací rozhodnutí Z-1, Z-4 a pravidel `NA`) · **Stav:** ✅ **opraveno pro všechny skupiny** (Z-12, `27b2510`); rozpor s metodikou obojživelníků — **H-91** · **Odkazy:** H-71, H-79, H-80, PR-POP, R-13; cévnaté H-23
- **Mechanismus:** [`24:203-211`](../../R/02_druhy/24_n2k_druhy_lokality.R#L203-L211) dá „špatný" jen
  při **nesplněném** klíčovém indikátoru; nehodnocený (`NA`) se nepočítá (H-21). Dokud byl
  `POP_PRESENCE` klíčový, měla každá DP aspoň jeden hodnocený klíčový indikátor. Po Z-4
  (`info`) zbývají `POP_DYN` (≥ 2 roky, *Lampetra* ≥ 3) a `POP_VITALITA3` (≥ 2 průzkumy) —
  a protože klíč `LOKALITA` rozpadá řady na jednoleté lokality (55 % lokalit *Cottus*, H-71),
  nemá většina DP **žádný** hodnocený klíčový parametr. Výsledek je „dobrý", protože nic
  nesplněno není.
- **Měřený dopad (běh „po", všechny DP):** „dobrý" bez jediného hodnoceného klíčového parametru
  **616 z 659** (*Cottus*), **371 z 376** (*Lampetra*), **276 z 276** (*Misgurnus*); bez jakéhokoli
  hodnoceného indikátoru 422 / 327 / 215.
- **Metodika:** §5.1 „Oba parametry (dynamika populace i vitalita populace) **musí být hodnoceny
  jako dobré**, aby celkový stav populace … byl hodnocen jako dobrý." × §5.3 „Do celkového hodnocení
  … vstupují pouze parametry …, pro které je v daném roce k dispozici hodnocení."
- **Varianty (DP s cíleným monitoringem, běh „po"):**

  | | *Cottus* d / z / š / ? | *Lampetra* | *Misgurnus* | agregované lokality dobré (EVL) C / L / M |
  |---|---|---|---|---|
  | **A** dnes — nehodnocený klíčový se ignoruje | 279 / 42 / 173 / 0 | 180 / 95 / 92 / 0 | 66 / 79 / 34 / 0 | 8 / 6 / 0 |
  | **B** žádný hodnocený klíčový ⇒ neznámý | 43 / 3 / 173 / 275 | 5 / 1 / 92 / 269 | 0 / 2 / 34 / 143 | 2 / 1 / 0 |
  | **C** dobrý / zhoršený jen při **obou** hodnocených (§5.1), nesplněný ⇒ špatný, jinak neznámý | 42 / 3 / 173 / 276 | 5 / 1 / 92 / 269 | 0 / 0 / 34 / 145 | 2 / 1 / 0 |

- **Doporučení:** **C** — odpovídá §5.1 doslova a od B se liší jen u 1 + 2 lokalit. Patch: v `24`
  pro `DRUHY_RYBY_2026` doplnit `N_KEY_ALL` (počet klíčových řádků s limitem bez ohledu na `STAV_IND`)
  a větev `N_KEY_EXPECTED < N_KEY_ALL ~ NA` za větev „špatný" (vzor návrhu cévnatých H-23).
- **Souvislost:** varianta C/B činí většinu lokalit neznámými, dokud řady nejsou dost dlouhé — přímý
  důsledek fragmentace názvů `LOKALITA` (H-71) a toho, že monitoring běží od 2019. Kód lokality
  (D-1) by řady spojil.
- **Rozhodnutí zadavatele (Z-12, 2026-11-05):** *„minimálně jeden neznámý klíčový indikátor → neznámý
  celkový stav — implementovat napříč skupinami."* Doslovně, tedy přísněji než varianta C: neznámý
  má **přednost i před špatným** — shodně s rozhodnutím ke cévnatým (H-23 jejich registru,
  2026-10-28: „špatný nemá přednost").
- **Provedení** (`27b2510`): [`24:195`](../../R/02_druhy/24_n2k_druhy_lokality.R#L195) `N_KEY_DEFINED`
  = klíčové indikátory s limitem, které pro DP a rok mají řádek (21_2 vytvoří řádek pro každý
  indikátor s limitem i bez hodnoty); `N_KEY_UNKNOWN = N_KEY_DEFINED - N_KEY_EXPECTED`;
  [`24:240`](../../R/02_druhy/24_n2k_druhy_lokality.R#L240) `N_KEY_UNKNOWN > 0` ⇒ neznámý **před**
  větví „špatný"; interní kód `-1`, ve výstupu `STAV_IND = NA`, `HOD_IND = "neznámý"`
  ([`24:290`](../../R/02_druhy/24_n2k_druhy_lokality.R#L290)). Úroveň EVL: lokality už neznámou DP
  nezapočítávaly (`LOK_POCETSUM`); u hmyzu se nově nezapočítává ani neznámé pole
  ([`25:101`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L101)) a žádné hodnocené pole dává `NA` místo 0 %
  ([`25:122`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L122)).
- **Změřený dopad — tři druhy** (běh „aplikovano", spolu se Z-16; [README](../../Outputs/Test/Ryby_20261105/README.md#konečný-stav-větve--běh-aplikovano)):

  | | *Cottus* | *Lampetra* | *Misgurnus* |
  |---|---|---|---|
  | DP s cíleným monitoringem d / z / š / ? | 53 / 2 / 177 / 196 | 5 / 1 / 51 / 265 | 0 / 0 / 13 / 162 |
  | DP „neznámý" s nesplněným klíčovým (při přednosti špatného by byly špatné; všechny DP) | 3 | 64 | 23 |
  | EVL d / z / š / ? (v závorce běh „po") | 5 / 0 / 22 / 3 (8 / 0 / 22 / 0) | 1 / 0 / 14 / 13 (2 / 4 / 21 / 1) | 0 / 0 / 6 / 3 (0 / 0 / 9 / 0) |

  Dvě EVL *Cottus* přešly ze „špatný" na „dobrý": jejich špatné lokality mají vedle nesplněného
  i nehodnocený klíčový indikátor, staly se neznámými a z podílu dobrých lokalit vypadly — důsledek
  přednosti neznámého.
- **Změřený dopad — všechny skupiny** (export DP posledního plného běhu 2026-08-28, reprezentativní
  návštěva DP; starší kód a limity, tedy řádový odhad; skript a CSV v `Outputs/Test/Ryby_20261105/aplikovano/`):

  | Skupina | DP | dobrý před → po | zhoršený | špatný | neznámý po | špatný, kdyby měl přednost |
  |---|---|---|---|---|---|---|
  | Cévnaté rostliny | 5 275 | 3 473 → **125** | 172 → 32 | 1 630 → 900 | **4 218** | 1 630 |
  | Obojživelníci | 2 168 | 1 006 → **299** | 91 → 51 | 1 071 → 494 | **1 324** | 1 071 |
  | Ryby a mihule (všechny druhy) | 926 | 374 → 77 | 6 → 0 | 546 → 348 | 501 | 546 |
  | Brouci | 863 | 809 → 768 | 6 → 6 | 48 → 47 | 42 | 48 |
  | Letouni, motýli, vážky | 6 957 | beze změny | | | 0 | |

  Neznámé klíčové indikátory: cévnaté `POP_POCET` 3 148, `POP_VITAL` 1 849; obojživelníci
  `POP_ZMENARAD` 1 318, `POP_REPROPERIOD3` 148; ryby `POP_DYN` 481, `POP_VITALITA` 259; brouci
  stanovištní klíčové `STA_MRTDREVO`, `STA_TEKVODA` (34). Běh *T. cristatus* + *B. bombina* na
  současném kódu: viz H-91.
- **Zbývá:** pravidlo vychází z řádků — DP, kde řádek klíčového indikátoru vůbec nevznikl,
  nezachytí (**H-92**). Obdoba na úrovni EVL zavedena není (otázka Z-20).

### H-91 ⚠ — Pravidlo H-90 napříč skupinami odporuje textu metodiky obojživelníků a dělá neznámými 60–80 % DP
- **Závažnost:** vysoká · **Typ:** ROZPOR-METODIKY (důsledek rozhodnutí Z-12) · **Stav:** **zaznamenáno, otázka na zadavatele (Z-19)** · **Odkazy:** H-90, H-21; cévnaté H-23
- **Metodika obojživelníků**, kap. 2.1 ([`met_obojzivelnici_2026.md:242`](../Obojzivelnici/met_obojzivelnici_2026.md)):
  „Pokud je jeden klíčový populační indikátor hodnocen v nepříznivém stavu, je i druh v rámci DP
  hodnocen ve špatném stavu. … **Indikátor se hodnotí pouze, jsou–li dostupné informace k jeho
  hodnocení.**" — nehodnocený indikátor se podle textu vynechá, DP se tím neznámou nestává. Obdobné
  znění má ryby §5.3; proti tomu stojí ryby §5.1 a cévnaté kap. 7 (neznámý klíčový ⇒ neznámý).
- **Změřeno během** *T. cristatus* + *B. bombina* ([`Outputs/Test/TC_BB_20261105_cilmon/`](../../Outputs/Test/TC_BB_20261105_cilmon/README.md)):
  DP d / z / š / ? *B. bombina* 472 / 88 / 434 / 0 → 136 / 49 / 224 / **374**, *T. cristatus*
  318 / 32 / 374 / 0 → 54 / 18 / 176 / **304** (včetně párového filtru H-85). Neznámý je skoro vždy
  `POP_ZMENARAD` (374 a 301 DP). Z neznámých DP má **178 a 180 nesplněný jiný klíčový indikátor** —
  dosud byly špatné. EVL d / ? / z / š: *B. bombina* 9 / 0 / 46 / 38 → 8 / 9 / 39 / 37,
  *T. cristatus* 2 / 0 / 22 / 39 → 1 / 7 / 19 / 36.
- **Možnosti (Z-19):** (a) ponechat napříč skupinami (dnešní stav větve); (b) pravidlo jen pro
  skupiny, jejichž metodika je výslovně žádá (tři druhy ryb, cévnaté), ostatní jako dřív;
  (c) napříč skupinami, ale špatný má přednost (v exportu 2026-08-28 vrátí 577 špatných DP
  obojživelníků a 730 cévnatých); (d) u obojživelníků vyjmout z pravidla `POP_ZMENARAD`.
- **Pokyn zadavatele (2026-11-05):** *„Týká se to aktuálních dat? Pokud to významně nemění hodnocení
  od roku 2024, není to problém."* Změřeno podle roku hodnocené návštěvy DP; na současném kódu
  `24` bez pravidla (`433b610`) × s pravidlem (`27b2510`) nad týmž vstupem `21_2`
  (skripty a CSV v [`Outputs/Test/Cevnate_4sp_20261105/`](../../Outputs/Test/Cevnate_4sp_20261105/README.md)):

  | Druh / skupina | DP s návštěvou 2024+ | změněno | dobrý bez → s pravidlem | zdroj |
  |---|---|---|---|---|
  | *Bombina bombina* | 484 | **133 (27 %)** | 207 → 121 | běh „aplikovano" |
  | *Triturus cristatus* | 312 | **104 (33 %)** | 97 → 42 | běh „aplikovano" |
  | *Cottus* / *Lampetra* / *Misgurnus* | 38 / 20 / 12 | 35 / 20 / 12 | — | běh „aplikovano" (data 2024+ v NDOP téměř nejsou, **H-94**) |
  | cévnaté (4 druhy, 2 432 DP) | 638 | **0** | beze změny | běh `Cevnate_4sp_20261105` — klíčový indikátor se nepočítá vůbec (H-92) |
  | brouci | 201 | 5 (2,5 %) | 195 → 190 | export 2026-08-28 |
  | letouni, motýli, vážky | 1 678 | 0 | — | export 2026-08-28 |

  **Závěr:** u obojživelníků pravidlo hodnocení od 2024 **významně mění** — mezi DP s cíleným
  monitoringem (verdikt pro EVL) *B. bombina* 109 z 457, *T. cristatus* 92 z 299; dobrých ubude
  zhruba o polovinu. U ostatních skupin dnes bez podstatného dopadu (cévnaté jen proto, že klíčový
  indikátor chybí). Otázka Z-19 tedy zůstává otevřená **jen pro obojživelníky**.
- **Proč je `POP_ZMENARAD` u návštěv 2024+ neznámý** ([`21_1:1982-2013`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1982-L2013);
  referencí je poslední předchozí rok s cíleným monitoringem na DP), roky DP 2024+ s neznámou změnou:

  | Případ | *B. bombina* | *T. cristatus* |
  |---|---|---|
  | a) žádný předchozí rok s cíleným monitoringem (první monitoring DP) | 89 | 74 |
  | b) v hodnoceném roce chybí kategorie početnosti (záznam není negativní) | 59 | 55 |
  | c) poslední předchozí rok kategorii nemá, starší ji má | 30 | 18 |
  | d) žádný předchozí rok s kategorií | 14 | 9 |

  a), b), d) jsou skutečně chybějící data. **c) je volba kódu** — reference se bere z posledního
  předchozího roku s cíleným monitoringem, i když v něm kategorie početnosti chybí; vzít poslední
  předchozí rok **s kategorií** by hodnotitelnost rozšířilo (otázka Z-23).

### H-92 — Klíčový indikátor bez řádku pravidlo H-90 nezachytí (*Cobitis elongatoides*, *Salmo salar*)
- **Závažnost:** střední · **Typ:** BUG (latentní) · **Stav:** **zaznamenáno, mimo rozsah tří druhů** · **Odkazy:** H-90, H-53
- **Stav:** v exportu plného běhu 2026-08-28 mají DP *Cobitis elongatoides* (19) a *Salmo salar* (40)
  jen řádky `STA_MIGBARPOCET` a `STA_MIGBARVYS`; klíčové `POP_PRESENCE`, `POP_DYN`, `POP_VITALITA`
  z `limity_ryby.csv` (u *Salmo* `info` s `KLIC = ano`) chybějí úplně. Všech 59 DP je „dobrý" bez
  jediného hodnoceného indikátoru — tatáž vada jako H-90, ale `N_KEY_DEFINED` ji nevidí, protože
  počítá řádky, ne limity.
- **Totéž u cévnatých rostlin na současném kódu** (běh `Cevnate_4sp_20261105`: *Pulsatilla grandis*,
  *Campanula bohemica*, *Cypripedium calceolus*, *Klasea lycopifolia*, 2 432 DP): klíčový
  indikátor limitů 2026 `POP_POCETMINPRIP` (35 druhů; dále `POP_REPROUSP`, `POP_DELKAABSENCE`)
  se v `21_1` nepočítá (H-25 registru cévnatých, „sirotek"), takže DP nemá **žádný** řádek
  klíčového indikátoru: **2 410 DP je „dobrý" a 22 „zhoršený"** (*Cypripedium*, jen podle
  neklíčových indikátorů), žádná „špatný" ani „neznámý". Pravidlo H-90 to nevidí.
- **Návrh:** brát očekávané klíčové indikátory z tabulky limitů druhu (argument `limity` funkce
  `run_n2k_druhy_lok` dřív nevyužitý), ne z řádků — u cévnatých by pak do zavedení H-25 byly všechny
  DP neznámé, což odpovídá skutečnosti. Pro *Cobitis* a *Salmo* nejdřív zjistit, proč řádky
  nevznikají (srov. běh `Outputs/Test/Cobitis_elongatoides_20260904/`).
- **Test podle pokynu Z-24 (`982c999`):** zavedeno v `24` jen pro `DRUHY_KLIC_Z_LIMITU` =
  *Lampetra planeri*, *Misgurnus fossilis*; klíčové indikátory z limitů = `POP_DYN`, `POP_VITALITA3`.
  Běh [`Outputs/Test/Ryby_20261105/export_metodika/`](../../Outputs/Test/Ryby_20261105/export_metodika/):
  **bez změny** verdiktu i hodnoty u kterékoli DP — u obou druhů mají všechny DP řádky obou
  klíčových indikátorů. Rozšíření na *Cobitis*, *Salmo* a cévnaté = doplnit druhy do konstanty.

### H-93 — Nulový úlovek bez plochy úseku dával abundanci `NA`, ne 0
- **Závažnost:** střední · **Typ:** BUG · **Stav:** ✅ **opraveno** (`aa2d8bd`, ověřeno během) · **Odkazy:** H-74, H-90, Z-21
- **Stav v kódu:** [`21_1`](../../R/02_druhy/21_1_n2k_druhy_akce.R) `POP_ABUNDANCENAL = POP_POCETODLOV / POP_PLOCHALOV`;
  u negativního průzkumu bez tagu plochy je to `0 / NA = NA`, takže rok, kdy druh nebyl zjištěn,
  z řady dynamiky **vypadl** — pokles populace se tím skrýval. *Misgurnus* 65 záznamů cíleného
  monitoringu (*Cottus* 1, *Lampetra* 2).
- **Oprava:** u tří druhů nulový úlovek ⇒ abundance 0 bez ohledu na plochu.
- **Ověřeno během** ([`Outputs/Test/Ryby_20261105/oprava_h93/`](../../Outputs/Test/Ryby_20261105/oprava_h93/)),
  DP s cíleným monitoringem d / z / š / ?: *Misgurnus* 0 / 0 / 13 / 162 → 0 / 0 / **23** / 152,
  *Lampetra* 5 / 1 / 51 / 265 → 5 / 1 / 53 / 263, *Cottus* 53 / 2 / 177 / 196 → 53 / 2 / 180 / 193 —
  shodně se simulací. Nově hodnocené DP jsou všechny špatné (rok bez úlovku po letech s úlovkem =
  pokles o více než 50 %, dosud skrytý). EVL: *Misgurnus* 2× neznámý → špatný, *Cottus* a *Lampetra* 1×.

### H-94 ⚠ — V exportu NDOP chybí monitoring ryb z let 2024 a 2025
- **Závažnost:** vysoká · **Typ:** DATA · **Stav:** **zaznamenáno, otázka na zadavatele (Z-22)** · **Odkazy:** H-90, H-71, Z-21
- **Data:** záznamy projektu *Monitoring druhů ČR* (dvojice předmětů ochrany), export 2026-05-10:

  | Druh | 2019 | 2020 | 2021 | 2022 | 2023 | **2024** | **2025** |
  |---|---|---|---|---|---|---|---|
  | *Cottus gobio* | 95 | 159 | 179 | 195 | 195 | **3** | **0** |
  | *Lampetra planeri* | 47 | 140 | 126 | 138 | 133 | **3** | **0** |
  | *Misgurnus fossilis* | 19 | 38 | 69 | 41 | 62 | **0** | **0** |

- **Důsledek:** řady všech lokalit končí rokem 2023. Dynamika *Lampetra* potřebuje 3 roky, ostatní 2;
  vitalita 2 průzkumy s délkami. Hodnocený rok je 2025, ale hodnotí se data nejvýše z 2023 —
  a většina lokalit má v 2019–2023 jen jeden nebo dva průzkumy. To je hlavní příčina, proč je
  většina DP *Lampetra* a *Misgurnus* neznámá (viz diagnóza níže).
- **Ověřeno i podle `ZDROJ`** (H-89): zdroj „Kolektiv autorů (2024) Monitoring ryb …" má v exportu
  jen 7 záznamů a zdroj za rok 2025 žádný; ostatní záznamy ryb 2024–2025 jsou náhodná pozorování,
  mapování invazních druhů a jednotlivé studie (např. Halačka, Vetešník 2025 — Pohansko).
  Chybějící data tedy nejsou skryta pod jiným `PROJEKT`.

### Diagnóza *Lampetra planeri* a *Misgurnus fossilis* (pokyn k Z-21, 2026-11-05)

Pokyn zadavatele: *„nejdřív vyřešit piskoře a mihuli"*. Běh „aplikovano" dává u DP s cíleným
monitoringem hodnotitelné **oba** klíčové indikátory jen u 57 z 322 DP *Lampetra* a 13 z 175 DP
*Misgurnus* (*Cottus* 232 z 428). Příčiny podle vstupů `21_1`:

| | *Lampetra* | *Misgurnus* |
|---|---|---|
| DP s jediným rokem dat | 150 z 322 | 128 z 175 |
| DP s dost roky, ale bez abundance v některém z nich | 51 | 31 |
| — příčina | 184 pozitivních záznamů 2018–2020 bez počtu (`POCET` prázdný; délky jen volným textem v `POZNAMKA`, strojově čitelné u 1 z 217) | 185 pozitivních záznamů bez plochy úseku (tagy délky a šířky jsou prázdné); 65 negativních záznamů (H-93) |
| pozitivní roky lokality bez měření délek (vitalita) | 440 | 166 |

Simulace — DP s cíleným monitoringem s **oběma** klíčovými indikátory hodnotitelnými:

| Úprava | *Lampetra* (322 DP) | *Misgurnus* (175 DP) | Povaha |
|---|---|---|---|
| dnes | 57 | 13 | |
| **A** nulový úlovek ⇒ abundance 0 (H-93) | 59 | 23 | oprava — **provedeno** |
| A + **B** *Misgurnus*: dynamika z počtu (1. + 2. odlov), ne z hustoty | 59 | 35 | **zamítnuto** — zadavatel: abundance = `POCET` / plocha |
| A + B + lokality spojené podle souřadnic do 100 m v téže EVL | 71 (z 261) | 42 (z 142) | volba — proti Z-1 „just LOKALITA" |
| A + B + dynamika *Lampetra* už od 2 let | 122 | 35 | proti Z-2 (varianta C potřebuje 3 průzkumy) a A-2 |
| … + vitalita už z 1 průzkumu | 125 | 46 | proti Z-17 a A-4 |

Žádná úprava kódu nedá většinu lokalit hodnotitelnou — rozhoduje délka řad (H-94). Spojení podle
souřadnic (S-JTSK, přesnost 50 m) by počet lokalit přiblížil metodice (*Lampetra* 322 → 261 / 239
při 100 / 300 m, metodika 174; *Misgurnus* 175 → 142 / 98, metodika 69); `IDX_ND_LOK` je
u každého záznamu jiný a lokality nespojí.

---

# Fáze 3 — návrh, patch a testovací běh (2026-11-05)

| Commit | Obsah |
|---|---|
| `9f894fe` | volné soubory z kořene do `podklady/` (Z-9, H-88) |
| `24cfa29` | `R/05_metodiky/55_ryby_limity_2026.R` → `Data/Input/limity_ryby_2026.csv`, `cis_ryby_delky_strukt_2026.csv`, [diff](limity_ryby_2026_diff.csv), [protokol](limity_ryby_2026_kontrola.md) (pravidlo pásem Z-11) |
| `06cf575` | [patche/ryby_metodika_2026.patch](patche/ryby_metodika_2026.patch) + [README](patche/README.md) (tehdy neaplikováno), testovací běh [`Outputs/Test/Ryby_20261105/`](../../Outputs/Test/Ryby_20261105/README.md), aktualizace registru a revize |

Ve Fázi 3 zůstaly živý kód `R/02_druhy`, `R/00_config` a živé vstupy beze změny; změnila je až
[Fáze 4](#fáze-4--aplikace-2026-11-05).

## Testovací běh — hlavní výsledky

Podrobně v [README běhu](../../Outputs/Test/Ryby_20261105/README.md).

| | *Cottus* | *Lampetra* | *Misgurnus* |
|---|---|---|---|
| DP pred → po | 370 → 874 | 253 → 563 | 113 → 390 |
| DP s cíleným monitoringem hodnocené | 300 → 494 | 200 → 367 | 73 → 179 |
| EVL pred → po | bez výstupu → 8 dobrý, 22 špatný | 11 dobrý, 17 zhoršený → 2 dobrý, 4 zhoršený, 21 špatný, 1 neznámý | bez výstupu → 9 špatný |
| migrační prostupnost EVL: dobrá / špatná / neznámá | 0 / 17 / 13 | 0 / 19 / 9 | 0 / 4 / 5 |
| lokality se známou výškou bariéry | 106 z 494 | 100 z 367 | 10 z 179 |

- Replikace z Fáze 2 se s během „pred" shoduje tam, kde je srovnatelná (*Lampetra* `LOK_PROCDOBR`
  dobrý v 10 EVL, stav „špatný" v žádné EVL, CZ0213050 „dobrý" bez hodnocené lokality).
- **Migrace (Z-5):** prostupnost nemůže vyjít dobrá v žádné EVL — prázdný tag je neznámo
  a v každé EVL je alespoň jedna lokalita bez údaje (datová mezera D-5).
- **H-90** (nové): většina „dobrých" DP v běhu „po" nemá žádný hodnocený klíčový parametr.

## Stav nálezů po Fázi 3

| Nález | Stav |
|---|---|
| H-71 | patch F3-C, ověřeno; zbývá kód lokality ze Survey (D-1) a fragmentace názvů |
| H-72, H-73 | patch F3-H (*Lampetra* varianta C), ověřeno |
| H-74 | patch F3-F, ověřeno; zbývají tagy počtů odlovů (D-2) |
| H-75 | patch F3-J, ověřeno |
| H-76, H-77, H-78 | limity F3-A, délky F3-B, výpočty F3-G, ověřeno |
| H-79 | patch F3-H; méně než 2 průzkumy ⇒ `NA` **předběžně** (R-13, A-4) |
| H-80 | `POP_PRESENCE` = `info` (F3-A); vyvolává **H-90** |
| H-81, H-82, H-83 | patch F3-K, ověřeno (CZ0213050 → neznámý) |
| H-84 | patch F3-I, ověřeno |
| H-85 | patch F3-E, dopad na všechny skupiny změřen (README patche) |
| H-86, H-87 | neřešeno — mimo rozhodnutí Z-1 … Z-11 (`ind_id` nemá 14 z 16 `ID_IND` návrhu) |
| H-88 | provedeno (`9f894fe`) |
| H-89 | patch F3-D; dopad na obojživelníky během neměřen; do `cil_mon_zdroj.csv` nic nedoplněno |
| H-90 | nové, čeká na rozhodnutí |

Konečný stav po Fázi 4 je v [§Fáze 4](#fáze-4--aplikace-2026-11-05).

---

# Fáze 4 — aplikace (2026-11-05)

Po rozhodnutí ke Kontrolnímu bodu 2 (tabulka na začátku registru).

| Commit | Obsah | Rozhodnutí |
|---|---|---|
| `433b610` | patch [ryby_metodika_2026.patch](patche/ryby_metodika_2026.patch) aplikován beze změny; limity 2026 v živých `limity_ryby.csv` a `cis_ryby_delky_strukt.csv` (`55` zapisuje přímo, základ z gitu `35a3d99`, obdoba `54`); `*_2026.csv` zrušeny | Z-13 |
| `27b2510` | `24`: neznámý klíčový indikátor ⇒ neznámý celkový stav DP, **všechny skupiny**, přednost před špatným; `25`: neznámé pole hmyzu mimo jmenovatel | Z-12 (H-90) |
| `06a849d` | `02_n2k_data_druhy.R`: sjednocení zápisu `LOKALITA` | Z-16 (H-71) |
| `37bede4`, `a59a271` | otázky A-1 … A-10 na konci [textu metodiky](Metodika_EVL_upravena_mihule_piskor.md) (Z-18); běhy [`Outputs/Test/TC_BB_20261105_cilmon/`](../../Outputs/Test/TC_BB_20261105_cilmon/README.md) (Z-14) a [`Outputs/Test/Ryby_20261105/aplikovano/`](../../Outputs/Test/Ryby_20261105/README.md#konečný-stav-větve--běh-aplikovano); zápisy v registrech obojživelníků a cévnatých (Z-15) | Z-14, Z-15, Z-18 |

Kód větve (`06a849d`) je **shodný s kódem ověřeným během „aplikovano"** (porovnáno soubor po
souboru, `R/` a vstupy ryb). `limity_vse.csv` a `cil_mon_zdroj.csv` beze změny. Nic není sloučeno
ani odesláno; `Outputs/Data/` ani `Data/Temp/` repozitáře běhy nepřepsaly (izolované kopie).

### Export jen indikátorů metodiky (pokyn 2026-11-05, `eda447d`)

Limity tří druhů 46 → **27 řádků**: zůstávají parametry metodiky (`POP_DYN`, `POP_VITALITA3`;
stanoviště *Cottus* `STA_DNOTYPSOUCETPROCENT`, *Lampetra* oba `STA_ODHAD…`, *Misgurnus*
`STA_DNOTYPSOUCETPROCENT` a `STA_POKRVEGSUBMERZ`; EVL `LOK_PROCDOBR`, `STA_MIGPROSTUP`),
`POP_PRESENCE` jako info a `STA_MIGBARVYS` (info) — výška bariéry na lokalitě, ze které `25` počítá
parametr metodiky `STA_MIGPROSTUP` (metodika 4.1 „sledování přítomnosti migračních bariér na
monitorovaných lokalitách"). Vypuštěny informativní `POP_POCET`, `POP_VITALITA`, `POP_POCETODLOV`,
`POP_POCETODLOVZDROJ`, `STA_PODMINKYODLOVU` (dosavadní příznak podle Z-7) a `STA_MIGBARPOCET`;
jejich výpočet v `21_1` zůstává, jen se neexportují. Jednotky `POCET` pro abundanci se dřív braly
z řádků `POP_POCET` limitů — nově `JEDNOTKY_POCET_RYBY_2026` v `21_1` (táž hodnota).
Běh [`export_metodika`](../../Outputs/Test/Ryby_20261105/export_metodika/): **0 rozdílů** v hodnotách
i stavech `POP_DYN`, `POP_VITALITA3`, `STA_MIGBARVYS`, `POP_PRESENCE`, verdiktech DP a všech
indikátorech EVL proti běhu `abundance_pocet`.

## Stav nálezů po Fázi 4

| Nález | Stav |
|---|---|
| H-71 | ✅ přechodně: klíč `LOKALITA` se sjednoceným zápisem; trvale kód lokality (D-1) |
| H-72, H-73, H-74, H-75, H-76, H-77, H-78, H-81, H-82, H-83, H-84 | ✅ aplikováno (`433b610`), ověřeno během |
| H-79 | ✅ aplikováno; méně než 2 průzkumy ⇒ neznámý (Z-17) do odpovědi autorů (A-4) |
| H-80 | ✅ `POP_PRESENCE` = `info` |
| H-85 | ✅ aplikováno pro všechny skupiny (dopad v README patche) |
| H-86, H-87 | otevřené — mimo rozhodnutí (`ind_id` chybí 14 z 16 `ID_IND`) |
| H-88 | ✅ `9f894fe` |
| H-89 | ✅ aplikováno; dopad na obojživelníky změřen (Z-14); seznam beze změny (Z-15) |
| H-90 | ✅ aplikováno pro všechny skupiny (Z-12) |
| H-91 | **otázka Z-19** — od 2024 významné jen u obojživelníků |
| H-92 | očekávané klíčové indikátory z limitů zavedeny pro *Lampetra* a *Misgurnus* (bez změny výsledku); ostatní druhy po rozhodnutí |
| H-93 | ✅ opraveno (nulový úlovek ⇒ abundance 0) |
| H-94 | data 2024–2025 dodá zadavatel později; do té doby se hodnotí poslední dostupné roky (Z-22) |

## Otázky pro zadavatele — Kontrolní bod 3

| # | Otázka | Možnosti | Doporučení | Nález |
|---|---|---|---|---|
| Z-19 | Pravidlo H-90 u skupin, jejichž metodika říká „indikátor se hodnotí pouze, jsou-li dostupné informace" (obojživelníci) — *odpověď „2023" k upřesnění: hranice kontroly 2023, nebo platnost pravidla od 2023?* | (a) ponechat napříč skupinami / (b) jen ryby tří druhů a cévnaté / (c) napříč, ale špatný má přednost / (d) obojživelníci bez `POP_ZMENARAD` / (e) pravidlo jen pro hodnocení od 2023 | **(b)** — od 2023 i od 2024 mění 27–34 % DP obojživelníků; ostatní skupiny dnes bez dopadu | H-91 |
| Z-20 | *(zadavatel: „ano, ale nejdřív vyřešit Z-19")* Obdoba H-90 na úrovni EVL: obecná aritmetika [`25:409-413`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L409-L413) neznámý klíčový indikátor území odečte a hodnotí podle zbylých | zavést (neznámý ⇒ EVL neznámá) / ponechat | rozhodnout po Z-19; ryby mají vlastní rozhodovací tabulku (blok 5a), u obojživelníků (5b) vyšla EVL bez hodnocené DP v běhu „neznámý" — jiné skupiny neověřeny | H-90 |
| Z-21 | *(zadavatel: „nejdřív vyřešit piskoře a mihuli")* spojení lokalit podle souřadnic do 100 m — viz diagnóza; úprava B **zamítnuta** pokynem „abundance = `POCET` / plocha" | spojení ano (100 / 300 m) / ne | spojení až s kódem lokality (D-1) | H-93, H-94 |
| Z-22 | ~~Monitoring ryb 2024–2025 v NDOP chybí~~ — **zodpovězeno**: export dodá zadavatel později, teď poslední dostupné roky | — | — | H-94 |
| Z-23 | ~~Obojživelníci: reference `POP_ZMENARAD` s kategorií početnosti~~ — **zodpovězeno**: teď ne | — | — | H-91 |
| Z-24 | Očekávané klíčové indikátory z limitů — **otestováno** u *Lampetra* a *Misgurnus* (bez změny); rozšířit na *Cobitis*, *Salmo* a cévnaté? | ano / ne | ano, po Z-19 (u cévnatých do zavedení H-25 všechny DP neznámé) | H-92 |

---

# Potvrzeno — bez akce

| # | Co | Ověření |
|---|---|---|
| P-01 | Kód chemii vody nepoužívá | grep `R/`; metodika ji také nehodnotí (R-09) |
| P-02 | Tabulka 2 obojživelníků se na ryby neuplatní (H-50) | [`25:315-316`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L315-L316) `species_name %in% DRUHY_METODIKY_OBOJ`; `POP_POCETPRUM3` se v `27` počítá pro všechny druhy, u ryb ale nevstupuje |
| P-03 | Taxonomie | v exportu není *Cottus* sp., *C. poecilopus*, kříženec, *Lampetra* sp. ani Petromyzontidae; *Eudontomyzon mariae* (22) má vlastní název i řádky číselníku — záměna nedoložena, problém hybridizace nelze z dat ověřit |
| P-04 | `slice(1)` přes `POLE` v `24` (H-70) | u tří druhů mění verdikt jediné lokality (*Lampetra*) |
| P-05 | Hranice `max 10` / `max 5` cm | `<=` odpovídá „vyšší než 10 cm" ⇒ nesplněno až nad 10 cm |
