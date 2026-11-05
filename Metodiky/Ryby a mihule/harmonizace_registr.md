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

Fáze 1 a 2 **nezměnily žádný kód ani vstupní data.** Návrhy řešení jsou podklad pro Fázi 3.

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
| **Kritická** — plošně mění výsledek | 5 | H-71, H-72, H-73, H-75, H-81 |
| **Vysoká** — mění výsledek u části dat | 8 | H-74, H-76, H-77, H-78, H-79, H-80, H-83, H-85 |
| **Střední** | 2 | H-82, H-84 |
| **Nízká** | 4 | H-86, H-87, H-88, H-89 |
| *Potvrzeno — bez akce* | 5 | P-01 … P-05 |

---

# Kritické nálezy

### H-71 ⚠⚠ — `KOD_LOKAL` u ryb nenese kód monitorované lokality: úseky se slévají do jedné „lokality"
- **Závažnost:** kritická · **Typ:** DATA + KÓD · **Stav:** **zaznamenáno, čeká na rozhodnutí o zdroji kódu** · **Odkazy:** PR-LOK-1, PR-EVL-1, H-70
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
- **Rozhodnutí zadavatele:** _(zdroj kódu lokality: varianta 1, 2, nebo obojí?)_

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

### H-89 — `CIS_CILMON` se nikdy neuplatní (mimo rozsah ryb)
- **Závažnost:** nízká · **Typ:** BUG (mimo rozsah) · **Stav:** **zaznamenáno**
- [`02_n2k_data_druhy.R:249`](../../R/00_config/02_n2k_data_druhy.R#L249) `ZDROJ %in% CIS_CILMON`
  porovnává s tabulkou (tibble), ne se sloupcem `ZDROJ_NDOP`, takže neplatí nikdy; `CILMON`
  vzniká jen z `PROJEKT`. Seznam obsahuje citace monitoringu motýlů — ryb se netýká
  (žádný zdroj ryb v něm není).

---

# Potvrzeno — bez akce

| # | Co | Ověření |
|---|---|---|
| P-01 | Kód chemii vody nepoužívá | grep `R/`; metodika ji také nehodnotí (R-09) |
| P-02 | Tabulka 2 obojživelníků se na ryby neuplatní (H-50) | [`25:315-316`](../../R/02_druhy/25_n2k_druhy_uzemi.R#L315-L316) `species_name %in% DRUHY_METODIKY_OBOJ`; `POP_POCETPRUM3` se v `27` počítá pro všechny druhy, u ryb ale nevstupuje |
| P-03 | Taxonomie | v exportu není *Cottus* sp., *C. poecilopus*, kříženec, *Lampetra* sp. ani Petromyzontidae; *Eudontomyzon mariae* (22) má vlastní název i řádky číselníku — záměna nedoložena, problém hybridizace nelze z dat ověřit |
| P-04 | `slice(1)` přes `POLE` v `24` (H-70) | u tří druhů mění verdikt jediné lokality (*Lampetra*) |
| P-05 | Hranice `max 10` / `max 5` cm | `<=` odpovídá „vyšší než 10 cm" ⇒ nesplněno až nad 10 cm |
