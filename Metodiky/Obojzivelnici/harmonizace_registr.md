# Registr nálezů — harmonizace hodnocení obojživelníků s metodikou

**Fáze A (audit) i Fáze B (implementace) dokončeny 2026-08-20.** Zadání: [harmonizace_prompt.md](harmonizace_prompt.md).
Metodika ve stavu k 2026-08-20 (po revizích M-16, M-17, M-18).
Fáze A (audit) nezměnila žádný kód; rozhodnutí zadavatele k jednotlivým nálezům
byla doplněna 2026-08-20 (viz §Otázky a jednotlivé nálezy) a teprve poté
proběhla Fáze B — **[výsledky implementace jsou na konci dokumentu](#fáze-b--implementace-2026-08-20)**.

## Jak byl audit proveden

| Krok | Zdroj | Rozsah |
|---|---|---|
| Normativní obsah | `met_ssEVL_SLOUCENE_…_zmeny.docx` | extrakce s odstraněním `<w:delText>`, `<w:delInstrText>`, `<w:moveFrom>`; 1254 řádků |
| Pokrytí Přílohy 1 | `limity_vse.csv` | 11 řádků × 6 druhů, strojově porovnáno |
| Číselník | `cis_indikatory_popis.csv` | 45 řádků, kontrola `ind_id` |
| Algoritmy | `21_1`, `21_2`, `24`, `25`, `27` | čtení kódu + kontrola domén |
| **Kolize domén** | `host_data/export_data_evl.csv` | **31 733 záznamů 6 druhů, 12 985 s `STRUKT_POZN`, roky 1971–2026** |

Kolize domén byly ověřeny **proti ostrým datům z NDOP**, ne jen proti kódu. To odhalilo
čtyři nálezy, které z kódu ani z limitů nejsou vidět (H-03, H-04, H-07, H-08).

## Souhrn

| Závažnost | Počet | Nálezy |
|---|---|---|
| **Kritická** — plošně mění výsledek | 6 | H-01 … H-06 |
| **Vysoká** — mění výsledek u části dat | 6 | H-07 … H-12 |
| **Střední** — konzistence, dohledatelnost | 6 | H-13 … H-18 |
| **Potvrzeno OK** — bez akce | 5 | viz §Potvrzeno |
| *Nálezy z implementace* | 2 | H-19, H-20 |
| *Nálezy z testovacího běhu (2026-08-25)* | 3 | H-21, H-22, H-23 |
| *Nálezy z revize kategorií početnosti (2026-08-30)* | 3 | H-24, H-25, H-26 |
| *Nálezy z kontroly exportu proti šabloně (2026-08-31)* | 3 | H-27, H-28, H-29 |
| *Dořešení H-01 a H-02 (2026-08-31)* | 3 | H-30, H-31, H-32 |
| *Rozšíření kategorie `info` (2026-08-31)* | 4 | H-33, H-34, H-35, H-36 |
| *Dokončení `info` a revize limitů ryb (2026-09-03)* | 3 | H-37, H-38, H-39 |

---

# Kritické nálezy

### H-01 ✅ — `STA_VYSYCHANI`: doména 0/1 proti procentním limitům ⇒ indikátor selhává vždy
- **Závažnost:** kritická · **Typ:** BUG
- **Metodika:** Příloha 1, *pravidelné vysychání vodních ploch* — jediný řádek, pravidlo „špatně ve všech třech letech po sobě". Per-roční indikátor vysychání **v Příloze 1 není**.
- **Stav v kódu:** [`21_1:382`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L382) — `STA_VYSYCHANI` je `case_when(... ~ 1L, ... ~ 0L)`, tedy **celé číslo 0/1**.
- **Stav v datech:** `limity_vse.csv` má pro všech 6 druhů `min 25` a `val 26-50 % / 51-75 % / 76-100 %` — to jsou limity **procentního zaplavení dna**, ne příznaku 0/1.
- **Důsledek:** `val` větev: `"0"`/`"1"` se netrefí do žádného pásma → `0`. `min` větev: `0 < 25` i `1 < 25` → `0`. Agregace bere maximum → **`STAV_IND = 0` pokaždé, když je stav vody vůbec zaznamenán.** Každá DP se záznamem vody tak dostává jeden nesplněný stanovištní indikátor „zdarma"; ve spojení s jedním dalším selháním padá DP na „zhoršený".
- **Dotčené druhy:** všech 6
- **Návrh řešení:** odstranit limity `STA_VYSYCHANI` z `limity_vse.csv` (viz H-02); indikátor ponechat jako neomezený vstup pro `STA_VYSYCHANIPERIOD3`. Alternativa — pokud mají limity platit pro *stav vody* — vyžaduje nový `ID_IND`, ale Příloha 1 pro něj nemá řádek.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** dle návrhu — limity `STA_VYSYCHANI` odstranit.

### H-02 ✅ — `STA_VYSYCHANI` duplikuje řádek Přílohy 1
- **Závažnost:** kritická · **Typ:** BUG
- **Metodika:** Příloha 1 má pro vysychání **jeden** řádek; implementuje jej `STA_VYSYCHANIPERIOD3` (`max 2`, tj. špatně při 3 ze 3 let) — to je správně.
- **Stav v datech:** `limity_vse.csv` má vyhodnocované limity pro `STA_VYSYCHANI` **i** `STA_VYSYCHANIPERIOD3`.
- **Důsledek:** jedna skutečnost sráží DP dvakrát. Spolu s H-01 to znamená, že „vysychání" přispívá jedním jistým selháním a jedním skutečným hodnocením.
- **Dotčené druhy:** všech 6
- **Návrh řešení:** `STA_VYSYCHANI` ponechat v číselníku (`ind_id` 34) jako informativní hodnotu bez `LIM_IND`, obdobně jako `LOK_DILCDOBRE`.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno.** `ind_id` 34 ale **přechází na `STA_VYSYCHANIPERIOD3`** (viz H-14); `STA_VYSYCHANI` zůstane v číselníku bez `ind_id` a bez `LIM_IND`.

### H-03 ✅ — `STA_STAVVODA*`: převodní tabulka nepokrývá reálné hodnoty, vyschlé plochy se nepoznají
- **Závažnost:** kritická · **Typ:** BUG
- **Metodika:** §Sledované indikátory, *Stav vody*: „Zaznamenává se míra zaplavení dna DP **v procentech**, kde 100 % odpovídá plně zaplavenému dnu; 0 % odpovídá zcela vyschlé ploše."
- **Stav v kódu:** [`21_1:366–389`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L366-L389) sjednocuje `STA_STAVVODATUNE` → `STA_STAVVODALITORAL` → `STA_STAVVODARYBNIK` a mapuje **jen pásma** `"1-25 %"`, `"26-50 %"`, `"51-75 %"`, `"76-90 %"`, `"91-100 %"`, `"vyschlá"`, `"zaniklá"`.
- **Stav v datech (ověřeno na exportu):**

  | tag | záznamů | rozpozná `case_when` | nerozpozná |
  |---|---|---|---|
  | `STA_STAVVODATUNE` | 3 413 | 82,3 % | 603 |
  | `STA_STAVVODARYBNIK` | 2 611 | 56,5 % | 1 137 |
  | `STA_STAVVODALITORAL` | 2 106 | **0,0 %** | 2 106 |
  | `STA_STAVVODAPERTUNE` | 2 411 | 29,8 % | 1 692 |

- **Čtyři samostatné příčiny:**
  1. **`"vyschlé"` ≠ `"vyschlá"`.** Data obsahují tvar `vyschlé` (88× tůně + 83× periodické tůně = **171 záznamů**); `vyschlá` se v datech nevyskytuje **ani jednou**. Podmínka tedy nikdy nesedne a plocha propadne na `is.na(STA_STAVVODA) == FALSE ~ 0L`, tj. **„nevysychá"**. Zcela vyschlá plocha je vyhodnocena jako v pořádku.
  2. **`"0-25 %"` není v převodní tabulce** (kód zná jen `"1-25 %"`) — 136× tůně, 131× periodické tůně, dále rybníky. Rovněž → „nevysychá".
  3. **Holá čísla.** `STA_STAVVODALITORAL` obsahuje **výhradně** holá čísla (`100`, `90`, `80`, `0`, …) — proto 0 % rozpoznání. Totéž z velké části `STA_STAVVODAPERTUNE` a `STA_STAVVODARYBNIK`. Hodnota `0` (60× u periodických tůní) = zcela vyschlá plocha → opět „nevysychá".
  4. **`STA_STAVVODAPERTUNE` kód vůbec nečte** — 2 411 záznamů. Přitom jde o **periodické tůně**, tj. právě ty plochy, u nichž je vysychání sledovaným jevem.
- **Důsledek:** indikátor *pravidelné vysychání* (Příloha 1, všech 6 druhů) **systematicky podhodnocuje vysychání**. Chyba jde vždy jedním směrem — plocha se jeví lepší, než je.
- **Dotčené druhy:** všech 6
- **Návrh řešení:** přepsat převod na normalizaci hodnoty (nejprve pokus o číslo v procentech, pak pásmo, pak slovní stav) s podporou obou tvarů `vyschlá/vyschlé`, pásma `0-25 %` a holých čísel; přidat `STA_STAVVODAPERTUNE` do sjednocení. **Nerozpoznanou hodnotu ponechat `NA`, nikdy nemapovat na „nevysychá".**
- **Otevřená otázka pro autory metodiky:** má mít `STA_STAVVODAPERTUNE` (periodické tůně) v hodnocení vysychání stejnou váhu jako trvalé tůně? Metodika periodické tůně zmiňuje jako žádoucí jev („periodické vysychání je klíčovým mechanismem… spíše příznivé"), ale Příloha 1 rozlišení nezavádí.
- **Odpověď autorů (2026-08-20):** „ano, stejná váha" — `STA_STAVVODAPERTUNE` se do hodnocení vysychání započítává **stejnou vahou** jako trvalé tůně.
- **Rozhodnutí zadavatele:** ✅ **schváleno** dle návrhu, včetně doplnění `STA_STAVVODAPERTUNE` do sjednocení.

### H-04 ⏸ — `STA_HLOUBKAMENSI20`: zdrojový tag v datech neexistuje ⇒ indikátor Přílohy 1 je mrtvý
- **Závažnost:** kritická · **Typ:** GAP
- **Metodika:** Příloha 1, *plocha s hloubkou menší než 50 cm* — hodnoceno u **všech 6 druhů** (špatně pod 25 % u BBOM a Triturus, pod 75 % u BVAR a LMON). §Sledované indikátory: „Zaznamenává se v procentech aktuálně zaplavené plochy DP."
- **Stav v kódu:** [`21_1:494`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L494) čte tag `<STA_HLOUBKAMENSI20>`; komentář v kódu přiznává, že název nebyl ověřen.
- **Stav v datech:** **tag `<STA_HLOUBKAMENSI20>` se v exportu nevyskytuje ani jednou.** Inventura všech tagů ve `STRUKT_POZN` u 6 druhů neobsahuje žádný tag pro hloubku/mělčinu.
- **Důsledek:** hodnota je vždy `NA` → indikátor nikdy nevstoupí do `N_OTH_EXPECTED` → **jeden z 11 indikátorů Přílohy 1 se fakticky nehodnotí u žádného druhu.** Nejde o chybu v hodnocení, ale o tichou neúplnost: hodnocení DP stojí na 10 indikátorech místo 11 a nikde to není vidět.
- **Dotčené druhy:** všech 6
- **Návrh řešení:** bez zásahu do Survey123 nelze vyřešit. Do té doby ponechat `NA` (nikoli 0) a **doplnit do výstupu explicitní informaci, že indikátor nebyl sledován**, aby neúplnost nebyla tichá.
- **Otevřená otázka pro autory metodiky / správce Survey123:** pod jakým tagem se bude *plocha s hloubkou menší než 50 cm* zaznamenávat? Metodika změnila práh z 20 cm na 50 cm — je potřeba nový tag i nový `ind_id` v ISOP.
- **Odpověď autorů (2026-08-20):** tag bude **`STA_PLOCHA50CM`**, indikátor se bude **hodnotit až od roku 2027**; v datech zatím chybí.
- **Rozhodnutí zadavatele:** ⏸ **částečně** — proměnnou i tag přejmenovat na `STA_PLOCHA50CM` (kód, `limity_vse.csv`, číselník), hodnota zůstane `NA`, dokud data nezačnou chodit. **Otevřeno:** `ind_id` pro `STA_PLOCHA50CM` nebylo přiděleno — viz otázka 2b.

### H-05 — úroveň EVL nemůže nikdy dosáhnout stavu „špatný"
- **Závažnost:** kritická · **Typ:** BUG
- **Metodika:** Tabulka 2 — „špatný" nastává, jsou-li **oba** indikátory hodnoceny jako špatné.
- **Stav v kódu:** [`25`](../../R/02_druhy/25_n2k_druhy_uzemi.R), blok `IND_SUMKLIC` / `LENIND_SUMKLIC`:
  ```r
  IND_SUMKLIC <  (LENIND_SUMKLIC - 1 - LENIND_NAKLIC) ~ 0     # spatny
  IND_SUMKLIC <  (LENIND_SUMKLIC     - LENIND_NAKLIC) ~ 0.5   # zhorseny
  IND_SUMKLIC >= (LENIND_SUMKLIC     - LENIND_NAKLIC) ~ 1     # dobry
  ```
- **Stav v datech:** na úrovni `chu` je pro všech 6 druhů **jediný** klíčový indikátor — `LOK_PROCDOBR` (`min 70`, `KLIC = ano`). Ostatní `chu` řádky (`LOK_DILCDOBRE`, `LOK_DILCPOCET`) nemají `LIM_IND`.
- **Důsledek:** `LENIND_SUMKLIC = 1`. Při selhání `IND_SUMKLIC = 0`, první podmínka `0 < 0` je nepravdivá → výsledek **0,5 (zhoršený)**. Stav „špatný" je nedosažitelný a EVL se špatným stavem populace i špatným procentem DP se vykáže jako „zhoršená".
- **Dotčené druhy:** všech 6
- **Návrh řešení:** implementovat druhý indikátor Tabulky 2 (H-06) a poté nahradit počítání klíčových indikátorů **explicitní rozhodovací tabulkou 2×2** podle Tabulky 2, ne obecnou aritmetikou. **H-05 a H-06 řešit společně** — samotné H-05 bez druhého indikátoru vyřešit nelze.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — řešit společně s H-06; aritmetiku nahradit explicitní rozhodovací tabulkou 2×2 dle Tabulky 2.

### H-06 ✅ — druhý indikátor Tabulky 2 (početnost vs. cílový stav) není implementován
- **Závažnost:** kritická · **Typ:** GAP
- **Metodika:** Tabulka 2 — druhý vstup je *počet jedinců (klouzavý průměr za poslední 3 roky)* porovnaný s cílovým stavem specifickým pro každé území.
- **Stav v kódu:** neimplementováno; komentář v [`25`](../../R/02_druhy/25_n2k_druhy_uzemi.R) to přiznává.
- **Zdroj dat rozhodnut zadavatelem:** `navrzena_hodnota` z `sdo_cilove_druhy.csv` (repozitář `BiodivMonCZ/digitalizaceSDO`). Ověřeno: 1 639 řádků, z toho **250 pro obojživelníky** ve **174 dvojicích `sitecode × druh`**.
- **Nástrahy ověřené na datech** (detailně v §4.8 zadání):

  | # | Zjištění |
  |---|---|
  | S-1 | `Lissotriton montandoni` je veden pod synonymem **`Triturus montandoni`** (`sdf_code` 2001) — přímý join na `nazev_lat` jej tiše zahodí |
  | S-2 | všech **6 řádků** `Triturus montandoni` má `navrzena_hodnota = NA` ⇒ pro tento druh cílový stav **neexistuje** |
  | S-3 | **69 duplicitních** dvojic `sitecode × druh`; u 171 ze 174 je hodnota shodná, **u 3 se liší** |
  | S-4 | `ndop_pocitano` nabývá jen `jedinci` / `adulti` / `NA` — **nikdy `samci`**, přestože u BBOM je hodnocenou jednotkou *vokalizující samci* |
  | S-5 | `navrzena_hodnota ≈ floor(max(pop_prum, ndop_pop_max))` — částečně odvozena z týchž NDOP dat, která se hodnotí; `varovani == TRUE` u 34 z 250 řádků |
  | S-6 | soubor je UTF-8, ale české textové sloupce jsou už ze zdroje poškozené na `U+FFFD`; potřebné sloupce jsou ASCII a nedotčené |

- **Strukturální překážka:** `limity_vse.csv` je klíčovaný `DRUH × ID_IND` a **nemá rozměr území**, takže limit specifický pro každou EVL v něm nelze vyjádřit. Varianty (a) rozšířit o `KOD_CHU`, (b) napojit cílovou hodnotu přímo v `25` mimo obecnou `minmax` větev — viz §4.8 zadání.
- **Otevřená otázka pro autory metodiky:** **S-4.** Jak porovnat cílový stav vedený v jedincích/adultech s hodnocenou jednotkou *vokalizující samci* u *Bombina bombina*? Bez rozhodnutí nelze indikátor u BBOM počítat.
- **Odpověď autorů (2026-08-20) k S-4:** „porovnat jedinci / adulti se samci. Dořešíme v příštích verzích cílových stavů, které projdou expertní revizí. Důležitá je funkčnost kódu."
- **Rozhodnutí zadavatele:** ✅ **schváleno** — indikátor implementovat a porovnávat **bez přepočtu jednotek**. Nesoulad jednotek u *Bombina bombina* (cíl v jedincích/adultech vs. hodnocení ve vokalizujících samcích) se **vědomě dočasně toleruje** a bude vyřešen v příští, expertně revidované verzi cílových stavů. **Podmínka:** rozdíl jednotek musí být zdokumentován v kódu i propsán do výstupu, aby nezůstal neviditelný.

---

# Vysoká závažnost

### H-07 — `POP_REPRO`: jednotka `metamorf. ex` vs. `metamorf. ex.` ⇒ 302 záznamů nezapočteno
- **Závažnost:** vysoká · **Typ:** BUG
- **Metodika:** §Vyhodnocení — reprodukce je doložena mj. **metamorfovanými jedinci**.
- **Stav v datech:** `limity_vse.csv` uvádí jednotku `metamorf. ex` (bez tečky); NDOP obsahuje **`metamorf. ex.` (s tečkou) — 302 záznamů; tvar bez tečky se v datech nevyskytuje ani jednou.**
- **Stav v kódu:** [`21_1:30`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L30) `lim_repro` se plní z `JEDNOTKA`, porovnání je `POCITANO_CLEAN %in% lim_repro` — přesná shoda.
- **Důsledek:** doklad reprodukce metamorfovanými jedinci se **nikdy** nezapočítá. Klíčový indikátor *prokázaná reprodukce* (`KLIC = ano` u BVAR, LMON, Triturus) tak může vyjít nepříznivě i tam, kde reprodukce doložena byla.
- **Dotčené druhy:** 6 (dopad na hodnocení u 5 — u BBOM je reprodukce `KLIC = ne`)
- **Návrh řešení:** opravit řetězec v `limity_vse.csv` na `metamorf. ex.`. **Zároveň prověřit ostatní jednotky** — `amplexus`, `snůšky m2/dm2/cm2` se v exportu rovněž nevyskytují a mohou být buď zastaralé, nebo dalšími překlepy.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — opravit na `metamorf. ex.` a prověřit i ostatní jednotky.

### H-08 — `STA_POKRVEGETACE`: hodnota `0 %` je u BVAR hodnocena jako nepříznivá
- **Závažnost:** vysoká · **Typ:** BUG
- **Metodika:** Příloha 1 — BVAR: „špatně nad 50 %". §Vyhodnocení: „Výjimkou je *Bombina variegata*, která pro rozmnožování vyhledává tůně v **ranných sukcesních stádiích**." Nulová pokryvnost je tedy pro BVAR **příznivá**.
- **Stav v datech:** BVAR má `val` výčet `0 | 0-25 % | 1-10 % | 11-25 % | 26-50 % | 1-25 %` a `max 50`. NDOP obsahuje **`0 %` (330 záznamů)** i `0` (231 záznamů).
- **Důsledek:** `"0 %"` se netrefí do `val` výčtu (ten zná jen `"0"`) a neprojde ani numerickou větví, protože `"0 %"` nevyhoví regexu `^-?\d+(\.\d+)?$` → `STAV_IND = 0`. **330 záznamů s nulovou pokryvností je u BVAR vyhodnoceno jako nepříznivých**, ačkoli jde o stav, který metodika pro tento druh označuje za vyhledávaný.
- **Dotčené druhy:** BVAR (u Triturus je `0 %` nepříznivé správně — práh „pod 1 %")
- **Návrh řešení:** doplnit `0 %` do `val` výčtu u BVAR; systémověji normalizovat procentní řetězce před porovnáním (viz H-03).
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — doplnit `0 %` a normalizovat procentní řetězce.

### H-09 ✅ — `POP_POCETNOSTNAL`: osmistupňová škála místo šestistupňové, `11-100` slučuje dvě třídy
- **Závažnost:** vysoká · **Typ:** BUG
- **Metodika:** §Sledované indikátory i §Vyhodnocení definují **šestistupňovou** škálu: absence 0 · jednotky 1–10 (1) · **nižší desítky 11–50 (2)** · **vyšší desítky 51–100 (3)** · stovky 101–1000 (4) · tisíce 1001+ (5).
- **Stav v kódu:** [`21_1:128–157`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L128-L157) používá stupně **až 8** a mapuje `"11-100" → 3`.
- **Stav v datech:** `REL_POC` obsahuje `11-100` u **1 630 záznamů** — ty spadají celé do stupně 3, ačkoli podle metodiky mohou patřit do 2 i do 3. Dále mezerové varianty `1 - 10` (79 záznamů) a `101 - 1000` (31) nejsou mapovány vůbec → `NA`. Slovní hodnoty (`ojediněle`, `hojně`, `roztroušeně`, `vzácně`, `velmi hojně`) rovněž → `NA`.
- **Důsledek:** klíčový indikátor `POP_ZMENARAD` („pokles o více než 1 kategorii") měří na jiné škále, než metodika předepisuje. Záznam `11-100` v jednom roce a `řádově nižší desítky` v druhém dá umělý rozdíl 1 kategorie bez reálné změny početnosti.
- **Dotčené druhy:** všech 6
- **Návrh řešení:** srovnat škálu na 0–5 podle metodiky; `11-100` řešit buď rozpadem podle `POCET`, je-li k dispozici, nebo ponechat `NA` (nelze zařadit); doplnit mezerové varianty.
- **Otevřená otázka pro autory metodiky:** jak zařadit historické záznamy `11-100`, které přesahují hranici mezi *nižšími* a *vyššími desítkami*?
- **Odpověď autorů (2026-08-20):** „bere nižší kategorie, konzervativní předběžná opatrnost".
- **Rozhodnutí zadavatele:** ✅ **schváleno** — škálu srovnat na 0–5 dle metodiky; u rozpětí přesahujícího hranici tříd se bere **nižší kategorie** (`"11-100"` → **2**, tj. nižší desítky). Totéž pravidlo platí pro další víceznačná rozpětí. Doplnit mezerové varianty (`1 - 10`, `101 - 1000`).

### H-10 — `STA_PRUHLEDNOSTVODAR` se vůbec nečte ⇒ 1 931 záznamů zahozeno
- **Závažnost:** vysoká · **Typ:** BUG
- **Metodika:** Příloha 1 — *průhlednost vody* hodnocena u BBOM a Triturus („špatně pod 50 cm").
- **Stav v kódu:** [`21_1:438–457`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L438-L457) čte `STA_PRUHLEDNOSTVODA` a `STA_PRUHLEDNOSTVODAT`, **`STA_PRUHLEDNOSTVODAR` nikoli**.
- **Stav v datech:** `STA_PRUHLEDNOSTVODAT` 3 318 · `STA_PRUHLEDNOSTVODA` 1 508 · **`STA_PRUHLEDNOSTVODAR` 1 931** (zjevně varianta pro rybníky).
- **Důsledek:** u rybničních DP průhlednost chybí → indikátor se nehodnotí, ačkoli data existují.
- **Dotčené druhy:** BBOM, Triturus ×3
- **Návrh řešení:** doplnit `STA_PRUHLEDNOSTVODAR` do sjednocení. **Pozor:** priorita `VODAT > VODA` dnes přebíjí číselnou hodnotu kategorií; při doplnění třetího zdroje je nutné pravidlo priority stanovit explicitně a zdůvodnit.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — doplnit `STA_PRUHLEDNOSTVODAR`; pravidlo priority stanovit explicitně a zdůvodnit v komentáři.

### H-11 — `POP_REPROPERIOD3` a `STA_VYSYCHANIPERIOD3` se připojují bez `ROK`
- **Závažnost:** vysoká · **Typ:** BUG
- **Metodika:** reprodukce — „ani jednou ze **tří posledních sezón s monitoringem dané DP**"; vysychání — „v každém ze **tří posledních hodnocených let**".
- **Stav v kódu:** [`21_1:1141–1155`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1141-L1155) počítá obě hodnoty jednou pro `KOD_LOKAL + DRUH` ze tří nejnovějších řádků; závěrečný `left_join` je **jen podle `KOD_LOKAL` a `DRUH`, bez `ROK`**.
- **Důsledek:** všechny ročníky dané DP dostanou tutéž hodnotu, odvozenou z nejnovějších dat. Hodnocení roku 2019 tak může být ovlivněno pozorováním z roku 2025. Zpětné hodnocení není reprodukovatelné.
- **Dotčené druhy:** všech 6
- **Návrh řešení:** počítat oba indikátory jako klouzavé okno **pro každý hodnocený rok zvlášť** (3 poslední monitorované sezóny **do daného roku včetně**) a připojovat i podle `ROK`.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — klouzavé okno počítat pro každý hodnocený rok zvlášť. **Implementovat až po H-03.**

### H-12 — `STA_MANIPULACE`: prázdný řetězec je hodnocen jako nepříznivý
- **Závažnost:** vysoká · **Typ:** BUG
- **Stav v datech:** hodnoty tagu jsou `ne` (1 647), **prázdný řetězec (402)** a `ano` (**57**). *(Oprava proti první verzi registru, kde byly počty `ano` a prázdného řetězce prohozeny — správně je prázdných 402.)*
- **Důsledek:** `""` ≠ `"ne"` → `STAV_IND = 0`. **402 nevyplněných záznamů je penalizováno jako zjištěná manipulace** — sedmkrát více, než kolik je skutečných záznamů `ano` (57). Metodika přitom říká: „Indikátor se hodnotí pouze, jsou-li dostupné informace k jeho hodnocení."
- **Dotčené druhy:** BBOM, LMON, Triturus ×3 (u BVAR se nehodnotí)
- **Návrh řešení:** prázdné řetězce normalizovat na `NA` **globálně při extrakci ze `STRUKT_POZN`** — tentýž vzorec se týká i dalších tagů.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno.** Řeší se společně s H-18 (nový zdroj indikátoru).

---

# Střední závažnost

### H-13 — mrtvé slučování `STA_ZASTINENIHLADINA` v `21_1`
- **Závažnost:** střední · **Typ:** SIMPLIFIKACE
- Po revizi metodiky (2026-08-20) je hodnoceným indikátorem `STA_ZASTINENILITORAL`. [`21_1:465–493`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L465-L493) stále přepisuje `STA_ZASTINENIHLADINA` horší z dvojice hladina/litorál — vzniklý sloupec už **nikdo nekonzumuje**.
- **Poznámka k dopadu:** `STA_ZASTINENIHLADINA` má v datech **8 065** záznamů, `STA_ZASTINENILITORAL` jen **4 410**. Přechod na litorál tedy snižuje pokrytí indikátoru zhruba na polovinu. Zato je doména litorálu **čistá** — právě 4 pásma `0-25 / 26-50 / 51-75 / 76-100 %`, bez holých čísel, na rozdíl od hladiny (31 různých hodnot).
- **Návrh řešení:** slučování odstranit, `STA_ZASTINENILITORAL` používat přímo.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — slučování odstranit.

### H-14 ✅ — číselník: chybějící `ind_id` pro tři hodnocené indikátory
- **Závažnost:** střední · **Typ:** STOPA-DO-ISOP
- `POP_REPROPERIOD3` — **v číselníku chybí úplně**
- `STA_VYSYCHANIPERIOD3` — **v číselníku chybí úplně**
- `STA_HLOUBKAMENSI20` — v číselníku je, ale **bez `ind_id`**
- **Důsledek:** [`27:620`](../../R/02_druhy/27_n2k_druhy_zapis.R#L620) napojuje `ind_r → ind_id`; bez záznamu propadne export na surový textový název místo kódu ISOP.
- **Poznámka:** `POP_REPROPERIOD3` a `STA_VYSYCHANIPERIOD3` jsou přitom **jediné dva indikátory, které skutečně implementují dva řádky Přílohy 1** (reprodukce za 3 roky, vysychání ve 3 letech). Jejich `ind_id` je proto potřeba získat z ISOP.
- **Odpověď autorů (2026-08-20):** **`POP_REPROPERIOD3` = 30**, **`STA_VYSYCHANIPERIOD3` = 34**.
- **Rozhodnutí zadavatele:** ✅ **schváleno.** **Pozor — jde o přesun, ne o volné kódy:** `ind_id` 30 dnes patří `POP_REPRO` a 34 patří `STA_VYSYCHANI`. Obojí jsou přitom indikátory, které se **nevyhodnocují** (`POP_REPRO` má `LIM_IND = NA` a slouží jen jako výčet jednotek; `STA_VYSYCHANI` přijde o limity dle H-01/H-02). Kódy proto přecházejí na skutečně hodnocené tříleté indikátory a původní řádky zůstanou v číselníku **bez `ind_id`**. **Otevřeno:** `ind_id` pro `STA_PLOCHA50CM` — viz H-04.

### H-15 ✅ — sezónní okno `STA_PRUHLEDNOSTVODA` bez opory v metodice
- **Závažnost:** střední · **Typ:** GAP
- [`21_1:455`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L455) omezuje průhlednost na **květen až 15. června**. Metodika žádné takové omezení neuvádí; nejblíže je věta z terénní části: „Je to vhodné především pro zaznamenání průhlednosti v **reprezentativnějším období**" — doporučení k pořadí návštěv, ne limit pro vyhodnocení.
- **Důsledek:** záznamy mimo okno se zahazují bez normativního důvodu.
- **Otevřená otázka pro autory metodiky:** má se průhlednost hodnotit jen z určitého období? Pokud ano, patří pravidlo do §Vyhodnocení.
- **Odpověď autorů (2026-08-20):** „zruš časové omezení, ale popiš změnu".
- **Rozhodnutí zadavatele:** ✅ **schváleno** — sezónní okno květen – 15. června **odstranit**; do kódu doplnit komentář, že omezení nemělo oporu v metodice, a změnu vypsat v reportu Fáze B.

### H-16 — `cis_pocet_kat`: medián kategorie *stovky* je 550, metodika uvádí 500
- **Závažnost:** střední · **Typ:** BUG
- Metodika (Tabulka 2, *početnost populace*): „převádí se na odpovídající hodnotu **mediánu** dané kategorie… (např. **500 jedinců pro kategorii stovky**)". `Data/Input/cis_pocet_kat.csv` má pro stupeň 4 hodnotu **550**.
- **Dopad zatím nulový** — převod se použije až v druhém indikátoru Tabulky 2 (H-06), který není implementován. Řešit **spolu s H-06**.
- **Rozhodnutí zadavatele (2026-08-20):** ✅ **schváleno** — srovnat na 500; implementovat spolu s H-06.

### H-17 ⚠ — `STA_INVDRUHRYBA`: nová čtyřkategoriová doména proti limitu `val "ne"`
- **Závažnost:** střední · **Typ:** GAP (výhledový)
- **Metodika (nová):** „Zaznamenává se v kategoriích: **ano / ne / nelze vyloučit / nehodnoceno**."
- **Stav v datech:** export zatím obsahuje **pouze `ne` (3 018) a `ano` (427)** — nové kategorie se ještě nepoužívají.
- **Důsledek:** dnes je chování správné. Jakmile se Survey123 přepne na novou škálu, `val "ne"` začne hodnotit **`nehodnoceno` i `nelze vyloučit` jako nepříznivý stav** — přitom `nehodnoceno` má být neznámý stav.
- **Otevřená otázka pro autory metodiky:** jak hodnotit `nelze vyloučit`? (`nehodnoceno` → jednoznačně neznámý stav.)
- **Odpověď autorů (2026-08-20):** „viz prompt".
- **Stav:** ⚠ **nedořešeno.** Zadání ani metodika neurčují, jak hodnotit kategorii **`nelze vyloučit`**; `nehodnoceno` → neznámý stav je naproti tomu jednoznačné. Dopad je zatím nulový (data obsahují pouze `ano`/`ne`), takže **Fázi B to neblokuje** — rozhodnout je ale nutné dřív, než Survey123 přejde na novou škálu. Viz otázka 4.

### H-18 ✅ — `STA_MANIPULACE`: metodika uvádí jiný zdroj záznamu než kód
- **Závažnost:** střední · **Typ:** NÁZVOSLOVÍ
- **Metodika:** „Manipulace s vodní hladinou. Zaznamenává se **ve Vlivech v části Voda**."
- **Stav v kódu/datech:** kód čte tag `<STA_MANIPULACE>` ze `STRUKT_POZN`; tento tag v datech **existuje** (2 106 záznamů), zatímco `VLV_VLIVY` má 8 888 záznamů.
- **Důsledek:** buď je věta v metodice nepřesná, nebo se část manipulací zaznamenává do Vlivů a hodnocení je nezachytí.
- **Otevřená otázka pro autory metodiky:** je `<STA_MANIPULACE>` závazným zdrojem, nebo se má indikátor odvozovat (i) z Vlivů?
- **Odpověď autorů (2026-08-20):** „STA_MANIPULACE odvozovat podle metodiky" — tj. z **Vlivů, část Voda**.
- **Ověřeno na datech:** `VLV_VLIVY` odpovídající kategorie skutečně obsahuje — *„změna hydrologických poměrů (např. [nevhodná] manipulace s vodní hladinou)"* a *„regulování vodní hladiny"*; 542 záznamů odpovídá vzoru `manipul`, 617 vzoru `hladin`. Pokrytí je navíc výrazně vyšší než u tagu: `VLV_VLIVY` 8 888 záznamů vs. `STA_MANIPULACE` 2 106 (1 293 má oba, 7 595 jen `VLV_VLIVY`).
- **Rozhodnutí zadavatele:** ✅ **schváleno** — indikátor odvozovat z `VLV_VLIVY` dle metodiky. **Podmínky:** (a) zachovat sezónní omezení duben–červenec dle §Vyhodnocení; (b) `VLV_VLIVY` je víceznačný seznam, jehož názvy kategorií samy obsahují čárky — párovat **vzorem nad celým řetězcem**, nikdy ne dělením podle čárky; (c) doložit dopad změny zdroje na počet hodnocených DP.

---

# Potvrzeno — bez akce

| # | Zjištění |
|---|---|
| P-01 | **Pokrytí Přílohy 1 je úplné a správné.** Všech 11 řádků × správné množiny druhů, strojově ověřeno. Žádný chybějící ani přebývající druh. |
| P-02 | **`KLIC` odpovídá metodice.** Klíčové jsou právě `POP_PRESENCE`, `POP_ZMENARAD`, `POP_REPROPERIOD3`; u BBOM je reprodukce správně `KLIC = ne` („doplňkově"). Žádný stanovištní indikátor nemá `KLIC = ano`. |
| P-03 | **M-06 vyřešeno** (2026-08-20): pásmo `val 0-25 %` u BBOM přesunuto na `STA_ZASTINENILITORAL`; BBOM i BVAR mají shodnou sadu `0-25 / 26-50 / 51-75 / max 75`. |
| P-04 | **M-16, M-17, M-18 vyřešeny revizí metodiky.** Stav „neznámý" na úrovni DP nevzniká; implementace v [`24`](../../R/02_druhy/24_n2k_druhy_lokality.R) (`N_KEY_EXPECTED` / `N_OTH_EXPECTED` počítají jen ne-`NA` indikátory) je **správná a nemění se**. |
| P-05 | **`POP_ZMENARAD` referenční rok je implementován správně** — [`21_1:1064–1095`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L1064-L1095) hledá poslední předchozí rok s `CILMON == 1` na téže DP, přesně dle metodiky. (Vada je jen ve škále, viz H-09.) |
| P-06 | **Žádní sirotci.** Každý `ID_IND` s limitem u 6 druhů má odpovídající výpočet (`LOK_PROCDOBR` se počítá v `25`, což je v pořádku). |

---

# Matice pokrytí

`OK` = shoda metodika ↔ limity ↔ kód ↔ data · `N/A` = pro druh se nehodnotí · `H-nn` = nález

| Řádek Přílohy 1 | ID_IND | BBOM | BVAR | LMON | Tcri | Tcar | Tdob |
|---|---|---|---|---|---|---|---|
| přítomnost druhu | `POP_PRESENCE` | OK | OK | OK | OK | OK | OK |
| porovnání odhadované početnosti | `POP_ZMENARAD` | H-09 | H-09 | H-09 | H-09 | H-09 | H-09 |
| zaznamenávání reprodukce | `POP_REPROPERIOD3` | H-07 H-11 H-14 | H-07 H-11 H-14 | H-07 H-11 H-14 | H-07 H-11 H-14 | H-07 H-11 H-14 | H-07 H-11 H-14 |
| nadměrný tlak ryb | `STA_RYBY` | H-17 | H-17 | H-17 | H-17 | H-17 | H-17 |
| manipulace s vodní hladinou | `STA_MANIPULACE` | H-12 H-18 | **N/A** | H-12 H-18 | H-12 H-18 | H-12 H-18 | H-12 H-18 |
| pravidelné vysychání | `STA_VYSYCHANIPERIOD3` | H-03 H-11 H-14 | H-03 H-11 H-14 | H-03 H-11 H-14 | H-03 H-11 H-14 | H-03 H-11 H-14 | H-03 H-11 H-14 |
| *(navíc, mimo Přílohu 1)* | `STA_VYSYCHANI` | H-01 H-02 | H-01 H-02 | H-01 H-02 | H-01 H-02 | H-01 H-02 | H-01 H-02 |
| zastoupení vodní vegetace | `STA_POKRVEGETACE` | **N/A** | H-08 | **N/A** | OK | OK | OK |
| průhlednost vody | `STA_PRUHLEDNOSTVODA` | H-10 H-15 | **N/A** | **N/A** | H-10 H-15 | H-10 H-15 | H-10 H-15 |
| zastínění litorálu | `STA_ZASTINENILITORAL` | OK (H-13) | OK (H-13) | **N/A** | **N/A** | **N/A** | **N/A** |
| plocha s hloubkou < 50 cm | `STA_HLOUBKAMENSI20` | **H-04** | **H-04** | **H-04** | **H-04** | **H-04** | **H-04** |
| úhyn obojživelníků | `STA_UHYNOBOJZIVELNIK` | OK | OK | OK | OK | OK | OK |
| **Tabulka 1** (úroveň DP) | `24` | OK (P-04) | OK | OK | OK | OK | OK |
| **Tabulka 2** (úroveň EVL) | `25` | **H-05 H-06** | **H-05 H-06** | **H-05 H-06 S-2** | **H-05 H-06** | **H-05 H-06** | **H-05 H-06** |

**Zcela čisté jsou pouze 2 z 11 indikátorů** (`POP_PRESENCE`, `STA_UHYNOBOJZIVELNIK`)
plus `STA_ZASTINENILITORAL` a `STA_POKRVEGETACE` u části druhů.

---

# Limity bez normativního zdroje — `Epidalea calamita`

**Mimo rozsah harmonizace** (rozhodnutí zadavatele). Druh má v `limity_vse.csv`
**36 řádků**, ale **v Příloze 1 metodiky se nevyskytuje** — není druhem přílohy II
Směrnice o stanovištích. Evidováno pro dohledatelnost, **neupravovat**:

| Zjištění |
|---|
| `POP_ZMENARAD` má `TYP_IND = max` (u všech 6 druhů metodiky je `min`) — **obrácená logika**: dobrý stav by nastal jen při poklesu o 1 a více kategorií |
| **Chybí `POP_PRESENCE`** — klíčový indikátor, který mají všechny ostatní druhy |
| **Chybí `LOK_PROCDOBR`** — jediný hodnocený indikátor na úrovni `chu` |
| **Duplicitní řádky** u `LOK_POCETDOBR` a `POP_REPROPERIOD3` |
| `LOK_POCETDOBR` je **sirotek** — žádný kód tento název nepočítá (`25` počítá `LOK_POCETDOB` a `LOK_PROCDOBR`) |
| `ID_IND` jen u tohoto druhu: `LOK_POCETDOBR`, `POP_POCET`, `POP_POCETMAX`, `POP_POCETMIN`, `POP_REPRO`, `STA_ZTRATABIO`, `VLV_VLIVY` |

**Doporučení:** rozhodnout, podle jakého dokumentu se *Epidalea calamita* hodnotí
(záchranný program / PROSPECTIVE LIFE), a harmonizovat ji samostatným během.

---

# Otázky na autory metodiky — stav k 2026-08-20

| # | Otázka | Odpověď | Blokuje |
|---|---|---|---|
| **1** | **S-4:** Cílový stav v `sdo_cilove_druhy.csv` je veden v jednotkách `jedinci`/`adulti`, hodnocenou jednotkou u *Bombina bombina* jsou ale **vokalizující samci**. Jak obě veličiny porovnat? | ✅ Porovnávat bez přepočtu; nesoulad se dořeší v příští, expertně revidované verzi cílových stavů. Přednost má funkčnost kódu. | — |
| **2** | **H-04:** Pod jakým tagem se bude zaznamenávat *plocha s hloubkou menší než 50 cm*? | ✅ Tag bude **`STA_PLOCHA50CM`**, hodnoceno **až od roku 2027**; v datech zatím chybí. | — |
| **2b** | **H-04 / H-14:** Jaké `ind_id` dostane `STA_PLOCHA50CM` v ISOP? | ⚠ **nezodpovězeno** | dokončení H-04 |
| **3** | **H-03:** Má mít `STA_STAVVODAPERTUNE` (periodické tůně) stejnou váhu jako trvalé tůně? | ✅ **Ano, stejná váha.** | — |
| **4** | **H-17:** Jak hodnotit kategorii **`nelze vyloučit`** u nadměrného tlaku ryb? | ⚠ **nezodpovězeno** — odpověď zněla „viz prompt", zadání ani metodika však tuto kategorii neřeší. `nehodnoceno` → neznámý stav je jednoznačné. Dopad je zatím nulový (data mají jen `ano`/`ne`). | H-17 (neblokuje Fázi B) |
| **5** | **H-09:** Jak zařadit historické záznamy `REL_POC = "11-100"`? | ✅ **Nižší kategorie** — konzervativní předběžná opatrnost. | — |
| **6** | **H-15:** Má se průhlednost hodnotit jen z určitého období? | ✅ **Časové omezení zrušit**, změnu popsat. | — |
| **7** | **H-18:** Je `<STA_MANIPULACE>` závazným zdrojem, nebo se má odvozovat z Vlivů? | ✅ **Odvozovat podle metodiky**, tj. z Vlivů (část Voda). Ověřeno, že `VLV_VLIVY` potřebné kategorie obsahuje. | — |
| **8** | **H-14:** `ind_id` pro tříleté indikátory. | ✅ `POP_REPROPERIOD3` = **30**, `STA_VYSYCHANIPERIOD3` = **34**. | — |

**Zbývají dvě otevřené položky — 2b a 4 — a ani jedna neblokuje Fázi B.**

---

# Doporučené pořadí implementace (Fáze B)

| Pořadí | Nálezy | Stav | Poznámka |
|---|---|---|---|
| 1 | H-01, H-02 | ✅ | nezávislé, čistě datové, okamžitý efekt |
| 2 | H-07, H-08, H-12 | ✅ | opravy řetězců a normalizace prázdných hodnot |
| 3 | **H-03** | ✅ | přepis převodu stavu vody — největší jednotlivý dopad na výsledky |
| 4 | H-11 | ✅ | klouzavá okna per rok; **provést až po H-03**, jinak se opraví jen šíření chybné hodnoty |
| 5 | H-09, H-10, H-15 | ✅ | škála početnosti, doplnění zdroje průhlednosti, zrušení sezónního okna |
| 6 | **H-18** + H-12 | ✅ | změna zdroje `STA_MANIPULACE` na Vlivy; doložit dopad na počet hodnocených DP |
| 7 | H-13, H-14, H-04 | ✅ / ⏸ | úklid, přesun `ind_id`, přejmenování na `STA_PLOCHA50CM` (hodnota zůstává `NA` do 2027) |
| 8 | **H-05 + H-06 + H-16 společně** | ✅ | úroveň EVL — vendorování `sdo_cilove_druhy.csv`, nový `chu` indikátor, rozhodovací tabulka 2×2 |
| — | H-17 | ⚠ | odloženo do rozhodnutí o `nelze vyloučit`; dnes bez dopadu |

**Všech 17 z 18 nálezů je schváleno k implementaci** (H-04 částečně — přejmenování ano,
naplnění daty až 2027; H-17 odloženo bez dopadu).

---

**Fáze A ukončena, rozhodnutí zadavatele zaznamenána 2026-08-20. Fáze B provedena — viz níže.**


---

# FÁZE B — implementace (2026-08-20)

Provedeno v 8 commitech, jeden krok = jeden commit. Větev `202608-obojzivelnici`.

| Commit | Nálezy | Soubory |
|---|---|---|
| `824aa2f` | H-01, H-02, H-17 (data) | `limity_vse.csv`, `cis_indikatory_popis.csv` |
| `adef4d9` | H-07, H-08 | `limity_vse.csv` |
| `b2e3473` | **H-03** | `21_1` |
| `564a682` | H-11 | `21_1` |
| `7678b87` | H-09, H-10, H-15 | `21_1` |
| `d532550` | H-18, H-12 | `21_1` |
| `06f1d41` | H-13, H-14, H-04, H-17 (kód) | `21_1`, `limity_vse.csv`, `cis_indikatory_popis.csv` |
| `004c3bd` | **H-05, H-06, H-16** | `00_n2k_config.R`, `25`, `27`, `cis_pocet_kat.csv`, + vendorovaný snapshot |

## Stav nálezů

| Nález | Stav |
|---|---|
| H-01, H-02, H-03, H-05, H-06 | ✅ implementováno |
| H-07, H-08, H-09, H-10, H-11, H-12 | ✅ implementováno |
| H-13, H-14, H-15, H-16, H-18 | ✅ implementováno |
| H-04 | ⏸ přejmenováno na `STA_PLOCHA50CM`; hodnota zůstává `NA` do roku 2027 (tag v datech neexistuje). `ind_id` nepřiděleno — odsouhlaseno jako dočasný stav |
| H-17 | ✅ implementováno — `nelze vyloučit` v limitech jako příznivé, `nehodnoceno` → `NA` |
| **H-19, H-20** | ⚠ **nové, vzniklé při implementaci — čekají na rozhodnutí** |

## Měřený dopad (ostrý export z NDOP, 31 733 záznamů 6 druhů)

| Nález | Před | Po |
|---|---|---|
| H-03 · záznamů „nevysychá" → „vysychá" | — | **436** (`0-25 %` 193×, `vyschlé` 116×, holá `0` 58×) |
| H-03 · nově vůbec hodnoceno | — | **1 289** (hlavně periodické tůně) |
| H-03 · prázdné řetězce vydávané za měření | 305 | 0 (nyní `NA`) |
| H-07 · doklady reprodukce metamorfy | 0 | **302** |
| H-08 · `0 %` pokryvnost u BVAR chybně nepříznivá | 330 | 0 |
| H-09 · `REL_POC = "11-100"` přeřazeno na nižší kategorii | — | **1 630** |
| H-10 + H-15 · hodnot průhlednosti | 2 115 | **6 691** (3,2×) |
| H-12 · prázdné `STA_MANIPULACE` hodnocené jako manipulace | 358 | 0 |
| H-18 · hodnotitelných záznamů manipulace | 1 815 | **8 365** |
| H-05 · dosažitelnost stavu „špatný" na úrovni EVL | **nikdy** | dle Tabulky 2 |
| H-06 · území s cílovým stavem | 0 | **174** dvojic území × druh |

## Ověření

| Co | Výsledek |
|---|---|
| Syntaktická kontrola všech 6 dotčených `.R` souborů | ✅ `parse()` prochází |
| Jednotkové testy `norm_stavvody()`, `stav_vody_slovni()`, `roll3_sum()` | ✅ 19 + 6 + 5 případů |
| Rozhodovací tabulka 2 — všech 9 kombinací vstupů | ✅ odpovídá metodice |
| Načtení `cilove_stavy` v `00_n2k_config.R` | ✅ 910 řádků, 174 pro druhy metodiky, 0 duplicit |
| Matice pokrytí Přílohy 1 po zásazích | ✅ 11/11 řádků, žádný indikátor mimo Přílohu 1 |
| `ind_id` u hodnocených indikátorů | ✅ všechny kromě `STA_PLOCHA50CM` (vědomě) |
| **Plný běh kaskády `20_n2k_druhy_run.R`** | ❌ **nelze** — `00_n2k_config.R:396` čte `Data/Input/AktualizacniOkrsky.shp`, který v repozitáři není (zaveden commitem `49c92c3`, nesouvisí s harmonizací). Blok `cilove_stavy` na ř. 235 se stihne načíst před touto chybou. |

**Plný běh proto nebyl proveden.** Ověření stojí na statické kontrole, jednotkových
testech a měření dopadu nad ostrými daty — ne na průchodu celé kaskády.

## Kontrola neregrese mimo obojživelníky

Sdílený kód obsluhuje i ryby a mihule, hmyz, savce a cévnaté rostliny.

| Zásah | Dopad mimo obojživelníky |
|---|---|
| `POP_POCETNOSTNAL`, kategorie `"11-100"` | **podmíněno druhem** (`je_obojzivelnik`); ostatní skupiny si drží původní zařazení 3 |
| `POP_POCETNOSTNAL`, díry u hodnot 50 a 51 | opraveno pro všechny — dřív končily jako `NA`, jde o jednoznačnou chybu |
| `norm_stavvody()`, `STA_STAVVODAPERTUNE` | týká se jen tagů `STA_STAVVODA*`, tj. obojživelníků (včetně `Epidalea calamita`) — vždy jen zpřesnění rozpoznání, nikdy změna prahu |
| `STA_ZTRATABIO` | větev `TRUE ~ "ne"` **záměrně ponechána**, aby se nezměnilo hodnocení `Epidalea calamita` |
| `cis_pocet_kat` 550 → 500 | týká se převodu relativních kategorií; u ostatních skupin se uplatní jen tam, kde se počty odvozují z kategorií |
| `25` — Tabulka 2 | aktivuje se **jen** při předaných `cilove_stavy` a `pocetnost_uzemi`; jinak zůstává původní větev |
| `27` — fáze 1b | nový výstup do `Data/Temp/`, nic nepřepisuje |

`Epidalea calamita` — limity nedotčeny (36 řádků), viz §Limity bez normativního zdroje.

---

# Nové nálezy vzniklé při implementaci

### H-19 ⚠ — prázdné tříleté okno: `0` (nesplněno) vs. `NA` (nehodnoceno)
- **Závažnost:** vysoká · **Typ:** BUG · **Stav:** implementováno jako `NA`, **k potvrzení**
- **Kontext:** původní `sum(x, na.rm = TRUE)` vracel pro okno bez jediné hodnoty **0**.
  U `POP_REPROPERIOD3` (limit `min 1`) to znamená **nesplněný KLÍČOVÝ indikátor** —
  a jediný nesplněný klíčový indikátor sráží DP rovnou na „špatný". Přitom příčinou
  je pouze to, že reprodukce nebyla vůbec zjišťována (např. návštěva zaznamenala
  jen dospělce). Týká se BVAR, LMON a všech tří druhů *Triturus*, kde je
  `KLIC = ano`.
- **Metodika:** *„Indikátor se hodnotí pouze, jsou-li dostupné informace k jeho
  hodnocení."* (§Vyhodnocení, závazné dle P-04).
- **Provedeno:** `roll3_sum()` vrací `NA`, není-li v okně ani jedna nechybějící hodnota.
- **Proč je to zapsáno jako nález:** jde o změnu chování nad rámec doslovného znění
  H-11. Výsledky se posouvají směrem k lepšímu hodnocení, proto to má být vědomé
  rozhodnutí, ne vedlejší efekt.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-20 ⚠ — území bez evidovaného cílového stavu v Tabulce 2
- **Závažnost:** střední · **Typ:** GAP · **Stav:** implementován předpoklad, **k potvrzení**
- **Kontext:** Tabulka 2 kombinuje dva indikátory a neřeší případ, kdy jeden z nich
  chybí. Cílový stav chybí u **celého druhu `Lissotriton montandoni`** (všech 6 řádků
  v SDO má prázdnou `navrzena_hodnota`) a u území, která v SDO nejsou.
- **Provedeno:** je-li cílový stav neznámý, hodnotí se území **jen podle
  `LOK_PROCDOBR`** (≥ 70 % → dobrý, < 70 % → zhoršený) a stav „špatný" nemůže
  nastat, protože ten Tabulka 2 vyhrazuje selhání **obou** indikátorů. Odpovídá to
  chování před harmonizací.
- **Alternativa:** označit takové území jako „neznámý". To by ale u
  *Lissotriton montandoni* znamenalo neznámý stav ve **všech** územích.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Nálezy z testovacího běhu (2026-08-25, *Triturus cristatus*)

Zdroj: běh kaskády `21_1` → `27` nad jediným druhem (8 322 záznamů,
**724 DP ve 191 EVL**). Plný `00_n2k_config.R` nelze na pracovní stanici
spustit (chybí `export_redlist.csv`, `export_invaze.csv`, `export_expanze.csv`,
`BiotopZvld.shp`, `AktualizacniOkrsky.shp` a sahá za běhu na WFS ČÚZK), proto
byl použit ořezaný konfigurační skript, který příslušné bloky configu přebírá
doslovně a omezuje `n2k_load` na jeden druh. Všechny následné transformace jsou
řádkové, předfiltrování je tedy vůči plnému běhu ekvivalentní.

### H-21 ✅ — nevyhodnocený indikátor se počítal jako splněný (Tabulka 1)
- **Závažnost:** kritická · **Typ:** BUG · **Stav:** implementováno (commit `e7460ba`)
- **Metodika:** Tabulka 1 — „min 1 špatně hodnocený populační (klíčový) indikátor
  → **špatný**"; „0 špatných klíčových a min 2 špatné stanovištní → **zhoršený**".
- **Stav v kódu:** [`24:130-131`](../../R/02_druhy/24_n2k_druhy_lokality.R#L130-L131) —
  `n_distinct(ID_IND[... & STAV_IND == 1])`. Pro řádek se `STAV_IND = NA` se celá
  podmínka vyhodnotí na `NA`, `ID_IND[NA]` vrátí `NA_character_` a `n_distinct()`
  jej započítá jako další hodnotu. `N_KEY_EXPECTED` / `N_OTH_EXPECTED` (ř. 126–127)
  filtr `!is.na(STAV_IND)` **už obsahovaly** — nesouměrnost byla jen u `*_PASSED`.
- **Důsledek:** každá DP s alespoň jedním nevyhodnoceným indikátorem dostala
  k počtu splněných indikátorů **+1**. Podmínka `N_KEY_PASSED < N_KEY_EXPECTED`
  proto vyšla nepravdivá i tam, kde klíčový indikátor skutečně selhal, a DP se
  vykázala jako „dobrý". U stanovištních indikátorů se hranice „min 2" fakticky
  posunula na „min 3" — u obojživelníků **univerzálně**, protože `STA_PLOCHA50CM`
  má vyplněný limit, ale hodnota se sbírá až od r. 2027, takže je `NA` pro
  **100 % DP** (724/724). Stav „zhoršený" tak na úrovni DP vůbec nevznikal.
- **Rozsah v testovacím běhu:** inflace se projevila u **487/724 DP** u klíčových
  a u **724/724 DP** u stanovištních indikátorů.
- **Doklad o dopadu:**

  | úroveň | před | po |
  |---|---|---|
  | DP | 377 dobrý / **0** zhoršený / 347 špatný | 334 / 16 / 374 |
  | EVL | 39 dobrý / 33 zhoršený / 31 špatný / 88 neznámý | 38 / 28 / 37 / 88 |

  **27 DP** mělo selhávající klíčový indikátor a přesto stav „dobrý".
  Přes `LOK_PROCDOBR` se změna promítla do **7 ze 103** hodnocených EVL,
  z toho 6× zhoršený → **špatný**.
- **Minimální příklad:** `POP_PRESENCE = 1`, `POP_REPROPERIOD3 = 0`,
  `POP_ZMENARAD = NA` ⇒ `N_KEY_EXPECTED = 2`, `N_KEY_PASSED = 2` (správně 1)
  ⇒ „dobrý" místo „špatný".
- **Kontrola neregrese:** větev je sdílená i pro ryby, hmyz, savce a rostliny;
  změna tam **není neutrální**. Je však **jednosměrná** — opravený počet splněných
  indikátorů je vždy ≤ původnímu, hodnocení DP se proto může jen zhoršit, nikdy
  zlepšit, a dotkne se pouze DP, kde některý indikátor s vyplněným limitem zůstal
  nevyhodnocen. Podmínit opravu druhem/skupinou by znamenalo vědomě ponechat
  „chybějící údaj = splněný indikátor" u ostatních skupin.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel — zejména potvrzení, že se oprava
  má uplatnit i mimo obojživelníky)_

### H-22 ✅ — řádky `POP_POCETPRUM3` se ztrácely před zápisem
- **Závažnost:** vysoká · **Typ:** BUG / STOPA-DO-ISOP · **Stav:** implementováno (commit `83e41e2`)
- **Metodika:** Tabulka 2 — druhý vstup je *počet jedinců (klouzavý průměr za
  poslední 3 roky)* porovnaný s cílovým stavem území. Rozhodnutí zadavatele
  k [H-06](#h-06--druhý-indikátor-tabulky-2-početnost-vs-cílový-stav-není-implementován)
  navíc **podmínkou** ukládá propsat rozdíl jednotek do výstupu.
- **Stav v kódu:** blok `radky_cil` v [`25`](../../R/02_druhy/25_n2k_druhy_uzemi.R)
  vytvářel řádky přes `transmute()`, který **nevytvářel sloupec `ROK`**. Závěrečný
  filtr téže funkce `filter(is.na(ROK) == FALSE & ROK != "NA")` je proto po
  `bind_rows()` beze zbytku zahodil. Totéž by později udělal filtr
  `CILMON_CHU == 1` v `chu_export()` ([`27`](../../R/02_druhy/27_n2k_druhy_zapis.R)).
- **Důsledek:** druhý indikátor Tabulky 2 správně ovlivňoval `CELKOVE`, ale ve
  výstupu po něm nezůstala **žádná stopa** — z exportu nebylo poznat, proč bylo
  území sraženo na „zhoršený" či „špatný". Podmínka u H-06 tím nebyla splněna.
- **Doklad o dopadu:** před opravou obsahoval výstup `chu` pouze
  `CELKOVE_HODNOCENI`, `LOK_PROCDOBR`, `LOK_DILCDOBRE`, `LOK_DILCPOCET`.
  Po opravě navíc **97 řádků `POP_POCETPRUM3`** (62 s cílovým stavem → 11 „dobrý"
  / 51 „špatný"; 35 bez cílového stavu → „nehodnocen"), z toho **63** se dostane
  až do exportu pro ISOP — stejný počet území jako u ostatních indikátorů.
- **Kontrola neregrese:** blok je ohraničen podmínkou `!is.null(cil_chu)`, tj. běží
  jen tam, kde volající předá cílové stavy i řadu početností — dnes výhradně větev
  obojživelníků. Pro ostatní skupiny je `radky_cil` `NULL` a `bind_rows()` jej ignoruje.
- **Zbývá:** `POP_POCETPRUM3` **nemá řádek v `cis_indikatory_popis.csv`**, takže se
  do exportu propisuje surový název `POP_POCETPRUM3` místo kódu ISOP. Před opravou
  bylo toto skryté, protože řádek do exportu vůbec nedošel. Viz §Co zbývá, položka 8.

### H-23 ⚠ — oba indikátory Tabulky 2 pracují s jiným časovým oknem
- **Závažnost:** střední · **Typ:** GAP · **Stav:** **neřešeno**, pouze zaznamenáno
- **Kontext:** `LOK_PROCDOBR` staví na jedné reprezentativní návštěvě každé DP
  (výběr v [`24`](../../R/02_druhy/24_n2k_druhy_lokality.R), libovolný rok
  2013–2026), zatímco `POP_POCETPRUM3` průměruje **poslední 3 monitorované roky**
  daného území ([`27:214`](../../R/02_druhy/27_n2k_druhy_zapis.R#L214),
  `slice_max(ROK, n = 3)` bez ukotvení na `current_year`).
- **Zjištěno v testovacím běhu:** okna se rozcházejí u 4 ze 103 EVL; u 6 EVL končí
  okno početnosti před rokem 2023. Krajní případ **CZ0523003**: DP hodnocena podle
  roku 2025, početnost průměrována z let **2014–2016**. Dále **29 z 97** území
  průměruje z méně než tří let (15 z jediného roku) a `POP_POCETPRUM3_LET` se
  nikam neexportuje, takže to není z výstupu poznat.
- **Otevřená otázka pro autory metodiky:** znamená „klouzavý průměr za poslední
  3 roky" tři poslední **kalendářní** roky hodnoceného období, nebo tři poslední
  roky **s monitoringem**? A jak se má hodnotit území, kde jsou k dispozici méně
  než tři roky?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Nálezy z revize převodu kategorií početnosti (2026-08-30, *Triturus cristatus*)

Podnět: kontrola, zda se relativní početnost `REL_POC` převádí na medián
kategorie podle číselníku `Data/Input/cis_pocet_kat.csv`.

### H-24 ✅ — žebříčky `POP_POCETMIN` a `POP_POCETMAX` se rozcházely s číselníkem
- **Závažnost:** vysoká · **Typ:** BUG · **Stav:** implementováno
- **Číselník:** `cis_pocet_kat.csv` definuje pro každou kategorii početnosti
  dolní mez (`POP_POCETNMIN`), medián (`POP_POCETSTRED`) a horní mez
  (`POP_POCETNMAX`).
- **Stav v kódu:** [`21_1`](../../R/02_druhy/21_1_n2k_druhy_akce.R) měl obě meze
  **natvrdo vypsané** v `case_when` a číselník se na jejich výpočtu nepodílel.
  Rozcházely se ve dvou bodech:

  | kategorie | `POP_POCETMIN` před → po | `POP_POCETMAX` před → po |
  |---|---|---|
  | 1 | 1 → 1 | **10000 → 10** |
  | 2 | 11 → 11 | **10000 → 50** |
  | 3 | **50 → 51** | **10000 → 100** |
  | 4–5 | beze změny | beze změny |
  | 6–8 | beze změny | **NA → 100000 / 1000000 / 1000000** |

- **Důsledek:** `POP_POCETMIN` vstupuje přes `POP_POCETFIN` do `POP_POCET`,
  tedy i do `POP_POCETPRUM3` (druhý indikátor Tabulky 2). `POP_POCETMAX`
  vstupuje do celého trendového bloku — u záznamů bez číselného počtu se
  regrese počítala z konstanty 10000, takže kategorie 1, 2 a 3 byly z hlediska
  trendu nerozlišitelné.
- **Doklad o dopadu** (testovací běh, 8 723 řádků fáze 1):
  `POP_POCETMIN` 10 změn, `POP_POCETMAX` **182**, `POP_POCET` 10,
  `POP_TRENDLM` 193. `POP_POCETPRUM3` se změnil u **3 z 97** území
  (CZ0723423 50,0 → 51,0; CZ0813444 46,3 → 46,7; CZ0813455 29,7 → 30,0).
  **Celkový stav DP i EVL zůstal beze změny** (334/16/374 a 5/21/37).
- **Provedeno:** obě meze se čtou z číselníku přes
  `match(POP_POCETNOSTNAL, cis_pocet_kat$POP_POCETNOSTMAX)`. Kategorie `0`
  (nepřítomnost) a `NA` dávají dál `NA`, tedy shodně s původní větví
  `TRUE ~ NA_real_`. Na začátku `run_n2k_druhy()` přibyla kontrola, že
  číselník existuje a má očekávané sloupce.
- **Semantika ZŮSTÁVÁ:** dosazuje se **dolní mez** kategorie, nikoli medián.
  Přechod na `POP_POCETSTRED` je metodické rozhodnutí, ne oprava chyby —
  viz H-26.
- **Kontrola neregrese:** větev je sdílená. Kategorie 6–8 dřív u
  `POP_POCETMAX` propadaly na `NA`, nově vracejí hodnotu — obojživelníků se
  to netýká (tak vysoké kategorie u nich nejsou), ale skupin s velkými počty
  (rostliny, hmyz) ano. **Ověřeno pouze pro obojživelníky.**
- **Rozhodnutí zadavatele:** _(vyplní zadavatel — potvrzení dopadu mimo obojživelníky)_

### H-25 ✅ — trendový blok se počítal i pro druhy bez limitu `POP_TREND*`
- **Závažnost:** střední · **Typ:** BUG / ÚKLID · **Stav:** implementováno
- **Metodika:** metodika obojživelníků žádný populační trend nezná; v
  `limity_vse.csv` nemá žádný z druhů Přílohy 1 řádek `POP_TREND*`.
  Trendové limity existují jen v `limity_cevky.csv` — indikátor `POP_TREND`
  (`max 1`, `KLIC = ano`, `UROVEN = lok`) u **34 druhů cévnatých rostlin**.
- **Stav v kódu:** blok `n2k_druhy_lokpop_trend_desc` v
  [`21_1`](../../R/02_druhy/21_1_n2k_druhy_akce.R) počítal
  `POP_POCETMAXREF`, `POP_TREND1`, `POP_TREND2`, `POP_TREND` a `POP_TRENDLM`
  **pro každý druh**. U obojživelníků hodnoty prošly celou fází 1 a teprve ve
  fázi 2 je zahodil `right_join` na limity.
- **Důsledek:** práce navíc bez vlivu na výsledek — a hlavně zavádějící údaj.
  `POP_TRENDLM` se u záznamů bez číselného počtu počítal z dosazených mezí
  kategorie (před H-24 dokonce z konstanty 10000), takže číslo vypadalo jako
  platný populační trend, ač jím nebylo.
- **Provedeno:** blok je podmíněn příznakem `pocitat_trend`, odvozeným
  **z tabulky limitů**, ne ze skupiny druhů — přibude-li trendový limit další
  skupině, začne se počítat sám od sebe. Sloupce zůstávají v tabulce
  (prázdné), protože fáze 2 pivotuje pevný rozsah sloupců a změna šířky by
  rozhodila `ncol_orig`.
- **Kontrola neregrese:** u 34 druhů cévnatých rostlin s limitem `POP_TREND`
  se blok počítá dál, beze změny. Pro ostatní skupiny byly hodnoty stejně
  zahazovány ve fázi 2.

### H-26 ⚠ — dolní mez kategorie místo mediánu (`POP_POCETSTRED`)
- **Závažnost:** střední · **Typ:** GAP · **Stav:** **neřešeno**, pouze zaznamenáno
- **Kontext:** nemá-li záznam číselný `POCET`, dosadí se za `POP_POCET`
  **dolní mez** kategorie (kat. 2 „11-100" → 11). Číselník přitom nese i
  sloupec `POP_POCETSTRED` s mediánem (kat. 2 → 25), který se **načte,
  přes `POP_POCETNOSTMAX` připojí a nikde nepoužije** — nemá řádek v
  `limity_vse.csv`, takže ho fáze 2 zahodí.
- **Rozsah:** v testovacím běhu má **3 414 z 8 723** řádků fáze 1 `POP_POCET`
  odvozený z kategorie, ne ze spočítaného čísla (z toho 3 204 je nepřítomnost).
  Přechod na medián by změnil `POP_POCETPRUM3` u **15 z 97** území
  (např. CZ0513244 1,0 → 5,0), **ale stav ani jednoho z 62 území s cílovým
  stavem by se nezměnil** (`STAV_CIL` splněn 11× v obou variantách).
- **Proč to není jen oprava:** `POP_POCETPRUM3` se porovnává s
  `navrzena_hodnota` ze SDO, u níž je nesoulad jednotek vědomě tolerován
  (nález S-4). Dolní mez je konzervativní odhad; medián je méně konzervativní
  proti cíli, jehož jednotky zatím nejsou vyjasněné.
- **Otázka pro autory metodiky:** má se za relativní kategorii dosazovat dolní
  mez (konzervativně), nebo medián kategorie?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_


# Nálezy z kontroly exportu proti importní šabloně (2026-08-31)

Podnět: srovnání závěrečné kompilace úrovní DP a EVL s
`Data/Templates/import_vzor_obojzivelnici.csv` — názvy sloupců, struktura,
kódování, oddělovače.

**Struktura je v pořádku.** Export `chu` (UTF-8) má všech 18 sloupců šablony
ve shodném pořadí a se shodnými názvy, oddělovač `;`, bez uvozovek, kódování
UTF-8, desetinná tečka, datum `YYYY-MM-DD`. Ověřeno, že **žádná hodnota
neobsahuje `;`**, takže neuvozovaný zápis je bezpečný — drží to konfigurace,
která u `oop` nahrazuje `;` čárkou. Dvě odchylky proti šabloně i proti
souboru, který ISOP přijal (`amp_evl_2024_20250908`): exporty mají **konce
řádků LF** místo CRLF a jsou **gzipované** (`.csv.gz`) místo prostého `.csv`.
Obojí je důsledek přechodu na `write_export_gz()`; neověřeno proti importu ISOP.

**Úroveň DP (`lok`) šablonu nemá** — má 26 sloupců interních názvů
(`ROK`, `KOD_LOKAL`, `ID_IND`, `HOD_IND`, `STAV_IND`, …) a není importním
formátem ISOP. Jako pracovní/auditní výstup je konzistentní.

### H-27 ✅ — `feature_code` se do exportu zapisoval jako `NA`
- **Závažnost:** vysoká · **Typ:** BUG / STOPA-DO-ISOP · **Stav:** implementováno
- **Stav v kódu:** [`27`](../../R/02_druhy/27_n2k_druhy_zapis.R) měl
  `dplyr::mutate(… feature_code = NA …)`, přestože `chu_export()` seznam
  předmětů ochrany už připojoval — jen z něj bral pouze `site_code`
  a `nazev_lat`.
- **Doklad:** šablona i soubor přijatý ISOP nesou kód druhu podle SDF:

  | druh | šablona | `sites_subjects$sdf_code` |
  |---|---|---|
  | *Triturus cristatus* | 1166 | 1166 |
  | *Bombina bombina* | 1188 | 1188 |
  | *Bombina variegata* | 1193 | 1193 |
  | *Triturus carnifex* | 1167 | 1167 |
  | *Triturus dobrogicus* | 1993 | 1993 |

- **Provedeno:** `feature_code` se bere z `sites_subjects$sdf_code`.
  **POZOR:** nikoli ze sloupce `sites_subjects$feature_code` — ten nese
  `Kód.ISOP` (pro *Triturus cristatus* hodnotu 21), tedy jiný číselník;
  jeho použití by do importu poslalo špatný kód.

### H-28 ✅ — `metodika` byla natvrdo 15087 pro všechny druhy
- **Závažnost:** vysoká · **Typ:** BUG / STOPA-DO-ISOP · **Stav:** implementováno
- **Stav v kódu:** [`27`](../../R/02_druhy/27_n2k_druhy_zapis.R) zapisoval
  `metodika = 15087` všem druhům. `Data/Input/cis_metodika.csv` přitom přiřazení
  druh → metodika obsahuje a config ho načítá — objekt `cis_metodika` se ale
  **nikde v kódu nepoužíval** (stejný vzorec jako H-16 a H-24).
- **Doklad:** šablona i soubor přijatý ISOP mají u obojživelníků `19269`,
  export `15087` — tedy cizí metodika u každého exportovaného řádku.
- **Rozhodnutí zadavatele (2026-08-31):** kód metodiky pro obojživelníky je
  **22257** (ne 19269 ze šablony — ta pochází z běhu 2025). Hodnota zapsána do
  `cis_metodika.csv` u všech 7 řádků skupiny *Obojživelníci*.
- **Provedeno:** `metodika` se připojuje z `cis_metodika` podle druhu.
- **Kontrola neregrese:** číselník má metodiku vyplněnou **jen** u
  obojživelníků (22257) a cévnatých rostlin (19192); zbylých 12 skupin
  (ryby, brouci, motýli, letouni, savci, měkkýši, mechorosty, vážky …) má
  sloupec prázdný. Proto `dplyr::coalesce(metodika_cis, METODIKA_VYCHOZI)`
  se zálohou `METODIKA_VYCHOZI = 15087` — bez ní by těmto skupinám metodika
  zmizela. *Epidalea calamita* v číselníku není, zůstává tedy na 15087,
  v souladu s jejím vyřazením z rozsahu harmonizace.

### H-29 ⚠ — `trend` je natvrdo „neznámý"
- **Závažnost:** střední · **Typ:** GAP · **Stav:** **neřešeno**, pouze zaznamenáno
- **Kontext:** [`27`](../../R/02_druhy/27_n2k_druhy_zapis.R) zapisuje všem
  řádkům `trend = "neznámý"`. Šablona i soubor přijatý ISOP obsahují všechny
  čtyři hodnoty (`setrvalý`, `zlepšující se`, `zhoršující se`, `neznámý`).
- **Otázka:** vyplňují trend hodnotitelé až v ISOP? Pokud ano, import
  s natvrdo „neznámý" jim dříve zapsanou hodnotu přepíše.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Dořešení H-01 a H-02 (2026-08-31)

Rozhodnutí z 2026-08-20 (odstranit limity `STA_VYSYCHANI`, ponechat jej jako
informativní hodnotu) bylo implementováno jen zčásti a při kontrole vyšly
najevo dvě navazující věci. Zadavatel je rozhodl 2026-08-31.

### H-30 ✅ — práh vysychání 25 % neměl oporu v metodice
- **Závažnost:** vysoká · **Typ:** ZMĚNA PRAVIDLA · **Stav:** implementováno
- **Metodika:** §Sledované indikátory, *Stav vody*: „0 % odpovídá zcela
  vyschlé ploše."
- **Stav v kódu:** `STA_VYSYCHANI` se odvozoval prahem
  `STA_STAVVODAPROC <= 25`. Hodnota 25 byla převzatá ze starého pásma
  „1-25 %", které původní převod považoval za vysychání (viz H-03), nikoli
  z věty metodiky. Ze 331 příznaků „vysychá" jich 248 pocházelo z pásma
  1-25 %, jen 83 ze skutečné nuly.
- **Rozhodnutí zadavatele (2026-08-31):** rovnat se doslovnému znění, tedy
  práh **0 %**.
- **Provedeno:** zavedena pojmenovaná konstanta `PRAH_VYSYCHANI = 0`
  v [`21_1`](../../R/02_druhy/21_1_n2k_druhy_akce.R); práh už není zapsán
  natvrdo v `case_when`, takže jde příště změnit na jednom místě.
- **Doklad o dopadu** (testovací běh *Triturus cristatus*):

  | `STA_VYSYCHANIPERIOD3` | před | po |
  |---|---|---|
  | 0 | 422 | 486 |
  | 1 | 62 | 28 |
  | 2 | 26 | 5 |
  | **3 (nesplněno)** | **11** | **2** |
  | neznámý | 203 | 203 |

  Na úrovni DP se změnily **2 z 724** ploch, obě zhoršený → dobrý
  (CZ0323147 `PERIOD3` 3 → 0; CZ0613322 / amp291 3 → 1). U dalších **77 DP**
  se `PERIOD3` změnil, ale verdikt ne — padaly už na jiném indikátoru.
  Na úrovni EVL: špatný 37 → 36, zhoršený 21 → 22.

### H-31 ✅ — informativní řádek se na úroveň DP nedostal
- **Závažnost:** střední · **Typ:** BUG / STOPA-DO-ISOP · **Stav:** implementováno
- **Kontext:** rozhodnutí u H-02 znělo „ponechat jako informativní hodnotu bez
  `LIM_IND`, **obdobně jako `LOK_DILCDOBRE`**". Ta analogie ale na úrovni
  dílčí plochy neplatí.
- **Stav v kódu:** [`21_2`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R)
  filtroval `is.na(LIM_IND) == FALSE` na **dvou** místech — jednou při
  sestavení `ind_cols_keep` (sloupec se tím ztratil z celé široké tabulky)
  a podruhé v `right_join` na limity. Naproti tomu
  [`25`](../../R/02_druhy/25_n2k_druhy_uzemi.R) filtruje jen podle
  `UROVEN == "chu"`, bez podmínky na limit — proto `LOK_DILCDOBRE`
  s prázdným limitem ve výstupu EVL je, ale `STA_VYSYCHANI` ve výstupu DP
  nebyl **ani jednou** (ověřeno: 12 indikátorů × 724 DP, `STA_VYSYCHANI`
  mezi nimi chyběl).
- **Důsledek:** hodnotitel viděl verdikt `STA_VYSYCHANIPERIOD3` = 3 →
  „špatný", ale neměl jak zjistit, které roky byly suché. Stejná třída jako
  H-22.
- **Provedeno:** zaveden marker `TYP_IND = "info"` pro řádky bez limitu, které
  se mají propsat do výstupu. `21_2` je na obou místech propouští; výpočtu
  `STAV_IND` nesedne žádná větev, takže zůstává `NA`, a `24` je do
  `N_KEY_EXPECTED` ani `N_OTH_EXPECTED` nezapočítá, protože ty berou jen
  řádky s vyplněným `LIM_IND`. Do `limity_vse.csv` přidáno 6 řádků
  (`<DRUH>,STA_VYSYCHANI,info,NA,NA,ne,lok`) pro druhy Přílohy 1.
- **Doklad:** výstup DP nově obsahuje 724 řádků `STA_VYSYCHANI`
  (489× „0", 14× „1", 221× „neznámý"), vždy se `STAV_IND = NA`. Počty všech
  ostatních dvanácti indikátorů zůstaly na 724, celkové hodnocení DP se
  změnou nedotčeno — řešení je tedy prokazatelně neutrální vůči verdiktu.
- **Řešení je obecné:** stejným markerem lze zviditelnit i další podkladové
  indikátory (např. `STA_STAVVODAPROC`), aniž by vstoupily do hodnocení.

### H-32 ⚠ — pásmo „0-25 %" se při prahu 0 % nepozná jako vysychání
- **Závažnost:** střední · **Typ:** GAP · **Stav:** **neřešeno**, pouze zaznamenáno
- **Kontext:** `norm_stavvody()` bere u procentního pásma **horní mez**
  (zdokumentováno u H-03, aby zůstalo zachováno původní chování). Při prahu
  0 % z toho plyne, že záznam **„0-25 %" dá 25 a za vysychání se nepovažuje**,
  přestože jeho dolní konec je nula. Týká se **84 záznamů** testovacího běhu.
- **Tvary, které práh 0 % zachytí:** `vyschlé` (58), holá `0` (24),
  `zaniklá` (1). Naopak nezachytí `1-25 %` (130), `0-25 %` (84) a holá čísla
  5-20 (34).
- **Otázka pro autory metodiky:** má se pásmo číst horní mezí (pak je stav
  správný), nebo má pásmo obsahující nulu platit za vysychání (pak je nutné
  upravit i `norm_stavvody()`)?
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### *Epidalea calamita* — limity `STA_VYSYCHANI` ponechány
Původní vadné limity H-01 (`val 26-50 %`, `51-75 %`, `76-100 %`, `76-90 %`,
`91-100 %`, `min 25` proti doméně 0/1) u tohoto druhu **zůstávají**.
Rozhodnutí zadavatele 2026-08-31: vyřešit až v samostatné harmonizaci
*Epidalea calamita* (položka 7 v §Co zbývá). Do té doby dostává každá DP
tohoto druhu se záznamem stavu vody jeden zaručeně nesplněný stanovištní
indikátor.

---

# Rozšíření kategorie `info` (2026-08-31)

Zadavatel: *„info kategorie pro limity, které se mají zobrazit, ale nemají
přispívat k hodnocení — zavést i pro ostatní, kde min/max/val neplatí."*

Při procházení se ukázalo, že „`min`/`max`/`val` neplatí" pokrývá **tři
různé věci**, ne jednu — proto H-33 (skutečné `info`), H-34 (`neg`, jiný
problém) a H-35 (chyba, kterou to odhalilo).

### H-33 ✅ — kategorie `info` rozšířena na ostatní nehodnocené indikátory
- **Závažnost:** střední · **Typ:** STOPA-DO-ISOP · **Stav:** implementováno
- **Provedeno:** v `limity_vse.csv` označeno `TYP_IND = "info"` u **64 řádků**
  na úrovni `lok` u 23 druhů (7 obojživelníků + 16 druhů hmyzu):

  | indikátor | řádků | co to je |
  |---|---|---|
  | `POP_POCET` | 18 | výčet jednotek pro `lim_pocet` |
  | `POP_POCETSUM` | 8 | výčet jednotek pro `lim_pocetsum` |
  | `POP_POCETMIN` | 7 | placeholder bez limitu |
  | `POP_POCETMAX` | 7 | placeholder bez limitu |
  | `VLV_VLIVY` | 24 | placeholder bez limitu |

- **Doklad o dopadu** (testovací běh *Triturus cristatus*): výstup DP má nově
  16 indikátorů místo 13 — přibyly `POP_POCET`, `POP_POCETSUM` a `VLV_VLIVY`,
  vždy 724 řádků se `STAV_IND = NA`.
  **Celkové hodnocení DP se nezměnilo** (336 / 14 / 374 před i po) a
  `CELKOVE_SUM` se nezměnil u ani jedné ze 724 ploch — řešení je tedy
  prokazatelně neutrální vůči verdiktu.
- **`POP_POCET` je z nich nejcennější:** je to surový počet a zároveň vstup
  do `POP_POCETPRUM3` (druhý indikátor Tabulky 2). Dosud se do výstupu DP
  nedostal vůbec.
- **Známé omezení:** u indikátorů s více jednotkami (`POP_POCET`:
  `adulti` / `jedinci`, `POP_POCETSUM`: `samci` / `samice`) nechá fáze 2 po
  `slice(1)` jeden řádek, takže `HOD_IND` je správně, ale `JEDNOTKA` je jedna
  ze dvou. Pro informativní řádek přijatelné.

**Vědomě neoznačeno:**

| co | proč |
|---|---|
| 17 řádků `DRUH = "stanoviste"` (`ROZLOHA`, `KVALITA`, `MINIMIAREAL`, `MOZAIKA_FIN`, `TYPICKE_DRUHY`, `MRTVE_DREVO`, `RED_LIST`, `INVASIVE` …) | pseudodruh, do druhové kaskády se nikdy nedostane (`species_list` je průnik s daty NDOP); patří do `R/01_stanoviste`, dopad neověřen |
| 34 řádků na úrovni `chu` (`LOK_DILCDOBRE`, `LOK_DILCPOCET`, `LOK_POCETDOBR`, `POP_POCETPOLE0/1`) | ve výstupu EVL **už jsou** a hlásí se jako „nehodnocen“, protože `25` filtruje jen podle `UROVEN == "chu"`; přeznačení by bylo kosmetické a u `LOK_PROCDOBR` by míchalo `info` řádky s reálným `min 70` do téhož `slice(1)` |
| `limity_cevky.csv`, `limity_ryby.csv` | stejná změna by dávala smysl, ale týká se rostlin a ryb, kde nebyl změřen dopad — **doplněno 2026-09-03**, viz položka 15 a commit `8fc5f1c` |
| `POP_REPRO` (53 řádků) | **přehlédnuto** — má tentýž tvar jako `POP_POCET`, viz **H-37** |

### H-34 ✅ — `TYP_IND = "neg"` se nikdy nevyhodnotí
- **Závažnost:** vysoká · **Typ:** BUG · **Stav:** **vyřešeno 2026-09-03** — převedeno na `val`, viz níže
- **Kontext:** `limity_ryby.csv` používá **čtvrtý** typ limitu `neg`, který
  se v `limity_vse.csv` ani `limity_cevky.csv` nevyskytuje — 12 řádků,
  6 druhů ryb, indikátory `STA_TRASATOKU` (`uměle napřímený`) a
  `STA_VARIABILITAHLOUBEK` (`antropogenní nízká`).
- **Stav v kódu:** výpočet `STAV_IND` v
  [`21_2`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R) i v
  [`25`](../../R/02_druhy/25_n2k_druhy_uzemi.R) zná jen větve `min`, `max`
  a `val`. Pro `neg` nesedne žádná, takže `STAV_IND` zůstane `NA`.
  `IND_GRP` se navíc nastaví na `"neg"`, na který nesedne ani agregace.
- **Důsledek:** ~~oba indikátory mají vyplněný limit, jsou tedy započítány do
  `N_OTH_EXPECTED`, ale nikdy nemohou být splněny~~ — **oprava 2026-09-03:**
  tato část byla nepřesná už v době zápisu. Po H-21 vyžadují oba čítače
  v [`24`](../../R/02_druhy/24_n2k_druhy_lokality.R#L127-L128) navíc
  `!is.na(STAV_IND)`, takže řádek `neg` do `N_OTH_EXPECTED` **nevstupuje**
  a nic nepenalizuje. Zbývající důsledek je tedy tichá nevyhodnocenost, ne
  trvalé selhání — mírnější, než registr uváděl.
- **Význam `neg` je zřejmý z hodnot** („uměle napřímený", „antropogenní
  nízká"): „shoda s touto hodnotou = nepříznivý stav", tedy opak `val`.
- **Rozhodnutí zadavatele (2026-09-03):** ✅ **převést na úplnou `val` logiku.**
- **Provedeno:** 12 řádků `neg` nahrazeno 48 řádky `val` s výčtem **příznivých**
  hodnot, tj. doplňkem k původní nepříznivé hodnotě. Retězce jsou převzaty
  z **domény v ostrých datech**, ne z původních limitů — viz H-38, kde je
  doloženo, že původní znění („uměle napřímený", „antropogenní nízká")
  neodpovídalo ani jedné skutečné hodnotě, takže by po prostém převodu na
  `val` vyšly **všechny** záznamy jako nepříznivé.
- **Pozor na přirozené protějšky:** doména obsahuje u obou indikátorů
  přírodní obdobu nepříznivé hodnoty — `Přirozeně přímý tok` (436×) a
  `Přirozeně nízká` (632×). Zápis přes `neg` je pokrýval automaticky, výčet
  přes `val` je musí uvádět výslovně; jejich vynechání by chybně penalizovalo
  druhou nejčastější hodnotu indikátoru.
- **Známá nevýhoda převodu:** `neg` byl vůči rozšíření domény odolný, výčet
  přes `val` není — nová kategorie v Survey123 se stane nepříznivou, aniž by
  to bylo vidět. Stejná třída rizika jako H-01 a H-03; **při každé změně
  číselníku formuláře je nutné výčet zkontrolovat.**
- **Dopad dnes nulový** — ani jeden z obou indikátorů nemá v kódu výpočet
  (viz H-38), takže `21_2` řádky odfiltruje jako sirotčí limit bez nálezu.
  Až se tagy zavedou, začnou oba vstupovat do `N_OTH_EXPECTED` u šesti
  dotčených druhů a mohou měnit verdikt DP.
- **Mimo rozsah harmonizace obojživelníků**, zaznamenáno pro úplnost.

### H-35 ✅ — `POP_POCETMIN` a `POP_POCETMAX` byly neviditelné kvůli příponám `.x` / `.y`
- **Závažnost:** vysoká · **Typ:** BUG · **Stav:** implementováno
- **Kontext:** odhaleno až rozšířením `info` v H-33 — po označení se oba
  indikátory ve výstupu **stále neobjevily**.
- **Stav v kódu:** `n2k_druhy_pre` (úroveň nálezu) i `n2k_druhy_lokpop`
  (agregace za DP a rok) obsahují sloupce `POP_POCETMIN`, `POP_POCETMAX`
  a `POP_POCETNOSTMAX`. Jejich `left_join` v
  [`21_1`](../../R/02_druhy/21_1_n2k_druhy_akce.R) je proto rozdvojil na
  `.x` a `.y` a sloupec s **přesným názvem indikátoru v tabulce vůbec
  neexistoval**.
- **Důsledek:** `21_2` páruje indikátory přes
  `intersect(názvy sloupců, ID_IND limitů)`, takže `POP_POCETMIN` ani
  `POP_POCETMAX` se nespárovaly **nikdy** — byly neviditelné ve všech
  výstupech, přestože mají řádek v limitech. Navíc součet
  `sum(ID_IND == "POP_POCETMIN")` na úrovni území v `25` vracel **vždy 0**,
  protože takové `ID_IND` nikdy nevzniklo. Stejná třída jako H-22.
- **Provedeno:** join dostal `suffix = c("_NAL", "")`. Indikátorem je hodnota
  za dílčí plochu a rok (limity mají `UROVEN = lok`), tedy strana `y`, která
  si nechává holý název; hodnota za jednotlivý nález zůstává zachována pod
  příponou `_NAL`.
- **Kontrola neregrese:** `POP_POCETMIN` ani `POP_POCETMAX` nemají **v žádném
  ze tří souborů limitů** vyhodnotitelný limit (7 + 7 řádků, všechny nově
  `info`, `LIM_IND` prázdný). Oprava proto nemůže nic nově *hodnotit* —
  pouze zviditelňuje. Totéž platí pro `POP_POCETNOSTMAX`, který join rozdvojil
  také a který nemá limit vůbec žádný.
- **Doklad o dopadu:** výstup DP má po opravě **18 indikátorů**
  (13 před dnešními změnami + 3 z H-33 + 2 zde), oba nové vždy 724 řádků
  se `STAV_IND = NA`. Celkové hodnocení DP zůstalo 336 / 14 / 374.

### H-36 ⚠ — `POP_POCETMIN` vracelo `Inf`, `POP_POCETMAX` vrací zavádějící `0`
- **Závažnost:** střední · **Typ:** BUG (částečně opraveno) · **Stav:** minimum opraveno, maximum **k rozhodnutí**
- **Kontext:** odhaleno až opravou H-35 — po zviditelnění obou indikátorů se
  ukázalo, co vlastně obsahují. `min(POP_POCET, na.rm = TRUE)` nad samými `NA`
  vrací `Inf`; u maxima ošetření `Inf → 0` existovalo, u minima **chybělo**.
- **Doklad:** ve výstupu DP testovacího běhu **73 ze 724 řádků**
  `POP_POCETMIN` neslo hodnotu `Inf`.
- **Provedeno:** u minima doplněno `Inf → NA`. Záměrně **ne** na 0 — nula by
  tvrdila „napočítáno nula jedinců", zatímco skutečnost je „počet nebyl
  zaznamenán". Po opravě je ve výstupu 73× „neznámý" a `Inf` se nevyskytuje
  nikde.
- **Zůstává k rozhodnutí:** `POP_POCETMAX` převádí `Inf` na **0** u
  **426 ze 724** řádků, tedy tvrdí „nula jedinců" tam, kde počet nebyl
  zaznamenán — stejná vada. Neopraveno, protože `POP_POCETMAX` vstupuje do
  `POP_TRENDLM` a `POP_TREND1`/`POP_TREND2` u **34 druhů cévnatých rostlin**
  (jediná skupina s limitem `POP_TREND`), kde by změna nebyla neutrální
  a nebyla změřena. Asymetrie je zdokumentována přímo v kódu.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel — má se `Inf → 0` u maxima
  změnit na `NA` i za cenu zásahu do trendů cévnatých rostlin?)_

---

# Dokončení kategorie `info` a revize limitů ryb (2026-09-03)

Zadavatel: *„info commit bez další změny; `neg` projít tak, aby byla dosažena
úplná `val` logika, pak commitnout."* Rozhodnutí padlo nad necommitnutými
změnami v pracovní kopii (`limity_cevky.csv`, `limity_ryby.csv`).

Provedeno ve třech commitech, jeden krok = jeden commit:

| Commit | Nálezy | Soubory |
|---|---|---|
| `8fc5f1c` | položka 15 (`info` pro cévky a ryby) | `limity_cevky.csv`, `limity_ryby.csv` |
| `7d811e4` | **H-34** (`neg` → `val`) | `limity_ryby.csv` |
| `7865d31` | **H-37** (`POP_REPRO` → `info`) | `limity_vse.csv` |

**Položka 15 — `info` pro cévky a ryby.** 39 řádků v `limity_cevky.csv`
(`POP_POCETSUM` 22, `POP_POCETVITAL` 13, `POP_POCETSUMLOD` 2, `POP_VITAL` 2)
a 2 řádky v `limity_ryby.csv` (`Salmo salar` — `POP_DYN`, `POP_VITALITA`).
Bajtově ověřeno, že všech 41 změn se týká **výhradně** sloupce `TYP_IND`
(`""`/`NA` → `info`); žádný řádek nezměnil limit, jednotku, `KLIC` ani
`UROVEN`. Neutralita: všech 41 řádků má prázdný `LIM_IND`, takže do čítačů
v `24` nevstupují, a `LIM_INDLIST` se nemění, protože `00_n2k_config.R`
mapuje na text jen `min`/`max`/`val` a následný `toString()` přes `na.omit()`
řádek s `NA` zahodí. **Dopad nezměřen** (týká se rostlin a ryb, mimo
testovací běh obojživelníků) — na rozdíl od H-37 níže.

### H-37 ✅ — `POP_REPRO` zůstal mimo rozšíření kategorie `info`
- **Závažnost:** vysoká · **Typ:** STOPA-DO-ISOP · **Stav:** implementováno
- **Kontext:** H-33 označilo `info` u výčtů jednotek `POP_POCET` (18 řádků) a
  `POP_POCETSUM` (8 řádků), ale **`POP_REPRO` téhož tvaru přehlédlo** —
  53 řádků (`TYP_IND = val`, prázdný `LIM_IND`), z toho 44 u šesti druhů
  metodiky. Je to výčet jednotek dokládajících reprodukci: `larvy`,
  `juvenilové`, `snůšky`, `snůšky m2/m3/dm2/cm2`, `amplexus`, `metamorf. ex.`
- **Stav v kódu:** filtr v [`21_2`](../../R/02_druhy/21_2_n2k_druhy_akce_lim.R#L97)
  propouští řádek jen s vyplněným limitem **nebo** s `info`. `val` s prázdným
  `LIM_IND` neprojde ani do `ind_cols_keep`, ani do `right_join`.
- **Důsledek:** indikátor `POP_REPRO` **nebyl v žádném výstupu** — ověřeno
  během před zásahem: výstup DP měl 18 indikátorů a `POP_REPRO` mezi nimi
  nebyl. Přitom je to přímý vstup do `POP_REPROPERIOD3`, což je **klíčový**
  indikátor, jehož jediné selhání sráží DP rovnou na „špatný". V testovacím
  běhu je `POP_REPROPERIOD3 = 0` u **336 ze 724 DP**, tedy nejčastější jediná
  příčina verdiktu „špatný". Správce lokality viděl výsledek tříletého okna,
  ale ne roční záznamy, ze kterých plyne.
- **Souměrnost s vysycháním** — tentýž vzorec už je vyřešen na druhé straně:

  | per-roční (informativní) | tříleté okno (hodnocené) |
  |---|---|
  | `STA_VYSYCHANI` — `info`, bez `ind_id` (H-02, H-31, H-33) | `STA_VYSYCHANIPERIOD3` — `max 2`, `ind_id 34` |
  | `POP_REPRO` — **do 2026-09-03 `val` bez limitu** | `POP_REPROPERIOD3` — `min 1`, `ind_id 30` |

  Číselník `cis_indikatory_popis.csv` už `POP_REPRO` takto vede — řádek
  *„rozmnožování druhu"* existuje a `ind_id` záměrně nemá.
- **Provedeno:** 44× `val` → `info` u šesti druhů metodiky. *Epidalea
  calamita* (9 řádků) **ponechána** — viz §Limity bez normativního zdroje.
- **Neutralita je dvojitá:** řádky nemají `LIM_IND` **a navíc** mají
  `KLIC = NA`, takže nesplňují ani `KLIC == "ano"`, ani `KLIC == "ne"` ve
  filtrech `N_KEY_EXPECTED` / `N_OTH_EXPECTED`. `lim_repro` v `21_1` čte jen
  sloupec `JEDNOTKA`, detekce reprodukce se proto nemění.
- **Měřený dopad** (testovací běh *Triturus cristatus*, 724 DP ve 191 EVL,
  celá kaskáda `21_1` → `27` před zásahem i po něm):

  | Co | Před | Po |
  |---|---|---|
  | indikátorů ve výstupu DP | 18 | **19** |
  | `POP_REPRO` — řádků | 0 | **724** (`ano` 189 · `ne` 399 · `neznámý` 136) |
  | `STAV_IND` u `POP_REPRO` | — | vždy `NA` |
  | `CELKOVE_HODNOCENI` (DP) | 336 / 14 / 374 | **336 / 14 / 374** |
  | změněných verdiktů DP | — | **0** (z 724) |
  | změněných `CELKOVE_SUM` | — | **0** |
  | úroveň EVL | 38 / 29 / 36 / 88 | **beze změny** (`UROVEN = lok`, `25` filtruje `chu`) |

- **Kontrola konzistence s klíčovým indikátorem:** neexistuje DP, kde by
  `POP_REPRO = "ano"` a zároveň `POP_REPROPERIOD3 = 0`. Opačné dvojice
  (`ne`/`neznámý` u ročního záznamu, ale splněné tříleté okno — 102 DP) jsou
  v pořádku, okno zahrnuje i ostatní roky.
- **Známé omezení — `JEDNOTKA` je u tohoto řádku nevypovídající.** `POP_REPRO`
  má devět řádků limitu (jeden na jednotku), `right_join` je rozdvojí a
  následný `distinct()` ponechá první, takže `JEDNOTKA` vyjde u všech
  724 řádků `larvy` bez ohledu na to, čím byla reprodukce doložena.
  `HOD_IND` je správně. H-33 tentýž jev popisuje u `POP_POCET`, kde jsou
  jednotky dvě; **zde jich je devět, takže údaj může přímo svádět ke špatnému
  čtení** („reprodukce doložena larvami"). Oprava vyžaduje zásah do sdíleného
  kódu `21_2` nebo `24`, proto není součástí této změny.
- **Rozhodnutí zadavatele:** ⚠ _(má se `JEDNOTKA` u informativních řádků
  s více jednotkami vyprazdňovat?)_

### H-38 ⚠ — `limity_ryby.csv`: 19 z 26 indikátorů nemá v kódu žádný výpočet
- **Závažnost:** vysoká (mimo rozsah obojživelníků) · **Typ:** GAP · **Stav:** **zaznamenáno**
- **Kontext:** odhaleno při řešení H-34 — než šlo rozhodnout, co `neg` znamená,
  bylo nutné zjistit, co se s ním v kódu vůbec děje.
- **Zjištění:** ze **26 `ID_IND`** v `limity_ryby.csv` má v celém `R/` definici
  jen **7** (`LOK_PROCDOBR`, `POP_DYN`, `POP_POCET`, `POP_PRESENCE`,
  `POP_VITALITA`, `STA_MIGBARPOCET`, `STA_MIGBARVYS`). Zbylých **19 je
  sirotků** a pokrývají **180 z 254 řádků limitů**, mj. `STA_PROUD` (36),
  `STA_TRASATOKU` (31), `STA_VARIABILITAHLOUBEK` (23), `STA_DNO` (19),
  `STA_DNOTYP` (17).
- **Příčina — jiná konvence tagů.** Data ryb nesou strukturované poznámky pod
  **malými zkrácenými tagy**, ne pod názvy `ID_IND`. Ověřeno na 2 771
  záznamech 18 druhů ryb se `STRUKT_POZN`:

  | tag v datech | záznamů | odpovídá `ID_IND` |
  |---|---|---|
  | `<tr_tok_char>` | 2 338 | `STA_TRASATOKU` |
  | `<var_hl_pr>` | 2 338 | `STA_VARIABILITAHLOUBEK` |
  | `<breh_upr>` | 2 338 | `STA_UPRAVABREHU` |
  | `<upr_dno>` | 2 338 | `STA_UPRAVADNA` |
  | `<sub_dno>` | 2 338 | `STA_DNO` / `STA_DNOTYP` |
  | `<char_prou>` | 2 338 | `STA_PROUD` |
  | `<zahl_kor>` | 2 338 | `STA_ZAHLOUBENIKORYTA` |
  | `<veg_tok>` | 2 338 | `STA_VEGETACE` |

  Z celé této sady čte kód **jediný tag** — `<pocet_bar>`
  ([`21_1:771`](../../R/02_druhy/21_1_n2k_druhy_akce.R#L771)). Velkými písmeny
  se v datech ryb vyskytují jen `<STA_PRUHLEDNOSTVODA>` a `<VLV_VLIVY>`.
- **Důsledek:** `21_2` sirotčí limity odfiltruje (`filter(is.na(ID_ND_NALEZ) == FALSE)`),
  takže se do výstupu nedostanou vůbec a **hodnocení ryb stojí na 7
  indikátorech místo 26**, aniž by to bylo kdekoli vidět. Jde o tutéž tichou
  neúplnost jako H-04, ale v mnohem větším měřítku.
- **Dopad na P-06:** konstatování *„žádní sirotci"* platí **jen pro 6 druhů
  obojživelníků**, u nichž byla matice pokrytí ověřována. Pro ryby neplatí.
- **Mimo rozsah harmonizace obojživelníků.** Řešení je mapování tagů, ne
  úprava limitů — a patří autorům metodiky ryb spolu se správcem formuláře.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

### H-39 ⚠ — tři názvy druhů v `limity_ryby.csv` nemají v NDOP jediný záznam
- **Závažnost:** vysoká (mimo rozsah obojživelníků) · **Typ:** BUG · **Stav:** **zaznamenáno**
- **Stav v datech:** limity se napojují přes `DRUH`; tři názvy se neshodují
  s ničím v exportu:

  | název v limitech | řádků | co je v NDOP |
  |---|---|---|
  | `Cobitis elangotoides` | 10 | **překlep** — správně `Cobitis elongatoides` (211 záznamů) |
  | `Romanogobio albipinatus` | 19 | zastaralé jméno — NDOP vede `R. vladykovi` (250), `R. belingi` (19) |
  | `Romanogobio kessleri` | 13 | zastaralé jméno — NDOP vede `R. banaticus` (80) |

- **Důsledek u `Cobitis elongatoides` je nejzávažnější:** pod překlepem leží
  **oba klíčové indikátory** druhu — `POP_DYN` (`max 50`, `KLIC = ano`) a
  `POP_VITALITA` (`min 2`, `KLIC = ano`) — plus 8 dalších. Pod správným
  názvem zbývají jen `STA_MIGBARVYS` a `STA_MIGBARPOCET`, oba `KLIC = ne`.
- **Následek v hodnocení:** `N_KEY_EXPECTED = 0`, takže větev
  `N_KEY_EXPECTED > 0 & N_KEY_PASSED < N_KEY_EXPECTED ~ 0` v
  [`24`](../../R/02_druhy/24_n2k_druhy_lokality.R#L175) nemůže nikdy nastat a
  **DP tohoto druhu nelze vyhodnotit jako „špatnou"** — nejvýš „zhoršenou".
  Je to táž vada jako H-05, jen na úrovni DP a u ryb.
- **Rozsah:** ze **15 druhů ryb**, které v NDOP záznamy mají, nemá jediný
  vyhodnotitelný klíčový indikátor **3** — `Cobitis elongatoides` (překlep),
  `Romanogobio banaticus` (limity leží pod zastaralými jmény) a `Salmo salar`
  (`POP_DYN` i `POP_VITALITA` mají `KLIC = ano`, ale prázdný limit — právě ty
  dva řádky dostaly v položce 15 značku `info`; ta gap nezpůsobila, jen ji
  zviditelnila).
- **Neopraveno záměrně:** přejmenování druhu je věcný zásah do limitů ryb a
  u rodu *Romanogobio* navíc taxonomické rozhodnutí (jedno zastaralé jméno
  vs. dvě až tři dnešní) — patří autorům metodiky ryb.
- **Souvislost s H-34:** obě `val` sady rodu *Romanogobio* proto zůstaly
  nedotčeny, včetně `R. albipinatus;STA_TRASATOKU;val;uměle napřímený`, což
  je oproti `neg` u šesti druhů **obrácené znaménko** („dobrý stav = uměle
  napřímené koryto"), a výčtu `mírný / střední / vysoká`, který se s doménou
  dat nekryje.
- **Rozhodnutí zadavatele:** _(vyplní zadavatel)_

---

# Co zbývá

| # | Položka | Kdo |
|---|---|---|
| 1 | Potvrdit H-19, H-20 a H-23; u H-21 a H-24 potvrdit dopad mimo obojživelníky | zadavatel |
| 2 | Promítnout přesun `ind_id` 30 a 34 do ISOP (potvrzeno 2026-08-20) | zadavatel |
| 3 | Přidělit `ind_id` pro `STA_PLOCHA50CM` | ISOP |
| 4 | Zavést tag `STA_PLOCHA50CM` do Survey123 (hodnocení od 2027) | správce formuláře |
| 5 | Expertní revize cílových stavů — vyřešit jednotky u *Bombina bombina* (S-4) | autoři metodiky |
| 6 | Plný běh kaskády po doplnění `AktualizacniOkrsky.shp` | provoz |
| 7 | Samostatná harmonizace `Epidalea calamita` dle jejího vlastního dokumentu | zadavatel |
| 8 | ~~Přidělit `ind_id` pro `POP_POCETPRUM3`~~ — **hotovo 2026-08-30**, přiděleno `ind_id = 190`, řádek doplněn do `cis_indikatory_popis.csv` (`ind_nadr = 2` podle sesterského `LOK_PROCDOBR`, k potvrzení) | ISOP |
| 9 | Rozhodnout H-26 — dolní mez vs. medián kategorie početnosti | autoři metodiky |
| 10 | Rozhodnout H-29 — má import přepisovat `trend` hodnotou „neznámý"? | zadavatel |
| 11 | Ověřit proti importu ISOP konce řádků a kompresi. **Zjištěno 2026-09-03:** export je **LF**, UTF-8, `;`, bez uvozovek, **gzipovaný** (`.csv.gz`) — `write.table()` má výchozí `eol = "\n"` a připojení nepřekládá na CRLF. Otázka tedy zní, zda import LF a `.gz` přijme. | ISOP / provoz |
| 12 | Rozhodnout H-32 — má pásmo „0-25 %" platit za vysychání? | autoři metodiky |
| 13 | ~~Rozhodnout H-34 — význam `TYP_IND = "neg"` u ryb~~ — **hotovo 2026-09-03**, převedeno na úplnou `val` logiku. **Nově místo toho:** rozhodnout **H-38** (19 z 26 indikátorů ryb bez výpočtu — mapování malých tagů) a **H-39** (tři názvy druhů neodpovídají NDOP). Bez H-38 nemá `neg` ani `val` u těchto dvou indikátorů žádný efekt. | autoři metodiky ryb |
| 14 | Rozhodnout H-36 — má `POP_POCETMAX` vracet `NA` místo `0`, i za cenu zásahu do trendů rostlin? | zadavatel |
| 15 | ~~Zvážit `info` i pro `limity_cevky.csv` a `limity_ryby.csv`~~ — **hotovo 2026-09-03** (41 řádků, commit `8fc5f1c`); dopad na rostliny a ryby stále nezměřen | zadavatel |
| 16 | Rozhodnout H-37 — má se `JEDNOTKA` u informativních řádků s více jednotkami vyprazdňovat? Dnes `POP_REPRO` hlásí u všech 724 DP `larvy`. | zadavatel |
| 17 | Změřit dopad položky 15 na cévnaté rostliny a ryby (obdoba testovacího běhu u obojživelníků) | zadavatel / provoz |
