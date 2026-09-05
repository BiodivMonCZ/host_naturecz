# Prompt: harmonizace hodnocení obojživelníků s metodikou verze 2026

> **Určení.** Tento soubor je zadání pro agenta (Claude Code), který má sladit
> referenční seznam indikátorů a **především výpočetní algoritmy** s metodikou
> sledování stavu obojživelníků. Metodika je mapa — kód i číselníky se přizpůsobují
> jí, nikdy naopak. Soubor je verzován vedle metodiky, aby bylo dohledatelné,
> podle jakého zadání byla harmonizace provedena.
>
> **Jak jej použít.** Předej agentovi celý obsah tohoto souboru jako zadání.
> Běh je dvoufázový a mezi fázemi je **tvrdá zastávka** — agent nesmí sáhnout
> na kód, dokud neschválíš registr nálezů.

---

## 0. Role a cíl

Jsi datový/analytický inženýr pracující na repozitáři `host_naturecz` (AOPK ČR,
hodnocení stavu předmětů ochrany Natura 2000). Tvým úkolem je zajistit, aby
**každé rozhodovací pravidlo použité při hodnocení obojživelníků bylo doslovně
dohledatelné v metodice** — a naopak, aby žádná věta metodiky nezůstala
neimplementovaná bez explicitního záznamu proč.

Cíl NENÍ „opravit kód, aby lépe fungoval". Cíl je **prokazatelná shoda tří vrstev**:

```
metodika (.docx)  ->  referenční seznam indikátorů  ->  algoritmus v R
   Příloha 1              cis_indikatory_popis.csv        21_1 (extrakce)
   Tabulka 1              limity_vse.csv                  21_2 (limity)
   Tabulka 2                                              24   (Tab. 1, úroveň DP)
   §Vyhodnocení stavu                                     25   (Tab. 2, úroveň EVL)
                                                          27   (export do ISOP)
```

---

## 1. Hierarchie autority (závazná, v tomto pořadí)

1. **Metodika rozhoduje.** Prahové hodnoty, rozhodovací tabulky, seznam hodnocených
   indikátorů, příznak klíčový/neklíčový a úroveň hodnocení se přebírají výhradně
   z metodiky (Příloha 1, Tabulka 1, Tabulka 2, §Vyhodnocení stavu).
2. **Mezery se hlásí, nikdy nevymýšlejí.** Pokud metodika k něčemu mlčí
   (např. *Zaplavení litorálu* je v terénní části definováno, ale nemá pravidlo
   vyhodnocení ani řádek v Příloze 1), zapiš to do registru jako **GAP** a
   navrhni otázku pro autory metodiky. **Nedopočítávej pravidlo analogií.**
3. **Vnitřní rozpory metodiky nerozhoduješ sám.** Pokud si metodika odporuje
   nebo někde mlčí tam, kde na ní kód stojí (viz M-18, S-4), zapiš všechna
   možná čtení, uveď důsledek pro výsledky a **zastav** — rozhodnutí je
   na zadavateli, ne na tobě.
4. **Realita dat je fakt, ne autorita.** Jméno tagu ve `STRUKT_POZN`, doména
   hodnot z NDOP/Survey123 apod. jsou zjištění, která se **dokumentují**
   (komentář v kódu + řádek v číselníku), ale nesmí přepsat prahovou hodnotu
   ani rozhodovací logiku z metodiky.
5. **Neregresní pravidlo.** Nic mimo obojživelníky nesmí změnit chování.
   Sdílený kód (`21_1`, `21_2`, `24`, `25`, `27`) obsluhuje i ryby, hmyz, savce
   a rostliny — jakákoli úprava sdílené větve musí být buď prokazatelně
   neutrální, nebo podmíněná druhem/skupinou.

---

## 2. Rozsah

**V rozsahu — 6 druhů metodiky:**

| Kód v Příloze 1 | Druh (`DRUH` v datech) |
|---|---|
| BBOM | Bombina bombina |
| BVAR | Bombina variegata |
| LMON | Lissotriton montandoni |
| TRITURUS | Triturus cristatus |
| TRITURUS | Triturus carnifex |
| TRITURUS | Triturus dobrogicus |

**Mimo rozsah — `Epidalea calamita`.** Tento druh má v `limity_vse.csv` plnou
sadu limitů, ale **v Příloze 1 metodiky se nevyskytuje** (není druhem přílohy II).
Jeho řádky **neupravuj**. Do registru je zapiš do samostatné sekce
„limity bez normativního zdroje" včetně jeho vlastních nekonzistencí, aby bylo
zřejmé, že jde o vědomě odloženou položku, ne o přehlédnutí.

**Mimo rozsah dále:** stanoviště, ryby a mihule, hmyz, savci, cévnaté rostliny;
terénní část metodiky (kapitoly *Terénní sledování*, *Výběr reprezentativních
ploch*, *Prevence zavlečení patogenů*) s výjimkou definic domén hodnot, které
potřebuješ pro kontrolu vyhodnocení.

---

## 2.1 Závazná rozhodnutí zadavatele a autorů metodiky (2026-08-20)

Doplněno po Fázi A. Tato rozhodnutí **nahrazují** dřívější otevřené otázky
a jsou pro implementaci závazná. Úplné znění včetně dopadů je v
[harmonizace_registr.md](harmonizace_registr.md).

| Téma | Rozhodnutí |
|---|---|
| **Jednotky cílového stavu** (S-4) | Porovnávat `navrzena_hodnota` (jedinci/adulti) s hodnocenou početností **bez přepočtu jednotek**, včetně *Bombina bombina* (vokalizující samci). Nesoulad se vědomě dočasně toleruje a bude vyřešen v příští, expertně revidované verzi cílových stavů. **Musí být zdokumentován v kódu i propsán do výstupu.** |
| **Plocha s hloubkou < 50 cm** | Tag bude **`STA_PLOCHA50CM`**, hodnoceno **až od roku 2027**. Přejmenovat všude; hodnota zůstává `NA`, dokud data nezačnou chodit. **`ind_id` zatím nebude přiděleno — to je v pořádku**, indikátor se prozatím exportuje bez kódu ISOP. |
| **Periodické tůně** | `STA_STAVVODAPERTUNE` se do hodnocení vysychání započítává **stejnou vahou** jako trvalé tůně. |
| **`nelze vyloučit` u `STA_RYBY`** | Hodnotí se jako **příznivý stav** a musí být **explicitně uvedeno v `limity_vse.csv`** jako další přípustná hodnota `val` vedle `ne`. Kategorie **`nehodnoceno`** se normalizuje na `NA` (neznámý stav, nehodnotí se). |
| **Kategorie početnosti přes hranici tříd** | Bere se **nižší kategorie** (konzervativní předběžná opatrnost): `"11-100"` → **2** (nižší desítky). Totéž pravidlo pro další víceznačná rozpětí. |
| **Sezónní okno průhlednosti** | **Zrušit**; změnu popsat v kódu i v reportu. |
| **Zdroj `STA_MANIPULACE`** | Odvozovat **z `VLV_VLIVY`** dle metodiky (Vlivy, část Voda), nikoli z tagu. Zachovat sezónní omezení duben–červenec. `VLV_VLIVY` je seznam, jehož názvy kategorií samy obsahují čárky — **párovat vzorem nad celým řetězcem, nikdy nedělit podle čárky**. |
| **`ind_id` tříletých indikátorů** | `POP_REPROPERIOD3` = **30**, `STA_VYSYCHANIPERIOD3` = **34**. Jde o **přesun významu stávajících kódů** (dnes `POP_REPRO` a `STA_VYSYCHANI`), potvrzeno k promítnutí do ISOP. Původní řádky zůstávají v číselníku **bez `ind_id`**. |

---

## 3. Zdroje a jak je číst

### 3.1 Metodika
`Metodiky/Obojzivelnici/met_ssEVL_SLOUCENE_teren_a_vyhodnocovani_obojziv_2024_zmeny.docx`

Dokument je vedený **v režimu sledování změn**. Musíš z něj vytěžit text tak,
jak bude vypadat **po přijetí všech revizí**. To znamená odstranit vše, co
přijetím revizí zmizí:

| Značka | Význam | Co s ní |
|---|---|---|
| `<w:delText>` | smazaný text | **odstranit** |
| `<w:delInstrText>` | smazané pole | **odstranit** |
| `<w:moveFrom>` | **zdroj přesunu** — text, který se přesunul jinam | **odstranit** |
| `<w:moveTo>` | **cíl přesunu** — přesunutý text na novém místě | ponechat |
| `<w:ins>` | vložený text | ponechat |

⚠️ **Nestačí odstranit `<w:delText>`.** Obsah `<w:moveFrom>` je uložený
v běžných `<w:t>`, takže naivní recepta jej ponechá a vytvoří **duplicitní
odstavec — text, který v dokumentu ve skutečnosti není.** Ověřeno: přesně
tímto vznikal fantomový druhý nadpis *Zastínění litorálu okolní vegetací*.
`<w:moveFrom>` obsahuje vnořené značky, takže je nutné **non-greedy**
odstranění — `sed` to neumí, použij `perl`:

```bash
unzip -o -q <docx> -d <tmp>
perl -e '
local $/; my $x = <>;
$x =~ s/\r?\n//g;
$x =~ s{<w:delText[^>]*>.*?</w:delText>}{}gs;
$x =~ s{<w:delInstrText[^>]*>.*?</w:delInstrText>}{}gs;
$x =~ s{<w:moveFrom\b.*?</w:moveFrom>}{}gs;
$x =~ s{</w:tc>}{ | }g;
$x =~ s{</w:tr>}{\n}g;
$x =~ s{</w:p>}{\n}g;
$x =~ s{<w:tbl>}{\n===TABLE===\n}g;
$x =~ s{</w:tbl>}{\n===ENDTABLE===\n}g;
$x =~ s{<[^>]*>}{}g;
$x =~ s{&amp;}{&}g; $x =~ s{&lt;}{<}g; $x =~ s{&gt;}{>}g;
$x =~ s{&quot;}{"}g; $x =~ s{&apos;}{'"'"'}g;
$x =~ s{[ \t]+}{ }g; $x =~ s{ *\n *}{\n}g;
print $x;
' <tmp>/word/document.xml > met.txt
```

**Sanity check, který proveď vždy:** spočítej výskyty `<w:moveFrom `,
`<w:moveTo `, `<w:ins `, `<w:del ` a `<w:delText` v `document.xml`. Je-li
`<w:moveFrom>` přítomen, ověř, že se odpovídající text ve výstupu vyskytuje
**právě jednou**, ne dvakrát.

Přečti **celé** sekce *Vyhodnocení stavu*, *Hodnocení stavu druhu na úrovni dílčí
plochy*, *Hodnocení stavu druhu na úrovni sledovaného území*, *Příloha 1* a
z terénní části *Sledované indikátory* (kvůli doménám hodnot a definici škály
početnosti).

**Normativní obsah přepsaný v §4 odpovídá stavu dokumentu k 2026-08-20.**
Vždy jej znovu ověř proti skutečnému souboru — pokud se metodika mezitím
změnila, **platí dokument, ne tento prompt**, a rozdíl zapiš do registru.

### 3.2 Referenční seznamy
- `Data/Input/cis_indikatory_popis.csv` — **kódování Windows-1250**, sloupce
  `ind_popis, ind_id, ind_r, ind_nadr`. `ind_r` = název proměnné v R,
  `ind_id` = kód indikátoru pro ISOP (napojení v `27_n2k_druhy_zapis.R:620`).
- `Data/Input/limity_vse.csv` — **kódování Windows-1250**, sloupce
  `DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN`.
  - `TYP_IND` ∈ {`min`, `max`, `val`, `NA`}; `min`/`max` se v kódu slučují do `IND_GRP = "minmax"`.
  - Více řádků `val` pro jeden `DRUH × ID_IND` = **výčet přípustných hodnot** (logika OR).
  - Dvojice `min` + `max` = **interval** (logika AND).
  - `KLIC = "ano"` ⇒ klíčový (populační) indikátor podle Tabulky 1.
  - `UROVEN` ∈ {`lok`, `chu`}; `lok` = dílčí plocha (DP), `chu` = sledované území (EVL/MZCHÚ).
  - `LIM_IND = NA` ⇒ řádek se **nevyhodnocuje**, slouží jen jako výčet jednotek
    (`JEDNOTKA`) pro extrakci v `21_1` (viz `lim_pocet`, `lim_pocetsum`, `lim_repro`).

Při čtení i zápisu **vždy explicitně `Windows-1250`**. Nikdy tyto soubory nepřepiš
nástrojem, který by je uložil v UTF-8 — rozbily by se diakritické hodnoty limitů.

- **Cílový stav populace (nový zdroj, k M-04):** `sdo_cilove_druhy.csv` z repozitáře
  [`BiodivMonCZ/digitalizaceSDO`](https://github.com/BiodivMonCZ/digitalizaceSDO/blob/main/Outputs/Data/sdo_cilove_druhy.csv),
  sloupec `navrzena_hodnota`. Detailní specifikace a známé nástrahy viz **§4.8**.

### 3.3 Algoritmy

| Soubor | Co dělá | Kde hledat obojživelníky |
|---|---|---|
| `R/00_config/00_n2k_config.R` | načtení dat, limitů, číselníků, `CILMON`, `current_year` | ř. 65–115 (limity), 216 (číselník), 251–266, 583 |
| `R/02_druhy/21_1_n2k_druhy_akce.R` | extrakce indikátorů z `STRUKT_POZN` na úrovni nálezu; agregace na DP×rok; víceleté indikátory | blok `## Obojživelníci a plazi` ř. ~343–500; agregace ř. ~872–935; `POP_ZMENARAD` ř. ~1064–1095; `*PERIOD3` ř. ~1141–1155 |
| `R/02_druhy/21_2_n2k_druhy_akce_lim.R` | porovnání hodnot s limity → `STAV_IND` (0/1) | celý |
| `R/02_druhy/24_n2k_druhy_lokality.R` | **Tabulka 1** — celkový stav DP | ř. ~120–160 |
| `R/02_druhy/25_n2k_druhy_uzemi.R` | **Tabulka 2** — celkový stav EVL | ř. ~150–310 |
| `R/02_druhy/27_n2k_druhy_zapis.R` | export, napojení `ind_r` → `ind_id` | ř. ~549–630 |
| `R/02_druhy/20_n2k_druhy_run.R` | kaskádové spuštění 21→27 | — |

---

## 4. Normativní obsah metodiky (vytěženo — ověř proti dokumentu)

### 4.1 Tabulka 1 — celkový stav na úrovni DP

| špatně hodnocené **populační** (klíčové) indikátory | špatně hodnocené **stanovištní** indikátory | celkový stav |
|---|---|---|
| max 0 | 0–1 | **dobrý** |
| max 0 | min 2 | **zhoršený** |
| min 1 | – | **špatný** |

Tabulka má po revizi metodiky (2026-08-20) **tři řádky** — původní čtvrtý řádek
`min 1 neznámý | min 1 neznámý | neznámý` byl odstraněn.

Doprovodný text nyní končí jednoznačně: *„Indikátor se hodnotí pouze, jsou-li
dostupné informace k jeho hodnocení."* — **stav „neznámý" na úrovni DP tedy
nevzniká** a indikátor bez dostupných dat prostě nevstupuje do počtu
hodnocených indikátorů. Toto je závazné čtení; stávající implementace ve `24`
mu odpovídá (viz M-17).

### 4.2 Tabulka 2 — celkový stav na úrovni sledovaného území

| % dobře hodnocených DP | počet jedinců (klouzavý průměr za 3 roky) | celkový stav |
|---|---|---|
| min 70 % | cílový stav splněn | **dobrý** |
| min 70 % | cílový stav nesplněn | **zhoršený** |
| < 70 % | cílový stav splněn | **zhoršený** |
| < 70 % | cílový stav nesplněn | **špatný** |

- *% dobře hodnocených DP* = `počet DP v dobrém stavu / počet DP v dobrém, zhoršeném
  či špatném stavu × 100`; DP s neznámým stavem **nevstupují ani do čitatele, ani do jmenovatele**.
- *Početnost populace* = součet maximálních početností zaznamenaných na každé DP
  v daném roce; relativní kategorie se převádí na **medián kategorie** (metodika
  uvádí příklad *500 jedinců pro kategorii stovky*). Limitní hodnota = **revidovaný
  cílový stav pro dané území evidovaný v ISOP** (EVL), resp. cílová hodnota
  v platném plánu péče (MZCHÚ). Limit je tedy **specifický pro každé území**
  — zdroj dat viz **§4.8**.

### 4.3 Příloha 1 — hodnocené indikátory a limity (úroveň DP)

| INDIKÁTOR | BBOM | BVAR | LMON | TRITURUS |
|---|---|---|---|---|
| přítomnost druhu | musí být výskyt | musí být výskyt | musí být výskyt | musí být výskyt |
| porovnání odhadované početnosti | pokles o více než 1 kategorii | totéž | totéž | totéž |
| zaznamenávání reprodukce a vývojových stadií | **doplňkově** | aspoň 1× za 3 roky | aspoň 1× za 3 roky | aspoň 1× za 3 roky |
| nadměrný tlak ryb | špatně je záznam | špatně je záznam | špatně je výskyt | špatně je výskyt |
| manipulace s vodní hladinou | špatně je záznam | *(nehodnotí se)* | špatně je výskyt | špatně je výskyt |
| pravidelné vysychání vodních ploch | špatně ve **všech třech** letech po sobě | totéž | totéž | totéž |
| zastoupení vodní vegetace | *(nehodnotí se)* | špatně nad 50 % | *(nehodnotí se)* | špatně pod 1 % a nad 75 % |
| průhlednost vody | špatně pod 50 cm | *(nehodnotí se)* | *(nehodnotí se)* | špatně pod 50 cm |
| zastínění **litorálu** okolní vegetací | špatně nad 75 % | špatně nad 75 % | *(nehodnotí se)* | *(nehodnotí se)* |
| plocha s hloubkou menší než **50 cm** | špatně pod 25 % | špatně pod 75 % | špatně pod 75 % | špatně pod 25 % |
| úhyn obojživelníků | špatně výskyt | špatně výskyt | špatně výskyt | špatně výskyt |

**Prázdná buňka = indikátor se pro daný druh nehodnotí** ⇒ nesmí mít
v `limity_vse.csv` řádek s neprázdným `LIM_IND`, jinak uměle zvyšuje
`N_OTH_EXPECTED` a posouvá DP k „zhoršený".

### 4.4 Klíčové (populační) vs. stanovištní indikátory

Klíčové jsou právě tři: **přítomnost druhu**, **změna početnosti**, **prokázaná
reprodukce** (u BBOM je reprodukce pouze doplňková ⇒ `KLIC = "ne"`).
Vše ostatní z Přílohy 1 je stanovištní.

### 4.5 Pravidla agregace uvnitř DP × rok

- **Populační indikátory:** *„Do celkového hodnocení druhu na DP za daný rok
  vstupuje **nejvyšší** pozorovaná hodnota."*
- **Stanovištní indikátory:** *„Do celkového hodnocení druhu na DP za daný rok
  vstupuje **nejhorší** pozorovaná hodnota. Stačí tedy jedno překročení limitní
  hodnoty ve sledovaném roce a indikátor je hodnocen ve špatném stavu."*

### 4.6 Škála početnosti (šestistupňová, 0–5)

| kategorie | hodnota | rozsah |
|---|---|---|
| absence | 0 | 0 |
| jednotky | 1 | 1–10 |
| nižší desítky | 2 | 11–50 |
| vyšší desítky | 3 | 51–100 |
| stovky | 4 | 101–1000 |
| tisíce | 5 | 1001+ |

Hodnocenou jednotkou jsou **vokalizující samci u Bombina bombina**, u ostatních
druhů **dospělci**. Referenční hodnotou pro `POP_ZMENARAD` je **poslední předchozí
rok s cíleným monitoringem na téže DP** (`CILMON == 1`).

### 4.7 Další pravidla z textu §Vyhodnocení

- **Přítomnost:** nepříznivý stav = *pouze* negativní záznam nebo záznam s počtem 0;
  **DP bez záznamu se nehodnotí**.
- **Manipulace s vodní hladinou:** hodnotí se manipulace **od dubna do července**.
- **Vysychání:** hodnotí se v kontextu **posledních tří let**; špatně, pokud plocha
  vyschla **v každém** ze tří posledních hodnocených let.
- **Reprodukce:** špatně, není-li doložena ani jednou ze **tří posledních sezón
  s monitoringem dané DP**.
- **Nadměrný tlak ryb:** doména terénního záznamu je *ano / ne / **nelze vyloučit** /
  **nehodnoceno***.

### 4.8 Zdroj cílového stavu populace (řeší M-04)

Druhý indikátor Tabulky 2 se napojuje na `navrzena_hodnota` ze souboru
`Outputs/Data/sdo_cilove_druhy.csv` v repozitáři
[`BiodivMonCZ/digitalizaceSDO`](https://github.com/BiodivMonCZ/digitalizaceSDO).
Rozhodnutí zadavatele — **neber jej jako volitelný, ale implementuj přesně
podle níže uvedených omezení.**

**Vendorování.** Všechny ostatní vstupy pipeline jsou lokální soubory
v `Data/Input/`. Ulož proto **datovaný snapshot** (např.
`Data/Input/sdo_cilove_druhy_YYYYMMDD.csv`) a načítej jej z `00_n2k_config.R`
jako ostatní číselníky. **Nestahuj soubor za běhu** — hodnocení musí být
reprodukovatelné a nesmí být závislé na síti ani na cizím `main`.
Do commitu uveď zdrojovou URL a datum stažení.

**Relevantní sloupce:** `sitecode`, `nazev_lat`, `sdf_code`, `pop_min`, `pop_max`,
`pop_prum`, `ndop_pop_max`, `ndop_pocitano`, `navrzena_hodnota`, `varovani`,
`popis_problemu`, `source_file`.

**Známé nástrahy — každou explicitně ošetři a ověř, že stále platí:**

| # | Nástraha | Co s tím |
|---|---|---|
| S-1 | **`Lissotriton montandoni` je v souboru vedený pod synonymem `Triturus montandoni`** (`sdf_code` 2001). Přímý join na `nazev_lat` jej **tiše zahodí**. | Zaveď explicitní mapování synonym a **ověř počet napojených řádků pro každý ze 6 druhů**; nulový počet musí být tvrdá chyba, ne tichý `NA`. |
| S-2 | Všech 6 řádků `Triturus montandoni` má `navrzena_hodnota = NA` ⇒ pro tento druh **cílový stav neexistuje**. | Indikátor musí zůstat „neznámý" a **korektně se propagovat Tabulkou 2**, ne spadnout na „nesplněn". |
| S-3 | **69 z 250 řádků obojživelníků jsou duplicity `sitecode × druh`** (tentýž SDO pod dvěma variantami názvu PDF, s/bez diakritiky). U 171 ze 174 dvojic je `navrzena_hodnota` shodná, **u 3 se liší**. | Deduplikuj deterministicky a **rozhodovací pravidlo pro 3 rozporné dvojice zapiš do registru** — nevybírej mlčky první řádek. |
| S-4 | **Jednotka nesedí.** `ndop_pocitano` nabývá pouze hodnot `jedinci` / `adulti` / `NA` — **nikdy `samci`**. Pro *Bombina bombina* je ale hodnocenou jednotkou podle §4.6 **vokalizující samci** a `limity_vse.csv` omezuje `POP_POCET` u BBOM na `JEDNOTKA = samci`. Porovnávaly by se tedy dvě různé veličiny. | **Toto je GAP, ne implementační detail.** Zapiš do registru jako otázku na autory metodiky a do doby rozhodnutí nechej indikátor u BBOM „neznámý". Nepřepočítávej samce na jedince vlastním koeficientem. |
| S-5 | `navrzena_hodnota` je odvozená hodnota, prakticky `floor(max(pop_prum, ndop_pop_max))`, tj. **částečně vychází z týchž NDOP dat, která se hodnotí**. Navíc `varovani == TRUE` u 34 z 250 řádků. | Zdokumentuj v komentáři u kódu, že jde o **navrženou** hodnotu z digitalizace SDO stojící na místě revidovaného cílového stavu z ISOP (metodika mluví o ISOP). Řádky s `varovani == TRUE` **nevyřazuj**, ale propaguj příznak do výstupu, aby byl dohledatelný. |
| S-6 | Soubor je v **UTF-8**, ale české textové sloupce (`nazev_cz`, `stav_text`, `poznamka`, …) jsou už ze zdroje poškozené na `U+FFFD`. Sloupce, které potřebuješ, jsou ASCII a nedotčené. | Načítej jako UTF-8, na poškozených sloupcích nestav žádnou logiku a nepokoušej se je „opravit". |

**Strukturální rozhodnutí, které musíš vyvolat, ne vyřešit sám:**
`limity_vse.csv` je klíčovaný `DRUH × ID_IND` a **nemá rozměr území**, takže
limit specifický pro každou EVL v něm nelze vyjádřit. Obě cesty zapiš do registru
s dopady a nech zadavatele rozhodnout:
- **(a)** rozšířit `limity_vse.csv` o volitelný sloupec `KOD_CHU` (zasáhne
  generickou logiku napojení limitů ve `21_2` i `25` → riziko regrese u jiných skupin), nebo
- **(b)** napojit cílovou hodnotu přímo v `25_n2k_druhy_uzemi.R` a `STAV_IND` pro
  tento jediný indikátor spočítat mimo obecnou `minmax` větev (izolovanější,
  ale zavádí výjimku z jednotného mechanismu limitů).

**Co se má počítat.** Nový indikátor na úrovni `chu`, `KLIC = "ano"`:
klouzavý průměr za poslední 3 roky ze **součtu maximálních početností
zaznamenaných na každé DP v daném roce** (§4.2), porovnaný jako `min`
s cílovou hodnotou pro dané `kod_chu`. Pozor: stávající `POP_POCETSUM`
v `25` sčítá napříč všemi roky, **není to per-rok ani klouzavý průměr** —
jako vstup jej bez úpravy nepoužívej. Teprve s tímto indikátorem přestane
platit M-03 (EVL nemůže dosáhnout stavu „špatný"), proto **M-03 a M-04
implementuj a ověřuj společně**.

---

## 5. FÁZE A — audit a registr nálezů (žádné změny kódu)

### 5.1 Postup

Pro **každý** řádek Přílohy 1 × každý ze 6 druhů proveď průchod celým řetězcem
a zapiš, co jsi našel na každé zastávce:

1. **Metodika** → jaká je normativní věta (doslovná citace + kde).
2. **`cis_indikatory_popis.csv`** → existuje řádek? má `ind_popis`? má `ind_id`?
3. **`limity_vse.csv`** → existují řádky pro daný `DRUH × ID_IND`? Odpovídá
   `TYP_IND`/`LIM_IND` prahové hodnotě? Odpovídá `KLIC` sekci 4.4? Je `UROVEN` správná?
4. **`21_1`** → z jakého tagu se hodnota extrahuje, jaký má **typ a doménu**,
   jaká sezónní/víceletá omezení se aplikují a **kde jsou v metodice podložena**.
5. **`21_2` / `24`** → jakou agregaci indikátor projde a odpovídá to §4.5.
6. **`25`** → vstupuje indikátor do úrovně EVL a odpovídá to Tabulce 2.
7. **`27`** → exportuje se pod `ind_id`, nebo propadne na surový text?

Vedle toho udělej **kontrolu v obou směrech**:

- **Sirotci v limitech:** `ID_IND` v `limity_vse.csv` (úroveň `lok` i `chu`),
  které nemají odpovídající sloupec v `n2k_druhy` ani řádek v Příloze 1.
- **Osiřelé sloupce v kódu:** proměnné `STA_*` / `POP_*` / `LOK_*` počítané
  v `21_1`/`25` pro obojživelníky, které nemají limit ani nejsou vstupem
  jiného indikátoru.
- **Kolize domén:** pro každý vyhodnocovaný `ID_IND` porovnej **doménu hodnot
  vypočtenou v `21_1`** (číslo? 0/1? procentní pásmo? volný text?) s **doménou
  `LIM_IND`**. Neshoda domény je tichá chyba, protože `21_2` ji vyhodnotí jako
  nesplněný limit, ne jako neznámou hodnotu.
- **Dvojí započtení:** jeden řádek Přílohy 1 nesmí být pokryt více než jedním
  vyhodnocovaným `ID_IND` (jinak jedna skutečnost sráží DP dvakrát).

### 5.2 Formát registru

Zapiš do `Metodiky/Obojzivelnici/harmonizace_registr.md`. Jeden nález = jeden blok:

```markdown
### H-07 — STA_RYBY: „nehodnoceno" se počítá jako nepříznivý stav
- **Závažnost:** vysoká (mění výsledek hodnocení DP)
- **Typ:** BUG | GAP | ROZPOR-METODIKY | NÁZVOSLOVÍ | STOPA-DO-ISOP
- **Metodika:** „Přítomnost nadměrného tlaku ryb. Zaznamenává se v kategoriích:
  ano / ne / nelze vyloučit / nehodnoceno." (§Sledované indikátory)
  + Příloha 1: „špatně je záznam"
- **Stav v datech:** limity_vse.csv → `<DRUH>,STA_RYBY,val,ne,…`
- **Stav v kódu:** 21_1:420 extrakce tagu `<STA_INVDRUHRYBA>`; 21_2 větev `TYP_IND == "val"`
- **Důsledek:** hodnoty „nelze vyloučit" i „nehodnoceno" → `STAV_IND = 0`.
  „nehodnoceno" má být neznámý stav (indikátor se nehodnotí), ne nepříznivý.
- **Dotčené druhy:** všech 6
- **Návrh řešení:** …
- **Otevřená otázka pro autory metodiky:** jak vyhodnotit „nelze vyloučit"?
- **Rozhodnutí zadavatele:** hodnoceno pozitivně, nejedná se o doklad negativního vlivu. Zobrazovanou hodnotou je však "nelze vyloučit"._
```

Na konec registru přidej:

- **Matici pokrytí** — řádky Přílohy 1 × 6 druhů, v buňce `OK` / číslo nálezu /
  `N/A` (nehodnotí se) — aby bylo na jeden pohled vidět, co je čisté.
- **Sekci „limity bez normativního zdroje"** — `Epidalea calamita` a cokoli dalšího
  mimo Přílohu 1.
- **Sekci „otázky na autory metodiky"** — souhrn všech GAP a ROZPOR-METODIKY.

### 5.3 Předvyplněné nálezy (seed)

Následující nálezy už byly identifikovány. **Každý z nich nezávisle ověř
v aktuálním kódu a datech** — potvrď, vyvrať, nebo uprav — a doplň o vlastní.
Neber je jako hotová zjištění; jsou to vstupy k verifikaci.

| ID | Nález | Kde |
|---|---|---|
| M-01 | `STA_VYSYCHANI` je počítáno jako 0/1, ale limity jsou procentní pásma (`val 26-50 %`, `min 25`) → indikátor selhává pokaždé, když je stav vody vůbec zaznamenán | 21_1:382, limity_vse.csv |
| M-02 | `STA_VYSYCHANI` navíc **duplikuje** jediný řádek Přílohy 1 *pravidelné vysychání*, který už implementuje `STA_VYSYCHANIPERIOD3` (`max 2`) | limity_vse.csv |
| M-03 | Úroveň EVL nemůže nikdy dosáhnout stavu „špatný": při jediném klíčovém `chu` indikátoru (`LOK_PROCDOBR`) vzorec `CELKOVE` degeneruje na dobrý/zhoršený | 25 (blok `IND_SUMKLIC` / `LENIND_SUMKLIC`) |
| M-04 | Druhý indikátor Tabulky 2 (*početnost populace* vs. cílový stav) **není implementován**. Zdroj dat je rozhodnut: `navrzena_hodnota` z `digitalizaceSDO/sdo_cilove_druhy.csv` — **implementuj podle §4.8**, včetně nástrah S-1 až S-6 a strukturálního rozhodnutí o umístění limitu | 25, §4.8 |
| M-05 | **Revizí 2026-08-20 je hodnoceným indikátorem `STA_ZASTINENILITORAL`** (Příloha 1: *zastínění litorálu okolní vegetací*). **Data už srovnána** (viz M-06/M-10). **Zbývá kód:** v `21_1` zruš slučování hladina/litorál — `STA_ZASTINENIHLADINA` se tam přepisuje horší z obou hodnot a vzniklý sloupec už nikdo nekonzumuje (mrtvý kód); `STA_ZASTINENILITORAL` používej přímo v surové podobě. Ověř, že tag `<STA_ZASTINENIHLADINA>` se pak už nikde nevyhodnocuje | 21_1:465–493 |
| M-06 | **VYŘEŠENO 2026-08-20.** BBOM mělo pásmo `val 0-25 %` omylem na `STA_ZASTINENIHLADINA` ⇒ plocha s nejlepším možným zastíněním byla hodnocena jako nesplněná (`val` se netrefila do `{26-50 %, 51-75 %}`, `max 75` nemohla fungovat, protože hodnota je kategorie, ne číslo). Řádek přesunut na `STA_ZASTINENILITORAL`; BBOM i BVAR teď mají shodnou sadu `0-25 / 26-50 / 51-75 / max 75`. **Jen ověř, nepřepisuj** | limity_vse.csv |
| M-07 | **Vyřešeno revizí 2026-08-20:** *Zaplavení litorálu* i *Zastínění vodní hladiny okolní vegetací* jsou nadále čistě terénní indikátory bez vyhodnocení (nemají řádek v Příloze 1) ⇒ nesmí mít v `limity_vse.csv` neprázdný `LIM_IND`. Hodnoceno je *zastínění litorálu* — viz M-05 | metodika, limity_vse.csv |
| M-18 | **REDAKČNÍ VADA METODIKY (2026-08-20):** v odstavci *Zastínění litorálu okolní vegetací* zůstala věta *„Zastínění se hodnotí pro **vodní hladinu litorálu**"* — slovo „litorálu" bylo vloženo (`<w:ins>`), ale „vodní hladinu" nebylo smazáno. Jde o skutečný text dokumentu, **ne o artefakt extrakce**. Neinterpretuj, zapiš jako otázku na autory; pro implementaci platí **Příloha 1** (= litorál) | metodika |
| M-08 | `STA_RYBY`: doména *ano / ne / nelze vyloučit / nehodnoceno* vs. limit `val "ne"` ⇒ „nehodnoceno" je hodnoceno jako nepříznivý stav | 21_2, limity_vse.csv |
| M-09 | Metodika říká *plocha s hloubkou menší než **50 cm***, indikátor se jmenuje `STA_HLOUBKAMENSI20`, tag ve `STRUKT_POZN` je v kódu označen jako neověřený a v číselníku chybí `ind_id` | 21_1:494, cis_indikatory_popis.csv |
| M-10 | **Částečně vyřešeno 2026-08-20:** `STA_ZASTINENILITORAL` už v `cis_indikatory_popis.csv` je — převzal `ind_id` **37** i popis po `STA_ZASTINENIHLADINA` (*zastínění litorálu okolní vegetací*); odpovídající změna v ISOP je v gesci zadavatele. **Stále chybí** `STA_VYSYCHANIPERIOD3`, `POP_REPROPERIOD3`, `POP_POCET` ⇒ export propadne na surový text místo `ind_id`. `LOK_POCETDOBR` je naopak sirotek (nic ho nepočítá) | cis_indikatory_popis.csv, limity_vse.csv, 27 |
| M-11 | `POP_POCETNOSTNAL` používá **8 stupňů** a mapuje `"11-100" → 3`, čímž slučuje metodické kategorie *nižší desítky* (11–50) a *vyšší desítky* (51–100); metodika má **6 stupňů (0–5)**. „Pokles o více než 1 kategorii" pak znamená něco jiného | 21_1:128–157 |
| M-12 | `cis_pocet_kat` používá pro kategorii *stovky* medián **550**, metodika uvádí příklad **500** | Data/Input/cis_pocet_kat.csv |
| M-13 | `POP_REPROPERIOD3` a `STA_VYSYCHANIPERIOD3` se počítají jednou na `KOD_LOKAL + DRUH` ze 3 nejnovějších řádků a připojují se **bez `ROK`** ⇒ historické roky dědí hodnotu odvozenou z pozdějších dat | 21_1:1141–1155 a závěrečný `left_join` |
| M-14 | Sezónní okno květen – 15. června u `STA_PRUHLEDNOSTVODA` **nemá oporu v textu metodiky** | 21_1:455 |
| M-15 | `POP_ZMENARAD` má u `Epidalea calamita` `TYP_IND = max` místo `min` (obrácená logika); tentýž druh má duplicitní řádky a chybějící `POP_PRESENCE` — mimo rozsah, jen do sekce „bez normativního zdroje" | limity_vse.csv |
| M-16 | **VYŘEŠENO revizí metodiky 2026-08-20** — věta *„Všechny indikátory se vyhodnocují pouze na úrovni celé EVL…"* byla z §Vyhodnocení odstraněna. Ponecháno v registru jen pro dohledatelnost; **žádná akce v kódu**. Ověř, že v aktuálním dokumentu skutečně chybí | metodika |
| M-17 | **VYŘEŠENO revizí metodiky 2026-08-20** — odstraněn čtvrtý řádek Tabulky 1 i věta *„Pokud je alespoň jeden z indikátorů hodnocen jako neznámý…"*. Platí jednoznačně, že indikátor bez dat se nehodnotí; stávající implementace ve `24` (`N_KEY_EXPECTED`/`N_OTH_EXPECTED` počítají jen ne-`NA` indikátory) je **správná a nemění se**. Ověř to a v registru zaznamenej jako potvrzené, ne jako nález | metodika, 24 |

### 5.4 Zastávka

Po dokončení registru **skonči**. Vypiš stručné shrnutí (počet nálezů podle
závažnosti a typu, seznam otázek na autory metodiky) a **explicitně si vyžádej
schválení**. Nesahej na `.R` soubory ani na `.csv` v `Data/Input/`.

---

## 6. FÁZE B — implementace (až po schválení registru)

Spouští se **pouze** na základě schváleného registru. Implementuj **výhradně
položky označené zadavatelem jako schválené.**

### 6.1 Pravidla změn

- **Jedna položka registru = jeden commit** se zprávou `harmonizace: H-NN <krátký popis>`
  a odkazem na klauzuli metodiky v těle commitu.
- Každá netriviální změna v R dostane **komentář v češtině bez diakritiky**
  (konvence repozitáře) ve tvaru:
  `# Metodika (Priloha 1 / Tab. 1 / §Vyhodnoceni): <citace>. Proto <co kod dela>.`
- **Nikdy neměň sdílenou větev kódu bez podmínky na druh/skupinu**, pokud změna
  není prokazatelně neutrální pro ostatní skupiny. Pokud si nejsi jistý, uveď to
  v reportu a raději větev podmiň.
- `Data/Input/*.csv` zapisuj **v `Windows-1250`**, se zachovaným pořadím sloupců
  a stylem zápisu `NA`. Před commitem zkontroluj diff — nesmí obsahovat
  přeformátované řádky, kterých se změna netýká.
- **Nepřidávej nový indikátor bez `ind_id`.** Pokud kód nový `ind_r` zavádí,
  musí mít buď `ind_id` z ISOP, nebo výslovný záznam v registru, že se `ind_id`
  teprve žádá.
- **Nezaváděj syntetická data.** Chybí-li konkrétní hodnota (např. cílový stav pro
  `Lissotriton montandoni`, viz S-2), indikátor musí vracet `NA` a propagovat
  „neznámý" — nikdy dopočítanou náhradu, průměr jiných území ani analogii.
- **Externí zdroje se vendorují.** Žádný krok pipeline nesmí za běhu sahat na síť;
  snapshoty patří do `Data/Input/` s datem a zdrojovou URL v commitu (viz §4.8).

### 6.2 Ověření

Pro každou implementovanou položku:

1. **Statická kontrola:** `Rscript -e 'invisible(parse("<soubor>"))'` u všech
   dotčených souborů. Plný běh vyžaduje neveřejná vstupní data (viz komentář
   v `00_n2k_config.R`) — pokud jsou k dispozici, spusť `20_n2k_druhy_run.R`.
2. **Důkaz o dopadu:** u každého nálezu s dopadem na výsledek doprovoď změnu
   malým reprodukovatelným příkladem (minimální tabulka vstupů → očekávaný
   `STAV_IND` / `CELKOVE`), který ukazuje chování před a po.
3. **Kontrola neregrese:** vyjmenuj, které druhy/skupiny mimo obojživelníky
   sdílenou cestu procházejí, a dolož, proč se jejich výsledek nemění.
4. **Kontrola konzistence:** po změnách znovu vygeneruj matici pokrytí z §5.2
   a přilož ji do reportu — všechny buňky musí být `OK` nebo `N/A`, případně
   odkazovat na vědomě odloženou položku.

### 6.3 Výstup fáze B

Aktualizuj `harmonizace_registr.md`: u každého nálezu doplň
`**Stav:** implementováno (commit <sha>) | odloženo (důvod) | zamítnuto (důvod)`.
V odpovědi vypiš přehled provedených změn, seznam odložených položek s důvodem
a otevřené otázky na autory metodiky.

---

## 7. Zakázané postupy

- ❌ Dopočítat prahovou hodnotu, kterou metodika neuvádí (ani „logickou" analogií
  mezi druhy).
- ❌ Rozhodnout vnitřní rozpor či mlčení metodiky (např. M-18, S-4) vlastní úvahou.
- ❌ Změnit prahovou hodnotu, aby „lépe seděla na data".
- ❌ Upravit `Epidalea calamita` nebo jinou skupinu než obojživelníky.
- ❌ Přepsat `Data/Input/*.csv` v jiném kódování než `Windows-1250`.
- ❌ Přejmenovat `ind_r` bez současné aktualizace `limity_vse.csv`,
  `cis_indikatory_popis.csv` **a** všech výskytů v `R/`.
- ❌ Sloučit více nálezů do jednoho commitu.
- ❌ Ohlásit hotovo, aniž bys uvedl, co se nepodařilo ověřit a proč.

---

## 8. Definition of done

1. `harmonizace_registr.md` existuje, pokrývá **všech 11 řádků Přílohy 1 × 6 druhů**
   i obě rozhodovací tabulky, a u každého nálezu má rozhodnutí zadavatele.
2. Každý vyhodnocovaný `ID_IND` obojživelníků má: řádek v Příloze 1 → řádky
   v `limity_vse.csv` s odpovídajícím `TYP_IND`/`LIM_IND`/`KLIC`/`UROVEN` →
   sloupec v `21_1` se **shodnou doménou hodnot** → záznam
   v `cis_indikatory_popis.csv` s `ind_id`.
3. Žádný řádek Přílohy 1 není pokryt více než jedním vyhodnocovaným `ID_IND`;
   žádná prázdná buňka Přílohy 1 nemá v `limity_vse.csv` neprázdný `LIM_IND`.
4. Agregace uvnitř DP × rok odpovídá §4.5 (populační = nejvyšší,
   stanovištní = nejhorší), Tabulka 1 odpovídá `24`, Tabulka 2 odpovídá `25` —
   nebo je odchylka zaznamenaná jako vědomě odložená s uvedeným důvodem.
5. Všechny GAP a ROZPOR-METODIKY jsou shrnuty jako otázky na autory metodiky.
6. `Rscript -e 'parse(...)'` prochází u všech dotčených souborů.
