# Prompt: kritická revize metodiky hodnocení stavu cévnatých rostlin, harmonizace se strukturou `host_naturecz` a audit kódu

> **Určení.** Tento soubor je zadání pro agenta (Claude Code) pracujícího
> v repozitáři `BiodivMonCZ/host_naturecz`. Navazuje na postup, kterým byla
> harmonizována metodika obojživelníků
> (`Metodiky/Obojzivelnici/harmonizace_prompt.md` a `harmonizace_registr.md`),
> a přebírá z něj hierarchii autority, formát registru i zakázané postupy.
> Soubor patří do `Metodiky/Cevnate/` vedle revidované metodiky, aby bylo
> dohledatelné, podle jakého zadání revize proběhla.
>
> **Jak jej použít.** Předej agentovi celý obsah tohoto souboru jako zadání.
> Běh má dvě fáze a **dvě tvrdé zastávky**:
>
> 1. **Fáze A** — kritická revize metodiky a návrh harmonizovaného znění → **STOP**, čeká se na schválení a odpovědi autorů.
> 2. **Fáze B** — audit kódu a dat proti schválenému znění metodiky → **STOP**, výstupem je registr nálezů.
>
> **Implementace změn v kódu není součástí tohoto zadání.** V žádné fázi se
> nemění `R/*.R` ani `Data/Input/*.csv`.

---

## 0. Role a cíl

Jsi datový/analytický inženýr a zároveň pečlivý odborný redaktor pracující na
repozitáři `host_naturecz` (AOPK ČR, hodnocení stavu předmětů ochrany
Natura 2000). Máš tři úkoly v tomto pořadí:

1. **Kriticky zrevidovat** pracovní znění metodiky hodnocení stavu cévnatých
   rostlin v EVL — vnitřní konzistence, mezery, nejasnosti, terminologie,
   redakční vady, soulad se záznamem z posledního projednání.
2. **Harmonizovat strukturu** metodiky s přístupem repozitáře, tj. se
   strukturou metodiky obojživelníků a s třívrstvým modelem, na kterém stojí
   pipeline:

   ```
   metodika (.md)            ->  referenční seznamy            ->  algoritmus v R
     Příloha 1 (DP)               limity_cevky.csv                21_1 (extrakce)
     Tabulka 1 (DP)               cis_indikatory_popis.csv        21_2 (limity)
     Tabulka 2 (EVL)                                              24   (Tab. 1, úroveň DP)
     §Vyhodnocení stavu                                           25   (Tab. 2, úroveň EVL)
                                                                  27   (export do ISOP)
   ```

3. **Použít harmonizovanou metodiku jako měřítko** a zrevidovat kód a
   referenční seznamy, které ji mají implementovat.

Cíl NENÍ „vylepšit metodiku podle svého" ani „opravit kód". Cíl je metodika,
z níž lze **každé rozhodovací pravidlo doslovně dohledat**, a registr, který
ukáže, kde se od ní kód a data odchylují.

---

## 1. Hierarchie autority (závazná, v tomto pořadí)

1. **Dohodnutý další postup z projednání 13. 8. 2026** (úvodní blok souboru
   metodiky) má přednost před tělem metodiky (verze 2024) — **ale jen tam, kde
   je jednoznačný.** Kde je zápis neúplný nebo si odporuje (viz §2.1), nevybírej
   čtení sám: zapiš všechna možná čtení, jejich důsledek a otázku pro autory.
2. **Komentáře v závorkách s iniciálami** (např. „JG: doporučuji…") jsou
   **návrhy konzultanta, ne rozhodnutí.** Zapracuj je jako návrh k potvrzení.
3. **Mezery se hlásí, nikdy nevymýšlejí.** Prahové hodnoty, rozhodovací tabulky,
   seznam hodnocených indikátorů a příznak klíčový/stanovištní se přebírají
   výhradně z metodiky a zápisu. **Nedopočítávej limit analogií** — ani z jiného
   druhu, ani z metodiky obojživelníků, ani z `limity_cevky.csv`.
4. **Principy obojživelníků jsou vzor struktury, ne zdroj normy.** Kapitola 5
   obecné části výslovně říká „DOPLNIT PODLE PRINCIPŮ V METODICE HODNOCENÍ STAVU
   OBOJŽIVELNÍKŮ". Smíš proto navrhnout převzetí *principů* (např. jak se
   agreguje více návštěv v sezóně, jak se zachází s indikátorem bez dat, jak se
   počítá podíl DP v dobrém stavu) — **každé takové převzetí označ jako návrh
   k potvrzení autory** s odkazem na zdrojové místo metodiky obojživelníků.
   Číselné prahy se nepřebírají nikdy.
5. **Kód a referenční seznamy nejsou autoritou pro metodiku.** Hodnota
   v `limity_cevky.csv`, která se liší od textu metodiky, je **nález**, ne
   oprava textu. Nepřepisuj metodiku tak, aby seděla na kód.
6. **Realita dat je fakt, ne autorita.** Názvy tagů ve `STRUKT_POZN`, domény
   hodnot z NDOP/Survey123 apod. se dokumentují, ale nepřepisují rozhodovací
   logiku metodiky.
7. **Neregrese.** Sdílený kód (`21_1`, `21_2`, `24`, `25`, `27`) obsluhuje
   i obojživelníky (harmonizace dokončena a schválena), ryby, hmyz a savce.
   Každý návrh, který by sdílenou větev měnil, musí ve Fázi B uvádět, které
   skupiny se ho týkají, a zda musí být podmíněn skupinou/druhem.

---

## 2. Rozsah

**Metodika — v rozsahu:** celý přiložený soubor, tj. obecná část (kap. 1–6)
a druhová kapitola *Aconitum firmum* subsp. *moravicum* (oměj tuhý moravský).
Druhová kapitola slouží jako **pilot a vzor** pro ostatní druhy; obecná část
musí po harmonizaci fungovat pro všechny druhy cévnatých rostlin přílohy II.

**Kód — v rozsahu:**
- veškerá logika specifická pro cévnaté rostliny (bez ohledu na druh),
- úplný průchod řetězcem pro *Aconitum firmum* subsp. *moravicum*,
- sdílená logika (`24`, `25`) v rozsahu, v jakém určuje výsledek u rostlin.

**Mimo rozsah:** ostatních 33 druhů v `limity_cevky.csv` jednotlivě (nemají
v tomto souboru metodiku — viz §6.3), stanoviště, obojživelníci, ryby, hmyz,
savci; terénní metodiky monitoringu jednotlivých druhů (na ty metodika jen
odkazuje) s výjimkou definic domén hodnot, které potřebuješ pro kontrolu
vyhodnocení.

---

## 2.1 Záznam z projednání 13. 8. 2026 — známé nejasnosti (seed)

Následující body zápisu nejsou jednoznačné. **Každý ověř v aktuálním znění
souboru**, zapiš do revize jako samostatný nález a formuluj otázku pro autory.
Seznam není úplný — doplň další, které najdeš.

| ID | Text zápisu (zkráceně) | Proč to nejde implementovat beze změny |
|---|---|---|
| N-1 | „indikátory rozděleny na populační (klíčové) a stanovištní. Hodnoceny budou všechny… k celkovému hodnocení budou přispívat pouze klíčové. **Stanovištní indikátory a populační indikátory nepouštět do hodnocení.** Zkusit bez poškození." | Druhá věta si s první odporuje (populační = klíčové). Možná čtení: (a) do celkového hodnocení DP vstupují jen klíčové populační, stanovištní se jen zobrazují; (b) nevstupují stanovištní ani *neklíčové* populační. „Zkusit bez poškození" může znamenat vyřazení *poškození rostlin* z klíčových, nebo testovací variantu výpočtu. Důsledek pro Tabulku 1 a pro sdílený kód `24` je zásadní. |
| N-2 | Nový indikátor DP „dlouhodobý trend" (JG: přejmenovat na „početnost populace") = porovnání aktuální početnosti s **cílovou hodnotou navrženou RP**; „minimi" princip po vzoru stanovišť. | Tělo metodiky už indikátor *početnost populace* má (podíl k počáteční početnosti, ≥ 75 %). Přejmenováním by vznikly dva indikátory stejného jména. Není jasné, zda nový indikátor stávající **nahrazuje**, nebo **doplňuje**. Zdroj cílové hodnoty na úrovni DP zatím neexistuje. Význam „minimi principu" u druhů je třeba vyložit z metodiky stanovišť (viz §3.2), ne domýšlet. |
| N-3 | „Na úrovni dílčí plochy bude indikátor ‚vývoj početnosti populace' Krátkodobý trend" | Neúplná věta. Pravděpodobně přejmenování; potvrdit, zda se mění i definice (3 poslední monitoringy). |
| N-4 | Na úrovni EVL nový indikátor „celková početnost populace", limit „předmětem revize cílového stavu". | Vztah k podmínce 5.3 „celková početnost ≥ 95 % (90 %) cílového stavu" není uveden — je to tentýž indikátor? Zdroj cílového stavu pro EVL (viz K-03). |
| N-5 | EVL: zhoršený = „více než jeden indikátor ve špatném stavu", špatný = „více než dva". | Nahrazuje to procentní dvojstupňové podmínky v 5.3 (75/60 %, 50/40 %, 95/90 %)? Pokud ano, každý indikátor EVL potřebuje **jediný** limit — který (příznivý stupeň)? Při třech indikátorech znamená „více než dva" „všechny tři". „Po výpočtu bude nastavení ještě posouzeno" ⇒ jde o provizorium; zaznamenat. |
| N-6 | Nahradit „parametr" → „indikátor". | Jednoznačné; proveď všude. Souvisí i se sjednocením „lokalita" → „dílčí plocha (DP)", které zápis sám používá, ale nenařizuje — navrhni a nech potvrdit (terénní metodiky monitoringu používají „lokalita"). |

---

## 3. Zdroje a jak je číst

### 3.1 Metodika k revizi

`Metodiky/Cevnate/met_cevnate_2026_for_review.md`

- Soubor vznikl převodem z `.docx` do Markdownu. Obsahuje **artefakty převodu**
  (escapované znaky `\.`, `\=`, `\:`, tučné písmo použité místo nadpisů,
  nadpis „2. Postup prací v terénu" slitý s předchozím odstavcem). Při citacích
  je odstraň, ale **obsahově nic neměň**.
- Pokud soubor v této cestě není, **zastav a požádej o něj**. Nehledej jinou verzi
  na webu ani v bibliografii AOPK.
- Původní soubor **nikdy neupravuj** — je to referenční snímek pro revizi.

### 3.2 Vzor struktury a principů

- `Metodiky/Obojzivelnici/met_ssEVL_SLOUCENE_teren_a_vyhodnocovani_obojziv_2024_zmeny.docx`
  — vytěž text **po přijetí všech revizí** receptem z §3.1 v
  `Metodiky/Obojzivelnici/harmonizace_prompt.md` (odstranit `<w:delText>`,
  `<w:delInstrText>` **i `<w:moveFrom>`**; proveď i tam popsaný sanity check).
  Zajímá tě: členění kapitol, oddělení populačních a stanovištních indikátorů,
  Tabulka 1, Tabulka 2, Příloha 1 (matice indikátor × druh), pravidla agregace
  v rámci DP × rok a zacházení s neznámým stavem.
- `Metodiky/Obojzivelnici/harmonizace_prompt.md` a `harmonizace_registr.md` —
  vzor procesu, formátu nálezů, matice pokrytí a sekce otázek na autory.
- `Metodiky/Stanoviste/hodnoceni_stavu_stanovist.docx` a
  `Data/Input/minimisize.csv` — **jen** kvůli výkladu „minimi principu" (N-2).
  Z metodiky stanovišť vytěž doslovnou definici; analogii pro druhy formuluj
  pouze jako návrh k potvrzení.
- `Diagrams/hodnoceni_urovne.drawio` — jen ke čtení; pokud harmonizace mění
  logiku úrovní, poznamenej, které diagramy by bylo nutné aktualizovat.
  Diagramy neupravuj.

### 3.3 Referenční seznamy

- `Data/Input/limity_cevky.csv` — **kódování Windows-1250**, sloupce
  `DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN, pozn`.
  Oproti `limity_vse.csv` má navíc sloupec `pozn` a v `TYP_IND` se kromě
  `min`/`max`/`val` vyskytuje `info` a prázdná hodnota. Zjisti, jak
  `00_n2k_config.R` oba soubory slučuje a co se se sloupcem `pozn`
  a s typy `info`/prázdný v `21_2` děje.
  Význam ostatních sloupců viz §3.2 obojživelníkového promptu
  (`min`+`max` = interval, více řádků `val` = výčet přípustných hodnot,
  `LIM_IND = NA` = jen výčet jednotek pro extrakci).
- `Data/Input/cis_indikatory_popis.csv` — **Windows-1250**, napojení
  `ind_r` → `ind_id` pro export do ISOP.
- `Data/Input/sdo_cilove_druhy_20260820.csv` — vendorovaný snímek cílových
  stavů (UTF-8, české textové sloupce jsou ze zdroje poškozené — nestav na nich
  logiku). Ověř pokrytí cévnatých rostlin (viz K-03).
- `Data/Input/SDO_II_predmetolokality.csv`,
  `Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx` — kontrola počtu
  druhů a EVL uváděných v úvodu metodiky.

Při čtení **vždy explicitně uveď kódování.** V této úloze nic z `Data/Input/`
nezapisuješ.

### 3.4 Algoritmy

Čísla řádků jsou orientační — vždy je ověř `grep`em.

| Soubor | Co dělá | Kde hledat rostliny |
|---|---|---|
| `R/00_config/00_n2k_config.R` | načtení limitů a číselníků | ř. ~60–90 (`limity_cev`, sloučení s `limity_vse`, `limity_ryb`) |
| `R/00_config/02_n2k_data_druhy.R` | příprava dat z NDOP | ř. ~228 — zvláštní větev `KOD_LOKALITY` pro `SKUPINA == "Cévnaté rostliny"` |
| `R/02_druhy/21_1_n2k_druhy_akce.R` | extrakce indikátorů | ř. ~46 `DRUHY_METODIKY_OBOJ`; ř. ~300–330 `pocitat_trend`; blok `## Cévnaté rostliny` od ř. ~1141 (`POP_POCETVITAL`, `STA_POKR*`, poškození, `POP_VITAL`, `POP_POSKPOC`, `POP_POSKPERC`); trendový blok ř. ~1700–1800 (`POP_POCETMAXREF`, `POP_TREND1/2`, `POP_TREND`, `POP_TRENDLM`) |
| `R/02_druhy/21_2_n2k_druhy_akce_lim.R` | hodnota vs. limit → `STAV_IND` | celý; `val_shoda()` |
| `R/02_druhy/22_n2k_obdobi.R`, `23_n2k_posledni_nalez.R` | hodnocené období, poslední nález | pravidlo zaniklých lokalit (10 let) |
| `R/02_druhy/24_n2k_druhy_lokality.R` | agregace DP × rok a **Tabulka 1** | ř. ~50–110 (směr agregace), ~130–200 (`N_KEY_*`, `N_OTH_*`, `CELKOVE`) — **Tabulka 1 je dnes převzatá z metodiky obojživelníků a platí pro všechny skupiny** |
| `R/02_druhy/25_n2k_druhy_uzemi.R` | úroveň EVL | ř. ~105–175 (`POP_POCETSUM`, `POP_POCETDOB`, `POP_PROCDOB`, `LOK_PROCDOBR`); ř. ~225–290 cílový stav (omezen na `DRUHY_METODIKY_OBOJ`); ř. ~340–410 `CELKOVE` |
| `R/02_druhy/27_n2k_druhy_zapis.R` | export, `ind_r` → `ind_id` | napojení číselníku |
| `R/02_druhy/20_n2k_druhy_run.R` | kaskádové spuštění | — |

Jsou-li k dispozici neveřejná data (`host_data/export_data_evl.csv` apod., viz
komentář v `00_n2k_config.R`), **ověřuj domény hodnot proti ostrým datům**
(počty záznamů *Aconitum firmum* subsp. *moravicum*, skutečné hodnoty tagů).
Pokud k dispozici nejsou, uveď v registru, co zůstalo neověřeno.

---

## 4. FÁZE A — kritická revize a harmonizované znění metodiky

### 4.0 Příprava

1. Založ větev `harmonizace-cevnate`.
2. Ověř přítomnost `Metodiky/Cevnate/met_cevnate_2026_for_review.md`.
3. Přečti **celý** obojživelníkový prompt i registr a vytěž text metodiky
   obojživelníků (§3.2).

### 4.1 Kritická revize (A1)

Projdi metodiku odstavec po odstavci a kontroluj zejména:

- **Vnitřní konzistenci** — text vs. tabulky, obecná část vs. druhová kapitola,
  tělo vs. zápis z 13. 8. 2026.
- **Determinismus** — dá se každé pravidlo vyhodnotit bez lidského úsudku?
  Má každý indikátor jednotku, hodnocené období, doménu hodnot a limit?
- **Úplnost rozhodovacích pravidel** — jsou definovány všechny stavy
  (dobrý / zhoršený / špatný / neznámý) na úrovni DP i EVL?
- **Referenční hodnoty** — kdo, kdy a kde stanovuje počáteční početnost,
  cílovou hodnotu pro DP a cílový stav pro EVL; kde jsou evidovány.
- **Terminologii** — parametr/indikátor, lokalita/DP, EVL/území, klíčový/
  populační/doplňující/stanovištní, příznivý/dobrý.
- **Aktuálnost** — verze dokumentu, názvy aplikací pro sběr dat, citace.
- **Redakci** — překlepy, gramatika, neúplné věty, chybějící citace v seznamu
  literatury.

**Předvyplněné nálezy k ověření (seed).** Nejsou hotová zjištění — každý
potvrď, vyvrať, nebo uprav, a doplň vlastní.

| ID | Nález | Kde |
|---|---|---|
| M-01 | 5.2 požaduje pro příznivý stav aktuální početnost **≥ 50 %** počáteční, Tab. 1 má u *početnosti populace* limit **≥ 75 %**. Dvě různé hranice pro tutéž veličinu; není jasné, zda jde o dvě samostatná pravidla (indikátor + „minimální početnost") | 5.2, Tab. 1 |
| M-02 | 5.2 připouští příznivý stav při **jednom** nesplněném klíčovém indikátoru; metodika obojživelníků (a `24`) má „min 1 špatný populační ⇒ špatný". Navíc 5.2 definuje jen *příznivý* stav — rozlišení zhoršený/špatný na DP chybí | 5.2 |
| M-03 | „V případě jednoho a více neznámých populačních parametrů nelze hodnocení provést" — opak obojživelníkového pravidla „indikátor bez dat se nehodnotí". Rozhodnout, které platí | 5.2 |
| M-04 | Více návštěv v sezóně: metodika vybírá **celý záznam** s nejvyšší početností (shoda ⇒ nejstarší datum); obojživelníci agregují **po indikátorech** (populační = nejvyšší, stanovištní = nejhorší). Dává jiné výsledky | 5.2 |
| M-05 | *Vývoj početnosti*: text („obě hodnoty z posledních dvou monitoringů nejsou nižší než hodnota z prvního") ≠ Tab. 1 („setrvalý nebo vzestupný trend"). Text ve skutečnosti selže, jen když jsou **obě** nižší. Okno „tři poslední monitoringy" může u extenzivních lokalit (interval až 12 let) pokrývat desítky let | 4.1, Tab. 1, kap. 3 |
| M-06 | Počáteční početnost = průměr prvních tří monitoringů, „může být stanovena arbitrárně" — kdo ji stanoví, kde se eviduje, jak se pozná v datech? Co do doby, než existují tři monitoringy? | 4.1 |
| M-07 | „Minimální početnost druhu na lokalitě" je zmíněna v kap. 2 i 6, ale pro *Aconitum* není nikde vyčíslena (pokud jí není míněno pravidlo 50 % z M-01) | kap. 2, 6 |
| M-08 | Úroveň EVL: procentní dvojstupňové podmínky 5.3 vs. pravidlo počtu špatných indikátorů ze zápisu (N-5); špatný stav EVL není v 5.3 výslovně definován | 5.3 |
| M-09 | Tab. 1: hodnocená jednotka u *hydrologických poměrů* chybí; u *poškození* je limit „≤ 50 % prýtů" a jednotka „% počtu prýtů" (zdvojení); u *expanzivních druhů* „%t". Kategorie hydrologických poměrů jsou v textu s velkým i malým písmenem | Tab. 1, 4.2 |
| M-10 | Úvod uvádí 37 druhů a 221 EVL — ověř proti `SDO_II_predmetolokality.csv`/seznamu předmětolokalit; `limity_cevky.csv` obsahuje 34 druhů | kap. 1 |
| M-11 | Hlavička „Verze: 2024" vs. projednání 2026 a název souboru `…2026…`; kap. 4 uvádí Collector for ArcGIS — ověř, zda odpovídá současné praxi (Collector byl nahrazen ArcGIS Field Maps) | hlavička, kap. 4 |
| M-12 | Kap. 5 obecné části je prázdná („DOPLNIT…") — obecná pravidla vyhodnocení jsou dnes jen v druhové kapitole | kap. 5 |
| M-13 | Redakční vady (neúplný výčet): „poskytnou", „tjednoho", „se se", „sebe lépe", „stěžující", „Srpna", „. .", „Lusytk", „management stratégy"; *Kolektiv autorů (2016)* citováno v kap. 2, ale chybí v literatuře | celý text |

**Výstup A1:** `Metodiky/Cevnate/revize_metodika_cevnate.md`. Jeden nález =
jeden blok:

```markdown
### R-07 — Více návštěv v sezóně: výběr celého záznamu vs. agregace po indikátorech
- **Typ:** ROZPOR | GAP | NEJASNOST | TERMINOLOGIE | ZASTARALÉ | REDAKCE
- **Závažnost:** mění výsledek | mění interpretaci | redakční
- **Kde:** 5.2, odst. 2 — „Pokud je v průběhu jedné sezóny lokalita monitorována
  vícekrát, do hodnocení vstupuje záznam (parametry) s nejvyšší početností…"
- **Souvisí:** seed M-04; zápis N-1; metodika obojživelníků §Vyhodnocení stavu
- **Problém:** …
- **Návrh znění:** … (konkrétní text, ne obecné doporučení)
- **Otázka pro autory:** … (jen u ROZPOR / GAP / NEJASNOST)
```

Na konec revize přidej:
- **Souhrn** — počet nálezů podle typu a závažnosti.
- **Otázky pro autory** — číslovaný seznam všech ROZPOR / GAP / NEJASNOST,
  formulovaný tak, aby šel poslat autorům bez dalšího kontextu.

### 4.2 Harmonizované znění (A2)

Vytvoř `Metodiky/Cevnate/met_cevnate_2026_harmonizovana.md` v této cílové
struktuře (vychází z metodiky obojživelníků; přizpůsob, kde to obsah vyžaduje,
a odchylku zdůvodni v mapě změn):

```
Metodika sledování stavu předmětů ochrany — Cévnaté rostliny přílohy II směrnice o stanovištích
  hlavička: autoři, konzultace, verze, datum
1  Cíl sledování a metodiky vyhodnocování          (z dosavadních kap. 1 a 2)
2  Terénní sledování
   2.1 Sledované indikátory
       Stav populace (klíčové indikátory)          — definice, jednotka, doména, hodnocené období
       Stav stanoviště druhu                       — definice, plocha hodnocení (populace + ~5 m)
   2.2 Vymezení dílčích ploch                      — odkaz na metodiky monitoringu druhů
   2.3 Frekvence sledování
   2.4 Ukládání dat v systémech AOPK ČR            (z dosavadní kap. 4)
3  Vyhodnocení stavu
   3.1 Obecná pravidla                             — výběr/agregace záznamů v DP × rok,
                                                     indikátor bez dat, referenční hodnoty
   3.2 Hodnocení stavu druhu na úrovni dílčí plochy  — Tabulka 1 (rozhodovací tabulka)
   3.3 Hodnocení stavu druhu na úrovni EVL           — Tabulka 2 (indikátory + rozhodovací pravidlo),
                                                     neznámý stav, nové a zaniklé DP
   3.4 Slabé stránky a interpretace výsledků        (z dosavadní kap. 6)
Příloha 1  Hodnocené indikátory a limity pro jednotlivé druhy na úrovni DP
           (matice indikátor × druh; prázdná buňka = nehodnotí se)
Příloha 2  Druhové listy — jeden oddíl na druh (pilot: Aconitum firmum subsp. moravicum):
           vymezení DP, odkaz na terénní metodiku, frekvence, druhově specifické
           definice indikátorů, specifická literatura
Literatura
```

Pravidla pro A2:

- **Nic nepřibývá bez stopy.** Každý přesun, sloučení či nové znění musí mít
  řádek v **mapě změn** na konci `revize_metodika_cevnate.md`:
  `původní oddíl → nový oddíl → typ (přesun | přeformulování | ze zápisu N-x |
  návrh převzatý z obojživelníků | oprava R-xx)`.
- **Prahy beze změny.** Limity se do Přílohy 1 přenášejí z druhové kapitoly
  doslova. Kde zápis limit mění, ale nová hodnota není stanovena, buňka
  zůstává s vyznačenou otázkou, ne prázdná a ne dopočítaná.
- **Otevřené body jsou vidět v textu**: `**[OTÁZKA R-xx]**` u nevyřešeného
  místa, `**[NÁVRH — převzato z metodiky obojživelníků, §…]**` u převzatého
  principu, `**[ZÁPIS N-x]**` u změny ze zápisu.
- **Tabulky 1 a 2 musí být deterministické** — každá kombinace vstupů vede
  k právě jednomu stavu, včetně stavu „neznámý" na úrovni EVL. Pokud to bez
  rozhodnutí autorů nejde, uveď varianty vedle sebe.
- **Bez kódových názvů.** `ID_IND`, názvy proměnných a souborů do metodiky
  nepatří; mapování indikátorů na kód patří do registru Fáze B.
- **Jednotná terminologie** podle rozhodnutí v revizi (min. „parametr" →
  „indikátor" dle N-6).
- Zachovej veškerý odborný obsah autorů včetně oddílu o slabých stránkách.
  Redakční opravy dělej rovnou; obsahové jen přes nález R-xx.
- Formát: Markdown bez artefaktů převodu, nadpisy `#`/`##`, tabulky v Markdownu,
  latinská jména kurzívou.

### 4.3 Zastávka 1

Commitni pouze tři soubory v `Metodiky/Cevnate/` (tento prompt, revizi,
harmonizované znění) zprávou `cevnate: revize a harmonizovane zneni metodiky`.
Pak **skonči** a vypiš:

1. souhrn nálezů podle typu a závažnosti,
2. seznam otázek pro autory (číslovaný),
3. seznam míst, kde harmonizované znění závisí na odpovědi (odkazy R-xx).

**Explicitně si vyžádej schválení** harmonizovaného znění a odpovědi na otázky.
Fázi B nespouštěj, dokud je nedostaneš. Rozhodnutí zadavatele pak doplň do
revize (pole `**Rozhodnutí zadavatele:**` u každého nálezu) a promítni do
harmonizovaného znění **před** zahájením Fáze B.

---

## 5. FÁZE B — audit kódu a dat proti harmonizované metodice

Měřítkem je **schválené** harmonizované znění. Kde zůstal bod otevřený,
audituj proti všem variantám a uveď, která varianta odpovídá dnešnímu kódu.

### 5.1 Postup

Pro **každý řádek Přílohy 1 × *Aconitum firmum* subsp. *moravicum*** a pro
**oba indikátory/podmínky úrovně EVL** projdi celý řetězec a zapiš nález
na každé zastávce:

1. **Metodika** → normativní věta (doslovná citace + oddíl harmonizovaného znění).
2. **`cis_indikatory_popis.csv`** → řádek, `ind_popis`, `ind_id`.
3. **`limity_cevky.csv`** → řádky `DRUH × ID_IND`; odpovídá `TYP_IND`/`LIM_IND`
   prahu, `JEDNOTKA` jednotce, `KLIC` rozdělení klíčový/stanovištní, `UROVEN` úrovni?
4. **`21_1`** → zdrojový tag a pole, **typ a doména** hodnoty, časové okno,
   referenční hodnota; kde je to v metodice podloženo.
5. **`21_2` / `24`** → směr agregace v DP × rok a vstup do Tabulky 1.
6. **`25`** → vstup do Tabulky 2.
7. **`27`** → export pod `ind_id`, nebo propad na surový text?

Dále proveď kontroly v obou směrech (definice viz §5.1 obojživelníkového promptu):
**sirotci v limitech**, **osiřelé sloupce v kódu**, **kolize domén**
(hodnota z `21_1` vs. doména `LIM_IND` — tichá chyba, protože `21_2` ji vyhodnotí
jako nesplněný limit) a **dvojí započtení**.

Zvlášť posuď **sdílenou logiku**: Tabulka 1 v `24` a výpočet `CELKOVE` v `25`
byly psány a dokumentovány pro obojživelníky. U každého rozdílu mezi nimi
a metodikou rostlin uveď, zda by náprava musela být podmíněna skupinou, a které
skupiny by se jinak změnily.

**Předvyplněné nálezy k ověření (seed).** Z rychlého čtení kódu při přípravě
zadání; **neověřeno během běhu** — každý potvrď, vyvrať, nebo uprav.

| ID | Nález | Kde |
|---|---|---|
| K-01 | **Možná obrácená logika `POP_TREND`.** `POP_TREND1/2 = 1`, když je rok **stejný nebo lepší** než reference; `POP_TREND` je jejich součet, limit `max 1` s jednotkou „posledních dvou let s **menším** počtem jedinců než referenční hodnota". Pokud `21_2` hodnotu neinvertuje, indikátor selže právě tehdy, když jsou **oba** roky dobré. Dále: `sum(..., na.rm = TRUE)` dává 0 i bez dat; reference je 3. nejnovější `ROK` bez ohledu na `CILMON` (filtr je zakomentovaný), zatímco metodika mluví o „třech posledních monitoringách". Doplň malý reprodukovatelný příklad | 21_1 trendový blok, limity_cevky.csv |
| K-02 | *Početnost populace* u *Aconitum*: limit `POP_POCET min 50` (počet prýtů, absolutně) vs. metodika ≥ 75 % počáteční početnosti (podíl). Ověř, zda kód počáteční početnost vůbec počítá a zda existuje podmínka 50 % z 5.2 | limity_cevky.csv, 21_1 |
| K-03 | Úroveň EVL: `POP_PROCDOB min 50` vs. metodika ≥ 75 % populace v DP v příznivém stavu; podmínka *celková početnost vs. cílový stav* pro rostliny **není implementována** (blok cílového stavu v `25` je omezen na `DRUHY_METODIKY_OBOJ`) a ve snímku `sdo_cilove_druhy_20260820.csv` ***Aconitum* chybí** (z rostlin jen *Cypripedium calceolus* a *Liparis loeselii*). Ověř i v upstreamu `BiodivMonCZ/digitalizaceSDO` — **ale nic nestahuj do pipeline** | 25, Data/Input |
| K-04 | `POP_POSKPERC` u *Aconitum* = **součet kategorií** 0–3 přes typy poškození, limit `max 2` s jednotkou „procenta počtu prýtů"; metodika: ≤ 50 % poškozených prýtů. Kolize domény. `POP_POSKPOC max 3` (počet typů poškození) nemá v metodice oporu — sirotek? | 21_1 ~1321–1360, limity_cevky.csv |
| K-05 | Tabulka 1 v `24` (obojživelníková) vs. pravidlo DP pro rostliny (M-02, N-1). Dolož, jaké hodnocení DP dnes rostliny dostávají a čím by se lišilo | 24 |
| K-06 | Výběr záznamu při více návštěvách (M-04): metodika celý záznam s max. početností, kód agreguje po indikátorech (`24`, populační max, stanovištní min, `POP_POSK` min) | 24 |
| K-07 | Neznámý stav: metodika (M-03) vs. `24`, kde se indikátory s `STAV_IND = NA` nepočítají; pravidlo EVL „≥ 50 % populace v DP v neznámém stavu ⇒ EVL nehodnotit" — je implementováno? | 24, 25 |
| K-08 | `limity_cevky.csv`: řádky `TYP_IND = info` a prázdný `TYP_IND` (např. `POP_VITAL info` vedle `POP_VITAL min 30` u *Aconitum*); sloupec `pozn`. Ověř, jak se chovají při sloučení v `00_n2k_config.R` a ve `21_2` / `24` (`n_distinct(ID_IND)` a metadata „první nalezená hodnota") | 00_config, 21_2, 24 |
| K-09 | Stanovištní pásma *Aconitum* (`do 5 %`, `5-25 %`, …) vs. limity metodiky (≤ 75 %, ≤ 25 %, = 0) a vs. skutečné hodnoty tagů v datech — kolize domén? | limity_cevky.csv, 21_1 |
| K-10 | Zaniklé lokality (10 let od posledního záznamu) a nové lokality — kde a jak implementováno | 23, 25 |
| K-11 | Zvláštní větev `KOD_LOKALITY` pro cévnaté rostliny — odpovídá vymezení DP v metodice? | 02_n2k_data_druhy.R ~228 |

### 5.2 Formát registru

`Metodiky/Cevnate/harmonizace_registr.md`, formát nálezu **shodný
s obojživelníkovým registrem** (ID `H-NN`, typ BUG | GAP | ROZPOR-METODIKY |
NÁZVOSLOVÍ | STOPA-DO-ISOP, závažnost, citace metodiky, stav v datech, stav
v kódu s čísly řádků, důsledek, dotčené druhy, návrh řešení, otevřená otázka,
pole pro rozhodnutí zadavatele). U každého nálezu navíc:

- **Dotčené skupiny ve sdíleném kódu:** rostliny / obojživelníci / ryby / hmyz / savci.
- **Odkaz na seed** (K-xx) a na nález revize (R-xx), je-li.

Na začátek registru dej tabulku „Jak byl audit proveden" (zdroje, rozsah,
zda byla k dispozici ostrá data a kolik záznamů *Aconitum* bylo zkontrolováno).
Na konec přidej:

- **Matici pokrytí pro *Aconitum*** — řádky = indikátory Přílohy 1 + indikátory
  EVL, sloupce = metodika / číselník / limity / `21_1` / `24` / `25` / `27`;
  v buňce `OK` / číslo nálezu / `N/A`.
- **Sdílená logika cévnatých rostlin** — nálezy, které se netýkají jen
  *Aconitum*, se seznamem všech dotčených druhů z `limity_cevky.csv`.
- **Limity bez normativního zdroje** — ostatních 33 druhů: jen souhrnně
  (počet řádků, indikátory mimo strukturu harmonizované Přílohy 1); jednotlivě
  je neaudituj.
- **Otázky na autory metodiky** — souhrn všech GAP a ROZPOR-METODIKY.
- **Diagramy k aktualizaci** — pokud se mění logika úrovní.

Jsou-li k dispozici ostrá data a zápis N-5 to vyžaduje („po výpočtu bude
nastavení ještě posouzeno"), smíš připravit **srovnávací výpočet** (dnešní
logika vs. varianty z harmonizovaného znění) jako jednorázový skript **mimo
`R/` a mimo commit**; do registru dej jen výslednou tabulku a popis postupu.

### 5.3 Zastávka 2

Commitni pouze `Metodiky/Cevnate/harmonizace_registr.md` zprávou
`cevnate: registr nalezu auditu kodu`. **Skonči** a vypiš počet nálezů podle
závažnosti a typu, nálezy dotýkající se sdíleného kódu a otázky pro autory.
Implementaci nenavrhuj jako hotovou věc — ta bude předmětem samostatného zadání.

---

## 6. Zakázané postupy

- ❌ Dopočítat prahovou hodnotu, kterou metodika ani zápis neuvádí (ani analogií
  mezi druhy, ani z metodiky obojživelníků, ani z `limity_cevky.csv`).
- ❌ Rozhodnout rozpor nebo mlčení metodiky či zápisu vlastní úvahou (N-1 až N-5).
- ❌ Upravit text metodiky tak, aby odpovídal kódu nebo limitům.
- ❌ Upravit `met_cevnate_2026_for_review.md`.
- ❌ Měnit `R/*.R`, `Data/Input/*`, `Diagrams/*` nebo cokoli mimo `Metodiky/Cevnate/`.
- ❌ Číst nebo zapisovat `Windows-1250` soubory bez explicitního kódování.
- ❌ Stahovat data za běhu pipeline nebo nahrazovat vendorované snímky.
- ❌ Přeskočit zastávku nebo spustit Fázi B bez schváleného znění.
- ❌ Ohlásit hotovo, aniž bys uvedl, co se nepodařilo ověřit a proč.

---

## 7. Definition of done

**Fáze A**
1. `revize_metodika_cevnate.md` pokrývá každý oddíl metodiky, každý bod zápisu
   z 13. 8. 2026 a všechny seedy M-01 až M-13 (potvrzeno / vyvráceno / upraveno).
2. `met_cevnate_2026_harmonizovana.md` má cílovou strukturu z §4.2, deterministické
   Tabulky 1 a 2 (nebo vyznačené varianty), Přílohu 1 a druhový list *Aconitum*;
   každý otevřený bod je v textu označen.
3. Mapa změn umožňuje dohledat původ každého oddílu.
4. Otázky pro autory jsou samostatně srozumitelné.

**Fáze B**
5. Matice pokrytí *Aconitum* nemá prázdnou buňku.
6. Každý seed K-01 až K-11 je potvrzen, vyvrácen, nebo upraven, s odkazem na řádky kódu.
7. U každého nálezu ve sdíleném kódu jsou vyjmenovány dotčené skupiny.
8. Je uvedeno, co bylo ověřeno proti ostrým datům a co jen proti kódu.
