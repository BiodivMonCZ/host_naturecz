# Revize metodiky hodnocení stavu cévnatých rostlin — Fáze A

**Zadání:** [harmonizace_cevnate_prompt.md](harmonizace_cevnate_prompt.md)
**Revidovaný snímek:** [met_cevnate_2026_for_review.md](met_cevnate_2026_for_review.md) (beze změny)
**Harmonizované znění:** [met_cevnate_2026_harmonizovana.md](met_cevnate_2026_harmonizovana.md)
**Datum:** 2026-10-02 · **Větev:** `harmonizace-cevnate` (založena z `druhy-202609`)
**Stav:** Fáze A dokončena 2026-10-02; **rozhodnutí zadavatele doplněna 2026-10-02** (u každého nálezu a v souhrnu [Rozhodnutí zadavatele](#rozhodnutí-zadavatele--souhrn-2026-10-02)) a promítnuta do harmonizovaného znění. Otevřené zůstávají jen dílčí body R-19, R-24, R-31 a R-34.

> Před zahájením byla složka `Metodiky/Rostliny/` (soubory nebyly v gitu) na pokyn
> zadavatele přejmenována na `Metodiky/Cevnate/`, aby odpovídala cestám v zadání.

## Jak byla revize provedena

| Krok | Zdroj | Kódování | Poznámka |
|---|---|---|---|
| Revidovaný text | `met_cevnate_2026_for_review.md` | UTF-8 | převod z .docx; v citacích odstraněny artefakty převodu (`\.`, `\=`, `\:`, tučné písmo místo nadpisů, nadpis „2. Postup prací v terénu" slitý s předchozím odstavcem), obsah beze změny |
| Vzor struktury a principů | `Metodiky/Obojzivelnici/met_ssEVL_SLOUCENE_…_zmeny.docx` | XML UTF-8 | text po přijetí revizí receptem z §3.1 obojživelníkového promptu (odstraněny `<w:delText>`, `<w:delInstrText>`, `<w:moveFrom>`); **sanity check:** `<w:moveFrom>` 0 · `<w:moveTo>` 0 · `<w:ins>` 4 · `<w:del>` 0 · `<w:delText>` 0 ⇒ duplicitní odstavce vzniknout nemohou; 1 247 řádků |
| Vzor procesu | `harmonizace_prompt.md`, `harmonizace_registr.md` (obojživelníci) | UTF-8 | přečteny celé |
| Výklad „minimi principu" (N-2) | `Metodiky/Stanoviste/hodnoceni_stavu_stanovist.docx` | XML UTF-8 | revize přijaty stejným receptem (`<w:del>` 32 · `<w:delText>` 22 · `<w:ins>` 48 · `<w:moveFrom>` 0); 1 633 řádků |
| | `Data/Input/minimisize.csv` | ASCII (⊂ UTF-8) | 119 řádků stanoviště × biotop × minimiareál |
| Počty druhů a EVL (M-10) | `Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx` | XML UTF-8 | list *Předmětolokality* 3 330 řádků, list *Souhrn* |
| | `Data/Input/SDO_II_predmetolokality.csv` | **Windows-1250** | jen podmnožina (214 lokalit se SDO II) |
| | `Data/Input/cis_metodika.csv` | **Windows-1250** | příslušnost druhu ke skupině *Cévnaté rostliny* |
| | `Data/Input/limity_cevky.csv` | **Windows-1250** | jen počet druhů (kontext; audit až ve Fázi B) |
| Cílový stav (jen ilustrace k R-31) | `Data/Input/sdo_cilove_druhy_20260820.csv` | UTF-8 | české textové sloupce jsou ze zdroje poškozené (`U+FFFD`) — citace v R-31 je z nich rekonstruována, žádná logika na nich nestojí |
| Diagramy | `Diagrams/hodnoceni_urovne.drawio` | — | jen čtení |

Kód v `R/` a referenční seznamy **nejsou ve Fázi A autoritou** a nebyly auditovány.
Nic v `R/`, `Data/Input/` ani `Diagrams/` nebylo změněno.

### Co se ve Fázi A nepodařilo ověřit

- **Terénní metodika monitoringu** *Aconitum firmum* subsp. *moravicum*
  (Popelářová et al. 2022) v repozitáři není ⇒ domény terénního záznamu (přesné
  procento vs. třídy, způsob záznamu poškození) nelze ověřit proti zdroji —
  viz R-24; ve Fázi B se ověří proti ostrým datům (seed K-04, K-09).
- **Zdroj čísel 37 druhů / 221 EVL** v úvodu (R-11) — referenční datum ani zdroj nejsou uvedeny.
- **Ostrá data** (`../host_data/export_data_evl.csv` aj.) jsou na pracovní stanici
  k dispozici, ve Fázi A použita nebyla; použijí se ve Fázi B.

---

# Nálezy

Pořadí sleduje text metodiky: hlavička → zápis z 13. 8. 2026 → obecná část
(kap. 1–6) → kapitola *Aconitum*. Pokrytí seedů a oddílů je v tabulkách na konci.

### R-01 — Hlavička: verze 2024, chybějící afiliace a datum
- **Typ:** GAP
- **Závažnost:** redakční
- **Kde:** hlavička — „Metodiky hodnocení stavu předmětů ochrany evropsky významných lokalit cévnatých rostlin · Ondřej Popelka, Martin Vojík · Konzultace: Jonáš Gaigr · Verze: 2024"
- **Souvisí:** seed M-11 (část 1); název souboru `met_cevnate_2026_…`; zápis z 13. 8. 2026
- **Problém:** Hlavička uvádí verzi 2024, dokument ale obsahuje zápis z projednání 13. 8. 2026 a soubor je veden jako 2026. Chybí afiliace autorů a datum verze. Název („Metodiky hodnocení…", množné číslo) se liší od vzoru metodiky obojživelníků („Metodika sledování stavu předmětů ochrany — Obojživelníci Přílohy 2 Směrnice o stanovištích"). Konzultant druhové kapitoly (Martin Kočí) je uveden jen u ní.
- **Návrh znění:** „Metodika sledování stavu předmětů ochrany — Cévnaté rostliny přílohy II směrnice o stanovištích. Autoři: Ondřej Popelka¹, Martin Vojík². Konzultace: Jonáš Gaigr; druhový list *Aconitum firmum* subsp. *moravicum*: Martin Kočí. Verze 2026 (po projednání 13. 8. 2026)." + afiliace.
- **Otázka pro autory:** Jaké jsou afiliace autorů a jaké číslo a datum má nést nová verze?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Všichni autoři i konzultanti: Agentura ochrany přírody a krajiny ČR. Verze 2026.

### R-02 — Zápis N-1: které indikátory přispívají k celkovému stavu DP
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** zápis z 13. 8. 2026, bod 1 — „Na úrovni dílčí plochy budou indikátory rozděleny na populační (klíčové) a stanovištní. Hodnoceny budou všechny dosavadní indikátory; k celkovému hodnocení budou přispívat pouze klíčové. Stanovištní indikátory a populační indikátory nepouštět do hodnocení. Zkusit bez poškození."
- **Souvisí:** seed N-1; R-03, R-26; metodika obojživelníků Tabulka 1
- **Problém:** První dvě věty jsou jednoznačné: všechny indikátory se hodnotí, do celkového stavu DP vstupují jen klíčové = populační. Třetí věta si s tím odporuje, protože „populační" jsou podle první věty právě „klíčové". Možná čtení:
  - **(a)** zkratka/překlep — míněno „stanovištní indikátory nepouštět do hodnocení"; všechny populační indikátory jsou klíčové a vstupují;
  - **(b)** populační indikátory se dělí na klíčové a neklíčové; do celkového stavu vstupují jen klíčové populační, stanovištní a neklíčové populační se jen zobrazují. Které populační indikátory jsou neklíčové, zápis neříká (kandidát: poškození rostlin, R-03);
  - **(c)** „nepouštět do hodnocení" = vůbec nehodnotit (indikátor nedostane stav) — odporuje větě „Hodnoceny budou všechny dosavadní indikátory", proto málo pravděpodobné.

  **Ve všech třech čteních platí, že stanovištní indikátory do celkového stavu DP nevstupují.** Tuto část považuji za jednoznačnou a závaznou (hierarchie autority, bod 1). Odpovídá i dosavadnímu 5.2, kde je podmínka příznivého stavu formulována jen přes populační parametry. Metodika rostlin se tím zásadně liší od Tabulky 1 metodiky obojživelníků, kde ≥ 2 špatné stanovištní indikátory dávají stav „zhoršený".
- **Důsledek:** Bez stanovištních indikátorů nemá rozhodovací tabulka DP vstup, ze kterého v metodice obojživelníků vzniká stav „zhoršený" (viz R-26). Pro sdílený výpočet (Fáze B) to znamená, že Tabulku 1 obojživelníků nelze na rostliny použít beze změny.
- **Návrh znění:** „Na úrovni DP se hodnotí všechny indikátory uvedené v Příloze 1. Do celkového stavu DP vstupují pouze klíčové indikátory (stav populace). Stanovištní indikátory se hodnotí a zobrazují, do celkového stavu DP nevstupují."
- **Otázka pro autory:** Platí čtení (a), tj. všechny populační indikátory jsou klíčové a stanovištní se jen zobrazují? Nebo (b) — a pak které populační indikátory jsou neklíčové? Nebo (c)?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Čtení (b): klíčové jsou populační indikátory **kromě poškození rostlin** — u *Aconitum* velikost populace, krátkodobý trend populace a vitalita populace (shodně s dnešním příznakem klíčovosti v `limity_cevky.csv`). Stanovištní indikátory a poškození se hodnotí a zobrazují, do celkového stavu DP nevstupují.

### R-03 — Zápis N-1: „Zkusit bez poškození"
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** zápis, bod 1, poslední věta — „Zkusit bez poškození."; 4.1 — „Poškození rostlin: Podíl poškozených prýtů například okusem nebo rytím zvěře."; Tab. 1 — „poškození rostlin | ≤ 50 % prýtů"
- **Souvisí:** seed N-1; R-02, R-26
- **Problém:** Možná čtení: **(i)** poškození rostlin přestane být klíčovým indikátorem (hodnotí se a zobrazuje, do stavu DP nevstupuje); **(ii)** jednorázový srovnávací výpočet s poškozením a bez něj, rozhodnutí až podle výsledku (obdoba věty „Po výpočtu hodnocení bude nastavení ještě posouzeno" u N-5); **(iii)** indikátor se vypustí úplně.
- **Důsledek:** Mění se počet klíčových indikátorů. Při pravidle 5.2 (dobrý stav snese jeden nesplněný klíčový indikátor) je DP se třemi klíčovými indikátory relativně přísněji hodnocena než se čtyřmi; při pravidle obojživelníků (R-26, varianta B) odpadá jedna z možných příčin stavu „špatný".
- **Návrh znění:** V Příloze 1 ponechat poškození rostlin jako klíčové s vyznačením otázky; rozhodnutí promítnout do sloupce „klíčový".
- **Otázka pro autory:** Znamená „Zkusit bez poškození" (i) přeřazení poškození mezi neklíčové indikátory, (ii) srovnávací výpočet s poškozením a bez něj, nebo (iii) vypuštění indikátoru? Pokud (ii), podle čeho se o výsledku rozhodne?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Čtení (i): poškození rostlin je **neklíčový** indikátor (hodnotí se a zobrazuje).

### R-04 — Zápis N-2: nový indikátor „dlouhodobý trend" vs. stávající „početnost populace"
- **Typ:** ROZPOR
- **Závažnost:** mění výsledek
- **Kde:** zápis, bod 2 — „Na úrovni dílčí plochy bude u všech druhů doplněn indikátor “dlouhodobý trend” odvozený od početnosti populace (JG: doporučuji přejmenování na “početnost populace”, je to srozumitelnější – trend evokuje hodnocení stabilní/rostoucí/klesající). Jde o porovnání aktuální početnosti populace s cílovou hodnotou…"; 4.1 — „Početnost populace: Podíl aktuální (údaj z posledního monitoringu) a počáteční početnosti druhu na lokalitě, vyjádřený v procentech."; Tab. 1 — „početnost populace | ≥ 75 | % počtu prýtů"
- **Souvisí:** seed N-2; R-05, R-06, R-19, R-25
- **Problém:** Zápis říká „doplněn", tj. indikátor navíc. Návrh konzultanta (JG, tedy návrh, ne rozhodnutí) přejmenovat jej na „početnost populace" by ale vytvořil druhý indikátor téhož jména — stávající „početnost populace" porovnává s **počáteční početností**, nový s **cílovou hodnotou**. Poslední věta bodu 2 („bude vyžadována alespoň cílová velikost populace, která odpovídá aktuální limitní hodnotě indikátoru ‚početnost populace‘") předpokládá, že stávající indikátor nebo aspoň jeho limit existuje dál. Možná čtení:
  - **(A) nahrazuje** — reference stávajícího indikátoru se mění z počáteční početnosti na cílovou hodnotu; název „početnost populace" zůstává (návrh JG dává smysl);
  - **(B) doplňuje** — oba indikátory existují vedle sebe, potřebují odlišné názvy a oba (?) vstupují do počtu klíčových indikátorů.
- **Důsledek:** V čtení (B) přibývá klíčový indikátor, který nelze hodnotit, dokud neexistují cílové hodnoty (R-05); při pravidle 5.2 „jeden neznámý klíčový ⇒ nelze hodnotit" (R-27) by tím do jejich stanovení byla neznámá **každá** DP.
- **Návrh znění:** Do rozhodnutí vést oba indikátory odděleně: „početnost populace (vůči počáteční početnosti)" a „dlouhodobý trend (pracovní název; vůči cílové hodnotě DP)"; návrh JG uvést jako návrh konzultanta.
- **Otázka pro autory:** Nový indikátor „dlouhodobý trend" stávající „početnost populace" (vůči počáteční početnosti) nahrazuje, nebo doplňuje? Pokud doplňuje: jak se mají oba jmenovat (návrh JG „početnost populace" koliduje se stávajícím názvem) a je nový indikátor klíčový?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Doplňuje.** Dva indikátory: **velikost populace** (dosavadní „početnost populace“; DP, vůči počáteční početnosti DP, limit ≥ 75 %) a **dlouhodobý trend populace** (nový). Upřesnění zadavatele (2026-10-02): dlouhodobý trend populace se hodnotí **na úrovni EVL** (viz R-05).

### R-05 — Zápis N-2: cílová hodnota pro DP zatím neexistuje
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** zápis, bod 2 — „…s cílovou hodnotou, kterou navrhne na základě podkladů Ústředí patřičné RP AOPK ČR."
- **Souvisí:** seed N-2; R-04, R-06, R-27
- **Problém:** Není určeno, (1) kde se cílová hodnota pro DP eviduje (ISOP? druhový list? samostatný seznam?), (2) v jaké jednotce (u *Aconitum* prýty), (3) kdo ji schvaluje a jak často se reviduje, (4) jaké hodnocené období indikátor má a (5) jak se indikátor hodnotí, dokud hodnota neexistuje. Dnes není stanovena pro žádnou DP.
- **Důsledek:** viz R-04 — pokud je indikátor klíčový a platí pravidlo 5.2 o neznámých parametrech, nelze do stanovení cílových hodnot hodnotit žádnou DP.
- **Návrh znění:** „Cílovou hodnotu pro DP navrhuje příslušné regionální pracoviště AOPK ČR na základě podkladů Ústředí." + otevřená otázka k evidenci a k období do jejího stanovení.
- **Otázka pro autory:** Kde a v jaké jednotce bude cílová hodnota pro DP evidována, kdo ji schvaluje a jak se indikátor hodnotí, dokud pro DP stanovena není (neznámý stav, nebo se nehodnotí)?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Cílové hodnoty se stanoví **na úrovni území (EVL) z výsledků digitalizace SDO** (navržená hodnota). Cílové hodnoty pro jednotlivé DP se nezavádějí. Upřesnění zadavatele (2026-10-02): protože SDO má jednu hodnotu na EVL (*Aconitum*: jedna EVL, ~257 DP), hodnotí se dlouhodobý trend populace na úrovni EVL a výsledky digitalizace SDO se používají jen tam.

### R-06 — Zápis N-2: výklad „minimi" principu
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** zápis, bod 2 — „Po vzoru hodnocení stanovištních předmětů ochrany bude aplikován “minimi” princip a bude vyžadována alespoň cílová velikost populace, která odpovídá aktuální limitní hodnotě indikátoru “početnost populace”."
- **Souvisí:** seed N-2; R-04, R-13, R-25; metodika stanovišť
- **Problém:** Výraz „minimi princip" se v metodice stanovišť (revize přijaty) **doslovně nevyskytuje**. Nejbližší pojmy:
  - definice předmětu ochrany: „Tato specifikace slouží k určení, uplatňuje-li se požadavek splnění minimálního areálu pro hodnocení stavu předmětu ochrany pro daný biotop."
  - parametr *Minimiareál*: „Parametr minimiareál zastupuje celkovou rozlohu stanoviště v celistvém stavu a má ukázat, nakolik je zastoupení stanoviště na lokalitě dostatečné a jakou má perspektivu do budoucna (obecně platí, že čím je plocha větší, tím je perspektiva lepší)." … „Popis parametru – procento rozlohy všech polygonů daného stanoviště, které v celistvém prostorovém uspořádání dosahují alespoň hodnoty minimiareálu."
  - hodnoty: expertně stanovené pro jednotlivé biotopy (Příloha 2 metodiky stanovišť; v repozitáři `minimisize.csv`, 119 řádků); „V případě, že se stanoviště v EVL vyskytuje ve více biotopech s různými hodnotami minimiareálu, je využita vždy vyšší hodnota."
  - role v hodnocení (kap. 5 metodiky stanovišť): minimiareál patří k parametrům indikujícím vývoj, u nichž „je sledován pouze jejich trend a nevstupují do celkového hodnocení lokality".
  - pro srovnání *Rozloha*: „Hodnota rozlohy je porovnávána s hodnotou stanovenou jako cílový stav na lokalitě a je hodnoceno její dosažení."

  Analogie pro druhy proto není přímá. Možná čtení věty ze zápisu:
  - **(A) dolní mez cílové hodnoty** — cílová hodnota DP navržená RP nesmí být nižší než velikost populace odpovídající aktuálnímu limitu indikátoru „početnost populace" (u *Aconitum* 75 % počáteční početnosti); „minimi" = minimum přípustné cílové hodnoty;
  - **(B) minimální velikost populace na DP** jako podmínka dobrého stavu (obdoba prahu „alespoň hodnoty minimiareálu"), totožná s „minimální početností druhu na lokalitě" z kap. 2 a 6 (R-13), případně s podmínkou ≥ 50 % v 5.2 (R-25);
  - **(C) jako u stanovišť** — sledovaný ukazatel vývoje bez vlivu na celkový stav. Odporuje formulaci „bude vyžadována", je to ale jediná doslovná role minimiareálu v metodice stanovišť.
- **Návrh znění:** V harmonizovaném znění citovat zápis bez výkladu; analogii formulovat až po odpovědi.
- **Otázka pro autory:** Co znamená „minimi princip" u druhů: (A) dolní mez pro cílovou hodnotu DP navrhovanou RP, (B) minimální velikost populace na DP jako podmínka dobrého stavu, nebo (C) sledovaný ukazatel bez vlivu na celkový stav (jako minimiareál u stanovišť)? A je „aktuální limitní hodnotou indikátoru ‚početnost populace‘" míněno 75 % počáteční početnosti (Tab. 1), nebo 50 % (5.2)?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) „Minimi“ = **75 %**: dlouhodobý trend populace je splněn, dosahuje-li celková početnost populace v EVL alespoň 75 % cílového stavu podle digitalizace SDO.

### R-07 — Zápis N-3: neúplná věta o „vývoji početnosti populace"
- **Typ:** NEJASNOST
- **Závažnost:** mění interpretaci
- **Kde:** zápis, bod 3 — „Na úrovni dílčí plochy bude indikátor “vývoj početnosti populace” Krátkodobý trend"
- **Souvisí:** seed N-3; R-04, R-20
- **Problém:** Věta nemá přísudek. Nejpravděpodobněji jde o přejmenování na „krátkodobý trend" jako dvojici k „dlouhodobému trendu" z N-2. Není jasné, zda se mění i definice (tři poslední monitoringy, R-20). Přijme-li se návrh JG přejmenovat „dlouhodobý trend" na „početnost populace", dvojice dlouhodobý/krátkodobý trend se rozpadne.
- **Návrh znění:** pracovně „vývoj početnosti populace (dle zápisu ‚krátkodobý trend‘)", definice beze změny.
- **Otázka pro autory:** Má se „vývoj početnosti populace" přejmenovat na „krátkodobý trend"? Mění se tím i definice a okno tří posledních monitoringů?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Přejmenovat na **krátkodobý trend populace**. Nová definice: početnost v hodnocené sezóně se porovná s **průměrem tří předchozích hodnocených sezón** (hodnocená sezóna do průměru nevstupuje), v rámci 12letého okna (R-18); splněno, je-li početnost hodnocené sezóny alespoň rovna tomuto průměru (upřesnění zadavatele (2026-10-02)). **Doplněno po Fázi B (2026-10-02):** tři předchozí sezóny jsou ideál, **přípustné jsou dvě**.

### R-08 — Zápis N-4: „celková početnost populace" na úrovni EVL vs. podmínka 5.3
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** zápis, bod 4 — „Na úrovni EVL indikátorů bude u všech druhů doplněn indikátor “celková početnost populace”. Limitní hodnota bude předmětem revize cílového stavu."; 5.3 — „Celková početnost populace v EVL dosahuje alespoň 95 % cílového stavu pro EVL." / „…alespoň 90 % cílového stavu pro EVL."
- **Souvisí:** seed N-4; R-09, R-31
- **Problém:** 5.3 podmínku celkové početnosti vůči cílovému stavu už obsahuje, zápis přesto mluví o „doplnění". Možná čtení: **(A)** tentýž ukazatel, zápis jej jen formálně ustavuje jako indikátor (místo „podmínky") a limit přijde s revizí cílového stavu; **(B)** nový, další indikátor — pak má EVL čtyři indikátory, což mění pravidlo N-5 (R-09). Nejasná je i věta „Limitní hodnota bude předmětem revize cílového stavu": zůstávají hranice 95 % / 90 % cílového stavu, nebo se limitem stane přímo (revidovaný) cílový stav?
- **Návrh znění:** První indikátor Tabulky 2 = „celková početnost populace v EVL" s limity 95 % / 90 % cílového stavu z 5.3 a vyznačenou otázkou.
- **Otázka pro autory:** Je indikátor „celková početnost populace" ze zápisu totožný s podmínkou 5.3 „celková početnost populace v EVL dosahuje alespoň 95 % (90 %) cílového stavu"? Pokud ano, platí dál hranice 95 % / 90 %, nebo se limitem stane přímo revidovaný cílový stav? Pokud ne, čím se oba indikátory liší?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Ano**, tentýž ukazatel. Sloučen s dlouhodobým trendem populace na úrovni EVL (R-05); limit 75 % cílového stavu (R-06) nahrazuje 95 % / 90 %.

### R-09 — Zápis N-5: pravidlo počtu špatných indikátorů vs. dvojstupňové hranice 5.3
- **Typ:** ROZPOR
- **Závažnost:** mění výsledek
- **Kde:** zápis, bod 5 — „Na úrovni EVL bude limit zhoršeného stavu změněn na více než jeden indikátor ve špatném stavu a limit špatného stavu na více než dva indikátory. Po výpočtu hodnocení bude nastavení ještě posouzeno."; 5.3 celý oddíl
- **Souvisí:** seed N-5; R-08, R-29, R-31; metodika obojživelníků Tabulka 2
- **Problém:** 5.3 určuje stav EVL dvěma sadami procentních hranic (dobrý 95/75/50 %, zhoršený 90/60/40 %). Pravidlo ze zápisu předpokládá, že každý indikátor EVL má **jediný** limit (dobrý/špatný) — který z nich (95/75/50, nebo 90/60/40), zápis neříká. Při třech indikátorech znamená „více než dva" „všechny tři" a „více než jeden" „dva"; stav „dobrý" tedy připouští **jeden** indikátor ve špatném stavu (u obojživelníků dává jeden špatný indikátor ze dvou „zhoršený"). Při čtyřech indikátorech (R-08, čtení B) by „špatný" nastal při třech nebo čtyřech. Zápis nastavení výslovně označuje za provizorní.
- **Důsledek — příklad:**

  | EVL | početnost / cílový stav | podíl populace v DP v dobrém stavu | podíl DP v dobrém stavu | 5.3 doslovně | N-5, limity dobrého stavu | N-5, limity zhoršeného stavu |
  |---|---|---|---|---|---|---|
  | 1 | 97 % | 80 % | 55 % | dobrý | dobrý (0 špatných) | dobrý (0) |
  | 2 | 92 % | 80 % | 45 % | zhoršený | zhoršený (2) | **dobrý** (0) |
  | 3 | **50 %** | 80 % | 60 % | špatný | **dobrý** (1) | **dobrý** (1) |
  | 4 | 85 % | 55 % | 35 % | špatný | špatný (3) | špatný (3) |

  EVL 3 ukazuje, že podle N-5 by EVL s polovičním stavem populace vůči cílovému stavu vyšla jako „dobrá".
- **Návrh znění:** Tabulka 2 obsahuje obě varianty vedle sebe; varianta N-5 označena jako provizorní.
- **Otázka pro autory:** Nahrazuje pravidlo počtu špatných indikátorů (N-5) dvojstupňové hranice 5.3? Pokud ano, který limit je jediným limitem každého indikátoru — hodnota pro dobrý stav (95/75/50 %), nebo pro zhoršený (90/60/40 %)? Je záměr, že EVL s jediným indikátorem ve špatném stavu (např. početnost na 50 % cílového stavu) vychází jako „dobrá"?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Postupovat jako u obojživelníků.** Upřesnění zadavatele (2026-10-02): na úrovni EVL **dva indikátory** — podíl DP v dobrém stavu (jediný limit = hodnota dobrého stavu z verze 2024, ≥ 50 %) a dlouhodobý trend populace (≥ 75 % cílového stavu); rozhodovací tabulka 2×2 (oba splněny = dobrý, jeden nesplněn = zhoršený, oba nesplněny = špatný). Podmínka „podíl populace v DP v dobrém stavu“ se **vypouští**.

### R-10 — Terminologie: parametr/indikátor, lokalita/DP, doplňující/stanovištní, příznivý/dobrý
- **Typ:** TERMINOLOGIE
- **Závažnost:** mění interpretaci
- **Kde:** celý text; zápis, bod 6 — „Nahradit termín “parametr” termínem “indikátor”"
- **Souvisí:** seed N-6; R-23
- **Problém a provedené sjednocení:**
  - **„parametr" → „indikátor"** — jednoznačné ze zápisu; provedeno všude (včetně „klíčové parametry", „doplňující parametry", „populační parametry", „dílčí parametry").
  - **„lokalita" → „dílčí plocha (DP)"** — kapitola *Aconitum* užívá „lokalita" pro dílčí plochu (5.2 „Stav lokality", 5.3 „lokalit v příznivém stavu", „Nově vzniklé lokality", „Zaniklé lokality"), kap. 1 i pro EVL („na úrovni jednotlivých lokalit a EVL"). Zápis sám píše „dílčí plocha", metodika obojživelníků „dílčí plocha (DP)". **Návrh k potvrzení:** terénní metodiky monitoringu (Popelářová et al. 2022) užívají „lokalita" — proto se v oddílu 2.2 jednou výslovně uvádí, že DP = „lokalita" terénní metodiky.
  - **„doplňující parametry" (4.2) → „stanovištní indikátory"** — podle zápisu (N-1) a vzoru obojživelníků („Stav stanoviště druhu").
  - **„příznivý stav" → „dobrý stav"**, „limitní hodnota příznivého stavu" → „limitní hodnota dobrého stavu" — stavy dobrý / zhoršený / špatný / neznámý používá metodika obojživelníků (Tab. 1, 2) i metodika stanovišť („Stav předmětu ochrany … Nabývá hodnot: dobrý, zhoršený, špatný, neznámý"). Kde text verze 2024 dává jen dvojici příznivý/nepříznivý (5.2), je převod na čtyřstupňovou škálu otevřený — viz R-26.
  - **„EVL"** zůstává — metodika nepokrývá MZCHÚ (R-12).
- **Návrh znění:** promítnuto do harmonizovaného znění.
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Schváleno.

### R-11 — Úvod: počty druhů a EVL neodpovídají seznamu předmětolokalit
- **Typ:** NEJASNOST
- **Závažnost:** mění interpretaci
- **Kde:** kap. 1, odst. 2 — „V České republice se v současnosti vyskytuje 37 původních druhů cévnatých rostlin přílohy II, pro které je vyhlášeno 221 EVL."
- **Souvisí:** seed M-10
- **Problém (ověřeno, seed upraven):** Seznam předmětolokalit Natura 2000 (verze 2/2025, list *Předmětolokality*) vede **36 druhů** cévnatých rostlin jako předměty ochrany ve **175 EVL** (213 dvojic druh × EVL; souhrnný list dává pro tyto druhy rovněž 213). Druhy byly určeny podle skupiny „Cévnaté rostliny" v číselníku metodik a s ohledem na synonyma, pod nimiž je seznam vede (*Adenophora lilifolia*, *Gentianella bohemica*, *Iris humilis* subsp. *arenaria*, *Pontechium maculatum* subsp. *maculatum*, *Stipa zalesskii*, *Vandenboschia speciosa*). Číslo **221** neodpovídá počtu EVL (175) ani počtu dvojic (213). Číslo **37** může odpovídat počtu původních druhů přílohy II vyskytujících se v ČR bez ohledu na to, zda jsou předmětem ochrany některé EVL (číselník metodik vede ve skupině 39 položek, z toho jeden poddruh duplicitně a *Aldrovanda vesiculosa*, která není původní) — bez zdroje nelze potvrdit.
  Pro kontext Fáze B (nikoli pro text metodiky): `limity_cevky.csv` obsahuje 34 druhů, z toho 6 pod jiným jménem než seznam předmětolokalit; *Artemisia pancicii* a *Crambe tataria* v něm chybí. `SDO_II_predmetolokality.csv` je jen podmnožina (214 lokalit se SDO II) a k ověření počtů se nehodí. *Aconitum firmum* subsp. *moravicum* je předmětem ochrany v **jediné EVL** (CZ0724089 Beskydy).
- **Návrh znění:** „V České republice je předmětem ochrany v EVL 36 druhů cévnatých rostlin přílohy II, a to ve 175 EVL (stav podle seznamu předmětolokalit Natura 2000, verze 2/2025)." Alternativně čísla vypustit a odkázat na aktuální seznam předmětolokalit.
- **Otázka pro autory:** Z jakého zdroje a k jakému datu pocházejí údaje „37 původních druhů" a „221 EVL"? Lze je nahradit údajem ze seznamu předmětolokalit 2/2025 (36 druhů ve 175 EVL), nebo čísla z textu vypustit?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Metodika zatím pokrývá jen *Aconitum firmum* subsp. *moravicum*; čísla v úvodu ponechána a v textu vyznačena jako neověřená.

### R-12 — Úvod: povinnost reportingu „na úrovni jednotlivých EVL"; rozsah EVL vs. MZCHÚ
- **Typ:** NEJASNOST
- **Závažnost:** mění interpretaci
- **Kde:** kap. 1, odst. 2 — „Členské státy EU mají kromě povinnosti vytvoření sítě EVL a legislativní ochrany, také povinnost pravidelného reportingu stavu jejich populací a stanovišť na úrovni celostátní i na úrovni jednotlivých EVL. … Zatímco reporting stavu druhů na celostátní úrovni probíhá dle standardizovaných metodik v šestiletém intervalu, hodnocení na úrovni jednotlivých EVL nebylo dosud systematicky řešeno."
- **Souvisí:** metodika obojživelníků §Cíl sledování a metodiky vyhodnocování
- **Problém:** Věty si částečně odporují (povinnost reportingu na úrovni EVL vs. „nebylo dosud systematicky řešeno") a tvrzení o povinnosti na úrovni EVL není doloženo odkazem. Metodika obojživelníků formuluje účel jinak: „Výsledky budou podkladem pro vyhotovení Hodnotících zpráv podle článku 17 Směrnice o stanovištích (92/43/EEC; §45f zákona 114/1992 Sb.), péče o druh jako předmět ochrany a jeho stanoviště a případně standardních datových formulářů v rámci sledovaného území." Metodika obojživelníků navíc pokrývá „sledované území" včetně MZCHÚ („V případě hodnocení předmětů ochrany MZCHÚ jde o cílovou hodnotu uvedenou v platném plánu péče."); metodika rostlin mluví jen o EVL.
- **Návrh znění:** doplnit větu o účelu převzatou z metodiky obojživelníků (vyznačeno jako návrh); větu o povinnosti reportingu na úrovni EVL buď doložit, nebo přeformulovat.
- **Otázka pro autory:** Je tvrzení o povinnosti reportingu stavu „na úrovni jednotlivých EVL" záměrné (a na jaký předpis se odkazuje), nebo lze účel formulovat jako v metodice obojživelníků (podklad pro zprávy podle čl. 17, péči a standardní datové formuláře)? Vztahuje se metodika rostlin i na předměty ochrany MZCHÚ, nebo jen na EVL?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Metodika je určena **jen pro EVL (Natura 2000)**; MZCHÚ se nehodnotí. Věta autorů o reportingu ponechána beze změny, navržená věta z metodiky obojživelníků vypuštěna.

### R-13 — Minimální početnost druhu na lokalitě (kap. 2 a 6) není u *Aconitum* vyčíslena
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** kap. 2 — „Kromě jednotlivých parametrů je pro každý druh stanovena také minimální početnost druhu na lokalitě, při které není možné lokalitu hodnotit pozitivně."; kap. 6 — „Kromě vybraných parametrů, je tedy pro každý druh stanovena také minimální početnost druhu na lokalitě, při které není možné lokalitu hodnotit pozitivně."
- **Souvisí:** seed M-07; R-06, R-25
- **Problém (seed potvrzen):** Kapitola *Aconitum* žádnou absolutní minimální početnost neuvádí. Jediná podmínka tohoto druhu je relativní: „aktuální velikost populace dosahuje alespoň 50 % počáteční početnosti" (5.2, R-25). Formulace „při které není možné" je logicky obrácená — míněno je zřejmě „pod kterou". Pojem může souviset s „minimi" principem ze zápisu (R-06).
- **Návrh znění:** „Kromě jednotlivých indikátorů je pro každý druh stanovena také minimální početnost druhu na DP, pod kterou není možné DP hodnotit jako v dobrém stavu (Příloha 1)."
- **Otázka pro autory:** Je „minimální početností" míněna podmínka 5.2 „alespoň 50 % počáteční početnosti", nebo má mít každý druh absolutní minimální početnost (počet prýtů / jedinců)? Pokud absolutní, jaká je u *Aconitum*? A je správně „pod kterou" místo „při které"?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Minimální početnost se odvozuje z výsledků digitalizace SDO: na úrovni EVL **75 % cílového stavu** (R-06). Samostatná podmínka minimální početnosti na úrovni DP není (viz R-25); věta v kap. 2 a 6 přeformulována na úroveň EVL.

### R-14 — Ukládání dat: aplikace Collector for ArcGIS
- **Typ:** ZASTARALÉ
- **Závažnost:** redakční
- **Kde:** kap. 4 — „Data z monitoringu se zadávají do Nálezové databáze ochrany přírody (NDOP) pomocí aplikací Survey123 for ArcGIS (formulář) a Collector for ArcGIS (zákres)."
- **Souvisí:** seed M-11 (část 2)
- **Problém (seed potvrzen):** Aplikaci Collector for ArcGIS nahradila u Esri aplikace ArcGIS Field Maps. Metodika obojživelníků uvádí jen „aplikace ESRI ArcGIS Survey123". Zda zákres dnes probíhá ve Field Maps, nebo v mapové otázce Survey123, z repozitáře zjistit nelze — prosím potvrdit při schvalování.
- **Návrh znění:** „…pomocí aplikací ArcGIS Survey123 (formulář) a ArcGIS Field Maps (zákres)."
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Schváleno (ArcGIS Field Maps).

### R-15 — „Jiná data z NDOP … pouze jako prezence": k čemu prezence slouží a co je „monitoring"
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** kap. 4 — „Takto získaná data jsou v NDOP zadaná pod projektem Monitoring druhů ČR. Jiná data z NDOP mohou být zahrnuta pouze jako prezence druhu na lokalitě. Ostatní údaje do hodnocení nevstupují, protože u nich nelze dodržet požadavek na jednotnost sběru a jednotnou strukturu jejich zadání."
- **Souvisí:** R-18, R-19, R-32; registr obojživelníků H-62, H-69
- **Problém:** Mezi indikátory *Aconitum* žádný indikátor přítomnosti není, takže není jasné, kam „prezence" z jiných dat vstupuje. Kandidáti: (a) poslední prokazatelný záznam u zaniklých DP (5.3), (b) vznik nové DP, (c) nikam (jen informativně). Dále není řečeno, zda „monitoring" v pojmech „poslední monitoring", „první tři monitoringy", „tři poslední monitoringy" znamená výhradně sběr v projektu Monitoring druhů ČR podle terénní metodiky. U obojživelníků vedla nejasnost téhož typu k nesouměrnému uplatnění cíleného monitoringu mezi úrovní DP a EVL (registr obojživelníků H-62) — u rostlin je vhodné ji vyjasnit předem.
- **Návrh znění:** „Monitoringem se v této metodice rozumí sběr dat v projektu Monitoring druhů ČR podle terénní metodiky monitoringu druhu. Jiná data z NDOP se použijí pouze jako doklad přítomnosti druhu na DP, a to pro …" (doplnit dle odpovědi).
- **Otázka pro autory:** K čemu slouží „prezence druhu" z jiných dat NDOP — k určení posledního prokazatelného záznamu u zaniklých DP, k vymezení nových DP, nebo k něčemu jinému? Rozumí se „monitoringem" (poslední monitoring, první tři monitoringy) výhradně sběr v projektu Monitoring druhů ČR?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Přítomnost druhu (včetně posledního prokazatelného záznamu) ze **všech záznamů NDOP**; všechny ostatní indikátory výhradně ze záznamů projektu **„Monitoring druhů ČR“**.

### R-16 — Kapitola 5 obecné části je prázdná
- **Typ:** GAP
- **Závažnost:** mění interpretaci
- **Kde:** kap. 5 — „DOPLNIT PODLE PRINCIPŮ V METODICE HODNOCENÍ STAVU OBOJŽIVELNÍKŮ"
- **Souvisí:** seed M-12; R-17, R-26–R-28
- **Problém (seed potvrzen):** Obecná pravidla vyhodnocení jsou dnes jen v druhové kapitole (5.1–5.3). Harmonizované znění je přesouvá do obecné části (3.1–3.3) a principy metodiky obojživelníků uvádí jako vyznačené varianty k rozhodnutí — nikde je nepřebírá mlčky.
- **Návrh znění:** kapitola 3 harmonizovaného znění.
- **Otázka pro autory:** Souhlasíte, aby pravidla z kapitoly *Aconitum* (5.1–5.3) tvořila obecnou část vyhodnocení pro všechny druhy (rozdělení viz R-17) a principy metodiky obojživelníků byly uvedeny jen jako varianty k rozhodnutí?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Ano.

### R-17 — Které definice a pravidla z kapitoly *Aconitum* platí pro všechny druhy
- **Typ:** NEJASNOST
- **Závažnost:** mění interpretaci
- **Kde:** kapitola *Aconitum* — 4.1 (počáteční početnost), 4.2 (plocha hodnocení stanoviště „ca do vzdálenosti 5 m", definice E1, invazních a expanzivních druhů), 5.2 (výběr záznamu, neznámé parametry), 5.3 (celý oddíl včetně hranic 95/75/50 a 90/60/40 %)
- **Souvisí:** R-16; zadání §2 („Druhová kapitola slouží jako pilot a vzor… obecná část musí po harmonizaci fungovat pro všechny druhy")
- **Problém:** Všechna rozhodovací pravidla existují jen v kapitole jednoho druhu. Není stanoveno, která jsou obecná a která druhově specifická. Hranice úrovně EVL jsou zapsány v druhové kapitole, mohou tedy být u každého druhu jiné.
- **Návrh znění (rozdělení v harmonizovaném znění):**
  - *obecné* (kap. 2–3): pravidla 5.2 a 5.3 bez číselných hodnot, definice počáteční početnosti, plocha hodnocení stanoviště, obecné definice stanovištních indikátorů (včetně vymezení invazních druhů podle Pergl et al. 2016 a Pyšek et al. 2022), pravidla pro nové a zaniklé DP;
  - *druhově specifické* (Příloha 1, druhový list): všechny číselné limity včetně hranic úrovně EVL a minimální početnosti, hodnocená jednotka (prýt), vymezení kategorií hydrologických poměrů, výjimka *Arrhenatherum elatius* u invazních druhů, frekvence monitoringu.
- **Otázka pro autory:** Platí uvedené rozdělení na obecná a druhově specifická pravidla? Zejména: jsou hranice úrovně EVL (95/75/50 %, 90/60/40 %) společné pro všechny druhy, nebo druhově specifické? Platí výjimka *Arrhenatherum elatius* u invazních druhů jen pro *Aconitum*?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Zatím řešit **jen *Aconitum***. Obecná část je ponechána, ale v textu vyznačena jako odvozená z kapitoly *Aconitum* a dosud ověřená jen pro tento druh; limity úrovně EVL vedeny v Příloze 1B u druhu.

### R-18 — „Hodnocené období" a „poslední monitoring" nejsou vymezeny
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** kapitola *Aconitum*, 3 — „Aktualizace hodnocení probíhá vždy při zadání nových dat, odvíjí se tedy od frekvence monitoringu jednotlivých lokalit. Vzhledem k množství lokalit jsou lokality rozděleny na lokality intenzivního monitoringu s tříletým intervalem návštěv a lokality extenzivního monitoringu s až dvanáctiletým intervalem návštěv…"; 4.1 — „…údaje ze třech posledních monitoringů (včetně hodnoceného období)"; Tab. 1, sloupec „Hodnocené období" („poslední monitoring", „tři poslední monitoringy")
- **Souvisí:** R-15, R-20, R-30, R-31; metodika obojživelníků („Do celkového hodnocení druhu na DP za daný rok vstupuje…")
- **Problém:** Pojem „hodnocené období" je použit, ale nikde vymezen (kalendářní rok? šestileté období reportingu? rok posledního monitoringu?). Na úrovni EVL není řečeno, který záznam každé DP vstupuje do hodnocení: při až dvanáctiletém intervalu extenzivního monitoringu mohou „poslední monitoringy" jednotlivých DP pocházet z let vzdálených až 12 let a není stanoveno, kdy je záznam příliš starý (kromě 10 let u zaniklých DP, R-32). Metodika obojživelníků hodnotí DP pro každý rok („DP za daný rok").
- **Návrh znění:** Pro úroveň DP převzít princip obojživelníků — stav DP se hodnotí pro každý rok s monitoringem (DP × rok); vyznačeno jako návrh. Pro úroveň EVL otázka.
- **Otázka pro autory:** Co je „hodnocené období"? Hodnotí se stav DP pro každý rok s monitoringem zvlášť (jako u obojživelníků)? Které záznamy jednotlivých DP vstupují do hodnocení EVL — vždy poslední monitoring každé DP bez ohledu na stáří, nebo jen monitoring v určitém okně (např. 12 let, šestileté období reportingu)?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Okno 12 let**: do hodnocení DP i EVL vstupují jen záznamy z posledních 12 let; DP se hodnotí pro každou sezónu (DP × rok). Délku okna zapsat do číselníku period druhů (`cis_evd_perioda.csv`) — stav ověří Fáze B, změna dat není součástí tohoto zadání.

### R-19 — Počáteční početnost: stanovení, evidence, období do tří monitoringů
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** 4.1 — „Za počáteční početnost druhu na lokalitě se se považuje průměr početností z prvních tří monitoringů. V případě relevantních důvodů, může být počáteční početnost populace stanovena arbitrárně."
- **Souvisí:** seed M-06; R-04, R-15, R-21, R-27, R-28
- **Problém (seed potvrzen a rozšířen):**
  1. **Arbitrární stanovení** — kdo o něm rozhoduje (RP? Ústředí? autoři?), co jsou „relevantní důvody", kde se hodnota eviduje a jak se pozná, že platí arbitrární místo vypočtené.
  2. **Početnost jednoho monitoringu** při více návštěvách v sezóně — podle 5.2 záznam s nejvyšší početností (R-28)?
  3. **Co je „první monitoring"** — první záznam podle terénní metodiky (Popelářová et al. 2022), nebo i starší data (R-15)?
  4. **Období do tří monitoringů** — indikátor do té doby nelze spočítat. Při pravidle 5.2 „jeden a více neznámých populačních parametrů ⇒ nelze hodnotit" (R-27) by se DP do třetího monitoringu nehodnotila vůbec; u DP extenzivního monitoringu (interval až 12 let) to může znamenat **až zhruba 24 let** od prvního monitoringu.
  5. **Stálost** — je počáteční početnost po stanovení pevná? Co když vyjde 0 (dělení nulou)?
  6. **Vztah k N-2** — nahradí-li „dlouhodobý trend" stávající indikátor (R-04, čtení A), zůstane pojem počáteční početnosti potřebný jen pro podmínku 50 % (R-25).
- **Návrh znění:** text ponechat a vyznačit otázku; po odpovědi doplnit, kde se arbitrární hodnota eviduje.
- **Otázka pro autory:** Kdo a kde eviduje arbitrárně stanovenou počáteční početnost a z jakých důvodů ji lze stanovit? Počítá se průměr z prvních tří monitoringů podle terénní metodiky, nebo z prvních tří záznamů vůbec? Jak se DP hodnotí, dokud tři monitoringy neexistují? Je počáteční početnost po stanovení pevná a jak postupovat, vyjde-li 0?
- **Rozhodnutí zadavatele:** ⏸ (2026-10-02) **Částečně.** Počáteční početnost DP = průměr z prvních tří monitoringů DP (projekt Monitoring druhů ČR, R-15); výsledky digitalizace SDO se používají jen na úrovni EVL (upřesnění R-05). Dokud tři monitoringy neexistují, je velikost populace neznámá, a tedy i stav DP (R-27). **Otevřené:** kdo stanoví arbitrární počáteční početnost a jak postupovat, vyjde-li počáteční početnost 0. **Doplněno po Fázi B (2026-10-02):** tři monitoringy jsou ideál, **počáteční početnost lze spočítat i ze dvou**. Arbitrární počáteční početnost stanoví **samostatný skript pro všechny skupiny a všechny DP — maximální početnost kdy zaznamenaná pro druh na DP** (mimo toto zadání). Otevřené zůstává jen: počáteční početnost 0.

### R-20 — Vývoj početnosti populace: tři neshodné formulace a délka okna
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** 4.1 — „Vývoj početnosti populace: Vývoj početnosti, založený na třech posledních údajích o velikosti populace, respektive údaje ze třech posledních monitoringů (včetně hodnoceného období). Limitní hodnoty je dosaženo, pokud vývoj početnosti populace nevykazuje sestupný trend, respektive obě hodnoty z posledních dvou monitoringů nejsou nižší, než hodnota z prvního monitoringu."; Tab. 1 — „vývoj početnosti populace | = setrvalý nebo vzestupný trend | počet prýtů | tři poslední monitoringy"
- **Souvisí:** seed M-05; R-07, R-18, R-27
- **Problém (seed upraven — jeho čtení je jen jedno ze dvou doslovných):** Označme x1, x2, x3 početnosti tří posledních monitoringů (x1 nejstarší). Text obsahuje tři různá pravidla:
  - **(T)** Tab. 1 a první část věty — „nevykazuje sestupný trend"; způsob určení trendu (např. sklon regrese) ani tolerance pro „setrvalý" nejsou uvedeny;
  - **(A)** „obě hodnoty … nejsou nižší" ve smyslu „ani jedna není nižší": splněno, jen když x2 ≥ x1 **a** x3 ≥ x1;
  - **(B)** „není pravda, že obě jsou nižší": nesplněno, jen když x2 < x1 **a** x3 < x1 (čtení seedu M-05).

  | x1, x2, x3 | (A) | (B) | (T) sklon regrese přes pořadí monitoringů |
  |---|---|---|---|
  | 100, 80, 120 | nesplněno | splněno | +10 → splněno |
  | 100, 120, 90 | nesplněno | splněno | −5 → nesplněno |
  | 100, 90, 95 | nesplněno | nesplněno | −2,5 → nesplněno |
  | 100, 100, 100 | splněno | splněno | 0 → setrvalý, splněno |

  (Sklon přes pořadí = (x3 − x1) / 2; počítá-li se přes skutečné roky monitoringu, vyjde při nestejných intervalech jinak.) **Okno:** „tři poslední monitoringy" pokrývají u intenzivních DP asi 6 let, u extenzivních až asi 24 let. **Jednotka:** Tab. 1 uvádí „počet prýtů", tj. vstup; výsledkem je kategorie trendu. **Méně než tři monitoringy:** neřešeno (R-27).
- **Návrh znění:** text ponechat a vyznačit otázku; v Příloze 1 uvést jednotku „trend (z počtu prýtů)".
- **Otázka pro autory:** Které pravidlo pro vývoj početnosti platí — (A) obě poslední hodnoty jsou ≥ první, (B) nesplněno jen tehdy, jsou-li obě nižší než první, nebo (T) směr trendu (a jak se určí, včetně tolerance pro „setrvalý")? Je okno tří monitoringů přijatelné i u DP extenzivního monitoringu (až asi 24 let)? Jak se indikátor hodnotí, jsou-li k dispozici méně než tři monitoringy?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Závazné je pravidlo z R-07. Čtení (A), (B) a trend (T) se namodelují ve srovnávacím výpočtu Fáze B. DP s řídkým monitoringem se hodnotí stejně v 12letém okně (R-18). **Důsledek k vědomí:** u DP extenzivního monitoringu (interval až 12 let) nebudou v okně zpravidla k dispozici tři předchozí sezóny ⇒ krátkodobý trend neznámý ⇒ DP neznámá (R-27); rozsah změří Fáze B. **Doplněno po Fázi B (2026-10-02):** viz R-07 (dvě sezóny přípustné). Srovnávací výpočet a rozbor příčin jsou v registru Fáze B (§Srovnávací výpočet, §Rozbor příčin).

### R-21 — Nulová početnost: vitalita a poškození nejsou definovány
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** 4.1 — „Vitalita populace: Podíl kvetoucích a plodných prýtů vyjádřený v procentech." / „Poškození rostlin: Podíl poškozených prýtů…"; 5.2 — „V případě tjednoho a více neznámých populačních parametrů nelze hodnocení provést."; 4.2 — „V případě absence druhu na lokalitě se údaje o stanovišti zaznamenají pro pravděpodobnou plochu dřívějšího výskytu."
- **Souvisí:** R-27, R-32; metodika obojživelníků („Přítomnost druhu … Za nepříznivý stav se považuje pouze negativní záznam nebo záznam s počtem 0")
- **Problém:** Při nulové početnosti (druh na DP nezjištěn) jsou oba podíly nedefinované (dělení nulou). Podle 5.2 by pak DP s vymizelou populací byla „nehodnotitelná", nikoli v nepříznivém stavu — přestože početnost populace (0 % počáteční) jasně selhává. Metodika řeší absenci druhu jen u stanovištních údajů (4.2). U obojživelníků tento problém nevzniká, protože přítomnost druhu je samostatný klíčový indikátor.
- **Důsledek:** Bez pravidla by zaniklé populace mohly z hodnocení vypadávat jako „neznámé", a přes pravidlo 5.3 o 50 % populace v DP v neznámém stavu dokonce měnit stav EVL na neznámý.
- **Návrh znění:** vyznačit otázku v 3.1.3 harmonizovaného znění.
- **Otázka pro autory:** Jak se hodnotí DP, na níž byla při monitoringu zjištěna nulová početnost: mají se vitalita a poškození nehodnotit a stav DP určit jen podle početnosti (tj. nepříznivý), nebo je DP ve stavu neznámém? Má se po vzoru obojživelníků zavést indikátor přítomnosti druhu?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Nulová početnost ⇒ **všechny populační indikátory nesplněny** (včetně vitality a poškození); DP tedy není neznámá, ale ve špatném stavu.

### R-22 — Tabulka 1 (kapitola *Aconitum*): jednotky, plocha hodnocení, zápis limitů
- **Typ:** NEJASNOST
- **Závažnost:** mění interpretaci
- **Kde:** Tab. 1 — hydrologické poměry: jednotka prázdná; poškození rostlin: „≤ 50 % prýtů | % počtu prýtů"; pokryvnost expanzivních druhů: „≤ 25 | %t"; vývoj početnosti: „počet prýtů"; 4.2 — „**Hydrologické poměry: Dobré** = … Zhoršené = … **Špatné** = …"; „Pokryvnost dřevin vyšších než 1 m."
- **Souvisí:** seed M-09; R-20, R-23, R-24
- **Problém (seed potvrzen a doplněn):** (1) u hydrologických poměrů chybí jednotka (kategorie); (2) u poškození je „%" v limitu i v jednotce (zdvojení); (3) „%t" je překlep; (4) kategorie hydrologických poměrů jsou zapsány nejednotně (velké písmeno, nejednotné tučné písmo); (5) u vývoje početnosti je jednotkou vstup (počet prýtů), výsledkem je kategorie trendu; (6) u pokryvností není uvedeno, k jaké ploše se procento vztahuje — z 4.2 plyne plocha populace s okolím cca 5 m, ale v Tab. 1 to není; (7) „Pokryvnost dřevin vyšších než 1 m" nemá kromě názvu žádnou definici.
- **Návrh znění:** (1)–(6) opraveno v Příloze 1 harmonizovaného znění (jednotka „kategorie", „% plochy hodnocení", „trend (z počtu prýtů)").
- **Otázka pro autory:** Vztahují se všechny pokryvnosti stanovištních indikátorů k ploše hodnocení stanoviště (populace a okolí cca 5 m)? Jak se pokryvnost dřevin vyšších než 1 m určuje (např. kolmým průmětem korun)?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Pokryvnosti se zaznamenávají v **procentních třídách**; vztahují se k ploše hodnocení podle 4.2 (populace a okolí cca 5 m).

### R-23 — Hydrologické poměry: kategorie „Zhoršené" je příznivá; kolize s názvem stavu
- **Typ:** TERMINOLOGIE
- **Závažnost:** mění interpretaci
- **Kde:** 4.2 — „Hydrologické poměry: Dobré = bez zjevného narušení hydrologických poměrů (lesní prameniště v neovlivněném porostu); Zhoršené = narušené hydrologické poměry bez vlivu na vegetaci (…); Špatné = silně narušené hydrologické poměry (…)"; Tab. 1 — „= Dobré; Zhoršené"
- **Souvisí:** R-10, R-22
- **Problém:** Limit dobrého stavu připouští dvě kategorie, Dobré i Zhoršené — doslovně, a to se nemění. Název kategorie „Zhoršené" se ale shoduje se stavem „zhoršený", takže výstup typu „hydrologické poměry: Zhoršené — indikátor v dobrém stavu" může mást. Názvy kategorií jsou doménou terénního záznamu a neměním je; v textu je jen jasně odlišena kategorie záznamu od stavu indikátoru.
- **Návrh znění:** „Hydrologické poměry se zaznamenávají v kategoriích Dobré / Zhoršené / Špatné. Kategorii terénního záznamu je třeba odlišovat od výsledného stavu indikátoru: indikátor je v dobrém stavu při kategorii Dobré nebo Zhoršené."
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Schváleno.

### R-24 — Doména záznamu pokryvností a poškození; subjektivní vymezení expanzivních druhů
- **Typ:** NEJASNOST
- **Závažnost:** mění interpretaci
- **Kde:** 4.2 — „Přítomnost a pokryvnost expanzivních druhů. Jako expanzivní druhy jsou chápány druhy, které se na lokalitě šíří na úkor monitorovaného druhu a zároveň nejsou považovány za invazní."; Tab. 1 — limity ≤ 75, ≤ 75, = 0, ≤ 25 (%) a „≤ 50 % prýtů"
- **Souvisí:** R-22; seed K-04, K-09 (Fáze B)
- **Problém:**
  1. **Doména záznamu.** Metodika neuvádí, zda se pokryvnosti a poškození zaznamenávají přesným procentem, nebo v třídách. Při třídách nelze limity „≤ 75 %", „≤ 25 %", „≤ 50 %" vyhodnotit jednoznačně, leží-li hranice uvnitř třídy, a „= 0 %" vyžaduje samostatnou třídu „0". Terénní metodika v repozitáři není; ověří se ve Fázi B proti datům.
  2. **Expanzivní druhy** — výběr druhů je ponechán úsudku pozorovatele na každé DP (žádný seznam). Hodnota indikátoru tak závisí na pozorovateli, což jde proti požadavku kap. 2 („co nejméně subjektivní").
  3. **Název** — „Přítomnost a pokryvnost" invazních/expanzivních druhů, ale hodnocena je jen pokryvnost.
- **Návrh znění:** v harmonizovaném znění indikátory pojmenovány podle hodnocené veličiny („pokryvnost invazních druhů") s poznámkou o původním názvu; doména vyznačena otázkou.
- **Otázka pro autory:** Zaznamenávají se pokryvnosti stanovištních indikátorů a poškození rostlin přesným procentem, nebo v třídách (jakých)? Pokud v třídách, jak se vyhodnotí limit, jehož hranice leží uvnitř třídy? Má být pro druh stanoven seznam expanzivních druhů?
- **Rozhodnutí zadavatele:** ⏸ (2026-10-02) Pokryvnosti i poškození se zaznamenávají v **procentních třídách**; soulad hranic tříd s limity ověří Fáze B na datech. **Otevřené:** seznam expanzivních druhů. **Doplněno po Fázi B (2026-10-02):** expanzivní druhy = **Axmanová et al. (2024)**, Katalog expanzivních rostlin ČR (Preslia 96: 299–327). Otevřené: katalog hodnotí expanzivitu po regionech a biotopech — zda platí celý seznam, nebo jen druhy expanzivní v daném regionu / biotopu.

### R-25 — Dvě hranice početnosti: 75 % (indikátor) a 50 % (podmínka dobrého stavu DP)
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** Tab. 1 — „početnost populace | ≥ 75 | % počtu prýtů"; 5.2 — „…a aktuální velikost populace dosahuje alespoň 50 % počáteční početnosti (definice počáteční početnosti viz kapitola 4.1)."
- **Souvisí:** seed M-01; R-06, R-13, R-26
- **Problém (seed upraven — nejde o rozpor, ale o dvě pravidla, jejichž vztah není výslovně popsán):** Doslovně platí obě hranice: indikátor početnost populace je nesplněn pod 75 %; dobrý stav DP navíc vyžaduje ≥ 50 %. Protože 5.2 snese jeden nesplněný klíčový indikátor, dávají spolu smysl — DP s početností 50–75 % může být v dobrém stavu, jsou-li ostatní klíčové indikátory splněny; DP pod 50 % v dobrém stavu být nemůže nikdy. Podmínka 50 % tak plní roli „minimální početnosti" z kap. 2 a 6 (R-13).

  | početnost vůči počáteční | ostatní klíčové indikátory | výsledek podle 5.2 |
  |---|---|---|
  | 80 % | 1 nesplněn | dobrý (1 nesplněný) |
  | 60 % | všechny splněny | dobrý (1 nesplněný = početnost) |
  | 60 % | 1 nesplněn | nepříznivý (2 nesplněné) |
  | 45 % | všechny splněny | nepříznivý (pod 50 %) |

  Ve variantě B Tabulky 1 (princip obojživelníků, R-26) je podmínka 50 % bezpředmětná — každý nesplněný klíčový indikátor tam dává „špatný".
- **Návrh znění:** v Příloze 1 samostatný řádek „minimální početnost (podmínka dobrého stavu DP): ≥ 50 % počáteční početnosti".
- **Otázka pro autory:** Je správný výklad, že limit 75 % platí pro indikátor početnost populace a 50 % je samostatná podmínka dobrého stavu DP (a zároveň „minimální početnost" z kap. 2 a 6)?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Výklad potvrzen. Po přijetí pravidla obojživelníků (R-26) je podmínka 50 % bezpředmětná a z hodnocení DP se **vypouští**.

### R-26 — Úroveň DP: tolerance jednoho nesplněného klíčového indikátoru; chybí stavy zhoršený a špatný
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** 5.2 — „Stav lokality je hodnocen jako příznivý, není-li limitní hodnota příznivého stavu splněna nejvýše pro jeden ze sledovaných populačních (klíčových) parametrů a aktuální velikost populace dosahuje alespoň 50 % počáteční početnosti…"
- **Souvisí:** seed M-02; R-02, R-25, R-27; metodika obojživelníků („Pokud je jeden klíčový populační indikátor hodnocen v nepříznivém stavu, je i druh v rámci DP hodnocen ve špatném stavu. Pokud jsou klíčové (populační) indikátory hodnoceny dobře, ale hodnocení alespoň 2 stanovištních indikátorů je negativní, je celkové hodnocení označeno jako zhoršené.")
- **Problém (seed potvrzen):** 5.2 definuje jen stav „příznivý"; co nastane jinak (zhoršený? špatný?), neříká. Metodika obojživelníků je opačně přísná — jediný nesplněný klíčový indikátor ⇒ špatný — a stav „zhoršený" v ní vzniká jen ze stanovištních indikátorů, které u rostlin podle zápisu N-1 do celkového stavu nevstupují (R-02). **Převzetím Tabulky 1 obojživelníků beze změny by u rostlin stav „zhoršený" na DP nemohl vzniknout vůbec.**
- **Důsledek — příklad** (varianty A a B Tabulky 1 harmonizovaného znění; čtyři klíčové indikátory):

  | DP | klíčové indikátory | početnost vůči počáteční | A (5.2) | B (obojživelníci) |
  |---|---|---|---|---|
  | 1 | všechny splněny | 90 % | dobrý | dobrý |
  | 2 | vitalita nesplněna | 90 % | dobrý | špatný |
  | 3 | početnost nesplněna | 60 % | dobrý | špatný |
  | 4 | vitalita a poškození nesplněny | 90 % | nepříznivý | špatný |
  | 5 | jen početnost nesplněna | 45 % | nepříznivý | špatný |
  | 6 | početnost a trend neznámé (méně než 3 monitoringy), vitalita a poškození splněny | — | neznámý | dobrý |

- **Návrh znění:** Tabulka 1 harmonizovaného znění uvádí obě varianty vedle sebe.
- **Otázka pro autory:** Jaký stav má DP, která nesplní podmínky dobrého stavu podle 5.2 — zhoršený, nebo špatný? Má se mezi nimi rozlišovat (např. podle počtu nesplněných klíčových indikátorů nebo podle podmínky 50 %)? Nebo se má převzít pravidlo obojživelníků (jediný nesplněný klíčový indikátor ⇒ špatný), s vědomím, že pak stav „zhoršený" na úrovni DP nevzniká?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Pravidlo obojživelníků** bez stanovištních indikátorů: jediný nesplněný klíčový indikátor ⇒ špatný; všechny klíčové splněny ⇒ dobrý. Stav zhoršený na úrovni DP vědomě nevzniká.

### R-27 — Neznámý klíčový indikátor: „nelze hodnocení provést" vs. „indikátor bez dat se nehodnotí"
- **Typ:** ROZPOR
- **Závažnost:** mění výsledek
- **Kde:** 5.2 — „V případě tjednoho a více neznámých populačních parametrů nelze hodnocení provést."; metodika obojživelníků, §Hodnocení stavu druhu na úrovni dílčí plochy — „Indikátor se hodnotí pouze, jsou–li dostupné informace k jeho hodnocení."
- **Souvisí:** seed M-03; R-19, R-20, R-21, R-26
- **Problém (seed potvrzen):** Obě pravidla dávají jiný výsledek u každé DP, kde chybí byť jediný klíčový údaj. Podle 5.2 je taková DP ve stavu neznámém (a přes pravidlo 5.3 o 50 % populace v DP v neznámém stavu může vyřadit i celou EVL). Podle principu obojživelníků se DP hodnotí ze zbylých indikátorů. Ve spojení s R-19 a R-20 (početnost i trend jsou známé až od třetího monitoringu) by podle 5.2 nebyla hodnotitelná žádná DP před třetím monitoringem; podle principu obojživelníků by se hodnotila jen z vitality a poškození (příklad DP 6 v R-26). Kde nelze hodnotit žádný klíčový indikátor, metodika obojživelníků stav neřeší (u obojživelníků nastat nemůže, protože přítomnost lze vyhodnotit vždy) — u rostlin je pro determinismus nutné doplnit „neznámý".
- **Návrh znění:** obě varianty vedle sebe (3.1.3 harmonizovaného znění).
- **Otázka pro autory:** Platí pravidlo 5.2 (jediný neznámý klíčový indikátor ⇒ DP se nehodnotí, stav neznámý), nebo princip obojživelníků (indikátor bez dat se nehodnotí, DP se hodnotí ze zbylých indikátorů)? V druhém případě: je DP, u níž nelze hodnotit žádný klíčový indikátor, ve stavu neznámém? A musí být známa alespoň početnost populace?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Varianta A:** jediný neznámý klíčový indikátor ⇒ stav DP neznámý (výjimka: nulová početnost, R-21).

### R-28 — Více návštěv v sezóně: výběr celého záznamu vs. agregace po indikátorech
- **Typ:** ROZPOR
- **Závažnost:** mění výsledek
- **Kde:** 5.2, odst. 2 — „Pokud je v průběhu jedné sezóny lokalita monitorována vícekrát, do hodnocení vstupuje záznam (parametry) s nejvyšší početností populace. Pokud je početnost populace u všech záznamů stejná, do hodnocení vstupuje záznam, s nejstarším datem monitoringu."; metodika obojživelníků — stav populace: „Do celkového hodnocení druhu na DP za daný rok vstupuje nejvyšší pozorovaná hodnota."; stav stanoviště: „Do celkového hodnocení druhu na DP za daný rok vstupuje nejhorší pozorovaná hodnota. Stačí tedy jedno překročení limitní hodnoty ve sledovaném roce a indikátor je hodnocen ve špatném stavu."
- **Souvisí:** seed M-04; R-19
- **Problém (seed potvrzen a upřesněn):** Pravidla dávají jiné výsledky. Příklad — dvě návštěvy DP v jedné sezóně:

  | | návštěva 1 | návštěva 2 | 5.2 (celý záznam s nejvyšší početností) | obojživelníci (po indikátorech) |
  |---|---|---|---|---|
  | početnost | 100 prýtů | 90 prýtů | 100 | 100 |
  | vitalita | 10 % | 40 % | 10 % → nesplněno | 40 % → splněno |
  | poškození | 60 % | 20 % | 60 % → nesplněno | 60 %, nebo 20 %? (viz níže) |
  | hydrologické poměry | Dobré | Špatné | Dobré → splněno | Špatné (nejhorší) → nesplněno |
  | **stav DP (Tab. 1, varianta A)** | | | **nepříznivý** (2 nesplněné) | **dobrý** (0–1 nesplněný) |

  Pravidlo obojživelníků „nejvyšší pozorovaná hodnota" je u poškození nejednoznačné: nejvyšší *podíl poškození* (60 %) je nejhorší stav, nejlepší stav je nejnižší podíl (20 %). U obojživelníků tento případ nenastává (přítomnost, početnost a reprodukce jsou „čím víc, tím lépe"). Pravidlo 5.2 navíc nerozhoduje shodu početnosti **i** data.
- **Návrh znění:** obě varianty vedle sebe (3.1.2 harmonizovaného znění).
- **Otázka pro autory:** Má se při více návštěvách v sezóně vybírat celý záznam s nejvyšší početností (5.2), nebo agregovat po indikátorech (populační = nejlepší, stanovištní = nejhorší pozorovaná hodnota, jako u obojživelníků)? Bere se u poškození rostlin nejnižší (nejlepší), nebo nejvyšší naměřený podíl? Který záznam rozhoduje při shodě početnosti i data?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Agregace po indikátorech:** populační = nejlepší hodnota; stanovištní a poškození = nejhorší hodnota.

### R-29 — Úroveň EVL: dvojstupňové podmínky, stav „špatný" není definován
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** 5.3 — „Stav druhu je hodnocen jako příznivý při splnění následujících podmínek: Celková početnost populace v EVL dosahuje alespoň 95 % cílového stavu pro EVL. Alespoň 75 % populace druhu se nachází v lokalitách v příznivém stavu. Alespoň 50 % lokalit se nachází v příznivém stavu. Stav druhu je hodnocen jako zhoršený při splnění následujících podmínek: … 90 % … 60 % … 40 %…"
- **Souvisí:** seed M-08; R-09, R-30, R-31
- **Problém (seed potvrzen):** Stav „špatný" není výslovně definován; přirozené čtení je „špatný = nesplněna sada podmínek dobrého ani zhoršeného stavu". Že podmínky každé sady musí platit současně, plyne z formulace „při splnění následujících podmínek", výslovně to ale řečeno není. Stav „neznámý" je definován jen pravidlem 50 % populace v DP v neznámém stavu — ne pro případ, kdy chybí cílový stav (R-31).
- **Návrh znění:** Tabulka 2, varianta A: „dobrý — všechny tři indikátory dosahují limitu dobrého stavu; zhoršený — není dobrý a všechny tři dosahují limitu zhoršeného stavu; špatný — jinak."
- **Otázka pro autory:** Platí, že EVL je ve špatném stavu, není-li splněna ani sada podmínek dobrého, ani zhoršeného stavu? Musí být podmínky každé sady splněny všechny současně?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) **Pravidlo obojživelníků** (Tabulka 2, 2×2) — viz R-09; špatný = oba indikátory nesplněny.

### R-30 — Úroveň EVL: výpočet podílu populace a podílu DP v dobrém stavu
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** 5.3 — „Alespoň 75 % populace druhu se nachází v lokalitách v příznivém stavu."; „Alespoň 50 % lokalit se nachází v příznivém stavu."; „Stav druhu v EVL nemůže být hodnocen v případě, že se 50 %, nebo více než 50 % populace druhu nachází v lokalitách v neznámém stavu. Do celkové početnosti druhu v EVL se pro účely hodnocení početnost z lokalit (je-li známa) v neznámém stavu nezapočítává."
- **Souvisí:** R-18, R-29, R-32; metodika obojživelníků („počet DP v dobrém stavu / počet DP v dobrém, zhoršeném či špatném stavu * 100. Lokality, jejichž stav je hodnocen jako neznámý do výpočtu nevstupují.")
- **Problém:**
  1. **Podíl populace** — z jaké početnosti každé DP (poslední monitoring?) a z kterých let (R-18). Podle věty o neznámém stavu se početnost DP v neznámém stavu nezapočítává do celkové početnosti — nevstupuje tedy ani do jmenovatele podílu?
  2. **Podíl DP** — tvoří jmenovatel všechny DP, nebo jen DP se známým stavem (jako u obojživelníků)? V jakém stavu vstupují zaniklé DP započítávané 10 let (R-32)?
  3. **Pravidlo neznámého stavu EVL** potřebuje početnost i u DP v neznámém stavu; je-li neznámá („je-li známa"), nelze podíl spočítat — započte se taková DP nulou, nebo se EVL nehodnotí?
- **Návrh znění:** vzorce v 3.3 harmonizovaného znění; jmenovatel podílu DP převzat z metodiky obojživelníků jako návrh.
- **Otázka pro autory:** Jak se počítá podíl populace v DP v dobrém stavu (z jaké početnosti každé DP a z jakých let)? Vstupují do jmenovatele obou podílů jen DP se známým stavem (jako u obojživelníků)? Jak se uplatní pravidlo „≥ 50 % populace v DP v neznámém stavu ⇒ EVL se nehodnotí", není-li početnost těchto DP známa?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Upřesnění zadavatele (2026-10-02): DP v neznámém stavu **zůstávají ve jmenovateli** podílu DP a počítají se jako DP, které nejsou v dobrém stavu. Pravidlo „≥ 50 % populace v DP v neznámém stavu ⇒ EVL neznámý“ platí dál. Podíl populace v DP v dobrém stavu se vypouští (R-09).

### R-31 — Úroveň EVL: celková početnost vs. cílový stav — zdroj, jednotka, časový základ
- **Typ:** GAP
- **Závažnost:** mění výsledek
- **Kde:** 5.3 — „Celková početnost populace v EVL dosahuje alespoň 95 % cílového stavu pro EVL."; zápis, bod 4 — „Limitní hodnota bude předmětem revize cílového stavu."
- **Souvisí:** R-08, R-09, R-18, R-29; metodika obojživelníků („Limitní hodnotou je v případě hodnocení stavu předmětů ochrany EVL revidovaný cílový stav pro dané území, který je evidován v ISOP." a Tab. 2 „počet jedinců (klouzavý průměr za poslední 3 roky)"); registr obojživelníků H-20, H-23, H-68
- **Problém:** Metodika nestanoví (1) kde je cílový stav EVL evidován (ISOP? SDO?), (2) v jaké jednotce (prýty?), (3) jak se počítá celková početnost (součet posledních monitoringů všech DP? za jaké období? — obojživelníci: součet maxim za rok, klouzavý průměr 3 let), (4) jak se EVL hodnotí, chybí-li cílový stav nebo je v jiné jednotce.
  **Ilustrace z dat (nejde o audit):** ve snímku cílových stavů ze SDO (`sdo_cilove_druhy_20260820.csv`) má jediná EVL druhu (CZ0724089 Beskydy) navrženou hodnotu **600, jednotka „jedinci"**, zatímco text SDO uvádí (rekonstruováno z poškozeného sloupce): „Na lokalitě se v současnosti vyskytuje cca 30–35 000 prýtů. Cílem je toto množství udržet." Jednotka cílového stavu se tedy liší od hodnocené jednotky (prýt) a obě hodnoty se liší o dva řády; sám snímek řádek označuje varováním. Jde o tutéž třídu problému jako u obojživelníků (registr H-68, S-4).
- **Návrh znění:** vyznačit otázku u indikátoru 1 Tabulky 2.
- **Otázka pro autory:** Kde je evidován cílový stav EVL a v jaké jednotce? Jak se počítá celková početnost populace v EVL (součet posledních monitoringů všech DP, roční součet, klouzavý průměr)? Jak se EVL hodnotí, není-li cílový stav stanoven nebo je v jiné jednotce než hodnocená početnost (u *Aconitum*: SDO 600 „jedinců" vs. 30–35 000 prýtů v textu SDO)?
- **Rozhodnutí zadavatele:** ⏸ (2026-10-02) Cílový stav EVL = **navržená hodnota z výsledků digitalizace SDO**. Celková početnost = součet početností z posledního monitoringu každé DP v 12letém okně, bez DP v neznámém stavu (dle 5.3) — **odvozeno z R-18, k potvrzení**. **Otevřené:** hodnocení EVL bez cílového stavu (u *Aconitum* nenastává). Nesoulad jednotek (SDO 600 „jedinců“ vs. prýty) se dokumentuje ve Fázi B. **Doplněno po Fázi B (2026-10-02):** **potvrzeno** — celková početnost EVL je součtem za DP a porovnává se s cílem z digitalizace SDO; u *Aconitum* se cíl 600 „jedinců“ zatím ponechává a nesoulad jednotek se vyznačí (registr H-08). Otevřené: EVL bez cílového stavu (u *Aconitum* nenastává).

### R-32 — Nové a zaniklé DP: nedeterministická pravidla
- **Typ:** NEJASNOST
- **Závažnost:** mění výsledek
- **Kde:** 5.3 — „Nové výskyty druhu se jako samostatné lokality pro účely hodnocení započítávají, pokud splňují požadavky na vymezení nové lokality dle metodiky monitoringu Popelářové et al. (2022)."; „Zaniklé lokality (lokality s dlouhodobou absencí druhu) jsou do celkového hodnocení na úrovni EVL zpravidla započítávány po dobu deseti let od posledního prokazatelného záznamu druhu. Lokality lze do hodnocení započítávat delší dobu, pokud je to relevantní z hlediska opětovného výskytu druhu na lokalitě."
- **Souvisí:** R-15, R-19, R-21, R-30; seed K-10 (Fáze B)
- **Problém:** (1) „zpravidla" a „lze … pokud je to relevantní" — kdo o prodloužení rozhoduje a kde se to eviduje; bez toho nelze pravidlo vyhodnotit automaticky. (2) „Dlouhodobá absence" není časově vymezena. (3) V jakém stavu a s jakou početností zaniklá DP vstupuje do podílů na úrovni EVL — s ohledem na R-21 by byla podle 5.2 „neznámá" a do podílů by nevstoupila, čímž by její desetileté započítávání ztratilo účinek. (4) „Posledního prokazatelného záznamu" — jen z monitoringu, nebo i z jiných dat NDOP (R-15)? (5) Od kdy se hodnotí nová DP, která nemá počáteční početnost (R-19)?
- **Návrh znění:** text ponechat (zobecněno na „metodiku monitoringu daného druhu") a vyznačit otázku.
- **Otázka pro autory:** Kdo a kde rozhoduje o započítávání zaniklé DP déle než 10 let? Po jak dlouhé absenci je DP „zaniklá"? V jakém stavu a s jakou početností zaniklá DP vstupuje do výpočtů na úrovni EVL? Počítá se „prokazatelný záznam" i z jiných dat NDOP než z monitoringu? Od kdy se hodnotí nová DP?
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) O započítávání zaniklé DP déle než 10 let rozhoduje **regionální garant s potvrzením koordinátora monitoringu**. Prokazatelný záznam = jakýkoli záznam přítomnosti v NDOP (R-15). Zaniklá DP vstupuje s výsledkem posledního hodnocení; nulová početnost ⇒ špatný (R-21).

### R-33 — Redakční vady
- **Typ:** REDAKCE
- **Závažnost:** redakční
- **Kde:** celý text
- **Souvisí:** seed M-13 (potvrzeno vše, doplněno)
- **Problém a provedená oprava** (v harmonizovaném znění opraveno rovnou):

  | Kde | Původně | Opraveno |
  |---|---|---|
  | zápis, datum | „13. Srpna 2026" | „13. srpna 2026" |
  | zápis, bod 4 | „Na úrovni EVL  indikátorů" (dvojitá mezera) | jedna mezera |
  | kap. 1 | „poskytnou jednotný postup" | „poskytnout" |
  | kap. 1 | „legislativní ochrany, také povinnost" | bez čárky |
  | kap. 2, nadpis | „parametrů  hodnocení" | jedna mezera |
  | kap. 4 | „koordinován Agentura ochrany přírody" | „Agenturou" |
  | kap. 4 | „prováděn, nebo koordinován" | bez čárky |
  | kap. 6 | „Dalším faktorem stěžující" | „ztěžujícím" |
  | kap. 6 | „sebe lépe navržené parametry, nikdy nedokáží" | „sebelépe navržené indikátory nikdy nedokáží" |
  | kap. 6 | „Kromě vybraných parametrů, je tedy" | bez čárky |
  | kap. 6 | „S přibývajícím množství dat" | „množstvím" |
  | *Aconitum* 1/2 | nadpis „2. Postup prací v terénu" slitý s odstavcem | samostatný oddíl (artefakt převodu) |
  | *Aconitum* 3 | „Popelářová et al. 2022. ." | jedna tečka |
  | *Aconitum* 4.1 | „se se považuje" | „se považuje" |
  | *Aconitum* 4.1 | „V případě relevantních důvodů, může být" | bez čárky |
  | *Aconitum* 4.1 | „údaje ze třech posledních monitoringů" | „na údajích z tří posledních monitoringů" |
  | *Aconitum* 4.1 | „nejsou nižší, než hodnota" | bez čárky |
  | *Aconitum* 4.2 | „ca do vzdálenosti 5 m" | „cca" |
  | *Aconitum* 4.2 | kategorie „**Dobré** / Zhoršené / **Špatné**" (nejednotné tučné písmo) | jednotně |
  | Tab. 1 | „%t" | „%" |
  | *Aconitum* 5.2 | „V případě tjednoho a více" | „jednoho" |
  | *Aconitum* 5.2 | „vstupuje záznam, s nejstarším datem" | bez čárky |
  | literatura | „Lusytk P." | „Lustyk P." |
  | literatura | „management stratégy" | „management strategy" |
  | literatura | rodová jména v názvech prací bez kurzívy | kurzívou |
- **Rozhodnutí zadavatele:** ✅ (2026-10-02) Schváleno.

### R-34 — Chybějící citace „Kolektiv autorů (2016)"
- **Typ:** GAP
- **Závažnost:** redakční
- **Kde:** kap. 2 — „Tvorba a struktura metodik vychází z konceptu metodik Příprava a zavedení sledování stavu předmětů ochrany evropsky významných lokalit (Kolektiv autorů 2016)."
- **Souvisí:** seed M-13
- **Problém (seed potvrzen):** Citace v seznamu literatury chybí. Metodika stanovišť cituje „Kučera T., Čížek L., Hofmeister J., Hošek J., Kloubcová P., Kučerová A., Malíček J., Svoboda T. & Zítková I. (2016): Metodika sledování stavu předmětů ochrany evropsky významných lokalit – Ms., 827 p. [Depon in: Agentura ochrany přírody a krajiny ČR, Praha]" — může jít o tentýž dokument, ověřit to nelze.
- **Návrh znění:** doplnit plnou citaci do Literatury.
- **Otázka pro autory:** Je „Kolektiv autorů (2016)" totožný s prací Kučera et al. (2016), kterou cituje metodika stanovišť? Pokud ne, jaká je plná citace?
- **Rozhodnutí zadavatele:** ⚠ (2026-10-02) Zdroj ssEVL (Kolektiv autorů 2016) v repozitáři **není** (metodika obojživelníků cituje příbuznou „Metodiku sledování stavu EVL (Jeřábková & Fischer 2015)“ bez seznamu literatury) — vyznačeno v textu, citaci doplní autoři.

---

# Souhrn

| Typ \ Závažnost | mění výsledek | mění interpretaci | redakční | celkem |
|---|---|---|---|---|
| ROZPOR | 4 (R-04, R-09, R-27, R-28) | — | — | **4** |
| GAP | 8 (R-05, R-13, R-18, R-19, R-21, R-26, R-29, R-31) | 1 (R-16) | 2 (R-01, R-34) | **11** |
| NEJASNOST | 9 (R-02, R-03, R-06, R-08, R-15, R-20, R-25, R-30, R-32) | 6 (R-07, R-11, R-12, R-17, R-22, R-24) | — | **15** |
| TERMINOLOGIE | — | 2 (R-10, R-23) | — | **2** |
| ZASTARALÉ | — | — | 1 (R-14) | **1** |
| REDAKCE | — | — | 1 (R-33) | **1** |
| **celkem** | **21** | **9** | **4** | **34** |

**Hlavní závěry:**

1. **Úroveň DP nelze bez rozhodnutí autorů jednoznačně vyhodnotit.** Text 2024 (5.2) definuje jen „příznivý" stav a toleruje jeden nesplněný klíčový indikátor; zápis N-1 jednoznačně vyřazuje stanovištní indikátory z celkového stavu. Tabulku 1 obojživelníků proto nelze převzít beze změny — stav „zhoršený" by na DP nevznikal (R-02, R-26).
2. **Pravidlo pro neznámé hodnoty (R-27) rozhoduje o tom, zda se DP do třetího monitoringu vůbec hodnotí** — u extenzivních DP až zhruba 24 let (R-19, R-20).
3. **Zápis z 13. 8. 2026 je v bodech 2–5 neúplný:** nový indikátor „dlouhodobý trend" koliduje názvem i významem se stávajícím (R-04), cílové hodnoty DP neexistují (R-05), „minimi princip" se v metodice stanovišť doslovně nevyskytuje (R-06) a pravidlo N-5 pro EVL by mohlo hodnotit jako „dobrou" EVL s polovičním stavem populace vůči cílovému stavu (R-09).
4. **Cílový stav EVL pro *Aconitum*** je ve snímku SDO veden v jiné jednotce (600 „jedinců") než text SDO (30–35 000 prýtů) i hodnocená jednotka metodiky (R-31).

## Pokrytí seedů zadání

| Seed | Nález | Výsledek ověření |
|---|---|---|
| N-1 | R-02, R-03 | **potvrzeno**; doplněno, že vyřazení stanovištních indikátorů je ve všech čteních jednoznačné |
| N-2 | R-04, R-05, R-06 | **potvrzeno**; upraveno — „minimi princip" se v metodice stanovišť doslovně nevyskytuje, uvedeny tři možné výklady |
| N-3 | R-07 | **potvrzeno** |
| N-4 | R-08 | **potvrzeno** |
| N-5 | R-09 | **potvrzeno**; doplněn příklad, kdy N-5 dává „dobrý" při 50 % cílového stavu |
| N-6 | R-10 | **potvrzeno**; provedeno |
| M-01 | R-25 | **upraveno** — nejde o rozpor, ale o dvě pravidla (75 % indikátor, 50 % podmínka dobrého stavu DP), jejichž vztah je třeba potvrdit |
| M-02 | R-26 | **potvrzeno** |
| M-03 | R-27 | **potvrzeno**; doplněn důsledek pro DP do třetího monitoringu |
| M-04 | R-28 | **potvrzeno**; doplněna nejednoznačnost „nejvyšší hodnoty" u poškození |
| M-05 | R-20 | **upraveno** — čtení seedu („selže, jen když jsou obě nižší") je jedno ze dvou doslovných; s Tab. 1 jsou formulace tři |
| M-06 | R-19 | **potvrzeno a rozšířeno** |
| M-07 | R-13 | **potvrzeno** |
| M-08 | R-29 | **potvrzeno** |
| M-09 | R-22 | **potvrzeno a doplněno** (plocha hodnocení, definice dřevin > 1 m) |
| M-10 | R-11 | **upraveno** — seznam předmětolokalit 2/2025: 36 druhů ve 175 EVL (213 dvojic); 34 druhů je počet v `limity_cevky.csv`, ne v seznamu |
| M-11 | R-01, R-14 | **potvrzeno** |
| M-12 | R-16 | **potvrzeno** |
| M-13 | R-33, R-34 | **potvrzeno**; doplněny další redakční vady |

## Pokrytí oddílů revidovaného textu

| Oddíl | Nálezy |
|---|---|
| Hlavička | R-01 |
| Poznámky z projednání 13. 8. 2026 | R-02 … R-10, R-33 |
| 1. Úvod a cíle | R-10, R-11, R-12, R-33 |
| 2. Principy tvorby metodik a výběru parametrů hodnocení | R-10, R-13, R-33, R-34 |
| 3. Sběr dat v terénu | R-10 (jinak bez věcného nálezu) |
| 4. Ukládání a zdroj dat | R-14, R-15, R-33 |
| 5. Hodnocení stavu | R-16 |
| 6. Slabé stránky | R-13, R-33 |
| *Aconitum* — hlavička, konzultace | R-01 |
| *Aconitum* 1. Výběr a vymezení lokalit | R-10, R-17 |
| *Aconitum* 2. Postup prací v terénu | R-33 (artefakt převodu; jinak bez nálezu) |
| *Aconitum* 3. Frekvence sledování | R-18, R-20, R-33 |
| *Aconitum* 4.1 Stav populace | R-04, R-19, R-20, R-21, R-33 |
| *Aconitum* 4.2 Stav stanoviště | R-17, R-22, R-23, R-24, R-33 |
| *Aconitum* 5.1 a Tab. 1 | R-22, R-24, R-25 |
| *Aconitum* 5.2 | R-21, R-25, R-26, R-27, R-28, R-33 |
| *Aconitum* 5.3 | R-08, R-09, R-29, R-30, R-31, R-32 |
| *Aconitum* 6. Literatura | R-33, R-34 |

## Diagramy

`Diagrams/hodnoceni_urovne.drawio` je koncepční (hodnocení nálezu → dílčí lokalita →
chráněné území → oblast → ČR) a rozhodovací logiku nezobrazuje — harmonizace jej
**nemění**. Rozhodovací stromy `strom_obojzivelnici*.drawio` se týkají jen
obojživelníků. Po rozhodnutí Tabulek 1 a 2 by bylo vhodné vytvořit **nový** strom
pro cévnaté rostliny (obdoba `strom_obojzivelnici_lok.drawio`); to není aktualizace
stávajícího diagramu a není součástí tohoto zadání.

---

# Otázky pro autory

Otázky jsou formulovány tak, aby je bylo možné poslat bez dalšího kontextu. Pojem
„DP" = dílčí plocha, v terénních metodikách monitoringu „lokalita". Odkaz v závorce
vede na nález v této revizi.

### Závěry projednání 13. 8. 2026

1. **Rozdělení indikátorů na DP** (R-02). Zápis uvádí: „k celkovému hodnocení budou přispívat pouze klíčové. Stanovištní indikátory a populační indikátory nepouštět do hodnocení." Populační indikátory jsou ale podle téže věty klíčové. Platí, že (a) všechny populační indikátory jsou klíčové a do celkového stavu DP vstupují, kdežto stanovištní se jen hodnotí a zobrazují; nebo (b) některé populační indikátory jsou neklíčové — a které; nebo (c) něco jiného?
2. **„Zkusit bez poškození"** (R-03). Znamená to (i) přeřadit poškození rostlin mezi neklíčové indikátory, (ii) provést srovnávací výpočet s poškozením a bez něj a rozhodnout podle výsledku, nebo (iii) indikátor vypustit?
3. **Nový indikátor „dlouhodobý trend"** (R-04). Nahrazuje stávající indikátor „početnost populace" (podíl aktuální a počáteční početnosti, limit ≥ 75 %), nebo jej doplňuje? Pokud doplňuje: jak se mají oba jmenovat (návrh konzultanta „početnost populace" koliduje se stávajícím názvem) a je nový indikátor klíčový?
4. **Cílová hodnota pro DP** (R-05). Kde a v jaké jednotce bude cílová hodnota navržená regionálním pracovištěm evidována, kdo ji schvaluje a jak se indikátor hodnotí, dokud pro DP stanovena není?
5. **„Minimi" princip** (R-06). V metodice stanovišť se tento výraz nevyskytuje; nejbližší je minimiareál (minimální rozloha celistvého stanoviště), který se u stanovišť jen sleduje a do celkového hodnocení nevstupuje. Co tedy znamená pro druhy: (A) dolní mez pro cílovou hodnotu DP, (B) minimální velikost populace na DP jako podmínka dobrého stavu, nebo (C) sledovaný ukazatel bez vlivu na celkový stav? Je „aktuální limitní hodnotou indikátoru početnost populace" míněno 75 % počáteční početnosti, nebo 50 %?
6. **„Krátkodobý trend"** (R-07). Neúplná věta zápisu: má se „vývoj početnosti populace" přejmenovat na „krátkodobý trend"? Mění se tím i jeho definice a okno tří posledních monitoringů?
7. **„Celková početnost populace" na úrovni EVL** (R-08). Je to tentýž ukazatel jako dosavadní podmínka „celková početnost populace v EVL dosahuje alespoň 95 % (90 %) cílového stavu"? Pokud ano, zůstávají hranice 95 % / 90 %, nebo se limitem stane přímo revidovaný cílový stav? Pokud ne, čím se liší?
8. **Pravidlo pro EVL „více než jeden / více než dva indikátory ve špatném stavu"** (R-09). Nahrazuje dosavadní dvě sady hranic (dobrý 95/75/50 %, zhoršený 90/60/40 %)? Pokud ano, který limit je u každého indikátoru rozhodující — hodnota pro dobrý, nebo pro zhoršený stav? Je záměr, že EVL s jediným indikátorem ve špatném stavu (např. početnost na 50 % cílového stavu, ostatní v pořádku) vychází jako „dobrá"?

### Obecná část

9. **Počty druhů a EVL v úvodu** (R-11). Odkud a k jakému datu pochází údaj „37 původních druhů cévnatých rostlin přílohy II, pro které je vyhlášeno 221 EVL"? Seznam předmětolokalit Natura 2000 (verze 2/2025) vede 36 druhů ve 175 EVL. Lze údaj nahradit, nebo vypustit?
10. **Účel a rozsah** (R-12). Je tvrzení o povinnosti reportingu stavu „na úrovni jednotlivých EVL" záměrné a na jaký předpis odkazuje? Lze účel formulovat jako v metodice obojživelníků (podklad pro zprávy podle čl. 17 směrnice, péči a standardní datové formuláře)? Vztahuje se metodika i na předměty ochrany maloplošných chráněných území, nebo jen na EVL?
11. **Minimální početnost** (R-13). Obecná část uvádí, že „pro každý druh [je] stanovena také minimální početnost druhu na lokalitě, při které není možné lokalitu hodnotit pozitivně". U oměje tuhého moravského žádná absolutní hodnota uvedena není. Je jí míněna podmínka „aktuální velikost populace dosahuje alespoň 50 % počáteční početnosti", nebo má mít každý druh absolutní minimum (a jaké má oměj)? Je správně „pod kterou" místo „při které"?
12. **Data mimo monitoring** (R-15). Text uvádí, že jiná data z NDOP lze zahrnout „pouze jako prezence druhu na lokalitě". K čemu prezence slouží — k určení posledního prokazatelného záznamu u zaniklých DP, k vymezení nových DP, nebo k něčemu jinému? Rozumí se „monitoringem" (poslední monitoring, první tři monitoringy) výhradně sběr dat v projektu Monitoring druhů ČR?
13. **Obecná kapitola o vyhodnocení** (R-16). Kapitola 5 obecné části je prázdná. Souhlasíte, aby ji tvořila pravidla z kapitoly oměje (5.1–5.3) a aby principy metodiky obojživelníků byly uvedeny jen jako varianty k rozhodnutí?
14. **Obecná vs. druhová pravidla** (R-17). Navrhujeme jako obecná pro všechny druhy vést: pravidla 5.2 a 5.3 bez číselných hodnot, definici počáteční početnosti, plochu hodnocení stanoviště (populace a okolí cca 5 m) a obecné definice stanovištních indikátorů; jako druhově specifické: všechny číselné limity (včetně hranic na úrovni EVL a minimální početnosti), hodnocenou jednotku (prýt), kategorie hydrologických poměrů a výjimku *Arrhenatherum elatius* u invazních druhů. Souhlasíte? Jsou hranice na úrovni EVL společné všem druhům?
15. **Hodnocené období** (R-18). Co je „hodnocené období"? Hodnotí se stav DP pro každý rok s monitoringem zvlášť? Které záznamy jednotlivých DP vstupují do hodnocení EVL — vždy poslední monitoring každé DP bez ohledu na jeho stáří (u extenzivního monitoringu až 12 let), nebo jen monitoring v určitém okně?

### Úroveň dílčí plochy

16. **Počáteční početnost** (R-19). Kdo a kde eviduje arbitrárně stanovenou počáteční početnost a z jakých důvodů ji lze stanovit? Počítá se průměr z prvních tří monitoringů podle terénní metodiky, nebo z prvních tří záznamů vůbec? Jak se DP hodnotí, dokud tři monitoringy neexistují (u extenzivního monitoringu až zhruba 24 let)? Je počáteční početnost po stanovení pevná? Jak postupovat, vyjde-li 0?
17. **Vývoj početnosti** (R-20). Text obsahuje tři různá pravidla: (A) obě hodnoty z posledních dvou monitoringů jsou alespoň rovny hodnotě z prvního; (B) indikátor selže, jen jsou-li obě nižší; (T) trend není sestupný. Například řada 100 → 120 → 90 prýtů je podle (A) nesplněna, podle (B) splněna a podle sklonu regrese nesplněna. Které pravidlo platí (a u trendu: jak se určí a jaká je tolerance pro „setrvalý")? Je okno tří monitoringů přijatelné i u extenzivního monitoringu? Jak se indikátor hodnotí při méně než třech monitorinzích?
18. **Nulová početnost** (R-21). Je-li na DP zjištěna nulová početnost, nelze spočítat podíl kvetoucích ani poškozených prýtů. Podle textu 5.2 („jeden a více neznámých parametrů ⇒ nelze hodnotit") by DP s vymizelou populací byla „neznámá", nikoli v nepříznivém stavu. Jak se má taková DP hodnotit? Má se po vzoru obojživelníků zavést indikátor přítomnosti druhu?
19. **Plocha a způsob hodnocení pokryvností** (R-22). Vztahují se všechny pokryvnosti stanovištních indikátorů k ploše populace a jejímu okolí do cca 5 m? Jak se určuje pokryvnost dřevin vyšších než 1 m (např. kolmým průmětem korun)?
20. **Doména terénního záznamu** (R-24). Zaznamenávají se pokryvnosti a poškození rostlin přesným procentem, nebo v třídách (jakých)? Pokud v třídách, jak se vyhodnotí limit, jehož hranice leží uvnitř třídy (např. ≤ 25 %)? Má být pro druh stanoven seznam expanzivních druhů, aby hodnota nezávisela na pozorovateli?
21. **Hranice 75 % a 50 %** (R-25). U početnosti populace je limit indikátoru ≥ 75 % počáteční početnosti a podmínka dobrého stavu DP ≥ 50 %. Je správný výklad, že jde o dvě pravidla — DP s 60 % může být v dobrém stavu, jsou-li ostatní klíčové indikátory splněny, DP pod 50 % nikdy?
22. **Stavy DP mimo „příznivý"** (R-26). Text 5.2 definuje jen příznivý stav (nejvýše jeden nesplněný klíčový indikátor a alespoň 50 % počáteční početnosti). Jaký stav má DP, která tyto podmínky nesplní — zhoršený, nebo špatný, a podle čeho se mezi nimi rozhodne? Nebo se má převzít pravidlo obojživelníků (jediný nesplněný klíčový indikátor ⇒ špatný)? Pak by stav „zhoršený" na DP nevznikal, protože stanovištní indikátory do celkového stavu nevstupují.
23. **Chybějící údaj** (R-27). Platí pravidlo 5.2 (jediný neznámý klíčový indikátor ⇒ DP se nehodnotí), nebo princip obojživelníků (indikátor bez dat se nehodnotí, DP se hodnotí ze zbylých)? V druhém případě: je DP bez jediného hodnotitelného klíčového indikátoru ve stavu neznámém? Musí být známa alespoň početnost populace?
24. **Více návštěv v sezóně** (R-28). Vybírá se celý záznam s nejvyšší početností (5.2), nebo se agreguje po indikátorech (populační = nejlepší, stanovištní = nejhorší hodnota, jako u obojživelníků)? Obojí může dát opačný stav DP. Bere se u poškození nejnižší, nebo nejvyšší naměřený podíl? Který záznam rozhoduje při shodě početnosti i data?

### Úroveň EVL

25. **Stav „špatný"** (R-29). Platí, že EVL je ve špatném stavu, není-li splněna ani sada podmínek dobrého (95/75/50 %), ani zhoršeného (90/60/40 %) stavu? Musí být podmínky každé sady splněny všechny současně?
26. **Výpočet podílů** (R-30). Z jaké početnosti každé DP (a z jakých let) se počítá podíl populace v DP v dobrém stavu? Vstupují do jmenovatele podílu populace i podílu DP jen DP se známým stavem (jako u obojživelníků)? Jak se uplatní pravidlo „50 % a více populace v DP v neznámém stavu ⇒ EVL se nehodnotí", není-li početnost těchto DP známa?
27. **Cílový stav EVL** (R-31). Kde je cílový stav EVL evidován a v jaké jednotce? Jak se počítá celková početnost populace v EVL (součet posledních monitoringů všech DP, roční součet, klouzavý průměr)? Jak se EVL hodnotí, není-li cílový stav stanoven nebo je v jiné jednotce? U oměje uvádí standardní datový formulář 600 „jedinců", jeho text však „cca 30–35 000 prýtů; cílem je toto množství udržet".
28. **Nové a zaniklé DP** (R-32). Kdo a kde rozhoduje o započítávání zaniklé DP déle než 10 let? Po jak dlouhé absenci je DP „zaniklá"? V jakém stavu a s jakou početností zaniklá DP vstupuje do výpočtů na úrovni EVL? Počítá se „prokazatelný záznam" i z jiných dat NDOP než z monitoringu? Od kdy se hodnotí nová DP, která ještě nemá počáteční početnost?

### Redakční

29. **Hlavička** (R-01). Jaké jsou afiliace autorů a jaké číslo a datum má nést nová verze?
30. **Citace** (R-34). Je „Kolektiv autorů (2016)" totožný s prací Kučera T. et al. (2016): Metodika sledování stavu předmětů ochrany evropsky významných lokalit, kterou cituje metodika stanovišť? Pokud ne, jaká je plná citace?

---

# Mapa změn

`původní oddíl → nový oddíl → typ`. Původní oddíly odkazují na
`met_cevnate_2026_for_review.md` (obecná část kap. 1–6, kapitola *Aconitum* 1–6),
nové na `met_cevnate_2026_harmonizovana.md`. Ve všech oddílech je navíc provedeno
sjednocení terminologie R-10 a redakční opravy R-33; v tabulce se uvádějí jen tam,
kde jsou jediným důvodem řádku.

| Původní oddíl | Nový oddíl | Typ |
|---|---|---|
| Hlavička (název, autoři, konzultace, verze) | Hlavička | přeformulování; oprava R-01 |
| — | Hlavička, rámeček „Stav dokumentu" | nové — vysvětlení značek (nejde o obsah metodiky) |
| — | Věta „Součástí metodiky je Příloha 1 … Příloha 2 …" | návrh převzatý z obojživelníků (struktura) |
| Poznámky z projednání, bod 1 (rozdělení indikátorů) | 2.1 úvod; 2.1 Stav stanoviště; 3.2; Příloha 1A sloupec „Klíčový" | ze zápisu N-1 (R-02) |
| Poznámky z projednání, bod 1 („Zkusit bez poškození") | 2.1 Poškození rostlin; Příloha 1A | ze zápisu N-1 (R-03) |
| Poznámky z projednání, bod 2 (dlouhodobý trend, cílová hodnota, minimi) | 2.1 Početnost populace a Dlouhodobý trend; 3.1.4; Příloha 1A | ze zápisu N-2 (R-04, R-05, R-06) |
| Poznámky z projednání, bod 3 (krátkodobý trend) | 2.1 Vývoj početnosti; Příloha 1A | ze zápisu N-3 (R-07) |
| Poznámky z projednání, bod 4 (celková početnost na úrovni EVL) | 3.3 indikátor 1; Příloha 1B | ze zápisu N-4 (R-08) |
| Poznámky z projednání, bod 5 (pravidlo počtu špatných indikátorů) | 3.3 Tabulka 2, varianta B | ze zápisu N-5 (R-09) |
| Poznámky z projednání, bod 6 (parametr → indikátor) | celý text | ze zápisu N-6 (R-10) |
| Kap. 1 Úvod a cíle | 1, odst. 1–2 | přesun; čísla ponechána s otázkou R-11, R-12 |
| — | 1, odst. 3 (účel: zprávy podle čl. 17, péče, SDF) | návrh převzatý z obojživelníků, §Cíl sledování a metodiky vyhodnocování (R-12) |
| Kap. 2 Principy tvorby metodik a výběru parametrů hodnocení | 1, odst. 4–5 | přesun (R-13, R-34) |
| Kap. 3 Sběr dat v terénu | 2, úvodní odstavec | přesun; doplněn odkaz na druhový list |
| Kap. 4 Ukládání a zdroj dat | 2.4 | přesun; oprava R-14 (Field Maps); otázka R-15 |
| Kap. 5 Hodnocení stavu (prázdná) | 3 (úvod), 3.1 | oprava R-16 — obsah z kapitoly *Aconitum* 5.1–5.3 |
| — | 3, úvod (indikátory ve dvou skupinách, každý hodnocen samostatně) | návrh převzatý z obojživelníků, §Vyhodnocení stavu |
| — | 3, věta o stavech dobrý / zhoršený / špatný / neznámý | návrh převzatý z obojživelníků, Tabulka 1 a 2 (R-10, R-26) |
| Kap. 6 Slabé stránky | 3.4 | přesun |
| *Aconitum* — hlavička, „Konzultace: Martin Kočí" | Hlavička; Příloha 2 | přesun |
| *Aconitum* 1. Výběr a vymezení lokalit | 2.2 (obecně, „metodikou monitoringu daného druhu"); Příloha 2 Vymezení DP | přesun + přeformulování (zobecnění, R-17) |
| *Aconitum* 2. Postup prací v terénu | Příloha 2 Terénní metodika | přesun |
| *Aconitum* 3. Frekvence sledování | 2.3 (první věta, obecně); Příloha 2 Frekvence sledování | přesun + přeformulování (R-17, R-18) |
| *Aconitum* 4.1 Početnost populace — definice | 2.1 Početnost populace | přesun + přeformulování (jednotka „dle Přílohy 1") |
| *Aconitum* 4.1 Početnost populace — počáteční početnost | 3.1.4 | přesun (R-19) |
| *Aconitum* 4.1 Vývoj početnosti populace | 2.1 Vývoj početnosti; Příloha 1A | přesun (R-07, R-20) |
| *Aconitum* 4.1 Vitalita populace | 2.1 (obecně „hodnocených jednotek"); Příloha 2 (prýty); Příloha 1A | přesun + přeformulování (R-17) |
| *Aconitum* 4.1 Poškození rostlin | 2.1 (obecně); Příloha 2 (prýty); Příloha 1A | přesun + přeformulování (R-03, R-17) |
| *Aconitum* 4.2 úvod (plocha hodnocení ~5 m, absence druhu) | 2.1 Stav stanoviště druhu | přesun (R-17, R-22) |
| *Aconitum* 4.2 Hydrologické poměry | 2.1 (obecně: kategorie); Příloha 2 (vymezení kategorií); Příloha 1A (limit) | přesun + přeformulování (R-23) |
| *Aconitum* 4.2 Pokryvnost E1 | 2.1 | přesun |
| *Aconitum* 4.2 Pokryvnost dřevin vyšších než 1 m | 2.1 | přesun (R-22) |
| *Aconitum* 4.2 Přítomnost a pokryvnost invazních druhů | 2.1 (definice podle Pergl 2016 a Pyšek 2022); Příloha 2 (výjimka *Arrhenatherum elatius*) | přesun + rozdělení (R-17, R-24) |
| *Aconitum* 4.2 Přítomnost a pokryvnost expanzivních druhů | 2.1 | přesun (R-24) |
| *Aconitum* 5.1 věta „Všechny uvedené limitní hodnoty jsou expertními odhady…" | 3, úvod | přesun |
| *Aconitum* 5.1 Tab. 1 (limity) | Příloha 1A | přesun; oprava R-22 (jednotky, plocha hodnocení, „%t") |
| *Aconitum* 5.2, odst. 1 (podmínka příznivého stavu) | 3.2, varianta A a Tabulka 1 — varianta A; Příloha 1A řádek „minimální početnost" | přesun + přeformulování (R-25, R-26) |
| — | 3.2, varianta B a Tabulka 1 — varianta B | návrh převzatý z obojživelníků, Tabulka 1 a §Hodnocení stavu druhu na úrovni DP (R-26) |
| *Aconitum* 5.2, věta o neznámých parametrech | 3.1.3, varianta A | přesun (R-27) |
| — | 3.1.3, varianta B | návrh převzatý z obojživelníků, §Hodnocení stavu druhu na úrovni DP (R-27) |
| *Aconitum* 5.2, odst. 2 (více návštěv v sezóně) | 3.1.2, varianta A | přesun (R-28) |
| — | 3.1.2, varianta B | návrh převzatý z obojživelníků, §Hodnocení stavu druhu na úrovni DP (R-28) |
| — | 3.1.1 (DP × rok) | návrh převzatý z obojživelníků, §Hodnocení stavu druhu na úrovni DP (R-18) |
| *Aconitum* 5.3 podmínky příznivého a zhoršeného stavu | 3.3 indikátory 1–3, Tabulka 2 — varianta A; Příloha 1B (hranice) | přesun + přeformulování (R-29, R-17) |
| — | 3.3 indikátor 3, jmenovatel podílu DP | návrh převzatý z obojživelníků, §Hodnocení stavu druhu na úrovni sledovaného území (R-30) |
| *Aconitum* 5.3 EVL s lokalitami v neznámém stavu | 3.3 „EVL s DP v neznámém stavu" | přesun (R-30, R-31) |
| *Aconitum* 5.3 Nově vzniklé lokality | 3.3 „Nově vzniklé DP" | přesun + přeformulování (zobecnění na „metodiku monitoringu daného druhu", R-32) |
| *Aconitum* 5.3 Zaniklé lokality | 3.3 „Zaniklé DP" | přesun (R-32) |
| *Aconitum* 6. Doporučená a použitá literatura | Literatura; Příloha 2 Specifická literatura | přesun; oprava R-33; doplněn řádek Kolektiv autorů (2016) s otázkou R-34 |

## Odchylky od cílové struktury zadání (§4.2)

| Odchylka | Zdůvodnění |
|---|---|
| **Příloha 1 má dvě části** — 1A (úroveň DP) a 1B (úroveň EVL) | Hranice úrovně EVL (95/75/50 %, 90/60/40 %) jsou ve verzi 2024 zapsány v kapitole druhu, mohou tedy být druhově specifické (R-17). V obecné Tabulce 2 by takové číslo porušilo požadavek, aby obecná část fungovala pro všechny druhy. |
| **Oddíl 3.1 je členěn na 3.1.1–3.1.4** | Každé obecné pravidlo má vlastní varianty a otázky; členění drží varianty od sebe. |
| **„Tabulka 1" znamená rozhodovací tabulku DP** (dle vzoru obojživelníků), nikoli tabulku limitů jako ve verzi 2024 | Limity z původní Tab. 1 jsou v Příloze 1A. Při citaci verze 2024 se proto píše „Tab. 1 (verze 2024)". |
| **Zápis z projednání není v textu jako samostatný blok** | Každý bod je rozpuštěn do oddílu, kterého se týká, se značkou [ZÁPIS N-x]; dohledatelnost zajišťuje tato mapa. |
| **Řádek „minimální početnost" v Příloze 1A** | Kap. 2 a 6 uvádějí, že minimální početnost je stanovena „pro každý druh" — patří tedy do druhové matice (R-13, R-25). |

---

# Rozhodnutí zadavatele — souhrn (2026-10-02)

Odpovědi zadavatele na otázky R-01 … R-34 a navazující upřesnění k R-04/05/06/13/19
(úroveň referenčních hodnot), R-08/09/29 (struktura úrovně EVL), R-07/20 (krátkodobý
trend) a R-30 (DP v neznámém stavu). Výsledná pravidla, na nichž stojí harmonizované
znění a audit Fáze B:

| Oblast | Platné pravidlo | Nálezy |
|---|---|---|
| Rozsah | jen EVL (Natura 2000); zatím jen *Aconitum firmum* subsp. *moravicum*; obecná část odvozena z jeho kapitoly | R-11, R-12, R-17 |
| Zdroj dat | přítomnost (vč. posledního prokazatelného záznamu) ze všech záznamů NDOP; ostatní indikátory jen z projektu „Monitoring druhů ČR“ | R-15 |
| Okno | do hodnocení DP i EVL vstupují záznamy z posledních 12 let; DP se hodnotí pro každou sezónu | R-18, R-20 |
| Klíčové indikátory DP | velikost populace (≥ 75 % počáteční početnosti DP), krátkodobý trend populace (sezóna ≥ průměr tří předchozích hodnocených sezón), vitalita populace (≥ 30 %) | R-02, R-04, R-07 |
| Neklíčové (zobrazované) | poškození rostlin; všechny stanovištní indikátory | R-02, R-03 |
| Více návštěv v sezóně | po indikátorech: populační nejlepší, stanovištní a poškození nejhorší | R-28 |
| Chybějící údaj | jediný neznámý klíčový indikátor ⇒ DP neznámá | R-27 |
| Nulová početnost | všechny populační indikátory nesplněny ⇒ DP špatná | R-21 |
| Tabulka 1 (DP) | všechny klíčové splněny ⇒ dobrý; některý nesplněn ⇒ špatný; zhoršený nevzniká; podmínka 50 % vypuštěna | R-25, R-26 |
| Úroveň EVL | dva indikátory: podíl DP v dobrém stavu (≥ 50 %; neznámé DP ve jmenovateli jako „ne dobré“) a dlouhodobý trend populace (celková početnost ≥ 75 % cílového stavu podle digitalizace SDO) | R-04 … R-09, R-13, R-30, R-31 |
| Tabulka 2 (EVL) | 2×2 jako u obojživelníků: oba splněny = dobrý, jeden = zhoršený, žádný = špatný | R-09, R-29 |
| Neznámý stav EVL | ≥ 50 % populace v DP v neznámém stavu | R-30 |
| Zaniklé DP | 10 let od posledního prokazatelného záznamu; prodloužení: regionální garant s potvrzením koordinátora monitoringu | R-32 |
| Záznam pokryvností a poškození | procentní třídy | R-22, R-24 |

**Zbývá otevřené (nebrání Fázi B):** *(aktualizováno po Fázi B, 2026-10-02)*

| # | Bod | Nález |
|---|---|---|
| 1 | ~~Kdo stanoví arbitrární počáteční početnost~~ — samostatný skript „maximum kdy zaznamenané" (mimo zadání); **zbývá** postup při počáteční početnosti 0 | R-19 |
| 2 | ~~Seznam expanzivních druhů~~ — Axmanová et al. (2024); **zbývá** regionální / biotopová platnost katalogu | R-24 |
| 3 | ~~Součet celkové početnosti EVL~~ — potvrzeno; **zbývá** EVL bez cílového stavu | R-31 |
| 4 | Plná citace „Kolektiv autorů (2016)" — v repozitáři není | R-34 |

**Důsledky k vědomí** (rozsah změří Fáze B):

- U DP extenzivního monitoringu nebudou v 12letém okně zpravidla k dispozici tři
  předchozí sezóny pro krátkodobý trend, ani tři monitoringy pro počáteční početnost
  ⇒ podle R-27 budou tyto DP ve stavu neznámém.
- Cílový stav *Aconitum* podle digitalizace SDO je 600 „jedinců“, text SDO uvádí
  30–35 000 prýtů; hodnoceno je v prýtech.

## Mapa změn — doplněk po rozhodnutí zadavatele

| Oddíl harmonizovaného znění (verze z Fáze A) | Změna | Rozhodnutí |
|---|---|---|
| Hlavička | afiliace AOPK ČR; nová značka [ROZHODNUTÍ R-xx]; poznámka o rozsahu (jen *Aconitum*) | R-01, R-11, R-17 |
| 1, odst. 2–3 | čísla vyznačena jako neověřená; doplněna věta o rozsahu (jen EVL); navržená věta z obojživelníků vypuštěna | R-11, R-12 |
| 1, odst. 5 a 3.4 | minimální početnost přeformulována na úroveň EVL (75 % cílového stavu) | R-13 |
| 2.1 | „početnost populace“ → „velikost populace“; „vývoj početnosti populace“ → „krátkodobý trend populace“ s novou definicí; poškození neklíčové; dlouhodobý trend přesunut do 3.3; záznam pokryvností v procentních třídách | R-02 … R-07, R-22, R-24 |
| 2.3, 3.1.1 | 12leté okno | R-18 |
| 2.4 | pravidlo zdroje dat (přítomnost vs. ostatní indikátory) | R-15 |
| 3.1.2, 3.1.3 | ponechána jediná varianta; doplněno pravidlo nulové početnosti | R-21, R-27, R-28 |
| 3.1.4 | cílová hodnota DP zrušena; cílový stav EVL a minimální početnost z digitalizace SDO | R-05, R-06, R-13, R-19, R-31 |
| 3.2 | Tabulka 1: jediná varianta (pravidlo obojživelníků); podmínka 50 % vypuštěna | R-25, R-26, R-27 |
| 3.3 | dva indikátory, Tabulka 2 2×2; „podíl populace v DP v dobrém stavu“ vypuštěn; neznámé DP ve jmenovateli; zaniklé DP — kdo rozhoduje | R-08, R-09, R-29, R-30, R-31, R-32 |
| Příloha 1A | řádky „dlouhodobý trend“ a „minimální početnost“ odstraněny; poškození neklíčové; limit krátkodobého trendu | R-03, R-05, R-07, R-25 |
| Příloha 1B | dva indikátory s jediným limitem (≥ 50 %, ≥ 75 % cílového stavu) | R-06, R-09 |

---

# Verze 2026-10-21 (Popelka et al.) — kontrolní seznam pravidel a nálezy ve zdrojích

**Zadání:** [prompt_host_n2kcz_cevnate_review.md](prompt_host_n2kcz_cevnate_review.md), fáze 1.
**Zdroj:** [Metodika hodnocení stavu předmětů ochrany cevnate 20261021.docx](Metodika%20hodnocení%20stavu%20předmětů%20ochrany%20cevnate%2020261021.docx)
(O. Popelka, spolupráce J. Gaigr, M. Vojík, I. Hönigová, D. Leština, P. Kuncová; verze 2026)
a [Tabulky fin.xlsx](Tabulky%20fin.xlsx) (Přílohy 1–5).
**Datum:** 2026-10-28 · **Větev:** `cevnate-metodika-2026` (odbočena z `harmonizace-cevnate`).

Nová verze metodiky je **autorský text**, který nahrazuje dosavadní harmonizované znění
([met_cevnate_2026_harmonizovana.md](met_cevnate_2026_harmonizovana.md)) jako měřítko pro kód.
Mění rozsah (všech 36 druhů, EVL i MZCHÚ), sadu indikátorů i rozhodovací pravidla.
Rozhodnutí zadavatele z 2026-10-02 (výše) proto platí jen tam, kde je nový text nepřekrývá —
kde ano, je to uvedeno u pravidla.

## Jak byla revize provedena

| Krok | Postup |
|---|---|
| Text | `.docx` převeden pandokem (z instalace Quarta) dvakrát: `--track-changes=all` (s komentáři a vloženými úseky) a `--track-changes=accept`. Dokument obsahuje **23 komentářů** (Popelka, Hönigová) a vložené úseky (indikátor *Délka absence*, slovo „maximální“ u pokryvnosti dřevin). Komentáře jsou číslovány interními ID Wordu (c2 … c50); stejná čísla používá zadání. |
| Tabulky | `readxl` po buňkách (`col_names = FALSE`, `trim_ws = FALSE`), sloupce hledány podle textu hlavičky, ne podle pořadí. List *Příloha 3* má 37 sloupců a 17 řádků indikátorů. |
| Data | Neveřejný export NDOP (`export_data_evl.csv` + `export_data_zprap.csv`, cévnaté rostliny 41 290 záznamů, z toho projekt Monitoring druhů ČR 8 984) — jen ke kontrole slovníku hodnot; nic se nehodnotilo. |

## Kontrolní seznam pravidel (PR-xx)

Každé pravidlo, které musí kód implementovat. Verdikty auditu kódu jsou v
[harmonizace_registr.md](harmonizace_registr.md), Fáze C, matice PR-xx.

| # | Pravidlo | Kde v textu | Výklad / poznámka |
|---|---|---|---|
| PR-01 | Do hodnocení vstupují jen data z monitoringu druhů (NDOP, projekt *Monitoring druhů ČR*); ostatní záznamy NDOP jen dokládají prezenci. | kap. 3 | Shodné s rozhodnutím R-15. |
| PR-02 | Informativní indikátor **Poslední doložený výskyt**: datum posledního doloženého výskytu z NDOP, všechny projekty; jen nálezy garantované jako věrohodné, přesnost lokalizace ≤ 1 km; přiřazení protnutím zákresu lokality a nálezu, u polygonů a linií překryv ≥ 10 %. | kap. 7.1 | Nový indikátor (komentář c30); buffer pro body/linie otevřený (c31). |
| PR-03 | Do hodnocení vstupují jen **validní** záznamy monitoringu (nevalidní např. po pokosení). | kap. 7 | Mechanismus označení neurčen (c17). |
| PR-04 | Více návštěv lokality v jednom roce: vstupuje **celý záznam s nejvyšší početností**; při shodě záznam s **nejstarším datem**. | kap. 7 | Vrací pravidlo verze 2024 a **nahrazuje rozhodnutí R-28** (agregace po indikátorech). |
| PR-05 | Relativní stupnice početnosti → **medián kategorie** (1–10 = 5,5; 11–100 = 55,5; 101–1 000 = 550,5; 1 001–10 000 = 5 500,5; 10 001–100 000 = 55 000,5; 100 001–1 000 000 = 550 000,5; > 1 000 000 = 1 000 001); povinně u *Coleanthus subtilis* a *Campanula bohemica*. | kap. 7.1 | Překlepy v textu viz R-38. |
| PR-06 | Každý druh má **hlavní populační jednotku** (Příloha 1). | kap. 7.1 | Rozhodující je Příloha 1 (rozhodnutí 2026-10-28). |
| PR-07 | **Cílová početnost** lokality = průměr početností z **prvních tří monitoringů**; expertně ji lze stanovit při rozdělení/sloučení lokalit, dlouhodobě vyšší nebo neovlivnitelně nižší početnosti. | kap. 7.1 | Nahrazuje „počáteční početnost“ (R-19) i návrh „maximum kdy zaznamenané“ (odpověď 7, 2026-10-02) — k potvrzení, viz registr. |
| PR-08 | **Minimální přípustná početnost** (klíčový, všechny druhy kromě *Coleanthus*) = aktuální (poslední monitoring) / cílová × 100 ≥ limit Přílohy 2. | kap. 7.1 | Komentář c29: dříve podmínka hodnocení lokality, nyní indikátor. |
| PR-09 | **Početnost** (neklíčový) = tentýž poměr ≥ vyšší limit Přílohy 2. | kap. 7.1 | Stejná veličina jako PR-08, jiný práh. |
| PR-10 | **Vývoj početnosti** (neklíčový): tři poslední údaje T1–T3; negativní, jsou-li T2 i T3 < T1; limit „pozitivní; setrvalý“. | kap. 7.1 | Výklad R-44. |
| PR-11 | **Vitalita** (neklíčový) = kvetoucí a plodné / **sterilní** populační jednotky × 100 ≥ limit. | kap. 7.1 | Rozhodnutí 2026-10-28: podle textu (R-43). |
| PR-12 | **Poškození rostlin** (neklíčový; u *Coleanthus* jen informativní) = **součet** měr poškození všech zjištěných typů; čtyřčlenná stupnice → mediány 5,5 / 30,5 / 75,5; limit ≤ 40 %. | kap. 7.1 | Rozhoduje otevřenou otázku H-12 (součet, ne maximum). |
| PR-13 | **Úspěšnost reprodukce** (*Coleanthus*, klíčový): dostatečná, existuje-li v posledních 6 letech pozitivní záznam monitoringu s > 100 jedinci a podílem odplozených ≥ 11 %. | kap. 7.1 | Výjimka z pravidla neznámého stavu (komentář c19); vstupní data viz R-50. |
| PR-14 | **Délka absence druhu** (*Coleanthus*, klíčový): < 6 let; hodnotitelná jen při ≥ 2 negativních záznamech monitoringu v posledních 6 letech a žádném jiném doloženém výskytu v NDOP, jinak neznámá. | kap. 7.1 | Vložený úsek; c23/c24 otevřené (R-40). |
| PR-15 | Indikátor je **neznámý**, chybí-li kterákoli vstupní (primární) hodnota. | kap. 7 | Komentář c18 („neznámé jsme neřešili“). |
| PR-16 | **Stanovištní indikátory jsou jen informativní** — hodnotí se a zobrazují, do verdiktu lokality nevstupují; plocha populace + ~5 m; limity = výčet přípustných kategorií (Příloha 3). | kap. 2, 7, 7.2 | Rozhodnutí 2026-10-28: `KLIC = info`. |
| PR-17 | **Verdikt lokality: dobrý / špatný / neznámý** (zhoršený nevzniká). Dobrý = klíčový indikátor v dobrém stavu **a** nejvýše jeden neklíčový ve špatném; neznámý = klíčový neznámý **nebo** více než jeden neklíčový neznámý. | kap. 2, 7 | Přednost neznámého před špatným nejasná (R-45). |
| PR-18 | **Verdikt území (EVL, MZCHÚ): dobrý / zhoršený / špatný** podle Tabulky 1 (*Coleanthus*: podíl lokalit v dobrém stavu + cílový stav) a Tabulky 2 (ostatní: podíl populace v lokalitách v dobrém stavu + podíl lokalit v dobrém stavu + cílový stav). | kap. 8 | Rozhodnutí 2026-10-28: jediný práh na indikátor (R-46). |
| PR-19 | **Celková početnost** území = součet posledních zjištěných početností lokalit; **podíl populace** v dobrém stavu = součet početností lokalit v dobrém stavu / celková × 100. | kap. 8 | |
| PR-20 | **Podíl lokalit v dobrém stavu** = počet lokalit v dobrém stavu / celkový počet lokalit × 100. | kap. 8 | ~~Jmenovatel „celkový počet“ = včetně neznámých (shodně s R-30).~~ Rozhodnutí 2026-10-28 (2. kolo, ot. 1): lokality v neznámém stavu se nezapočítávají (ani do jmenovatele). |
| PR-21 | **Cílový stav**: celková početnost / cílový stav × 100; cílové stavy EVL z ISOP, MZCHÚ z plánů péče; jednotky sjednotit s monitoringem. | kap. 8 | Komentáře c41, c43. |
| PR-22 | Do hodnocení vstupují **všechny lokality v hodnoceném území**, kromě *Campanula bohemica* (jen Příloha 4) a *Cerastium alsinifolium* (jen Příloha 5). | kap. 5, 5.2, 5.3 | Příloha 5 nedodána (R-49). |
| PR-23 | **Zaniklé lokality** se započítávají 10 let od posledního prokazatelného výskytu (lze déle); *Coleanthus* nejméně 12 let, vyřazení jen při doložených vhodných podmínkách ve ≥ 2 sezónách posledních 6 let. | kap. 5.1 | Evidence prodloužení (c10). |
| PR-24 | **Nová lokalita** vymezená podle metodiky monitoringu se započítává. | kap. 5.1 | |
| PR-25 | Hodnocení se aktualizuje při nových datech; u neaktualizovaných lokalit vstupují **původní údaje** (žádné pevné okno). | kap. 6 | Rozhodnutí 2026-10-28: okno po lokalitách z číselníku druhu (R-48). |
| PR-26 | Hodnocená území jsou **EVL i MZCHÚ**. | kap. 2, 8 | Nové proti R-12 (jen EVL); komentář c2. |
| PR-27 | **Lokalita** = lokalita monitoringu s jedinečným kódem, vymezená podle metodiky monitoringu druhu. | kap. 5 | Pojem „lokalita“ místo „DP“ (c7). |

## Nálezy ve zdrojích (R-35 …)

### R-35 — Názvy druhů se liší mezi listy, textem, NDOP a evropským seznamem
- **Typ:** TERMINOLOGIE · **Závažnost:** mění výsledek (bez převodu se druh nenapojí)
- **Kde:** Příloha 1 „*Aconitum\* moravicum*“, „*Gentianella\* bohemica*“, „*Tephroseris\* moravica*“, „*Dianthus lumnitzerii*“, „*Dianthus arenarius*“; Příloha 2 „*Aconitum firmum*“, „*Gentianella bohemica*“; Příloha 3 „*Gentianella praecox* subsp. *bohemica*“; text Přílohy 1 v .docx „*Campanula bohemika*“, „*Gentianella\* bohemika*“; literatura „*Iris arenaria* subsp. *humilis*“ (obrácené pořadí), „*Jurine cyanoides*“.
- **Problém:** limity se na data napojují přes název z NDOP; 21_1 navíc vyžaduje název ze seznamu předmětolokalit. Šest druhů má v obou zdrojích různé jméno (*Adenophora lilifolia*, *Gentianella bohemica*, *Vandenboschia speciosa*, *Iris humilis* subsp. *arenaria*, *Pontechium maculatum* subsp. *maculatum*, *Stipa zalesskii*). *Stipa zalesskii* vede NDOP jako *Stipa glabrata* („kavyl olysalý“, 61 záznamů).
- **Rozhodnutí zadavatele (2026-10-28):** ✅ `DRUH` = název z NDOP; převodník evropský seznam ↔ NDOP: `Data/Input/cis_druhy_nazvy.csv`. Úplný převod zdrojových tvarů je ve skriptu `54_cevnate_limity_2026.R` (`NAZVY_ZDROJ`).

### R-36 — Příloha 1 a Příloha 2 si odporují (frekvence, jednotka)
- **Typ:** ROZPOR · **Závažnost:** mění interpretaci
- **Kde:** frekvence *Aconitum* 3–6 (P1) × 3–12 (P2); *Artemisia pancicii* a *Cirsium brachycephalum* 1–2 × 1; P2 od *Cypripedium* dál frekvenci neuvádí. Jednotka *Cerastium alsinifolium* a *Galium sudeticum* plocha (m²) × plocha (dm²). Překlep „kvateoucí“ (P2, *Campanula bohemica*).
- **Rozhodnutí zadavatele (2026-10-28):** ✅ rozhoduje **Příloha 1**; rozpory vyznačeny ve sloupci `pozn` číselníku `cis_cevky_frekvence.csv`.
- **Otázka pro autory:** sjednotit P1 a P2; u *Cerastium* data uvádějí převážně cm² (35 záznamů), dm² 3, m² 4.

### R-37 — Mezery a nejednotný zápis kategorií v tabulkách
- **Typ:** REDAKCE · **Závažnost:** mění výsledek (při doslovném převzetí)
- **Kde:** P2 Vitalita *Dianthus arenarius* „ ≥ 30 %“ a *Poa riphaea* „ ≥ 15 %“ (mezera na začátku); P3 název řádku „Pokryvnost ostatních natantních a submerzních druhů “; P3 „75-100%“ (5×) a „50-75%“ (1×) bez mezery před %, v datech převažuje „75-100 %“.
- **Řešení při převodu:** mezery oříznuty; kategorie zapsány v převažujícím tvaru dat (`N-M %`). Zaznamenáno v protokolu převodu.

### R-38 — Mediány relativní stupnice: překlepy
- **Typ:** REDAKCE · **Závažnost:** mění výsledek (při doslovném převzetí)
- **Kde:** kap. 7.1 — „**1-0** = 5,5“ (má být 1–10), „100 001-1000 000“ (mezera), „více než 1000 000 = **1 000 0001**“ (má být 1 000 001).
- **Poznámka:** mediány metodiky se liší od číselníku `cis_pocet_kat.csv` (8 tříd, 5 / 25 / 75 / 500 …), který kód používá (a bere navíc dolní mez) — nález v registru.

### R-39 — Čtyřčlenná stupnice „51-10“
- **Typ:** REDAKCE · **Závažnost:** redakční
- **Kde:** kap. 7.2 — „čtyřčlenná stupnice: 0; 1-10; 11-50, **51-10**“ → 51–100.

### R-40 — *Coleanthus*: jeden, nebo dva klíčové indikátory; poškození; vývoj početnosti
- **Typ:** ROZPOR · **Závažnost:** mění výsledek
- **Kde:** kap. 7.1 „Pro druh byl tedy stanoven **pouze jeden** klíčový indikátor“ (komentář c23 „upravit, pokud se rozhodneme rozdělit na dva“); alternativní odstavec v indikátoru Úspěšnost reprodukce (c24) vs. vložený indikátor Délka absence; Příloha 2 má oba indikátory jako klíčové. Poškození je podle textu u puchýřky informativní, P2 mu dává limit ≤ 40 %. Vývoj početnosti je v P2 vyplněn, ačkoli Minimální přípustná početnost a Početnost jsou „.“.
- **Rozhodnutí zadavatele (2026-10-28):** ✅ podle tabulky dva klíčové indikátory; poškození `KLIC = info` s limitem; vývoj početnosti ponechán (neklíčový).
- **Otázka pro autory:** potvrdit dva klíčové indikátory, škrtnout alternativní odstavec (c24) a větu o jediném klíčovém indikátoru (c23).

### R-41 — Délka absence „< 6 let“: ostrá nerovnost
- **Typ:** NEJASNOST · **Závažnost:** mění interpretaci
- **Kde:** P2 *Coleanthus* „< 6 let“; formát limitů zná jen ≤ / ≥.
- **Rozhodnutí zadavatele (2026-10-28):** ✅ délka absence v celých letech ⇒ `max 5`.

### R-42 — Pěti- a šestičlenná stupnice; podíl dna
- **Typ:** NEJASNOST · **Závažnost:** mění výsledek (potenciálně)
- **Kde:** P3 mísí pětičlennou (`do 25 %`) a šestičlennou (`do 5 %`, `5-25 %`) stupnici; některé buňky (např. *Aconitum* E1, E2E3, expanzivní; *Jurinea*; *Pulsatilla patens*) vyjmenovávají jen šestičlennou — záznam „do 25 %“ by limit nikdy nesplnil. **V dnešních datech se to netýká žádného záznamu** (protokol převodu). *Podíl dna nad vodní hladinou*: text říká tříčlenná stupnice, data nesou `do 10 %` / `11-50 %` / `51-100%` — limit `11-50 %; 51-100 %` je tedy tříčlenný, jen data píší `51-100%` bez mezery (shoda v kódu selže).
- **Otázka pro autory:** sjednotit stupnici ve formulářích; u šestičlenných buněk doplnit `do 25 %`, má-li pětičlenný záznam projít.

### R-43 — Vitalita: podíl ke sterilním, ne k celku
- **Typ:** NEJASNOST · **Závažnost:** mění výsledek
- **Kde:** kap. 7.1 — „podíl kvetoucích a plodných populačních jednotek **a sterilních** populačních jednotek“. Kód počítal kvetoucí / celkem. Při 30 % kvetoucích z celku je podíl ke sterilním 43 %; limit 30 % ke sterilním odpovídá 23 % z celku.
- **Rozhodnutí zadavatele (2026-10-28):** ✅ podle textu; kód upraven (commit `677102a`).
- **Otázka pro autory:** potvrdit, že limity Přílohy 2 (5–40 %) jsou stanoveny pro poměr ke sterilním.

### R-44 — Vývoj početnosti: pozitivní trend není definován
- **Typ:** GAP · **Závažnost:** mění výsledek
- **Kde:** kap. 7.1 — definováno jen „negativní, pokud T2 i T3 < T1“; příklady: pozitivní 10; 12; 11; setrvalý 10; 10; 10 / 10; 12; 10 / 10; 2; 10 / 10; 10; 9.
- **Výklad použitý v registru (k potvrzení):** pozitivní = T2 i T3 > T1; negativní = T2 i T3 < T1; jinak setrvalý; méně než tři údaje ⇒ neznámý. Všechny příklady textu tomu odpovídají. Limit „pozitivní; setrvalý“ ⇒ selže jen negativní trend.

### R-45 — Verdikt lokality: přednost neznámého a špatného
- **Typ:** NEJASNOST · **Závažnost:** mění výsledek
- **Kde:** kap. 7 — „Pokud není klíčový indikátor v dobrém stavu, nemůže být lokalita hodnocena v dobrém stavu, **bez ohledu na stav ostatních** indikátorů“ × „V případě, že je v neznámém stavu klíčový indikátor, **nebo více než jeden z neklíčových** indikátorů, **nelze hodnocení provést**“.
- **Otázka:** (a) klíčový špatný a dva neklíčové neznámé ⇒ špatný, nebo neznámý? (b) klíčový dobrý, dva neklíčové špatné, jeden neznámý ⇒ špatný, nebo neznámý? (c) *Coleanthus* má dva klíčové — jeden dobrý a jeden neznámý ⇒ neznámý?
- **Návrh:** špatný má přednost, pokud je špatný výsledek jistý bez ohledu na neznámé (klíčový špatný, nebo ≥ 2 neklíčové špatné); jinak neznámý ⇒ neznámý; jinak dobrý.

### R-46 — Tabulky 1 a 2: neúplnost a vypuštěné prahy
- **Typ:** GAP · **Závažnost:** mění výsledek
- **Kde:** kap. 8, Tab. 1 (*Coleanthus*) a Tab. 2 (ostatní).
- **Neúplnost (doslovné čtení):** Tab. 1 neřeší podíl lokalit ≥ 50 % s cílovým stavem < 80 %. Tab. 2 neřeší podíl populace < 60 % s podílem lokalit ≥ 40 % ani podíl populace ≥ 60 % s podílem lokalit < 40 %; řádky 4–5 („≥ 60 % / ≥ 40 %“) lze číst jako náhradní větev, nebo jako pásmo 60–74 / 40–49 (pak není pokryt ani podíl populace ≥ 75 % s podílem lokalit 40–49 %). Žádná tabulka neřeší chybějící cílový stav ani stav **neznámý**.
- **Rozhodnutí zadavatele (2026-10-28):** ✅ každý indikátor má **jediný** práh (pro dobrý stav: 75 % / 50 % / 100 %); mírnější prahy 60 %, 40 % a 80 % se **vypouštějí a vyznačují** (sloupec `pozn` v limitech). Jak se tři (u *Coleanthus* dva) splněné/nesplněné indikátory skládají do dobrý/zhoršený/špatný, viz registr H-30 — k potvrzení.
- **Otázka pro autory:** stav neznámý na úrovni území; území bez cílového stavu (c43: „možná to bude plocha, počet rybníků“).

### R-47 — Více návštěv v roce: návrat k výběru celého záznamu
- **Typ:** ROZPOR (s rozhodnutím R-28) · **Závažnost:** mění výsledek
- **Kde:** kap. 7 — „do hodnocení vstupuje záznam s nejvyšší početností druhu … při shodě záznam s nejstarším datem“.
- **Poznámka:** rozhodnutí R-28 (2026-10-02) zavedlo agregaci po indikátorech; nový text ji výslovně ruší. Audit kódu (registr) bere za měřítko nový text.

### R-48 — Hodnocené období
- **Typ:** GAP · **Závažnost:** mění výsledek
- **Kde:** nový text pevné okno neuvádí (dříve 12 let, R-18); kap. 6 „u neaktualizovaných lokalit vstupují do hodnocení původní údaje“, kap. 5.1 zaniklé lokality 10 (12) let.
- **Rozhodnutí zadavatele (2026-10-28):** ✅ řídit se textem; okno se uplatní na **lokality** (lokalita zůstává, dokud její poslední prokazatelný výskyt není starší než 10 let, *Coleanthus* 12 let), záznamy lokality vstupují s celou historií; délka okna je v číselníku druhů `cis_cevky_frekvence.csv` (`OKNO_LET`). Kód upraven (commit `677102a`).

### R-49 — Výběr lokalit: Příloha 4 a 5
- **Typ:** GAP · **Závažnost:** mění výsledek
- **Kde:** Příloha 4 má **120** lokalit (ne 125); **32 kódů** (Cboh269, Cboh332 – Cboh439) nemá v NDOP jediný záznam; 239 kódovaných lokalit monitoringu *Campanula bohemica* v seznamu není. Příloha 5 (*Cerastium alsinifolium*) chybí („Jednou určitě bude :-)“). Názvy lokalit s překlepy („Labská bouda - seve“, „Casta Mevědí bouda Martinka 1“, „Jelení boud …“).
- **Rozhodnutí zadavatele (2026-10-28):** ✅ seznam se bude rozšiřovat; vendorován do `Data/Input/cevky_vyber_lokalit.csv`.
- **Blokuje:** výběr lokalit *Cerastium alsinifolium* (Příloha 5).

### R-50 — *Coleanthus*: vstupní data pro úspěšnost reprodukce
- **Typ:** GAP (data) · **Závažnost:** mění výsledek
- **Kde:** PR-13 potřebuje podíl jedinců, kteří odplodí. V NDOP je jen ve strukturované poznámce `POZN_BIO` „Míra úspěšnosti reprodukce: N %“ u **18 záznamů monitoringu z let 2020–2022** (9 pozitivních), a jako `POCITANO` „kvetoucí jedinci“ / „plodní jedinci“ u 3 záznamů mimo monitoring. Od roku 2023 žádné pole.
- **Rozhodnutí zadavatele (2026-10-28):** plodnost = „kvetoucí“ nebo „plodní“; kandidátní hodnoty viz výše.
- **Otázka pro autory / správce formuláře:** zavést pole „podíl plodných jedinců“ do formuláře Survey123 (aktualizace 2027, komentář c50).

### R-51 — Populační jednotky v datech neodpovídají Příloze 1
- **Typ:** GAP (data) · **Závažnost:** mění výsledek
- **Kde:** např. *Pedicularis sudetica* „kvetoucí a plodná lodyha“ × data jen „trsy“; *Echium maculatum* a *Gladiolus palustris* „kvetoucí a plodný jedinec“ × převažující „jedinci“ (celkový počet, do 2022); *Campanula bohemica* „kvetoucí a plodný trs“ × 86 záznamů „kvetoucí jedinci“ (2011–2016); plochy v cm² / dm² u druhů s jednotkou m² (*Cerastium*, *Klasea*), *Luronium natans* „růžice“ × cm², m²; *Dracocephalum austriacum* „trs/kvetoucí a plodný trs“ (dvojí jednotka); *Poa riphaea* bez kvetoucích a sterilních trsů (vitalitu nelze spočítat).
- **Řešení při převodu:** úplná tabulka jednotka → `POCITANO` se stavem shoda / kandidát / chybí je v protokolu převodu ([limity_cevky_2026_kontrola.md](limity_cevky_2026_kontrola.md)); „kvetoucí X“ zařazeno jako starší tvar „kvetoucí a plodné X“ (kandidát).
- **Otázka:** potvrdit kandidáty; má se zavést převod ploch (cm² → m²)? Formát limitů převod nezná.

### R-52 — Redakční vady a zastaralé údaje
- **Typ:** REDAKCE / ZASTARALÉ · **Závažnost:** redakční
- „poskytnou“ (kap. 1); „Hodnocení celého území stavu dobrý…“ (kap. 2, chybí sloveso); „Collector for ArcGIS“ (kap. 3, nahrazen ArcGIS Field Maps — R-14); „Citace metodik jsou uvedeny v kapitole 10“ (kap. 4, jsou v 9.1); „Pokryvnost volná půdy“, „aktuální početnost druh“ (kap. 7.1, 7.2); „management stratégy“ (Pergl et al. 2016); citace *Pulsatilla grandis* zvýrazněna (autoři k ověření); „Jurine cyanoides“, „*Iris arenaria* subsp. *humilis*“ v literatuře. *Kolektiv autorů (2016)* je nově v literatuře ✓ (R-34 vyřešeno).

## Rozhodnutí zadavatele — souhrn (2026-10-28)

| # | Otázka (Fáze 0) | Rozhodnutí |
|---|---|---|
| 1 | Konvence záznamu, umístění vstupů | ✅ podle návrhu; vstupy zůstávají v `Metodiky/Cevnate/` |
| 2 | Informativní stanovištní indikátory | ✅ `KLIC = info` (nová hodnota) |
| 3 | Názvy druhů | ✅ `DRUH` z NDOP + převodník evropský seznam ↔ NDOP |
| 4 | Dva indikátory početnosti | ✅ `POP_POCETMINPRIP` (klíčový), `POP_POCETCIL` (neklíčový) |
| 5 | Vývoj početnosti | ✅ `POP_TREND` (týká se jen rostlin); každý indikátor musí mít v číselníku plný název a číselný kód |
| 6 | *Coleanthus* | ✅ vše podle návrhu; plodnost = „kvetoucí“ nebo „plodní“, ověřit kandidátní hodnoty v datech |
| 7 | Úroveň území | ✅ každý indikátor **jediný** práh (min, max, nebo jejich dvojice); **jediná** tabulka limitů |
| 8 | Jednotka a frekvence | ✅ podle návrhu; založit soubory, kde je třeba |
| 9 | Normalizace kategorií | ✅ podle návrhu |
| 10 | Rozpory P1 × P2 | ✅ rozhoduje Příloha 1, vyznačit; vendorovat Přílohu 4 |
| 11–12 | Tabulky 1 a 2 | ✅ jediný práh na indikátor, mírnější vypustit a vyznačit |
| 13 | Vitalita | ✅ podle textu, upravit kód |
| 14 | Hodnocené období | ✅ podle textu, upravit kód; okno uložit v číselníku druhů |
| 15 | Příloha 4 | ✅ seznam se bude rozšiřovat |

## Rozhodnutí zadavatele — 2. kolo (2026-10-28, otázky Fáze C)

Podrobně včetně provedení v [harmonizace_registr.md](harmonizace_registr.md), část „Rozhodnutí zadavatele k otázkám Fáze C“.

| # | Otázka (Fáze C) | Nález | Rozhodnutí |
|---|---|---|---|
| 1 | Přednost špatného a neznámého ve verdiktu lokality | R-45 | ✅ špatný **nemá** přednost — doslovně kap. 7 (klíčový neznámý nebo > 1 neklíčový neznámý ⇒ neznámý); lokality v neznámém stavu se do procentních hodnocení území nezapočítávají vůbec (PR-20: jmenovatel **bez** neznámých — ruší poznámku „včetně neznámých“) |
| 2 | Úroveň území | R-46 | ✅ území bez cílového stavu ⇒ neznámý; sloučená Tab. 1 (*Coleanthus*) zůstává otevřená |
| 3 | Cílová početnost | PR-07 | ✅ první tři monitoringy; přednost má expertní hodnota v `Data/Input/cevky_cilova_pocetnost.csv` |
| 4 | Vývoj početnosti | R-44 | ✅ výklad ověřen proti textu — slovy je definován jen negativní trend, výklad odpovídá všem příkladům; o stavu rozhoduje jen negativní trend |
| 5 | *Cerastium* do dodání Přílohy 5 | R-49 | ✅ všechny lokality pod cíleným monitoringem |
| 6 | *Coleanthus* — vstup pro úspěšnost reprodukce | R-50 | pole je ve formuláři; v datech nalezen jen `POZN_BIO` „Míra úspěšnosti reprodukce: N %“ (2020–2022, 18 záznamů), od 2023 v exportu chybí — **k validaci** |
| 7 | Jednotky | R-51 | ✅ ekvivalence „kvetoucí X“ potvrzeny (12 řádků, ne 16); plochy převádět podle Přílohy 1 (provedeno v `21_1`) |
| 8 | Poškození | PR-12 | ✅ součet omezit na 100 % |
| 9 | Evidence c10 a c17 | PR-03, PR-23 | ✅ soubory v repozitáři (`cevky_lokality_stav.csv`, `cevky_neplatne_zaznamy.csv`) |
| 10 | Kódy ISOP | — | bude rozhodnuto |
| 11 | Opravy kódu; limity | — | opravy bude rozhodnuto; limity aktualizovat novými hodnotami ve starém formátu — živý `limity_cevky.csv` nahrazen |
