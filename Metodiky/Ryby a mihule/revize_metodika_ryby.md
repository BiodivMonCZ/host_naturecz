# Revize hodnocení stavu ryb a mihulí proti metodice EVL 2026

**Druhy:** *Cottus gobio* (vranka obecná), *Lampetra planeri* (mihule potoční),
*Misgurnus fossilis* (piskoř pruhovaný).
**Zdroj:** [Metodika_EVL_upravena_mihule_piskor.md](Metodika_EVL_upravena_mihule_piskor.md)
(L. Hanel, M. Muška) — tři kapitoly, každá s oddíly 1 cíl, 2 výběr lokalit, 3 terénní metodika,
4 sledované parametry, 5 hodnocení stavu (5.1 analýza, 5.2 Tab. 1, 5.3 lokalita, 5.4 EVL),
6 slabé stránky. Text je **částečně upravený**: některé odstavce byly přepsány podle chování
kódu, takže není vnitřně konzistentní (R-08).
**Datum:** 2026-11-05 · **Větev:** `ryby-metodika-2026` (z `cevnate-metodika-2026`, `35a3d99`).
**Registr nálezů kódu a dat:** [harmonizace_registr.md](harmonizace_registr.md) (H-71 …).

Fáze 1 (kontrolní seznam) a 2 (audit kódu a limitů) **nezměnily žádný kód ani vstupní data.**
Fáze 3 (2026-11-05, po rozhodnutí ke Kontrolnímu bodu 1) vytvořila návrh limitů a **neaplikovaný**
patch kódu, ověřené testovacím během — viz [Návrhy oprav](#návrhy-oprav--fáze-3-neaplikováno),
[Testovací běh](#testovací-běh-2026-11-05) a [otázky ke Kontrolnímu bodu 2](#otázky-pro-zadavatele--kontrolní-bod-2).

## Jak byla revize provedena

| Krok | Postup |
|---|---|
| Text | Markdown metodiky čten celý; pravidla vypsána po druzích, sdílená pravidla mají společné ID (`PR-DYN-C`, `PR-DYN-L` …). |
| Kód | `R/00_config`, `R/02_druhy/21_1` … `27` k commitu `35a3d99`; odkazy na řádky jsou k tomuto stavu. |
| Limity | `limity_ryby.csv` (46 řádků tří druhů), `cis_ryby_delky_strukt.csv`, `cis_indikatory_popis.csv`. |
| Data | Export NDOP 2026-05-10 (`export_data_evl.csv` + `export_data_zprap.csv`): 13 290 záznamů ryb; *Cottus* 3 045, *Lampetra* 1 849, *Misgurnus* 790; projekt *Monitoring druhů ČR* od 2013: 1 063 / 701 / 246. |
| Kvantifikace | Replikace logiky `21_1` → `24` (`25` jen podíl lokalit) mimo repozitář, s pomocnými funkcemi načtenými přímo z `21_1` a `21_2`. **Pipeline se nespouštěla**; čísla ověří testovací běh. |

## Souhrn

1. **Kód hodnotí jiné „lokality", než má metodika** (H-71). Ryby jako jediná skupina s cíleným
   monitoringem nemají v NDOP kód lokality (`KOD_LOKALITY` 0 % proti 79 % u obojživelníků
   a 47 % u cévnatých), takže `KOD_LOKAL` nese název EVL nebo kód EVL. 34–53 % záznamů
   monitoringu sdílí „lokalitu" s jinými úseky, až 68 průzkumů pod jedním klíčem. Pravidlo
   seskupení podle `KOD_LOKAL` je správné — chybí mu vstup.
2. **Populační parametry neodpovídají algoritmu metodiky.** `POP_DYN` průměruje řádky místo let,
   u 69 % lokalit *Cottus* jde fakticky o porovnání jednoho roku s jedním a lokalita s jediným
   rokem dat vychází jako stabilní (H-72). *Lampetra* má v metodice jiný algoritmus, který kód
   nemá a jehož znění připouští tři čtení (H-73). Vitalita se hodnotí v rámci roku, ne přes tři
   průzkumy, s hranicí skupin 60 místo 50 mm (H-78, H-79); *Misgurnus* ji nesplní nikde.
3. **Stanovištní parametry metodiky v limitech nejsou — data pro ně ale jsou.** Limity nesou
   kategoriální indikátory z dřívější verze (H-76); parametry metodiky (podíl substrátů,
   submerzní vegetace, plochy náplavů a trdlišť na 100 m) lze spočítat z tagů `rsub_*`,
   `veg_tok_sub`, `odhad_j_n`, `odhad_p_u`, `delka_prolov_u`. Tím se opravuje i H-42 (H-77).
4. **Rozhodovací pravidla se liší na obou úrovních.** Lokalita: kód žádá dva nesplněné
   stanovištní parametry pro „zhoršený", metodika jeden (H-75). EVL: jen *Lampetra* má
   indikátor, s prahem 50 místo 75 %, stav „špatný" nemůže vzniknout a migrační prostupnost
   na úrovni EVL chybí (H-81, H-83).
5. **Počty po odlovech nejsou v tazích**, jen jako strojový text v `POZNAMKA` (2019–2024), a u mihule
   se do „počtu" sčítají larvy, dospělci i spatření jedinci (H-74).

## Kontrolní seznam pravidel (PR-xx) a verdikty

Verdikty: **OK** · **liší se** · **chybí** (pravidlo v kódu není) · **nejasné** (rozhoduje výklad
nebo data) · **mimo kód** (pravidlo terénu, kód jej nemá a nemá mít). *C* = *Cottus*,
*L* = *Lampetra*, *M* = *Misgurnus*.

### Výběr lokalit a rozsah

| # | Pravidlo | Kde | C | L | M | Nález / poznámka |
|---|---|---|---|---|---|---|
| PR-LOK-1 | Lokalita = úsek ≥ 100 m (široké toky, M i stojaté vody ≥ 800 m²), prostorově fixovaný, GPS začátku a konce | §2 | liší se | liší se | liší se | H-71 — kód nemá identitu úseku |
| PR-LOK-2 | Tok ≤ 40 km: lokalita každých 5 km (C) / 4 km (L, M); nad 40 km každých 10 km; nejméně 3 lokality na EVL | §2 | mimo kód | mimo kód | mimo kód | Hodnocených klíčů < 3: C 4 EVL, L 7 EVL (H-81) |
| PR-LOK-3 | M: v EVL s výskytem mimo hlavní tok nebo v tůních jen známé lokality | §2 | — | — | mimo kód | |
| PR-EVL-0 | Seznam EVL a počtů lokalit (C 30 / 221, L 28 / 174, M 10 / 69) | §2 | OK | OK¹ | liší se | M: Kokořínsko — piskoř není předmětem ochrany (R-15). Kód: filtr předmětů ochrany není párový (H-85) |

¹ Názvy EVL v tabulkách: pět překlepů (R-15). Součty lokalit odpovídají řádkům tabulek.

### Terénní protokol → data, na kterých kód stojí

| # | Pravidlo | Kde | C | L | M | Nález / poznámka |
|---|---|---|---|---|---|---|
| PR-SEZ | Sezóna (C, M konec VIII – začátek XI; L IX–X); voda ≥ 5 °C; vzduch ≤ 25 °C (C, M); ne za vyšších průtoků, zákalu, deště | §3.1 | chybí | chybí | chybí | H-84 — jen evidovat; mimo okno 54 / 66 / 92 záznamů monitoringu |
| PR-ODL | Dva odlovy, počty zvlášť; L: 1. kontinuálně, 2. bodově na náplavech ≥ 10 cm, plocha prolovených a odhad celkové plochy náplavů; spatření se počítají | §3.1 | liší se | liší se | liší se | H-74 — `POCET` = součet odlovů (± spatření); počty odlovů jen v textu |
| PR-MER | Měří se do 50 jedinců druhu (C, L), do 100 za oba odlovy (M); 1 mm (M: ostatní ryby 5 mm); C SL × TL | §3.1, Tab. 1 | nejasné | OK² | OK | R-06, H-78 |
| PR-ABU | Odhad početnosti z plochy celého proloveného úseku (délka × šířka); C v ind./ha | §3.1, §4.1 | OK³ | liší se | OK³ | `<plocha_prolov_p>` = délka × šířka u 99 % záznamů L; jednotka m² vs ha poměr nemění. L: H-74 |
| PR-KVV | Kvalita vody (Povodí, ČHMÚ) „uvedena v tab. 1" | §3.1 | OK | OK | OK | V Tab. 1 není, nehodnotí se (R-09); kód ji nepoužívá (P-01) |
| PR-ULO | Ukládání: Survey → NDOP (C, L); NDOP + MOD (M) | §4.2 | OK | OK | nejasné | Kód čte jen NDOP — zda jsou data piskoře jen v MOD, nelze ověřit (R-19) |
| PR-FRQ | Zpočátku každoročně (L pět let), později alespoň 1× za 3 roky | §3.3 | mimo kód | mimo kód | mimo kód | Do hodnoceného roku nejvýše 4 roky s abundancí (H-72) |

² TL u mihulí; v seznamu délek jsou ale i dospělci (H-79). ³ U bodové metody plocha bodů (`<plocha_prolov_pbm>`, H-48).

### Parametry na úrovni lokality (5.1–5.2)

| # | Pravidlo | Kde | C | L | M | Nález / poznámka |
|---|---|---|---|---|---|---|
| PR-DYN-C, PR-DYN-M | Průměr posledních 3 průzkumů ze 3 různých let vs. průměr prvních 3 let; < 6 měření ⇒ 2 : 2; pokles > 50 % ⇒ špatný; M: více měření v roce ⇒ nejvyšší | §5.1 | liší se | — | liší se | H-72; směr limitu potvrzen (H-47); hranice R-01; degenerované případy R-11 |
| PR-DYN-L | Výchozí = 1. odlov; součet relativních změn dvou dalších odlovů < −50 % ⇒ špatný; po 4 průzkumech výchozí = jejich průměr; jednotka: larvy | §5.1, Tab. 1 | — | chybí | — | H-73 (výklad A / B / C), R-12 |
| PR-VIT-C/L/M | Všechny 3 délkové skupiny alespoň ve 2 z posledních 3 průzkumů v různých letech; skupiny C < 5 / 5–9 / > 9 cm, L < 5 / 5–12 / > 12, M < 5 / 5–15 / > 15 | §5.1, Tab. 1 | liší se | liší se | liší se | H-78 (hranice), H-79 (okno); M nesplní nikde (R-13) |
| PR-HAB-C | Součet relativní pokryvnosti kameny + balvany + štěrk > 50 % (Tab. 1 ≥ 50 %) | §5.1, Tab. 1 | chybí | — | — | sirotek `STA_DNOTYPSOUCETPROCENT`; zdroj `rsub_*` (H-76, H-77); R-02 |
| PR-HAB-L1 | Jemné náplavy (pro larvy) ≥ 10 m² na 100 m toku | §5.1, Tab. 1 | — | chybí | — | sirotek; zdroj `odhad_j_n` / `delka_prolov_u` (H-77) |
| PR-HAB-L2 | Štěrkopísčité proudné úseky (trdliště) ≥ 10 m² na 100 m toku | §4.1, §5.1, Tab. 1 | — | chybí | — | sirotek; zdroj `odhad_p_u`; definice R-05 |
| PR-HAB-M1 | Vhodný substrát (bahno, písek) ≥ 50 % plochy dna | §5.1, Tab. 1 | — | — | liší se | kód: `STA_DNO` = přítomnost bahna/písku (H-76) |
| PR-HAB-M2 | Submerzní makrofyta ≥ 11 % (Tab. 1 ≥ 10 %) plochy | §5.1, Tab. 1 | — | — | liší se | kód: `STA_VEGETACE` = přítomnost submerzní vegetace (H-76); R-02 |
| PR-HAB-X | Jiné stanovištní indikátory metodika nezná | Tab. 1 | liší se | liší se | liší se | C `STA_DNO`, `STA_DNOPREF`, `STA_PROUD`, `STA_PROUDPOCETTYPU`, `STA_DNOPOCET`; L `STA_DNOTYP` (H-45); M `STA_DNOPOCETTYPU`, `STA_PROUD`; všude `STA_MIGBAR*` (H-83) |
| PR-PAR | Do verdiktu jen parametry s limitem a hodnotou v daném roce | §5.3 | OK | OK | OK | `24` počítá jen `!is.na(STAV_IND)` (H-21) |
| PR-POP | Oba populační parametry musí být dobré | §5.1 | liší se | liší se | liší se | logika OK, ale navíc klíčový `POP_PRESENCE` (H-80) |

### Verdikt lokality (5.3)

| # | Pravidlo | Kde | C | L | M | Nález / poznámka |
|---|---|---|---|---|---|---|
| PR-DP | Klíčový nesplněn ⇒ špatný; klíčové splněny, ≥ 1 stanovištní nesplněn ⇒ zhoršený; jinak dobrý | §5.3 | liší se | liší se | liší se | H-75 (kód ≥ 2); M §5.1 popisuje kód (R-08) |
| PR-REP | Stanoviště z posledního monitoringu, populace z posledních průzkumů | Tab. 1 | OK | OK | OK | reprezentativní návštěva = poslední rok cíleného monitoringu (`24:219-228`) |

### Úroveň EVL (5.4)

| # | Pravidlo | Kde | C | L | M | Nález / poznámka |
|---|---|---|---|---|---|---|
| PR-EVL-1 | Podíl hodnocených lokalit v dobrém stavu; neznámé se nezapočítávají | §5.4 | chybí | OK | chybí | C, M bez řádku `chu` (H-41, H-81); `LOK_POCETDOB` počítá jen „dobrý" |
| PR-EVL-2 | Agregované lokality dobré při ≥ 75 %; při třech lokalitách stačí dvě | §5.4 | chybí | liší se | chybí | L `min 50`; *n* < 3 nedefinováno (R-10) |
| PR-EVL-3 | Verdikt: lokality dobré + prostupnost dobrá ⇒ dobrý; dobré + špatná ⇒ zhoršený; lokality špatné ⇒ špatný | §5.4 | chybí | chybí | chybí | H-81 — „špatný" nevzniká; H-82 latentní „dobrý" |
| PR-MIG | Prostupnost EVL: žádná příčná překážka přes celý profil vyšší než 10 cm (C, M) / 5 cm (L) v lokalitách a okolí ~ 500 m; má-li EVL hodnocení prostupnosti celého toku, použije se | §4.1, §5.1, Tab. 1 | liší se | liší se | liší se | H-83 — dnes neklíčový indikátor lokality; přednost u C jen v Tab. 1 (R-07) |

### Taxonomie

| # | Pravidlo | Kde | C | L | M | Nález / poznámka |
|---|---|---|---|---|---|---|
| PR-TAX | C: pozor na hybridizaci s *C. poecilopus* (Marešová et al. 2012); L vs. *Eudontomyzon mariae* | Poznámka C; L §4.1 | nejasné | OK | — | v exportu žádné *Cottus* sp. ani kříženci, *E. mariae* samostatně (P-03); identifikaci hybridů z dat ověřit nelze |

## Nálezy v textu metodiky (R-xx)

Ověřeno proti textu; **text se neopravuje**, nálezy jsou podklad pro autory.

### R-01 — *Cottus* Tab. 1, dynamika: sloupec „příznivý stav" popisuje nepříznivý; dva místo tří; hranice 50 %
- **Typ:** ROZPOR · **Závažnost:** mění výklad limitu
- Buňka ve sloupci „limitní hodnota příznivého stavu" zní „… byl vypočtený průměr abundance
  nižší o 50 a více procent …" — to je podmínka **nepříznivého** stavu. Navíc „posledních třech
  (případně dvou)", ale sloupec „zdroj dat" uvádí „z posledních dvou průzkumů".
- Hranice: §5.1 „pokles o **více než** 50 %" ⇒ pokles přesně o 50 % je dobrý; Tab. 1 „o 50 **a více**
  procent" ⇒ špatný. Kód (`POP_DYN >= 50` vyhoví) odpovídá §5.1. Směr limitu `min 50` je tím
  **potvrzen** (H-47).
- **Otázka:** platí „> 50 %" (§5.1)?

### R-02 — Prahy stanoviště: „> 50" × „≥ 50" (C), 11 × 10 % (M)
- **Typ:** ROZPOR · **Závažnost:** nízká až střední
- *Cottus*: §5.1 „> 50 %", Tab. 1 „minimálně 50 %". Při převodu pásem mediány (H-76) hodnota přesně
  50 v datech nevzniká, rozpor výsledek nemění.
- *Misgurnus*: §5.1 „minimálně v 11 % plochy lokality", Tab. 1 „minimálně 10 % plochy dna". Data
  nesou pásma *Vzácný (0-10 %)* a *Běžný (11-25 %)* — **11 % je hranice pásma**, 10 % leží uvnitř
  pásma 0–10 a nelze o něm z dat rozhodnout. Navíc text „plochy lokality", tabulka „plochy dna".
- **Návrh:** 11 % (slučitelné s pásmy); plochu dna.

### R-03 — *Misgurnus* Tab. 1, dynamika: znění mihule proti algoritmu vranky v §5.1
- **Typ:** ROZPOR · **Závažnost:** mění algoritmus
- Tab. 1: „… zjištěn **součtem relativních změn** početnosti pokles **oproti výchozí hodnotě** menší
  než 50 %" (= znění *Lampetra*). §5.1: průměr posledních tří proti prvním třem, < 6 ⇒ 2 : 2
  (= *Cottus*), s pravidlem maxima za rok. Kód i tento audit se řídí §5.1.

### R-04 — Označení klíčových parametrů
- **Typ:** REDAKČNÍ · **Závažnost:** nízká
- *Misgurnus* §5.1 označuje „(klíčový parametr)" jen dynamiku, vitalitu ne — obě ale musí vyhovět
  („Oba parametry … musí být hodnoceny jako vyhovující"). *Lampetra* §5.1 neoznačuje ani jeden.
  §5.3 všech kapitol: „Parametry označené jako klíčové jsou posuzovány jako populační".

### R-05 — *Lampetra*: definice trdliště v §5.1 zkopírována z parametru larev
- **Typ:** ROZPOR · **Závažnost:** mění zdroj dat
- §5.1 „přítomnost dostatečné plochy vhodných mezohabitatů pro tření — hodnotí se přítomnost/
  nepřítomnost vhodných ploch **jemných náplavů**" — týž text jako u larev. §4.1 definuje trdliště
  jako **štěrkopísčité dno s proudným charakterem**. Audit se řídí §4.1 (zdroj `<odhad_p_u>`).

### R-06 — Typ délky: SL × celková délka (*Cottus*); překlep „()TL"
- **Typ:** ROZPOR · **Závažnost:** mění zařazení do skupin
- *Cottus* §3.1: „u ryb se měří délka těla (**SL**, longitudo corporis)"; Tab. 1, pozn. a: skupiny
  „(**celkové délky**)". *Lampetra* §3.1: u ryb TL. „u mihulí se měří délka těla ()TL" — překlep.
- Volba typu posouvá tytéž ryby přes hranici 5 cm (H-78); data typ neuvádějí.

### R-07 — Migrační prostupnost: přednost hodnocení celého toku a rozsah
- **Typ:** NEÚPLNOST · **Závažnost:** střední
- Přednost hodnocení prostupnosti celého toku uvádí §5.1 *Lampetra* a *Misgurnus*; u *Cottus*
  jen Tab. 1 („případně hodnocení … celého toku v EVL, pokud je dostupné").
- §5.1 „pouze v případě nepřítomnosti žádných příčných překážek **v EVL**" × §4.1 „v monitorovaných
  lokalitách a v jejich okolí". Data nesou výšku a počet bariér, ne „přes celý profil" ani vzdálenost
  od úseku (H-83). Zdroj „databáze migračních překážek" není jmenován.

### R-08 — *Misgurnus* §5.1: odstavec popisuje kód, ne metodiku
- **Typ:** CHYBA TEXTU · **Závažnost:** vysoká (mate čtenáře)
- Odstavec „Kód hodnotí stanoviště piskoře pomocí několika dílčích parametrů … Pokud nevyhovuje
  žádný nebo pouze jeden z nich … je lokalita hodnocena jako dobrá. Pokud nevyhovují alespoň dva …
  zhoršená" je **popis dnešního `24`** (H-75, H-76) a odporuje §5.3 téže kapitoly („nevyhoví alespoň
  jeden … zhoršený"). **Návrh:** vypustit.

### R-09 — Kvalita vody „uvedena v tab. 1"
- **Typ:** ROZPOR · **Závažnost:** nízká
- Všechny tři §3.1: „Vybrané parametry k hodnocení kvality vody jsou uvedeny v tab. 1 (vodivost, BSK5,
  N-NO2, N-NO3, N-NH4, rozpuštěný kyslík, celkový fosfor)". Tab. 1 je neobsahuje a v §5 se kvalita vody
  nehodnotí. Prototyp hodnocení chemie v repozitáři existuje mimo pipeline (H-88).
- **Otázka:** má kvalita vody vstoupit do hodnocení, nebo odkaz vypustit?

### R-10 — Úroveň EVL: „příznivý" × „dobrý" a pravidlo pro 1–2 lokality
- **Typ:** NEÚPLNOST · **Závažnost:** střední
- *Cottus* §5.4: „75 % lokalit (případně dvě ze tří) **na základě hodnocení klíčových parametrů
  v příznivém stavu**" — lze číst i tak, že se počítají lokality dobré i zhoršené (klíčové parametry
  vyhoví v obou). *Lampetra* a *Misgurnus*: „v **dobrém** stavu".
- Pro 1 nebo 2 hodnocené lokality pravidlo „75 %, při třech dvě" neříká nic (dnes: C 4 EVL, L 7 EVL).
  Doslovně: 1/1, 2/2.
- *Cottus* tabulka 5.4 má jiné pořadí řádků než *Lampetra*/*Misgurnus* (bez věcného rozdílu).

### R-11 — Dynamika (*Cottus*, *Misgurnus*): okrajové případy algoritmu
- **Typ:** NEÚPLNOST · **Závažnost:** střední
- „Méně než šest **po sobě následujících** měření z různých let" — po sobě následujících let, nebo
  průzkumů?
- 2 roky dat: „dva první" a „dva poslední" jsou tytéž ⇒ poměr vždy 100 %. 3 roky: okna se překrývají
  (*y*₁,*y*₂ × *y*₂,*y*₃). Reprezentativní lokality mají v datech nejvýše 4 roky (H-72).
- „Prvních tří let monitoringu": od začátku monitoringu vůbec, nebo v hodnoceném období (kód má okno
  12 let)?
- Výchozí průměr 0 (druh dříve nezjištěn) — kód: 0 / 0 ⇒ 100 %, 0 / > 0 ⇒ nehodnoceno.

### R-12 — *Lampetra*: znění algoritmu dynamiky
- **Typ:** NEJEDNOZNAČNOST · **Závažnost:** vysoká
- „Součet relativních změn početnosti" připouští tři čtení s rozdílným výsledkem (H-73: A 26, B 21,
  C 15 lokalit špatných ze 67). Metodika potřebuje ≥ 3 průzkumy (výchozí + dva další); 90 lokalit
  dnes nehodnotitelných.
- „Po čtyřech … bude z těchto čtyř údajů stanoven průměr … s kterým bude v dalších letech počítáno":
  pevný průměr prvních čtyř, nebo klouzavý? Při právě čtyřech průzkumech platí ještě první odlov?
- Jednotka „počet larev" × kód (celkový počet; H-74).

### R-13 — Vitalita: proveditelnost a okno
- **Typ:** NEÚPLNOST · **Závažnost:** vysoká
- Méně než tři průzkumy v okně (C 71, L 30, M 32 reprezentativních lokalit jen s jedním): podmínku
  „alespoň ve dvou průzkumech" nelze splnit — špatný, nebo neznámý?
- *Misgurnus*: všechny tři skupiny v jednom průzkumu **ani jednou** (0 ze 73 lokalit). Klíčový parametr
  tak určuje verdikt všech lokalit piskoře.
- *Lampetra*: §4.1 sama uvádí, že larvy < 5 cm se „loví jen zcela výjimečně"; skupinu > 12 cm navíc
  plní **dospělci**, jejichž délky jsou v témž seznamu (135 ze 136 záznamů). Skupiny mají být larvální.

### R-14 — Co je „počet" a abundance
- **Typ:** NEÚPLNOST · **Závažnost:** vysoká
- *Cottus* §4.1 ind./ha × Tab. 1 „počet jedinců"; *Lampetra* Tab. 1 „počet larev".
- Oba odlovy, nebo jen první? Spatření neulovení jedinci ano/ne? U *Lampetra* je 2. odlov bodový na
  náplavech — přičíst k 1. odlovu na ploše celého úseku, nebo počítat zvlášť?
- Citovaná literatura (Cowx 1991, Raleigh & Short 1981) je o odhadu velikosti populace z odlovů
  s odstraňováním; metodika ale odhad (depletion) nikde nepředepisuje. Audit předpokládá úlovek.

### R-15 — Výběr lokalit: tabulky EVL
- **Typ:** DATA · **Závažnost:** střední (Kokořínsko), nízká (názvy)
- *Misgurnus*: CZ0214013 **Kokořínsko** (6 lokalit) — podle seznamu předmětů ochrany 2025 piskoř
  **není** předmětem ochrany této EVL. Ostatní EVL všech tří tabulek předmětem ochrany jsou a žádná EVL,
  kde je druh předmětem ochrany, v tabulkách nechybí.
- Názvy proti seznamu: *Krkanka a Strádovské peklo* (Krkanka-Strádovské peklo), *Dědina u Dobruška*
  (L; Dědina u Dobrušky), *Svitávka* (Svitavka), *Veverský potok* (Veveřský potok), *Travní Dvůr*
  (Trávní dvůr).
- *Lampetra* CZ0213050 Ohrazenický potok: od 2013 bez záznamu monitoringu (H-82).
- *Misgurnus* §2: „v tocích s výskytem zájmového v délce do 40 km" — chybí slovo.

### R-16 — Sezóna a podmínky se mezi kapitolami liší
- **Typ:** INFORMACE · **Závažnost:** nízká
- *Lampetra* IX–X a bez podmínky teploty vzduchu; *Cottus*, *Misgurnus* konec VIII – začátek XI,
  vzduch ≤ 25 °C. Věcně v pořádku; pro případný příznak H-84 je nutné okno po druzích.

### R-17 — Měření a formuláře
- **Typ:** INFORMACE · **Závažnost:** nízká
- *Cottus*, *Lampetra* „do 50 jedinců pro každý druh"; *Misgurnus* „do celkového součtu 100 jedinců za
  oba lovy". V datech 62 záznamů *Cottus* s více než 50 délkami (max. 304) — bez dopadu na skupiny.

### R-18 — Literatura a redakce
- **Typ:** REDAKČNÍ · **Závažnost:** nízká
- Adámek & Jurajda: v literatuře **2005** („Metodiky **VKV** TGM" — správně VÚV), v textu *Cottus*
  a *Lampetra* **2006**, *Misgurnus* 2005.
- Cross & Stott 1975: ve všech třech seznamech **dvakrát**, poprvé slepeno na jednom řádku s Cowx 1991.
- *Lampetra* §3.1: „(Muška 2025\." — neuzavřená závorka. *Cottus* „Muška 2024", *Lampetra* „Muška 2025"
  jako „metodika monitoringu druhu" — tentýž dokument? V literatuře **chybí oba**; chybí i Vlach 2011
  (*Misgurnus*) a Harvey a Cowx 2003 je jen u mihule.
- „Humpl a Lusk 2006a" — v seznamu dvě práce 2006 bez rozlišení a/b; „Podlesný 2010" × „Podlesný a kol. 2010".
- Překlepy: „formulářú", „Klíčové paramentry", „počet délkových skupina", „ani z deště", „contribution o
  the knowledge", „Histria", „draihage", „Dědina u Dobruška".

### R-19 — *Misgurnus*: data v MOD
- **Typ:** DATA · **Závažnost:** nejasná
- §4.2: populační parametry do NDOP, **všechny parametry lokality do MOD**. Pipeline čte jen NDOP;
  data piskoře mají stanovištní tagy u 236 z 246 záznamů monitoringu, takže zásadní ztráta se nezdá —
  ale zda jsou v MOD údaje, které v NDOP nejsou, nelze z repozitáře ověřit.

## Stav nálezů H-38 … H-58 podle nového textu

Nálezy z [registru obojživelníků](../Obojzivelnici/harmonizace_registr.md).

| Nález | Předmět | Nová metodika | Stav |
|---|---|---|---|
| H-38 ⚠ | 19 z 26 indikátorů ryb bez výpočtu | po H-42 zbývají u tří druhů 4; tvrzení „chybí tag" neplatí | **částečně vyvráceno** → H-77 |
| H-39 ⚠ | názvy druhů v limitech | tři druhy mají názvy shodné v NDOP, seznamu i limitech | mimo rozsah |
| H-40 ⚠ | filtr zahazuje pět druhů | tři druhy filtrem projdou; filtr ale není párový | mimo rozsah; navazuje **H-85** |
| H-41 ⚠ | 16 ze 17 druhů bez úrovně EVL | metodika úroveň EVL definuje pro všechny tři (75 %) | **řeší text** → H-81 |
| H-42 ✅ | extrakce tagů ryb | zapnuté indikátory nemají v metodice oporu; zdroje parametrů metodiky existují | **odporuje** (sada) → H-76, H-77 |
| H-43, H-44 | `POP_REPRO` obojživelníků | — | mimo rozsah |
| H-45 ⚠ | `STA_DNOTYP` „kompaktní jílové dno" (*Lampetra*) | metodika jej nezná; místo něj náplavy a trdliště | **řeší text** — vypustit |
| H-46 ⚠ | `STA_DALSIPARAMETRY` (*Rhodeus*) | — | mimo rozsah |
| H-47 ⚠ | znaménko `POP_DYN` (prozatímně `min 50`) | „pokles o více než 50 % ⇒ špatný" ⇔ `MEAN / REF × 100 < 50` ⇒ **`min 50` je správně** | **směr potvrzen**; hranice R-01; algoritmus H-72 |
| H-48 ✅ | plocha bodové metody | metodika: plocha celého úseku; srovnatelnost bodové metody neřeší | otevřené (R-14) |
| H-49 ✅ | larvy v `POP_POCET` | v monitoringu `POCET` = larvy + dospělci + spatření; metodika chce larvy | **znovu otevřeno** → H-74 |
| H-50 ✅ | Tabulka 2 jen pro obojživelníky | potvrzeno (P-02); důsledkem je, že EVL ryb nemůže být „špatná" | platí; navazuje **H-81** |
| H-51 ✅ | jednotka `POP_VITALITA` | tří druhů se netýká | mimo rozsah |
| H-52 ✅ | nejvyšší bariéra | platí; parametr se přesouvá na EVL | platí; navazuje **H-83** |
| H-53 ✅ | prázdný `KLIC` | opraveno; nová cesta téže pasti přes prázdnou hodnotu | platí; navazuje **H-82** |
| H-54 ⏸ | `val 10 m2` u ploch *Lampetra* | „minimálně 10 m² na 100 m" ⇒ `min` a normalizace na 100 m | **řeší text** → H-77 |
| H-55 ✅ | `POP_POCET` jako `info` | — | platí |
| H-56 ✅ | `POP_PRESENCE` u všech ryb | metodika přítomnost jako klíčový parametr nezná | **odporuje** → H-80 |
| H-57 ⏸ | *Salmo salar* v číselníku délek | — | mimo rozsah |
| H-58 ✅ | `POP_PRESENCE` jako `val ano` | technicky správně | platí (viz H-80) |

## Datové mezery — požadavky na formulář a přenos do NDOP

Pro správce formuláře Survey a přenosu do NDOP (zadavatel 2026-11-05: „mají to být tagy").

| # | Údaj | Dnes | Požadavek | Nález |
|---|---|---|---|---|
| D-1 | kód monitorované lokality | chybí; `<naz_tok>` nese název EVL | kód lokality v `KOD_LOKALITY`, stabilní mezi roky (jako `amp…` u obojživelníků) | H-71 |
| D-2 | počty v 1. a 2. odlovu, spatření | jen text v `POZNAMKA` (2019–2024) | tagy, např. `<poc_lov1>`, `<poc_lov2>`, `<poc_spatr>`; u mihule zvlášť minohy a adulti (`<poc_lov1_min>`, `<poc_lov1_ad>` …) | H-74 |
| D-3 | délky larev a dospělců mihule | jeden seznam `<velikosti>` | dva seznamy (`<velikosti_min>`, `<velikosti_ad>`) | H-79 |
| D-4 | typ délky | neuveden | SL / TL v metadatech formuláře | H-78 |
| D-5 | „bez bariér" | prázdný tag (nerozlišitelné od nevyplněno) | `<pocet_bar>0</pocet_bar>`; příznak „přes celý profil"; vzdálenost od úseku | H-83 |
| D-6 | hodnocení prostupnosti celého toku | v repozitáři není | zdroj (databáze migračních překážek) a formát | H-83 |
| D-7 | teplota vzduchu, srážky | chybí | volitelné tagy, jen pro příznak podmínek | H-84 |

## Návrhy oprav — Fáze 3 (neaplikováno)

Rozhodnutí zadavatele ke Kontrolnímu bodu 1 (2026-11-05) jsou v
[registru](harmonizace_registr.md). Limity: [limity_ryby_2026_kontrola.md](limity_ryby_2026_kontrola.md)
(protokol převodu, včetně pravidla pro pásma). Kód: [patche/ryby_metodika_2026.patch](patche/ryby_metodika_2026.patch)
a [patche/README.md](patche/README.md). **Nic z toho není v živém kódu ani v živých vstupech** —
obojí bylo ověřeno testovacím během v izolované kopii (§Testovací běh níže).

| # | Soubor · místo | Před | Po | Které výstupy se mění a kam | Nález / rozhodnutí |
|---|---|---|---|---|---|
| F3-A | `Data/Input/limity_ryby_2026.csv` (generuje `55_ryby_limity_2026.R`) | 43 řádků tří druhů, viz H-76 | 46 řádků: populace `POP_DYN`, `POP_VITALITA3` (klíčové), `POP_VITALITA`, `POP_PRESENCE`, počty (`info`); stanoviště podle metodiky; `STA_MIGBAR*` jako `info`; EVL `LOK_PROCDOBR min 75` a `STA_MIGPROSTUP` | DP: jiná sada indikátorů; EVL: výstup nově i pro *Cottus* a *Misgurnus* | H-76 … H-83 |
| F3-B | `Data/Input/cis_ryby_delky_strukt_2026.csv` | 1. skupina do 60 mm (C, L), do 50 mm včetně (M) | do 49,99 mm u všech tří | vitalita: více délek ve 2. skupině | H-78 |
| F3-C | `02_n2k_data_druhy.R` · `KOD_LOKAL` | ryby: `<naz_tok>` (název EVL), jinak kód EVL | ryby: `LOKALITA` | DP ryb ≈ 2× více, sloučení úseků mizí | H-71, Z-1 |
| F3-D | `02_n2k_data_druhy.R` · `CILMON` | `ZDROJ %in% CIS_CILMON` (nikdy pravda) | `ZDROJ %in% CIS_CILMON$ZDROJ_NDOP` | **mimo ryby**: 2 128 záznamů nově cílený monitoring (obojživelníci 2 106) | H-89, Z-1 |
| F3-E | `21_1` · filtr předmětů ochrany (a `27` · předfiltr) | druh a EVL zvlášť | dvojice druh × EVL | DP ve všech skupinách bez EVL, kde druh není předmětem ochrany (viz README patche) | H-85, Z-8 |
| F3-F | `21_1` · `POP_POCETODLOV`, `POP_ABUNDANCENAL` | abundance z `POCET` (oba odlovy ± spatření; u mihule i dospělci) | z 1. + 2. odlovu podle `POZNAMKA`, u mihule jen larvy; jinak `POCET`; zdroj v `POP_POCETODLOVZDROJ` | `POP_DYN` | H-74, Z-3 |
| F3-G | `21_1` · stanovištní sloupce | 4 sirotci, kategoriální zástupné indikátory | `soucet_pasem()`, `vegetace_submerzni()`, `plocha_na_100m()` | stanovištní parametry metodiky se poprvé hodnotí | H-76, H-77, Z-11 |
| F3-H | `21_1` · `n2k_druhy_lokpop_ryby2026` | `POP_DYN` z prvních tří **řádků**, jedna hodnota na lokalitu | `dyn_rok()` za lokalitu a rok; *Lampetra* varianta C; vitalita `vitalita3()` přes tři poslední průzkumy | `POP_DYN`, `POP_VITALITA3`; ostatní druhy ryb beze změny | H-72, H-73, H-79, Z-2 |
| F3-I | `21_1` · `STA_PODMINKYODLOVU` | — | „v pořádku" / důvody (mimo sezónu, voda < 5 °C, zvýšený průtok, zákal) | jen zobrazení | H-84, Z-7 |
| F3-J | `24` · `CELKOVE` | „zhoršený" od 2 nesplněných stanovištních | tři druhy od 1 | DP: dobrý → zhoršený | H-75 |
| F3-K | `25` · `STA_MIGPROSTUP`, rozhodovací tabulka | aritmetika bez stavu „špatný"; prázdné `LOK_PROCDOBR` ⇒ dobrý | lokality (75 %, při třech dvě) × prostupnost; bez hodnocené lokality ⇒ neznámý | EVL: vzniká „špatný" a „zhoršený" z prostupnosti | H-81, H-82, H-83, Z-5, Z-6 |

## Testovací běh (2026-11-05)

Kaskáda `21_1` → `27` pro tři druhy v izolovaných kopiích repozitáře: **pred** = živý kód
a limity, **po** = patch + `limity_ryby_2026.csv` + `cis_ryby_delky_strukt_2026.csv`.
Výsledky a soubory: [Outputs/Test/Ryby_20261105/](../../Outputs/Test/Ryby_20261105/README.md).

| | *Cottus* | *Lampetra* | *Misgurnus* |
|---|---|---|---|
| DP pred → po | 370 → 874 | 253 → 563 | 113 → 390 |
| verdikty DP s cíleným monitoringem po (d / z / š) | 279 / 42 / 173 | 180 / 95 / 92 | 66 / 79 / 34 |
| — z toho „dobrý" bez hodnoceného klíčového parametru (H-90) | 236 | 175 | 66 |
| EVL pred → po | — → 8 dobrý, 22 špatný | 11 d, 17 z → 2 d, 4 z, 21 š, 1 neznámý | — → 9 špatný |
| prostupnost EVL dobrá / špatná / neznámá | 0 / 17 / 13 | 0 / 19 / 9 | 0 / 4 / 5 |

Co běh ukázal:

1. Nová pravidla fungují podle návrhu: EVL dostává stav „špatný" (H-81) a „zhoršený" z prostupnosti
   (*Lampetra* 4 EVL), *Cottus* a *Misgurnus* mají poprvé výstup EVL, CZ0213050 je „neznámý" (H-82),
   stanovištní parametry metodiky se hodnotí.
2. **Nová vada H-90:** po převedení `POP_PRESENCE` na `info` nemá většina DP žádný hodnocený klíčový
   parametr a `24` je prohlásí za „dobré". S variantou C (dobrý jen při obou hodnocených, §5.1) by
   agregované lokality byly dobré jen ve 2 EVL *Cottus* a 1 EVL *Lampetra*.
3. **Migrace:** výška bariéry je známa jen u 21 % / 27 % / 6 % lokalit, a protože formulář nezapisuje
   „0 bariér", nemůže prostupnost vyjít dobrá v žádné EVL (D-5).
4. **Vitalita podle metodiky** se splní zřídka — *Cottus* 46 z 218 hodnocených DP, *Lampetra* 6 z 98,
   *Misgurnus* 0 z 35 (R-13).

## Otázky pro zadavatele — Kontrolní bod 2

| # | Otázka | Možnosti | Doporučení | Nález |
|---|---|---|---|---|
| Z-12 | Lokalita bez hodnoceného klíčového parametru | A ponechat „dobrý" / B neznámý, není-li hodnocen žádný / C dobrý jen při obou hodnocených (§5.1) | **C** | H-90 |
| Z-13 | Aplikovat patch a návrh limitů na větev? | ano (vše) / ano bez F3-D a F3-E (sdílený kód ostatních skupin) / ne | ano, **až s opravou Z-12**; F3-D a F3-E s vlastním testovacím během obojživelníků | patch README |
| Z-14 | Oprava `CIS_CILMON` (F3-D) — změřit dopad na obojživelníky během *T. cristatus* + *B. bombina* před aplikací? | ano / ne | ano | H-89 |
| Z-15 | Doplnit do `cil_mon_zdroj.csv` „Fajmon (2025) Sledování stavu populací srpice karbincolisté…" (303 záznamů)? | ano / ne | ne bez rozhodnutí ke cévnatým (PR-01) | H-89 |
| Z-16 | Sjednotit zápis `LOKALITA` (pomlčky, mezery, velikost písmen) jako mezikrok do zavedení kódu lokality? | ano / ne | ano — spojí 67 / 45 / 4 lokalit a prodlouží řady | H-71, H-90 |
| Z-17 | Vitalita při méně než dvou průzkumech: `NA` (předběžně) / špatný | — | `NA` do odpovědi autorů (A-4) | H-79 |
| Z-18 | Odeslat otázky A-1 … A-10 autorům metodiky | ano / po úpravě | ano | — |

## Otázky pro zadavatele — Kontrolní bod 1 (zodpovězeno 2026-11-05)

Rozhodnutí jsou v [registru](harmonizace_registr.md), tabulka „Rozhodnutí zadavatele ke Kontrolnímu bodu 1".

| # | Otázka | Možnosti | Doporučení | Nález |
|---|---|---|---|---|
| Z-1 | Zdroj kódu lokality ryb | (1) požadavek na `KOD_LOKALITY` ze Survey; (2) přechodně odvozený klíč úseku (tok z `LOKALITA` + shluk souřadnic) | **(1) i (2)** — 2 navrhnout ve Fázi 3 s ověřením na datech | H-71 |
| Z-2 | Vzorec dynamiky *Lampetra* | A součet / B průměr / C oba | **B** | H-73 |
| Z-3 | Smí Fáze 3 číst počty odlovů z textu `POZNAMKA` do doby, než budou tagy? | ano / ne (zůstává `POCET`) | ano, jen jako přechodné řešení s příznakem zdroje | H-74 |
| Z-4 | `POP_PRESENCE` | ponechat klíčový / `info` / vypustit | **`info`** | H-80 |
| Z-5 | Prostupnost na úrovni EVL: časová agregace a prázdný tag | poslední průzkum každé lokality / celé okno; prázdný = bez bariéry / neznámo | poslední průzkum lokality; prázdný = **neznámo** | H-83 |
| Z-6 | EVL s 1–2 hodnocenými lokalitami | doslovně 75 % (1/1, 2/2) / neznámý | doslovně 75 % | H-81 |
| Z-7 | Sezóna a podmínky | jen příznak / filtr | jen příznak | H-84 |
| Z-8 | Párový filtr předmětů ochrany ve sdíleném kódu | opravit pro všechny skupiny / jen ryby | všechny skupiny, s měřením dopadu | H-85 |
| Z-9 | Volné soubory v kořeni | přesunout do `Metodiky/Ryby a mihule/podklady/` / ponechat | přesunout | H-88 |
| Z-10 | Testovací běh | (a) běh „před" teď pro tři druhy; (b) běh „po" s patchi Fáze 3 | obojí v **izolované kopii repozitáře** (`27` zapisuje do `Outputs/Data/druhy/` a `Data/Temp/`), výsledky do `Outputs/Test/Ryby_<datum>/` | — |
| Z-11 | Převod pásem na číslo | součty = středy pásem (5 / 18 / 38 / 63 / 87,5 / 100), jedno pásmo = výčet `val` | potvrdit návrh, nebo nejdřív autoři | H-76 |

## Otázky pro autory metodiky (L. Hanel, M. Muška)

| # | Okruh | Otázka | Odkaz |
|---|---|---|---|
| A-1 | Dynamika C, M | Platí „pokles o více než 50 %" (§5.1), nebo „o 50 a více" (Tab. 1 C)? Jak naložit se 2–3 lety dat (shodná / překrývající se okna)? „Po sobě následující" = roky, nebo průzkumy? „První tři roky" = od začátku monitoringu, nebo v hodnoceném období? Má Tab. 1 M znít jako §5.1? | R-01, R-03, R-11 |
| A-2 | Dynamika L | Který vzorec (A / B / C)? Výchozí hodnota po čtyřech průzkumech — pevná, nebo klouzavá? Jen larvy? | R-12 |
| A-3 | Počet | Oba odlovy, nebo jen první? Spatření ano/ne? *Lampetra*: bodový 2. odlov na náplavech přičítat? Hustota (ind./ha), nebo počet? Odhad z odlovů s odstraňováním ne? | R-14 |
| A-4 | Vitalita | Méně než tři průzkumy ⇒ špatný, nebo neznámý? *Misgurnus* nesplní nikde — upravit skupiny? *Lampetra*: jen larvy, oddělit dospělce; je skupina < 5 cm reálně dosažitelná? | R-13 |
| A-5 | Délky | *Cottus*: SL, nebo TL? Hranice „< 5 cm" ostře (50 mm patří do 2. skupiny)? | R-06 |
| A-6 | Stanoviště | Převod pásem pokryvnosti středy pásem? C „> 50", nebo „≥ 50"? M 10, nebo 11 %; plocha dna, nebo lokality; jen submerzní, nebo i emerzní? L trdliště = štěrkopísčité proudné úseky (§4.1)? Plochy `odhad_j_n` / `odhad_p_u` vztahovat k délce celého úseku? | R-02, R-05 |
| A-7 | Migrace | Odkud brát „přes celý profil" a okolí ~ 500 m? Která databáze / hodnocení celého toku? Platí přednost i u *Cottus*? Kterou bariéru v čase (poslední průzkum, celé období)? | R-07 |
| A-8 | EVL | Pravidlo pro 1–2 hodnocené lokality? *Cottus*: počítat lokality „v příznivém stavu" i se zhoršeným stanovištěm? | R-10 |
| A-9 | Rozsah | Kokořínsko u piskoře; data piskoře v MOD; má kvalita vody vstoupit do hodnocení? | R-09, R-15, R-19 |
| A-10 | Redakce | Literatura a překlepy | R-18 |
