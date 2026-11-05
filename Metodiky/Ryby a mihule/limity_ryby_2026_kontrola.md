# Protokol převodu limitů ryb a mihulí — metodika EVL 2026

**Generováno:** `R/05_metodiky/55_ryby_limity_2026.R` · 2026-11-05 · metodika EVL 2026 (Hanel, Muška), rozhodnutí 2026-11-05.
**Registr:** [harmonizace_registr.md](harmonizace_registr.md) · **Revize:** [revize_metodika_ryby.md](revize_metodika_ryby.md)

Skript zapisuje **živé** `limity_ryby.csv` a `cis_ryby_delky_strukt.csv` (rozhodnutí Z-13,
2026-11-05). Řádky ostatních druhů a diff vycházejí ze stavu před metodikou 2026
(`35a3d99`). Nové indikátory počítá kód z patche
[patche/ryby_metodika_2026.patch](patche/ryby_metodika_2026.patch), aplikovaného týmž rozhodnutím.

## Rozhodnutí zadavatele, ze kterých převod vychází (2026-11-05)

| # | Otázka | Rozhodnutí | Dopad na limity |
|---|---|---|---|
| Z-2 | vzorec dynamiky *Lampetra* | C — oba poslední odlovy pod polovinou výchozí hodnoty | `POP_DYN min 50` beze změny; hodnota = 100 × max(dva poslední) / výchozí |
| Z-3 | počty odlovů z `POZNAMKA` | ano, přechodně | info `POP_POCETODLOV`, `POP_POCETODLOVZDROJ` |
| Z-4 | `POP_PRESENCE` | `info` | `POP_PRESENCE;info` |
| Z-5 | prázdný tag bariér | neznámo | `STA_MIGPROSTUP` (chu) neznámý, není-li výška známa u všech lokalit |
| Z-6 | EVL s 1–2 lokalitami | doslovně 75 % | `LOK_PROCDOBR min 75`; výjimka „při třech dvě“ v `25` |
| Z-7 | sezóna a podmínky | jen příznak | info `STA_PODMINKYODLOVU` |
| Z-11 | převod pásem | potvrzeno, zapsat do protokolu | viz níže |
| Z-13 | aplikace návrhu | ano | zapsáno do živých souborů; samostatné `*_2026.csv` zrušeny |

## Převod pásem pokryvnosti na číslo (rozhodnutí Z-11)

Data nesou pokryvnost substrátů (`<rsub_*>`) a vodní vegetace (`<veg_tok_sub>`) v šesti pásmech.
Převod je sladěný s ostatními skupinami:

- **obojživelníci (H-60):** kategorie relativní početnosti se převádí na **medián kategorie**;
- **cévnaté rostliny (PR-12):** *součet* měr poškození přes typy se počítá ze **středů pásem**;
  **(PR-16):** *jedno* pásmo stanovištního indikátoru se hodnotí **výčtem přípustných kategorií** (`val`).

Pro ryby tedy:

| pásmo v datech | klíč v kódu | dolní mez | horní mez | střed pro součet |
|---|---|---|---|---|
| Vzácný (0-10 %) | vzácný | 0 | 10 | 5 |
| Běžný (11-25 %) | běžný | 11 | 25 | 18 |
| Hojný (26-50 %) | hojný | 26 | 50 | 38 |
| Velmi hojný (51-75 %) | velmi hojný | 51 | 75 | 63 |
| Převažující (> 75 %) | převažující | 76 | 99 | 87.5 |
| Dominantní (100%) | dominantní | 100 | 100 | 100 |

1. **Součtový parametr** — *Cottus*: kameny + balvany + štěrk; *Misgurnus*: bahno + písek
   (`STA_DNOTYPSOUCETPROCENT`). Hodnota = součet **středů** pásem; substrát, který v `<sub_dno>`
   není uveden, = 0; substrát uvedený bez pásma ⇒ hodnota `NA` (neznámá); strop 100 %.
   Limit `min 50`. Metodika *Cottus* §5.1 má „> 50 %“, Tab. 1 „minimálně 50 %“ (R-02); součet
   středů tří pásem z {0, 5, 18, 38, 63, 87,5, 100} hodnotu přesně 50 dát nemůže, takže `min 50`
   a „> 50“ dávají totéž a ostrá nerovnost, kterou formát nezná, není potřeba.
2. **Jednopásmový parametr** — *Misgurnus* submerzní vegetace (`STA_POKRVEGSUBMERZ`). Hodnota =
   klíč pásma; `<veg_tok>` bez submerzní vegetace = `bez vegetace`. Limit `val` = výčet pásem od
   *Běžný (11-25 %)* výše. Práh 11 % (§5.1) leží **přesně na hranici pásem**, takže výčet je
   přesný; práh 10 % (Tab. 1) by padl dovnitř pásma 0–10 % a rozhodnout jej nelze (R-02).
3. **Plochy *Lampetra*** nejsou pásma: `odhad / délka úseku × 100` (m² na 100 m toku), limit `min 10`;
   bez délky úseku ⇒ `NA`.

### Kontrola slovníku a citlivost (Monitoring druhů ČR od 2013)

Rozpoznání štítků pásem (každý štítek v datech musí dát klíč):

| tag | hodnota | pasmo | rozpoznano | zaznamu |
|---|---|---|---|---|
| rsub_bah | Běžný (11-25 %) | běžný | TRUE | 134 |
| rsub_bah | Dominantní (100%) | dominantní | TRUE | 107 |
| rsub_bah | Hojný (26-50 %) | hojný | TRUE | 111 |
| rsub_bah | Převažující (> 75 %) | převažující | TRUE | 43 |
| rsub_bah | Velmi hojný (51-75 %) | velmi hojný | TRUE | 30 |
| rsub_bah | Vzácný (0-10 %) | vzácný | TRUE | 140 |
| rsub_bal | Běžný (11-25 %) | běžný | TRUE | 253 |
| rsub_bal | Hojný (26-50 %) | hojný | TRUE | 68 |
| rsub_bal | Převažující (> 75 %) | převažující | TRUE | 7 |
| rsub_bal | Velmi hojný (51-75 %) | velmi hojný | TRUE | 17 |
| rsub_bal | Vzácný (0-10 %) | vzácný | TRUE | 290 |
| rsub_kam | Běžný (11-25 %) | běžný | TRUE | 328 |
| rsub_kam | Hojný (26-50 %) | hojný | TRUE | 656 |
| rsub_kam | Převažující (> 75 %) | převažující | TRUE | 36 |
| rsub_kam | Velmi hojný (51-75 %) | velmi hojný | TRUE | 208 |
| rsub_kam | Vzácný (0-10 %) | vzácný | TRUE | 167 |
| rsub_pis | Běžný (11-25 %) | běžný | TRUE | 604 |
| rsub_pis | Dominantní (100%) | dominantní | TRUE | 7 |
| rsub_pis | Hojný (26-50 %) | hojný | TRUE | 392 |
| rsub_pis | Převažující (> 75 %) | převažující | TRUE | 20 |
| rsub_pis | Velmi hojný (51-75 %) | velmi hojný | TRUE | 117 |
| rsub_pis | Vzácný (0-10 %) | vzácný | TRUE | 339 |
| rsub_st | Běžný (11-25 %) | běžný | TRUE | 438 |
| rsub_st | Hojný (26-50 %) | hojný | TRUE | 766 |
| rsub_st | Převažující (> 75 %) | převažující | TRUE | 75 |
| rsub_st | Velmi hojný (51-75 %) | velmi hojný | TRUE | 99 |
| rsub_st | Vzácný (0-10 %) | vzácný | TRUE | 110 |
| veg_tok_sub | Běžný (11-25 %) | běžný | TRUE | 371 |
| veg_tok_sub | Dominantní (100%) | dominantní | TRUE | 3 |
| veg_tok_sub | Hojný (26-50 %) | hojný | TRUE | 63 |
| veg_tok_sub | Převažující (> 75 %) | převažující | TRUE | 1 |
| veg_tok_sub | Velmi hojný (51-75 %) | velmi hojný | TRUE | 8 |
| veg_tok_sub | Vzácný (0-10 %) | vzácný | TRUE | 93 |

Citlivost součtového parametru na volbu bodu uvnitř pásma (práh 50 %):

| druh | zaznamu | splneno_stredy | jiste_splneno | jiste_nesplneno | nejiste | presne_prah |
|---|---|---|---|---|---|---|
| Cottus gobio | 931 | 771 | 560 | 89 | 282 | 0 |
| Misgurnus fossilis | 235 | 168 | 160 | 25 | 50 | 0 |

„Nejisté“ = součet dolních mezí < 50 a součet horních mezí ≥ 50 — o těchto záznamech rozhoduje volba středu.

*Misgurnus* — submerzní vegetace:

| hodnota | n | splneno |
|---|---|---|
| bez vegetace | 95 | FALSE |
| běžný | 98 | TRUE |
| dominantní | 3 | TRUE |
| hojný | 20 | TRUE |
| velmi hojný | 7 | TRUE |
| vzácný | 1 | FALSE |
| (tag chybí) | 12 | neznámý |

## Řádky tří druhů (metodika 2026)

| DRUH | ID_IND | TYP_IND | LIM_IND | JEDNOTKA | KLIC | UROVEN | nalez |
|---|---|---|---|---|---|---|---|
| Cottus gobio | POP_DYN | min | 50 | % | ano | lok | H-72, H-73, H-47 |
| Cottus gobio | POP_VITALITA3 | min | 2 | průzkumy se všemi délkovými skupinami | ano | lok | H-79 |
| Cottus gobio | POP_VITALITA | info | NA | délkové skupiny v roce | NA | lok | H-79 |
| Cottus gobio | POP_PRESENCE | info | NA | přítomnost druhu | NA | lok | H-80 |
| Cottus gobio | POP_POCET | info | NA | jedinci | NA | lok | H-55 |
| Cottus gobio | POP_POCETODLOV | info | NA | jedinci (1. + 2. odlov) | NA | lok | H-74 |
| Cottus gobio | POP_POCETODLOVZDROJ | info | NA | NA | NA | lok | H-74 |
| Cottus gobio | STA_PODMINKYODLOVU | info | NA | NA | NA | lok | H-84 |
| Cottus gobio | STA_MIGBARPOCET | info | NA | bariéry | NA | lok | H-83 |
| Cottus gobio | STA_MIGBARVYS | info | NA | cm | NA | lok | H-83 |
| Cottus gobio | STA_DNOTYPSOUCETPROCENT | min | 50 | % | ne | lok | H-76, H-77 |
| Cottus gobio | LOK_PROCDOBR | min | 75 | % lokalit v dobrém stavu | ano | chu | H-81 |
| Cottus gobio | STA_MIGPROSTUP | max | 10 | cm | ano | chu | H-83 |
| Lampetra planeri | POP_DYN | min | 50 | % | ano | lok | H-72, H-73, H-47 |
| Lampetra planeri | POP_VITALITA3 | min | 2 | průzkumy se všemi délkovými skupinami | ano | lok | H-79 |
| Lampetra planeri | POP_VITALITA | info | NA | délkové skupiny v roce | NA | lok | H-79 |
| Lampetra planeri | POP_PRESENCE | info | NA | přítomnost druhu | NA | lok | H-80 |
| Lampetra planeri | POP_POCET | info | NA | jedinci | NA | lok | H-55, H-49 |
| Lampetra planeri | POP_POCET | info | NA | larvy | NA | lok | H-55, H-49 |
| Lampetra planeri | POP_POCETODLOV | info | NA | larvy (1. + 2. odlov) | NA | lok | H-74 |
| Lampetra planeri | POP_POCETODLOVZDROJ | info | NA | NA | NA | lok | H-74 |
| Lampetra planeri | STA_PODMINKYODLOVU | info | NA | NA | NA | lok | H-84 |
| Lampetra planeri | STA_MIGBARPOCET | info | NA | bariéry | NA | lok | H-83 |
| Lampetra planeri | STA_MIGBARVYS | info | NA | cm | NA | lok | H-83 |
| Lampetra planeri | STA_ODHADCELKOVEPLOCHYJEMNYCHNAPLAVUVHODNYCHPROVYVOJLAREV | min | 10 | m2 na 100 m toku | ne | lok | H-76, H-77, H-54 |
| Lampetra planeri | STA_ODHADCELKOVEPLOCHYPROUDNYCHUSEKUSESTERKOPISKOVYMDNEMVHODNYCHPROTRENI | min | 10 | m2 na 100 m toku | ne | lok | H-76, H-77, H-54 |
| Lampetra planeri | LOK_PROCDOBR | min | 75 | % lokalit v dobrém stavu | ano | chu | H-81 |
| Lampetra planeri | STA_MIGPROSTUP | max | 5 | cm | ano | chu | H-83 |
| Misgurnus fossilis | POP_DYN | min | 50 | % | ano | lok | H-72, H-73, H-47 |
| Misgurnus fossilis | POP_VITALITA3 | min | 2 | průzkumy se všemi délkovými skupinami | ano | lok | H-79 |
| Misgurnus fossilis | POP_VITALITA | info | NA | délkové skupiny v roce | NA | lok | H-79 |
| Misgurnus fossilis | POP_PRESENCE | info | NA | přítomnost druhu | NA | lok | H-80 |
| Misgurnus fossilis | POP_POCET | info | NA | jedinci | NA | lok | H-55 |
| Misgurnus fossilis | POP_POCETODLOV | info | NA | jedinci (1. + 2. odlov) | NA | lok | H-74 |
| Misgurnus fossilis | POP_POCETODLOVZDROJ | info | NA | NA | NA | lok | H-74 |
| Misgurnus fossilis | STA_PODMINKYODLOVU | info | NA | NA | NA | lok | H-84 |
| Misgurnus fossilis | STA_MIGBARPOCET | info | NA | bariéry | NA | lok | H-83 |
| Misgurnus fossilis | STA_MIGBARVYS | info | NA | cm | NA | lok | H-83 |
| Misgurnus fossilis | STA_DNOTYPSOUCETPROCENT | min | 50 | % | ne | lok | H-76 |
| Misgurnus fossilis | STA_POKRVEGSUBMERZ | val | běžný | pásmo pokryvnosti | ne | lok | H-76 |
| Misgurnus fossilis | STA_POKRVEGSUBMERZ | val | hojný | pásmo pokryvnosti | ne | lok | H-76 |
| Misgurnus fossilis | STA_POKRVEGSUBMERZ | val | velmi hojný | pásmo pokryvnosti | ne | lok | H-76 |
| Misgurnus fossilis | STA_POKRVEGSUBMERZ | val | převažující | pásmo pokryvnosti | ne | lok | H-76 |
| Misgurnus fossilis | STA_POKRVEGSUBMERZ | val | dominantní | pásmo pokryvnosti | ne | lok | H-76 |
| Misgurnus fossilis | LOK_PROCDOBR | min | 75 | % lokalit v dobrém stavu | ano | chu | H-81 |
| Misgurnus fossilis | STA_MIGPROSTUP | max | 10 | cm | ano | chu | H-83 |

## Souhrn rozdílu proti `limity_ryby.csv` před metodikou 2026 (`35a3d99`)

| DRUH | beze změny | odebráno | přidáno | změněno |
|---|---|---|---|---|
| Cottus gobio | 3 | 5 | 6 | 4 |
| Lampetra planeri | 2 | 1 | 5 | 7 |
| Misgurnus fossilis | 2 | 4 | 8 | 4 |

| DRUH | UROVEN | ID_IND | zmena | puvodni | novy | nalez | pozn |
|---|---|---|---|---|---|---|---|
| Cottus gobio | chu | LOK_PROCDOBR | přidáno |  | min \| 75 \| % lokalit v dobrém stavu \| ano | H-81 |  |
| Cottus gobio | chu | STA_MIGPROSTUP | přidáno |  | max \| 10 \| cm \| ano | H-83 |  |
| Cottus gobio | lok | POP_DYN | beze změny | min \| 50 \| % \| ano | min \| 50 \| % \| ano | H-72, H-73, H-47 | limit beze změny; mění se výpočet v 21_1 |
| Cottus gobio | lok | POP_POCET | beze změny | info \| NA \| jedinci \| NA | info \| NA \| jedinci \| NA | H-55 |  |
| Cottus gobio | lok | POP_POCETODLOV | přidáno |  | info \| NA \| jedinci (1. + 2. odlov) \| NA | H-74 |  |
| Cottus gobio | lok | POP_POCETODLOVZDROJ | přidáno |  | info \| NA \| NA \| NA | H-74 |  |
| Cottus gobio | lok | POP_PRESENCE | změněno | val \| ano \| přítomnost druhu \| ano | info \| NA \| přítomnost druhu \| NA | H-80 |  |
| Cottus gobio | lok | POP_VITALITA | změněno | min \| 3 \| NA \| ano | info \| NA \| délkové skupiny v roce \| NA | H-79 |  |
| Cottus gobio | lok | POP_VITALITA3 | přidáno |  | min \| 2 \| průzkumy se všemi délkovými skupinami \| ano | H-79 |  |
| Cottus gobio | lok | STA_DNO | odebráno | val \| balvany \| NA \| ne \|\| val \| kameny \| NA \| ne \|\| val \| štěrk \| NA \| ne |  | H-76 - metodika nezna (C: soucet pokryvnosti; M: podil bahna a pisku) |  |
| Cottus gobio | lok | STA_DNOPOCET | odebráno | min \| 3 \| NA \| ne |  | H-77 - sirotek bez opory v metodice |  |
| Cottus gobio | lok | STA_DNOPREF | odebráno | val \| kameny \| NA \| ne \|\| val \| písek \| NA \| ne \|\| val \| štěrk \| NA \| ne |  | H-76 - metodika nezna |  |
| Cottus gobio | lok | STA_DNOTYPSOUCETPROCENT | beze změny | min \| 50 \| % \| ne | min \| 50 \| % \| ne | H-76, H-77 | dosud sirotek; výpočet nově v 21_1 |
| Cottus gobio | lok | STA_MIGBARPOCET | změněno | max \| 1 \| NA \| ne | info \| NA \| bariéry \| NA | H-83 |  |
| Cottus gobio | lok | STA_MIGBARVYS | změněno | max \| 10 \| cm \| ne | info \| NA \| cm \| NA | H-83 |  |
| Cottus gobio | lok | STA_PODMINKYODLOVU | přidáno |  | info \| NA \| NA \| NA | H-84 |  |
| Cottus gobio | lok | STA_PROUD | odebráno | val \| mírný \| NA \| ne \|\| val \| peřeje \| NA \| ne \|\| val \| tůně \| NA \| ne |  | H-76 - metodika nezna |  |
| Cottus gobio | lok | STA_PROUDPOCETTYPU | odebráno | min \| 2 \| NA \| ne |  | H-76 - metodika nezna |  |
| Lampetra planeri | chu | LOK_PROCDOBR | změněno | min \| 50 \| % lokalit v dobrém stavu \| ano | min \| 75 \| % lokalit v dobrém stavu \| ano | H-81 |  |
| Lampetra planeri | chu | STA_MIGPROSTUP | přidáno |  | max \| 5 \| cm \| ano | H-83 |  |
| Lampetra planeri | lok | POP_DYN | beze změny | min \| 50 \| % \| ano | min \| 50 \| % \| ano | H-72, H-73, H-47 | limit beze změny; mění se výpočet v 21_1 |
| Lampetra planeri | lok | POP_POCET | beze změny | info \| NA \| jedinci \| NA \|\| info \| NA \| larvy \| NA | info \| NA \| jedinci \| NA \|\| info \| NA \| larvy \| NA | H-55, H-49 |  |
| Lampetra planeri | lok | POP_POCETODLOV | přidáno |  | info \| NA \| larvy (1. + 2. odlov) \| NA | H-74 |  |
| Lampetra planeri | lok | POP_POCETODLOVZDROJ | přidáno |  | info \| NA \| NA \| NA | H-74 |  |
| Lampetra planeri | lok | POP_PRESENCE | změněno | val \| ano \| přítomnost druhu \| ano | info \| NA \| přítomnost druhu \| NA | H-80 |  |
| Lampetra planeri | lok | POP_VITALITA | změněno | min \| 3 \| kategorie \| ano | info \| NA \| délkové skupiny v roce \| NA | H-79 |  |
| Lampetra planeri | lok | POP_VITALITA3 | přidáno |  | min \| 2 \| průzkumy se všemi délkovými skupinami \| ano | H-79 |  |
| Lampetra planeri | lok | STA_DNOTYP | odebráno | val \| kompaktní jílové dno \| NA \| ne |  | H-45, H-76 - metodika nezna |  |
| Lampetra planeri | lok | STA_MIGBARPOCET | změněno | max \| 1 \| NA \| ne | info \| NA \| bariéry \| NA | H-83 |  |
| Lampetra planeri | lok | STA_MIGBARVYS | změněno | max \| 5 \| cm \| ne | info \| NA \| cm \| NA | H-83 |  |
| Lampetra planeri | lok | STA_ODHADCELKOVEPLOCHYJEMNYCHNAPLAVUVHODNYCHPROVYVOJLAREV | změněno | val \| 10 \| m2 \| ne | min \| 10 \| m2 na 100 m toku \| ne | H-76, H-77, H-54 | dosud sirotek; výpočet nově v 21_1 (na 100 m toku) |
| Lampetra planeri | lok | STA_ODHADCELKOVEPLOCHYPROUDNYCHUSEKUSESTERKOPISKOVYMDNEMVHODNYCHPROTRENI | změněno | val \| 10 \| m2 \| ne | min \| 10 \| m2 na 100 m toku \| ne | H-76, H-77, H-54 | dosud sirotek; výpočet nově v 21_1 (na 100 m toku) |
| Lampetra planeri | lok | STA_PODMINKYODLOVU | přidáno |  | info \| NA \| NA \| NA | H-84 |  |
| Misgurnus fossilis | chu | LOK_PROCDOBR | přidáno |  | min \| 75 \| % lokalit v dobrém stavu \| ano | H-81 |  |
| Misgurnus fossilis | chu | STA_MIGPROSTUP | přidáno |  | max \| 10 \| cm \| ano | H-83 |  |
| Misgurnus fossilis | lok | POP_DYN | beze změny | min \| 50 \| % \| ano | min \| 50 \| % \| ano | H-72, H-73, H-47 | limit beze změny; mění se výpočet v 21_1 |
| Misgurnus fossilis | lok | POP_POCET | beze změny | info \| NA \| jedinci \| NA | info \| NA \| jedinci \| NA | H-55 |  |
| Misgurnus fossilis | lok | POP_POCETODLOV | přidáno |  | info \| NA \| jedinci (1. + 2. odlov) \| NA | H-74 |  |
| Misgurnus fossilis | lok | POP_POCETODLOVZDROJ | přidáno |  | info \| NA \| NA \| NA | H-74 |  |
| Misgurnus fossilis | lok | POP_PRESENCE | změněno | val \| ano \| přítomnost druhu \| ano | info \| NA \| přítomnost druhu \| NA | H-80 |  |
| Misgurnus fossilis | lok | POP_VITALITA | změněno | min \| 3 \| kategorie \| ano | info \| NA \| délkové skupiny v roce \| NA | H-79 |  |
| Misgurnus fossilis | lok | POP_VITALITA3 | přidáno |  | min \| 2 \| průzkumy se všemi délkovými skupinami \| ano | H-79 |  |
| Misgurnus fossilis | lok | STA_DNO | odebráno | val \| bahno \| NA \| ne \|\| val \| písek \| NA \| ne |  | H-76 - metodika nezna (C: soucet pokryvnosti; M: podil bahna a pisku) |  |
| Misgurnus fossilis | lok | STA_DNOPOCETTYPU | odebráno | min \| 2 \| NA \| ne |  | H-76 - metodika nezna |  |
| Misgurnus fossilis | lok | STA_DNOTYPSOUCETPROCENT | přidáno |  | min \| 50 \| % \| ne | H-76 |  |
| Misgurnus fossilis | lok | STA_MIGBARPOCET | změněno | max \| 1 \| NA \| ne | info \| NA \| bariéry \| NA | H-83 |  |
| Misgurnus fossilis | lok | STA_MIGBARVYS | změněno | max \| 10 \| cm \| ne | info \| NA \| cm \| NA | H-83 |  |
| Misgurnus fossilis | lok | STA_PODMINKYODLOVU | přidáno |  | info \| NA \| NA \| NA | H-84 |  |
| Misgurnus fossilis | lok | STA_POKRVEGSUBMERZ | přidáno |  | val \| běžný \| pásmo pokryvnosti \| ne \|\| val \| dominantní \| pásmo pokryvnosti \| ne \|\| val \| hojný \| pásmo pokryvnosti \| ne \|\| val \| převažující \| pásmo pokryvnosti \| ne \|\| val \| velmi hojný \| pásmo pokryvnosti \| ne | H-76 |  |
| Misgurnus fossilis | lok | STA_PROUD | odebráno | val \| kaskády \| NA \| ne \|\| val \| mírný \| NA \| ne \|\| val \| peřeje \| NA \| ne \|\| val \| tůně \| NA \| ne |  | H-76 - metodika nezna |  |
| Misgurnus fossilis | lok | STA_VEGETACE | odebráno | val \| submerzní \| NA \| ne |  | H-76 - nahrazeno STA_POKRVEGSUBMERZ |  |

## Délkové skupiny (`cis_ryby_delky_strukt.csv`)

Metodika Tab. 1, pozn. a. Hranice „< 5 cm“ je ostrá; délky jsou v celých mm, proto horní mez
1. skupiny 49,99. Typ délky (SL × TL u *Cottus*) zůstává otázkou na autory (R-06).

| DRUH | KAT | puvodni | novy |
|---|---|---|---|
| Cottus gobio | 1 | 0.01 - 60 mm | 0.01 - 49.99 mm |
| Cottus gobio | 2 | 60.01 - 90 mm | 50 - 90 mm |
| Cottus gobio | 3 | 90.01 - Inf mm | 90.01 - Inf mm |
| Lampetra planeri | 1 | 0.01 - 60 mm | 0.01 - 49.99 mm |
| Lampetra planeri | 2 | 60.01 - 120 mm | 50 - 120 mm |
| Lampetra planeri | 3 | 120.01 - Inf mm | 120.01 - Inf mm |
| Misgurnus fossilis | 1 | 0.01 - 50 mm | 0.01 - 49.99 mm |
| Misgurnus fossilis | 2 | 50.01 - 150 mm | 50 - 150 mm |
| Misgurnus fossilis | 3 | 150.01 - Inf mm | 150.01 - Inf mm |

## Kontrola formátu

| kontrola | vysledek |
|---|---|
| řádky ostatních druhů shodné (text) | TRUE |
| řádky ostatních druhů shodné (bajty CP1250) | TRUE |
| konce řádků CRLF | TRUE |
| zpětné načtení `read_csv2` jako v `00_n2k_config.R` | OK (stejné sloupce) |
| řádků nově / před metodikou 2026 | 274 / 271 |
| H-53: vyplněný limit bez `KLIC`/`UROVEN` | 0 řádků |
| `ind_id` v `cis_indikatory_popis.csv` pro nové `ID_IND` | 2 z 16 má kód (H-87) |
