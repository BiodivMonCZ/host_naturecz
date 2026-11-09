# Integrace a harmonizace kódu druhů — 2026-11-09

Větev `druhy-integrace-202611` (z `main` @ `ea0e450`). Cíl: spojit veškerou práci na druzích
do jedné větve, kód projít a sjednotit — kratší, kde to jde, srozumitelnější všude —
a zachovat **přesně tytéž výsledky hodnocení**, kromě změn podložených zaznamenaným
rozhodnutím. Priorita: **správnost > srozumitelnost > stručnost**.

**Ověřeno na ostrých datech:** export NDOP 2026-05-10 (`../host_data/export_data_evl.csv`
+ `export_data_zprap.csv`), plný běh kaskády `21_1` → `27` pro **všech 82 druhů**, které mají
limity i záznamy (hodnocený rok 2025). Syntetická data jen doplňkově (test jedné úpravy).

## 1. Co bylo sloučeno, přeneseno a vynecháno

| Větev | Naložení | Důvod |
|---|---|---|
| `ryby-metodika-2026` (`fedd237`) | **sloučeno** `merge --no-ff` (`960b4ab`) | lineárně obsahuje `cevnate-metodika-2026` (`35a3d99`), `harmonizace-cevnate` (`83f3727`, jen lokální) a `druhy-202609` (`a172ba1`); bez squashe/rebase, hashe v registrech platí; konflikt žádný (`main` od `f9540f9` měnil jen `load_vmb.R` a stanoviště, `load_vmb()` druhy nepoužívají) |
| `eriogaster-catax-2025-01` (`84a7106`) | **přeneseny 2 buňky limitů** *E. catax* (`9350260`, `aaf639d`) | větev přeřadila řádky celého `limity_vse.csv` a vůči své základně měnila jen *catax* `POP_POCET`; ostatní řádky *catax* jsou v `main` novější (`91dce53`, H-33, H-67) — viz H-96. Vynechány: `Data/Temp/*.csv`, natvrdo `species_list <- "Eriogaster catax"` v `27`, graf mrtvého dřeva v `32` (stanoviště) |
| `ryby-indikatory` (`470c439`) | **nic nepřeneseno** | všech 12 tagů je ve `21_1` (pod jinými názvy sloupců); `<ohad_p_u>` byl ve větvi překlep (v datech jen `<odhad_p_u>`, 2 462×, `21_1` ho čte); `<prit_host_mlz>` → `STA_DALSIPARAMETRY` *Rhodeus amarus* je otevřené rozhodnutí H-46 (tag nese `Ano`/`Ne`, limit `anodonta`) |
| `202608-obojzivelnici`, `stanoviste-hotfix-20251016-2` | — | už v `main` |
| `stanoviste-revize-2025Q1-3` | — | stanoviště, mimo rozsah |

## 2. Regrese — výsledek

Běhy v izolovaných kopiích (`git worktree`) mimo pracovní strom; `Data/Temp/` ani
`Outputs/Data/` repozitáře nebyly dotčeny. Srovnání [`skripty/porovnej_behy.R`](skripty/porovnej_behy.R):
každý z 11 souborů `Data/Temp/*.csv` (záznamy s indikátory `n2k_druhy`, `_lim`, `_lok`, `_chu`,
`_pocetnost`, období, poslední nález) a 6 exportů (`nal`/`lok`/`chu` × Windows-1250/UTF-8)
**řádek po řádku jako text** včetně pořadí; maskuje se jen datum běhu (`datum_hodnoceni`).

| Běh | Kód | Druhy | Proti | Výsledek (17 souborů) |
|---|---|---|---|---|
| `base_ALL` | `fedd237` (tip `ryby-metodika-2026`, **před**) | všech 82 | — | základ, 41 min |
| `int_merge` | `960b4ab` (merge) | 82 | `base_ALL` | **17 / 17 shodných** — merge nic nemění, kaskáda je deterministická |
| `int_catax` | `aaf639d` (merge + 2 buňky *catax*) | 82 | `base_ALL` | 8 shodných; 9 se liší **výhradně řádky *Eriogaster catax*** (ověřeno po řádcích ve všech 6 exportech i v `_lim`, `_lok`, `_chu`) — viz níže |
| `int_tip1` | `7483428` (refaktoring r1–r8 + úklid) | 82 | `int_catax` | **17 / 17 shodných** |
| `int_final` | `22a6ee1` (konečný stav; kód = `da0a304`) | 82 | `int_catax` | **17 / 17 shodných** |
| `sub_catax` | `aaf639d` | 17 vybraných¹ | — | reference podmnožiny |
| `sub_r2` | `afa0553` (r1 tagy, r2 jednotky) | 17 | `sub_catax` | **17 / 17 shodných** |
| `sub_r4` | `440480c` (+ r3 exporty, r4 `24`/`25`) | 17 | `sub_catax` | **17 / 17 shodných** |
| `sub_r6` | `a9e2900` (+ r5 `KOD_LOKAL`, r6 úklid `21_1`) | 17 | `sub_catax` | **17 / 17 shodných** |

¹ *T. cristatus*, *B. variegata*, *B. bombina*, *Cottus gobio*, *Lampetra planeri*, *Misgurnus fossilis*,
*Cobitis elongatoides*, čtyři cévnaté z `Cevnate_4sp_20261105` (*Pulsatilla grandis*, *Campanula bohemica*,
*Cypripedium calceolus*, *Klasea lycopifolia*), *E. catax*, *E. aurinia*, *Lucanus cervus*,
*Ophiogomphus cecilia*, *Myotis myotis*, *Barbastella barbastellus* — každá skupina a každá dotčená větev.

**Každý refaktorovací commit** je tedy ověřen buď samostatně, nebo v kontrolním bodě
(r1–r2, r3–r4, r5–r6 na podmnožině; r1–r8 a r1–r9 + komentáře na všech 82 druzích).
Commity `88ba11a` (`55`, mimo kaskádu), `7483428` (úklid `Outputs/Test`) a registry kaskádu nemění.

**Zamýšlené rozdíly — jen H-96 (*Eriogaster catax*):**

| Úroveň | Před | Po |
|---|---|---|
| DP (304) — `CELKOVE_HODNOCENI` | 27 dobrý · 277 špatný | **beze změny** |
| DP — řádek `POP_POCET` | `info`, bez stavu | `min 30`, `KLIC = ne`: 234 nesplněno · 5 splněno · 65 neznámý |
| nálezy (`_lim`, export `nal`) | 659 řádků *catax* | 891 řádků (`POP_POCET` se nově vyhodnocuje po nálezech) |
| EVL (23) — `CELKOVE_HODNOCENI` | 20 dobrý · 3 zhoršený | **beze změny** |
| EVL — `STA_HABPOKRYV` | nevzniká (`UROVEN = NA`) | 23 řádků „neznámý" (pro *catax* nejsou data pokryvnosti biotopů), z toho 6 v exportu do ISOP |

**Zneplatněné běhy (přiznání):** první fronta podmnožiny po zastavení úlohy nadále běžela
a od 00:40 sdílela izolovanou kopii s druhou frontou (střídání commitů, společné `Data/Temp`).
Výsledky `sub_r1`, `sub_r3` a první verze `sub_r2`, `sub_r4`, `sub_r6` jsem proto zahodil
a kontrolní body zopakoval v čisté kopii (tabulka výše). Plné běhy `int_*` běžely v jiné
kopii a dotčeny nebyly.

Souhrny srovnání: [`srovnani/`](srovnani/).

## 3. Nové nálezy H-96 … H-103

Podrobně v [registru obojživelníků, §Integrace](../../../Metodiky/Obojzivelnici/harmonizace_registr.md#integrace-a-revize-kódu-druhů-2026-11-09)
(odkazy z registrů ryb a cévnatých rostlin).

| Nález | Závažnost | Stav | Mění výsledky? |
|---|---|---|---|
| **H-96** *E. catax*: limity z větve `eriogaster-catax-2025-01` | střední | **provedeno** (`9350260`, `aaf639d`); KLIC u `POP_POCET` = otázka Z-26 | ano, bez změny verdiktů: `POP_POCET` nově hodnocen u 304 DP (234 nesplněno), +23 řádků `STA_HABPOKRYV` „neznámý" na EVL (6 v exportu) |
| **H-97** `je_druh_pole()` vrací vždy FALSE (`sites_subjects` nemá `DRUH`/`SKUPINA`); `POP_PROCPOLE1D` se nepočítá u žádného z 20 druhů hmyzu, pravidlo Z-12 pro hmyz je v mrtvé větvi | vysoká | otevřeno, Z-27; v kódu označeno | oprava ano (14 druhů hmyzu s daty, DP i EVL) |
| **H-98** `Bolbelamus unicornis` (překlep) — druh se nehodnotí | nízká | otevřeno, Z-28 | oprava ano (1 druh, 2 záznamy) |
| **H-99** letouni: klíčové indikátory limitů (`POP_DIFZIM` …) bez výpočtu; `POP_PRESENCE_ZIMNI` vadná podmínka; `mean(a, b, c)` | vysoká | otevřeno; mrtvý výpočet v `25` odstraněn | oprava ano (5 druhů) |
| **H-100** `21_1` agreguje lokalitu bez `kod_chu` | střední | otevřeno, Z-29 | oprava pravděpodobně (neměřeno) |
| **H-101** seznam kódů `amp…` → `NA` v `02_n2k_data_druhy.R` nic nevyřazuje | nízká | otevřeno (záměr?) | podle záměru |
| **H-102** hodnocené období v exportu = rozpětí všech záznamů (od roku 1800) | střední | otevřeno, Z-30 | export ano, verdikty ne |
| **H-103** *Phengaris nausithous* a *P. teleius* se nehodnotí (seznam vede *Maculinea*) | vysoká | otevřeno, Z-28 | oprava ano (2 druhy, 6 674 záznamů) |

**Opraveno bez rozhodnutí: nic.** Žádná z nalezených vad není jednoznačným rozporem s
existujícím rozhodnutím nebo textem metodiky tak, aby oprava nepředjímala volbu zadavatele
(H-97 by aktivovalo celou logiku polí hmyzu, ne jen pravidlo Z-12; názvy druhů čekají na
H-39/H-40 od 2026-09). Kód je jen označen komentářem s číslem nálezu.

## 4. Rozhodnutí, která čekají

| # | Otázka | Možnosti | Doporučení |
|---|---|---|---|
| **Z-19** (H-91) | Platí pravidlo H-90 („neznámý klíčový indikátor ⇒ neznámý stav DP") pro všechny skupiny a roky? Registr ryb vede 2. kolo jako zodpovězené („stejné pravidlo pro všechny roky"), zadání integrace jako otevřené. | (a) všechny skupiny i roky — dnešní kód / (b) bez obojživelníků (kap. 2.1 metodiky) / (c) jen od určitého roku | potvrdit (a), nebo (b) kvůli textu metodiky obojživelníků; **kód beze změny** |
| **Z-20** (H-90) | Obdoba H-90 na úrovni EVL: má neznámý klíčový indikátor území dělat EVL neznámou? Dnes ho `25` odečte (`KLIC` → `ne`). | zavést / ponechat | rozhodnout po Z-19; **kód beze změny** |
| **Z-26** (H-96) | *E. catax* `POP_POCET` `min 30` hnízd: platí, a je klíčový? | `KLIC = ne` (dnes) / `ano` / vrátit `info` | `ne` |
| **Z-27** (H-97) | Opravit rozpoznání druhů vázaných na mapovací pole? | opravit + testovací běh hmyzu / ponechat | opravit |
| **Z-28** (H-98, H-103, s cévnatými H-32, H-40, S-1) | Sdílená tabulka synonym názvů pro celou kaskádu? | tabulka synonym / přejmenovat v limitech / ponechat | tabulka (rozšířit `cis_druhy_nazvy.csv`) |
| **Z-29** (H-100) | Seskupovat lokalitu ve `21_1` i podle `kod_chu`? | ano / ne | ano, po změření |
| **Z-30** (H-102) | Hodnocené období v exportu podle hodnoceného okna? | okno / ponechat | okno |
| H-99 | Výpočet indikátorů letounů podle limitů | autoři metodiky letounů | — |
| H-101 | Co měl seznam kódů `amp…` → `NA` dělat (vyřadit lokality?) | — | — |

Otevřené zůstávají i Z-21, Z-24, Z-25 (registr ryb) — kód beze změny.

## 5. Počty řádků

| Skript | `main` | `fedd237` (před) | po | rozdíl |
|---|---:|---:|---:|---:|
| `20_n2k_druhy_run.R` | 93 | 93 | 93 | 0 |
| `21_1_n2k_druhy_akce.R` | 1 873 | 2 288 | 1 950 | −338 |
| `21_2_n2k_druhy_akce_lim.R` | 319 | 353 | 353 | 0 |
| `22_n2k_obdobi.R` | 54 | 54 | 54 | 0 |
| `23_n2k_posledni_nalez.R` | 56 | 56 | 25 | −31 |
| `24_n2k_druhy_lokality.R` | 280 | 412 | 410 | −2 |
| `25_n2k_druhy_uzemi.R` | 538 | 653 | 596 | −57 |
| `27_n2k_druhy_zapis.R` | 821 | 832 | 732 | −100 |
| **`R/02_druhy` celkem** | **4 034** | **4 741** | **4 213** | **−528 (−11 %)** |
| `R/00_config/02_n2k_data_druhy.R` | 260 | 307 | 296 | −11 |
| `R/05_metodiky/55_ryby_limity_2026.R` | — | 420 | 422 | +2 |

Co se zkrátilo (vše bez změny výsledků): 63 bloků extrakce tagů → `tag_text()` / `tag_cislo()` /
`tag_vyber()` (`21_1`); sedm dotazů na jednotky limitů → `jednotky_limitu()`; zimní/letní počty
letounů → `zima_y()` / `leto_y()`; tři kopie exportu a čtyři zápisy do `Data/Temp` →
`zapis_exporty()` / `zapis_temp()` (`27`); dvě kopie `is_pole_druh` → `je_druh_pole()`, mrtvé
součty a duplicitní metadata v `25` (`META_CHU`, `habpokryv_evl()`); `23` přebírá poslední nález
z `22`; duplicitní větve `case_when` a dvojí doplnění `KOD_LOKAL` (`02`); zakomentovaný kód
a zastaralé komentáře.

**Proč tagy nejsou v jedné tabulce** (zadání ji navrhovalo): pořadí sloupců je nosné —
`21_2` převádí do dlouhého formátu sloupce v pořadí vzniku (od `POP_PRESENCE_N` a od
`ncol_orig`) a pořadí řádků pak rozhoduje u `first()` / `slice(1)` ve `24` a `25`. Jedna
tabulka by sloupce vytvořila najednou na jiném místě. Volání pomocné funkce proto zůstávají
v původních blocích (jedna řádka na tag = tabulka po skupinách). Skupinové větve nebyly
převedeny na data: liší se výpočtem, ne jen parametry, a převod by vyžadoval přepis bez
možnosti ověřit jinak než během.

## 6. Opakování srovnání na ostrých datech

Z kořene repozitáře (Git Bash), export NDOP v `../host_data/` (cesta `slozka_lokal`
v `00_n2k_config.R`). Každý plný běh ~40–50 min, ~1 GB RAM.

```bash
# 1) izolované kopie (pracovní strom ani Data/Temp repozitáře se nedotknou)
git worktree add --detach ../wt_pred fedd237                 # stav před integrací (tip ryby-metodika-2026)
git worktree add --detach ../wt_catax aaf639d                # merge + přenos catax (reference refaktoringu)
git worktree add --detach ../wt_po druhy-integrace-202611    # konečný stav

# 2) běhy kaskády 21_1 -> 27 pro všechny druhy ("ALL"; jinak názvy oddělené "|")
R=Outputs/Test/Integrace_druhy_20261109/skripty
Rscript $R/beh_kaskady.R ../wt_pred  ../behy/pred  ALL ../behy/_cache
Rscript $R/beh_kaskady.R ../wt_catax ../behy/catax ALL ../behy/_cache
Rscript $R/beh_kaskady.R ../wt_po    ../behy/po    ALL ../behy/_cache

# 3) srovnání (návratový kód 0 = všechny soubory shodné)
Rscript $R/porovnej_behy.R ../behy/catax ../behy/po    ../behy/srovnani_refaktoring.csv   # očekáváno: shodné
Rscript $R/porovnej_behy.R ../behy/pred  ../behy/catax ../behy/srovnani_catax.csv         # očekáváno: jen Eriogaster catax
Rscript $R/dopad_catax.R   ../behy/pred  ../behy/catax                                    # verdikty catax před/po

# 4) úklid
git worktree remove ../wt_pred; git worktree remove ../wt_catax; git worktree remove ../wt_po
```

`beh_kaskady.R` načítá config výraz po výrazu jako `Test/00_config_test_2sp.R` (vynechá jen
bloky, které druhová kaskáda nepoužívá: vodstvo, BiotopZvld, n2k_union, aktualizační okrsky,
redlist/invaze/expanze, rn2kcz), config cachuje podle md5 configů a `Data/Input`, výstupy
`Data/Temp/*.csv` a `Outputs/Data/druhy/*` zkopíruje do cílové složky a nový export z kopie smaže.

## Úklid `Outputs/Test`

Smazáno (`7483428`) — vygenerovaný render Quarto, nic na něj neodkazuje, lze obnovit
`quarto render README.md`:

- `Outputs/Test/Triturus_cristatus_20260828/README.html`
- `Outputs/Test/Triturus_cristatus_20260828/README_files/libs/` (12 souborů: `bootstrap/` 4,
  `clipboard/` 1, `quarto-html/` 7)

Ponecháno — každá složka je doklad, na který odkazuje registr nebo jiná zpráva:

| Složka | Doklad k (nálezy citované v jejím README) |
|---|---|
| `Triturus_cristatus_20260828` | H-21, H-22 |
| `Triturus_cristatus_20260903` | H-37 |
| `Triturus_cristatus_20260904` | H-59 … H-64 |
| `Triturus_cristatus_20260905` | H-59, H-60 |
| `TC_BV_20260909` | H-04, H-64 |
| `TC_BV_20260911_cilmon` | H-62, H-69, H-70 |
| `TC_BV_20260911_plocha50` | H-04, H-21 |
| `Ryby_20260904` | H-42, H-45 |
| `Ryby_audit_20260904` | H-47 … H-58 (commit `e32ab2b`) |
| `Cobitis_elongatoides_20260904` | H-39, H-41 |
| `Ryby_20261105` | H-71 … H-90 (běhy pred / po / aplikovano / export_metodika) |
| `TC_BB_20261105_cilmon` | H-85, H-89, H-90, H-91 (Z-14) |
| `Cevnate_4sp_20261105` | H-90, H-91, H-92 (dopad od roku 2024) |
| `Audit_nezname_20261105` | H-94, H-95 (Z-25) |

Na každou složku kromě `Ryby_audit_20260904` odkazuje registr nebo jiná zpráva (ověřeno grepem).
`Ryby_audit_20260904` odkaz zvenku nemá, je ale doklad k H-47 … H-58 z commitu `e32ab2b`
a sám na registr odkazuje — ponechán; doporučeno doplnit odkaz do registru obojživelníků (H-47).

**Návrh dalšího úklidu (neprovedeno, rozhodne autor):** velké `.csv.gz` uvnitř dokladových
složek (`TC_BV_20260909/*.csv.gz` 5,8 MB, `Ryby_20261105/*/export`, `TC_BB_20261105_cilmon`)
jsou regenerovatelné z commitů v README, ale registry citují čísla z nich — mazat až po
rozhodnutí, že stačí čísla v README. `Test/` (skripty 2sp testu) duplikuje `skripty/` těchto
složek; sloučit do jednoho parametrického skriptu (např. `beh_kaskady.R`).

## Nezávislá kontrola (Fáze 6)

Diff proti `main`, registry a tuto zprávu prošel samostatný agent bez znalosti práce (jen čtení).
Závěr: **žádný blokující nález**; všech 10 refaktorovacích commitů posoudil jako sémanticky
shodné (nezávisle na bězích), merge jako skutečný dvourodičovský (`diff(fedd237, 960b4ab)` =
`diff(f9540f9, 77062dd)`), citované hashe dosažitelné, CSV beze změny kódování.

| Připomínka | Naloženo |
|---|---|
| H-96: větev neměnila kódování ani konce řádků, jen pořadí řádků | opraveno v registru (`22a6ee1`) i zde |
| H-96 a zpráva commitu `9350260`: `chu` u `STA_HABPOKRYV` má všech 18 ostatních druhů hmyzu, ne „čtyři" | registr opraven; zpráva commitu zůstává (historie se nepřepisuje) |
| H-103 / Z-28 odkazují na H-32 bez určení registru | upřesněno „H-32 registru cévnatých rostlin" |
| H-97: oprava se projeví u 14, ne 16 druhů (*Phengaris* bez výstupu kvůli H-103) | opraveno v registru i zde |
| zpráva commitu `440480c`: `POP_POCETMAX/MIN` na úrovni DP existují (7 obojživelníků) | věcně bez dopadu (žádný limit `chu` je nečte); zpráva commitu zůstává |
| `Outputs/Test/TC_BV_20260911_cilmon/skripty/harness.R` předává `n2k_druhy_posledni_chu` | harness zdrojuje kopie skriptů ze `SRC_DIR` (stav z doby běhu), ne živé `R/02_druhy`; doklad funguje — ponecháno, s aktuálním `25` by bylo nutné argument vypustit |
| aaf639d mění výstupy na základě otevřené volby `KLIC` (Z-26) | přenos řádků je pokyn zadání; volba `KLIC = ne` zapsána jako otázka Z-26, commit je samostatný a vratný |
| `.gitattributes` existuje v `Metodiky/Ryby a mihule/patche/` | upřesněno níže |

## Push a pull request

Větev je odeslána na `origin/druhy-integrace-202611`. Nástroj `gh` na stanici není, PR
nebyl založen. Draft PR do `main`:
<https://github.com/BiodivMonCZ/host_naturecz/compare/main...druhy-integrace-202611?expand=1>
(označit jako *draft*, **nesloučit** před rozhodnutím Z-26 … Z-30).

Navržený text PR:

> **Integrace kódu druhů (ryby, cévnaté, obojživelníci) a sjednocení bez změny výsledků**
>
> - merge `ryby-metodika-2026` (`--no-ff`; obsahuje `cevnate-metodika-2026`, `druhy-202609`)
> - *Eriogaster catax*: 2 buňky limitů z `eriogaster-catax-2025-01` (H-96, otázka Z-26)
> - refaktoring `R/02_druhy` 4 741 → 4 213 řádků; plný běh 82 druhů na exportu NDOP
>   2026-05-10 bajtově shodný (jediný zamýšlený rozdíl: řádky *E. catax*)
> - nálezy H-96 … H-103, otázky Z-26 … Z-30; Z-19, Z-20 beze změny
> - zpráva: `Outputs/Test/Integrace_druhy_20261109/README.md`

## Poznámky k formátu souborů

- `.gitattributes` v kořeni repozitáře **neexistuje** (zadání ho zmiňuje; jediný je v `Metodiky/Ryby a mihule/patche/`). Konce řádků řídí
  `core.autocrlf = true` a stav indexu; všechny úpravy zachovaly kódování i konce řádků
  (CSV byte po bytu, `limity_vse.csv` zůstává Windows-1250 + CRLF, diff jediného řádku).
- `24_n2k_druhy_lokality.R` končí osamělým `\r` bez `\n`, proto ho git vede jako binární
  (`-text`) a `git diff` ukazuje jen „Binary files differ" (použít `git diff --text`).
  Zachováno podle zadání; doporučeno doplnit `\n` nebo zavést `.gitattributes`.
