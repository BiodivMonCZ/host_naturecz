# Workflow hodnocení stanovišť

## Účel a základní členění

Nové workflow hodnocení stanovišť je rozdělené na několik navazujících, ale funkčně oddělených částí. Nejdůležitější princip je, že výpočet pasek, vlastní výpočet parametrů stanovišť, vyhodnocení jejich stavu a výpočet trendu nejsou smíchány do jednoho monolitického skriptu.

Paseky se počítají samostatně, protože jejich výpočet vyžaduje současnou práci s několika generacemi VMB. Výsledkem pasekového workflow je běžná tabelární tabulka s údaji pro jednotlivé kombinace lokality a habitatu. Tato tabulka se uloží do stabilního souboru `paseky_results_latest.csv` a při následném výpočtu stanovišť se už paseky znovu prostorově nepočítají. Hlavní workflow je pouze načte jako hotový vstup.

Hlavní workflow hodnocení stanovišť pracuje především s aktuálním VMB, prostorovou vrstvou hodnocených lokalit, druhovými daty, pomocnými tabulkami a již vypočtenými pasekami. Pro každou kombinaci `SITECODE × HABITAT_CODE` vypočte jednotlivé parametry stanoviště, například rozlohu, kvalitu, typické druhy, druhové indikátory, minimiareál a mozaikovitost. Z těchto surových parametrů se následně určí stav klíčových indikátorů a celkové hodnocení stanoviště.

Trend představuje rozšířenou větev hlavního workflow. Nejde o samostatný nový prostorový výpočet. Nejprve se standardně spočítá aktuální stav stanovišť a tento nový výsledek se následně porovná s historickým výsledkem. Historický výsledek může být předán přímo jako datový objekt nebo načten ze staršího CSV. Pokud historický dataset ještě neobsahuje vyhodnocení stavu, workflow jej nejprve vyhodnotí stejnou funkcí jako nový výsledek a až potom porovná obě období.

Toto rozdělení umožňuje přepočítávat paseky pouze tehdy, když je to skutečně potřeba, provádět běžné hodnocení stanovišť bez načítání starých generací VMB a počítat trend pouze v případech, kdy je k dispozici starší srovnatelný výsledek.

## Adresářová struktura

```text
R/01_stanoviste/

├── RUN_paseky.R
├── RUN_stanoviste.R
│
├── functions/
│   ├── FUN_paseky_select_pairs.R
│   ├── FUN_paseky_spat.R
│   ├── FUN_paseky_sum.R
│   ├── FUN_stanoviste_paseky.R
│   ├── FUN_paseky_batch.R
│   │
│   ├── FUN_stanoviste_klicove_parametry.R
│   ├── FUN_stanoviste_druhy.R
│   ├── FUN_stanoviste_prostor.R
│   ├── FUN_stanoviste_batch.R
│   ├── FUN_stanoviste_hodnoceni.R
│   ├── FUN_stanoviste_load_previous.R
│   └── FUN_stanoviste_trend.R
│
├── paseky/
│   ├── paseky_load_inputs.R
│   ├── paseky_input_validator.R
│   └── paseky_export.R
│
├── orchestrator/
│   ├── stanoviste_load_inputs.R
│   ├── stanoviste_input_validator.R
│   ├── stanoviste_central_orchestrator.R
│   ├── stanoviste_hodnoceni_inputs.R
│   └── stanoviste_trend_workflow.R
│
└── io/
    └── stanoviste_export.R
```

# Workflow pasek

## `RUN_paseky.R`

Soubor `R/01_stanoviste/RUN_paseky.R` je vstupním bodem pasekového workflow. Uživatel by za běžných okolností neměl ručně volat jednotlivé nízkoúrovňové funkce z ostatních skriptů. Pro běžnou práci má používat funkce definované právě v `RUN_paseky.R`.

Důležitou vlastností tohoto runneru je lazy loading. Pouhé načtení `RUN_paseky.R` ještě nenačte všechny tři velké generace VMB. K jejich načtení dojde až při prvním skutečném požadavku na výpočet. Načtené vstupy lze navíc uchovat v interní cache a opakovaně je používat v jedné R session.

V současné podobě runner poskytuje dvě hlavní veřejné funkce: *run_paseky_once()* a *run_paseky_workflow()*. Samostatný wrapper *run_paseky_batch()* v `RUN_paseky.R` momentálně není; hromadný výpočet je součástí *run_paseky_workflow()* a interně jej provádí funkce *paseky_batch()*.

## *run_paseky_once()*

*run_paseky_once()* slouží k výpočtu pasek pro jednu konkrétní kombinaci lokality a habitatu. Je vhodná zejména pro testování jednotlivých kombinací, ladění nebo kontrolu výsledků. Funkce neprovádí hromadný export ani nevytváří produkční soubor `paseky_results_latest.csv`.

Argument `site_code` určuje kód hodnocené lokality, například konkrétní `SITECODE` EVL. Musí odpovídat lokalitě přítomné ve vstupní vrstvě site.

Argument `hab_code` určuje kód habitatu, pro který se mají paseky spočítat. Pro nelesní habitat funkce prostorový výpočet pasek neprovádí a výsledné pasekové indikátory jsou nehodnotitelné. Vlastní paseková logika je určena především pro lesní habitaty, tedy Natura kódy začínající `9` a případně české lesní biotopy začínající `L`.

Argument `force_reload` má výchozí hodnotu `FALSE`. Pokud je `FALSE`, může funkce použít vstupy již načtené a uložené v interní cache runneru. Pokud se nastaví na `TRUE`, vstupní VMB a ostatní data se načtou znovu. To se hodí například po změně vstupních dat během stejné R session.

Funkce načte vstupy, připraví kandidátní dvojice VMB pomocí *paseky_select_pairs()* a následně zavolá *stanoviste_paseky()*. Výsledkem je agregovaný výsledek pro jedinou kombinaci `SITECODE × HABITAT_CODE`.

## *run_paseky_workflow()*

*run_paseky_workflow()* představuje standardní provozní způsob spuštění pasekového workflow. Načte vstupy, provede jejich validaci, spustí batch výpočet pro požadované kombinace a případně zapíše výsledky na disk.

Argument `targets` určuje kombinace lokalit a habitatů, které se mají zpracovat. Očekává tabulku obsahující minimálně sloupce `SITECODE` a `HABITAT_CODE`. Výchozí hodnota je `NULL`. Pokud zůstane `NULL`, workflow použije standardní tabulku kombinací lokalit a habitatů dostupnou z konfigurace projektu. Pokud uživatel předá vlastní tabulku, vypočítají se pouze kombinace uvedené v této tabulce.

Argument `output_dir` určuje adresář pro výstupy pasekového workflow. Výchozí hodnota je:

```text
Outputs/Data/stanoviste/paseky
```

Argument `calculation_date` určuje datum výpočtu, které se zapisuje do výstupu a používá se také v názvech archivních souborů. Výchozí hodnota je aktuální datum získané pomocí `Sys.Date()`.

Argument `parallel` určuje, zda se jednotlivé kombinace site × habitat mohou počítat paralelně. Výchozí hodnota je `TRUE`.

Argument `workers` určuje počet paralelních workerů. Výchozí hodnota je `NULL`. V takovém případě se počet workerů odvodí automaticky z dostupných CPU jader.

Argument `parallel_plan` určuje způsob paralelizace. Podporované možnosti jsou:

- `"multicore"`
- `"multisession"`
- `"sequential"`

Pokud je hodnota `NULL`, workflow zvolí plán podle operačního systému. Na Windows se použije `multisession`, na ostatních systémech typicky `multicore`.

`multicore` používá fork procesů. Je efektivní například při běhu R ze shellu na Linuxu, ale není doporučený uvnitř RStudia.

`multisession` používá samostatné R procesy. Každý worker má vlastní R session, takže je obecně robustnější vůči prostředí, ve kterém se výpočet spouští.

`sequential` vypne skutečnou paralelizaci a všechny targety zpracovává postupně.

Argument `future_seed` se předává balíku `future.apply` a řídí práci s generátorem náhodných čísel při paralelním běhu. Výchozí hodnota `TRUE` zajišťuje korektní a reprodukovatelné zacházení s RNG streamy. Samotný výpočet pasek není primárně stochastický, ale nastavení je součástí obecné paralelní infrastruktury.

Argument `stop_on_error` určuje, co se stane, pokud některá kombinace skončí chybou. Při `TRUE` workflow po dokončení batch části skončí chybou, pokud některý target selhal. Při `FALSE` se chyby uloží do logu a úspěšně spočítané kombinace mohou být vráceny spolu s nimi.

Argument `write_results` určuje, zda se mají výsledky zapisovat na disk. Při `FALSE` se výpočet provede a výsledek se vrátí pouze jako R objekt.

Argument `overwrite_archive` řídí přepis archivního souboru s konkrétním datem. Výchozí hodnota je `FALSE`, takže pokud archivní soubor pro dané datum již existuje, workflow jej nepřepíše. To chrání dřívější výpočet před neúmyslným nahrazením.

Argument `allow_partial_export` určuje, zda je dovoleno exportovat neúplný výsledek, pokud některé targety v batch výpočtu selhaly. Výchozí hodnota `FALSE` export takového výsledku blokuje. Produkční `paseky_results_latest.csv` by proto za normálních okolností měl vzniknout pouze z kompletního výpočtu.

Argument `force_reload` má stejný význam jako u *run_paseky_once()*. Při `TRUE` se vstupy načtou znovu bez použití cache.

Návratovou hodnotou je list obsahující:

- `result` — agregované výsledky pasek,
- `log` — průběh jednotlivých targetů,
- `selected_pairs` — skutečně zvolené páry VMB,
- `settings` — informace o batch běhu,
- `export` — manifest vytvořených výstupních souborů.

# Vnitřní části pasekového workflow

## *load_paseky_inputs()* — `paseky/paseky_load_inputs.R`

Tato funkce připravuje kompletní vstupní data pro výpočet pasek. Jako jediná část nového systému současně pracuje se staršími generacemi VMB.

Načítá VMB1, tedy původní mapování, VMB2 odpovídající Aktualizaci 1 a VMB0 odpovídající aktuálnímu VMB. Z jednotlivých generací získává jednak základní mapové vrstvy, jednak update vrstvy potřebné k identifikaci změn mezi mapováními.

Současně sjednocuje názvy důležitých atributů, například `REGION_ID`, `DATUM` a `ROK_AKT`, připravuje jednoduchá metadata `REGION_ID × DATUM` a normalizuje tabulku targetů.

Výstupem je pojmenovaný list s prostorovými daty, metadaty, targety a manifestem, který ukazuje původ jednotlivých vstupních objektů.

## *paseky_validate_inputs()* — `paseky/paseky_input_validator.R`

Funkce kontroluje strukturu pasekových vstupů ještě před zahájením vlastního výpočtu.

Ověřuje například:

- zda jsou prostorové vstupy objekty `sf`,
- zda obsahují požadované sloupce,
- zda základní VMB vrstvy obsahují například `HABITAT`, `BIOTOP`, `STEJ_PR`, `SEGMENT_ID`, `DATUM` a `REGION_ID`,
- zda update vrstvy obsahují také `ROK_AKT`,
- zda target tabulka obsahuje `SITECODE` a `HABITAT_CODE`,
- zda targety nejsou duplicitní,
- zda všechny požadované `SITECODE` skutečně existují ve vrstvě hodnocených lokalit.

Pokud některá základní podmínka není splněna, workflow skončí ještě před časově náročným prostorovým výpočtem.

## *paseky_select_pairs()* — `functions/FUN_paseky_select_pairs.R`

Funkce připravuje kandidátní dvojice generací VMB, které mohou být později porovnávány.

Uvažuje kombinace:

- `VMB2_VMB0`
- `VMB1_VMB2`
- `VMB1_VMB0`

Zároveň jim přiřazuje prioritu. Nejvyšší prioritu má porovnání VMB2 s aktuálním VMB0, poté VMB1 s VMB2 a až poslední možností je přímé porovnání VMB1 s VMB0.

Tato funkce sama ještě nerozhoduje, který pár bude pro konkrétní místo skutečně použit. Vytváří pouze možné kandidáty podle dostupných `REGION_ID`.

Definitivní volba se provádí až po reálném prostorovém výpočtu. Tím se zabrání situaci, kdy by byl pár vybrán pouze podle metadat, přestože v konkrétní lokalitě a habitatu nemá použitelný prostorový průnik.

## *paseky_spat()* — `functions/FUN_paseky_spat.R`

*paseky_spat()* provádí vlastní prostorové porovnání staršího a novějšího mapování.

Pro konkrétní site, habitat, dvojici VMB a sadu `REGION_ID` vybere příslušné segmenty obou mapování a vypočte jejich skutečné prostorové průniky.

Pro Natura lesní habitaty se ve starším mapování používají pouze lesní biotopy `L*`, čímž je zachována metodika původního workflow.

Na novější vrstvě se podle kódu biotopu a roku aktualizace určí, zda konkrétní překryv představuje paseku. Mezi příslušné kategorie patří například `LP` a `X10`, případně `X11`, `X12A` a `X12B` v odpovídajícím období aktualizace.

Z plochy geometrického průniku a hodnot `STEJ_PR` obou mapování se vypočte efektivní plocha daného průniku. Současně se identifikuje, zda překryv splňuje definici holiny.

Výsledkem je detailní `sf` objekt jednotlivých průniků, nikoliv ještě finální souhrnná tabulka.

## *stanoviste_paseky()* — `functions/FUN_stanoviste_paseky.R`

Tato funkce řídí celý pasekový výpočet pro jednu kombinaci `SITECODE × HABITAT_CODE`.

Nejprve zjistí relevantní `REGION_ID` zasahující lokalitu. Následně spočítá všechny kandidátní dvojice mapování, pro které mohou existovat data.

Teprve po reálném prostorovém výpočtu zkontroluje časovou posloupnost mapování pomocí podmínky:

```text
DATUM_NEW > DATUM_OLD
```

Pro každý `REGION_ID` následně vybere jeden skutečně použitý pár. Prioritu má:

1. `VMB2_VMB0`
2. `VMB1_VMB2`
3. `VMB1_VMB0`

Vybraný pár tedy není určen pouze dostupností metadat, ale také existencí konkrétního prostorového výsledku pro danou kombinaci site × habitat × region.

Nakonec předá vybrané průniky funkci *paseky_sum()*.

## *paseky_sum()* — `functions/FUN_paseky_sum.R`

Funkce převádí prostorový detail pasek na jednoduchou tabulku pro jednu kombinaci site × habitat.

Vypočítá:

- `ROZLOHA_PASEKY`
- `ROZLOHA_HOLINY`
- `POCET_SEGMENTU_PASEKY`

Rozlohy jsou vráceny v hektarech.

`POCET_SEGMENTU_PASEKY` je založen na počtu unikátních `SEGMENT_ID_NEW`, tedy na segmentech novějšího mapování.

U nelesních habitatů jsou pasekové parametry `NA`.

U lesního habitatu, ve kterém nebyla nalezena žádná paseka, jsou výsledné hodnoty:

```text
ROZLOHA_PASEKY = 0
ROZLOHA_HOLINY = 0
POCET_SEGMENTU_PASEKY = 0
```

## *paseky_batch()* — `functions/FUN_paseky_batch.R`

*paseky_batch()* obaluje výpočet jedné kombinace a umožňuje jej spustit pro větší množství targetů.

Před samotným batch výpočtem připraví kandidátní dvojice VMB pouze jednou za celý běh. Pro každou lesní lokalitu také předpočítá relevantní `REGION_ID`, aby se stejná prostorová operace neopakovala pro každý habitat zvlášť.

Potom spouští *stanoviste_paseky()* pro jednotlivé kombinace.

Funkce podporuje sekvenční i paralelní výpočet a pro každý target vytváří samostatný log obsahující mimo jiné:

- `.target_id`
- `SITECODE`
- `HABITAT_CODE`
- `status`
- `elapsed_sec`
- `error_message`

Vedle agregovaných výsledků vrací také tabulku skutečně použitých VMB párů.

## *paseky_export()* — `paseky/paseky_export.R`

Tato funkce ukládá výsledky pasekového workflow.

Vytváří archivní soubor:

```text
paseky_results_YYYYMMDD.csv
```

a současně aktualizuje stabilní produkční soubor:

```text
paseky_results_latest.csv
```

Právě `paseky_results_latest.csv` je vstupem hlavního workflow stanovišť.

Kromě toho zapisuje také:

```text
paseky_log_YYYYMMDD.csv
paseky_selected_pairs_YYYYMMDD.csv
```

Soubor `paseky_selected_pairs_YYYYMMDD.csv` umožňuje zpětně dohledat, která dvojice VMB byla použita pro konkrétní kombinaci site, habitatu a regionu.

Pokud batch obsahuje chyby, export neúplné tabulky je standardně zablokován.

Soubor `latest` se zapisuje nejprve do dočasného souboru a teprve poté se přejmenuje. Tím se snižuje riziko, že po přerušeném zápisu zůstane poškozený produkční vstup.

# Hlavní workflow hodnocení stanovišť

## `RUN_stanoviste.R`

Soubor `R/01_stanoviste/RUN_stanoviste.R` je hlavním uživatelským vstupem pro výpočet stanovišť.

Na rozdíl od `RUN_paseky.R` se při jeho načtení rovnou připravují vstupy hlavního workflow. Skript načte potřebné výpočetní, orchestrující a exportní funkce, zavolá *load_stanoviste_inputs()* a připraví také pomocné vstupy pro následné hodnocení a export.

Runner poskytuje tři hlavní veřejné funkce:

- *run_stanoviste_once()*
- *run_stanoviste_batch()*
- *run_stanoviste_workflow()*

Rozdíl mezi nimi je důležitý.

*run_stanoviste_once()* a *run_stanoviste_batch()* počítají pouze surové parametry stanovišť.

Teprve *run_stanoviste_workflow()* pokračuje vyhodnocením stavu, případným výpočtem trendu a exportem.

## *run_stanoviste_once()*

*run_stanoviste_once()* je nejjednodušší způsob výpočtu jedné kombinace lokality a habitatu.

Volá centrální funkci *stanoviste_eval()* a vrací raw parametry. Neprovádí hodnocení stavu, trend ani export.

Argument `site_code` určuje jednu hodnocenou lokalitu.

Argument `hab_code` určuje jeden habitat.

Argument `return_components` má výchozí hodnotu `FALSE`.

Při `FALSE` funkce vrátí pouze jeden výsledný řádek se všemi raw parametry.

Při `TRUE` vrátí detailnější list, ve kterém je výsledný spojený řádek a současně samostatné výstupy:

- pasek,
- klíčových parametrů,
- druhových parametrů,
- prostorových parametrů.

Tato možnost je vhodná hlavně pro debugging a kontrolu toho, která výpočetní komponenta vytvořila konkrétní hodnotu.

Typické použití:

```r
result <- run_stanoviste_once(
  site_code = "CZ0414127",
  hab_code = "9130"
)
```

## *run_stanoviste_batch()*

*run_stanoviste_batch()* počítá raw parametry pro více kombinací lokalit a habitatů. Je to uživatelský wrapper nad funkcí *stanoviste_batch()*.

Argument `targets` je povinný a musí obsahovat tabulku kombinací s minimálně dvěma sloupci:

```text
SITECODE
HABITAT_CODE
```

Každý řádek představuje jednu samostatně hodnocenou kombinaci.

Argument `parallel` určuje, zda se jednotlivé kombinace mají spustit paralelně. Výchozí hodnota je `FALSE`, takže bez explicitního požadavku je batch sekvenční.

Argument `workers` určuje počet workerů. Pokud je `NULL`, počet workerů odvodí paralelní backend automaticky.

Argument `parallel_plan` má ve wrapperu výchozí hodnotu `"multisession"`.

Podporované možnosti jsou:

- `"multisession"`
- `"multicore"`
- `"sequential"`

`multisession` používá samostatné R procesy a je vhodný i v prostředích, kde fork není bezpečný.

`multicore` používá forkované procesy a je praktický zejména při spuštění R mimo RStudio na Linuxu.

`sequential` výpočet skutečně neparalelizuje.

Argument `future_seed` se předává `future.apply::future_lapply()` a standardně je `TRUE`.

Argument `return_components` řídí, zda se mají uchovávat samostatné komponenty každého targetu.

Při `FALSE` jsou vráceny pouze výsledné raw řádky, log a nastavení batch běhu.

Při `TRUE` se kromě výsledků zachovají také vnitřní výstupy *stanoviste_eval()* pro jednotlivé targety. To výrazně zvyšuje objem vraceného objektu, ale je užitečné při ladění.

Argument `stop_on_error` má výchozí hodnotu `TRUE`.

Pokud některý target selže, batch po dokončení vyhodí chybu.

Při `FALSE` se chyba pouze zaznamená v logu a ostatní targety mohou být zpracovány.

Výstupem je list obsahující:

- `results`
- `log`
- případně `components`
- `settings`

Typické použití:

```r
batch <- run_stanoviste_batch(
  targets = targets,
  parallel = TRUE,
  workers = 8,
  parallel_plan = "multisession",
  stop_on_error = TRUE
)
```

## *run_stanoviste_workflow()*

*run_stanoviste_workflow()* je hlavní produkční funkce. Je určena pro standardní výpočet výsledků stanovišť, jejich vyhodnocení, volitelný trend a export.

Argument `targets` je povinná tabulka kombinací `SITECODE × HABITAT_CODE`, které se mají spočítat.

Argument `previous_results` umožňuje předat historický výsledek přímo jako R tabulku.

Může jít o raw nebo již vyhodnocený široký dataset. Pokud jde o raw výsledek, trendová větev jej před porovnáním sama vyhodnotí pomocí *stanoviste_hodnoceni()*.

Argument `previous_source` je alternativa k `previous_results`.

Umožňuje zadat cestu nebo URL k historickému CSV. Funkce *stanoviste_load_previous()* umí načíst:

- lokální CSV,
- raw GitHub URL,
- běžnou GitHub `blob` URL.

`previous_results` a `previous_source` se nemají používat současně.

Argument `calculate_trend` řídí, zda se má trend skutečně počítat.

Výchozí hodnota je `NULL`, což znamená automatický režim.

Pokud je předáno `previous_results` nebo `previous_source`, trend se automaticky aktivuje.

Pokud není předán žádný historický výsledek, trend se nepočítá.

Hodnotou `TRUE` lze trend explicitně vynutit. V tom případě ale musí být k dispozici `previous_results` nebo `previous_source`.

Hodnotou `FALSE` lze trend naopak explicitně vypnout i v případě, že historický výsledek byl zadán.

Argument `period_id` slouží především k označování výstupních souborů.

Výchozí hodnota je dvouciferné označení aktuálního roku. `period_id` nemění metodiku výpočtu, ale vstupuje do názvů archivních výsledků.

Argument `assessment_year` určuje rok hodnocení používaný zejména v názvu systémového N2K exportu.

Výchozí hodnota je aktuální kalendářní rok.

Argument `output_dir` určuje adresář, do kterého se zapíší všechny výstupy hlavního workflow.

Výchozí hodnota je:

```text
Outputs/Data/stanoviste
```

Argument `write_results` určuje, zda se mají výsledky zapisovat na disk.

Při `FALSE` se celý výpočet včetně hodnocení a případného trendu provede, ale nic se neexportuje.

Argument `overwrite` řídí, zda mohou být nahrazeny již existující výstupní soubory.

Výchozí hodnota `FALSE` chrání existující exporty před náhodným přepsáním.

Argument `parallel` řídí paralelní běh raw batch části.

Výchozí hodnota je `FALSE`.

Argument `workers` určuje počet paralelních workerů.

Argument `parallel_plan` určuje backend paralelizace raw výpočtu.

Výchozí hodnota je `"multisession"`.

Podporované možnosti jsou:

- `"multisession"`
- `"multicore"`
- `"sequential"`

Argument `future_seed` řídí RNG handling v paralelní části stejně jako u *run_stanoviste_batch()*.

Argument `return_components` má širší dopad než u samotného batch runneru.

Při `TRUE` jsou do výsledného listu přidány detailní informace z batch výpočtu a z trendové větve. U trendu lze pak získat například detailní porovnání aktuální a historické hodnoty každého indikátoru.

Při běžném produkčním běhu není tato podrobnost nutná a lze ponechat `FALSE`.

Argument `stop_on_error` určuje chování batch části při chybách jednotlivých targetů.

Při výchozím `TRUE` chyba některého targetu zablokuje pokračování workflow.

Při `FALSE` lze získat částečný raw výsledek a log, ale u produkčního výpočtu je vhodné tuto možnost používat opatrně.

Návratovou hodnotou *run_stanoviste_workflow()* je list obsahující:

- `result` — finální výsledek po případném doplnění trendu,
- `raw` — původní vypočtené parametry,
- `evaluated` — výsledek po vyhodnocení stavu,
- `batch_log` — log jednotlivých targetů,
- `export` — manifest vytvořených souborů.

Pokud je `return_components = TRUE`, přidávají se také:

- `batch` — detailní objekt batch výpočtu,
- `trend` — detailní objekt trendové větve.

Typický běh bez trendu může vypadat například:

```r
result <- run_stanoviste_workflow(
  targets = targets,
  parallel = TRUE,
  workers = 8,
  parallel_plan = "multisession"
)
```

Běh s trendem může použít například:

```r
result <- run_stanoviste_workflow(
  targets = targets,
  previous_source = "historicky_results_habitats.csv",
  calculate_trend = TRUE,
  parallel = TRUE,
  workers = 8,
  parallel_plan = "multisession"
)
```

# Vstupy hlavního workflow

## *load_stanoviste_inputs()* — `orchestrator/stanoviste_load_inputs.R`

Tato funkce propojuje nové workflow se stávající konfigurační vrstvou repozitáře.

Načítá pouze aktuální VMB. Starší VMB1 a VMB2 se v hlavním workflow nepoužívají, protože jsou potřeba pouze při samostatném výpočtu pasek.

Připravuje prostorovou vrstvu lokalit, aktuální VMB, hranici ČR a pomocné tabulky potřebné pro jednotlivé výpočetní komponenty.

Mezi hlavní vstupy patří například:

```text
data$site
data$vmb
data$czechia_line

tables$habitat_areas
tables$minimisize
tables$red_list_species
tables$invasive_species
tables$expansive_species
tables$paseky
```

Zvláštní pozornost věnuje pasekám. Primárně načítá:

```text
Outputs/Data/stanoviste/paseky/paseky_results_latest.csv
```

Výsledek pasekového workflow tak vstupuje do hlavního hodnocení jako běžná tabulka. Hlavní workflow už neřeší, z jakých generací VMB byly paseky odvozeny.

Funkce také normalizuje některé názvy sloupců a datové typy, aby výpočetní funkce pracovaly se stabilním rozhraním.

## *stanoviste_validate_inputs()* — `orchestrator/stanoviste_input_validator.R`

Tato funkce validuje vstupy jedné kombinace site × habitat.

Kontroluje:

- strukturu objektu `data`,
- strukturu objektu `tables`,
- přítomnost potřebných prostorových objektů,
- požadované sloupce,
- dostupnost funkcí *stanoviste_klic()*, *stanoviste_druhy()* a *stanoviste_prostor()*,
- existenci požadovaného `SITECODE`,
- kompatibilitu vstupních objektů,
- přítomnost odpovídajícího záznamu v tabulce pasek.

Jejím účelem je zachytit strukturální chyby vstupů ještě předtím, než začne časově náročný prostorový výpočet.

# Výpočet jedné kombinace stanoviště

## *stanoviste_eval()* — `orchestrator/stanoviste_central_orchestrator.R`

*stanoviste_eval()* je centrální orchestrátor raw výpočtu jedné kombinace `SITECODE × HABITAT_CODE`.

Nejprve spustí *stanoviste_validate_inputs()*.

Následně z `tables$paseky` vybere jeden odpovídající řádek s pasekovými údaji.

Potom samostatně zavolá tři hlavní výpočetní funkce:

- *stanoviste_klic()*
- *stanoviste_druhy()*
- *stanoviste_prostor()*

Každá z těchto funkcí musí vrátit právě jeden řádek pro požadovanou kombinaci.

Orchestrátor kontroluje shodu `SITECODE` a `HABITAT_CODE` ve všech dílčích výstupech a následně je spojí do jednoho širokého výsledku.

Při `return_components = TRUE` zachová kromě společného výsledku také samostatný výstup každé výpočetní části.

# Klíčové parametry stanoviště

## *stanoviste_klic()* — `functions/FUN_stanoviste_klicove_parametry.R`

Funkce počítá základní charakteristiky stavu habitatu.

Nejprve provede prostorový průnik aktuálního VMB s hodnocenou lokalitou a vybere požadovaný habitat.

Plocha jednotlivých segmentů je korigována hodnotou `STEJ_PR`, aby se zohlednilo skutečné zastoupení biotopu v mapovaném segmentu.

Do celkové rozlohy stanoviště přidává také `ROZLOHA_PASEKY` z předpočítané pasekové tabulky.

Paseky tedy nejsou pouze pomocným výstupem. Přímo vstupují do výpočtu celkové rozlohy a do jmenovatelů některých dalších parametrů.

Funkce počítá zejména:

- `ROZLOHA`
- `KVALITA`
- `TYPICKE_DRUHY`
- `REPRE`
- `REPRE_SDF`
- `CONSERVATION`
- `DEGREE_OF_CONSERVATION`
- `MRTVE_DREVO`
- `KALAMITA_POLOM`

`MRTVE_DREVO` a `KALAMITA_POLOM` se počítají pouze pro lesní habitaty.

Současně se počítají pomocné plošné charakteristiky:

- `RELATIVE_AREA_PERC`
- `SITE_AREA_PERC`
- `GOOD_DOC_AREA_HA`
- `W_AREA_HA`
- `W_AREA_PERC`
- `PASEKY_AREA_HA`
- `PASEKY_AREA_PERC`
- `DEGRAD_AREA_HA`
- `DEGRAD_AREA_PERC`

Funkce také sleduje zastoupení segmentů podle období aktualizace VMB:

- `PERC_0`
- `PERC_1`
- `PERC_2`

a uchovává datumové charakteristiky mapování:

- `DATE_MIN`
- `DATE_MAX`
- `DATE_MEAN`
- `DATE_MEDIAN`

# Druhové parametry

## *stanoviste_druhy()* — `functions/FUN_stanoviste_druhy.R`

Tato funkce počítá druhové indikátory.

Pracuje s:

- aktuálním VMB,
- daty druhů červeného seznamu,
- daty invazních druhů,
- daty expanzivních druhů,
- tabulkou pasek.

Celková plocha stanoviště opět zahrnuje plochu předpočítaných pasek. Ta proto vstupuje do jmenovatele některých druhových indikátorů.

Výsledkem jsou numerické parametry:

- `RED_LIST`
- `INVASIVE`
- `EXPANSIVE`

a současně textové seznamy nalezených druhů:

- `RED_LIST_SPECIES`
- `INVASIVE_LIST`
- `EXPANSIVE_LIST`

`RED_LIST` je skóre odvozené z počtu druhů červeného seznamu vzhledem k velikosti habitatu.

`INVASIVE` vyjadřuje podíl plochy zasažené invazními druhy.

`EXPANSIVE` obdobně charakterizuje expanzivní druhy.

Pro některé habitaty jsou v kódu zachovány konkrétní metodické výjimky. Například `Arrhenatherum elatius` se pro habitat 6510 / T1.1 nepovažuje za invazní druh.

# Prostorové parametry

## *stanoviste_prostor()* — `functions/FUN_stanoviste_prostor.R`

Tato funkce počítá prostorové vlastnosti stanoviště, především minimiareál a mozaikovitost.

### Minimiareál

Při výpočtu minimiareálu propojuje vhodné segmenty do prostorových skupin podle skutečné vzdálenosti maximálně 50 metrů.

Následně zjišťuje, které skupiny splňují požadovaný minimální plošný limit.

Výstupem jsou:

- `MINIMIAREAL`
- `MINIMIAREAL_JADRA`
- `MINIMIAREAL_HODNOTA`

`MINIMIAREAL` vyjadřuje podíl plochy stanoviště, která se nachází v dostatečně velkých prostorových celcích.

`MINIMIAREAL_JADRA` udává počet takových celků.

`MINIMIAREAL_HODNOTA` obsahuje použitý plošný limit.

Paseky a degradované segmenty se nezapočítávají do vhodných jader minimiareálu, ale jsou součástí celkové plochy stanoviště ve jmenovateli.

### Mozaikovitost

Funkce počítá vnitřní a vnější mozaikovitost.

Vnitřní mozaika charakterizuje zastoupení nepřírodních biotopů uvnitř segmentů hodnoceného stanoviště.

Vnější mozaika hodnotí návaznost hranice stanoviště na okolní přírodní biotopy.

Speciálně se zohledňuje státní hranice, aby absence zahraničních dat nebyla automaticky považována za nepřírodní okolí.

Výsledkem jsou mimo jiné:

- `MOZAIKA_VNITRNI`
- `MOZAIKA_VNEJSI`
- `MOZAIKA_FIN`
- `VYPLNENOST_MOZAIKA`
- `PERC_SPAT`

# Batch hlavního výpočtu

## *stanoviste_batch()* — `functions/FUN_stanoviste_batch.R`

Funkce spouští *stanoviste_eval()* pro všechny řádky target tabulky.

Každému targetu přiřadí interní `.target_id`, zachytí případné chyby a sestaví společný log.

Výsledné řádky úspěšných targetů spojí do jedné široké tabulky.

Podporuje sekvenční i paralelní výpočet. Při paralelizaci využívá balíky `future` a `future.apply`.

Pro každý target ukládá:

- `.target_id`
- `SITECODE`
- `HABITAT_CODE`
- `status`
- `error_message`

Pokud je požadováno `return_components = TRUE`, ukládá také detailní komponenty každé jednotlivé kombinace.

# Hodnocení vypočtených parametrů

## *stanoviste_get_hodnoceni_inputs()* — `orchestrator/stanoviste_hodnoceni_inputs.R`

Tato funkce připravuje pomocné vstupy, které nejsou nutné k samotnému prostorovému výpočtu, ale jsou potřeba pro interpretaci výsledku a systémový export.

Připravuje:

- `limits`
- `minimisize`
- `site_context`
- `sdo_ii_sites`
- `indicator_lookup`
- `habitat_lookup`

`site_context` sjednocuje zejména:

- `SITECODE`
- `oop`
- `pracoviste`

Tyto informace se následně používají při hodnocení a exportu.

## *stanoviste_hodnoceni()* — `functions/FUN_stanoviste_hodnoceni.R`

Tato funkce převádí raw výsledky na vlastní hodnocení stavu.

Pro celkové hodnocení jsou rozhodující dva klíčové indikátory:

- `ROZLOHA`
- `KVALITA`

Pro každý z nich funkce dohledá relevantní limit a zdroj tohoto limitu.

Výstup je rozšířen například o:

- `ROZLOHA_LIMIT`
- `ROZLOHA_ZDROJ`
- `ROZLOHA_STAV`
- `KVALITA_LIMIT`
- `KVALITA_ZDROJ`
- `KVALITA_STAV`

Paralelně se počítají také varianty stavu s metodickou tolerancí 5 %.

Z těchto dvou klíčových indikátorů se následně odvodí `CELKOVE_HODNOCENI`.

Pokud jsou oba klíčové parametry dobré, je celkový stav `dobrý`.

Pokud je dobrý pouze jeden, je celkový stav `zhoršený`.

Pokud není dobrý ani jeden, je celkový stav `špatný`.

Některé předem definované habitaty jsou označeny jako `nehodnocen`.

Ve výchozím nastavení jde například o:

- `91T0`
- `3140`
- `3130`
- `8310`

# Trendová větev

Trend není součástí samotného prostorového výpočtu stanoviště.

Představuje další vrstvu, která porovnává aktuální výsledek se starším výpočtem.

Historický výsledek může být vytvořen starým workflow i novým workflow. Důležité je, aby obsahoval odpovídající kombinace `SITECODE × HABITAT_CODE` a sledované indikátory.

## *stanoviste_load_previous()* — `functions/FUN_stanoviste_load_previous.R`

Funkce načítá historický široký výsledek.

Podporuje:

- lokální CSV,
- raw GitHub URL,
- standardní GitHub `blob` URL.

Historický výstup musí obsahovat minimálně:

- `SITECODE`
- `HABITAT_CODE`
- `ROZLOHA`
- `KVALITA`

Funkce také normalizuje některé historické varianty kódů habitatů, například různé reprezentace kódu `91E0`.

U historické `ROZLOHA` zachovává kompatibilitu se starším workflow a chybějící hodnoty převádí na nulu.

## *stanoviste_trend_workflow()* — `orchestrator/stanoviste_trend_workflow.R`

Tato funkce propojuje historický výsledek s aktuálním výsledkem.

Dokáže přijmout historická data přímo jako objekt `previous_results` nebo je načíst z `previous_source`.

Současně rozpozná, zda aktuální a historická data už obsahují vyhodnocení stavu.

Za vyhodnocený dataset považuje takový, který již obsahuje například:

- `CELKOVE_HODNOCENI`
- `ROZLOHA_STAV`
- `KVALITA_STAV`

Pokud některý vstup ještě není vyhodnocený, funkce pro něj nejprve zavolá *stanoviste_hodnoceni()*.

Díky tomu lze porovnávat například:

- nový current se starým historickým raw CSV,
- nový current s previous vytvořeným novým workflow,
- dva již vyhodnocené wide datasety.

Teprve po sjednocení obou období se volá *stanoviste_trend()*.

## *stanoviste_trend()* — `functions/FUN_stanoviste_trend.R`

Funkce porovnává hodnoty jednotlivých indikátorů mezi aktuálním a předchozím obdobím.

### Rozloha

Pro `ROZLOHA` platí:

- změna do ±5 % → `stabilní`
- pokles větší než 5 % → `zhoršující se`
- růst větší než 5 % → `zlepšující se`

### Kvalita

U `KVALITA` je interpretace směru opačná, protože vyšší číselná hodnota znamená horší stav.

Proto platí:

- změna do ±5 % → `stabilní`
- růst větší než 5 % → `zhoršující se`
- pokles větší než 5 % → `zlepšující se`

### Ostatní indikátory

U ostatních indikátorů původní metodika obecně neurčuje směr zlepšení nebo zhoršení.

Proto je:

- shodná hodnota → `stabilní`
- numerická změna v toleranci ±5 % → `stabilní`
- jiná změna → `NA`

Pokud je aktuální nebo historická hodnota `NA`, trend zůstává také `NA`.

### Celkové hodnocení

Trend celkového hodnocení se nevyplňuje.

Sloupec:

```text
TREND_CELKOVE_HODNOCENI
```

proto zůstává `NA`.

Tím je zachováno chování původního systémového exportu.

# Export

## *stanoviste_export()* — `io/stanoviste_export.R`

Exportní funkce dostává několik úrovní výsledků současně:

- raw výpočet,
- vyhodnocený výsledek,
- finální výsledek po případném doplnění trendu,
- batch log.

Díky tomu je možné archivovat nejen finální systémový výsledek, ale také mezivýsledky potřebné pro kontrolu a budoucí srovnání.

### Raw výsledek

Obsahuje samotné vypočtené parametry před vyhodnocením stavu.

Typický název:

```text
results_habitats_<period>_<date>.csv
```

Tento formát je současně vhodný jako budoucí vstup `previous_results` nebo `previous_source` pro trendovou větev.

### Raw long

Exportuje se také historicky kompatibilní dlouhý formát raw výsledků.

### Evaluated výsledek

Obsahuje raw parametry rozšířené o:

- limity,
- zdroje limitů,
- stav `ROZLOHA`,
- stav `KVALITA`,
- `CELKOVE_HODNOCENI`.

### Final výsledek

Pokud se počítal trend, obsahuje také sloupce `TREND_*`.

Pokud se trend nepočítal, final výsledek odpovídá evaluated výsledku.

### Final long

Final dataset se ukládá také v dlouhém formátu vhodném pro další zpracování a kontrolu.

### Batch log

Ukládá se rovněž log jednotlivých kombinací site × habitat.

### Systémové exporty

Workflow vytváří také formáty kompatibilní s původním systémovým workflow.

Patří mezi ně například:

```text
stanoviste_YYYYMMDD.xlsx
```

a:

```text
n2k_stanoviste_<rok>_<datum>_Windows-1250.csv
```

spolu s UTF-8 variantou rozdělenou podle maximální velikosti části:

```text
n2k_stanoviste_<rok>_<datum>_UTF-8_part1.csv
n2k_stanoviste_<rok>_<datum>_UTF-8_part2.csv
...
```

Při vytváření systémového exportu funkce převádí názvy indikátorů na jejich systémová `ind_id`.

Textové stavy se převádějí na systémové kódy, například:

- `dobrý` → `11`
- `zhoršený` → `12`
- `špatný` → `13`
- `neznámý` → `1`
- `nehodnocen` → `8`

Trend se obdobně převádí například:

- `zlepšující se` → `2`
- `stabilní` → `3`
- `zhoršující se` → `4`

Parametry, které nemají definované systémové `ind_id`, se do systémového N2K CSV nedostanou.

Pokud trend nebyl vypočítán nebo není možné jej určit, zůstává v exportu `NA`.

# Doporučený způsob používání

Pasekové workflow je vhodné spouštět pouze při změně relevantních generací VMB nebo pokud je z jiného důvodu nutné paseky přepočítat.

Jeho hlavním produktem je:

```text
Outputs/Data/stanoviste/paseky/paseky_results_latest.csv
```

Tento soubor pak může být opakovaně používán hlavním workflow bez nutnosti znovu načítat a porovnávat starší generace VMB.

Pro kontrolu jedné kombinace pasek je vhodná funkce *run_paseky_once()*.

Pro kompletní přepočet pasek a vytvoření produkční tabulky se používá *run_paseky_workflow()*.

Běžný výpočet stanovišť následně používá `RUN_stanoviste.R`.

Pro kontrolu jedné kombinace site × habitat je nejvhodnější *run_stanoviste_once()*.

Pro kontrolní nebo výpočetní batch bez hodnocení stavu, trendu a exportu je vhodná *run_stanoviste_batch()*.

Pro produkční výpočet se používá *run_stanoviste_workflow()*, protože kromě raw parametrů provede také hodnocení stavu a případný export.

Pokud není potřeba trend, může *run_stanoviste_workflow()* běžet bez historického výsledku.

Pokud je předán `previous_results` nebo `previous_source`, může stejná funkce rozšířit výpočet o trendovou větev.

Tím zůstává jedna hlavní produkční funkce pro aktuální hodnocení i pro hodnocení s časovým srovnáním.