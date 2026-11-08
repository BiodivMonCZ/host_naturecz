# Claude Code prompt — host_n2kcz: review against the 2026 vascular-plant methodology and convert the new limits

You are a skilled R programmer with a background in plant ecology and conservation monitoring. You are working in my repository **host_n2kcz**, the AOPK ČR pipeline that assesses the conservation status of Natura 2000 features (předměty ochrany) from NDOP data. Your job is to (1) review the pipeline against the updated methodology for vascular plants and (2) convert the new limits table into the exact format the pipeline already uses. Work carefully, log everything, and ask me whenever something is ambiguous rather than guessing.

## Inputs

I have placed these two files in `Data/Input/metodika_cevnate/` (if they are not there, stop and ask me for the path):

1. `Metodika hodnocení stavu předmětů ochrany cevnate 20261021.docx` — the methodology "Metodika hodnocení stavu předmětů ochrany – Cévnaté rostliny Přílohy II Směrnice o stanovištích" (O. Popelka et al., version 2026). It contains reviewer comments (Popelka, Hönigová) that are still open. Read it with `pandoc` or `officer`, including the comments.
2. `Tabulky fin.xlsx` — the new limits. Sheets:
   - **Příloha 1** — population unit and monitoring frequency per species.
   - **Příloha 2** — population-indicator limits for good status, one row per species. Columns: Úspěšnost reprodukce (klíčový), Délka absence druhu (klíčový), Minimální přípustná početnost (klíčový), Početnost, Vývoj početnosti, Vitalita, Poškození rostlin, then Populační jednotka and Frekvence monitoringu. `.` means "indicator not used for this species".
   - **Příloha 3** — habitat-indicator limits, **wide format**: rows = indicators, columns = species. Each filled cell lists every scale category that counts as good status (e.g. `0; do 5 %; 5-25 %`). An empty cell means the indicator is not used for that species.
   - **Příloha 4** — the 125 localities (Cboh001…Cboh439) that enter the assessment for *Campanula bohemica*.
   - **Příloha 5** — placeholder for *Cerastium alsinifolium* localities (not delivered yet).

Read the xlsx with `readxl`/`openpyxl` by header position, not by eye: the Příloha 3 sheet has many empty cells and columns are easy to misalign.

## Ground rules

- **No assessment runs.** Do not run the assessment scripts, recompute status results or overwrite any output in this task. Reading code, reading input files and running small inspection snippets (e.g. `str()`, `unique()` on existing inputs) is fine. The only file you generate from data is the converted limits table (Phase 2).
- **Work on a new git branch** (e.g. `cevnate-metodika-2026`). Commit in small, described steps.
- **Never overwrite existing limits files.** Write the converted table as a new file next to the current one (see Phase 2); replacing the live file is my decision.
- **Logging.** Before doing anything else, find out how this repo logs its work so far — log files, a `log/` or `Data/Output/log` folder, a CHANGELOG/NEWS file, review notes such as `revize_metodika_*.md`, logging calls inside the R scripts, commit message style. Follow that convention exactly (file location, naming, language, level of detail). Every decision, assumption, normalisation, mapping and unresolved issue goes into the log. If there is no clear convention, stop and ask me.
- **Ask, don't guess.** If an interpretation affects the converted limits or a review verdict and the methodology does not settle it, record it as an open question and ask me. Batch questions at the checkpoints below rather than asking one at a time.
- Report and log in the language the repo already uses for these documents (earlier methodology reviews were in Czech).

## Phase 0 — Orient (then checkpoint)

1. Map the pipeline. I expect numbered scripts such as `00_n2k_config`, `21_1_n2k_druhy_akce` (indicators per record), `21_2_n2k_druhy_akce_lim` (comparison with limits), `22_n2k_obdobi`, `23_n2k_posledni_nalez`, `24_n2k_druhy_lokality` (DP/locality rollup), `25_n2k_druhy_uzemi` (site rollup), `27_n2k_druhy_zapis` (export), plus helpers from my package `rn2kcz`. Verify what actually exists; note any differences.
2. Document the **current limits format** precisely: `Data/Input/limity_vse.csv` (columns `DRUH, ID_IND, TYP_IND, LIM_IND, JEDNOTKA, KLIC, UROVEN`), how it is joined with `limity_cevky.csv` (and `limity_ryby.csv`), and the indicator codebook `cis_indikatory_popis.csv` (`ind_popis, ind_id, ind_r`). Record: encoding, separator, decimal mark, quoting, column order, species naming convention in `DRUH`, how direction/operators are encoded, how categorical limits with several acceptable values are encoded, how informative (non-assessed) indicators are marked, what `TYP_IND`, `KLIC`, `JEDNOTKA` and `UROVEN` take as values, and how site-level (`chu`) limits are stored vs locality-level (`lok`).
3. Find where vascular-plant indicator values come from (NDOP export fields, XML tags in `STRUKT_POZN`) and what the **raw category strings** look like in the data (e.g. how `do 5 %` or `dobrý` actually appear), so the converted limits match the strings the code compares against.

**Checkpoint 0:** show me (a) the logging convention you will follow, (b) the format specification above, (c) a proposed mapping from every column of Příloha 2 and every row of Příloha 3 to `ID_IND`/`ind_id` (existing IDs where they exist, clearly marked *provisional new IDs* following the existing pattern where they do not), and (d) your list of open questions. Wait for my answer before Phase 2.

## Phase 1 — Extract the methodology into a checklist

From the docx, build a numbered rule checklist (saved in the repo per the logging convention) of every rule the code must implement. At minimum:

- Data source: only records from the NDOP project *Monitoring druhů ČR* enter the assessment; other NDOP records only document presence (informative indicator *Poslední doložený výskyt*: verified records, localisation precision ≤ 1 km, ≥ 10 % overlap for polygon/line geometries).
- Only valid monitoring records; if a locality is visited several times in one year, use the record with the highest abundance, ties → the earliest date.
- Relative abundance classes converted to category medians (1–10 = 5.5; 11–100 = 55.5; 101–1 000 = 550.5; 1 001–10 000 = 5 500.5; 10 001–100 000 = 55 000.5; 100 001–1 000 000 = 550 000.5; > 1 000 000 = 1 000 001 — the docx has typos here, log them). Mandatory for *Coleanthus subtilis* and *Campanula bohemica*.
- Target abundance (cílová početnost) per locality = mean of the first three monitorings, with optional expert override.
- Indicator definitions: Minimální přípustná početnost and Početnost (current / target, %), Vývoj početnosti (last three values T1–T3; negative if T2 and T3 < T1), Vitalita, Poškození rostlin (sum of damage shares; four-class scale converted 1–10 = 5.5, 11–50 = 30.5, 51–100 = 75.5; informative only for *Coleanthus*), Úspěšnost reprodukce and Délka absence for *Coleanthus* (6-year window, > 100 individuals and ≥ 11 % fruiting; absence documented by ≥ 2 negative monitoring records and no other NDOP record).
- **Habitat indicators are informative only** and do not enter the locality verdict.
- Locality verdict: **dobrý / špatný / neznámý only** (no zhoršený at locality level). Dobrý = key indicator good AND at most one non-key indicator bad. Neznámý if the key indicator is unknown or more than one non-key indicator is unknown. An indicator is unknown if any primary input value is missing (exception for *Coleanthus* reproduction).
- Site level (EVL, MZCHÚ): dobrý / zhoršený / špatný from Tabulka 1 (*Coleanthus*: podíl lokalit v dobrém stavu + cílový stav) and Tabulka 2 (others: podíl populace v lokalitách v dobrém stavu + podíl lokalit v dobrém stavu + cílový stav). Total abundance = sum of the latest abundances per locality; cílové stavy from ISOP (EVL) or management plans (MZCHÚ).
- Locality selection: all localities in the assessed site, except *Campanula bohemica* (Příloha 4 list only) and *Cerastium alsinifolium* (Příloha 5, pending).
- Lost localities count towards the site assessment for 10 years after the last confirmed record; *Coleanthus* at least 12 years, removal only if suitable conditions were documented in ≥ 2 seasons in the last 6 years.

## Phase 2 — Convert the new limits into the original format

Produce the vascular-plant limits from `Tabulky fin.xlsx` in **exactly** the format documented in Phase 0, as a new file next to the current one (e.g. `Data/Input/limity_cevky_2026.csv` — follow the repo's naming if it differs). Write the conversion as a script (`R/` or wherever conversion/helper scripts live), so it is reproducible, not a hand-made table.

- Příloha 2 → one row per species × applicable indicator. Parse operator and value (`≥ 50 %`, `≤ 40 %`, `< 6 let`, `= pozitivní; setrvalý`, `= dostatečná`) into the encoding the code expects. `.` and empty cells → no row. Population unit and frequency are not limits; if the repo has a place for them, put them there, otherwise leave them out and log it.
- Key indicators (`KLIC`): Minimální přípustná početnost for all species except *Coleanthus subtilis*, where Úspěšnost reprodukce and Délka absence are key (see open issue below).
- Příloha 3 → pivot wide to long; keep every listed category, normalised to the raw strings used in the data (Phase 0, step 3). Mark all habitat indicators as informative using the repo's existing mechanism.
- Site-level limits from Tabulka 1 and 2 → `UROVEN` = site level, in whatever form the existing `chu` rows use.
- Species names → the `DRUH` convention already used (check against existing limits and NDOP taxon names); keep a mapping table of source name → `DRUH` in the log.
- Validation (structural checks only, no assessment): column names/order/types identical to the current file; every species of Příloha 1 present; every `ID_IND` present in `cis_indikatory_popis.csv` or listed as provisional new; no duplicate species × indicator × level rows; every categorical value exists in the raw data vocabulary or is flagged.
- Produce a **diff against the current `limity_cevky.csv`**: added, removed and changed rows per species and indicator.

## Phase 3 — Review the code against the methodology

For each checklist rule from Phase 1, determine whether the code implements it: **OK / differs / missing / unclear**, with file and line references and a short explanation. Pay particular attention to the locality verdict logic (the change to dobrý/špatný and the "max one non-key bad" rule), unknown-status handling, informative habitat indicators, visit selection within a year, target abundance, *Coleanthus*-specific logic, the 10/12-year rule for lost localities, the Příloha 4 locality filter for *Campanula bohemica*, and the site-level decision tables. Propose code changes as described patches in the report — **do not apply them** unless I say so.

Also report how the code currently handles the questions the methodology author left for me, so I can answer him:
- how localities are assigned to an assessed site (overlay, buffers, overlap %) — comment c8;
- where a "lost locality" flag could be stored, since the guarantor app does not persist it — c10;
- how invalid monitoring records (e.g. after mowing) could be excluded — c17;
- what buffer is applied to point/line geometries when matching NDOP records to localities — c31 (author suggests 5 or 10 m).

## Known issues in the sources — verify and log, do not silently fix

- Species names differ between sheets and the docx: *Aconitum\* moravicum* / *Aconitum firmum* / *Aconitum firmum subsp. moravicum*; *Gentianella\* bohemica* / *Gentianella bohemica* / *Gentianella praecox subsp. bohemica*; *Tephroseris\* moravica* / *Tephroseris longifolia subsp. moravica*; *Dianthus lumnitzerii* vs *lumnitzeri*; *Campanula bohemika* in the docx.
- Příloha 1 vs Příloha 2 disagree: frequency for *Aconitum* (3–6 vs 3–12), *Artemisia pancicii* and *Cirsium brachycephalum* (1–2 vs 1); unit for *Cerastium alsinifolium* and *Galium sudeticum* (m² vs dm²). Příloha 2 frequency is blank from *Cypripedium* onward. Typo `kvateoucí`.
- Leading spaces in some Příloha 2 values (*Dianthus arenarius*, *Poa riphaea* Vitalita); inconsistent `75-100%` / `50-75%` spacing in Příloha 3.
- *Coleanthus*: the text says one key indicator, the table has two (Úspěšnost reprodukce + Délka absence; see comments c23/c24). Poškození is informative for *Coleanthus* in the text but has a limit in the table. Vývoj početnosti is listed for *Coleanthus* although its abundance indicators are `.`.
- Příloha 3 mixes the five-class scale (`do 25 %`) and six-class scale (`do 5 %`, `5-25 %`). Some cells list only six-class categories — check whether data recorded on the five-class scale could ever pass. *Podíl dna nad vodní hladinou* is described as a three-class scale but its limits look like four-class categories. The four-class scale in the docx reads `51-10` (should be 51–100).
- Vitalita is defined as the ratio of flowering/fruiting to *sterile* units — check whether the code uses fertile/sterile or fertile/total, and flag the ambiguity.
- Vývoj početnosti: only the negative rule is stated explicitly; the positive rule is implied by the examples (T2 and T3 > T1). Log the interpretation used.
- Tabulka 2 is not exhaustive (e.g. population share ≥ 75 % with locality share 40–49 %, or population share < 60 % with locality share ≥ 40 %; the last row combines both conditions). Tabulka 1 does not cover locality share ≥ 50 % with cílový stav < 80 %. List every uncovered combination and ask me; do not invent a rule.
- *Cerastium alsinifolium* locality list (Příloha 5) is missing — log as a blocker for that species.

## Deliverables

1. Converted limits file + conversion script + diff vs current file.
2. Review report (Markdown, in the repo's review-document style) with: summary, rule checklist with verdicts, source-data issues, proposed patches, answers on c8/c10/c17/c31, open questions for me.
3. Log entries per the repo convention covering every step, decision and assumption.
4. Commits on the new branch; nothing merged, no live limits file replaced, no assessment outputs regenerated.

**Checkpoint at the end:** summarise the deliverables, list open questions, and wait for my decisions before any code changes are applied.
