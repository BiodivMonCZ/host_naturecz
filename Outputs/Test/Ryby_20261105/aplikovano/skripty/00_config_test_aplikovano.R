#----------------------------------------------------------#
# Orezany config pro testovaci beh - 2 druhy ----------------
#----------------------------------------------------------#
# Prebira R/00_config/00_n2k_config.R a R/00_config/02_n2k_data_druhy.R
# VYRAZ PO VYRAZU, doslovne. Nic neprepisuje ani neprepocitava - jen
# vynechava bloky, ktere na teto pracovni stanici nelze spustit, a omezuje
# vstupni data na hodnocene druhy.
#
# Prevzato z Outputs/Test/Triturus_cristatus_20260903/skripty/00_config_test_tc.R,
# jedina zmena je TEST_SPECIES (vektor misto jedne hodnoty) a odpovidajici
# filtr %in% misto ==.
#
# Vynechane bloky a duvod:
#   vodstvo                  - WFS CUZK (sit, dlouhy beh); druhova cast
#                              pipeline (R/02_druhy) ho nepouziva
#   biotop_zvld / BiotopZvld - chybi Data/Input/BiotopZvld.shp, fallback by
#                              stahoval z WFS; v R/02_druhy se nepouziva
#   n2k_union                - drahy prostorovy join evl x po; nepouziva se
#   akt_okrsky               - chybi Data/Input/AktualizacniOkrsky.shp
#   redlist / invaze /       - chybi export_redlist.csv, export_invaze.csv
#   expanze                    a export_expanze.csv v host_data
#   rn2kcz z GitHubu         - balicek se v R/02_druhy nikde nepouziva
#
# Omezeni na druhy: aplikuje se az POTE, co je znam `ncol_orig` (pocet
# sloupcu suroveho exportu), tedy pred blokem `n2k_load`. Vsechny
# transformace v tom bloku jsou radkove (mutate / distinct / rename),
# predfiltrovani je proto vuci plnemu behu ekvivalentni.
#
# POZOR - oba druhy v JEDNOM behu: faze 1 v 27_n2k_druhy_zapis.R deli data
# pres split(DRUH) a pocita po druzich, takze spolecny beh dava stejne
# vysledky jako dva samostatne behy. Zaroven se tim nacte 142MB export jen
# jednou.
#----------------------------------------------------------#

TEST_SPECIES <- c("Cottus gobio", "Lampetra planeri", "Misgurnus fossilis", "Triturus cristatus", "Bombina bombina")

cfg_paths <- c(
  "R/00_config/00_n2k_config.R",
  "R/00_config/02_n2k_data_druhy.R"
)

skip_tokens <- c(
  "vodstvo",
  "BiotopZvld",
  "biotop_zvld",
  "n2k_union",
  "akt_okrsky",
  "AktualizacniOkrsky",
  "export_redlist",
  "export_invaze",
  "export_expanze",
  "redlist_species",
  "invasive_species",
  "expansive_species",
  "rn2kcz"
)

cfg_exprs <- unlist(
  lapply(cfg_paths, function(p) as.list(parse(p, encoding = "UTF-8"))),
  recursive = FALSE
)

message("=== ORIZNUTY CONFIG: ", length(cfg_exprs), " vyrazu nejvyssi urovne z ",
        length(cfg_paths), " souboru ===")

cfg_skipped <- character(0)
cfg_filtered <- FALSE

for (i in seq_along(cfg_exprs)) {

  e <- cfg_exprs[[i]]
  txt <- paste(deparse(e), collapse = "\n")

  hit <- vapply(skip_tokens, grepl, logical(1), x = txt, fixed = TRUE)

  if (any(hit)) {
    lbl <- sub("\n.*$", "", txt)
    cfg_skipped <- c(cfg_skipped, lbl)
    message("  [skip ", i, "] ", substr(lbl, 1, 70),
            "   <- ", paste(skip_tokens[hit], collapse = ", "))
    next
  }

  eval(e, envir = globalenv())

  if (!cfg_filtered &&
      exists("ncol_orig", envir = globalenv()) &&
      exists("n2k_export", envir = globalenv())) {

    n_all <- nrow(get("n2k_export", envir = globalenv()))

    assign(
      "n2k_export",
      dplyr::filter(get("n2k_export", envir = globalenv()), DRUH %in% TEST_SPECIES),
      envir = globalenv()
    )
    assign(
      "volna_export",
      dplyr::filter(get("volna_export", envir = globalenv()), DRUH %in% TEST_SPECIES),
      envir = globalenv()
    )

    cfg_filtered <- TRUE

    message(
      "  [filtr] ", paste(TEST_SPECIES, collapse = " + "), ": export_data_evl ",
      n_all, " -> ", nrow(get("n2k_export", envir = globalenv())), " radku; ",
      "ncol_orig = ", get("ncol_orig", envir = globalenv())
    )

    gc(verbose = FALSE)
  }
}

# Pojistka: kdyby se filtr z jakehokoli duvodu neuplatnil vyse.
n2k_load <- dplyr::filter(n2k_load, DRUH %in% TEST_SPECIES) %>%
  dplyr::mutate(DRUH = droplevels(as.factor(DRUH)))

message("=== CONFIG HOTOV: n2k_load = ", nrow(n2k_load), " zaznamu, ",
        ncol(n2k_load), " sloupcu; preskoceno ", length(cfg_skipped), " vyrazu ===")
for (sp in TEST_SPECIES) {
  message("     ", sp, ": ", sum(as.character(n2k_load$DRUH) == sp), " zaznamu")
}

#----------------------------------------------------------#
# KONEC ----
#----------------------------------------------------------#
