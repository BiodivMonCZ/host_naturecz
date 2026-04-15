# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# cesty k adresarum

dir_vmb1_vmb0 <- "../host_data/VMB1_VMB0"
dir_vmb1_vmb2 <- "../host_data/VMB1_VMB2"
dir_vmb2_vmb0 <- "../host_data/VMB2_VMB0"

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# predmety ochrany

sites_subjects <- openxlsx::read.xlsx(
  "Data/Input/seznam_predmetolokalit_Natura2000_2_2025.xlsx",
  sheet = 1
) %>%
  dplyr::rename(
    site_code = `Kód.lokality`,
    site_name = `Název.lokality`,
    site_type = `Typ.lokality`,
    feature_type = `Typ.předmětu.ochrany`,
    sdf_code = `Kód.SDF`,
    feature_code = `Kód.ISOP`,
    nazev_cz = `Název.česky`,
    nazev_lat = `Název.latinsky.(druh)`
  )

sites_habitats <- sites_subjects %>%
  dplyr::filter(feature_type == "stanoviště")


protected_habitats <- sites_habitats |>
  dplyr::transmute(
    sitecode = base::as.character(site_code),
    habitat = base::as.character(feature_code)
  ) |>
  dplyr::distinct()

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# FUN
# nacteni vsech GPKG ve slozce, ocisteni sloupcu

read_paseky_simple <- function(dir_path, pair_name) {
  
  files <- fs::dir_ls(
    path = dir_path,
    glob = "*.gpkg",
    recurse = FALSE
  ) |>
    base::as.character()
  
  purrr::map_dfr(files, function(f) {
    
    sf::st_read(f, quiet = TRUE) |>
      sf::st_drop_geometry() |>
      janitor::clean_names() |>
      dplyr::mutate(
        pair = pair_name,
        source_file = base::basename(f),
        sitecode = base::as.character(sitecode),
        habitat = base::as.character(habitat),
        region_id = base::as.character(region_id_x),
        datum_new = base::as.Date(datum_x),
        datum_old = base::as.Date(datum_x_1)
      ) |>
      dplyr::select(
        sitecode,
        habitat,
        region_id,
        pair,
        source_file,
        datum_new,
        datum_old
      )
    
  })
}

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# load data

system.time(
  x <- read_paseky_simple(dir_vmb1_vmb0, "VMB1_VMB0")
)

all_paseky <- dplyr::bind_rows(
  read_paseky_simple(dir_vmb1_vmb0, "VMB1_VMB0"),
  read_paseky_simple(dir_vmb1_vmb2, "VMB1_VMB2"),
  read_paseky_simple(dir_vmb2_vmb0, "VMB2_VMB0")
)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# prune, nechat jen predmety ochrany

all_paseky_po <- all_paseky |>
  dplyr::inner_join(
    protected_habitats,
    by = c("sitecode", "habitat")
  )

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# sumarizace
# pro kombinaci EVL-habitat(PO)-okrsek-casovy prurez VMBx-VMBx vezmi stare a nove datum

pair_summary <- all_paseky_po |>
  dplyr::group_by(sitecode, habitat, region_id, pair) |>
  dplyr::summarise(
    datum_new = base::max(datum_new, na.rm = TRUE),
    datum_old = base::max(datum_old, na.rm = TRUE),
    .groups = "drop"
  )

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# vyber nenjnovejsi kombinace
# napric casovymi prurezy VMBx-VMBx pro dane EVL-habitat-okrsek serad sestupne
# data (terminy), nasledne vezmi jenom prvni radek

latest_choice <- pair_summary |>
  dplyr::group_by(sitecode, habitat, region_id) |>
  dplyr::arrange(
    dplyr::desc(datum_new),
    dplyr::desc(datum_old),
    .by_group = TRUE
  ) |>
  dplyr::slice(1) |>
  dplyr::ungroup()

latest_choice

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# zapis vysledku

write.csv(all_paseky, file = file.path("..", "host_data", "paseky_results", "all_paseky.csv"))
write.csv(all_paseky_po, file = file.path("..", "host_data", "paseky_results", "all_paseky_po.csv"))
write.csv(pair_summary, file = file.path("..", "host_data", "paseky_results", "pair_summary.csv"))
write.csv(latest_choice, file = file.path("..", "host_data", "paseky_results", "latest_choice.csv"))

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #