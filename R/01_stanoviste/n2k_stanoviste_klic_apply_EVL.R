# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# 11_n2k_stanoviste_klic.R aplikace na EVL
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

source(file.path("R", "01_stanoviste", "11_n2k_stanoviste_klic.R"))

out_path <- file.path("Output", "Data", "stanoviste")
out_file <- paste0("evl_klic_", format(Sys.Date(), "%Y%m%d"), ".csv")

# Check and define output

head(sites_habitats)

out <- data.frame(
  SITECODE = character(),
  NAZEV = character(),
  HABITAT_CODE = character(),
  ROZLOHA = numeric(),
  KVALITA = numeric(),
  TYPICKE_DRUHY = numeric(),
  REPRE = numeric(),
  REPRE_SDF = numeric(),
  CONSERVATION = numeric(),
  DEGREE_OF_CONSERVATION = numeric(),
  MRTVE_DREVO = numeric(),
  KALAMITA_POLOM = numeric(),
  RELATIVE_AREA_PERC = numeric(),
  EVL_AREA_PERC = numeric(),
  GOOD_DOC_AREA_HA = numeric(),
  W_AREA_HA = numeric(),
  W_AREA_PERC = numeric(),
  PASEKY_AREA_HA = numeric(),
  PASEKY_AREA_PERC = numeric(),
  DEGRAD_AREA_HA = numeric(),
  DEGRAD_AREA_PERC = numeric(),
  PERC_0 = numeric(),
  PERC_1 = numeric(),
  PERC_2 = numeric(),
  DATE_MIN = as.Date(character()),
  DATE_MAX = as.Date(character()),
  DATE_MEAN = as.Date(character()),
  DATE_MEDIAN = as.Date(character())
)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Main loop

for(i in seq_len(nrow(sites_habitats))){
  x <- n2k_hab_klic(evl_site = sites_habitats$site_code[[i]], hab_code = sites_habitats$feature_code[[i]])
  output_n2k_hab_klic <- rbind(output_n2k_hab_klic, x)
}

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Export

write_csv(out, file.path(out_path, out_file))

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# 
# # Paralelní výpočet
# # Napsátno tak, aby to fungovalo na Linuxu, nevím, jak si s tím poradí Win
# 
# # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# # Parallel setup
# 
# n_cores <- parallelly::availableCores()
# 
# cl <- parallel::makeCluster(n_cores, type = "FORK") # nutno přepsat pro win
# 
# doParallel::registerDoParallel(cl)
# 
# on.exit({
#   parallel::stopCluster(cl)
# }, add = TRUE)
# 
# 
# # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# # Parallel foreach loop
# 
# out <- foreach::`%dopar%`(
#   foreach::foreach(
#     i = seq_len(nrow(sites_habitats)),
#     .combine = dplyr::bind_rows
#   ),
#   {
#     n2k_hab_klic(
#       evl_site = sites_habitats$site_code[[i]],
#       hab_code = sites_habitats$feature_code[[i]]
#     )
#   }
# )
# 
# 
# # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# # Export
# 
# write_csv(out, file.path(out_path, out_file))
# 
# # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #