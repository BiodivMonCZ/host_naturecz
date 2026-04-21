#----------------------------------------------------------#
# Nacteni VMB ----
#----------------------------------------------------------#

data_orig <- load_vmb(vmb_x = 1)
data_akt <- load_vmb(vmb_x = 0)

vmb_shp_sjtsk_orig <- data_orig$vmb_shp_sjtsk_orig

vmb_pb_x_akt <- data_akt$vmb_pb_x_akt

#----------------------------------------------------------#
# Prostorová funkce pro výpočet pasek
#----------------------------------------------------------#
paseky_spat <- function(
    hab_code,
    evl_site,
    zakl = "VMB1",
    aktu = "VMB0",
    typ_chu
){
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Načíst data z .GlobalEnv
  
  # výběr typu chráněného území
  if(typ_chu == "EVL"){
    uzemi <- evl
  }else if(typ_chu == "MZCHU"){
    uzemi <- mzchu
  }else if(typ_chu == "OKRSEK"){
    uzemi <- akt_okrsek
  }else{
    stop("Neplatná hodnota argumentu 'typ_chu'! \n'EVL', 'MZCHU', 'OKRSEK'")
  }
  
  # výběr aktuálního mapování
  if(aktu == "VMB0"){
    vmb_aktu <- vmb_pb_x_akt
  }else if(aktu == "VMB2"){
    vmb_aktu <- vmb_pb_x_a1
  }else {
    stop("Neplatná hodnota argumentu 'aktu'! \n'VMB0', 'VMB2'")
  }
  
  # výběr základní vrstyv
  if(zakl == "VMB1"){
    vmb_zakl <- vmb_shp_sjtsk_orig
  }else if(zakl == "VMB2"){
    vmb_zakl <- vmb_shp_sjtsk_a1
  }else if(zakl == "VMB0"){
    stop("Hajdaláku, nejaktuálnější vrstva VMB0 nemůže být základem pro paseky!")
  }else{
    stop("Neplatná hodnota argumentu 'zakl'! \n'VMB1', 'VMB2'")
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Lesní biotop check
  
  # inicializace výsledku pro případ krachu
  result <- NULL
  
  # prvvní filtr na lesní biotopy
  if(!substr(hab_code, 1, 1) %in% c("9", "L")){
    message("Biotop ", hab_code, " není lesním biotopem a tudíž na jeho místě nemůže vzniknout paseka.")
    return(invisible(NULL))
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # CRS check
  
  target_crs <- sf::st_crs(uzemi)
  
  if(sf::st_crs(vmb_aktu) != target_crs){
    message("Transformuji vmb_aktu na shodný CRS...")
    vmb_aktu <- sf::st_transform(vmb_aktu, target_crs)
  }
  
  if(sf::st_crs(vmb_zakl) != target_crs){
    message("Transformuji vmb_zakl na shodný CRS...")
    vmb_zakl <- sf::st_transform(vmb_zakl, target_crs)
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Vyber uzemi
  
  # tvorba prosotorového filtru podle SITECODE
  if(!"SITECODE" %in% names(uzemi)){
    stop("Vrstva 'uzemi' neobsahuje sloupec SITECODE.")
  }
  
  uzemi_filter <- dplyr::filter(uzemi, SITECODE == evl_site)
  
  # check existence území
  if(nrow(uzemi_filter) == 0){
    message("Kód území ", evl_site, " nebyl nalezen.")
    return(invisible(NULL))
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # VMB ZAKL
  
  # zachovat pouze segmenty zakladniho mapovani s target biotopem
  vmb_zakl_target <- vmb_zakl %>%
    dplyr::filter(HABITAT == hab_code | BIOTOP == hab_code)
  
  # prefilter
  vmb_zakl_filtered <- sf::st_filter(
    x = vmb_zakl_target,
    y = uzemi_filter,
    .predicate = sf::st_intersects
  )
  
  # je target biotop ve starsim mapovani v danem uzemi?
  if(nrow(vmb_zakl_filtered) == 0){
    message("Pro ", evl_site, " není ", hab_code, " ve starším mapování v daném území.")
    return(invisible(NULL))
  }
  
  # intersection, kde je paseka?
  vmb_target_sjtsk_orig <- sf::st_intersection(
    vmb_zakl_filtered,
    uzemi_filter
  ) %>%
    dplyr::filter(
      as.character(sf::st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON")
    ) %>%
    dplyr::mutate(
      AREA_real_orig = units::drop_units(sf::st_area(.)),
      PLO_BIO_M2_EVL_orig = STEJ_PR / 100 * AREA_real_orig
    ) %>%
    dplyr::rename(
      FSB_orig = FSB,
      BIOTOP_orig = BIOTOP,
      STEJ_PR_orig = STEJ_PR
    )
  
  # safe check, tohle by se nikdy nemělo spustit, protože to testuje už st_intersects
  if(nrow(vmb_target_sjtsk_orig) == 0){
    message("Pro ", evl_site, " a ", hab_code, " po průniku starší vrstvy nevznikla žádná polygonová geometrie.")
    return(invisible(NULL))
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # VMB AKTU
  
  # prefilter ‒ vybere jenom ty segmenty, které interagují s uzemi
  vmb_aktu_filtered <- sf::st_filter(
    x = vmb_aktu,
    y = uzemi_filter,
    .predicate = sf::st_intersects
  )
  
  # check jestli se uzemi potkava s vmb_aktu
  if(nrow(vmb_aktu_filtered) == 0){
    message("Pro ", evl_site, " není v novějším mapování žádný segment.")
    return(invisible(NULL))
  }
  
  # intersection samotná
  vmb_target_sjtsk_update <- 
    sf::st_intersection(vmb_aktu_filtered, uzemi_filter) %>%
    dplyr::filter(
      base::as.character(sf::st_geometry_type(.)) %in% base::c("POLYGON", "MULTIPOLYGON")
    ) %>%
    dplyr::mutate(
      AREA_real_update = units::drop_units(sf::st_area(.)),
      PLO_BIO_M2_EVL_update = STEJ_PR / 100 * AREA_real_update
    ) %>%
    dplyr::rename(
      FSB_update = FSB,
      BIOTOP_update = BIOTOP,
      STEJ_PR_update = STEJ_PR,
      ROK_AKT_update = ROK_AKT.x
    )
  
  # safe check, tohle by se nikdy nemělo spustit, protože to testuje už st_intersects
  if(nrow(vmb_target_sjtsk_update) == 0){
    message("Pro ", evl_site, " jsou ve vmb_aktu segmenty, ale po průniku nevznikla žádná polygonová geometrie.")
    return(invisible(NULL))
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Příprava dat pro výpočet pasek
  
  # T/F dotčené segmenty, které má cenu uvažovat při výpočtu
  hit_list <- sf::st_intersects(
    vmb_target_sjtsk_update,
    vmb_target_sjtsk_orig
  )
  
  # safe check, nemělo by nikdy proběhnout, dokud je vmb_aktu kompletní
  if(!any(lengths(hit_list) > 0)){
    message("Pro ", evl_site, " a ", hab_code, " je biotop ve starším mapování přítomen, ale nepřekrývá se s žádným segmentem nového mapování.")
    return(invisible(NULL))
  }
  
  # vyber dotčené segmenty pro výpočet
  update_sub <- vmb_target_sjtsk_update[lengths(hit_list) > 0,] # vyber hit_list z vmb_aktu
  orig_sub <- vmb_target_sjtsk_orig[sort(unique(unlist(hit_list))),] # vyber hit_list z vmb_zakl
  
  # safe check, chytá to i safecheck hit_listu výše
  if(nrow(update_sub) == 0 || nrow(orig_sub) == 0){
    message("Pro ", evl_site, " a ", hab_code, " nevznikl žádný relevantní subset pro finální průnik.")
    return(invisible(NULL))
  }
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Výpočet pasek
  
  # hlavní výpočet pasek a holin
  result <- sf::st_intersection(
    update_sub,
    orig_sub
  ) %>%
    dplyr::filter(
      as.character(sf::st_geometry_type(.)) %in% c("POLYGON", "MULTIPOLYGON")
    ) %>%
    dplyr::mutate(
      PASEKA = dplyr::case_when(
        BIOTOP_update %in% c("LP", "X10") ~ 1,
        BIOTOP_update %in% c("X11", "X12A", "X12B") &
          ROK_AKT_update %in% 2007:2012 ~ 1,
        TRUE ~ 0
      ),
      AREA_real_intersection = units::drop_units(sf::st_area(.)),
      PLO_BIO_M2_EVL_intersection =
        AREA_real_intersection * STEJ_PR_orig / 100 * STEJ_PR_update / 100,
      HOLINA = dplyr::case_when(
        PASEKA == 1 & PLO_BIO_M2_EVL_intersection > 10000 ~ 1,
        TRUE ~ 0
      )
    )
  
  # check, že nějaký průnik vzniknul
  if(nrow(result) == 0){
    message("Pro ", evl_site, " a ", hab_code, " nevznikl žádný průnik.")
    return(invisible(NULL))
  }
  
  # deduplicate sloupců
  result <- janitor::clean_names(result)
  
  # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
  # Zápis výsledků
  
  # outdir
  out_dir <- file.path("Outputs", "Data", "stanoviste", "paseky", typ_chu, paste0(zakl, "_", aktu))
  dir.create(out_dir, recursive = TRUE)
  
  # outfile
  file_path <- file.path(out_dir, paste0(typ_chu, "_", evl_site, "_", hab_code, "_", zakl, "_", aktu, ".gpkg"))
  
  # vymazat, jestli už existuje
  if(file.exists(file_path)){
    ok_remove <- file.remove(file_path)
    if(!ok_remove){
      stop("Nelze smazat existující soubor: ", file_path, ". Ujisti se, že není otevřený v GISu.")
    }
  }
  
  # zápis dat na disk
  sf::st_write(
    obj = result,
    dsn = file_path,
    layer = paste0(typ_chu, "_", evl_site, "_", hab_code, "_", zakl, "_", aktu),
    driver = "GPKG"
  )
  
  message("Pro ", evl_site, " a ", hab_code, " vrstva zapsána.")
  
  # návrat pokud, chci výsledek rovnou do objektu
  return(invisible(result))
}

#----------------------------------------------------------#
# Vypocet GIS vrstvy ----
#----------------------------------------------------------#
paseky_spat(sites_habitats_mzchu_test[8,5], sites_habitats_mzchu_test[8,1], typ_chu = "MZCHU")

# Inicializace progress baru
pb <- progress::progress_bar$new(
  # Přidal jsem :current/:total pro lepší přehled
  format = "  Zpracovávám [:bar] :percent | :current/:total | ETA: :eta", 
  total = nrow(sites_habitats_mzchu_test),
  clear = FALSE,
  width = 100
)

# Loop s "odchytáváním" zpráv
for(i in 1:nrow(sites_habitats_mzchu_test)){
  
  # Posuneme bar
  pb$tick()
  
  # Spuštění funkce v obalce, která řeší vizuál
  tryCatch({
    withCallingHandlers({
      
      # Tvoje funkce
      paseky_spat(sites_habitats_mzchu_test[i,5], sites_habitats_mzchu_test[i,1], typ_chu = "MZCHU")
      
    }, message = function(m){
      # TOTO JE KLÍČOVÉ:
      # 1. Vezmeme text zprávy a odstraníme prázdné znaky na konci
      txt <- trimws(m$message, which = "right")
      
      # 2. Vypíšeme ji skrz progress bar (objeví se nad ním)
      if(nchar(txt) > 0){
        pb$message(txt) 
      }
      
      # 3. Potlačíme původní zprávu, aby se nevytiskla 2x
      invokeRestart("muffleMessage")
    })
  }, error = function(e){
    # Pokud nastane chyba, vypíšeme ji také hezky přes bar
    pb$message(paste("!!! CHYBA:", e$message))
  })
}
