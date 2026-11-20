#' Příprava prostorového filtru pro načtení VMB
#'
#' Funkce převede geometrii lokality do S-JTSK / Krovak East North (EPSG:5514)
#' a ze všech jejích částí vytvoří jednu obdélníkovou obálku (bounding box).
#'
#' @param site_geom Objekt třídy \code{sf} nebo \code{sfc} s definovaným
#'   souřadnicovým systémem a alespoň jednou neprázdnou geometrií.
#' @param buffer_m Jedno konečné nezáporné číslo. Určuje, o kolik metrů se
#'   každá strana obálky posune směrem ven. Výchozí hodnota \code{0}
#'   vrátí obálku v původním rozsahu lokality.
#'
#' @return Jeden textový řetězec (typ \code{character} délky 1) obsahující
#'   obálku zapsanou jako WKT \code{POLYGON} se souřadnicemi v EPSG:5514.
#'   Výsledek lze přímo předat do argumentu \code{wkt_filter} funkce
#'   \code{load_vmb()}.
#'
#' @examples
#' \dontrun{
#' hrad <- dplyr::filter(evl, SITECODE == "CZ0414127")
#' hrad_wkt_100 <- make_wkt_site(hrad, buffer_m = 100)
#' vmb_hrad <- load_vmb(vmb_x = 0, wkt_filter = hrad_wkt_100)
#' }
#' @export
make_wkt_site <- function(site_geom, buffer_m = 0) {
  base::stopifnot(
    base::inherits(site_geom, "sf") || base::inherits(site_geom, "sfc"),
    !base::is.na(sf::st_crs(site_geom)),
    base::is.numeric(buffer_m), base::length(buffer_m) == 1L,
    base::is.finite(buffer_m), buffer_m >= 0
  )
  
  geom <- sf::st_transform(sf::st_geometry(site_geom), 5514)
  bbox <- sf::st_bbox(geom)
  if (!base::all(base::is.finite(bbox))) {
    base::stop("Lokalita má prázdnou nebo neurčitou prostorovou obálku.")
  }
  
  bbox[] <- bbox + base::c(-buffer_m, -buffer_m, buffer_m, buffer_m)
  sf::st_as_text(sf::st_as_sfc(bbox), digits = 16)
}
