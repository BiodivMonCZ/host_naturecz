#----------------------------------------------------------#
# Srovnani dvou behu kaskady (vystupy beh_kaskady.R)
#----------------------------------------------------------#
# Pouziti:  Rscript porovnej_behy.R <beh_A> <beh_B> [vystup.csv]
#
# Srovnava kazdy soubor v temp/ a export/ RADEK PO RADKU jako text (tedy
# presne to, co pipeline zapsala, vcetne poradi radku i sloupcu). Jedine
# maskovani: datum behu (sloupec datum_hodnoceni = Sys.Date() v exportu chu)
# se v obou behu nahradi retezcem <DATUM_BEHU>.
#
# Neni-li soubor shodny, vypise:
#   - zda se lisi jen poradi radku (stejna mnozina radku),
#   - pocet radku jen v A / jen v B,
#   - u CSV z temp/ sloupce, ve kterych se lisi (po sparovani radku podle
#     poradi, jsou-li pocty radku shodne), a pocty rozdilu po DRUH.
# Navratovy kod: 0 = vse shodne, 1 = nektery soubor se lisi.
#----------------------------------------------------------#

args <- commandArgs(trailingOnly = TRUE)
a_dir <- args[1]
b_dir <- args[2]
out_csv <- if (length(args) >= 3) args[3] else NA_character_

datum_behu <- function(d) {
  m <- readLines(file.path(d, "meta.txt"), encoding = "UTF-8")
  unique(sub("^datum_behu(_konec)?: ", "", grep("^datum_behu", m, value = TRUE)))
}
dat_a <- datum_behu(a_dir)
dat_b <- datum_behu(b_dir)

cti_radky <- function(path, data) {
  enc <- if (grepl("Windows-1250", path)) "CP1250" else "UTF-8"
  con <- if (grepl("\\.gz$", path)) gzfile(path, encoding = enc) else file(path, encoding = enc)
  on.exit(close(con))
  x <- readLines(con, warn = FALSE)
  for (d in data) x <- gsub(d, "<DATUM_BEHU>", x, fixed = TRUE)
  x
}

soubory <- function(d) {
  f <- c(file.path("temp", list.files(file.path(d, "temp"))),
         file.path("export", list.files(file.path(d, "export"))))
  f
}
fa <- soubory(a_dir)
fb <- soubory(b_dir)
vsechny <- sort(union(fa, fb))

vysledek <- list()
detail <- character(0)

for (f in vsechny) {
  if (!(f %in% fa) || !(f %in% fb)) {
    vysledek[[f]] <- data.frame(soubor = f, shoda = FALSE,
                                pozn = if (f %in% fa) "jen v A" else "jen v B",
                                radku_A = NA, radku_B = NA, jen_A = NA, jen_B = NA)
    next
  }
  xa <- cti_radky(file.path(a_dir, f), dat_a)
  xb <- cti_radky(file.path(b_dir, f), dat_b)
  if (identical(xa, xb)) {
    vysledek[[f]] <- data.frame(soubor = f, shoda = TRUE, pozn = "identicky",
                                radku_A = length(xa), radku_B = length(xb), jen_A = 0, jen_B = 0)
    next
  }
  ta <- table(xa); tb <- table(xb)
  k <- union(names(ta), names(tb))
  na_ <- as.integer(ta[k]); na_[is.na(na_)] <- 0L
  nb_ <- as.integer(tb[k]); nb_[is.na(nb_)] <- 0L
  jen_a <- sum(pmax(na_ - nb_, 0L)); jen_b <- sum(pmax(nb_ - na_, 0L))
  pozn <- if (jen_a == 0 && jen_b == 0) "lisi se jen poradi radku" else "lisi se obsah"
  if (xa[1] != xb[1]) pozn <- paste0(pozn, "; lisi se hlavicka")
  vysledek[[f]] <- data.frame(soubor = f, shoda = FALSE, pozn = pozn,
                              radku_A = length(xa), radku_B = length(xb), jen_A = jen_a, jen_B = jen_b)

  # Detail rozdilu pro CSV v temp/ (ctene jako text, sloupce dle hlavicky)
  if (grepl("^temp/.*\\.csv$", f) && jen_a + jen_b > 0) {
    da <- readr::read_csv(file.path(a_dir, f), col_types = readr::cols(.default = "c"), na = character())
    db <- readr::read_csv(file.path(b_dir, f), col_types = readr::cols(.default = "c"), na = character())
    detail <- c(detail, paste0("== ", f, ": A ", nrow(da), " x ", ncol(da), ", B ", nrow(db), " x ", ncol(db)))
    if (!identical(names(da), names(db))) {
      detail <- c(detail, paste0("   sloupce jen v A: ", paste(setdiff(names(da), names(db)), collapse = ", ")),
                  paste0("   sloupce jen v B: ", paste(setdiff(names(db), names(da)), collapse = ", ")))
    }
    spol <- intersect(names(da), names(db))
    ka <- do.call(paste, c(da[spol], sep = "\x1f"))
    kb <- do.call(paste, c(db[spol], sep = "\x1f"))
    ra <- da[!(ka %in% kb), , drop = FALSE]
    rb <- db[!(kb %in% ka), , drop = FALSE]
    if ("DRUH" %in% names(da)) {
      tab <- merge(as.data.frame(table(DRUH = ra$DRUH), responseName = "jen_A"),
                   as.data.frame(table(DRUH = rb$DRUH), responseName = "jen_B"), all = TRUE)
      detail <- c(detail, paste0("   ", capture.output(print(tab, row.names = FALSE))))
    }
    if (nrow(da) == nrow(db) && identical(names(da), names(db))) {
      dif <- vapply(names(da), function(cl) sum(da[[cl]] != db[[cl]]), integer(1))
      dif <- dif[dif > 0]
      detail <- c(detail, paste0("   radkove parovani - lisici se sloupce: ",
                                 paste(names(dif), dif, sep = "=", collapse = ", ")))
    }
  }
}

res <- do.call(rbind, vysledek)
rownames(res) <- NULL
print(res, right = FALSE)
if (length(detail)) cat("", detail, sep = "\n")
if (!is.na(out_csv)) {
  write.csv(res, out_csv, row.names = FALSE)
  if (length(detail)) writeLines(detail, sub("\\.csv$", "_detail.txt", out_csv))
}
vse <- all(res$shoda)
cat("\nVYSLEDEK: ", if (vse) "VSECHNY SOUBORY SHODNE" else "ROZDILY", "\n", sep = "")
quit(status = if (vse) 0 else 1)
