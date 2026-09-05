sc <- Sys.getenv("SCRATCH")
xml <- readChar(file.path(sc,"met","word","document.xml"), file.size(file.path(sc,"met","word","document.xml")), useBytes=TRUE)
Encoding(xml) <- "UTF-8"
# 1. prijmout revize: odstranit smazany a presunuty-pryc obsah
odstran <- function(x, tag) {
  vzor <- paste0("<w:", tag, "[ >].*?</w:", tag, ">")
  gsub(vzor, "", x, perl = TRUE)
}
for (t in c("del", "moveFrom")) xml <- odstran(xml, t)
xml <- gsub("<w:delText[^>]*>.*?</w:delText>", "", xml, perl=TRUE)

# 2. znacky pro strukturu
xml <- gsub("</w:tc>", "\t", xml, fixed=TRUE)          # konec bunky -> tabulator
xml <- gsub("</w:tr>", "\n[RADEK]\n", xml, fixed=TRUE) # konec radku tabulky
xml <- gsub("</w:p>", "\n", xml, fixed=TRUE)           # konec odstavce
xml <- gsub("<w:tbl>", "\n[TABULKA]\n", xml, fixed=TRUE)
xml <- gsub("</w:tbl>", "\n[/TABULKA]\n", xml, fixed=TRUE)
xml <- gsub("<w:br[^>]*/>", " ", xml, perl=TRUE)

# 3. vytahnout text
kusy <- gregexpr("<w:t(?: [^>]*)?>.*?</w:t>", xml, perl=TRUE)
txt <- xml
# nahradit vse mimo <w:t> prazdnem: jednodussi je odstranit tagy a nechat text
txt <- gsub("<w:t(?: [^>]*)?>", "\001", txt, perl=TRUE)
txt <- gsub("</w:t>", "\002", txt, fixed=TRUE)
# odstranit vsechny ostatni tagy
txt <- gsub("<[^>]*>", "", txt, perl=TRUE)
txt <- gsub("\001", "", txt, fixed=TRUE); txt <- gsub("\002", "", txt, fixed=TRUE)
# entity
txt <- gsub("&amp;","&",txt,fixed=TRUE); txt <- gsub("&lt;","<",txt,fixed=TRUE)
txt <- gsub("&gt;",">",txt,fixed=TRUE); txt <- gsub("&quot;",'"',txt,fixed=TRUE)
txt <- gsub("&apos;","'",txt,fixed=TRUE)
l <- strsplit(txt, "\n", fixed=TRUE)[[1]]
l <- trimws(l, which="right")
l <- l[nzchar(trimws(l))]
writeLines(l, file.path(sc, "metodika.txt"), useBytes=FALSE)
cat("radku:", length(l), " | tabulek:", sum(l == "[TABULKA]"), "\n")
