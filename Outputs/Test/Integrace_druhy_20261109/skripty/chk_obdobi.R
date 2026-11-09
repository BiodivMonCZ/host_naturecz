args <- commandArgs(TRUE); d <- args[1]
x <- readr::read_csv(file.path(d, "temp/n2k_druhy_obdobi_chu.csv"), show_col_types = FALSE)
cat("obdobi_chu radku:", nrow(x), "\n")
print(table(cut(as.numeric(format(x$HODNOCENE_OBDOBI_OD, "%Y")), c(-Inf, 1990, 2000, 2010, 2013, Inf), right = FALSE)))
e <- read.csv2(gzfile(file.path(d, "export/n2k_druhy_chu_2025_UTF-8.csv.gz"), encoding = "UTF-8"), sep = ";", quote = "")
od <- as.numeric(substr(e$hodnocene_obdobi_od, 1, 4))
cat("export chu radku:", nrow(e), " hodnocene_obdobi_od < 2013:", sum(od < 2013, na.rm = TRUE), " min:", min(od, na.rm = TRUE), "\n")
cat("druhu v exportu chu:", length(unique(e$druh)), "\n")
