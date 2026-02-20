# 1. NAČTENÍ DAT A FILTRACE NA LESNÍ STANOVIŠTĚ (kód 9)
df_raw <- read_csv(
  "Outputs/Data/stanoviste/n2k_stanoviste_2025_20260220_Windows-1250.csv", 
  locale = readr::locale(encoding = "Windows-1250")
  ) %>%
  # Okamžitá filtrace pouze na kódy začínající 9 (lesy)
  filter(str_starts(feature_code, "9"))

# 2. PŘÍPRAVA DAT (TIDY DATA)
df_clean <- df_raw %>%
  # Oprava textových desetinných čárek na tečky, aby šly převést na čísla
  mutate(parametr_hodnota = as.numeric(str_replace(parametr_hodnota, ",", "."))) %>%
  # Ponecháme si pouze řádky týkající se rozlohy (128) a mrtvého dřeva (132)
  filter(parametr_nazev %in% c(128, 162)) %>%
  # Vybereme jen potřebné sloupce
  select(kod_chu, nazev_chu, druh, feature_code, parametr_nazev, parametr_hodnota) %>%
  # Pojistka: pokud by pro jedno území a kód bylo zapsáno více hodnot, zprůměrujeme je
  group_by(kod_chu, nazev_chu, druh, feature_code, parametr_nazev) %>%
  summarise(hodnota = mean(parametr_hodnota, na.rm = TRUE), .groups = "drop") %>%
  # Překlopení dat z dlouhého do širokého formátu (rozloha a indikátor budou mít svůj sloupec)
  pivot_wider(
    names_from = parametr_nazev,
    values_from = hodnota,
    names_prefix = "param_"
  ) %>%
  # Přejmenování sloupců pro přehlednost
  rename(
    ROZLOHA = param_128,
    MRTVE_DREVO = param_162
  ) %>%
  # Odstraníme záznamy, kde chybí buď rozloha, nebo mrtvé dřevo (to způsobovalo tu chybu s facet_wrap)
  filter(!is.na(ROZLOHA) & !is.na(MRTVE_DREVO))

# 3. KATEGORIZACE A VYKRESLENÍ GRAFU
hist_md <- 
  df_clean %>%
  mutate(
    area_category = cut(
      ROZLOHA, 
      breaks = c(-Inf, 10, 100, Inf), 
      labels = c("malá rozloha (< 10 ha)", 
                 "střední rozloha (10–100 ha)", 
                 "velká rozloha (> 100 ha)")
    )
  ) %>%
  ggplot(aes(x = MRTVE_DREVO, fill = area_category, color = area_category)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity", linewidth = 0.5) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) + 
  scale_color_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  facet_wrap(~area_category, ncol = 1) +
  theme_minimal() +
  labs(
    #title = "Mrtvé dřevo dle rozlohy předmětu ochrany",
    #subtitle = "Pouze pro lesní stanoviště (kódy začínající na 9)",
    x = "\nhodnota indikátoru mrtvé dřevo", 
    y = "četnost\n"
  ) +
  theme(
    legend.position = "none", # Legenda už není potřeba, máme názvy nad grafy
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 10)
  )

ggsave(
  filename = "Outputs/Grafy/histogram_rozloha_mrtve_drevo.png", # Název a formát (může být i .jpg, .tiff nebo .pdf)
  plot = hist_md,     # Který graf ukládáme
  width = 20,          # Šířka obrázku
  height = 15,         # Výška obrázku
  units = "cm",        # Jednotky rozměrů ("cm", "mm", nebo "in" pro palce)
  dpi = 300,           # Rozlišení (300 je standard pro tisk/publikace)
  bg = "white"         # Zabrání průhlednému pozadí u theme_minimal()
)

# 3. VYKRESLENÍ SKLÁDANÉHO (STACKED) HISTOGRAMU
df_clean %>%
  mutate(
    area_category = cut(
      ROZLOHA, 
      breaks = c(-Inf, 10, 100, Inf), 
      labels = c("malá rozloha (< 10 ha)", 
                 "střední rozloha (10–100 ha)", 
                 "velká rozloha (> 100 ha)")
    )
  ) %>%
  # Použijeme fill pro kategorii, ale color necháme jednotnou pro ohraničení
  ggplot(aes(x = MRTVE_DREVO, fill = area_category)) +
  geom_histogram(
    bins = 30, 
    alpha = 0.85, # Menší průhlednost, u stacked vypadá lépe sytější barva
    position = "stack", 
    color = "white", # Bílé čáry oddělí jednotlivé "cihličky" ve sloupci
    linewidth = 0.3
  ) +
  scale_fill_viridis_d(
    option = "D", 
    begin = 0.2, 
    end = 0.8,
    name = "Kategorie rozlohy" # Návrat názvu legendy
  ) + 
  theme_minimal() +
  labs(
    title = "Mrtvé dřevo dle rozlohy předmětu ochrany (Skládaný histogram)",
    subtitle = "Celková distribuce a podíl rozlohových kategorií u lesních stanovišť",
    x = "hodnota indikátoru mrtvé dřevo\n", 
    y = "Celková četnost"
  ) +
  theme(
    legend.position = "top", # Legenda se vrací nahoru
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

# 4. STATISTICKÁ ANALÝZA V KONZOLI
# Používáme metodu Spearman, která je odolná vůči odlehlým hodnotám (outliers) v rozloze
cat("\n=== STATISTICKÁ ANALÝZA KORELACE ===\n")
cor_test <- cor.test(df_clean$ROZLOHA, df_clean$MRTVE_DREVO, method = "spearman")
print(cor_test)

# 3. BODOVÝ GRAF S TRENDOVOU PŘÍMKOU A LOGARITMICKOU OSOU X
korelace_md <- 
  ggplot(df_clean, aes(x = ROZLOHA, y = MRTVE_DREVO)) +
  # Přidání bodů s lehkou průhledností kvůli překryvu
  geom_point(alpha = 0.5, color = "#2D708E", size = 2) +
  # Regresní přímka s intervalem spolehlivosti
  geom_smooth(method = "lm", color = "firebrick", fill = "pink", alpha = 0.3) +
  # KLÍČOVÝ KROK: Logaritmická transformace osy X (lépe zobrazí malá i velká území)
  scale_x_log10(
    labels = scales::comma # Aby osa neukazovala e-notaci (1e3 apod.), ale normální čísla
  ) +
  theme_minimal() +
  labs(
    title = "Korelace mezi rozlohou a mrtvým dřevem",
    subtitle = sprintf(
      "Spearmanova korelace: rho = %.3f, p-value = %.4f (n = %d)", 
      cor_test$estimate, 
      cor_test$p.value, 
      nrow(df_clean)
    ),
    x = "\nrozloha (ha) - logaritmická škála", 
    y = "hodnota indikátoru mrtvé dřevo\n"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank() # Vyčištění drobných mřížek
  )

ggsave(
  filename = "Outputs/Grafy/korelace_rozloha_mrtve_drevo.png", # Název a formát (může být i .jpg, .tiff nebo .pdf)
  plot = korelace_md,     # Který graf ukládáme
  width = 20,          # Šířka obrázku
  height = 15,         # Výška obrázku
  units = "cm",        # Jednotky rozměrů ("cm", "mm", nebo "in" pro palce)
  dpi = 300,           # Rozlišení (300 je standard pro tisk/publikace)
  bg = "white"         # Zabrání průhlednému pozadí u theme_minimal()
)

# 1. DATA PREPARATION
data_beskydy <- sf::st_read(
  paste0(
    slozka_lokal,
    "CHKO_Beskydy/CHKO_Beskydy_Biotop.dbf"
  ),
  options = "ENCODING=WINDOWS-1250"
) %>%
  dplyr::filter(
    str_starts(BIOTOP, "L")
  ) %>%
  dplyr::mutate(
    MD_hod = dplyr::case_when(
      DG == "W" ~ 0,
      RB == "W" ~ 0,
      #MD == 3 ~ 0,
      #MD == 4 ~ 0,
      TRUE ~ as.numeric(MD)
    ),
    # Crucial step: Convert MD_hod to a factor for categorical analysis
    MD_hod = as.factor(MD_hod)
  )  %>%
  # Good practice: explicitly drop records missing the categorical variable
  tidyr::drop_na(MD_hod)

# 2. BEAUTIFICATION THEME
# Creating a clean, custom theme inspired by the tutorial to remove grey backgrounds
theme_codingclub <- function(){
  theme_bw() +
    theme(
      text = element_text(size = 12),
      axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14, face = "bold"),
      axis.line = element_line(colour = "black", linewidth = 0.5),
      panel.border = element_blank(),
      panel.grid.major = element_blank(), # Removes grid lines to focus on data
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, colour = "grey30"),
      legend.position = "none", # Removes redundant legend
      plot.margin = unit(c(1, 1, 1, 1), units = "cm")
    )
}

# 3. STATISTICAL ANALYSIS
cat("\n=== STATISTICKÁ ANALÝZA: KRUSKAL-WALLISŮV TEST ===\n")
kw_test_beskyd <- kruskal.test(PLO_BIO_M2 ~ MD_hod, data = data_beskydy)
print(kw_test_beskyd)

# 4. BEAUTIFIED RAINCLOUD PLOT
# 4. BEAUTIFIED RAINCLOUD PLOT - OPRAVA PŘEKRYVU
raincloud_md_beskydy <- 
  ggplot(data_beskydy, aes(x = PLO_BIO_M2, y = MD_hod, fill = MD_hod, colour = MD_hod)) +
  
  # 1. 'Cloud' (Hustota / half-violin)
  # justification = -0.1 odsadí housle mírně NAD osu kategorie
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -0.1, 
    .width = 0,           # Skryje vnitřní černý bod/interval (máme vlastní boxplot)
    point_colour = NA,    
    alpha = 0.7,
    # height = 0.6        # Odkomentuj, pokud by se housle horní kategorie dotýkaly
  ) +
  
  # 2. 'Umbrella' (Boxplot)
  # Zůstává vycentrovaný na samotné ose kategorie
  geom_boxplot(
    width = 0.12,         # Zúženo pro elegantnější vzhled
    outlier.shape = NA, 
    alpha = 0.5,
    colour = "black"    
  ) +
  
  # 3. 'Rain' (Surová data / Body)
  # Klíčový trik: Převedení faktoru MD_hod na číslo a fixní odečtení (-0.25)
  # posune body striktně POD boxplot, takže nedochází k žádnému překryvu.
  geom_jitter(
    aes(y = as.numeric(MD_hod) - 0.25), 
    height = 0.05,        # Záměrně velmi malý vertikální rozptyl (udrží čistý "déšť")
    alpha = 0.4,
    size = 1.2
  ) +
  
  # Barvy (ponechán mako z viridis)
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  scale_colour_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  
  # Logaritmická transformace osy X
  scale_x_log10(
    labels = scales::comma,
    expand = expansion(mult = c(0.05, 0.1)) # Brání ořezu největších/nejmenších hodnot na okrajích
  ) +
  
  # Použití vytvořeného tématu
  theme_codingclub() +
  
  labs(
    title = "Rozdělení plochy biotopu podle kategorie mrtvého dřeva",
    subtitle = sprintf(
      "Kruskal-Wallisův test: chi-squared = %.3f, p-value = %.4f (n = %d)", 
      kw_test_beskyd$statistic, 
      kw_test_beskyd$p.value, 
      nrow(data_beskydy)
    ),
    y = "Hodnota indikátoru mrtvé dřevo (kategorie)\n", 
    x = "\nRozloha (m2) - logaritmická škála"
  )

# Zobrazení grafu
print(raincloud_md_beskydy)
