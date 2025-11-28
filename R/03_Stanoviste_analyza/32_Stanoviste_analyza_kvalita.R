#----------------------------------------------------------#
# Nacteni dat -----
#----------------------------------------------------------#
data_raw <-
  readr::read_csv(
    "Outputs/Data/stanoviste/n2k_stanoviste_2024_20251103_Windows-1250.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  )

data_stanoviste <-
  data_raw %>%
  dplyr::mutate(
    parametr_hodnota = as.numeric(str_replace(parametr_hodnota, ",", "."))
    ) %>%
  dplyr::mutate(
    skupina = str_sub(feature_code, start = 1, end = 1)
  ) 

data_hab <- 
  data_stanoviste %>%
  dplyr::select(
    kod_chu,
    feature_code, 
    skupina,
    parametr_nazev,
    parametr_hodnota
  ) %>%
  dplyr::filter(parametr_nazev %in% c(128, 129)) %>%
  tidyr::pivot_wider(
    .,
    names_from = parametr_nazev,
    values_from = parametr_hodnota) %>%
  dplyr::rename(
    rozloha = `128`,
    kvalita = `129`
  ) %>%
  dplyr::left_join(
    .,
    cis_habitat,
    by = c("feature_code" = "KOD_HABITAT")
  ) 

rozloha_prumer <- 
  data_stanoviste %>%
  dplyr::filter(parametr_nazev == 128) %>%
  dplyr::group_by(
    feature_code
  ) %>%
  dplyr::reframe(
    rozloha_prumer = mean(parametr_hodnota, na.rm = TRUE)
  ) 

kvalita_prumer <- 
  data_stanoviste %>%
  dplyr::filter(parametr_nazev == 129) %>%
  dplyr::group_by(
    feature_code
  ) %>%
  dplyr::reframe(
    kvalita_prumer = mean(parametr_hodnota, na.rm = TRUE)
  ) 

stanoviste_prumer <- 
  rozloha_prumer %>%
  dplyr::left_join(
    .,
    kvalita_prumer
  ) %>%
  dplyr::mutate(
    skupina = str_sub(feature_code, start = 1, end = 1)
  ) %>%
  dplyr::left_join(
    .,
    cis_habitat,
    by = c("feature_code" = "KOD_HABITAT")
  ) 

lesy <-
  data_hab %>%
  dplyr::filter(skupina == 9)%>%
  dplyr::filter(grepl("buč", NAZEV_HABITAT) == TRUE)

lesy_prumer <-
  stanoviste_prumer %>%
  dplyr::filter(skupina == 9)

ggplot(lesy, aes(x = log10(rozloha), y = kvalita, color = feature_code)) +
  geom_point(size = 3, alpha = 0.8) +        # Points with slight transparency
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +                          # A clean, modern theme
  labs(
    x = "log(rozloha)",
    y = "kvalita",
    color = "Feature Code"
  )

lesy_oop <- 
  lesy %>%
  dplyr::left_join(
    .,
    evl %>%
      dplyr::select(
        SITECODE,
        oop
      ) %>%
      st_drop_geometry(),
    by = c("kod_chu" = "SITECODE")
  ) %>%
  tidyr::separate_rows(
    oop, 
    sep = ", "
  )

ggplot(lesy_oop, aes(x = as.factor(oop), y = kvalita, color = feature_code)) +
  
  # 1. Use geom_boxplot()
  geom_boxplot(aes(fill = feature_code), # Use fill for the boxes, color for outliers/borders
               outlier.shape = 21,      # Use a filled circle for outliers
               outlier.size = 2,
               alpha = 0.7) +            # Add slight transparency
  
  # 2. Flip Coordinates for Better X-axis Label Readability
  coord_flip() +
  
  # 3. Choose a better colour palette (Viridis is colorblind-friendly)
  scale_color_viridis_d(option = "D") +  # D is the default Viridis scale
  scale_fill_viridis_d(option = "D") +
  
  # 4. Use a cleaner theme
  theme_light() +
  
  # 5. Correct and Enhance Labels
  labs(
    x = "orgán ochrany přírody", # Corrected X-axis label
    y = "kvalita přírodního stanoviště",
    color = "kód typu přírodního stanoviště",
    fill = "kód typu přírodního stanoviště"
  ) +
  
  # 6. Final Theme Adjustments (e.g., center title)
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

hab_oop <- 
  data_hab %>%
  dplyr::left_join(
    .,
    evl %>%
      dplyr::select(
        SITECODE,
        oop
      ) %>%
      st_drop_geometry(),
    by = c("kod_chu" = "SITECODE")
  ) %>%
  tidyr::separate_rows(
    oop, 
    sep = ", "
  )

plot_habitaty_oop <- ggplot(
  hab_oop #%>%
    #dplyr::filter(grepl("rašeli", NAZEV_HABITAT) == TRUE)
    , 
  aes(
    x = fct_reorder(as.factor(oop), -kvalita, .fun = median), 
    y = kvalita, 
    #color = feature_code,
    fill = as.factor(oop)
    )
) +
  # 1. Use geom_boxplot()
  geom_boxplot(
    outlier.shape = 21,      # Use a filled circle for outliers
    outlier.size = 2,
    alpha = 0.7
    ) +            # Add slight transparency
  # 2. Flip Coordinates for Better X-axis Label Readability
  coord_flip() +
  # 3. Choose a better colour palette (Viridis is colorblind-friendly)
  scale_color_viridis_d(option = "D") +  # D is the default Viridis scale
  scale_fill_viridis_d(option = "D") +
  # 4. Use a cleaner theme
  theme_light() +
  # 5. Correct and Enhance Labels
  labs(
    x = "orgán ochrany přírody\n", # Corrected X-axis label
    y = "\nkvalita přírodního stanoviště (předměty ochrany)"
  ) +
  
  # 6. Final Theme Adjustments (e.g., center title)
  theme(
    legend.position = "blank",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

ggsave("Outputs/Grafy/plot_habitaty_oop.png", plot_habitaty_oop, width = 12, height = 8, dpi = 300)

lesy_celk <-
  data_stanoviste %>%
  dplyr::filter(skupina == 9) %>%
  dplyr::filter(grepl("kraj", oop) == TRUE | grepl("Prahy", oop) == TRUE) %>%
  dplyr::filter(parametr_nazev == 10) %>%
  dplyr::group_by(feature_code, druh, stav) %>%
  dplyr::reframe(pocet_lokalit = n()) %>%
  dplyr::mutate(
    stav = dplyr::case_when(
      stav == 11 ~ "dobrý",
      stav == 12 ~ "zhoršený",
      stav == 13 ~ "špatný",
      stav == 1 ~ "neznámý",
      stav == 8 ~ "nehodnocen" 
      )
    ) %>%
  mutate(
    stav = fct_relevel(stav, "dobrý", "zhoršený", "špatný"),
    druh = str_wrap(druh, width = 150)
  )

bezles_celk <-
  data_stanoviste %>%
  dplyr::filter(skupina == 6) %>%
  dplyr::filter(grepl("kraj", oop) == TRUE | grepl("Prahy", oop) == TRUE) %>%
  dplyr::filter(parametr_nazev == 10) %>%
  dplyr::group_by(feature_code, druh, stav) %>%
  dplyr::reframe(pocet_lokalit = n()) %>%
  dplyr::mutate(
    stav = dplyr::case_when(
      stav == 11 ~ "dobrý",
      stav == 12 ~ "zhoršený",
      stav == 13 ~ "špatný",
      stav == 1 ~ "neznámý",
      stav == 8 ~ "nehodnocen" 
    )
  ) %>%
  mutate(
    stav = fct_relevel(stav, "dobrý", "zhoršený", "špatný"),
    druh = str_wrap(druh, width = 80)
  )

barvy_stav <- c(
  "dobrý"    = "#94f204",
  "zhoršený" = "#f59b23",
  "špatný"   = "#cd011a"
)

p_hod_les <- ggplot(lesy_celk, aes(x = druh, y = pocet_lokalit, fill = stav)) +
  
  # Sloupcový graf
  geom_col(position = "stack", width = 0.8) + # Lehké zúžení pro lepší vzhled
  
  # Aplikace custom barev
  scale_fill_manual(
    values = barvy_stav,
    # Zajistíme pořadí položek v legendě odshora dolů (dobrý, zhoršený, špatný)
    breaks = c("dobrý", "zhoršený", "špatný") 
  ) +
  
  # Textové popisky
  geom_text(aes(label = pocet_lokalit),
            position = position_stack(vjust = 0.5),
            color = "black", 
            size = 3.5,
            fontface = "bold") + # Zvýraznění popisků
  coord_flip() +
  # Popisky os a tituly
  labs(
    x = "typ přírodního stanoviště",
    y = "počet lokalit",
    fill = "stav předmětu\nochrany"
  ) +
  
  # Téma a úpravy čitelnosti
  theme_minimal(base_size = 14) +
  theme(
    # Vyšší úhel rotace pro lepší čitelnost dlouhých kódů
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "blank" # Přesun legendy doprava
  )

# Zobrazení grafu
print(p_hod_les)

ggsave("Outputs/Grafy/graf_stav_lesy.png", plot = p_hod_les, width = 12, height = 5.5, dpi = 300)

p_hod_bezles <- ggplot(bezles_celk, aes(x = druh, y = pocet_lokalit, fill = stav)) +
  
  # Sloupcový graf
  geom_col(position = "stack", width = 0.8) + # Lehké zúžení pro lepší vzhled
  
  # Aplikace custom barev
  scale_fill_manual(
    values = barvy_stav,
    # Zajistíme pořadí položek v legendě odshora dolů (dobrý, zhoršený, špatný)
    breaks = c("dobrý", "zhoršený", "špatný") 
  ) +
  
  # Textové popisky
  geom_text(aes(label = pocet_lokalit),
            position = position_stack(vjust = 0.5),
            color = "black", 
            size = 3.5,
            fontface = "bold") + # Zvýraznění popisků
  coord_flip() +
  # Popisky os a tituly
  labs(
    x = "typ přírodního stanoviště",
    y = "počet lokalit",
    fill = "stav předmětu\nochrany"
  ) +
  
  # Téma a úpravy čitelnosti
  theme_minimal(base_size = 14) +
  theme(
    # Vyšší úhel rotace pro lepší čitelnost dlouhých kódů
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "blank" # Přesun legendy doprava
  )

# Zobrazení grafu
 print(p_hod_bezles)

ggsave("Outputs/Grafy/graf_stav_bezlesi.png", plot = p_hod_bezles, width = 12, height = 5.5, dpi = 300)
