#----------------------------------------------------------#
# Nacteni dat -----
#----------------------------------------------------------#
data_raw <-
  readr::read_csv(
    "Outputs/Data/druhy/amp_evl_2024_20250410_Windows-1250.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  )

data_poc <-
  data_raw %>%
  dplyr::filter(parametr_nazev == "CELKOVE_P0") %>%
  dplyr::group_by(stav) %>%
  dplyr::reframe(pocet_lokalit = n())

data_celk <-
  data_raw %>%
  dplyr::filter(parametr_nazev == "CELKOVE_HODNOCENI") %>%
  dplyr::group_by(druh, stav) %>%
  dplyr::reframe(pocet_lokalit = n())

p_hod_n <- ggplot(data_celk, aes(x = druh, y = pocet_lokalit, fill = stav)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = barvy_stav) +
  geom_text(aes(label = pocet_lokalit),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.8) +
  labs(
    title = "Hodnocení stavu předmětů ochrany EVL - obojživelníci",
    x = "Druh",
    y = "Počet lokalit",
    fill = "Stav předmětu\nochrany"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave("Outputs/Grafy/graf_stav_obojzivelnici.png", plot = p_hod_n, width = 8, height = 5.5, dpi = 300)


# Srovnani idikatoru ----
data_comp_c <-
  data_raw %>%
  dplyr::filter(parametr_nazev == "CELKOVE_HODNOCENI") %>%
  dplyr::mutate(
    stav_num = dplyr::case_when(
      stav == "špatný" ~ 0,
      stav == "zhoršený" ~ 0.5,
      stav == "dobrý" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  tidyr::pivot_wider(
    .,
    names_from = parametr_nazev,
    values_from = stav)

data_comp_c0 <-
  data_raw %>%
  dplyr::filter(parametr_nazev == "CELKOVE_P0") %>%
  dplyr::mutate(
    stav_num = dplyr::case_when(
      stav == "špatný" ~ 0,
      stav == "zhoršený" ~ 0.5,
      stav == "dobrý" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  tidyr::pivot_wider(
    .,
    names_from = parametr_nazev,
    values_from = parametr_hodnota) %>%
  dplyr::select(
    druh,
    kod_chu,
    CELKOVE_P0
  )

data_comp_pocmin <-
  data_raw %>%
  dplyr::filter(parametr_nazev == "POCET_MIN") %>%
  dplyr::mutate(
    stav_num = dplyr::case_when(
      stav == "špatný" ~ 0,
      stav == "zhoršený" ~ 0.5,
      stav == "dobrý" ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  tidyr::pivot_wider(
    .,
    names_from = parametr_nazev,
    values_from = parametr_hodnota) %>%
  dplyr::select(
    druh,
    kod_chu,
    POCET_MIN
  )

data_comp_spec <- 
  left_join(
    data_comp_c,
    data_comp_c0,
    by = c(
      "druh",
      "kod_chu")
    ) %>%
  left_join(
    .,
    data_comp_pocmin,
    by = c(
      "druh",
      "kod_chu")
  )
  

cols_status <- c(
  "0" = "#d73027",   # špatný
  "0.5" = "#fee08b", # zhoršený
  "1" = "#1a9850"    # dobrý
)

p <- ggplot(data = data_comp_spec, 
            aes(x = (CELKOVE_P0), 
                y = CELKOVE_HODNOCENI, 
                fill = druh)) +
  geom_violin() +
  #geom_boxplot(width = 0.15, outlier.shape = NA, color = "grey20", alpha = 0.3) +
  scale_fill_manual(
    name = "Výchozí stav (P0)",
    values = cols_status,
    labels = c("0" = "špatný", "0.5" = "zhoršený", "1" = "dobrý")
  ) +
  scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("špatný", "zhoršený", "dobrý"),
    limits = c(-0.05, 1.05)
  ) +
  labs(
    x = "Druh",
    y = "Celkové hodnocení (aktuální stav)",
    title = "Porovnání celkového hodnocení druhů v CHÚ",
    subtitle = "Barevně vyznačen výchozí stav (P0)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey30"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )
p
# --- Uložení ----
data_comp_spec2 <- data_comp_spec %>%
  # CELKOVE_P0 by měl být numerický (procenta, např. 46.15)
  mutate(
    CELKOVE_P0 = as.numeric(CELKOVE_P0),
    # pokud máš textový CELKOVE_HODNOCENI, zajistíme správné pořadí kategorií
    CELKOVE_HODNOCENI = factor(CELKOVE_HODNOCENI,
                               levels = c("špatný", "zhoršený", "dobrý"),
                               ordered = TRUE)
  ) %>%
  # drop rows bez hodnot
  filter(!is.na(CELKOVE_P0) & !is.na(CELKOVE_HODNOCENI))

# ---------- Barevná paleta (tvá) ----------
cols_status <- c(
  "špatný"   = "#d73027",   # odpovídá 0
  "zhoršený" = "#fee08b",   # 0.5
  "dobrý"    = "#1a9850"    # 1
)

# ---------- VIZUALIZACE: horizontální violiny + boxplot + body ----------
p <- ggplot(data_comp_spec2,
            aes(x = CELKOVE_P0, y = CELKOVE_HODNOCENI, fill = CELKOVE_HODNOCENI)) +
  # Violin z distribuce CELKOVE_P0 pro každou kategorie CELKOVE_HODNOCENI
  geom_violin(trim = FALSE, scale = "width", color = "grey30", linewidth = 0.3, alpha = 0.9) +
  # Boxplot uvnitř violiny pro kvartily
  geom_boxplot(width = 0.12, outlier.shape = NA, color = "grey10", alpha = 0.6) +
  # Jednotlivé CHÚ jako jitter body (malé, černé okraje, průhledné)
  geom_jitter(aes(color = CELKOVE_HODNOCENI), # nebo color = druh pokud chceš barvit podle druhu
              height = 0.12, width = 0, size = 1.8, alpha = 0.6, stroke = 0.2, shape = 21) +
  # Medián jako velký tečkovaný bod
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, color = "black", fill = "white") +
  # Barvy dle stavu (využijeme tvou paletu)
  scale_fill_manual(values = cols_status, guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = cols_status, guide = "none") + # používané pro obrys bodů, skryj legendu
  # Osa x jako procenta (předpokládáme hodnoty 0-100)
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10),
                     labels = function(x) paste0(x, " %")) +
  labs(
    x = "% lokalit v dobrém stavu (CELKOVE_P0)",
    y = "Celkové hodnocení (CELKOVE_HODNOCENI)",
    title = "Jak % lokalit v dobrém stavu souvisí s celkovým hodnocením",
    subtitle = "Violiny = rozložení % lokalit; body = jednotlivé CHÚ; bílý bod = medián"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  )

# vykresli
print(p)

# barvy po druhu ----
data_comp_spec2 <- data_comp_spec %>%
  mutate(
    CELKOVE_P0 = as.numeric(CELKOVE_P0),
    CELKOVE_HODNOCENI = factor(CELKOVE_HODNOCENI,
                               levels = c("špatný", "zhoršený", "dobrý"),
                               ordered = TRUE)
  ) %>%
  filter(!is.na(CELKOVE_P0) & !is.na(CELKOVE_HODNOCENI))

cols_status <- c(
  "špatný"   = "#d73027",
  "zhoršený" = "#fee08b",
  "dobrý"    = "#1a9850"
)

# --- Barvy pro druhy (volitelně: lze ručně definovat podle počtu druhů) ---
n_species <- length(unique(data_comp_spec2$druh))
cols_species <- scales::hue_pal()(n_species)

p_species <- ggplot(data_comp_spec2,
                    aes(x = CELKOVE_P0, y = CELKOVE_HODNOCENI, fill = CELKOVE_HODNOCENI)) +
  geom_violin(trim = FALSE, scale = "width", color = "grey30", linewidth = 0.3, alpha = 0.9) +
  geom_boxplot(width = 0.12, outlier.shape = NA, color = "grey10", alpha = 0.6) +
  geom_jitter(aes(color = druh),
              height = 0.12, width = 0, size = 1.8, alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, color = "black", fill = "white") +
  scale_fill_manual(values = cols_status, guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(values = cols_species) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    labels = function(x) paste0(x, " %")
  ) +
  labs(
    x = "% lokalit v dobrém stavu (CELKOVE_P0)",
    y = "Celkové hodnocení (CELKOVE_HODNOCENI)",
    title = "Vztah mezi % lokalit v dobrém stavu a celkovým hodnocením",
    subtitle = "Body rozlišují druhy; violiny a boxploty znázorňují rozložení hodnot"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  )

print(p_species)

ggsave("Outputs/Grafy/celkove_vs_p0_violin_druh.png", p_species, width = 9, height = 5, dpi = 300)

# rozdeleni pro druhu ----
# datová příprava (ujisti se, že data_comp_spec existuje)
data_comp_spec_plot <- data_comp_spec %>%
  mutate(
    # zachováme CELKOVE_P0 jako numeric 0-100
    CELKOVE_P0 = as.numeric(CELKOVE_P0),
    # zajistíme správné úrovně kategorií
    CELKOVE_HODNOCENI = factor(CELKOVE_HODNOCENI,
                               levels = c("špatný", "zhoršený", "dobrý"),
                               ordered = TRUE)
  ) %>%
  filter(!is.na(CELKOVE_P0) & !is.na(CELKOVE_HODNOCENI))

# paleta pro kategorie stavů
cols_status <- c(
  "špatný"   = "#d73027",
  "zhoršený" = "#fee08b",
  "dobrý"    = "#1a9850"
)

# FACETOVANÁ VERZE: v každém panelu tři violiny (pro 3 kategorie hodnocení)
p_faceted_fixed <- ggplot(data_comp_spec_plot,
                          aes(x = CELKOVE_HODNOCENI, y = CELKOVE_P0, fill = CELKOVE_HODNOCENI)) +
  geom_violin(trim = FALSE, scale = "width", color = "grey30", linewidth = 0.3, alpha = 0.9) +
  geom_boxplot(width = 0.12, outlier.shape = NA, color = "grey10", alpha = 0.6, position = position_dodge(width = 0.9)) +
  # jitter body (jednotlivé CHÚ) - menší, černý okraj, poloprůhledné
  geom_jitter(aes(shape = NULL), width = 0.15, height = 0, size = 1.6, alpha = 0.7, color = "grey20") +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, color = "black", fill = "white",
               position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = cols_status, guide = guide_legend(reverse = FALSE)) +
  # y osa 0-100 s procentními popisky
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10),
                     labels = function(x) paste0(x, " %")) +
  facet_wrap(~ druh, ncol = 3, scales = "fixed") +
  labs(
    x = "Celkové hodnocení (aktuální stav)",
    y = "% lokalit v dobrém stavu (P0)",
    title = "Vztah aktuálního celkového hodnocení k % dílčích lokalit v dobrém stavu (P0)",
    subtitle = "Každý panel = jeden druh; tři violiny v panelu = špatný / zhoršený / dobrý"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# vykreslíme a uložíme
print(p_faceted_fixed)
dir.create("Outputs", showWarnings = FALSE)
ggsave("Outputs/Grafy/celkove_vs_p0_facet_druh_fixed.png", p_faceted_fixed, width = 12, height = 8, dpi = 300)

# podle minimalni pocetnosti ----

# příprava dat
data_comp_spec_plot3 <- data_comp_spec %>%
  mutate(
    CELKOVE_HODNOCENI = factor(CELKOVE_HODNOCENI,
                               levels = c("špatný", "zhoršený", "dobrý"),
                               ordered = TRUE)
  ) %>%
  filter(!is.na(POCET_MIN), !is.na(CELKOVE_HODNOCENI))

cols_status <- c(
  "špatný"   = "#d73027",
  "zhoršený" = "#fee08b",
  "dobrý"    = "#1a9850"
)

# violin plot – rozložení počtu jedinců podle hodnocení, rozdělené po druzích
p_violin_pocet <- ggplot(data_comp_spec_plot3,
                         aes(x = CELKOVE_HODNOCENI, y = POCET_MIN, fill = CELKOVE_HODNOCENI)) +
  geom_violin(trim = FALSE, scale = "width", color = "grey20", linewidth = 0.3, alpha = 0.9) +
  geom_boxplot(width = 0.12, outlier.shape = NA, color = "grey10", alpha = 0.4) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3,
               color = "black", fill = "white", stroke = 0.6) +
  scale_fill_manual(values = cols_status, name = "Aktuální stav") +
  # osa y – volitelně logaritmická (pokud jsou velké rozdíly v počtech)
  scale_y_continuous(
    labels = label_number(big.mark = " "),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  # pokud je rozsah velký, přepni na log10:
  # scale_y_log10(labels = scales::label_number(big.mark = " ")) +
  facet_wrap(~ druh, scales = "free_y", ncol = 3) +
  labs(
    x = "Celkové hodnocení (aktuální stav)",
    y = "Minimální počet jedinců (POCET_MIN)",
    title = "Rozložení počtu jedinců podle celkového hodnocení druhů",
    subtitle = "Každý panel představuje jeden druh; barvy označují aktuální stav"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 11),
    strip.background = element_rect(fill = "grey95", color = NA),
    strip.text = element_text(face = "bold"),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p_violin_pocet)

# uložit do souboru
dir.create("Outputs", showWarnings = FALSE)
ggsave("Outputs/celkove_vs_pocetmin_violin_druh.png", p_violin_pocet, width = 12, height = 8, dpi = 300)

