#----------------------------------------------------------#
# Nacteni dat -----
#----------------------------------------------------------#
data_val_raw <-
  readr::read_csv(
    "Data/Input/Revisions/export_isop_obojzivelnici_202510.csv", 
    locale = readr::locale(encoding = "Windows-1250")
  )

data_val_n <-
  data_val_raw %>%
  dplyr::filter(RP != "RP Jižní Morava" & RP != "RP Střední Čechy") %>%
  dplyr::group_by(validace) %>%
  dplyr::reframe(pocet_lokalit = n())

data_val_rp_n <-
  data_val_raw %>%
  dplyr::filter(RP != "RP Jižní Morava" & RP != "RP Střední Čechy") %>%
  dplyr::group_by(RP, validace) %>%
  dplyr::reframe(pocet_lokalit = n())

data_val_hod_n <-
  data_val_raw %>%
  dplyr::filter(RP != "RP Jižní Morava" & RP != "RP Střední Čechy") %>%
  dplyr::filter(validace == "validováno") %>%
  dplyr::group_by(druh, stav) %>%
  dplyr::reframe(pocet_lokalit = n())

# Zobrazeni dat ----
barvy_stav <- c(
  "zhoršený" = "#f59b23",
  "špatný"   = "#cd011a",
  "dobrý"    = "#94f204"
)

p_val_n <- ggplot(data_val_n, aes(x = validace, y = pocet_lokalit, fill = validace)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = pocet_lokalit), vjust = -0.3, size = 4.2) +
  scale_fill_manual(values = c("#5DA5DA", "#FAA43A", "#60BD68", "#94f204")) +
  labs(
    title = "Počet lokalit podle stavu validace",
    x = "Validace",
    y = "Počet lokalit"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

p_val_rp_n <- ggplot(data_val_rp_n, aes(x = RP, y = pocet_lokalit, fill = validace)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = pocet_lokalit),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3.8) +
  scale_fill_manual(values = c("#5DA5DA", "#FAA43A", "#60BD68", "#94f204")) +
  labs(
    title = "Počet lokalit podle validace a regionálního pracoviště",
    x = "Regionální pracoviště",
    y = "Počet lokalit",
    fill = "Validace"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

p_val_hod_n <- ggplot(data_val_hod_n, aes(x = druh, y = pocet_lokalit, fill = stav)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = barvy_stav) +
  geom_text(aes(label = pocet_lokalit),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.8) +
  labs(
    title = "Počet lokalit s validovaným hodnocením podle druhu",
    x = "Druh",
    y = "Počet lokalit",
    fill = "Stav hodnocení"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("Outputs/Grafy/Validace/graf_validace_pocet.png", plot = p_val_n, width = 7, height = 5, dpi = 300)
ggsave("Outputs/Grafy/Validace/graf_validace_RP.png", plot = p_val_rp_n, width = 8, height = 5.5, dpi = 300)
ggsave("Outputs/Grafy/Validace/graf_stav_druh.png", plot = p_val_hod_n, width = 8, height = 5.5, dpi = 300)
