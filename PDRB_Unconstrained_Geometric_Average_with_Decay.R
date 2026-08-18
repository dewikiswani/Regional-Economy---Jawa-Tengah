# ==============================================================================
# Script: Bottom-Up Multi-Scenario Total PDRB Projection (2026–2045)
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1. Load Data Historis (17 Sektor)
raw_data <- read.csv2("Bukecap_ADHK_PDRB_TimeSeries.csv", dec = ",", check.names = FALSE)
colnames(raw_data)[1] <- "sektor"

# Bersihkan dan filter baris total
df_sektor <- raw_data %>%
  filter(!grepl("Total", sektor, ignore.case = TRUE)) %>%
  pivot_longer(cols = -sektor, names_to = "tahun", values_to = "pdrb") %>%
  mutate(tahun = as.numeric(tahun))

# 2. Hitung CAGR Historis Per Sektor (2010 - 2025)
tahun_awal  <- 2010
tahun_akhir <- 2025
n_tahun     <- tahun_akhir - tahun_awal
horizon     <- 20
tahun_proj  <- (tahun_akhir + 1):(tahun_akhir + horizon)

sektor_base <- df_sektor %>%
  group_by(sektor) %>%
  summarise(
    pdrb_2010     = pdrb[tahun == tahun_awal],
    pdrb_2025     = pdrb[tahun == tahun_akhir],
    cagr_historis = (pdrb_2025 / pdrb_2010)^(1 / n_tahun) - 1,
    .groups       = "drop"
  )

# 3. Definisi Aturan Parameter Skenario per Sektor
# Masing-masing skenario memiliki penyesuaian g0, g_term, dan laju peluruhan (lambda)
skenario_list <- list(
  "Tanpa Decay (Flat CAGR)" = function(df) {
    df %>% mutate(g0 = cagr_historis, g_term = cagr_historis, lambda = 0.00)
  },
  "Optimis (Decay Rendah)" = function(df) {
    df %>% mutate(
      g0     = cagr_historis * 1.05,
      g_term = case_when(
        grepl("Pertanian|Pertambangan", sektor) ~ 0.025,
        grepl("Industri|Konstruksi", sektor)    ~ 0.035,
        TRUE                                     ~ 0.055
      ),
      lambda = case_when(
        grepl("Pertanian|Pertambangan", sektor) ~ 0.025,
        grepl("Industri|Konstruksi", sektor)    ~ 0.015,
        TRUE                                     ~ 0.010
      )
    )
  },
  "Moderat (Decay Sedang)" = function(df) {
    df %>% mutate(
      g0     = cagr_historis,
      g_term = case_when(
        grepl("Pertanian|Pertambangan", sektor) ~ 0.020,
        grepl("Industri|Konstruksi", sektor)    ~ 0.030,
        TRUE                                     ~ 0.045
      ),
      lambda = case_when(
        grepl("Pertanian|Pertambangan", sektor) ~ 0.040,
        grepl("Industri|Konstruksi", sektor)    ~ 0.025,
        TRUE                                     ~ 0.020
      )
    )
  },
  "Pesimis (Decay Cepat)" = function(df) {
    df %>% mutate(
      g0     = cagr_historis * 0.90,
      g_term = case_when(
        grepl("Pertanian|Pertambangan", sektor) ~ 0.015,
        grepl("Industri|Konstruksi", sektor)    ~ 0.025,
        TRUE                                     ~ 0.035
      ),
      lambda = case_when(
        grepl("Pertanian|Pertambangan", sektor) ~ 0.050,
        grepl("Industri|Konstruksi", sektor)    ~ 0.035,
        TRUE                                     ~ 0.030
      )
    )
  }
)

# 4. Fungsi Proyeksi Sektoral & Agregasi Menjadi Total PDRB
hitung_total_skenario <- function(nama_sken, func_sken, df_base, tahun_vec) {
  df_params <- func_sken(df_base)
  t_rel     <- seq_along(tahun_vec)
  
  # Proyeksikan setiap sektor
  df_all_sektor <- lapply(1:nrow(df_params), function(i) {
    row  <- df_params[i, ]
    g_t  <- row$g_term + (row$g0 - row$g_term) * ((1 - row$lambda)^t_rel)
    
    val  <- numeric(length(t_rel))
    cur  <- row$pdrb_2025
    for (k in seq_along(t_rel)) {
      cur    <- cur * (1 + g_t[k])
      val[k] <- cur
    }
    data.frame(tahun = tahun_vec, pdrb = val, sektor = row$sektor)
  }) %>% bind_rows()
  
  # Agregasikan (Sum) 17 Sektor per Tahun
  df_all_sektor %>%
    group_by(tahun) %>%
    summarise(pdrb = sum(pdrb), .groups = "drop") %>%
    mutate(skenario = nama_sken, tipe = "Proyeksi")
}

# Jalankan agregasi total untuk ke-4 skenario
df_total_proyeksi <- bind_rows(lapply(names(skenario_list), function(nm) {
  hitung_total_skenario(nm, skenario_list[[nm]], sektor_base, tahun_proj)
}))

# 5. Data Historis Total PDRB
df_total_historis <- df_sektor %>%
  group_by(tahun) %>%
  summarise(pdrb = sum(pdrb), .groups = "drop") %>%
  mutate(skenario = "Historis", tipe = "Historis")

# 6. Visualisasi Total PDRB Skenario (Bottom-Up) dengan ggplot2
plot_total_pdrb <- ggplot() +
  # Garis & Titik Data Historis Total
  geom_line(
    data = df_total_historis, 
    aes(x = tahun, y = pdrb), 
    color = "#1F2937", 
    linewidth = 1.2
  ) +
  geom_point(
    data = df_total_historis, 
    aes(x = tahun, y = pdrb), 
    color = "#1F2937", 
    size = 2.5
  ) +
  # Garis Skenario Proyeksi
  geom_line(
    data = df_total_proyeksi, 
    aes(x = tahun, y = pdrb, color = skenario, linetype = skenario), 
    linewidth = 1.1
  ) +
  # Ribbon / Fan Shading antara Optimis dan Pesimis
  geom_ribbon(
    data = df_total_proyeksi %>%
      filter(skenario %in% c("Optimis (Decay Rendah)", "Pesimis (Decay Cepat)")) %>%
      pivot_wider(id_cols = tahun, names_from = skenario, values_from = pdrb),
    aes(x = tahun, ymin = `Pesimis (Decay Cepat)`, ymax = `Optimis (Decay Rendah)`),
    fill = "#3B82F6", 
    alpha = 0.12
  ) +
  # Garis Batas Historis
  geom_vline(xintercept = tahun_akhir, linetype = "dashed", color = "#9CA3AF", linewidth = 0.8) +
  annotate(
    "text", x = tahun_akhir - 0.6, y = max(df_total_proyeksi$pdrb) * 0.88, 
    label = "Batas Data Historis (2025)", angle = 90, color = "#6B7280", size = 3.5
  ) +
  # Skala Sumbu X dan Y
  scale_x_continuous(
    breaks = seq(tahun_awal, max(tahun_proj), by = 5),
    minor_breaks = seq(tahun_awal, max(tahun_proj), by = 1)
  ) +
  scale_y_continuous(
    labels = label_comma(prefix = "Rp ", suffix = " T", scale = 1e-3, big.mark = ".")
  ) +
  scale_color_manual(
    values = c(
      "Tanpa Decay (Flat CAGR)" = "#DC2626",
      "Optimis (Decay Rendah)"  = "#10B981",
      "Moderat (Decay Sedang)"  = "#2563EB",
      "Pesimis (Decay Cepat)"   = "#F59E0B"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Tanpa Decay (Flat CAGR)" = "dotted",
      "Optimis (Decay Rendah)"  = "solid",
      "Moderat (Decay Sedang)"  = "solid",
      "Pesimis (Decay Cepat)"   = "dashed"
    )
  ) +
  labs(
    title = "Proyeksi Total PDRB ADHK Kabupaten (2026–2045)",
    subtitle = "Agregasi Bottom-Up dari 17 Lapangan Usaha dengan Pemodelan Skenario & Geometric Decay",
    x = "Tahun",
    y = "Total PDRB ADHK (Triliun Rupiah)",
    color = "Skenario Kebijakan",
    linetype = "Skenario Kebijakan",
    caption = "Catatan: Area arsiran biru menunjukkan rentang ketidakpastian antara skenario optimis dan pesimis."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#111827"),
    plot.subtitle    = element_text(color = "#4B5563", margin = margin(b = 12)),
    panel.grid.minor = element_line(color = "#F3F4F6"),
    panel.grid.major = element_line(color = "#E5E7EB"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    legend.background = element_rect(fill = "#F9FAFB", color = NA),
    plot.margin      = margin(15, 15, 15, 15)
  )

# Tampilkan Chart Total PDRB
print(plot_total_pdrb)

# 7. Ekspor Tabel Rangkuman 5-Tahunan untuk Dokumen Perencanaan (RPJPD)
df_tabel_rpjpd <- df_total_proyeksi %>%
  filter(tahun %in% seq(2025, 2045, by = 5)) %>%
  pivot_wider(names_from = skenario, values_from = pdrb) %>%
  mutate(across(where(is.numeric) & !c(tahun), ~ round(.x, 2)))

cat("\n--- Rangkuman Angka Total PDRB per 5 Tahun (Miliar Rupiah) ---\n")
print(df_tabel_rpjpd)