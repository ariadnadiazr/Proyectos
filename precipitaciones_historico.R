# ============================================================
# 1. Cargar librerías
# ============================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(openxlsx)
# ============================================================
# 2. ANÁLISIS DE PRECIPITACIONES
# ============================================================

## 2.1 Preparación y limpieza
pre_data <- read_excel("C:/Users/diaz_/Documents/presas_ariadna/precipitaciones_historico_20250915.xlsx",
                       sheet = "precipitaciones") %>%
  mutate(Fecha_date = as.Date(as.numeric(Fecha), origin = "1899-12-30")) %>%
  select(-Fecha) %>%
  arrange(Estado, Fecha_date)

## 2.2 Línea base histórica (1985–2020)
historico_nacional <- pre_data %>%
  filter(Región == "Nacional", Año <= 2020)

linea_base_mensual <- historico_nacional %>%
  group_by(Mes_Num) %>%
  summarise(
    promedio_hist = mean(Precipitación, na.rm = TRUE),
    sd_hist       = sd(Precipitación, na.rm = TRUE),
    p10_hist      = quantile(Precipitación, 0.1, na.rm = TRUE),
    p90_hist      = quantile(Precipitación, 0.9, na.rm = TRUE)
  )

## 2.3 Anomalías recientes (2024–2025)
datos_recientes_anomalias <- pre_data %>%
  filter(Región == "Nacional", Año >= 2024) %>%
  left_join(linea_base_mensual, by = "Mes_Num") %>%
  mutate(
    anomalia_mm   = Precipitación - promedio_hist,
    z_score       = anomalia_mm / sd_hist,
    Etiqueta_Fecha = format(Fecha_date, "%b %Y")
  ) %>%
  arrange(Fecha_date)

## 2.4 Visualizaciones
# --- Comparación vs. rango histórico
grafico_comparativo <- ggplot() +
  geom_ribbon(data = linea_base_mensual,
              aes(x = Mes_Num, ymin = p10_hist, ymax = p90_hist),
              fill = "grey80", alpha = 0.6) +
  geom_line(data = linea_base_mensual,
            aes(x = Mes_Num, y = promedio_hist, group = 1),
            color = "grey40", linetype = "dashed", size = 1) +
  geom_line(data = datos_recientes_anomalias,
            aes(x = Mes_Num, y = Precipitación,
                color = factor(Año), group = factor(Año)),
            size = 1.2) +
  scale_color_manual(values = c("2024" = "darkorange", "2025" = "firebrick")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal(base_size = 14) +
  labs(
    title    = "Precipitación Nacional: 2024 y 2025 vs. Rango Histórico (1985–2020)",
    subtitle = "El área sombreada representa el 80% central (P10–P90).",
    x = "Mes", y = "Precipitación (mm)", color = "Año"
  )
print(grafico_comparativo)

# --- Anomalías estandarizadas (Z-score)
grafico_anomalias <- ggplot(datos_recientes_anomalias,
                            aes(x = factor(Etiqueta_Fecha, levels = Etiqueta_Fecha),
                                y = z_score, fill = z_score > 0)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = c(1.5, -1.5), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
                    guide = "none") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title    = "Anomalías de Precipitación Nacional (2024–2025)",
    subtitle = "Z-score respecto al promedio mensual histórico (1985–2020)",
    x = "Mes", y = "Anomalía Estandarizada"
  )
print(grafico_anomalias)


# ============================================================
# 3. ANÁLISIS DE SEQUÍA
# ============================================================

## 3.1 Preparación y categorización
seq_data <- read_excel("data_sequia.xlsx", sheet = "sequia") %>%
  mutate(
    Categoria_Sequia = case_when(
      Sequía == 0 ~ "Sin Sequía",
      Sequía <= 25 ~ "Anormalmente Seco",
      Sequía <= 50 ~ "Sequía Moderada",
      Sequía <= 75 ~ "Sequía Severa",
      Sequía > 75  ~ "Sequía Extrema",
      TRUE         ~ "Sin Datos"
    ) %>% factor(levels = c("Sin Sequía", "Anormalmente Seco",
                            "Sequía Moderada", "Sequía Severa",
                            "Sequía Extrema"))
  )

## 3.2 Visualización exploratoria (heatmap)
seq_data_0 <- seq_data %>% filter(Año >= 2020)

ggplot(seq_data_0, aes(x = Fecha, y = reorder(Estado, Sequía, FUN = mean), fill = Sequía)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("white", "#FFFF8F", "#FFC357", "#E59800", "#873D48"),
                       name = "% Afectado") +
  theme_minimal() +
  labs(
    title = "Intensidad de la Sequía por Estado (2020–2025)",
    x = "Año", y = "Estado"
  )

## 3.3 Identificación de eventos de sequía
umbral_sequia <- 50
estado_analizado <- "Nacional"

eventos_sequia <- seq_data %>%
  filter(Estado == estado_analizado) %>%
  mutate(
    en_sequia = Sequía > umbral_sequia,
    evento_id = cumsum(en_sequia != lag(en_sequia, default = FALSE))
  ) %>%
  filter(en_sequia) %>%
  group_by(evento_id) %>%
  summarise(
    fecha_inicio   = min(Fecha),
    fecha_fin      = max(Fecha),
    duracion_meses = n(),
    intensidad_pico = max(Sequía)
  )
print(eventos_sequia)

# Visualización de eventos
grafico_eventos <- ggplot(eventos_sequia,
                          aes(x = fecha_inicio, xend = fecha_fin,
                              y = reorder(factor(evento_id), fecha_inicio),
                              yend = reorder(factor(evento_id), fecha_inicio),
                              color = intensidad_pico)) +
  geom_segment(size = 4, alpha = 0.8) +
  scale_color_gradientn(colors = c("#FFC357", "#E59800", "#873D48"),
                        name = "Intensidad Pico") +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(
    title    = paste("Cronología de Eventos de Sequía Severa en", estado_analizado),
    subtitle = "Periodos continuos con >50% de superficie afectada",
    x = "Año", y = "Eventos"
  )
print(grafico_eventos)

## 3.4 Línea base histórica y anomalías recientes
linea_base_mensual_seq <- seq_data %>%
  filter(Región == "Nacional", Año <= 2020) %>%
  group_by(Mes_Num) %>%
  summarise(
    promedio_hist = mean(Sequía, na.rm = TRUE),
    sd_hist       = sd(Sequía, na.rm = TRUE),
    p10_hist      = quantile(Sequía, 0.1, na.rm = TRUE),
    p90_hist      = quantile(Sequía, 0.9, na.rm = TRUE)
  )

datos_recientes_anomalias_seq <- seq_data %>%
  filter(Región == "Nacional", Año >= 2024) %>%
  left_join(linea_base_mensual_seq, by = "Mes_Num") %>%
  mutate(
    anomalia_pp   = Sequía - promedio_hist,
    z_score       = anomalia_pp / sd_hist,
    Etiqueta_Fecha = format(Fecha, "%b %Y")
  ) %>%
  arrange(Fecha)

# --- Comparación vs. rango histórico
grafico_comparativo_seq <- ggplot() +
  geom_ribbon(data = linea_base_mensual_seq,
              aes(x = as.numeric(Mes_Num), ymin = p10_hist, ymax = p90_hist),
              fill = "#F3DAB9", alpha = 0.8) +
  geom_line(data = linea_base_mensual_seq,
            aes(x = as.numeric(Mes_Num), y = promedio_hist, group = 1),
            color = "grey40", linetype = "dashed", size = 1) +
  geom_line(data = datos_recientes_anomalias_seq,
            aes(x = as.numeric(Mes_Num), y = Sequía, color = factor(Año), group = factor(Año)),
            size = 1.2) +
  scale_color_manual(values = c("2024" = "darkorange", "2025" = "firebrick")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal(base_size = 14) +
  labs(
    title    = "Sequía Nacional: 2024 y 2025 vs. Rango Histórico (2004–2020)",
    subtitle = "El área sombreada representa el 80% central (P10–P90).",
    x = "Mes", y = "% de Superficie Afectada", color = "Año"
  )
print(grafico_comparativo_seq)

# --- Anomalías estandarizadas (Z-score)
grafico_anomalias_seq <- ggplot(datos_recientes_anomalias_seq,
                                aes(x = factor(Etiqueta_Fecha, levels = Etiqueta_Fecha),
                                    y = z_score, fill = z_score > 0)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = c(1.5, -1.5), linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue"),
                    guide = "none") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title    = "Anomalías de Sequía Nacional (2024–2025)",
    subtitle = "Z-score respecto al promedio mensual histórico (2004–2020)",
    x = "Mes", y = "Anomalía Estandarizada"
  )
print(grafico_anomalias_seq)




View(linea_base_mensual)
View(linea_base_mensual_seq)
View(datos_recientes_anomalias)
View(datos_recientes_anomalias_seq)

wb <- createWorkbook()
addWorksheet(wb, "Pre_mensual")
addWorksheet(wb, "Seq_mensual")
addWorksheet(wb, "Pre_anomalia")
addWorksheet(wb, "Seq_anomalia")
writeData(wb, sheet = "Pre_mensual", linea_base_mensual)
writeData(wb, sheet = "Seq_mensual", linea_base_mensual_seq)
writeData(wb, sheet = "Pre_anomalia", datos_recientes_anomalias)
writeData(wb, sheet = "Seq_anomalia", datos_recientes_anomalias_seq)

saveWorkbook(wb, file = "graficos_seq_pre_20250924.xlsx", overwrite = TRUE)



