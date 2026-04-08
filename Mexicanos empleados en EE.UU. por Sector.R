library(tidyverse)
library(seasonal)
library(lubridate)
library(scales)

# --- 1. CARGA Y PREPARACIÓN DE DATOS ---
df <- read_csv("census_sectores_2003_2025 (1).csv")

# Nombres
nombres_cols <- c(
  "Año", "Mes", 
  "Educación y Salud", "Manufactura", "Agro y Pesca", "Comercio",
  "Información", "Construcción", "Serv. Profesionales", "Fuerzas Armadas",
  "Finanzas", "Admin. Pública", "Turismo y Ocio", "Transporte y Serv.",
  "Minería", "Otros Servicios"
)
colnames(df) <- nombres_cols

datos_largos <- df %>%
  mutate(
    Mes_Num = match(tolower(Mes), c("jan", "feb", "mar", "apr", "may", "jun",
                                    "jul", "aug", "sep", "oct", "nov", "dec")),
    Fecha = make_date(year = Año, month = Mes_Num, day = 1)
  ) %>%
  pivot_longer(cols = 3:16, names_to = "Sector", values_to = "Empleo_Original")

# --- 2. FUNCIÓN DE CÁLCULO ROBUSTO (X-13 + Loess + Original) ---
calcular_tendencia_robusta <- function(df_sector) {
  df_sector <- df_sector %>% arrange(Fecha)
  tendencia <- rep(NA, nrow(df_sector))
  
  # Si son ceros, devolver original
  if(sum(df_sector$Empleo_Original, na.rm=TRUE) == 0) {
    df_sector$Empleo_Trend <- df_sector$Empleo_Original
    return(df_sector)
  }

  # Intento 1: X-13
  try({
    ts_data <- ts(df_sector$Empleo_Original, start = c(2003, 1), frequency = 12)
    modelo <- seas(ts_data)
    tendencia <- as.numeric(trend(modelo))
  }, silent = TRUE)
  
  # Intento 2: Loess
  if(all(is.na(tendencia))) {
    try({
      modelo_loess <- loess(Empleo_Original ~ as.numeric(Fecha), data = df_sector, span = 0.5)
      tendencia <- predict(modelo_loess)
    }, silent = TRUE)
  }
  
  # Intento 3: Original (Backup final)
  if(all(is.na(tendencia))) {
    tendencia <- df_sector$Empleo_Original
  }
  
  df_sector$Empleo_Trend <- tendencia
  return(df_sector)
}

# --- 3. PROCESAMIENTO ---
print("Calculando tendencias...")
datos_finales <- datos_largos %>%
  group_by(Sector) %>%
  nest() %>%
  mutate(data = map(data, calcular_tendencia_robusta)) %>%
  unnest(data)

# --- 4. LA GRÁFICA COMBINADA ---
ggplot(datos_finales, aes(x = Fecha)) +
  
  # 1. La línea GRIS (del segundo código)
  geom_line(aes(y = Empleo_Original), color = "grey85", linewidth = 0.5) +
  
  # 2. La línea AZUL (estilo del primer código: #2c3e50)
  geom_line(aes(y = Empleo_Trend), color = "#2c3e50", linewidth = 0.7) + 
  
  # Facetas
  facet_wrap(~ Sector, scales = "free_y", ncol = 4) + 
  
  # Escala abreviada (k/M)
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  
  # TEMA Y ESTILOS (Aquí recuperamos los tamaños pequeños del primero)
  theme_minimal() +
  labs(
    title = "Mexicanos empleados en EE.UU. por Sector",
    subtitle = "Tendencias 2003-2025: Original (Gris) vs Suavizada (Azul)", 
    x = "Año",
    y = "Personas Empleadas",
    caption = "Fuente: Census Bureau / Elaboración propia"
  ) +
  theme(
    # Títulos de las cajitas (estilo del primero)
    strip.text = element_text(face = "bold", size = 9, color = "#2c3e50"),
    
    # Eje X pequeño (estilo del primero)
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    
    # Eje Y pequeño (estilo del primero)
    axis.text.y = element_text(size = 7),
    
    # Título principal
    plot.title = element_text(face = "bold", size = 14)
  )
