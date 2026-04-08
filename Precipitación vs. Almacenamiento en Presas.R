# ============================================================
# 1. Cargar librerías
# ============================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(viridis)

# ============================================================
# 2. Cargar y agregar datos de PRECIPITACIÓN
# ============================================================

# Cargar el archivo de precipitaciones
pre_data <- read_excel("C:/Users/diaz_/Documents/presas_ariadna/precipitaciones_historico_20250915.xlsx",
                       sheet = "precipitaciones")

# Filtrar y agregar datos de precipitación (Junio, Julio, Agosto 2025)
precip_summary_jja <- pre_data %>%
  filter(Año == 2025, Mes_Num %in% c(6, 7, 8)) %>%
  group_by(Estado) %>%
  summarise(Precipitacion_Promedio_JJA = mean(Precipitación, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(Estado != "Nacional") # Excluimos el agregado nacional

print("--- Resumen de Precipitación (JJA 2025) ---")
print(head(precip_summary_jja))

# ============================================================
# 3. Cargar y agregar datos de PRESAS (COMPLETO)
# ============================================================

# --- 3.1 Definir archivos y hojas ---
file_junio <- "data_presas_06.xlsx"
file_julio <- "data_presas_07.xlsx"
file_agosto <- "data_presas_08.xlsx"
base_path_presas <- "C:/Users/diaz_/Documents/presas_ariadna/"
# ¡¡Asegúrate que este nombre de hoja sea correcto!!
sheet_name_presas <- "Sheet 1" 

# --- 3.2 Cargar y combinar---
# (Esto crea 'presas_junio', 'presas_julio', 'presas_agosto')
presas_junio  <- read_excel(paste0(base_path_presas, file_junio),  sheet = sheet_name_presas)
presas_julio  <- read_excel(paste0(base_path_presas, file_julio),  sheet = sheet_name_presas)
presas_agosto <- read_excel(paste0(base_path_presas, file_agosto), sheet = sheet_name_presas)
presas_data_raw <- bind_rows(presas_junio, presas_julio, presas_agosto)


# --- 3.3 Limpiar y agregar datos de presas ---
presas_summary_jja <- presas_data_raw %>%
  
  # Renombrar columnas
  rename(
    Estado = "Entidad federativa",
    Almacenamiento = "NAME Almacenamiento (hm³)" 
  ) %>%
  
  # Filtramos por el NÚMERO de mes, no por el texto
  filter(Año == 2025, Mes %in% c(6, 7, 8)) %>%
  
  # Agrupar y resumir
  group_by(Estado, Fecha) %>%
  summarise(Almacenamiento_Estatal_Diario = sum(Almacenamiento, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Estado) %>%
  summarise(Almacenamiento_Promedio_JJA = mean(Almacenamiento_Estatal_Diario, na.rm = TRUE)) %>%
  ungroup()

print("--- Resumen de Presas (JJA 2025) ---")
print(head(presas_summary_jja)) # <- ¡Ya no debe estar vacío!


# ============================================================
# 3.4 Diagnóstico de Nombres de Estado
# ============================================================
print("--- Estados en PRECIPITACIÓN (archivo 1) ---")
print(sort(unique(precip_summary_jja$Estado)))

print("--- Estados en PRESAS (archivos 2) ---")
print(sort(unique(presas_summary_jja$Estado)))

# !! COMPARA LAS DOS LISTAS DE ARRIBA !!
# Si no son idénticas (ej. "Estado de México" vs "México"),
# el gráfico saldrá en blanco.


# ============================================================
# 4. Unir datos y crear gráfico
# ============================================================

bubble_data <- inner_join(precip_summary_jja, 
                          presas_summary_jja, 
                          by = "Estado")

# Imprime los datos unidos para verificar (opcional)
print("--- Datos combinados (bubble_data) ---")
print(head(bubble_data))


# --- Generar el Gráfico de Burbuja---
bubble_chart <- ggplot(bubble_data, aes(x = Precipitacion_Promedio_JJA, y = Almacenamiento_Promedio_JJA)) +
  
  geom_point(aes(size = Almacenamiento_Promedio_JJA, color = Estado), alpha = 0.7) +
  
  geom_text_repel(aes(label = Estado), size = 3.5, max.overlaps = 20) +
  
  scale_size_continuous(range = c(4, 18), name = "Almacenamiento Promedio (hm³)", guide = "none") + 
  
  scale_color_viridis_d(guide = "none") +  
  
  theme_minimal(base_size = 14) +
  labs(
    title = "Precipitación vs. Almacenamiento en Presas (Junio - Agosto 2025)",
    subtitle = "Promedio por Estado. El tamaño de la burbuja representa el almacenamiento.",
    x = "Precipitación Promedio (mm)",
    y = "Almacenamiento Promedio (hm³)"
  )

# Imprimir el gráfico
print(bubble_chart)

