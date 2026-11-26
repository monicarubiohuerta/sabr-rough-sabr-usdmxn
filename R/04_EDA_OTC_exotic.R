# ============================================================
# EDA ACTUALIZADO - EJERCICIO 1: OTC_EXOTIC
# Calls y puts europeas USD/MXN
# Requiere haber corrido 01_preparacion_muestra_SABR_spot.R (objeto base_sabr_1)
# ============================================================

library(tidyverse)
library(lubridate)
library(viridis)
library(scales)

source("R/00_configuracion.R")  # rutas relativas del proyecto

# Carpeta de salida de gráficas
ruta_graficas_EDA <- file.path(ruta_graficas, "EDA_OTC_exotic")
dir.create(ruta_graficas_EDA, recursive = TRUE, showWarnings = FALSE)

# Orden común de los buckets
niveles_bucket <- c("0-30d", "31-90d", "91-180d", "181-365d", ">365d")

# ============================================================
# 1) Construcción de la muestra del Ejercicio 1
# ============================================================
# Se usa sólo OTC_exotic y se filtran:
# - calls y puts (C, V)
# - ejercicio europeo
# - subyacente DIVISAS
# - USD como moneda del subyacente (840)
# - MXN como moneda de liquidación (484)
# - strike positivo
# - plazo remanente positivo
# - volatilidad implícita válida, para que el EDA coincida con
#   la muestra utilizada posteriormente en SABR

base_eda_exotic <- base_sabr_1 %>%
  mutate(
    bucket_vencimiento = recode(
      bucket_venc,
      "0-30d" = "0-30 días",
      "31-90d" = "31-90 días",
      "91-180d" = "91-180 días",
      "181-365d" = "181-365 días",
      ">365d" = ">365 días"
    ),
    
    bucket_vencimiento = factor(
      bucket_vencimiento,
      levels = c(
        "0-30 días",
        "31-90 días",
        "91-180 días",
        "181-365 días",
        ">365 días"
      )
    ),
    
    tipo_opcion_desc = recode(
      tipo_opcion,
      "C" = "Call",
      "V" = "Put"
    )
  )

# ============================================================
# 2) Diagnóstico y resumen
# ============================================================

resumen_eda_exotic <- base_eda_exotic %>%
  summarise(
    n_operaciones = n(),
    n_calls = sum(tipo_opcion == "C"),
    n_puts = sum(tipo_opcion == "V"),
    n_strikes = n_distinct(strike),
    strike_min = min(strike),
    strike_mediana = median(strike),
    strike_max = max(strike),
    dias_min = min(dias_a_venc),
    dias_mediana = median(dias_a_venc),
    dias_max = max(dias_a_venc),
    prima_min = min(prima_actualizada, na.rm = TRUE),
    prima_mediana = median(prima_actualizada, na.rm = TRUE),
    prima_max = max(prima_actualizada, na.rm = TRUE)
  )

resumen_bucket_eda <- base_eda_exotic %>%
  count(bucket_venc, name = "n") %>%
  mutate(porcentaje = n / sum(n))

print(resumen_eda_exotic, width = Inf)
print(resumen_bucket_eda)

# ============================================================
# 3) Gráfica: operaciones por bucket de vencimiento
# Objeto solicitado: bucket_vencimiento_vanillas
# ============================================================

bucket_vencimiento_vanillas <- ggplot(
  base_eda_exotic,
  aes(
    x = bucket_vencimiento,
    fill = bucket_vencimiento
  )
) +
  geom_bar(
    width = 0.75,
    show.legend = FALSE
  ) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.4,
    size = 3.8
  ) +
  scale_fill_viridis_d(
    option = "C",
    begin = 0.15,
    end = 0.85
  ) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Distribución de operaciones por plazo a vencimiento",
    x = "Plazo a vencimiento",
    y = "Número de operaciones"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )


bucket_vencimiento_vanillas



# ============================================================
# 4) Gráfica: prima actualizada por bucket
# Objeto solicitado: prima_act_vanillas
# Se usa escala logarítmica por la fuerte asimetría de la prima.
# ============================================================

prima_act_vanillas <- ggplot(
  base_eda_exotic,
  aes(
    x = bucket_vencimiento,
    y = prima_actualizada,
    fill = bucket_vencimiento
  )
) +
  geom_boxplot(
    alpha = 0.80,
    outlier.alpha = 0.18,
    show.legend = FALSE
  ) +
  scale_fill_viridis_d(
    option = "C",
    begin = 0.15,
    end = 0.85
  ) +
  scale_y_log10(
    labels = label_number(
      big.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Distribución de la prima actualizada por vencimiento",
    subtitle = "Escala logarítmica",
    x = "Plazo a vencimiento",
    y = "Prima actualizada"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    plot.title = element_text(face = "bold")
  )


prima_act_vanillas

# ============================================================
# 5) Gráfica: densidad del strike
# Objeto solicitado: Strike_density
# ============================================================

Strike_density <- ggplot(
  base_eda_exotic,
  aes(x = strike)
) +
  geom_density(
    fill = "#75D5D0",
    color = "#087F8C",
    alpha = 0.45,
    linewidth = 1
  ) +
  geom_vline(
    xintercept = median(
      base_eda_exotic$strike,
      na.rm = TRUE
    ),
    color = "#C44536",
    linetype = "dashed",
    linewidth = 0.9
  ) +
  annotate(
    "text",
    x = median(
      base_eda_exotic$strike,
      na.rm = TRUE
    ),
    y = Inf,
    label = "Mediana",
    color = "#C44536",
    angle = 90,
    vjust = 1.5,
    hjust = 1.1,
    size = 3.5
  ) +
  labs(
    title = "Distribución de los precios de ejercicio",
    x = "Strike USD/MXN",
    y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

Strike_density

# ============================================================
# 6) Gráfica: strike vs tiempo al vencimiento
# Objeto solicitado: strike_TtM
# La visualización principal se restringe a TTM <= 1 año,
# consistente con la calibración SABR principal.
# ============================================================

strike_TtM <- base_eda_exotic %>%
  filter(ttm_anios <= 1) %>%
  ggplot(aes(x = ttm_anios, y = strike)) +
  geom_point(alpha = 0.25, size = 1.2) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.50, 0.75, 1.00),
    labels = number_format(accuracy = 0.01)
  ) +
  labs(
    title = "Strike y tiempo al vencimiento",
    subtitle = "Muestra principal con vencimiento menor o igual a un año",
    x = "Tiempo al vencimiento (años)",
    y = "Strike (MXN por USD)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title.position = "plot")




# CORRECCIÓN (oct-2026): el color representa la volatilidad implícita
# (antes repetía el plazo, ya mostrado en el eje x). Se excluyen los 45
# registros con IV reportada de 1% para no comprimir la escala de color;
# con ello la figura coincide con la muestra de calibración (10,888).
strike_TtM <- ggplot(
  base_eda_exotic %>%
    filter(ttm_anios <= 1, iv_obs >= 0.02) %>%
    # Mejora para que no se encime el color fuerte en los puntos claros
    arrange(iv_obs),
  aes(
    x = ttm_anios,
    y = strike,
    color = iv_obs
  )
) +
  geom_point(
    alpha = 0.55,
    size = 1.4
  ) +
  scale_color_viridis_c(
    option = "D",
    begin = 0.10,
    end = 0.90,
    labels = label_number(accuracy = 0.01),   # decimales, igual que Figs. 4 y 5
    name = "Volatilidad\nimplícita\n(decimal)"
  ) +
  labs(
    title = "Strike y tiempo a vencimiento",
    subtitle = "Opciones con vencimiento de hasta un año",
    x = "Tiempo a vencimiento en años",
    y = "Strike USD/MXN"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

strike_TtM

# ============================================================
# 7) Guardar gráficas en PNG y PDF
# ============================================================

guarda_grafica <- function(grafica, nombre, ancho = 9, alto = 6) {
  ggsave(
    filename = file.path(ruta_graficas_EDA, paste0(nombre, ".png")),
    plot = grafica,
    width = ancho,
    height = alto,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  ggsave(
    filename = file.path(ruta_graficas_EDA, paste0(nombre, ".pdf")),
    plot = grafica,
    width = ancho,
    height = alto,
    units = "in",
    device = cairo_pdf
  )
}

guarda_grafica(bucket_vencimiento_vanillas, "bucket_vencimiento_vanillas", 9, 6)
guarda_grafica(prima_act_vanillas, "prima_act_vanillas", 9, 6)
guarda_grafica(Strike_density, "Strike_density", 9, 6)
guarda_grafica(strike_TtM, "strike_TtM", 9, 6)

# Guardar la base EDA filtrada y tablas resumen
write_csv(base_eda_exotic, file.path(ruta_salidas, "base_EDA_OTC_exotic_ejercicio1.csv"))
write_csv(resumen_eda_exotic, file.path(ruta_salidas, "resumen_EDA_OTC_exotic.csv"))
write_csv(resumen_bucket_eda, file.path(ruta_salidas, "resumen_buckets_OTC_exotic.csv"))
