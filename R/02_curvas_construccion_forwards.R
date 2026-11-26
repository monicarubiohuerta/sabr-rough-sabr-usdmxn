# Lectura de curvas y construcción de tass forward y discount factors

# ============================================================
# 30) Lectura de curvas OIS MXN y USD
# ============================================================

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)   # dmy() para la fecha de la curva USD

source("R/00_configuracion.R")  # rutas relativas del proyecto

archivo_mxn <- file.path(
  ruta_curvas,
  "OISTIIE_20241231.csv"
)

archivo_usd <- file.path(
  ruta_curvas,
  "OIS_USD_20241231.csv"
)

if (!file.exists(archivo_mxn)) {
  stop("No se encontró la curva MXN: ", archivo_mxn)
}

if (!file.exists(archivo_usd)) {
  stop("No se encontró la curva USD: ", archivo_usd)
}

curva_mxn_raw <- read_csv(
  archivo_mxn,
  show_col_types = FALSE
)

curva_usd_raw <- read_csv(
  archivo_usd,
  show_col_types = FALSE
)

names(curva_mxn_raw)
names(curva_usd_raw)


# ============================================================
# 31) Homologar curvas
# ============================================================

fecha_valuacion <- as.Date("2024-12-31")

curva_mxn <- curva_mxn_raw %>%
  transmute(
    fecha = as.Date(fecha),
    plazo_dias = as.integer(plazo_dias),
    tasa_mxn_pct = as.numeric(tasa_oistIIE),
    tasa_mxn = tasa_mxn_pct / 100
  ) %>%
  filter(
    fecha == fecha_valuacion,
    plazo_dias >= 1,
    !is.na(tasa_mxn)
  ) %>%
  distinct(plazo_dias, .keep_all = TRUE) %>%
  arrange(plazo_dias)

curva_usd <- curva_usd_raw %>%
  transmute(
    fecha = dmy(fecha),
    plazo_dias = as.integer(plazo_dias),
    tasa_usd_pct = as.numeric(tasa_ois),
    tasa_usd = tasa_usd_pct / 100
  ) %>%
  filter(
    fecha == fecha_valuacion,
    plazo_dias >= 1,
    !is.na(tasa_usd)
  ) %>%
  distinct(plazo_dias, .keep_all = TRUE) %>%
  arrange(plazo_dias)



# ============================================================
# 32) Validar cobertura de ambas curvas
# ============================================================

diagnostico_curvas <- bind_rows(
  curva_mxn %>%
    summarise(
      curva = "MXN OIS TIIE Fondeo",
      n = n(),
      plazo_min = min(plazo_dias),
      plazo_max = max(plazo_dias),
      n_plazos = n_distinct(plazo_dias),
      n_na = sum(is.na(tasa_mxn)),
      tasa_min_pct = min(tasa_mxn_pct),
      tasa_mediana_pct = median(tasa_mxn_pct),
      tasa_max_pct = max(tasa_mxn_pct)
    ),
  
  curva_usd %>%
    summarise(
      curva = "USD OIS",
      n = n(),
      plazo_min = min(plazo_dias),
      plazo_max = max(plazo_dias),
      n_plazos = n_distinct(plazo_dias),
      n_na = sum(is.na(tasa_usd)),
      tasa_min_pct = min(tasa_usd_pct),
      tasa_mediana_pct = median(tasa_usd_pct),
      tasa_max_pct = max(tasa_usd_pct)
    )
)

diagnostico_curvas

# Acotmos las curvas 365 
# ============================================================
# Curvas de corto plazo: 1 a 365 días
# ============================================================

curva_mxn_1y <- curva_mxn %>%
  filter(plazo_dias <= 365) %>%
  select(
    plazo_dias,
    tasa_mxn_pct,
    tasa_mxn
  )

curva_usd_1y <- curva_usd %>%
  filter(plazo_dias <= 365) %>%
  select(
    plazo_dias,
    tasa_usd_pct,
    tasa_usd
  )

# Validar cobertura diaria completa
stopifnot(nrow(curva_mxn_1y) == 365)
stopifnot(nrow(curva_usd_1y) == 365)

stopifnot(
  identical(
    curva_mxn_1y$plazo_dias,
    curva_usd_1y$plazo_dias
  )
)

# ========================================
# FUNCIÓN DE DESCUENTO
# ========================================
calcula_df <- function(
    tasa,
    plazo_dias,
    base_dias = 360,
    convencion = c("continua", "simple", "anual")
) {
  
  convencion <- match.arg(convencion)
  T <- plazo_dias / base_dias
  
  switch(
    convencion,
    continua = exp(-tasa * T),
    simple   = 1 / (1 + tasa * T),
    anual    = 1 / ((1 + tasa)^T)
  )
}



# ============================================================
# Construcción de DF y forward USD/MXN
# ============================================================

# Tipo de cambio FIX determinado por Banxico el 31/12/2024 (SIE).
# CORRECCIÓN (nov-2026): antes 20.5103, que es el tipo de cambio publicado
# en el DOF el 31/12/2024 (FIX determinado el 30/12/2024).
S0 <- 20.7862
base_dias_curvas <- 360
convencion_df <- "continua"
# Nota (oct-2026): con convención "simple" el forward a 1 año baja de
# 22.05 a 21.96 (diferencia máx. de 0.4% en log-moneyness) y los
# parámetros calibrados prácticamente no cambian. Se conserva "continua"
# y se reporta como supuesto operativo en Limitaciones.

curvas_fx <- curva_mxn_1y %>%
  inner_join(
    curva_usd_1y,
    by = "plazo_dias"
  ) %>%
  mutate(
    T_curva = plazo_dias / base_dias_curvas,
    
    df_mxn = calcula_df(
      tasa = tasa_mxn,
      plazo_dias = plazo_dias,
      base_dias = base_dias_curvas,
      convencion = convencion_df
    ),
    
    df_usd = calcula_df(
      tasa = tasa_usd,
      plazo_dias = plazo_dias,
      base_dias = base_dias_curvas,
      convencion = convencion_df
    ),
    
    forward_usdmxn =
      S0 * df_usd / df_mxn,
    
    puntos_forward =
      forward_usdmxn - S0
  ) %>%
  arrange(plazo_dias)


# Validación
validacion_curvas_fx <- curvas_fx %>%
  summarise(
    n = n(),
    plazo_min = min(plazo_dias),
    plazo_max = max(plazo_dias),
    
    n_na = sum(
      is.na(df_mxn) |
        is.na(df_usd) |
        is.na(forward_usdmxn)
    ),
    
    n_df_mxn_fuera_rango =
      sum(df_mxn <= 0 | df_mxn > 1),
    
    n_df_usd_fuera_rango =
      sum(df_usd <= 0 | df_usd > 1),
    
    n_forward_menor_spot =
      sum(forward_usdmxn < S0),
    
    tasa_mxn_min = min(tasa_mxn),
    tasa_mxn_max = max(tasa_mxn),
    
    tasa_usd_min = min(tasa_usd),
    tasa_usd_max = max(tasa_usd),
    
    df_mxn_min = min(df_mxn),
    df_mxn_max = max(df_mxn),
    
    df_usd_min = min(df_usd),
    df_usd_max = max(df_usd),
    
    forward_min = min(forward_usdmxn),
    forward_mediana = median(forward_usdmxn),
    forward_max = max(forward_usdmxn)
  )

validacion_curvas_fx

# Nodos representativos 

curvas_fx %>%
  filter(
    plazo_dias %in%
      c(1, 7, 30, 60, 90, 180, 270, 360, 365)
  ) %>%
  select(
    plazo_dias,
    tasa_mxn_pct,
    tasa_usd_pct,
    df_mxn,
    df_usd,
    forward_usdmxn,
    puntos_forward
  )