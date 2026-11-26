# SABR solo con base exotica que trae volatilidad implícita

# ============================================================
# 0) Librerías
# ============================================================
library(readxl)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(ggplot2)


# ============================================================
# 1) Leer archivo
# ============================================================
# CORRECCIÓN (oct-2026): la base se lee directamente del reporte crudo
# OPTOS_EXOTIC_202412.csv. La hoja "OTC_exotic" del Excel
# base_filtrada_FX_europeas_CorP.xlsx se había construido fuera del código
# y conservaba solo posicion == "E" (entrega). Ahora se incluyen ambas
# posiciones (A = adquiere, E = entrega) y todos los filtros quedan en código.
library(readr)

source("R/00_configuracion.R")  # rutas relativas del proyecto

otc_exotic <- read_csv(
  file.path(ruta_bases, "OPTOS_EXOTIC_202412.csv"),
  col_types = cols(.default = col_character()),   # a_num/a_fecha convierten
  locale    = locale(encoding = "latin1")          # el archivo trae acentos/Ñ
) %>%
  clean_names()

names(otc_exotic)
count(otc_exotic, posicion)



# ============================================================
# 2) Funciones auxiliares
# ============================================================

a_num <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\$", "", x)
  x <- gsub("%", "", x)
  x <- gsub(" ", "", x)
  
  # Caso 1,234.56
  x <- ifelse(grepl(",.*\\.", x), gsub(",", "", x), x)
  
  # Caso 1.234,56
  x <- ifelse(grepl("\\..*,", x), gsub("\\.", "", x), x)
  x <- ifelse(grepl(",", x) & !grepl("\\.", x), gsub(",", ".", x), x)
  
  suppressWarnings(as.numeric(x))
}

a_fecha <- function(x) {
  x <- as.character(x)
  
  y1 <- suppressWarnings(lubridate::ymd(x))
  y2 <- suppressWarnings(lubridate::dmy(x))
  y3 <- suppressWarnings(lubridate::mdy(x))
  
  dplyr::coalesce(y1, y2, y3)
}

normaliza_texto <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper()
}


# ============================================================
# 3) Estandarizar campos relevantes de OTC_exotic
# ============================================================

otc_exotic_std <- otc_exotic %>%
  mutate(
    fe_corte   = a_fecha(fe_corte),
    fe_con_ope = a_fecha(fe_con_ope),
    fe_ven_ope = a_fecha(fe_ven_ope),
    fe_liq_ope = a_fecha(fe_liq_ope),
    
    precioejer = a_num(precioejer),
    pri_act    = a_num(pri_act),
    vol_implic = a_num(vol_implic),
    vol        = a_num(vol),
    delta_opc  = a_num(delta_opc),
    delta      = a_num(delta),
    
    tip_opc    = normaliza_texto(tip_opc),
    opc_ejer   = normaliza_texto(opc_ejer),
    cve_tit    = normaliza_texto(cve_tit),
    clase_suby = normaliza_texto(clase_suby),
    mdasuby    = normaliza_texto(mdasuby),
    mdaliquida = normaliza_texto(mdaliquida),
    mod_pre    = normaliza_texto(mod_pre)
  )

# ============================================================
# 4) Diagnóstico general de OTC_exotic
# ============================================================

diagnostico_base_exotic <- function(df) {
  df %>%
    summarise(
      n = n(),
      
      n_call = sum(tip_opc == "C", na.rm = TRUE),
      n_put  = sum(tip_opc == "V", na.rm = TRUE),
      n_nobarr = sum(mod_pre == "N", na.rm = TRUE),
      n_europeas = sum(opc_ejer == "E", na.rm = TRUE),
      n_divisas = sum(clase_suby == "DIVISAS", na.rm = TRUE),
      n_mdasuby_840 = sum(mdasuby == "840", na.rm = TRUE),
      n_mdaliquida_484 = sum(mdaliquida == "484", na.rm = TRUE),
      n_fx_usdmxn_por_monedas = sum(mdasuby == "840" & mdaliquida == "484", na.rm = TRUE),
      
      na_fe_corte = sum(is.na(fe_corte)),
      na_fe_ven_ope = sum(is.na(fe_ven_ope)),
      na_precioejer = sum(is.na(precioejer)),
      na_pri_act = sum(is.na(pri_act)),
      na_vol_implic = sum(is.na(vol_implic)),
      
      strike_min = min(precioejer, na.rm = TRUE),
      strike_mediana = median(precioejer, na.rm = TRUE),
      strike_max = max(precioejer, na.rm = TRUE),
      
      vol_implic_min = min(vol_implic, na.rm = TRUE),
      vol_implic_mediana = median(vol_implic, na.rm = TRUE),
      vol_implic_max = max(vol_implic, na.rm = TRUE),
      
      pri_act_min = min(pri_act, na.rm = TRUE),
      pri_act_mediana = median(pri_act, na.rm = TRUE),
      pri_act_max = max(pri_act, na.rm = TRUE)
    )
}

diagnostico_base_exotic(otc_exotic_std)

# ============================================================
# 5) Base SABR Ejercicio 1
# ============================================================

base_sabr_1 <- otc_exotic_std %>%
  filter(
    # (oct-2026) Se conservan ambas posiciones (A y E); no se filtra posicion.
    tip_opc %in% c("C", "V"),
    # (nov-2026) Se excluyen opciones con barrera (DI, DO, UI, UO, IN):
    # su valor depende de la trayectoria del subyacente, por lo que su
    # volatilidad implícita no es comparable con la de opciones europeas estándar.
    mod_pre == "N",
    opc_ejer == "E",
    clase_suby == "DIVISAS",
    mdasuby == "840",
    mdaliquida == "484",
    !is.na(precioejer),
    precioejer > 0,
    !is.na(vol_implic),
    vol_implic > 0,
    !is.na(fe_corte),
    !is.na(fe_ven_ope)
  ) %>%
  mutate(
    tipo_opcion = tip_opc,
    strike = precioejer,
    prima_actualizada = pri_act,
    dias_a_venc = as.numeric(fe_ven_ope - fe_corte),
    ttm_anios = dias_a_venc / 365,
    iv_obs = vol_implic / 100,
    bucket_venc = case_when(
      dias_a_venc <= 30  ~ "0-30d",
      dias_a_venc <= 90  ~ "31-90d",
      dias_a_venc <= 180 ~ "91-180d",
      dias_a_venc <= 365 ~ "181-365d",
      TRUE ~ ">365d"
    )
  ) %>%
  filter(
    dias_a_venc > 0,
    ttm_anios > 0,
    iv_obs > 0
  )

# ==================================================
# EDA base optos exotic
# ==================================================
# Creamos los objetos para ver un resumen de la base
# Resumen general
resumen_sabr_1 <- base_sabr_1 %>%
  summarise(
    n = n(),
    
    fecha_corte_min = min(fe_corte, na.rm = TRUE),
    fecha_corte_max = max(fe_corte, na.rm = TRUE),
    
    strike_min = min(strike, na.rm = TRUE),
    strike_q01 = quantile(strike, 0.01, na.rm = TRUE),
    strike_mediana = median(strike, na.rm = TRUE),
    strike_q99 = quantile(strike, 0.99, na.rm = TRUE),
    strike_max = max(strike, na.rm = TRUE),
    
    dias_min = min(dias_a_venc, na.rm = TRUE),
    dias_mediana = median(dias_a_venc, na.rm = TRUE),
    dias_max = max(dias_a_venc, na.rm = TRUE),
    
    ttm_min = min(ttm_anios, na.rm = TRUE),
    ttm_mediana = median(ttm_anios, na.rm = TRUE),
    ttm_max = max(ttm_anios, na.rm = TRUE),
    
    iv_min = min(iv_obs, na.rm = TRUE),
    iv_q01 = quantile(iv_obs, 0.01, na.rm = TRUE),
    iv_mediana = median(iv_obs, na.rm = TRUE),
    iv_q99 = quantile(iv_obs, 0.99, na.rm = TRUE),
    iv_max = max(iv_obs, na.rm = TRUE),
    
    prima_min = min(prima_actualizada, na.rm = TRUE),
    prima_mediana = median(prima_actualizada, na.rm = TRUE),
    prima_max = max(prima_actualizada, na.rm = TRUE)
  )

resumen_sabr_1

# Resumen por bucket
resumen_bucket_sabr_1 <- base_sabr_1 %>%
  group_by(bucket_venc) %>%
  summarise(
    n = n(),
    n_strikes = n_distinct(strike),
    n_call = sum(tipo_opcion == "C", na.rm = TRUE),
    n_put = sum(tipo_opcion == "V", na.rm = TRUE),
    
    dias_min = min(dias_a_venc, na.rm = TRUE),
    dias_mediana = median(dias_a_venc, na.rm = TRUE),
    dias_max = max(dias_a_venc, na.rm = TRUE),
    
    ttm_mediana = median(ttm_anios, na.rm = TRUE),
    
    strike_min = min(strike, na.rm = TRUE),
    strike_mediana = median(strike, na.rm = TRUE),
    strike_max = max(strike, na.rm = TRUE),
    
    iv_min = min(iv_obs, na.rm = TRUE),
    iv_q01 = quantile(iv_obs, 0.01, na.rm = TRUE),
    iv_mediana = median(iv_obs, na.rm = TRUE),
    iv_q99 = quantile(iv_obs, 0.99, na.rm = TRUE),
    iv_max = max(iv_obs, na.rm = TRUE),
    
    prima_mediana = median(prima_actualizada, na.rm = TRUE),
    prima_max = max(prima_actualizada, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(match(bucket_venc, c("0-30d", "31-90d", "91-180d", "181-365d", ">365d")))

resumen_bucket_sabr_1

# Resumen diagnostico NA
diagnostico_na_sabr_1 <- base_sabr_1 %>%
  summarise(
    n = n(),
    na_strike = sum(is.na(strike)),
    na_iv_obs = sum(is.na(iv_obs)),
    na_prima_actualizada = sum(is.na(prima_actualizada)),
    na_dias_a_venc = sum(is.na(dias_a_venc)),
    
    n_iv_mayor_100 = sum(iv_obs > 1, na.rm = TRUE),
    n_iv_mayor_50 = sum(iv_obs > 0.5, na.rm = TRUE),
    n_iv_menor_1pct = sum(iv_obs < 0.01, na.rm = TRUE),
    
    n_prima_cero = sum(prima_actualizada == 0, na.rm = TRUE),
    n_prima_negativa = sum(prima_actualizada < 0, na.rm = TRUE)
  )

diagnostico_na_sabr_1



# Revisar detalles de corto plazo
resumen_bucket_sabr_1 %>%
  select(
    bucket_venc,
    n,
    n_strikes,
    iv_min,
    iv_q01,
    iv_mediana,
    iv_q99,
    iv_max
  )

# ==================================
# Base final por decisión experta
# ==================================

# Me quedo con las operaciones de corto plazo
# Elimino volatilidades menores a 2% dado que seguro son errores de información
base_sabr_1_calib <- base_sabr_1 %>%
  filter(
    bucket_venc != ">365d",
    iv_obs >= 0.02
  )

# Reconciliación de la muestra (tesina, sección de volatilidad por vencimiento)
# Esperado (ambas posiciones, sin barreras): 10,095 -> 255 por vencimiento > 365d
#           -> 9,840 -> 40 con IV reportada de 1% (7 | 20 | 5 | 8) -> 9,800
# (Con barreras: 11,208 -> 10,933 -> 10,888. Solo posicion "E": 5,092 -> 4,929.)
reconciliacion_muestra <- base_sabr_1 %>%
  mutate(
    motivo = case_when(
      bucket_venc == ">365d" ~ "Excluida: vencimiento > 365 días",
      iv_obs < 0.02          ~ "Excluida: IV reportada < 2%",
      TRUE                   ~ "Muestra de calibración"
    )
  ) %>%
  count(bucket_venc, motivo) %>%
  tidyr::pivot_wider(names_from = motivo, values_from = n, values_fill = 0) %>%
  arrange(match(bucket_venc, c("0-30d", "31-90d", "91-180d", "181-365d", ">365d")))

reconciliacion_muestra
colSums(reconciliacion_muestra[, -1])


# Grafica para ver como queda la base final iv_obs vs strike
ggplot(base_sabr_1_calib, aes(x = strike, y = iv_obs)) +
  geom_point(alpha = 0.35) +
  facet_wrap(~ bucket_venc, scales = "free") +
  labs(
    title = "Volatilidad implícita observada vs strike",
    subtitle = "Ejercicio 1: OTC_exotic, base limpia para SABR clásico",
    x = "Strike",
    y = "Volatilidad implícita observada"
  ) +
  theme_minimal()

# Double check Tabla
base_sabr_1_calib %>%
  group_by(bucket_venc) %>%
  summarise(
    n = n(),
    n_strikes = n_distinct(strike),
    iv_min = min(iv_obs, na.rm = TRUE),
    iv_mediana = median(iv_obs, na.rm = TRUE),
    iv_max = max(iv_obs, na.rm = TRUE),
    .groups = "drop"
  )



# SABR MODEL -------------------------------------------------------------------

# ============================================================
# 13) Parámetros iniciales
# ============================================================

# CORRECCIÓN (nov-2026): FIX determinado por Banxico el 31/12/2024.
# El valor anterior (20.5103) era el publicado en el DOF el 31/12/2024,
# que corresponde al FIX determinado el día hábil previo (30/12/2024).
S0 <- 20.7862
beta_fijo <- 1  # SABR lognormal inicial

# ============================================================
# 14) Volatilidad implícita SABR - fórmula de Hagan
# ============================================================

sabr_vol_hagan <- function(F, K, T, alpha, beta, rho, nu) {
  
  # Protección numérica
  eps <- 1e-12
  
  F <- pmax(F, eps)
  K <- pmax(K, eps)
  T <- pmax(T, eps)
  
  one_minus_beta <- 1 - beta
  
  # Caso ATM: F aproximadamente igual a K
  is_atm <- abs(F - K) < 1e-08
  
  vol <- rep(NA_real_, length(K))
  
  # -----------------------------
  # Fórmula ATM
  # -----------------------------
  if (any(is_atm)) {
    F_atm <- F[is_atm]
    T_atm <- T[is_atm]
    
    FK_beta_atm <- F_atm^(one_minus_beta)
    
    term_atm <- (
      ((one_minus_beta)^2 / 24) * (alpha^2 / (F_atm^(2 * one_minus_beta))) +
        (rho * beta * nu * alpha / 4) / (F_atm^one_minus_beta) +
        ((2 - 3 * rho^2) / 24) * nu^2
    ) * T_atm
    
    vol[is_atm] <- (alpha / FK_beta_atm) * (1 + term_atm)
  }
  
  # -----------------------------
  # Fórmula no ATM
  # -----------------------------
  if (any(!is_atm)) {
    F_n <- F[!is_atm]
    K_n <- K[!is_atm]
    T_n <- T[!is_atm]
    
    logFK <- log(F_n / K_n)
    FK <- F_n * K_n
    FK_beta <- FK^((1 - beta) / 2)
    
    z <- (nu / alpha) * FK_beta * logFK
    
    x_z <- log(
      (sqrt(1 - 2 * rho * z + z^2) + z - rho) /
        (1 - rho)
    )
    
    # Protección si z/x(z) se vuelve inestable cerca de cero
    z_over_x <- ifelse(abs(z) < 1e-08, 1, z / x_z)
    
    denominator <- FK_beta * (
      1 +
        ((one_minus_beta)^2 / 24) * logFK^2 +
        ((one_minus_beta)^4 / 1920) * logFK^4
    )
    
    correction <- 1 + (
      ((one_minus_beta)^2 / 24) * (alpha^2 / (FK^(one_minus_beta))) +
        (rho * beta * nu * alpha / 4) / FK_beta +
        ((2 - 3 * rho^2) / 24) * nu^2
    ) * T_n
    
    vol[!is_atm] <- (alpha / denominator) * z_over_x * correction
  }
  
  return(vol)
}


# ============================================================
# 15) Función objetivo para calibración SABR
# ============================================================

objetivo_sabr <- function(par, data, beta = 1, S0 = 20.7862) {
  
  alpha <- par[1]
  rho   <- par[2]
  nu    <- par[3]
  
  # Penalización por parámetros inválidos
  if (alpha <= 0 || nu <= 0 || rho <= -0.999 || rho >= 0.999) {
    return(1e10)
  }
  
  F_vec <- rep(S0, nrow(data))  # aproximación inicial F ≈ S0
  
  iv_model <- sabr_vol_hagan(
    F = F_vec,
    K = data$strike,
    T = data$ttm_anios,
    alpha = alpha,
    beta = beta,
    rho = rho,
    nu = nu
  )
  
  if (any(!is.finite(iv_model))) {
    return(1e10)
  }
  
  error <- data$iv_obs - iv_model
  
  mean(error^2, na.rm = TRUE)
}


# ============================================================
# 16) Calibración SABR por bucket
# ============================================================

calibra_sabr_bucket <- function(data_bucket, beta = 1, S0 = 20.7862) {
  
  # Valores iniciales razonables
  par_ini <- c(
    alpha = median(data_bucket$iv_obs, na.rm = TRUE),
    rho   = 0.0,
    nu    = 0.5
  )
  
  fit <- optim(
    par = par_ini,
    fn = objetivo_sabr,
    data = data_bucket,
    beta = beta,
    S0 = S0,
    method = "L-BFGS-B",
    lower = c(0.0001, -0.999, 0.0001),
    upper = c(2.0000,  0.999, 5.0000),
    control = list(maxit = 5000)
  )
  
  alpha_hat <- fit$par[1]
  rho_hat   <- fit$par[2]
  nu_hat    <- fit$par[3]
  
  F_vec <- rep(S0, nrow(data_bucket))
  
  iv_fit <- sabr_vol_hagan(
    F = F_vec,
    K = data_bucket$strike,
    T = data_bucket$ttm_anios,
    alpha = alpha_hat,
    beta = beta,
    rho = rho_hat,
    nu = nu_hat
  )
  
  rmse <- sqrt(mean((data_bucket$iv_obs - iv_fit)^2, na.rm = TRUE))
  mae  <- mean(abs(data_bucket$iv_obs - iv_fit), na.rm = TRUE)
  
  tibble(
    bucket_venc = unique(data_bucket$bucket_venc),
    n = nrow(data_bucket),
    n_strikes = n_distinct(data_bucket$strike),
    beta = beta,
    alpha = alpha_hat,
    rho = rho_hat,
    nu = nu_hat,
    rmse = rmse,
    mae = mae,
    convergencia = fit$convergence,
    valor_objetivo = fit$value
  )
}


# ============================================================
# 17) Ejecutar calibración SABR clásica
# ============================================================

resultados_sabr_1 <- base_sabr_1_calib %>%
  filter(bucket_venc != ">365d") %>%
  group_by(bucket_venc) %>%
  group_split() %>%
  lapply(calibra_sabr_bucket, beta = beta_fijo, S0 = S0) %>%
  bind_rows() %>%
  arrange(match(bucket_venc, c("0-30d", "31-90d", "91-180d", "181-365d")))

resultados_sabr_1


# Limpiamos el nombre de las variables

resultados_sabr_1_limpio <- resultados_sabr_1 %>%
  rename(
    alpha_sabr = alpha,
    rho_sabr = rho,
    nu_sabr = nu,
    beta_sabr = beta,
    rmse_calib = rmse,
    mae_calib = mae
  )

resultados_sabr_1_limpio


# ============================================================
# 18) Agregar IV SABR ajustada a la base
# ============================================================

base_sabr_1_fit <- base_sabr_1_calib %>%
  filter(bucket_venc != ">365d") %>%
  left_join(resultados_sabr_1_limpio, by = "bucket_venc") %>%
  mutate(
    F_aprox = S0,
    iv_sabr = sabr_vol_hagan(
      F = F_aprox,
      K = strike,
      T = ttm_anios,
      alpha = alpha_sabr,
      beta = beta_sabr,
      rho = rho_sabr,
      nu = nu_sabr
    ),
    error_sabr = iv_obs - iv_sabr,
    error_abs_sabr = abs(error_sabr)
  )


# ============================================================
# 19) Resumen de errores SABR
# ============================================================

errores_sabr_1 <- base_sabr_1_fit %>%
  group_by(bucket_venc) %>%
  summarise(
    n = n(),
    rmse = sqrt(mean(error_sabr^2, na.rm = TRUE)),
    mae = mean(abs(error_sabr), na.rm = TRUE),
    error_mediano = median(error_sabr, na.rm = TRUE),
    error_abs_q95 = quantile(abs(error_sabr), 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(bucket_venc, c("0-30d", "31-90d", "91-180d", "181-365d")))

errores_sabr_1


# ============================================================
# 20) Gráfica IV observada vs IV SABR
# ============================================================

ggplot(base_sabr_1_fit, aes(x = strike)) +
  geom_point(aes(y = iv_obs), alpha = 0.25) +
  geom_point(aes(y = iv_sabr), alpha = 0.25) +
  facet_wrap(~ bucket_venc, scales = "free") +
  labs(
    title = "Volatilidad implícita observada vs SABR clásico",
    subtitle = "Ejercicio 1: OTC_exotic, beta = 1, F aproximado por spot",
    x = "Strike",
    y = "Volatilidad implícita"
  ) +
  theme_minimal()


# ============================================================
# 21) Curva SABR suavizada por bucket
# ============================================================

curva_sabr_1 <- base_sabr_1_fit %>%
  group_by(bucket_venc) %>%
  summarise(
    strike_min = quantile(strike, 0.01, na.rm = TRUE),
    strike_max = quantile(strike, 0.99, na.rm = TRUE),
    T_rep = median(ttm_anios, na.rm = TRUE),
    alpha_sabr = first(alpha_sabr),
    beta_sabr = first(beta_sabr),
    rho_sabr = first(rho_sabr),
    nu_sabr = first(nu_sabr),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    bucket <- .$bucket_venc
    K_grid <- seq(.$strike_min, .$strike_max, length.out = 150)
    
    tibble(
      bucket_venc = bucket,
      strike = K_grid,
      T_rep = .$T_rep,
      alpha_sabr = .$alpha_sabr,
      beta_sabr = .$beta_sabr,
      rho_sabr = .$rho_sabr,
      nu_sabr = .$nu_sabr,
      iv_sabr = sabr_vol_hagan(
        F = rep(S0, length(K_grid)),
        K = K_grid,
        T = rep(.$T_rep, length(K_grid)),
        alpha = .$alpha_sabr,
        beta = .$beta_sabr,
        rho = .$rho_sabr,
        nu = .$nu_sabr
      )
    )
  }) %>%
  ungroup()

# Gráfica por buckets

ggplot() +
  geom_point(
    data = base_sabr_1_fit,
    aes(x = strike, y = iv_obs),
    alpha = 0.20
  ) +
  geom_line(
    data = curva_sabr_1,
    aes(x = strike, y = iv_sabr),
    linewidth = 1
  ) +
  facet_wrap(~ bucket_venc, scales = "free") +
  labs(
    title = "Ajuste SABR clásico sobre volatilidad implícita observada",
    subtitle = "Ejercicio 1: OTC_exotic, beta = 1, F aproximado por spot",
    x = "Strike",
    y = "Volatilidad implícita"
  ) +
  theme_minimal()


# ============================================================
# 23) Calibración SABR por bucket con límites configurables
# ============================================================
# Restringir rho para mejorar el bucket 181-365d


calibra_sabr_bucket_rho_restringido <- function(
    data_bucket,
    beta = 1,
    S0 = 20.7862,
    rho_lower = -0.95,
    rho_upper = 0.95
) {
  
  par_ini <- c(
    alpha = median(data_bucket$iv_obs, na.rm = TRUE),
    rho   = 0.0,
    nu    = 0.5
  )
  
  fit <- optim(
    par = par_ini,
    fn = objetivo_sabr,
    data = data_bucket,
    beta = beta,
    S0 = S0,
    method = "L-BFGS-B",
    lower = c(0.0001, rho_lower, 0.0001),
    upper = c(2.0000, rho_upper, 5.0000),
    control = list(maxit = 5000)
  )
  
  alpha_hat <- fit$par[1]
  rho_hat   <- fit$par[2]
  nu_hat    <- fit$par[3]
  
  F_vec <- rep(S0, nrow(data_bucket))
  
  iv_fit <- sabr_vol_hagan(
    F = F_vec,
    K = data_bucket$strike,
    T = data_bucket$ttm_anios,
    alpha = alpha_hat,
    beta = beta,
    rho = rho_hat,
    nu = nu_hat
  )
  
  rmse <- sqrt(mean((data_bucket$iv_obs - iv_fit)^2, na.rm = TRUE))
  mae  <- mean(abs(data_bucket$iv_obs - iv_fit), na.rm = TRUE)
  
  tibble(
    bucket_venc = unique(data_bucket$bucket_venc),
    n = nrow(data_bucket),
    n_strikes = n_distinct(data_bucket$strike),
    beta = beta,
    alpha = alpha_hat,
    rho = rho_hat,
    nu = nu_hat,
    rmse = rmse,
    mae = mae,
    convergencia = fit$convergence,
    valor_objetivo = fit$value,
    rho_lower = rho_lower,
    rho_upper = rho_upper
  )
}

# ============================================================
# 24) Ejecutar SABR con rho restringido [-0.95, 0.95]
# ============================================================

resultados_sabr_1_rho95 <- base_sabr_1_calib %>%
  filter(bucket_venc != ">365d") %>%
  group_by(bucket_venc) %>%
  group_split() %>%
  lapply(
    calibra_sabr_bucket_rho_restringido,
    beta = beta_fijo,
    S0 = S0,
    rho_lower = -0.95,
    rho_upper = 0.95
  ) %>%
  bind_rows() %>%
  arrange(match(bucket_venc, c("0-30d", "31-90d", "91-180d", "181-365d")))

resultados_sabr_1_rho95

# ============================================================
# 25) Comparación: SABR original vs rho restringido
# ============================================================

comparacion_sabr_rho <- resultados_sabr_1 %>%
  select(
    bucket_venc,
    alpha_original = alpha,
    rho_original = rho,
    nu_original = nu,
    rmse_original = rmse,
    mae_original = mae
  ) %>%
  left_join(
    resultados_sabr_1_rho95 %>%
      select(
        bucket_venc,
        alpha_rho95 = alpha,
        rho_rho95 = rho,
        nu_rho95 = nu,
        rmse_rho95 = rmse,
        mae_rho95 = mae
      ),
    by = "bucket_venc"
  ) %>%
  mutate(
    dif_rmse = rmse_rho95 - rmse_original,
    dif_rmse_pp = dif_rmse * 100,
    dif_mae = mae_rho95 - mae_original,
    dif_mae_pp = dif_mae * 100
  )

comparacion_sabr_rho


# ============================================================
# 26) Base fit con rho restringido
# ============================================================

resultados_sabr_1_rho95_limpio <- resultados_sabr_1_rho95 %>%
  rename(
    alpha_sabr = alpha,
    rho_sabr = rho,
    nu_sabr = nu,
    beta_sabr = beta,
    rmse_calib = rmse,
    mae_calib = mae
  )

base_sabr_1_fit_rho95 <- base_sabr_1_calib %>%
  filter(bucket_venc != ">365d") %>%
  left_join(resultados_sabr_1_rho95_limpio, by = "bucket_venc") %>%
  mutate(
    F_aprox = S0,
    iv_sabr = sabr_vol_hagan(
      F = F_aprox,
      K = strike,
      T = ttm_anios,
      alpha = alpha_sabr,
      beta = beta_sabr,
      rho = rho_sabr,
      nu = nu_sabr
    ),
    error_sabr = iv_obs - iv_sabr,
    error_abs_sabr = abs(error_sabr)
  )

# ============================================================
# 27) Errores con rho restringido
# ============================================================

errores_sabr_1_rho95 <- base_sabr_1_fit_rho95 %>%
  group_by(bucket_venc) %>%
  summarise(
    n = n(),
    rmse = sqrt(mean(error_sabr^2, na.rm = TRUE)),
    mae = mean(abs(error_sabr), na.rm = TRUE),
    error_mediano = median(error_sabr, na.rm = TRUE),
    error_abs_q95 = quantile(abs(error_sabr), 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(bucket_venc, c("0-30d", "31-90d", "91-180d", "181-365d")))

errores_sabr_1_rho95

# ============================================================
# 28) Curva SABR suavizada con rho restringido
# ============================================================

curva_sabr_1_rho95 <- base_sabr_1_fit_rho95 %>%
  group_by(bucket_venc) %>%
  summarise(
    strike_min = quantile(strike, 0.01, na.rm = TRUE),
    strike_max = quantile(strike, 0.99, na.rm = TRUE),
    T_rep = median(ttm_anios, na.rm = TRUE),
    alpha_sabr = first(alpha_sabr),
    beta_sabr = first(beta_sabr),
    rho_sabr = first(rho_sabr),
    nu_sabr = first(nu_sabr),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    bucket <- .$bucket_venc
    K_grid <- seq(.$strike_min, .$strike_max, length.out = 150)
    
    tibble(
      bucket_venc = bucket,
      strike = K_grid,
      T_rep = .$T_rep,
      alpha_sabr = .$alpha_sabr,
      beta_sabr = .$beta_sabr,
      rho_sabr = .$rho_sabr,
      nu_sabr = .$nu_sabr,
      iv_sabr = sabr_vol_hagan(
        F = rep(S0, length(K_grid)),
        K = K_grid,
        T = rep(.$T_rep, length(K_grid)),
        alpha = .$alpha_sabr,
        beta = .$beta_sabr,
        rho = .$rho_sabr,
        nu = .$nu_sabr
      )
    )
  }) %>%
  ungroup()


# ============================================================
# 29) Gráfica ajuste SABR con rho restringido
# ============================================================

ggplot() +
  geom_point(
    data = base_sabr_1_fit_rho95,
    aes(x = strike, y = iv_obs),
    alpha = 0.20
  ) +
  geom_line(
    data = curva_sabr_1_rho95,
    aes(x = strike, y = iv_sabr),
    linewidth = 1
  ) +
  facet_wrap(~ bucket_venc, scales = "free") +
  labs(
    title = "Ajuste SABR clásico con rho restringido",
    subtitle = "Ejercicio 1: OTC_exotic, beta = 1, rho en [-0.95, 0.95], F aproximado por spot",
    x = "Strike",
    y = "Volatilidad implícita"
  ) +
  theme_minimal()


# ============================================
# RESULTADOS FINALES ETAPA 1
# ============================================

resultados_sabr_1_base <- resultados_sabr_1_rho95
base_sabr_1_fit_base <- base_sabr_1_fit_rho95
errores_sabr_1_base <- errores_sabr_1_rho95
curva_sabr_1_base <- curva_sabr_1_rho95

# Especificación base SABR clásico:
# beta = 1
# rho restringido a [-0.95, 0.95]
# F aproximado inicialmente por spot S0
# Pendiente forward pendiente de incorporar con curvas MXN/USD

