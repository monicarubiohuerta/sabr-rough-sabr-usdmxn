# Para este script se debió haber corrido 01_preparacion_muestra_SABR_spot.R y 02_curvas_construccion_forwards.R

# ============================================================
# 39) Incorporar forward individual a cada operación
# ============================================================

base_sabr_1_forward <- base_sabr_1_calib %>%
  filter(
    bucket_venc != ">365d",
    dias_a_venc >= 1,
    dias_a_venc <= 365
  ) %>%
  left_join(
    curvas_fx %>%
      select(
        plazo_dias,
        tasa_mxn,
        tasa_usd,
        df_mxn,
        df_usd,
        forward_usdmxn,
        puntos_forward
      ),
    by = c("dias_a_venc" = "plazo_dias")
  ) %>%
  mutate(
    moneyness_forward = strike / forward_usdmxn,
    log_moneyness = log(strike / forward_usdmxn)
  )


# Validamos
base_sabr_1_forward %>%
  summarise(
    n = n(),
    na_forward = sum(is.na(forward_usdmxn)),
    forward_min = min(forward_usdmxn, na.rm = TRUE),
    forward_mediana = median(forward_usdmxn, na.rm = TRUE),
    forward_max = max(forward_usdmxn, na.rm = TRUE),
    moneyness_min = min(moneyness_forward, na.rm = TRUE),
    moneyness_mediana = median(moneyness_forward, na.rm = TRUE),
    moneyness_max = max(moneyness_forward, na.rm = TRUE)
  )


# ============================================================
# 40) Función objetivo SABR con forward individual
# ============================================================

# objetivo_sabr_forward <- function(par, data, beta = 1) {
#   
#   alpha <- par[1]
#   rho   <- par[2]
#   nu    <- par[3]
#   
#   if (
#     alpha <= 0 ||
#     nu <= 0 ||
#     # limite a rho para ser consistente con el ejercicio SABR_1 
#     # (conveniente porque no cambia las salidas de los smile y es )
#     rho <= -0.95 ||
#     rho >= 0.95
#   ) {
#     return(1e10)
#   }
#   
#   iv_model <- sabr_vol_hagan(
#     F = data$forward_usdmxn,
#     K = data$strike,
#     T = data$ttm_anios,
#     alpha = alpha,
#     beta = beta,
#     rho = rho,
#     nu = nu
#   )
#   
#   if (any(!is.finite(iv_model))) {
#     return(1e10)
#   }
#   
#   error <- data$iv_obs - iv_model
#   
#   mean(error^2, na.rm = TRUE)
# }



# Corregimos el código pasado para controlar los límites de penalización de rho
# Restringimos optim() entre .95 y .95

objetivo_sabr_forward <- function(par, data, beta = 1) {
  
  alpha <- par[1]
  rho   <- par[2]
  nu    <- par[3]
  
  # Sólo se validan parámetros estrictamente positivos
  if (alpha <= 0 || nu <= 0) {
    return(1e10)
  }
  
  iv_model <- sabr_vol_hagan(
    F = data$forward_usdmxn,
    K = data$strike,
    T = data$ttm_anios,
    alpha = alpha,
    beta = beta,
    rho = rho,
    nu = nu
  )
  
  if (
    any(!is.finite(iv_model)) ||
    any(is.na(iv_model))
  ) {
    return(1e10)
  }
  
  error <- data$iv_obs - iv_model
  
  mean(error^2, na.rm = TRUE)
}

# ============================================================
# 41) Calibración SABR con forward por bucket
# ============================================================

# calibra_sabr_bucket_forward <- function(
#     data_bucket,
#     beta = 1,
#     rho_lower = -0.95,
#     rho_upper = 0.95
# ) {
#   
#   par_ini <- c(
#     alpha = median(data_bucket$iv_obs, na.rm = TRUE),
#     rho   = 0,
#     nu    = 0.5
#   )
#   
#   fit <- optim(
#     par = par_ini,
#     fn = objetivo_sabr_forward,
#     data = data_bucket,
#     beta = beta,
#     method = "L-BFGS-B",
#     lower = c(0.0001, rho_lower, 0.0001),
#     upper = c(2.0000, rho_upper, 5.0000),
#     control = list(maxit = 5000)
#   )
#   
#   alpha_hat <- fit$par[1]
#   rho_hat   <- fit$par[2]
#   nu_hat    <- fit$par[3]
#   
#   iv_fit <- sabr_vol_hagan(
#     F = data_bucket$forward_usdmxn,
#     K = data_bucket$strike,
#     T = data_bucket$ttm_anios,
#     alpha = alpha_hat,
#     beta = beta,
#     rho = rho_hat,
#     nu = nu_hat
#   )
#   
#   error <- data_bucket$iv_obs - iv_fit
#   
#   tibble(
#     bucket_venc = unique(data_bucket$bucket_venc),
#     n = nrow(data_bucket),
#     n_strikes = n_distinct(data_bucket$strike),
#     beta = beta,
#     alpha = alpha_hat,
#     rho = rho_hat,
#     nu = nu_hat,
#     rmse = sqrt(mean(error^2, na.rm = TRUE)),
#     mae = mean(abs(error), na.rm = TRUE),
#     error_mediano = median(error, na.rm = TRUE),
#     error_abs_q95 = quantile(abs(error), 0.95, na.rm = TRUE),
#     convergencia = fit$convergence,
#     valor_objetivo = fit$value
#   )
# }



# También mejoramos esta función empezando la calibración en valores con sentido (como el spot)

calibra_sabr_bucket_forward <- function(
    data_bucket,
    beta = 1,
    rho_lower = -0.95,
    rho_upper = 0.95,
    resultados_iniciales = resultados_sabr_1_rho95
) {
  
  bucket_actual <- unique(data_bucket$bucket_venc)
  
  inicial <- resultados_iniciales %>%
    filter(bucket_venc == bucket_actual)
  
  if (nrow(inicial) == 1) {
    par_ini <- c(
      alpha = inicial$alpha,
      rho   = inicial$rho,
      nu    = inicial$nu
    )
  } else {
    par_ini <- c(
      alpha = median(data_bucket$iv_obs, na.rm = TRUE),
      rho   = 0,
      nu    = 0.5
    )
  }
  
  fit <- optim(
    par = par_ini,
    fn = objetivo_sabr_forward,
    data = data_bucket,
    beta = beta,
    method = "L-BFGS-B",
    lower = c(0.0001, rho_lower, 0.0001),
    upper = c(2.0000, rho_upper, 5.0000),
    control = list(
      maxit = 10000,
      factr = 1e7,
      pgtol = 1e-8
    )
  )
  
  alpha_hat <- fit$par[1]
  rho_hat   <- fit$par[2]
  nu_hat    <- fit$par[3]
  
  iv_fit <- sabr_vol_hagan(
    F = data_bucket$forward_usdmxn,
    K = data_bucket$strike,
    T = data_bucket$ttm_anios,
    alpha = alpha_hat,
    beta = beta,
    rho = rho_hat,
    nu = nu_hat
  )
  
  error <- data_bucket$iv_obs - iv_fit
  
  tibble(
    bucket_venc = bucket_actual,
    n = nrow(data_bucket),
    n_strikes = n_distinct(data_bucket$strike),
    beta = beta,
    alpha = alpha_hat,
    rho = rho_hat,
    nu = nu_hat,
    rmse = sqrt(mean(error^2, na.rm = TRUE)),
    mae = mean(abs(error), na.rm = TRUE),
    error_mediano = median(error, na.rm = TRUE),
    error_abs_q95 = quantile(abs(error), 0.95, na.rm = TRUE),
    convergencia = fit$convergence,
    mensaje = ifelse(
      is.null(fit$message),
      NA_character_,
      fit$message
    ),
    valor_objetivo = fit$value
  )
}

# ============================================================
# 42) Ejecutar SABR clásico con forward
# ============================================================

# resultados_sabr_1_forward <- base_sabr_1_forward %>%
#   group_by(bucket_venc) %>%
#   group_split() %>%
#   lapply(
#     calibra_sabr_bucket_forward,
#     beta = 1,
#     rho_lower = -0.95,
#     rho_upper = 0.95
#   ) %>%
#   bind_rows() %>%
#   arrange(
#     match(
#       bucket_venc,
#       c("0-30d", "31-90d", "91-180d", "181-365d")
#     )
#   )


resultados_sabr_1_forward <- base_sabr_1_forward %>%
  group_by(bucket_venc) %>%
  group_split() %>%
  lapply(
    calibra_sabr_bucket_forward,
    beta = 1,
    rho_lower = -0.95,
    rho_upper = 0.95,
    resultados_iniciales = resultados_sabr_1_rho95
  ) %>%
  bind_rows() %>%
  arrange(
    match(
      bucket_venc,
      c("0-30d", "31-90d", "91-180d", "181-365d")
    )
  )

print(resultados_sabr_1_forward, width = Inf)

resultados_sabr_1_forward


# ============================================================
# 43) Incorporar parámetros y volatilidad ajustada
# ============================================================

resultados_sabr_1_forward_limpio <- resultados_sabr_1_forward %>%
  rename(
    alpha_sabr = alpha,
    rho_sabr = rho,
    nu_sabr = nu,
    beta_sabr = beta,
    rmse_calib = rmse,
    mae_calib = mae
  )

base_sabr_1_fit_forward <- base_sabr_1_forward %>%
  left_join(
    resultados_sabr_1_forward_limpio,
    by = "bucket_venc"
  ) %>%
  mutate(
    iv_sabr_forward = sabr_vol_hagan(
      F = forward_usdmxn,
      K = strike,
      T = ttm_anios,
      alpha = alpha_sabr,
      beta = beta_sabr,
      rho = rho_sabr,
      nu = nu_sabr
    ),
    error_sabr_forward =
      iv_obs - iv_sabr_forward,
    error_abs_sabr_forward =
      abs(error_sabr_forward)
  )


# ============================================================
# 44) Errores SABR con forward
# ============================================================

errores_sabr_1_forward <- base_sabr_1_fit_forward %>%
  group_by(bucket_venc) %>%
  summarise(
    n = n(),
    rmse = sqrt(
      mean(error_sabr_forward^2, na.rm = TRUE)
    ),
    mae = mean(
      abs(error_sabr_forward),
      na.rm = TRUE
    ),
    error_mediano = median(
      error_sabr_forward,
      na.rm = TRUE
    ),
    error_abs_q95 = quantile(
      abs(error_sabr_forward),
      0.95,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    match(
      bucket_venc,
      c("0-30d", "31-90d", "91-180d", "181-365d")
    )
  )

errores_sabr_1_forward

# ============================================================
# 45) Comparación SABR con spot vs forward
# ============================================================

comparacion_spot_forward <- resultados_sabr_1_rho95 %>%
  select(
    bucket_venc,
    alpha_spot = alpha,
    rho_spot = rho,
    nu_spot = nu,
    rmse_spot = rmse,
    mae_spot = mae
  ) %>%
  left_join(
    resultados_sabr_1_forward %>%
      select(
        bucket_venc,
        alpha_forward = alpha,
        rho_forward = rho,
        nu_forward = nu,
        rmse_forward = rmse,
        mae_forward = mae
      ),
    by = "bucket_venc"
  ) %>%
  mutate(
    dif_alpha = alpha_forward - alpha_spot,
    dif_rho = rho_forward - rho_spot,
    dif_nu = nu_forward - nu_spot,
    
    dif_rmse = rmse_forward - rmse_spot,
    dif_rmse_pp = 100 * dif_rmse,
    
    dif_mae = mae_forward - mae_spot,
    dif_mae_pp = 100 * dif_mae
  )

comparacion_spot_forward


# ============================================================
# 46) Curva suavizada SABR con forward
# ============================================================

curva_sabr_1_forward <- base_sabr_1_fit_forward %>%
  group_by(bucket_venc) %>%
  summarise(
    strike_min = quantile(strike, 0.01, na.rm = TRUE),
    strike_max = quantile(strike, 0.99, na.rm = TRUE),
    T_rep = median(ttm_anios, na.rm = TRUE),
    dias_rep = round(median(dias_a_venc, na.rm = TRUE)),
    F_rep = median(forward_usdmxn, na.rm = TRUE),
    alpha_sabr = first(alpha_sabr),
    beta_sabr = first(beta_sabr),
    rho_sabr = first(rho_sabr),
    nu_sabr = first(nu_sabr),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    
    K_grid <- seq(
      .$strike_min,
      .$strike_max,
      length.out = 150
    )
    
    tibble(
      bucket_venc = .$bucket_venc,
      strike = K_grid,
      T_rep = .$T_rep,
      F_rep = .$F_rep,
      iv_sabr_forward = sabr_vol_hagan(
        F = rep(.$F_rep, length(K_grid)),
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

grafica_sabr_forward <- ggplot() +
  geom_point(
    data = base_sabr_1_fit_forward,
    aes(
      x = strike,
      y = iv_obs
    ),
    alpha = 0.20
  ) +
  geom_line(
    data = curva_sabr_1_forward,
    aes(
      x = strike,
      y = iv_sabr_forward
    ),
    linewidth = 1
  ) +
  facet_wrap(
    ~ bucket_venc,
    scales = "free"
  ) +
  labs(
    title = "Ajuste SABR clásico con forward USD/MXN",
    subtitle = paste(
      "Ejercicio 1 | beta = 1 |",
      "rho en [-0.95, 0.95]"
    ),
    x = "Strike",
    y = "Volatilidad implícita"
  ) +
  theme_minimal()

grafica_sabr_forward

# Ruta gráficas

source("R/00_configuracion.R")  # rutas relativas del proyecto

dir.create(
  ruta_graficas,
  recursive = TRUE,
  showWarnings = FALSE
)

# Guardamos en PNG para la prensentación 

ggsave(
  filename = file.path(
    ruta_graficas,
    "SABR_clasico_forward_ejercicio1.png"
  ),
  plot = grafica_sabr_forward,
  width = 12,
  height = 7,
  units = "in",
  dpi = 300,
  bg = "white"
)


# Guardamos en pdf para Latex

ggsave(
  filename = file.path(
    ruta_graficas,
    "SABR_clasico_forward_ejercicio1.pdf"
  ),
  plot = grafica_sabr_forward,
  width = 12,
  height = 7,
  units = "in",
  device = cairo_pdf
)

# Resultados

resultados_sabr_1_forward
comparacion_spot_forward
errores_sabr_1_forward


# Resultados numéricos
# (oct-2026) Se guardan en la carpeta de salidas, igual que los demás scripts
# ruta_salidas se define en R/00_configuracion.R

write.csv(
  resultados_sabr_1_forward,
  file.path(
    ruta_salidas,
    "resultados_SABR_clasico_forward_ejercicio1.csv"
  ),
  row.names = FALSE
)

write.csv(
  comparacion_spot_forward,
  file.path(
    ruta_salidas,
    "comparacion_SABR_spot_forward_ejercicio1.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 46b) Verificación: consistencia entre prima actualizada y volatilidad
# ============================================================
# (nov-2026) Compara la prima actualizada con el precio de Garman-Kohlhagen
# (forma de Black sobre el forward) calculado con la volatilidad reportada,
# el forward construido y el nocional (impbase). Cociente = 1 implica
# consistencia exacta. Esperado: mediana ~1.02, ~52% en ±10%, ~80% en ±25%.

verificacion_prima <- base_sabr_1_forward %>%
  mutate(
    nocional = a_num(impbase),
    sd_T  = iv_obs * sqrt(ttm_anios),
    d1    = (log(forward_usdmxn / strike) + 0.5 * sd_T^2) / sd_T,
    d2    = d1 - sd_T,
    precio_gk = ifelse(
      tipo_opcion == "C",
      df_mxn * (forward_usdmxn * pnorm(d1)  - strike * pnorm(d2)),
      df_mxn * (strike * pnorm(-d2) - forward_usdmxn * pnorm(-d1))
    ),
    cociente = abs(prima_actualizada) / (nocional * precio_gk)
  ) %>%
  filter(is.finite(cociente), precio_gk > 1e-4)

verificacion_prima %>%
  summarise(
    n            = n(),
    mediana      = median(cociente),
    pct_10       = mean(abs(cociente - 1) < 0.10),
    pct_25       = mean(abs(cociente - 1) < 0.25)
  )

# Institución(es) que reportan una sola volatilidad para muchas operaciones
base_sabr_1_forward %>%
  group_by(inst) %>%
  summarise(n = n(), n_vol = n_distinct(iv_obs), n_strikes = n_distinct(strike),
            .groups = "drop") %>%
  filter(n_vol == 1, n > 50)
