
# Se necesitan las funciones base_sabr_1_forward y objetivo_sabr_forward
# del script 03_SABR_clasico_forward.R

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

source("R/00_configuracion.R")  # rutas relativas del proyecto

# ============================================================
# 47) Generar valores iniciales multistart por bucket
# ============================================================

crea_malla_inicial <- function(data_bucket) {
  
  alpha_ref <- median(data_bucket$iv_obs, na.rm = TRUE)
  
  tidyr::expand_grid(
    alpha_ini = alpha_ref * c(0.75, 1.00, 1.25),
    rho_ini   = c(-0.50, 0.00, 0.50, 0.85),
    nu_ini    = c(0.10, 0.30, 0.60, 1.00)
  ) %>%
    mutate(id_inicio = row_number())
}


# ============================================================
# 48) Ejecutar una calibración desde un punto inicial
# ============================================================

calibra_sabr_un_inicio <- function(
    data_bucket,
    alpha_ini,
    rho_ini,
    nu_ini,
    beta = 1,
    rho_lower = -0.95,
    rho_upper = 0.95
) {
  
  fit <- tryCatch(
    optim(
      par = c(
        alpha = alpha_ini,
        rho   = rho_ini,
        nu    = nu_ini
      ),
      fn = objetivo_sabr_forward,
      data = data_bucket,
      beta = beta,
      method = "L-BFGS-B",
      lower = c(
        alpha = 0.0001,
        rho   = rho_lower,
        nu    = 0.0001
      ),
      upper = c(
        alpha = 2,
        rho   = rho_upper,
        nu    = 5
      ),
      control = list(
        maxit = 10000,
        factr = 1e7,
        pgtol = 1e-8
      )
    ),
    error = function(e) NULL
  )
  
  if (is.null(fit)) {
    return(
      tibble(
        alpha = NA_real_,
        rho = NA_real_,
        nu = NA_real_,
        valor_objetivo = NA_real_,
        convergencia = 999L,
        mensaje = "Error en optim"
      )
    )
  }
  
  tibble(
    alpha = unname(fit$par["alpha"]),
    rho   = unname(fit$par["rho"]),
    nu    = unname(fit$par["nu"]),
    valor_objetivo = fit$value,
    convergencia = fit$convergence,
    mensaje = ifelse(
      is.null(fit$message),
      NA_character_,
      fit$message
    )
  )
}

# ============================================================
# 49) Multistart para un bucket
# ============================================================

calibra_sabr_multistart_bucket <- function(
    data_bucket,
    beta = 1,
    rho_lower = -0.95,
    rho_upper = 0.95
) {
  
  bucket_actual <- unique(data_bucket$bucket_venc)
  malla <- crea_malla_inicial(data_bucket)
  
  resultados <- pmap_dfr(
    malla,
    function(alpha_ini, rho_ini, nu_ini, id_inicio) {
      
      ajuste <- calibra_sabr_un_inicio(
        data_bucket = data_bucket,
        alpha_ini = alpha_ini,
        rho_ini = rho_ini,
        nu_ini = nu_ini,
        beta = beta,
        rho_lower = rho_lower,
        rho_upper = rho_upper
      )
      
      bind_cols(
        tibble(
          bucket_venc = bucket_actual,
          id_inicio = id_inicio,
          alpha_ini = alpha_ini,
          rho_ini = rho_ini,
          nu_ini = nu_ini
        ),
        ajuste
      )
    }
  )
  
  resultados %>%
    mutate(
      n = nrow(data_bucket),
      n_strikes = n_distinct(data_bucket$strike),
      beta = beta
    ) %>%
    arrange(valor_objetivo)
}


# ============================================================
# 50) Ejecutar calibración multistart
# ============================================================

t_inicio_sabr <- Sys.time()

lista_multistart <- base_sabr_1_forward %>%
  group_by(bucket_venc) %>%
  group_split() %>%
  lapply(
    calibra_sabr_multistart_bucket,
    beta = 1,
    rho_lower = -0.95,
    rho_upper = 0.95
  )

resultados_multistart <- bind_rows(lista_multistart)

tiempo_sabr_clasico <- as.numeric(difftime(Sys.time(), t_inicio_sabr, units = "secs"))
cat(sprintf(
  "Tiempo multistart SABR clásico: %.1f s (%.2f s por optimización, %d optimizaciones)\n",
  tiempo_sabr_clasico, tiempo_sabr_clasico / nrow(resultados_multistart),
  nrow(resultados_multistart)
))



# ============================================================
# 51) Mejor solución multistart por bucket
# ============================================================

mejor_sabr_multistart <- resultados_multistart %>%
  filter(
    convergencia == 0,
    is.finite(valor_objetivo)
  ) %>%
  group_by(bucket_venc) %>%
  slice_min(
    order_by = valor_objetivo,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  mutate(
    rmse = sqrt(valor_objetivo)
  ) %>%
  arrange(
    match(
      bucket_venc,
      c("0-30d", "31-90d", "91-180d", "181-365d")
    )
  )

print(mejor_sabr_multistart, width = Inf)


# Revisión del bucket 0-30

# ============================================================
# 52) Mejores soluciones para el bucket 0-30d
# ============================================================

top_multistart_0_30 <- resultados_multistart %>%
  filter(
    bucket_venc == "0-30d",
    convergencia == 0
  ) %>%
  mutate(
    rmse = sqrt(valor_objetivo)
  ) %>%
  arrange(valor_objetivo) %>%
  select(
    id_inicio,
    alpha_ini,
    rho_ini,
    nu_ini,
    alpha,
    rho,
    nu,
    rmse,
    valor_objetivo,
    convergencia
  ) %>%
  slice_head(n = 15)

print(top_multistart_0_30, width = Inf)



# Analizar los puntos que minimizan el error

# ============================================================
# 53) Identificación paramétrica en soluciones casi óptimas
# ============================================================

soluciones_casi_optimas <- resultados_multistart %>%
  filter(
    convergencia == 0,
    is.finite(valor_objetivo)
  ) %>%
  group_by(bucket_venc) %>%
  mutate(
    objetivo_min = min(valor_objetivo),
    brecha_relativa =
      (valor_objetivo - objetivo_min) / objetivo_min,
    es_casi_optima = brecha_relativa <= 0.001
  ) %>%
  ungroup()


estabilidad_multistart <- soluciones_casi_optimas %>%
  filter(es_casi_optima) %>%
  group_by(bucket_venc) %>%
  summarise(
    n_soluciones = n(),
    
    alpha_min = min(alpha),
    alpha_mediana = median(alpha),
    alpha_max = max(alpha),
    
    rho_min = min(rho),
    rho_mediana = median(rho),
    rho_max = max(rho),
    
    nu_min = min(nu),
    nu_mediana = median(nu),
    nu_max = max(nu),
    
    objetivo_min = min(valor_objetivo),
    objetivo_max = max(valor_objetivo),
    
    .groups = "drop"
  ) %>%
  arrange(
    match(
      bucket_venc,
      c("0-30d", "31-90d", "91-180d", "181-365d")
    )
  )

print(estabilidad_multistart, width = Inf)


# ============================================================
# 54) Comparación calibración actual vs multistart
# ============================================================

comparacion_forward_multistart <- resultados_sabr_1_forward %>%
  select(
    bucket_venc,
    alpha_actual = alpha,
    rho_actual = rho,
    nu_actual = nu,
    rmse_actual = rmse,
    valor_actual = valor_objetivo
  ) %>%
  left_join(
    mejor_sabr_multistart %>%
      select(
        bucket_venc,
        alpha_multistart = alpha,
        rho_multistart = rho,
        nu_multistart = nu,
        rmse_multistart = rmse,
        valor_multistart = valor_objetivo
      ),
    by = "bucket_venc"
  ) %>%
  mutate(
    dif_rmse = rmse_multistart - rmse_actual,
    dif_rmse_pp = 100 * dif_rmse,
    
    mejora_relativa_objetivo =
      (valor_actual - valor_multistart) /
      valor_actual
  )

print(comparacion_forward_multistart, width = Inf)


# Gráfica para ver los mínimos locales en el periodo 0-30d

ggplot(
  resultados_multistart %>%
    filter(
      bucket_venc == "0-30d",
      convergencia == 0
    ),
  aes(
    x = nu,
    y = valor_objetivo
  )
) +
  geom_point(
    aes(shape = factor(rho_ini)),
    alpha = 0.7
  ) +
  labs(
    title = "Calibración multistart SABR: bucket 0-30 días",
    subtitle = "Valor objetivo respecto al parámetro nu calibrado",
    x = "Nu calibrado",
    y = "Error cuadrático medio",
    shape = "Rho inicial"
  ) +
  theme_minimal()


# Encontramos estabilidad en los parámetros y encontramos una mejor solución para todos los buckets
# Se corre de nuevo la función con los nuevos parámetros y la guardamos como SABR_clasico

resultados_sabr_clasico_final <- mejor_sabr_multistart %>%
  select(
    bucket_venc,
    n,
    n_strikes,
    beta,
    alpha,
    rho,
    nu,
    rmse,
    valor_objetivo,
    convergencia
  )

# Guardamos los parámetros

# ruta_salidas se define en R/00_configuracion.R (la misma que lee 06_rough_SABR.R)

write.csv(
  resultados_sabr_clasico_final,
  file.path(
    ruta_salidas,
    "resultados_SABR_clasico_forward_multistart.csv"
  ),
  row.names = FALSE
)


# ===============================================
# Gráfica con los parámetros multistart
# ===============================================

# ============================================================
# Parámetros finales SABR clásico provenientes del multistart
# ============================================================

parametros_sabr_final <- mejor_sabr_multistart %>%
  select(
    bucket_venc,
    beta,
    alpha,
    rho,
    nu
  ) %>%
  rename(
    beta_sabr = beta,
    alpha_sabr = alpha,
    rho_sabr = rho,
    nu_sabr = nu
  )


# ============================================================
# Base ajustada con parámetros multistart
# ============================================================

base_sabr_1_fit_multistart <- base_sabr_1_forward %>%
  select(
    -any_of(c(
      "beta_sabr",
      "alpha_sabr",
      "rho_sabr",
      "nu_sabr",
      "iv_sabr_forward",
      "error_sabr_forward",
      "error_abs_sabr_forward"
    ))
  ) %>%
  left_join(
    parametros_sabr_final,
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
    error_sabr_forward = iv_obs - iv_sabr_forward,
    error_abs_sabr_forward = abs(error_sabr_forward)
  )

# ============================================================
# Curva SABR suavizada con parámetros multistart
# ============================================================

curva_sabr_1_multistart <- base_sabr_1_fit_multistart %>%
  group_by(bucket_venc) %>%
  summarise(
    strike_min = quantile(strike, 0.01, na.rm = TRUE),
    strike_max = quantile(strike, 0.99, na.rm = TRUE),
    T_rep = median(ttm_anios, na.rm = TRUE),
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

# (Se eliminó un bloque duplicado idéntico de la curva suavizada.)


library(viridis)

# ---------------------------
# Cambiamos el orden para mejor lectura

orden_buckets <- c(
  "0-30d",
  "31-90d",
  "91-180d",
  "181-365d"
)

base_sabr_1_fit_multistart <- base_sabr_1_fit_multistart %>%
  mutate(
    bucket_venc = factor(
      bucket_venc,
      levels = orden_buckets
    )
  )

curva_sabr_1_multistart <- curva_sabr_1_multistart %>%
  mutate(
    bucket_venc = factor(
      bucket_venc,
      levels = orden_buckets
    )
  )


# Gráfica

grafica_sabr_forward_multistart <- ggplot() +
  geom_point(
    data = base_sabr_1_fit_multistart,
    aes(
      x = strike,
      y = iv_obs
    ),
    color = "grey45",
    alpha = 0.22,
    size = 1.2
  ) +
  geom_line(
    data = curva_sabr_1_multistart %>%
      arrange(bucket_venc, strike),
    aes(
      x = strike,
      y = iv_sabr_forward,
      color = bucket_venc,
      group = bucket_venc
    ),
    linewidth = 1.25,
    show.legend = FALSE
  ) +
  scale_color_viridis_d(
    option = "C",
    begin = 0.10,
    end = 0.85
  ) +
  facet_wrap(
    ~ bucket_venc,
    ncol = 2,
    dir = "h",
    scales = "free"
  ) +
  labs(
    title = "Ajuste SABR clásico con forward USD/MXN",
    subtitle = paste(
      "Calibración multistart | beta = 1 |",
      "rho en [-0.95, 0.95]"
    ),
    x = "Strike",
    y = "Volatilidad implícita"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10
    ),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )



grafica_sabr_forward_multistart


ggsave(
  filename = file.path(
    ruta_graficas,
    "SABR_clasico_forward_multistart.png"
  ),
  plot = grafica_sabr_forward_multistart,
  width = 12,
  height = 7,
  units = "in",
  dpi = 400,
  bg = "white"
)

# Guardamos el pdf para el latex

ggsave(
  filename = file.path(
    ruta_graficas,
    "SABR_clasico_forward_multistart.pdf"
  ),
  plot = grafica_sabr_forward_multistart,
  width = 12,
  height = 7,
  units = "in",
  device = cairo_pdf
)