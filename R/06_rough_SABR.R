# ============================================================
# ROUGH SABR — Calibración sobre opciones USD/MXN
# Fukasawa & Gatheral (2021)
#
# Depende de: base_sabr_1_forward, mejor_sabr_multistart
# (generados en 03_SABR_clasico_forward.R y 05_multistart_SABR_clasico.R)
#
# Parametrización:
#   - alpha_0 por bucket  (4 parámetros de nivel)
#   - H, rho, eta globales (3 parámetros de forma)
#   Total: 7 parámetros  vs  12 del SABR clásico
#
# Implementación:
#   - Aproximación cerrada GA (Ec. 5.2 del paper) para calibrar
#   - Solución numérica de la ODE para validar
# ============================================================

# CORRIDAS PREVIAS ----------------------------
# CORRER DE 01_preparacion_muestra_SABR_spot.R: otc_exotic, otc_exotic_std, base_sabr_1, base_sabr_1_calib

# ============================================================
# CARGA DE RESULTADOS PREVIOS (sesión nueva)
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(viridis)
library(deSolve)

source("R/00_configuracion.R")  # rutas relativas del proyecto

# --- 1) Parámetros SABR clásico ---
mejor_sabr_multistart <- read.csv(
  file.path(ruta_salidas, "resultados_SABR_clasico_forward_multistart.csv"),
  stringsAsFactors = FALSE
)

# --- 2) Reconstruir curva suavizada SABR clásico ---
# (necesita base_sabr_1_forward en memoria y sabr_vol_hagan definida)

orden_buckets <- c("0-30d", "31-90d", "91-180d", "181-365d")

parametros_sabr_final <- mejor_sabr_multistart %>%
  select(bucket_venc, beta, alpha, rho, nu) %>%
  rename(
    beta_sabr  = beta,
    alpha_sabr = alpha,
    rho_sabr   = rho,
    nu_sabr    = nu
  )

base_sabr_1_fit_multistart <- base_sabr_1_forward %>%
  left_join(parametros_sabr_final, by = "bucket_venc") %>%
  mutate(
    iv_sabr_forward = sabr_vol_hagan(
      F     = forward_usdmxn,
      K     = strike,
      T     = ttm_anios,
      alpha = alpha_sabr,
      beta  = beta_sabr,
      rho   = rho_sabr,
      nu    = nu_sabr
    ),
    error_sabr_forward     = iv_obs - iv_sabr_forward,
    error_abs_sabr_forward = abs(error_sabr_forward),
    bucket_venc = factor(bucket_venc, levels = orden_buckets)
  )

curva_sabr_1_multistart <- base_sabr_1_fit_multistart %>%
  group_by(bucket_venc) %>%
  summarise(
    strike_min = quantile(strike, 0.01, na.rm = TRUE),
    strike_max = quantile(strike, 0.99, na.rm = TRUE),
    T_rep      = median(ttm_anios, na.rm = TRUE),
    F_rep      = median(forward_usdmxn, na.rm = TRUE),
    alpha_sabr = first(alpha_sabr),
    beta_sabr  = first(beta_sabr),
    rho_sabr   = first(rho_sabr),
    nu_sabr    = first(nu_sabr),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    K_grid <- seq(.$strike_min, .$strike_max, length.out = 150)
    tibble(
      bucket_venc    = .$bucket_venc,
      strike         = K_grid,
      iv_sabr_forward = sabr_vol_hagan(
        F     = rep(.$F_rep, 150),
        K     = K_grid,
        T     = rep(.$T_rep, 150),
        alpha = .$alpha_sabr,
        beta  = .$beta_sabr,
        rho   = .$rho_sabr,
        nu    = .$nu_sabr
      )
    )
  }) %>%
  ungroup() %>%
  mutate(bucket_venc = factor(bucket_venc, levels = orden_buckets))

cat("Todo listo. Puedes continuar con el bloque 55.\n")


# LIBRERIAS ------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(viridis)
library(deSolve)   # para la validación ODE


# ============================================================
# 55) Fórmulas auxiliares para Rough SABR
# ============================================================

# --- G_{1/2}(y): caso H = 1/2, equivalente al SABR clásico ---
# Ec. (4.2) del paper: g(y) = -2 log((sqrt(1 + rho*y + y^2/4) - rho - y/2) / (1 - rho))
# G_{1/2} = g^2

G_half <- function(y, rho) {
  interior <- (sqrt(1 + rho * y + y^2 / 4) - rho - y / 2) / (1 - rho)
  interior <- pmax(interior, 1e-12)   # evitar log(0)
  g <- -2 * log(interior)
  g^2
}

# --- G_0(y): caso H = 0 ---
# Ec. (4.3) del paper

G_zero <- function(y, rho) {
  rho2 <- sqrt(1 - rho^2)
  term1 <- log(1 + 2 * rho * y + y^2)
  term2 <- (2 * rho / rho2) * (atan(rho / rho2) - atan((y + rho) / rho2))
  term1 + term2
}

# --- Aproximación cerrada GA(y) ---
# Ec. (5.2) del paper: interpolación entre G_0 y G_{1/2}
# GA(y) = (2H+1)^2 * [ 3(1-2H)/(2H+3) * G_0(y/(2H+1))
#                     + 2H/(2H+3)    * G_{1/2}(2y/(2H+1)) ]

G_A <- function(y, H, rho) {
  c1 <- (2 * H + 1)^2
  w0 <- 3 * (1 - 2 * H) / (2 * H + 3)
  w1 <- 2 * H / (2 * H + 3)

  c1 * (w0 * G_zero(y / (2 * H + 1), rho) +
          w1 * G_half(2 * y / (2 * H + 1), rho))
}

# --- Volatilidad implícita Rough SABR (fórmula GA) ---
# Ec. (6.1) del paper:
#   Sigma(k, tau) = Sigma_ATM * |y| / sqrt(GA(y))
# donde:
#   y  = kappa(tau) * k / U
#   kappa(tau) = eta * sqrt(2H) * tau^(H - 0.5)
#   U  = alpha_0   (para calibración con forward variance plana)
#   k  = log(K / F)

sigma_rough_sabr_GA <- function(F, K, tau, alpha0, H, rho, eta) {

  # log-moneyness
  k <- log(K / F)

  # kappa(tau) = eta * sqrt(2H) * tau^(H - 0.5)
  kappa_tau <- eta * sqrt(2 * H) * tau^(H - 0.5)

  # U = alpha_0 (forward variance curve plana)
  U <- alpha0

  # y normalizado
  y <- kappa_tau * k / U

  # Volatilidad normalizada f(y) = |y| / sqrt(GA(y))
  GA_val <- G_A(y, H, rho)
  GA_val <- pmax(GA_val, 1e-12)

  f_y <- ifelse(
    abs(y) < 1e-8,
    1,                         # límite ATM: f(0) = 1
    abs(y) / sqrt(GA_val)
  )

  # Sigma_ATM * f(y): Sigma_ATM = alpha_0 en la aproximación de corto plazo
  sigma_atm <- alpha0

  sigma_iv <- sigma_atm * f_y

  # Seguridad numérica
  ifelse(is.finite(sigma_iv) & sigma_iv > 0, sigma_iv, NA_real_)
}


# ============================================================
# 56) Solución numérica de la ODE (para validación)
# ============================================================
# ODE de Fukasawa-Gatheral (Ec. 4.1):
# g'(y)^2 * q(y) = 1 - (1-2H) * (1 - y*g'(y)/g(y))
# donde q(y) = 1 + 2*rho*y/(2H+1) + y^2/(2H+1)^2
#
# Despejando g'(y) (raíz positiva de la ecuación cuadrática):
# g'(y) = [(1-2H)*f(y) + sqrt((1-2H)^2*f(y)^2 + 8H*q(y))] / (2*q(y))
# con f(y) = y/g(y), g(0)=0, condición inicial: g ~ y para y pequeño
# (g'(0) = 1).
#
# CORRECCIÓN (oct-2026): con rho != 0, q(y) no es simétrica y g NO es
# impar. La rama y < 0 se obtiene con la relación
#     g(y; rho) = -g(-y; -rho),
# es decir, resolviendo la misma ODE para y > 0 con -rho.
# La versión anterior usaba g(-y) = -g(y), lo que asignaba a strikes
# por debajo del forward la volatilidad del strike simétrico por
# arriba y generaba un sesgo artificial de ~1 p.p. en la validación.

sigma_rough_sabr_ODE <- function(F, K, tau, alpha0, H, rho, eta,
                                  y_grid_n = 400,
                                  y_max = 15) {

  k_vec  <- log(K / F)
  kappa_tau <- eta * sqrt(2 * H) * tau^(H - 0.5)
  U      <- alpha0
  y_vec  <- kappa_tau * k_vec / U

  # Grid para y > 0 (la rama y < 0 se obtiene con -rho, ver arriba)
  y_span <- seq(1e-6, y_max, length.out = y_grid_n)

  ode_rhs <- function(t, state, parms) {
    g_val <- state[1]
    H_p   <- parms$H
    rho_p <- parms$rho

    f_val <- t / g_val
    q_val <- 1 + 2 * rho_p * t / (2 * H_p + 1) + t^2 / (2 * H_p + 1)^2

    disc <- (1 - 2 * H_p)^2 * f_val^2 + 8 * H_p * q_val
    disc <- max(disc, 0)

    dgdt <- ((1 - 2 * H_p) * f_val + sqrt(disc)) / (2 * q_val)

    list(dgdt)
  }

  # Condición inicial: g(y) ~ y para y -> 0
  resuelve_g <- function(rho_p) {
    sol <- ode(
      y      = c(g = y_span[1]),
      times  = y_span,
      func   = ode_rhs,
      parms  = list(H = H, rho = rho_p),
      method = "lsoda"
    )
    # Interpolación por splines cúbicos: la interpolación lineal
    # agregaba un error de interpolación de hasta ~0.01 p.p.
    splinefun(sol[, "time"], sol[, "g"], method = "fmm")
  }

  g_pos <- resuelve_g(rho)    # rama y > 0
  g_neg <- resuelve_g(-rho)   # rama y < 0: g(y; rho) = -g(-y; -rho)

  if (any(abs(y_vec) > y_max)) {
    warning("Hay |y| > y_max: g se extrapola como constante; aumentar y_max.")
  }

  g_eval <- ifelse(
    abs(y_vec) < 1e-6,
    y_vec,
    ifelse(y_vec > 0, g_pos(y_vec), -g_neg(-y_vec))
  )

  # f(y) = y / g(y), con límite f(0) = 1
  f_y <- ifelse(abs(y_vec) < 1e-6, 1, y_vec / g_eval)

  sigma_iv <- alpha0 * f_y
  ifelse(is.finite(sigma_iv) & sigma_iv > 0, sigma_iv, NA_real_)
}


# ============================================================
# 56b) Verificación del procedimiento numérico (casos límite)
# ============================================================
# En H = 1/2 y H -> 0 la ODE tiene solución cerrada (G_{1/2} y G_0),
# y G_A coincide exactamente con ellas. Si el solver es correcto,
# la vol. de la ODE debe coincidir con la de G_A en ambos límites,
# para strikes por arriba y por debajo del forward y para rho > 0 y < 0.

verifica_limite_ode <- function(H_test, rho_test,
                                tau = 0.1, alpha0 = 0.12, F = 20.5) {
  # eta tal que kappa(tau) = 1, para cubrir |y| hasta ~2.5
  eta_test <- 1 / (sqrt(2 * H_test) * tau^(H_test - 0.5))
  K_grid   <- F * exp(seq(-0.30, 0.30, length.out = 61))

  iv_ga  <- sigma_rough_sabr_GA(F, K_grid, tau, alpha0, H_test, rho_test, eta_test)
  iv_ode <- sigma_rough_sabr_ODE(F, K_grid, tau, alpha0, H_test, rho_test, eta_test)

  tibble(
    H            = H_test,
    rho          = rho_test,
    dif_abs_max  = max(abs(iv_ga - iv_ode), na.rm = TRUE),
    dif_abs_max_pp = 100 * dif_abs_max
  )
}

verificacion_limites_ode <- bind_rows(
  verifica_limite_ode(0.5,  0.38),
  verifica_limite_ode(0.5, -0.38),
  verifica_limite_ode(1e-4, 0.38),
  verifica_limite_ode(1e-4, -0.38)
)

cat("\n--- Verificación de la ODE en casos límite (debe ser ~0) ---\n")
print(verificacion_limites_ode, width = Inf)


# ============================================================
# 57) Función objetivo Rough SABR (GA)
# ============================================================
# par = c(alpha_b1, alpha_b2, alpha_b3, alpha_b4, H, rho, eta)
# El orden de buckets sigue: "0-30d", "31-90d", "91-180d", "181-365d"

objetivo_rough_sabr <- function(par, data, orden_buckets) {

  alpha_list <- par[1:4]
  H   <- par[5]
  rho <- par[6]
  eta <- par[7]

  # Restricciones internas (penalización)
  if (
    any(alpha_list <= 0) ||
    H   <= 0 || H   >= 0.5 ||
    rho <= -0.95 || rho >= 0.95 ||
    eta <= 0
  ) {
    return(1e10)
  }

  # Asignar alpha según bucket (indexación directa; antes se usaba
  # mutate() de dplyr en cada evaluación, mismo resultado pero más lento,
  # lo que inflaba el tiempo de cómputo por implementación y no por modelo)
  alpha0_vec <- alpha_list[match(as.character(data$bucket_venc), orden_buckets)]

  # Calcular volatilidad modelo
  iv_model <- sigma_rough_sabr_GA(
    F      = data$forward_usdmxn,
    K      = data$strike,
    tau    = data$ttm_anios,
    alpha0 = alpha0_vec,
    H      = H,
    rho    = rho,
    eta    = eta
  )

  if (any(!is.finite(iv_model)) || any(is.na(iv_model))) {
    return(1e10)
  }

  error <- data$iv_obs - iv_model
  mean(error^2, na.rm = TRUE)
}


# ============================================================
# 58) Malla de valores iniciales para multistart
# ============================================================

orden_buckets <- c("0-30d", "31-90d", "91-180d", "181-365d")

# Alpha iniciales: medianas por bucket (sensatas y rápidas)
alpha_ini_por_bucket <- base_sabr_1_forward %>%
  group_by(bucket_venc) %>%
  summarise(
    alpha_med = median(iv_obs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(bucket_venc, orden_buckets)) %>%
  pull(alpha_med)

crea_malla_rough_sabr <- function(alpha_ref_vec) {
  
  tidyr::expand_grid(
    H_ini       = c(0.05, 0.10, 0.20, 0.35),
    rho_ini     = c(-0.50, 0.00, 0.40, 0.75),
    eta_ini     = c(0.30, 0.80, 1.50),
    alpha_scale = c(0.90, 1.00, 1.10)
  ) %>%
    mutate(id_inicio = row_number()) %>%
    rowwise() %>%
    mutate(
      alpha_ini = list(alpha_ref_vec * alpha_scale)
    ) %>%
    ungroup()
}

malla_rough <- crea_malla_rough_sabr(alpha_ini_por_bucket)

cat(
  "Número de puntos iniciales en la malla:",
  nrow(malla_rough), "\n"
)


# ============================================================
# 59) Calibración multistart Rough SABR
# ============================================================

calibra_rough_sabr_un_inicio <- function(
    data,
    alpha_ini_vec,
    H_ini,
    rho_ini,
    eta_ini,
    orden_buckets
) {

  par_ini <- c(
    alpha_ini_vec,
    H   = H_ini,
    rho = rho_ini,
    eta = eta_ini
  )

  # Cotas
  lower_bound <- c(
    rep(0.0001, 4),   # alphas > 0
    H   = 0.001,
    rho = -0.95,
    eta = 0.0001
  )
  upper_bound <- c(
    rep(1.5,    4),   # alphas razonables para vol implícita
    H   = 0.499,
    rho = 0.95,
    eta = 5.0
  )

  fit <- tryCatch(
    optim(
      par    = par_ini,
      fn     = objetivo_rough_sabr,
      data   = data,
      orden_buckets = orden_buckets,
      method = "L-BFGS-B",
      lower  = lower_bound,
      upper  = upper_bound,
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
        alpha_b1 = NA_real_, alpha_b2 = NA_real_,
        alpha_b3 = NA_real_, alpha_b4 = NA_real_,
        H = NA_real_, rho = NA_real_, eta = NA_real_,
        valor_objetivo = NA_real_,
        convergencia   = 999L,
        mensaje        = "Error en optim"
      )
    )
  }

  tibble(
    alpha_b1 = unname(fit$par[1]),
    alpha_b2 = unname(fit$par[2]),
    alpha_b3 = unname(fit$par[3]),
    alpha_b4 = unname(fit$par[4]),
    H        = unname(fit$par[5]),
    rho      = unname(fit$par[6]),
    eta      = unname(fit$par[7]),
    valor_objetivo = fit$value,
    convergencia   = fit$convergence,
    mensaje = ifelse(
      is.null(fit$message), NA_character_, fit$message
    )
  )
}


# Ejecutar multistart (puede tomar ~2-5 minutos según equipo)
cat("Iniciando calibración multistart Rough SABR...\n")

t_inicio_rough <- Sys.time()

resultados_rough_multistart <- malla_rough %>%
  rowwise() %>%
  mutate(
    resultado = list(
      calibra_rough_sabr_un_inicio(
        data          = base_sabr_1_forward,
        alpha_ini_vec = unlist(alpha_ini),
        H_ini         = H_ini,
        rho_ini       = rho_ini,
        eta_ini       = eta_ini,
        orden_buckets = orden_buckets
      )
    )
  ) %>%
  ungroup() %>%
  select(id_inicio, H_ini, rho_ini, eta_ini, resultado) %>%
  unnest(resultado)

tiempo_rough_sabr <- as.numeric(difftime(Sys.time(), t_inicio_rough, units = "secs"))

cat("Calibración multistart completada.\n")
cat(sprintf(
  "Tiempo multistart Rough SABR: %.1f s (%.2f s por optimización, %d optimizaciones)\n",
  tiempo_rough_sabr, tiempo_rough_sabr / nrow(malla_rough), nrow(malla_rough)
))


# ============================================================
# 60) Mejor solución Rough SABR
# ============================================================

mejor_rough_sabr <- resultados_rough_multistart %>%
  filter(
    convergencia == 0,
    is.finite(valor_objetivo)
  ) %>%
  slice_min(
    order_by = valor_objetivo,
    n = 1,
    with_ties = FALSE
  )

cat("\n--- Mejores parámetros Rough SABR ---\n")
print(mejor_rough_sabr, width = Inf)


# Extraer parámetros finales
alpha_rough_final <- c(
  mejor_rough_sabr$alpha_b1,
  mejor_rough_sabr$alpha_b2,
  mejor_rough_sabr$alpha_b3,
  mejor_rough_sabr$alpha_b4
)
H_final   <- mejor_rough_sabr$H
rho_final <- mejor_rough_sabr$rho
eta_final <- mejor_rough_sabr$eta

cat(
  sprintf(
    "\nParámetros globales: H = %.4f | rho = %.4f | eta = %.4f\n",
    H_final, rho_final, eta_final
  )
)
cat(
  "Alphas por bucket:",
  paste(round(alpha_rough_final, 4), collapse = " | "), "\n"
)


# ============================================================
# 61) Volatilidades ajustadas y errores (GA)
# ============================================================

alpha_map_final <- setNames(alpha_rough_final, orden_buckets)

base_rough_sabr_fit <- base_sabr_1_forward %>%
  mutate(
    alpha0_rough = alpha_map_final[bucket_venc],
    iv_rough_GA  = sigma_rough_sabr_GA(
      F      = forward_usdmxn,
      K      = strike,
      tau    = ttm_anios,
      alpha0 = alpha0_rough,
      H      = H_final,
      rho    = rho_final,
      eta    = eta_final
    ),
    error_rough_GA     = iv_obs - iv_rough_GA,
    error_abs_rough_GA = abs(error_rough_GA)
  )


# ============================================================
# 62) Métricas por bucket (GA)
# ============================================================

errores_rough_sabr_GA <- base_rough_sabr_fit %>%
  group_by(bucket_venc) %>%
  summarise(
    n             = n(),
    rmse          = sqrt(mean(error_rough_GA^2, na.rm = TRUE)),
    mae           = mean(error_abs_rough_GA, na.rm = TRUE),
    error_mediano = median(error_rough_GA, na.rm = TRUE),
    error_abs_q95 = quantile(error_abs_rough_GA, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(bucket_venc, orden_buckets))

cat("\n--- Errores Rough SABR (GA) por bucket ---\n")
print(errores_rough_sabr_GA, width = Inf)


# ============================================================
# 63) Validación con solución numérica de la ODE
# ============================================================
# Aplicamos la ODE solo al mejor punto calibrado para confirmar
# que la aproximación GA no distorsiona los resultados.
# Por eficiencia usamos una submuestra representativa por bucket.

cat("\nValidando con ODE numérica (submuestra)...\n")

set.seed(42)
submuestra_ode <- base_sabr_1_forward %>%
  group_by(bucket_venc) %>%
  slice_sample(n = 100, replace = FALSE) %>%
  ungroup() %>%
  mutate(alpha0_rough = alpha_map_final[bucket_venc])

# Calcular ODE observación por observación
iv_ode_vec <- mapply(
  function(F_i, K_i, tau_i, alpha_i) {
    sigma_rough_sabr_ODE(
      F      = F_i,
      K      = K_i,
      tau    = tau_i,
      alpha0 = alpha_i,
      H      = H_final,
      rho    = rho_final,
      eta    = eta_final
    )
  },
  submuestra_ode$forward_usdmxn,
  submuestra_ode$strike,
  submuestra_ode$ttm_anios,
  submuestra_ode$alpha0_rough
)

submuestra_ode <- submuestra_ode %>%
  mutate(
    iv_rough_GA  = sigma_rough_sabr_GA(
      F = forward_usdmxn, K = strike, tau = ttm_anios,
      alpha0 = alpha0_rough, H = H_final, rho = rho_final, eta = eta_final
    ),
    iv_rough_ODE = iv_ode_vec,
    dif_GA_ODE   = iv_rough_GA - iv_rough_ODE
  )

validacion_ode <- submuestra_ode %>%
  group_by(bucket_venc) %>%
  summarise(
    n               = n(),
    dif_media       = mean(dif_GA_ODE, na.rm = TRUE),
    dif_abs_media   = mean(abs(dif_GA_ODE), na.rm = TRUE),
    dif_abs_max     = max(abs(dif_GA_ODE), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    dif_media_pp     = 100 * dif_media,
    dif_abs_media_pp = 100 * dif_abs_media,
    dif_abs_max_pp   = 100 * dif_abs_max
  ) %>%
  arrange(match(bucket_venc, orden_buckets))

cat("\n--- Validación GA vs ODE (diferencia en vol implícita) ---\n")
print(validacion_ode, width = Inf)

# Diagnóstico por lado del smile: con la corrección, la diferencia
# debe ser del mismo orden para strikes por debajo y por arriba del forward
validacion_ode_lado <- submuestra_ode %>%
  mutate(lado = ifelse(strike < forward_usdmxn, "K < F", "K >= F")) %>%
  group_by(lado) %>%
  summarise(
    n                = n(),
    dif_media_pp     = 100 * mean(dif_GA_ODE, na.rm = TRUE),
    dif_abs_max_pp   = 100 * max(abs(dif_GA_ODE), na.rm = TRUE),
    .groups = "drop"
  )

print(validacion_ode_lado, width = Inf)
cat(
  "\nNota: diferencias < 0.001 (0.1 pp) indican que la",
  "aproximación GA es adecuada.\n"
)


# ============================================================
# 64) Comparación Rough SABR vs SABR clásico
# ============================================================

# Errores SABR clásico (del script multistart)
errores_sabr_clasico <- base_sabr_1_fit_multistart %>%
  group_by(bucket_venc) %>%
  summarise(
    n_clasico             = n(),
    rmse_clasico          = sqrt(mean(error_sabr_forward^2, na.rm = TRUE)),
    mae_clasico           = mean(error_abs_sabr_forward, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(match(bucket_venc, orden_buckets))

comparacion_modelos <- errores_sabr_clasico %>%
  left_join(
    errores_rough_sabr_GA %>%
      select(
        bucket_venc,
        n_rough  = n,
        rmse_rough = rmse,
        mae_rough  = mae
      ),
    by = "bucket_venc"
  ) %>%
  mutate(
    dif_rmse    = rmse_rough - rmse_clasico,
    dif_rmse_pp = 100 * dif_rmse,
    dif_mae     = mae_rough  - mae_clasico,
    dif_mae_pp  = 100 * dif_mae
  )

cat("\n--- Comparación RMSE y MAE: Rough SABR vs SABR Clásico ---\n")
print(comparacion_modelos, width = Inf)

# Resumen de parsimonia
cat(
  sprintf(
    "\nParámetros estimados — SABR clásico: %d | Rough SABR: %d\n",
    4 * 3L,   # 4 buckets x 3 parámetros
    4L + 3L   # 4 alphas + H, rho, eta
  )
)


# ============================================================
# 65) Estabilidad del multistart Rough SABR
# ============================================================

estabilidad_rough <- resultados_rough_multistart %>%
  filter(
    convergencia == 0,
    is.finite(valor_objetivo)
  ) %>%
  mutate(
    objetivo_min     = min(valor_objetivo),
    brecha_relativa  = (valor_objetivo - objetivo_min) / objetivo_min,
    es_casi_optima   = brecha_relativa <= 0.001
  ) %>%
  filter(es_casi_optima) %>%
  summarise(
    n_soluciones  = n(),

    H_min = min(H), H_med = median(H), H_max = max(H),
    rho_min = min(rho), rho_med = median(rho), rho_max = max(rho),
    eta_min = min(eta), eta_med = median(eta), eta_max = max(eta),

    alpha_b1_min = min(alpha_b1), alpha_b1_med = median(alpha_b1),
    alpha_b2_min = min(alpha_b2), alpha_b2_med = median(alpha_b2),
    alpha_b3_min = min(alpha_b3), alpha_b3_med = median(alpha_b3),
    alpha_b4_min = min(alpha_b4), alpha_b4_med = median(alpha_b4),

    objetivo_min = min(valor_objetivo),
    objetivo_max = max(valor_objetivo)
  )

cat("\n--- Estabilidad multistart Rough SABR (soluciones casi óptimas) ---\n")
print(estabilidad_rough, width = Inf)


# ============================================================
# 66) Curva Rough SABR representativa por bucket (GA)
# ============================================================

curva_rough_sabr <- base_rough_sabr_fit %>%
  group_by(bucket_venc) %>%
  summarise(
    strike_min   = quantile(strike, 0.01, na.rm = TRUE),
    strike_max   = quantile(strike, 0.99, na.rm = TRUE),
    T_rep        = median(ttm_anios, na.rm = TRUE),
    F_rep        = median(forward_usdmxn, na.rm = TRUE),
    alpha0_rep   = first(alpha0_rough),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  do({
    K_grid <- seq(.$strike_min, .$strike_max, length.out = 150)
    tibble(
      bucket_venc = .$bucket_venc,
      strike      = K_grid,
      iv_rough_GA = sigma_rough_sabr_GA(
        F      = rep(.$F_rep, 150),
        K      = K_grid,
        tau    = rep(.$T_rep, 150),
        alpha0 = .$alpha0_rep,
        H      = H_final,
        rho    = rho_final,
        eta    = eta_final
      )
    )
  }) %>%
  ungroup() %>%
  mutate(
    bucket_venc = factor(bucket_venc, levels = orden_buckets)
  )

base_rough_sabr_fit <- base_rough_sabr_fit %>%
  mutate(
    bucket_venc = factor(bucket_venc, levels = orden_buckets)
  )


# ============================================================
# 67) Gráfica ajuste Rough SABR
# ============================================================

grafica_rough_sabr <- ggplot() +
  geom_point(
    data = base_rough_sabr_fit,
    aes(x = strike, y = iv_obs),
    color = "grey45",
    alpha = 0.22,
    size  = 1.2
  ) +
  geom_line(
    data = curva_rough_sabr %>% arrange(bucket_venc, strike),
    aes(
      x     = strike,
      y     = iv_rough_GA,
      color = bucket_venc,
      group = bucket_venc
    ),
    linewidth    = 1.25,
    show.legend  = FALSE
  ) +
  scale_color_viridis_d(
    option = "C",
    begin  = 0.10,
    end    = 0.85
  ) +
  facet_wrap(
    ~ bucket_venc,
    ncol   = 2,
    dir    = "h",
    scales = "free"
  ) +
  labs(
    title    = "Ajuste Rough SABR con forward USD/MXN",
    subtitle = sprintf(
      "H = %.3f | rho = %.3f | eta = %.3f | alpha por bucket",
      H_final, rho_final, eta_final
    ),
    x = "Strike",
    y = "Volatilidad implícita"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    strip.text    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

grafica_rough_sabr


# ============================================================
# 68) Gráfica comparativa: Rough SABR vs SABR Clásico
# ============================================================

# Curva SABR clásico (ya disponible de multistart)
curva_clasico_comparacion <- curva_sabr_1_multistart %>%
  mutate(modelo = "SABR Clásico")

curva_rough_comparacion <- curva_rough_sabr %>%
  rename(iv_sabr_forward = iv_rough_GA) %>%
  mutate(modelo = "Rough SABR")

curvas_comparacion <- bind_rows(
  curva_clasico_comparacion,
  curva_rough_comparacion
)

grafica_comparacion <- ggplot() +
  geom_point(
    data = base_rough_sabr_fit,
    aes(x = strike, y = iv_obs),
    color = "grey45",
    alpha = 0.18,
    size  = 1.0
  ) +
  geom_line(
    data = curvas_comparacion %>% arrange(modelo, bucket_venc, strike),
    aes(
      x         = strike,
      y         = iv_sabr_forward,
      color     = modelo,
      linetype  = modelo
    ),
    linewidth = 1.1
  ) +
  scale_color_manual(
    values = c(
      "SABR Clásico" = "#1b7837",
      "Rough SABR"   = "#762a83"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "SABR Clásico" = "solid",
      "Rough SABR"   = "dashed"
    )
  ) +
  facet_wrap(
    ~ bucket_venc,
    ncol   = 2,
    dir    = "h",
    scales = "free"
  ) +
  labs(
    title    = "Comparación: SABR Clásico vs Rough SABR",
    subtitle = "Opciones USD/MXN | beta = 1 | forward individual",
    x        = "Strike",
    y        = "Volatilidad implícita",
    color    = "Modelo",
    linetype = "Modelo"
  ) +
  theme_minimal() +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )

grafica_comparacion


# ============================================================
# 69) Guardar resultados y gráficas
# ============================================================

# Parámetros finales Rough SABR
parametros_rough_final <- tibble(
  parametro = c(
    paste0("alpha_", orden_buckets),
    "H", "rho", "eta"
  ),
  valor = c(alpha_rough_final, H_final, rho_final, eta_final)
)

write.csv(
  parametros_rough_final,
  file.path(ruta_salidas, "parametros_rough_SABR.csv"),
  row.names = FALSE
)

write.csv(
  errores_rough_sabr_GA,
  file.path(ruta_salidas, "errores_rough_SABR_GA.csv"),
  row.names = FALSE
)

write.csv(
  comparacion_modelos,
  file.path(ruta_salidas, "comparacion_SABR_clasico_vs_rough.csv"),
  row.names = FALSE
)

write.csv(
  validacion_ode,
  file.path(ruta_salidas, "validacion_GA_vs_ODE.csv"),
  row.names = FALSE
)

write.csv(
  verificacion_limites_ode,
  file.path(ruta_salidas, "verificacion_ODE_casos_limite.csv"),
  row.names = FALSE
)

# Gráficas PNG
ggsave(
  filename = file.path(ruta_graficas, "rough_SABR_ajuste.png"),
  plot     = grafica_rough_sabr,
  width = 12, height = 7, units = "in", dpi = 400, bg = "white"
)

ggsave(
  filename = file.path(ruta_graficas, "comparacion_SABR_clasico_rough.png"),
  plot     = grafica_comparacion,
  width = 12, height = 7, units = "in", dpi = 400, bg = "white"
)

# Gráficas PDF para LaTeX
ggsave(
  filename = file.path(ruta_graficas, "rough_SABR_ajuste.pdf"),
  plot     = grafica_rough_sabr,
  width = 12, height = 7, units = "in", device = cairo_pdf
)

ggsave(
  filename = file.path(ruta_graficas, "comparacion_SABR_clasico_rough.pdf"),
  plot     = grafica_comparacion,
  width = 12, height = 7, units = "in", device = cairo_pdf
)

cat("\nTodos los resultados guardados en:", ruta_graficas, "\n")
