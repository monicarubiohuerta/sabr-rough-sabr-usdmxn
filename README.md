# Calibración del *smile* de volatilidad implícita en opciones USD/MXN: SABR clásico vs. *rough SABR*

Código en R de la tesina:

> Rubio Huerta, Mónica (2026). *Calibración del smile de volatilidad implícita en opciones USD/MXN: una comparación entre SABR clásico y rough SABR*. Tesina de Licenciatura en Actuaría, Instituto Tecnológico Autónomo de México (ITAM). Asesor: Dr. Oscar Peralta Gutiérrez.

## Descripción

El código calibra dos modelos sobre la volatilidad implícita reportada en operaciones OTC de opciones europeas *call* y *put* sobre USD/MXN vigentes al 31 de diciembre de 2024:

- **SABR clásico** con β = 1, usando la aproximación de Hagan et al. (2002). Se calibra por separado en cuatro intervalos de vencimiento (0–30, 31–90, 91–180 y 181–365 días), con forwards individuales por operación y una estrategia *multistart* de 48 puntos iniciales por intervalo.
- ***Rough SABR***, usando la aproximación cerrada $G_A$ de Fukasawa y Gatheral (2021). Se calibra de forma conjunta con un nivel $\alpha_0$ por intervalo y parámetros de forma globales $(H, \rho, \eta)$, lo que da 7 parámetros, y una estrategia *multistart* de 144 puntos iniciales. La aproximación se valida contra la solución numérica de la ODE con `deSolve`.

## Estructura

```
R/
├── 00_configuracion.R                  Rutas relativas del proyecto
├── 01_preparacion_muestra_SABR_spot.R  Lectura, filtros y limpieza de la muestra; SABR con F ≈ spot (diagnóstico)
├── 02_curvas_construccion_forwards.R   Curvas OIS MXN/USD, factores de descuento y forwards USD/MXN
├── 03_SABR_clasico_forward.R           SABR clásico con forward individual por operación
├── 04_EDA_OTC_exotic.R                 Análisis exploratorio y gráficas de la muestra
├── 05_multistart_SABR_clasico.R        Calibración multistart de SABR clásico
└── 06_rough_SABR.R                     Calibración de rough SABR (G_A) y validación con la ODE
graficas/                               Gráficas incluidas en la tesina
```

Los scripts se corren **en orden y en una misma sesión de R**, porque cada uno usa objetos creados por los anteriores. El directorio de trabajo debe ser la carpeta raíz del repositorio.

## Datos

**Los datos no se incluyen en este repositorio.** Las operaciones OTC son información confidencial y las curvas OIS son información de un proveedor de precios. En la tesina, los resultados se presentan solo de manera agregada.

Para reproducir el ejercicio con datos propios, deben colocarse en:

```
datos/OPTOS_EXOTIC_202412.csv
datos/curvas/OISTIIE_20241231.csv
datos/curvas/OIS_USD_20241231.csv
```

Como tipo de cambio spot se utiliza el FIX determinado por Banco de México el 31/12/2024 (serie SF43718): $S_0 = 20.7862$.

## Paquetes de R

`dplyr`, `tidyr`, `purrr`, `tibble`, `readr`, `readxl`, `janitor`, `lubridate`, `stringr`, `ggplot2`, `viridis`, `scales`, `tidyverse`, `deSolve`

```r
install.packages(c("tidyverse", "readxl", "janitor", "viridis", "scales", "deSolve"))
```

## Referencias principales

- Fukasawa, M. y Gatheral, J. (2021). A Rough SABR Formula. arXiv:2105.05359.
- Gatheral, J., Jaisson, T. y Rosenbaum, M. (2018). Volatility Is Rough. *Quantitative Finance*, 18(6), 933–949.
- Hagan, P. S., Kumar, D., Lesniewski, A. S. y Woodward, D. E. (2002). Managing Smile Risk. *Wilmott Magazine*.
