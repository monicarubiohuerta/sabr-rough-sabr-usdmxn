# ============================================================
# Configuración de rutas del proyecto
# ============================================================
# Todas las rutas son relativas a la carpeta raíz del repositorio.
# Abrir el proyecto con el directorio de trabajo en esa carpeta
# (por ejemplo, con setwd() o desde un proyecto de RStudio).
#
# Los datos de entrada NO se incluyen en el repositorio por ser
# información confidencial. Para reproducir el ejercicio se deben
# colocar en las siguientes carpetas:
#
#   datos/OPTOS_EXOTIC_202412.csv        Operaciones OTC de opciones al 31/12/2024
#   datos/curvas/OISTIIE_20241231.csv    Curva OIS MXN (TIIE de Fondeo)
#   datos/curvas/OIS_USD_20241231.csv    Curva OIS USD
# ============================================================

ruta_bases    <- "datos"
ruta_curvas   <- file.path("datos", "curvas")
ruta_graficas <- "graficas"
ruta_salidas  <- "salidas"

dir.create(ruta_graficas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_salidas,  recursive = TRUE, showWarnings = FALSE)
