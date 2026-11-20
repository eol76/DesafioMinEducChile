
# ============================================================================
# EJECUTOR (R) - DESAFÍO ABANDONO ESCOLAR 2021-2022
# Chile - MINEDUC
#
# Ejecuta los scripts específicos del desafío:
#   01_FE_desafio.R
#   02_TS_desafio.R
#   03_metodos_desafio.R
#   04_evaluar_desafio.R
#
# Uso desde la carpeta principal:
#   Rscript R/0_EXE_desafio.R config_desafio.yml
#
# Uso desde RStudio:
#   setwd(".../abandono_chile")
#   source("R/0_EXE_desafio.R")
# ============================================================================


# ---------------------------------------------------------------------------
# 1. LIMPIAR EL ENTORNO
# ---------------------------------------------------------------------------

rm(list = ls())
gc()


# ---------------------------------------------------------------------------
# 2. IDENTIFICAR EL ARCHIVO DE CONFIGURACIÓN
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

archivo_config <- if (length(args) > 0) {
  args[1]
} else {
  "config_desafio.yml"
}

if (!file.exists(archivo_config)) {
  
  archivo_alternativo <- file.path("..", archivo_config)
  
  if (file.exists(archivo_alternativo)) {
    archivo_config <- archivo_alternativo
  } else {
    stop("No se encontró el archivo de configuración: ", archivo_config)
  }
}

archivo_config <- normalizePath(archivo_config, winslash = "/")

carpeta_base <- dirname(archivo_config)
carpeta_R <- file.path(carpeta_base, "R")

if (!dir.exists(carpeta_R)) {
  stop("No se encontró la carpeta R: ", carpeta_R)
}


# ---------------------------------------------------------------------------
# 3. CARGAR PAQUETES BÁSICOS
# ---------------------------------------------------------------------------

paquetes_base <- c("data.table", "yaml")

for (paquete in paquetes_base) {
  
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete)
  }
  
  suppressPackageStartupMessages(
    library(paquete, character.only = TRUE)
  )
}


# ---------------------------------------------------------------------------
# 4. LEER LA CONFIGURACIÓN
# ---------------------------------------------------------------------------

PARAMS <- yaml::yaml.load_file(archivo_config)

if (is.null(PARAMS$datos$archivo_prediccion)) {
  stop("Falta datos$archivo_prediccion en el YAML.")
}

if (is.null(PARAMS$desafio$anio_foto) ||
    is.null(PARAMS$desafio$anio_prediccion) ||
    is.null(PARAMS$desafio$presupuesto_usd)) {
  stop("Faltan parámetros del bloque desafio en el YAML.")
}


# ---------------------------------------------------------------------------
# 5. DEFINIR CARPETAS Y ARCHIVOS DE DATOS
# ---------------------------------------------------------------------------

carpeta_exp <- file.path(
  carpeta_base,
  PARAMS$experiment$exp_dir_r,
  PARAMS$experiment$experiment_code
)

dir.create(
  carpeta_exp,
  recursive = TRUE,
  showWarnings = FALSE
)

archivo_historico <- file.path(carpeta_base, PARAMS$datos$archivo)
archivo_prediccion <- file.path(carpeta_base, PARAMS$datos$archivo_prediccion)

if (!file.exists(archivo_historico)) {
  stop("No se encontró el histórico: ", archivo_historico)
}

if (!file.exists(archivo_prediccion)) {
  stop("No se encontró la foto de predicción: ", archivo_prediccion)
}


# ---------------------------------------------------------------------------
# 6. CONFIGURAR EJECUCIÓN
# ---------------------------------------------------------------------------

data.table::setDTthreads(percent = 65)

t_inicio <- Sys.time()

cronometro <- function(etapa) {
  
  minutos <- as.numeric(
    difftime(Sys.time(), t_inicio, units = "mins")
  )
  
  cat(sprintf(
    "\n[%s] %.1f minutos desde el inicio\n",
    etapa,
    minutos
  ))
}

cat("\n============================================\n")
cat("EXPERIMENTO:", PARAMS$experiment$experiment_code, "\n")
cat("Foto:", PARAMS$desafio$anio_foto, "\n")
cat("Año a predecir:", PARAMS$desafio$anio_prediccion, "\n")
cat("Presupuesto:", PARAMS$desafio$presupuesto_usd, "USD\n")
cat("Resultados:", carpeta_exp, "\n")
cat("============================================\n")


# ---------------------------------------------------------------------------
# 7. VERIFICAR PAQUETES PARA LOS MODELOS
# ---------------------------------------------------------------------------

paquetes_modelos <- c(
  "lightgbm",
  "mlrMBO",
  "DiceKriging"
)

for (paquete in paquetes_modelos) {
  
  if (!requireNamespace(paquete, quietly = TRUE)) {
    
    cat("\nInstalando:", paquete, "\n")
    
    install.packages(paquete)
    
    if (!requireNamespace(paquete, quietly = TRUE)) {
      stop("No se pudo instalar el paquete: ", paquete)
    }
  }
}


# ---------------------------------------------------------------------------
# 8. EJECUTAR FEATURE ENGINEERING
# ---------------------------------------------------------------------------

cat("\n========== ETAPA 1: FEATURE ENGINEERING ==========\n")

source(
  file.path(carpeta_R, "01_FE_desafio.R"),
  local = FALSE
)

cronometro("FE")

if (!exists("d") || !exists("d_pred")) {
  stop("01_FE_desafio.R no generó los objetos d y d_pred.")
}


# ---------------------------------------------------------------------------
# 9. EJECUTAR PARTICIONES TEMPORALES
# ---------------------------------------------------------------------------

cat("\n========== ETAPA 2: PARTICIONES TEMPORALES ==========\n")

source(
  file.path(carpeta_R, "02_TS_desafio.R"),
  local = FALSE
)

cronometro("TS")


# ---------------------------------------------------------------------------
# 10. ENTRENAR LOS MODELOS
# ---------------------------------------------------------------------------

cat("\n========== ETAPA 3: ENTRENAMIENTO ==========\n")

source(
  file.path(carpeta_R, "03_metodos_desafio.R"),
  local = FALSE
)

cronometro("Métodos")


# ---------------------------------------------------------------------------
# 11. EVALUAR LOS MODELOS
# ---------------------------------------------------------------------------

cat("\n========== ETAPA 4: EVALUACIÓN ==========\n")

source(
  file.path(carpeta_R, "04_evaluar_desafio.R"),
  local = FALSE
)

cronometro("Evaluación")


# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# 12. CALIBRAR Y DISTRIBUIR
# ---------------------------------------------------------------------------

cat("\n========== ETAPA 5: CALIBRACION Y DISTRIBUCION ==========\n")

source(
  file.path(carpeta_R, "05_calibrar_distribuir_desafio.R"),
  local = FALSE
)


cronometro("Calibracion")


# ---------------------------------------------------------------------------


# 12. FINALIZACIÓN
# ---------------------------------------------------------------------------

cat("\n============================================\n")
cat("EXPERIMENTO FINALIZADO\n")
cat("Código:", PARAMS$experiment$experiment_code, "\n")
cat("Resultados:", carpeta_exp, "\n")
cat("============================================\n")

