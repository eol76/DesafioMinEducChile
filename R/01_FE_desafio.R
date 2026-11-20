
# ============================================================================
# FE: historia de cada alumno + preparación de la foto 2021
# Datos históricos: 2014-2020
# Foto de predicción: 2021
# Objetivo: predecir abandono_t1 para el año 2022
# ============================================================================

cat("\n=== FE: leyendo datos históricos y foto 2021 ===\n")


# ---------------------------------------------------------------------------
# 1. LEER LOS DOS ARCHIVOS
# ---------------------------------------------------------------------------

# Este script está diseñado para ejecutarse desde 0_EXE_desafio.R.
# El ejecutable ya construye las rutas completas a partir de carpeta_base.
# Por eso usamos esas rutas y no rutas relativas al directorio de trabajo
# (que normalmente es la carpeta R).

if (!exists("PARAMS", inherits = TRUE)) {
  stop("No existe PARAMS. Ejecutá este script desde 0_EXE_desafio.R.")
}

if (!exists("carpeta_base", inherits = TRUE)) {
  stop("No existe carpeta_base. Ejecutá este script desde 0_EXE_desafio.R.")
}

# Recuperar las rutas completas definidas por 0_EXE_desafio.R.
# Si no estuvieran disponibles, construirlas desde carpeta_base.

if (exists("archivo_historico", inherits = TRUE)) {
  ruta_historico <- get("archivo_historico", inherits = TRUE)
} else {
  ruta_historico <- file.path(carpeta_base, PARAMS$datos$archivo)
}

if (exists("archivo_prediccion", inherits = TRUE)) {
  ruta_prediccion <- get("archivo_prediccion", inherits = TRUE)
} else {
  ruta_prediccion <- file.path(
    carpeta_base,
    PARAMS$datos$archivo_prediccion
  )
}

ruta_historico <- normalizePath(
  ruta_historico,
  winslash = "/",
  mustWork = FALSE
)

ruta_prediccion <- normalizePath(
  ruta_prediccion,
  winslash = "/",
  mustWork = FALSE
)

cat("Ruta del histórico: ", ruta_historico, "\\n", sep = "")
cat("Ruta de la foto 2021: ", ruta_prediccion, "\\n", sep = "")

if (!file.exists(ruta_historico)) {
  stop("No se encontró el archivo histórico: ", ruta_historico)
}

if (!file.exists(ruta_prediccion)) {
  stop("No se encontró el archivo de predicción: ", ruta_prediccion)
}

d_hist <- data.table::fread(ruta_historico)
d_pred <- data.table::fread(ruta_prediccion)

cat("Histórico leído correctamente: ", nrow(d_hist), " registros\\n", sep = "")
cat("Foto 2021 leída correctamente: ", nrow(d_pred), " registros\\n", sep = "")

ID    <- PARAMS$datos$id
ESC   <- PARAMS$datos$escuela
PER   <- PARAMS$datos$periodo
CLASE <- PARAMS$datos$clase

anio_pred <- PARAMS$desafio$anio_foto


# ---------------------------------------------------------------------------
# 2. VALIDAR LA ESTRUCTURA DE LOS ARCHIVOS
# ---------------------------------------------------------------------------

columnas_base <- c(ID, ESC, PER, "sit_fin")

faltan_hist <- setdiff(columnas_base, names(d_hist))
faltan_pred <- setdiff(columnas_base, names(d_pred))

if (length(faltan_hist) > 0) {
  stop(
    "Faltan columnas en el histórico: ",
    paste(faltan_hist, collapse = ", ")
  )
}

if (length(faltan_pred) > 0) {
  stop(
    "Faltan columnas en la foto 2021: ",
    paste(faltan_pred, collapse = ", ")
  )
}

# Verificar que el archivo de predicción corresponda al año configurado
if (any(d_pred[[PER]] != anio_pred, na.rm = TRUE)) {
  stop("El archivo de predicción contiene años distintos del año_foto.")
}

# Verificar que no existan alumnos duplicados por año
if (anyDuplicated(d_hist, by = c(ID, PER)) > 0) {
  stop("Hay alumnos duplicados por año en el archivo histórico.")
}

if (anyDuplicated(d_pred, by = c(ID, PER)) > 0) {
  stop("Hay alumnos duplicados en la foto de predicción.")
}

# El histórico debe contener la variable objetivo
if (!(CLASE %in% names(d_hist))) {
  stop("No se encontró la variable objetivo en el histórico.")
}

# La foto 2021 no debe utilizar la variable objetivo
if (CLASE %in% names(d_pred)) {
  d_pred[, (CLASE) := NULL]
}


# ---------------------------------------------------------------------------
# 3. VERIFICAR LA VARIABLE DE EGRESO
# ---------------------------------------------------------------------------

# Si se excluyen egresados, la variable debe estar disponible en ambos archivos.
# No se eliminan registros con egresa desconocido silenciosamente.

if (isTRUE(PARAMS$datos$sacar_egresados)) {
  
  if (!("egresa" %in% names(d_hist))) {
    stop("No se encontró egresa en el archivo histórico.")
  }
  
  if (!("egresa" %in% names(d_pred))) {
    stop("No se encontró egresa en la foto 2021. Revisá el archivo antes de continuar.")
  }
  
  if (anyNA(d_hist$egresa)) {
    stop("Hay valores faltantes en egresa del histórico.")
  }
  
  if (anyNA(d_pred$egresa)) {
    stop("Hay valores faltantes en egresa de la foto 2021.")
  }
}


# ---------------------------------------------------------------------------
# 4. MARCAR EL ORIGEN Y UNIR TEMPORALMENTE LOS DATOS
# ---------------------------------------------------------------------------

d_hist[, es_prediccion := FALSE]
d_pred[, es_prediccion := TRUE]

# La unión permite calcular las variables históricas de los alumnos de 2021
# utilizando sus antecedentes de años anteriores.

d <- rbindlist(
  list(d_hist, d_pred),
  use.names = TRUE,
  fill = TRUE
)

rm(d_hist, d_pred)

# Ordenar por alumno y año
setorderv(d, c(ID, PER))


# ---------------------------------------------------------------------------
# 5. RECODIFICAR LA SITUACIÓN FINAL
# ---------------------------------------------------------------------------

# P = promovido
# R = reprobado
# Y = retirado

valores_sit_fin <- c(P = 0L, R = 1L, Y = 2L)

sit <- as.character(d[["sit_fin"]])

if (all(is.na(sit) | sit %in% names(valores_sit_fin))) {
  
  d[, sit_fin := unname(valores_sit_fin[sit])]
  
} else {
  
  # Si los valores ya son numéricos, se conservan como tales
  d[, sit_fin := as.integer(as.character(sit_fin))]
}


# ---------------------------------------------------------------------------
# 6. CONSTRUIR LAS VARIABLES DE FEATURE ENGINEERING
# ---------------------------------------------------------------------------

FE <- PARAMS$feature_engineering

nuevas <- character(0)


# ...........................................................................
# 6.1. Variables del año anterior (lag_1)
# ...........................................................................

# Sólo se utiliza el registro anterior cuando corresponde al año inmediato
# anterior del mismo alumno.

d[, seguido := shift(get(PER)) == get(PER) - 1, by = ID]

for (v in FE$lag_1) {
  
  nv <- paste0(v, "_lag1")
  
  d[, (nv) := fifelse(
    seguido,
    shift(get(v)),
    NA_real_
  ), by = ID]
  
  nuevas <- c(nuevas, nv)
}


# ...........................................................................
# 6.2. Variaciones respecto del año anterior (delta)
# ...........................................................................

for (v in FE$delta) {
  
  nv <- paste0(v, "_delta1")
  
  d[, (nv) := get(v) - get(paste0(v, "_lag1"))]
  
  nuevas <- c(nuevas, nv)
}


# ...........................................................................
# 6.3. Historia del alumno
# ...........................................................................

if (isTRUE(FE$historia)) {
  
  # Cantidad de repitencias anteriores
  d[, repitencias_previas :=
      shift(cumsum(sit_fin == 1L), fill = 0L),
    by = ID]
  
  # Cantidad de años anteriores registrados
  d[, anios_previos := seq_len(.N) - 1L, by = ID]
  
  # Si cambió de escuela respecto del año anterior
  d[, cambio_esc_entre := fifelse(
    seguido,
    as.numeric(shift(get(ESC)) != get(ESC)),
    NA_real_
  ), by = ID]
  
  nuevas <- c(
    nuevas,
    "repitencias_previas",
    "anios_previos",
    "cambio_esc_entre"
  )
}


# ...........................................................................
# 6.4. Sobreedad
# ...........................................................................

if (isTRUE(FE$sobreedad)) {
  
  # Diferencia entre la edad del alumno y la mediana de su grupo:
  # mismo año, enseñanza y grado.
  
  d[, sobreedad :=
      edad - median(edad, na.rm = TRUE),
    by = c(PER, "cod_ense2", "cod_grado")]
  
  nuevas <- c(nuevas, "sobreedad")
}


# ...........................................................................
# 6.5. Comparación del alumno con su escuela
# ...........................................................................

if (isTRUE(FE$vs_escuela)) {
  
  d[, `:=`(
    prom_vs_esc = prom_gral - esc_prom,
    asist_vs_esc = asistencia - esc_asist
  )]
  
  nuevas <- c(
    nuevas,
    "prom_vs_esc",
    "asist_vs_esc"
  )
}


# Eliminar variable auxiliar
d[, seguido := NULL]


# ---------------------------------------------------------------------------
# 7. EXCLUIR LOS ALUMNOS EGRESADOS
# ---------------------------------------------------------------------------

# Se aplica el mismo criterio tanto al histórico como a la foto 2021.

if (isTRUE(PARAMS$datos$sacar_egresados)) {
  
  d <- d[egresa == 0]
}


# ---------------------------------------------------------------------------
# 8. SEPARAR HISTÓRICO Y FOTO DE PREDICCIÓN
# ---------------------------------------------------------------------------

# d: registros históricos con variable objetivo.
# d_pred: alumnos de 2021 sin variable objetivo.

d_pred <- d[es_prediccion == TRUE]
d <- d[es_prediccion == FALSE]

# Eliminar la marca auxiliar
d[, es_prediccion := NULL]
d_pred[, es_prediccion := NULL]


# Verificar nuevamente que la foto no tenga la clase
if (CLASE %in% names(d_pred)) {
  d_pred[, (CLASE) := NULL]
}


# ---------------------------------------------------------------------------
# 9. DEFINIR LAS VARIABLES QUE UTILIZARÁN LOS MODELOS
# ---------------------------------------------------------------------------

crudas <- PARAMS$variables_crudas

con_fe <- c(crudas, nuevas)

# Verificar que todas las variables estén disponibles en ambos conjuntos

faltan_modelo_hist <- setdiff(con_fe, names(d))
faltan_modelo_pred <- setdiff(con_fe, names(d_pred))

if (length(faltan_modelo_hist) > 0) {
  
  stop(
    "Faltan variables para el modelo en el histórico: ",
    paste(faltan_modelo_hist, collapse = ", ")
  )
}

if (length(faltan_modelo_pred) > 0) {
  
  stop(
    "Faltan variables para el modelo en la foto 2021: ",
    paste(faltan_modelo_pred, collapse = ", ")
  )
}

if (!(CLASE %in% names(d))) {
  stop("No se encuentra la variable objetivo en el histórico.")
}

if (CLASE %in% names(d_pred)) {
  stop("La foto de predicción contiene la variable objetivo.")
}


# ---------------------------------------------------------------------------
# 10. RESUMEN DEL PROCESAMIENTO
# ---------------------------------------------------------------------------

cat("\n=== RESULTADO DEL FEATURE ENGINEERING ===\n")

cat(
  "Registros históricos:",
  format(nrow(d), big.mark = "."),
  "\n"
)

cat(
  "Alumnos para predecir en 2022:",
  format(nrow(d_pred), big.mark = "."),
  "\n"
)

cat(
  "Variables crudas:",
  length(crudas),
  "\n"
)

cat(
  "Variables nuevas de FE:",
  length(nuevas),
  "\n"
)

cat(
  "Total de variables explicativas:",
  length(con_fe),
  "\n"
)

cat("\nObjetos disponibles:\n")
cat("- d: datos históricos para entrenamiento\n")
cat("- d_pred: foto 2021 para predicción\n")
cat("- CLASE: variable objetivo\n")
cat("- crudas: variables originales\n")
cat("- nuevas: variables construidas\n")
cat("- con_fe: conjunto completo de variables explicativas\n")

cat("\n=== FE FINALIZADO ===\n")
