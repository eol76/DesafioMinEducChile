
# ============================================================================
# TS: PARTICIONES TEMPORALES - DESAFÍO ABANDONO ESCOLAR
#
# Histórico: 2014-2020
# Foto de predicción: 2021
# Objetivo: predecir abandono_t1 para 2022
#
# Este script:
#   1. Define las particiones de entrenamiento y evaluación.
#   2. Construye los índices de las filas de cada partición.
#   3. Prepara los índices de la foto 2021.
#   4. Genera un archivo de control de las particiones.
# ============================================================================

cat("\n=== TS: particiones temporales del desafío ===\n")


# ---------------------------------------------------------------------------
# 1. CONFIGURACIÓN
# ---------------------------------------------------------------------------

TS <- PARAMS$training_strategy

ID  <- PARAMS$datos$id
PER <- PARAMS$datos$periodo
CLASE <- PARAMS$datos$clase

ANIO_FOTO <- PARAMS$desafio$anio_foto
ANIO_PRED <- PARAMS$desafio$anio_prediccion


# ---------------------------------------------------------------------------
# 2. VALIDAR LOS OBJETOS RECIBIDOS DE 01_FE_desafio.R
# ---------------------------------------------------------------------------

if (!exists("d")) {
  stop("No existe el objeto d. Revisá la ejecución de 01_FE_desafio.R.")
}

if (!exists("d_pred")) {
  stop("No existe el objeto d_pred. Revisá la ejecución de 01_FE_desafio.R.")
}

if (!(PER %in% names(d))) {
  stop("No se encuentra la variable temporal en el histórico: ", PER)
}

if (!(PER %in% names(d_pred))) {
  stop("No se encuentra la variable temporal en d_pred: ", PER)
}

if (!(CLASE %in% names(d))) {
  stop("No se encuentra la variable objetivo en el histórico: ", CLASE)
}

if (CLASE %in% names(d_pred)) {
  stop("d_pred no debe contener la variable objetivo.")
}

if (nrow(d) == 0 || nrow(d_pred) == 0) {
  stop("El histórico o la foto de predicción están vacíos.")
}


# ---------------------------------------------------------------------------
# 3. VALIDAR LOS AÑOS Y LA VARIABLE OBJETIVO
# ---------------------------------------------------------------------------

if (anyNA(d[[PER]]) || anyNA(d_pred[[PER]])) {
  stop("Hay valores faltantes en la variable temporal.")
}

if (any(d_pred[[PER]] != ANIO_FOTO)) {
  stop("d_pred contiene años distintos del año de la foto configurado.")
}

if (anyNA(d[[CLASE]])) {
  stop("Hay valores faltantes en la variable objetivo del histórico.")
}

valores_clase <- unique(d[[CLASE]])

if (!all(valores_clase %in% c(0, 1))) {
  stop("La variable objetivo debe contener únicamente valores 0 y 1.")
}

# Convertir la clase a entero para los modelos
d[, (CLASE) := as.integer(get(CLASE))]


# ---------------------------------------------------------------------------
# 4. FUNCIÓN PARA IDENTIFICAR FILAS POR AÑO
# ---------------------------------------------------------------------------

# Devuelve los índices de las filas de d correspondientes a uno o varios años.
# Se utiliza el nombre de período definido en el YAML.

anios <- function(desde, hasta = desde) {
  
  which(
    d[[PER]] >= desde &
      d[[PER]] <= hasta
  )
}


# ---------------------------------------------------------------------------
# 5. PARTICIONES PRINCIPALES
# ---------------------------------------------------------------------------

# Entrenamiento inicial
tr <- anios(
  TS$train[1],
  TS$train[2]
)

# Validación
va <- anios(TS$validate)

# Test
te <- anios(TS$test)

# Entrenamiento final
tf <- anios(
  TS$train_final[1],
  TS$train_final[2]
)


# ---------------------------------------------------------------------------
# 6. VALIDAR QUE LAS PARTICIONES TENGAN REGISTROS
# ---------------------------------------------------------------------------

if (length(tr) == 0) {
  stop("La partición de entrenamiento está vacía.")
}

if (length(va) == 0) {
  stop("La partición de validación está vacía.")
}

if (length(te) == 0) {
  stop("La partición de test está vacía.")
}

if (length(tf) == 0) {
  stop("La partición train_final está vacía.")
}


# ---------------------------------------------------------------------------
# 7. AÑOS ADICIONALES DE EVALUACIÓN
# ---------------------------------------------------------------------------

# Se mantienen los años adicionales definidos en el YAML.
# Estos años tienen etiquetas y permiten evaluar el comportamiento temporal.

filas <- c(
  setNames(list(te), as.character(TS$test)),
  setNames(
    lapply(TS$otros_anios, anios),
    as.character(TS$otros_anios)
  )
)

for (a in names(filas)) {
  if (length(filas[[a]]) == 0) {
    stop("No hay registros para el año de evaluación: ", a)
  }
}


# ---------------------------------------------------------------------------
# 8. ÍNDICES PARA LA PREDICCIÓN 2022
# ---------------------------------------------------------------------------

# La foto 2021 no tiene clase.
# Por lo tanto, no se incorpora a tr, va, te ni tf.
#
# El objeto pred contiene los índices de d_pred que deberán utilizarse
# posteriormente para generar las probabilidades de abandono en 2022.

pred <- seq_len(nrow(d_pred))

if (length(pred) == 0) {
  stop("No hay alumnos disponibles para predecir en 2022.")
}


# ---------------------------------------------------------------------------
# 9. CONTROL DE LAS PARTICIONES
# ---------------------------------------------------------------------------

fila_control <- function(particion, anios_txt, i) {
  
  data.table(
    particion = particion,
    anios = as.character(anios_txt),
    filas = length(i),
    tasa_abandono = mean(d[[CLASE]][i])
  )
}

control <- rbindlist(list(
  
  fila_control(
    "train",
    paste(TS$train, collapse = "-"),
    tr
  ),
  
  fila_control(
    "validate",
    TS$validate,
    va
  ),
  
  fila_control(
    "test",
    TS$test,
    te
  ),
  
  fila_control(
    "train_final",
    paste(TS$train_final, collapse = "-"),
    tf
  )
  
))

# Incorporar los años adicionales de evaluación

for (a in TS$otros_anios) {
  
  control <- rbind(
    control,
    fila_control(
      paste("otro", a),
      a,
      filas[[as.character(a)]]
    )
  )
}

# Agregar la foto de predicción.
# No tiene tasa de abandono porque no conocemos la clase.

control <- rbind(
  control,
  data.table(
    particion = "prediccion",
    anios = as.character(ANIO_FOTO),
    filas = length(pred),
    tasa_abandono = NA_real_
  )
)


# ---------------------------------------------------------------------------
# 10. GUARDAR CONTROL
# ---------------------------------------------------------------------------

fwrite(
  control,
  file.path(carpeta_exp, "control_particiones.csv")
)

print(control)


# ---------------------------------------------------------------------------
# 11. RESUMEN DE LOS OBJETOS DISPONIBLES
# ---------------------------------------------------------------------------

cat("\n=== PARTICIONES GENERADAS ===\n")

cat(
  "Entrenamiento:",
  format(length(tr), big.mark = "."),
  "filas\n"
)

cat(
  "Validación:",
  format(length(va), big.mark = "."),
  "filas\n"
)

cat(
  "Test:",
  format(length(te), big.mark = "."),
  "filas\n"
)

cat(
  "Entrenamiento final:",
  format(length(tf), big.mark = "."),
  "filas\n"
)

cat(
  "Alumnos para predecir en 2022:",
  format(length(pred), big.mark = "."),
  "\n"
)

cat("\nObjetos disponibles para los siguientes scripts:\n")

cat("- tr: índices de entrenamiento\n")
cat("- va: índices de validación\n")
cat("- te: índices de test\n")
cat("- tf: índices de entrenamiento final\n")
cat("- filas: índices por año de evaluación\n")
cat("- pred: índices de la foto 2021\n")
cat("- anios(): función para seleccionar años\n")
cat("- TS: configuración temporal\n")

cat("\n=== TS FINALIZADO ===\n")
