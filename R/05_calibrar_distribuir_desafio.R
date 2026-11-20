# -----------------------------------------------------------------------------
# 05. CALIBRACION DE PROBABILIDADES Y DISTRIBUCION OPTIMA DEL PRESUPUESTO
#
# Requiere que 0_EXE_desafio.R haya ejecutado previamente los scripts 01 a 04.
# Utiliza las predicciones historicas out-of-time del metodo 9 ya guardadas
# en el objeto puntajes. No vuelve a entrenar los modelos.
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("05. CALIBRACION Y DISTRIBUCION DEL PRESUPUESTO\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# 1. VERIFICACIONES INICIALES
# -----------------------------------------------------------------------------

objetos_necesarios <- c(
  "PARAMS", "carpeta_exp", "d", "y", "filas",
  "puntajes", "d_pred"
)

faltantes <- objetos_necesarios[
  !vapply(objetos_necesarios, exists, logical(1), inherits = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    "Faltan objetos necesarios. Ejecute primero 0_EXE_desafio.R. ",
    "Objetos faltantes: ",
    paste(faltantes, collapse = ", ")
  )
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Se necesita el paquete 'data.table'.")
}

library(data.table)

metodo9_nombre <- "9. Reentrenado año a año"

if (is.null(puntajes[[metodo9_nombre]])) {
  stop("No se encontraron las predicciones del método 9 en 'puntajes'.")
}

pred_hist <- puntajes[[metodo9_nombre]]

anios_calibracion <- c("2018", "2019", "2020")

if (!all(anios_calibracion %in% names(pred_hist))) {
  stop(
    "El método 9 no contiene predicciones para todos los años requeridos: ",
    paste(anios_calibracion, collapse = ", ")
  )
}

if (!all(anios_calibracion %in% names(filas))) {
  stop("El objeto 'filas' no contiene los índices de 2018, 2019 y 2020.")
}

if (length(y) != nrow(d)) {
  stop("La longitud de 'y' no coincide con la cantidad de filas de 'd'.")
}

if (!identical(as.integer(y), as.integer(d$abandono_t1))) {
  stop("'y' no coincide con d$abandono_t1. Se cancela para evitar desalineaciones.")
}

if (anyNA(y)) {
  stop("La variable objetivo contiene valores faltantes.")
}

if (!all(as.integer(y) %in% c(0L, 1L))) {
  stop("La variable objetivo debe estar codificada como 0 y 1.")
}

# -----------------------------------------------------------------------------
# 2. CONSTRUIR LA BASE DE PREDICCIONES HISTORICAS
# -----------------------------------------------------------------------------

lista_historica <- lapply(anios_calibracion, function(a) {
  
  indices <- filas[[a]]
  probabilidades <- as.numeric(pred_hist[[a]])
  
  if (length(indices) != length(probabilidades)) {
    stop(
      "La cantidad de predicciones no coincide con las filas del año ", a, "."
    )
  }
  
  if (anyNA(indices) || any(indices < 1L) || any(indices > nrow(d))) {
    stop("Hay índices inválidos en filas[[", a, "]].")
  }
  
  if (!all(d$agno[indices] == as.integer(a))) {
    stop("Los índices de filas[[", a, "]] no corresponden al año indicado.")
  }
  
  if (anyNA(probabilidades) ||
      any(!is.finite(probabilidades)) ||
      any(probabilidades < 0 | probabilidades > 1)) {
    stop("Hay probabilidades inválidas en las predicciones de ", a, ".")
  }
  
  data.table(
    anio = as.integer(a),
    id_alumno = d$id_alumno[indices],
    prob_original = probabilidades,
    abandono_real = as.integer(y[indices])
  )
})

historico <- rbindlist(lista_historica)

if (anyNA(historico$id_alumno)) {
  stop("Hay identificadores de alumnos faltantes en la base histórica.")
}

if (anyNA(historico$abandono_real)) {
  stop("Hay etiquetas reales faltantes en la base histórica.")
}

cat("Predicciones históricas recuperadas:\n")
print(historico[, .(
  registros = .N,
  abandonos = sum(abandono_real),
  tasa_abandono = mean(abandono_real)
), by = anio])

cat("\nTotal de registros históricos para calibración:",
    format(nrow(historico), big.mark = "."), "\n")

# -----------------------------------------------------------------------------
# 3. FUNCIONES DE CALIBRACION Y METRICAS
# -----------------------------------------------------------------------------

limitar_prob <- function(p) {
  pmin(pmax(as.numeric(p), 1e-6), 1 - 1e-6)
}

ajustar_platt <- function(probabilidades, resultados) {
  
  if (length(unique(resultados)) < 2L) {
    stop("No hay variación suficiente en la variable objetivo para ajustar Platt.")
  }
  
  x <- qlogis(limitar_prob(probabilidades))
  
  modelo <- glm(
    resultados ~ x,
    family = binomial(link = "logit")
  )
  
  if (!isTRUE(modelo$converged)) {
    stop("La regresión logística de calibración no convergió.")
  }
  
  modelo
}

aplicar_platt <- function(modelo, probabilidades) {
  
  x <- qlogis(limitar_prob(probabilidades))
  
  p <- predict(
    modelo,
    newdata = data.frame(x = x),
    type = "response"
  )
  
  pmin(pmax(as.numeric(p), 0), 1)
}

calcular_auc <- function(y_real, p) {
  
  y_real <- as.integer(y_real)
  p <- as.numeric(p)
  
  n1 <- sum(y_real == 1L)
  n0 <- sum(y_real == 0L)
  
  if (n1 == 0L || n0 == 0L) {
    return(NA_real_)
  }
  
  rangos <- rank(p, ties.method = "average")
  
  (sum(rangos[y_real == 1L]) - n1 * (n1 + 1) / 2) /
    (n1 * n0)
}

calcular_metricas <- function(y_real, p, nombre) {
  
  p_segura <- limitar_prob(p)
  
  data.table(
    conjunto = nombre,
    registros = length(y_real),
    abandonos = sum(y_real == 1L),
    tasa_real = mean(y_real),
    probabilidad_media = mean(p),
    auc = calcular_auc(y_real, p),
    brier = mean((p - y_real)^2),
    logloss = -mean(
      y_real * log(p_segura) +
        (1 - y_real) * log(1 - p_segura)
    )
  )
}

crear_tabla_calibracion <- function(y_real, p, conjunto) {
  
  dt <- data.table(
    probabilidad = as.numeric(p),
    abandono_real = as.integer(y_real)
  )
  
  dt[, decil := pmin(
    10L,
    pmax(1L, ceiling(rank(probabilidad, ties.method = "first") / .N * 10))
  )]
  
  dt[, .(
    registros = .N,
    probabilidad_media = mean(probabilidad),
    tasa_abandono_real = mean(abandono_real),
    abandonos = sum(abandono_real)
  ), by = decil][order(decil)][, conjunto := conjunto][]
}

# -----------------------------------------------------------------------------
# 4. VALIDACION TEMPORAL: CALIBRAR 2018-2019 Y EVALUAR 2020
# -----------------------------------------------------------------------------

historico[, prob_calibrada_validacion := NA_real_]

base_ajuste <- historico[anio %in% c(2018L, 2019L)]
base_validacion <- historico[anio == 2020L]

if (nrow(base_ajuste) == 0L || nrow(base_validacion) == 0L) {
  stop("No hay registros suficientes para la validación temporal.")
}

modelo_platt_validacion <- ajustar_platt(
  base_ajuste$prob_original,
  base_ajuste$abandono_real
)

p_validacion_calibrada <- aplicar_platt(
  modelo_platt_validacion,
  base_validacion$prob_original
)

metricas_validacion <- rbindlist(list(
  calcular_metricas(
    base_validacion$abandono_real,
    base_validacion$prob_original,
    "2020 sin calibrar"
  ),
  calcular_metricas(
    base_validacion$abandono_real,
    p_validacion_calibrada,
    "2020 calibrado con 2018-2019"
  )
))

tabla_bins_validacion <- crear_tabla_calibracion(
  base_validacion$abandono_real,
  p_validacion_calibrada,
  "Validacion 2020"
)

cat("\nResultados de la validación temporal:\n")
print(metricas_validacion)

cat("\nCoeficientes del calibrador de validación:\n")
print(coef(modelo_platt_validacion))

# -----------------------------------------------------------------------------
# 5. AJUSTE DEFINITIVO DE PLATT CON 2018, 2019 Y 2020
# -----------------------------------------------------------------------------

modelo_platt_final <- ajustar_platt(
  historico$prob_original,
  historico$abandono_real
)

historico[, prob_calibrada := aplicar_platt(
  modelo_platt_final,
  prob_original
)]

metricas_historicas <- rbindlist(list(
  calcular_metricas(
    historico$abandono_real,
    historico$prob_original,
    "2018-2020 sin calibrar"
  ),
  calcular_metricas(
    historico$abandono_real,
    historico$prob_calibrada,
    "2018-2020 calibrado"
  )
))

tabla_bins_historica <- crear_tabla_calibracion(
  historico$abandono_real,
  historico$prob_calibrada,
  "Historico 2018-2020"
)

metricas_calibracion <- rbindlist(
  list(metricas_validacion, metricas_historicas),
  use.names = TRUE,
  fill = TRUE
)

tabla_calibracion_bins <- rbindlist(
  list(tabla_bins_validacion, tabla_bins_historica),
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  metricas_calibracion,
  file.path(carpeta_exp, "metricas_calibracion.csv")
)

fwrite(
  tabla_calibracion_bins,
  file.path(carpeta_exp, "tabla_calibracion_bins.csv")
)

coeficientes_platt <- data.table(
  termino = names(coef(modelo_platt_final)),
  coeficiente = as.numeric(coef(modelo_platt_final))
)

fwrite(
  coeficientes_platt,
  file.path(carpeta_exp, "coeficientes_platt.csv")
)

cat("\nResultados de calibración histórica:\n")
print(metricas_historicas)

cat("\nCoeficientes del calibrador definitivo:\n")
print(coeficientes_platt)

# Guardar también las predicciones históricas y sus etiquetas reales.
fwrite(
  historico,
  file.path(carpeta_exp, "predicciones_historicas_calibradas.csv")
)

# -----------------------------------------------------------------------------
# 6. LEER Y CALIBRAR LAS PREDICCIONES DE 2022
# -----------------------------------------------------------------------------

archivo_pred_2022 <- file.path(
  carpeta_exp,
  "predicciones_2022_modelo9.csv"
)

if (!file.exists(archivo_pred_2022)) {
  stop(
    "No se encontró el archivo de predicciones 2022: ",
    archivo_pred_2022
  )
}

pred_2022 <- fread(archivo_pred_2022)

columnas_requeridas <- c("id_alumno", "prob_abandono_2022")

if (!all(columnas_requeridas %in% names(pred_2022))) {
  stop(
    "El archivo de predicciones 2022 debe contener las columnas: ",
    paste(columnas_requeridas, collapse = ", ")
  )
}

if (anyNA(pred_2022$id_alumno) ||
    anyDuplicated(pred_2022$id_alumno) > 0L) {
  stop("Los identificadores de alumnos de 2022 son faltantes o están duplicados.")
}

if (anyNA(pred_2022$prob_abandono_2022) ||
    any(!is.finite(pred_2022$prob_abandono_2022)) ||
    any(pred_2022$prob_abandono_2022 < 0 |
        pred_2022$prob_abandono_2022 > 1)) {
  stop("El archivo 2022 contiene probabilidades inválidas.")
}

id_param <- PARAMS$datos$id

if (is.null(id_param) || !id_param %in% names(d_pred)) {
  stop("No se pudo identificar la columna de ID de alumno en d_pred.")
}

if (nrow(pred_2022) != nrow(d_pred)) {
  stop("La cantidad de predicciones 2022 no coincide con las filas de d_pred.")
}

if (!identical(
  as.character(pred_2022$id_alumno),
  as.character(d_pred[[id_param]])
)) {
  stop(
    "Los IDs del archivo 2022 no coinciden, en el mismo orden, ",
    "con los IDs de d_pred. Se cancela para evitar asignaciones incorrectas."
  )
}

pred_2022[, prob_calibrada := aplicar_platt(
  modelo_platt_final,
  prob_abandono_2022
)]

cat("\nPredicciones 2022 calibradas:\n")
cat("Registros:", format(nrow(pred_2022), big.mark = "."), "\n")
cat("Probabilidad original media:", mean(pred_2022$prob_abandono_2022), "\n")
cat("Probabilidad calibrada media:", mean(pred_2022$prob_calibrada), "\n")

# -----------------------------------------------------------------------------
# 7. DISTRIBUCION OPTIMA DEL PRESUPUESTO
#
# Beneficio esperado por alumno:
#   p_i * 0.6 * (1 - exp(-m_i / 200))
#
# Condicion de primer orden:
#   m_i = max(0, 200 * log(0.003 * p_i / lambda))
#
# Se busca lambda para que la suma de los montos sea igual al presupuesto.
# -----------------------------------------------------------------------------

presupuesto <- as.numeric(PARAMS$desafio$presupuesto_usd)

if (length(presupuesto) != 1L ||
    is.na(presupuesto) ||
    !is.finite(presupuesto) ||
    presupuesto <= 0) {
  stop("El presupuesto definido en PARAMS$desafio$presupuesto_usd no es válido.")
}

p <- as.numeric(pred_2022$prob_calibrada)

if (length(p) == 0L || anyNA(p) || any(!is.finite(p)) ||
    any(p < 0 | p > 1)) {
  stop("Las probabilidades calibradas para 2022 no son válidas.")
}

if (!any(p > 0)) {
  stop("Todas las probabilidades calibradas son cero; no se puede distribuir el presupuesto.")
}

monto_lambda <- function(lambda, probabilidades) {
  if (lambda <= 0) {
    return(rep(Inf, length(probabilidades)))
  }
  
  montos <- numeric(length(probabilidades))
  elegibles <- probabilidades > 0
  
  montos[elegibles] <- pmax(
    0,
    200 * log(0.003 * probabilidades[elegibles] / lambda)
  )
  
  montos
}

lambda_sup <- 0.003 * max(p)
lambda_inf <- lambda_sup * 1e-15

for (i in seq_len(200L)) {
  
  lambda_med <- (lambda_inf + lambda_sup) / 2
  suma_med <- sum(monto_lambda(lambda_med, p))
  
  if (suma_med > presupuesto) {
    lambda_inf <- lambda_med
  } else {
    lambda_sup <- lambda_med
  }
}

lambda_optimo <- (lambda_inf + lambda_sup) / 2
montos_sin_redondear <- monto_lambda(lambda_optimo, p)

# Redondeo hacia abajo a centavos para no exceder el presupuesto.
montos <- floor(montos_sin_redondear * 100) / 100

# Distribuir los centavos residuales entre los alumnos con mayor resto decimal.
centavos_restantes <- as.integer(round(
  (presupuesto - sum(montos)) * 100
))

if (centavos_restantes < 0L) {
  stop("El redondeo produjo un gasto superior al presupuesto.")
}

if (centavos_restantes > 0L) {
  
  restos <- montos_sin_redondear * 100 -
    floor(montos_sin_redondear * 100)
  
  orden <- order(restos, decreasing = TRUE)
  
  if (centavos_restantes > length(orden)) {
    stop(
      "No se pudieron distribuir los centavos residuales sin exceder ",
      "el presupuesto. Revise la precisión numérica."
    )
  }
  
  montos[orden[seq_len(centavos_restantes)]] <-
    montos[orden[seq_len(centavos_restantes)]] + 0.01
}

if (sum(montos) > presupuesto + 1e-8) {
  stop("La distribución final supera el presupuesto disponible.")
}

# Beneficio esperado según la función definida en el desafío.
beneficio_esperado <- p * 0.6 * (1 - exp(-montos / 200))

entrega <- data.table(
  id_alumno = pred_2022$id_alumno,
  prob_abandono_original = pred_2022$prob_abandono_2022,
  prob_abandono_calibrada = pred_2022$prob_calibrada,
  monto_usd = montos,
  beneficio_esperado = beneficio_esperado
)

# -----------------------------------------------------------------------------
# 8. RESUMEN Y GUARDADO DE RESULTADOS
# -----------------------------------------------------------------------------

archivo_entrega <- file.path(
  carpeta_exp,
  "distribucion_presupuesto_2022.csv"
)

fwrite(entrega, archivo_entrega)

# Guardar también la tabla de predicciones 2022 con ambas probabilidades.
fwrite(
  pred_2022,
  file.path(carpeta_exp, "predicciones_2022_calibradas.csv")
)

resumen <- data.table(
  concepto = c(
    "Alumnos evaluados",
    "Presupuesto disponible (USD)",
    "Presupuesto asignado (USD)",
    "Presupuesto sin asignar (USD)",
    "Alumnos con asignación",
    "Asignación media por alumno (USD)",
    "Asignación máxima (USD)",
    "Beneficio esperado total"
  ),
  valor = c(
    nrow(entrega),
    presupuesto,
    sum(entrega$monto_usd),
    presupuesto - sum(entrega$monto_usd),
    sum(entrega$monto_usd > 0),
    mean(entrega$monto_usd),
    max(entrega$monto_usd),
    sum(entrega$beneficio_esperado)
  )
)

fwrite(
  resumen,
  file.path(carpeta_exp, "resumen_distribucion_presupuesto.csv")
)

cat("\n============================================================\n")
cat("RESUMEN DE LA DISTRIBUCION DEL PRESUPUESTO\n")
cat("============================================================\n")
print(resumen)

cat("\nPrimeros registros de la distribución:\n")
print(head(entrega, 10))

cat("\nArchivos generados en:\n")
cat(normalizePath(carpeta_exp, winslash = "/", mustWork = FALSE), "\n")

cat("\n- metricas_calibracion.csv\n")
cat("- tabla_calibracion_bins.csv\n")
cat("- coeficientes_platt.csv\n")
cat("- predicciones_historicas_calibradas.csv\n")
cat("- predicciones_2022_calibradas.csv\n")
cat("- distribucion_presupuesto_2022.csv\n")
cat("- resumen_distribucion_presupuesto.csv\n")

cat("\nProceso 05 finalizado.\n")
