# ============================================================================
# 03_METODOS_DESAFIO.R
#
# Entrena y compara nueve métodos para predecir abandono escolar.
# Utiliza:
#   d       = histórico 2014-2020, con variable objetivo
#   d_pred  = foto de alumnos 2021, sin variable objetivo
#
# La predicción final corresponde al abandono esperado en 2022.
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(rpart)
  library(ranger)
  library(lightgbm)
  library(mlrMBO)
  library(DiceKriging)
})

cat("\n=== INICIO DE LOS MÉTODOS ===\n")

# ----------------------------------------------------------------------------
# 0. VALIDACIONES INICIALES
# ----------------------------------------------------------------------------

objetos_necesarios <- c(
  "d", "d_pred", "PARAMS", "CLASE",
  "crudas", "con_fe", "nuevas",
  "tr", "va", "te", "tf",
  "filas", "pred", "TS", "anios",
  "carpeta_exp"
)

faltantes <- objetos_necesarios[
  !vapply(objetos_necesarios, exists, logical(1), inherits = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    "Faltan objetos necesarios. Revisá la ejecución de 01_FE_desafio.R ",
    "y 02_TS_desafio.R. Objetos faltantes: ",
    paste(faltantes, collapse = ", ")
  )
}

if (!CLASE %in% names(d)) {
  stop("No se encuentra la variable objetivo ", CLASE, " en el histórico.")
}

if (!all(con_fe %in% names(d))) {
  stop("Hay variables de con_fe que no existen en el histórico.")
}

if (!all(con_fe %in% names(d_pred))) {
  stop("Hay variables de con_fe que no existen en d_pred.")
}

if (length(pred) != nrow(d_pred)) {
  stop("El vector pred no coincide con la cantidad de filas de d_pred.")
}

# Variable objetivo histórica
y <- as.integer(as.character(d[[CLASE]]))

if (anyNA(y) || !all(y %in% c(0L, 1L))) {
  stop("La variable objetivo histórica debe contener solamente 0 y 1, sin NA.")
}

SEM <- PARAMS$metodos$semilla

puntajes <- list()
segundos <- list()
importancia <- NULL

# ----------------------------------------------------------------------------
# FUNCIONES GENERALES
# ----------------------------------------------------------------------------

auc <- function(score, y) {
  
  ok <- !is.na(score) & !is.na(y)
  score <- score[ok]
  y <- y[ok]
  
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  
  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }
  
  r <- frank(score, ties.method = "average")
  
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}


correr <- function(nombre, f) {
  
  cat("\n", nombre, "...\n", sep = "")
  
  t0 <- Sys.time()
  
  puntajes[[nombre]] <<- f()
  
  segundos[[nombre]] <<- round(
    as.numeric(difftime(Sys.time(), t0, units = "secs")),
    1
  )
  
  pred_test <- puntajes[[nombre]][[as.character(TS$test)]]
  
  auc_test <- auc(pred_test, y[te])
  
  cat(
    sprintf(
      "  AUC test %.4f (%s segundos)\n",
      auc_test,
      segundos[[nombre]]
    )
  )
}


# Predice todos los años históricos incluidos en filas
predecir_todos <- function(f) {
  lapply(filas, f)
}


# Toma una muestra reproducible para métodos más lentos
muestra <- function(idx, n) {
  
  set.seed(SEM)
  
  if (length(idx) <= n) {
    idx
  } else {
    sample(idx, n)
  }
}


# Matrices para LightGBM
matriz <- function(i, cols) {
  data.matrix(d[i, ..cols])
}


matriz_pred <- function(i, cols) {
  data.matrix(d_pred[i, ..cols])
}


# Imputación sencilla para árboles y random forest
imputar <- function(i, cols) {
  
  x <- as.data.frame(d[i, ..cols])
  
  x[is.na(x)] <- -1
  
  x
}


# ----------------------------------------------------------------------------
# 1. REGLA SENCILLA: MENOR ASISTENCIA = MAYOR RIESGO
# ----------------------------------------------------------------------------

cat("\n1. Regla sencilla: menor asistencia = mayor riesgo\n")

correr(
  "1. Regla: la asistencia",
  function() {
    predecir_todos(function(i) -d$asistencia[i])
  }
)


# ----------------------------------------------------------------------------
# 2. REGRESIÓN LOGÍSTICA
# ----------------------------------------------------------------------------
#
# Corrección:
# - Excluye variables categóricas con menos de dos niveles en TRAIN.
# - Imputa categorías faltantes o desconocidas con la moda de TRAIN.
# - Imputa variables numéricas con la mediana de TRAIN.
# - Mantiene los mismos niveles de factores al predecir.
# ----------------------------------------------------------------------------

logistica <- function(extra = character(0)) {
  
  cat("\n  Preparando variables para regresión logística...\n")
  
  cols_iniciales <- c(
    setdiff(crudas, PARAMS$no_logistica),
    extra
  )
  
  cols_iniciales <- unique(cols_iniciales)
  
  cat_iniciales <- intersect(
    PARAMS$categoricas_logistica,
    cols_iniciales
  )
  
  num_iniciales <- setdiff(
    cols_iniciales,
    cat_iniciales
  )
  
  info_cat <- list()
  excluir_cat <- character(0)
  
  # Identificar categorías presentes en TRAIN
  for (v in cat_iniciales) {
    
    valores <- as.character(d[[v]][tr])
    
    niveles <- sort(
      unique(valores[!is.na(valores)])
    )
    
    # Si tiene una sola categoría, se excluye
    if (length(niveles) < 2) {
      
      excluir_cat <- c(excluir_cat, v)
      
      cat(
        "  Variable excluida por tener menos de dos categorías:",
        v, "\n"
      )
      
    } else {
      
      frecuencias <- table(
        valores[!is.na(valores)]
      )
      
      moda <- names(frecuencias)[which.max(frecuencias)]
      
      info_cat[[v]] <- list(
        niveles = niveles,
        moda = moda
      )
    }
  }
  
  cols <- setdiff(
    cols_iniciales,
    excluir_cat
  )
  
  cat_cols <- names(info_cat)
  
  num_cols <- setdiff(
    cols,
    cat_cols
  )
  
  # Medianas calculadas exclusivamente con TRAIN
  if (length(num_cols) > 0) {
    
    medianas <- d[
      tr,
      lapply(.SD, function(x) {
        
        med <- median(x, na.rm = TRUE)
        
        if (!is.finite(med)) {
          med <- 0
        }
        
        med
      }),
      .SDcols = num_cols
    ]
    
  } else {
    
    medianas <- data.table()
  }
  
  
  # Preparar los datos para entrenamiento o predicción
  armar <- function(i) {
    
    x <- copy(d[i, ..cols])
    
    # Imputar variables numéricas
    for (v in num_cols) {
      
      faltantes <- which(is.na(x[[v]]))
      
      if (length(faltantes) > 0) {
        
        set(
          x,
          i = faltantes,
          j = v,
          value = medianas[[v]]
        )
      }
    }
    
    # Imputar categóricas y fijar niveles
    for (v in cat_cols) {
      
      niveles <- info_cat[[v]]$niveles
      moda <- info_cat[[v]]$moda
      
      valores <- as.character(x[[v]])
      
      invalidos <- is.na(valores) |
        !(valores %in% niveles)
      
      valores[invalidos] <- moda
      
      set(
        x,
        j = v,
        value = factor(valores, levels = niveles)
      )
    }
    
    x
  }
  
  
  xtr <- armar(tr)
  
  xtr[, clase := as.integer(y[tr])]
  
  if (length(unique(xtr$clase)) < 2) {
    
    stop(
      "La variable objetivo tiene una sola clase en TRAIN. ",
      "No se puede entrenar la regresión logística."
    )
  }
  
  cat(
    "  Variables utilizadas:",
    ncol(xtr) - 1,
    "\n"
  )
  
  m <- glm(
    clase ~ .,
    data = xtr,
    family = binomial()
  )
  
  predecir_todos(
    function(i) {
      predict(
        m,
        newdata = armar(i),
        type = "response"
      )
    }
  )
}


correr(
  "2. Regresión logística",
  function() logistica()
)


correr(
  "2. Regresión logística + FE",
  function() logistica(nuevas)
)


# ----------------------------------------------------------------------------
# 3. ÁRBOL DE DECISIÓN
# ----------------------------------------------------------------------------

arbol <- function(cols) {
  
  a <- PARAMS$metodos$arbol
  
  trm <- muestra(
    tr,
    PARAMS$metodos$muestra_lentos
  )
  
  x <- imputar(trm, cols)
  
  x$clase <- y[trm]
  
  m <- rpart(
    clase ~ .,
    data = x,
    method = "anova",
    control = rpart.control(
      maxdepth = a$max_depth,
      minbucket = a$min_samples_leaf,
      cp = 0,
      xval = 0
    )
  )
  
  predecir_todos(
    function(i) {
      predict(
        m,
        imputar(i, cols)
      )
    }
  )
}


correr(
  "3. Árbol de decisión",
  function() arbol(crudas)
)


correr(
  "3. Árbol de decisión + FE",
  function() arbol(con_fe)
)


# ----------------------------------------------------------------------------
# 4. RANDOM FOREST
# ----------------------------------------------------------------------------

random_forest <- function(cols) {
  
  a <- PARAMS$metodos$random_forest
  
  trm <- muestra(
    tr,
    PARAMS$metodos$muestra_lentos
  )
  
  x <- imputar(trm, cols)
  
  x$clase <- factor(y[trm])
  
  m <- ranger(
    clase ~ .,
    data = x,
    num.trees = a$n_estimators,
    min.node.size = a$min_samples_leaf,
    probability = TRUE,
    seed = SEM,
    num.threads = parallel::detectCores()
  )
  
  predecir_todos(
    function(i) {
      predict(
        m,
        imputar(i, cols)
      )$predictions[, "1"]
    }
  )
}


correr(
  "4. Random forest",
  function() random_forest(crudas)
)


correr(
  "4. Random forest + FE",
  function() random_forest(con_fe)
)


# ----------------------------------------------------------------------------
# 5 Y 6. LIGHTGBM POR DEFECTO
# ----------------------------------------------------------------------------

lgbm_defecto <- function(cols) {
  
  m <- lgb.train(
    params = list(
      objective = "binary",
      seed = SEM,
      verbosity = -1
    ),
    data = lgb.Dataset(
      matriz(tr, cols),
      label = y[tr]
    ),
    nrounds = 100
  )
  
  predecir_todos(
    function(i) {
      predict(
        m,
        matriz(i, cols)
      )
    }
  )
}


correr(
  "5. LightGBM por defecto",
  function() lgbm_defecto(crudas)
)


correr(
  "6. LightGBM por defecto + FE",
  function() lgbm_defecto(con_fe)
)


# ----------------------------------------------------------------------------
# 7. LIGHTGBM + FEATURE ENGINEERING + BÚSQUEDA BAYESIANA
# ----------------------------------------------------------------------------

cat(
  "\n7. LightGBM + FE + búsqueda bayesiana ",
  "(compite en validate)...\n",
  sep = ""
)

t0 <- Sys.time()

HT <- PARAMS$hyperparameter_tuning

dtrain <- lgb.Dataset(
  matriz(tr, con_fe),
  label = y[tr],
  free_raw_data = FALSE,
  params = list(feature_pre_filter = FALSE)
)

dvalid <- lgb.Dataset.create.valid(
  dtrain,
  matriz(va, con_fe),
  label = y[va]
)

X_va <- matriz(va, con_fe)
X_te <- matriz(te, con_fe)

BO_log <- list()
modelos <- list()


objetivo <- function(x) {
  
  p <- c(
    list(
      objective = "binary",
      metric = "auc",
      seed = HT$semilla,
      verbosity = -1
    ),
    x
  )
  
  t1 <- Sys.time()
  
  m <- lgb.train(
    params = p,
    data = dtrain,
    nrounds = HT$num_iterations_max,
    valids = list(valid = dvalid),
    early_stopping_rounds = as.integer(
      50 + 5 / x$learning_rate
    ),
    verbose = -1
  )
  
  auc_va <- auc(
    predict(
      m,
      X_va,
      num_iteration = m$best_iter
    ),
    y[va]
  )
  
  # Se calcula para seguimiento, pero NO se utiliza
  # para elegir los hiperparámetros.
  auc_te <- auc(
    predict(
      m,
      X_te,
      num_iteration = m$best_iter
    ),
    y[te]
  )
  
  k <- length(BO_log) + 1
  
  modelos[[k]] <<- m
  
  BO_log[[k]] <<- data.table(
    as.data.table(x),
    arboles = m$best_iter,
    auc_validate = auc_va,
    auc_test = auc_te,
    segundos = round(
      as.numeric(
        difftime(
          Sys.time(),
          t1,
          units = "secs"
        )
      ),
      1
    )
  )
  
  cat(
    sprintf(
      "    intento %2d: AUC validate %.4f  test %.4f  árboles %4d\n",
      k,
      auc_va,
      auc_te,
      m$best_iter
    )
  )
  
  auc_va
}


R_ <- HT$rangos

espacio <- makeParamSet(
  
  makeNumericParam(
    "learning_rate",
    lower = log(R_$learning_rate[1]),
    upper = log(R_$learning_rate[2]),
    trafo = function(x) exp(x)
  ),
  
  makeIntegerParam(
    "num_leaves",
    lower = R_$num_leaves[1],
    upper = R_$num_leaves[2]
  ),
  
  makeIntegerParam(
    "min_data_in_leaf",
    lower = R_$min_data_in_leaf[1],
    upper = R_$min_data_in_leaf[2]
  ),
  
  makeNumericParam(
    "feature_fraction",
    lower = R_$feature_fraction[1],
    upper = R_$feature_fraction[2]
  ),
  
  makeNumericParam(
    "lambda_l2",
    lower = R_$lambda_l2[1],
    upper = R_$lambda_l2[2]
  )
)


fn <- makeSingleObjectiveFunction(
  fn = objetivo,
  par.set = espacio,
  minimize = FALSE,
  noisy = TRUE,
  has.simple.signature = FALSE
)


set.seed(HT$semilla)

diseno <- generateDesign(
  n = HT$intentos_al_azar,
  par.set = espacio
)


ctrl <- setMBOControlInfill(
  setMBOControlTermination(
    makeMBOControl(),
    iters = HT$iteraciones
  ),
  crit = makeMBOInfillCritEI()
)


configureMlr(
  show.learner.output = FALSE,
  show.info = FALSE
)


invisible(
  mbo(
    fn,
    design = diseno,
    control = ctrl,
    show.info = FALSE,
    learner = makeLearner(
      "regr.km",
      predict.type = "se",
      covtype = "matern3_2",
      control = list(trace = FALSE)
    )
  )
)


BO_log <- rbindlist(BO_log)

fwrite(
  BO_log,
  file.path(carpeta_exp, "BO_log.csv")
)


# Selecciona el mejor intento según AUC de VALIDACIÓN
k <- which.max(BO_log$auc_validate)

mejor <- modelos[[k]]

arboles <- BO_log$arboles[k]

params <- as.list(
  BO_log[
    k,
    .(
      learning_rate,
      num_leaves,
      min_data_in_leaf,
      feature_fraction,
      lambda_l2
    )
  ]
)


puntajes[["7. LightGBM + FE + HT"]] <-
  predecir_todos(
    function(i) {
      predict(
        mejor,
        matriz(i, con_fe),
        num_iteration = arboles
      )
    }
  )


segundos[["7. LightGBM + FE + HT"]] <- round(
  as.numeric(
    difftime(
      Sys.time(),
      t0,
      units = "secs"
    )
  ),
  1
)


cat(
  sprintf(
    "  Mejor intento: %d · %d árboles · AUC test %.4f\n",
    k,
    arboles,
    auc(
      puntajes[["7. LightGBM + FE + HT"]][[as.character(TS$test)]],
      y[te]
    )
  )
)


# ----------------------------------------------------------------------------
# 8. ESTRATEGIA COMPLETA: REENTRENAMIENTO CON TRAIN_FINAL
# ----------------------------------------------------------------------------

correr(
  "8. Estrategia completa (+ train_final)",
  function() {
    
    n <- max(
      1L,
      as.integer(
        arboles * length(tf) / length(tr)
      )
    )
    
    m <- lgb.train(
      params = c(
        list(
          objective = "binary",
          seed = HT$semilla,
          verbosity = -1
        ),
        params
      ),
      data = lgb.Dataset(
        matriz(tf, con_fe),
        label = y[tf]
      ),
      nrounds = n
    )
    
    importancia <<- lgb.importance(m)
    
    predecir_todos(
      function(i) {
        predict(
          m,
          matriz(i, con_fe)
        )
      }
    )
  }
)


# ----------------------------------------------------------------------------
# 9. REENTRENAMIENTO AÑO A AÑO
# ----------------------------------------------------------------------------
#
# Para cada año, entrena con todos los años anteriores que tienen etiqueta.
# En 2021, el modelo se entrena con 2015-2020 y predice la foto 2021.
# ----------------------------------------------------------------------------

correr(
  "9. Reentrenado año a año",
  function() {
    
    salida <- list()
    
    # Conserva el resultado del método 8 para el año de test
    salida[[as.character(TS$test)]] <-
      puntajes[[
        "8. Estrategia completa (+ train_final)"
      ]][[as.character(TS$test)]]
    
    
    for (a in TS$otros_anios) {
      
      tr_a <- anios(
        TS$train_final[1],
        a - 1
      )
      
      n <- max(
        1L,
        as.integer(
          arboles * length(tr_a) / length(tr)
        )
      )
      
      m <- lgb.train(
        params = c(
          list(
            objective = "binary",
            seed = HT$semilla,
            verbosity = -1
          ),
          params
        ),
        data = lgb.Dataset(
          matriz(tr_a, con_fe),
          label = y[tr_a]
        ),
        nrounds = n
      )
      
      salida[[as.character(a)]] <-
        predict(
          m,
          matriz(
            filas[[as.character(a)]],
            con_fe
          )
        )
      
      cat(
        sprintf(
          "    %d: entrena %d-%d (%s filas)\n",
          a,
          TS$train_final[1],
          a - 1,
          format(
            length(tr_a),
            big.mark = "."
          )
        )
      )
    }
    
    salida
  }
)


# ----------------------------------------------------------------------------
# 10. MODELO FINAL: ENTRENAMIENTO 2015-2020 Y PREDICCIÓN 2021
# ----------------------------------------------------------------------------

cat(
  "\n10. Modelo final: entrenamiento 2015-2020 ",
  "y predicción para 2022...\n",
  sep = ""
)

t0_final <- Sys.time()


# Todos los años históricos etiquetados hasta 2020
tr_final <- anios(
  TS$train_final[1],
  2020
)

if (length(tr_final) == 0) {
  stop("No se encontraron registros históricos para entrenar el modelo final.")
}

if (length(pred) == 0) {
  stop("No hay alumnos en la foto 2021 para predecir.")
}


# Ajustar la cantidad de árboles al tamaño del entrenamiento final
n_final <- max(
  1L,
  as.integer(
    arboles * length(tr_final) / length(tr)
  )
)


modelo_final <- lgb.train(
  params = c(
    list(
      objective = "binary",
      seed = HT$semilla,
      verbosity = -1
    ),
    params
  ),
  data = lgb.Dataset(
    matriz(tr_final, con_fe),
    label = y[tr_final]
  ),
  nrounds = n_final
)


# Probabilidad estimada de abandono para cada alumno de 2021
probabilidades_2022 <- predict(
  modelo_final,
  matriz_pred(pred, con_fe)
)


if (length(probabilidades_2022) != nrow(d_pred)) {
  stop("La cantidad de probabilidades no coincide con la cantidad de alumnos de 2021.")
}

if (anyNA(probabilidades_2022)) {
  stop("El modelo produjo probabilidades faltantes.")
}

if (any(probabilidades_2022 < 0 | probabilidades_2022 > 1)) {
  stop("El modelo produjo probabilidades fuera del intervalo [0,1].")
}


# Guardar probabilidades junto con el identificador del alumno
id_alumno <- PARAMS$datos$id

predicciones_2022 <- data.table(
  id_alumno = d_pred[[id_alumno]],
  prob_abandono_2022 = probabilidades_2022
)


fwrite(
  predicciones_2022,
  file.path(
    carpeta_exp,
    "predicciones_2022_modelo9.csv"
  )
)


segundos[["10. Modelo final 2022"]] <- round(
  as.numeric(
    difftime(
      Sys.time(),
      t0_final,
      units = "secs"
    )
  ),
  1
)


# ----------------------------------------------------------------------------
# 11. RESUMEN Y GUARDADO DE RESULTADOS
# ----------------------------------------------------------------------------

cat("\n============================================\n")
cat("RESUMEN DE EJECUCIÓN\n")
cat("============================================\n")

for (nombre in names(puntajes)) {
  
  cat(
    sprintf(
      "%-45s %8.1f segundos\n",
      nombre,
      segundos[[nombre]]
    )
  )
}


cat("\nPredicción final generada correctamente.\n")

cat(
  "Alumnos procesados:",
  format(nrow(predicciones_2022), big.mark = "."),
  "\n"
)

cat(
  "Archivo:",
  file.path(
    carpeta_exp,
    "predicciones_2022_modelo9.csv"
  ),
  "\n"
)

cat("\n=== FIN DE LOS MÉTODOS ===\n")