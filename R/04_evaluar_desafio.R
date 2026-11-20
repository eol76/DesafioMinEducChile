
# ============================================================================
# EVALUACIÓN DEL DESAFÍO:
# AUC por método y por año, curvas ROC e importancia de variables.
#
# Se evalúan exclusivamente los años con clase observada:
# 2018, 2019 y 2020.
#
# La foto 2021 se utiliza para predecir el abandono en 2022,
# pero NO se evalúa porque todavía no se conoce su resultado.
# ============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

cat("\n============================================================\n")
cat(" EVALUACIÓN DE MODELOS - DESAFÍO ABANDONO ESCOLAR\n")
cat("============================================================\n")

# ----------------------------------------------------------------------------
# 1. Verificar que existan los objetos necesarios
# ----------------------------------------------------------------------------

objetos_necesarios <- c(
  "filas",
  "puntajes",
  "y",
  "TS",
  "te",
  "carpeta_exp",
  "segundos",
  "auc"
)

faltantes <- objetos_necesarios[
  !vapply(objetos_necesarios, exists, logical(1), inherits = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    "Faltan objetos necesarios para evaluar: ",
    paste(faltantes, collapse = ", "),
    ". Revisá la ejecución de 02_TS_desafio.R y 03_metodos_desafio.R."
  )
}

if (!length(puntajes)) {
  stop("No hay predicciones disponibles en el objeto 'puntajes'.")
}

if (!length(filas)) {
  stop("No hay particiones de evaluación disponibles en 'filas'.")
}

# Años con resultados observados.
anios_eval <- names(filas)

if (length(anios_eval) == 0) {
  stop("No se encontraron años para evaluar.")
}

cat("\nAños incluidos en la evaluación:",
    paste(anios_eval, collapse = ", "), "\n")

# La evaluación no debe incluir la foto 2021.
if ("2021" %in% anios_eval) {
  stop(
    "La foto 2021 no debe estar incluida en 'filas'. ",
    "No tiene etiqueta de abandono observada."
  )
}

# ----------------------------------------------------------------------------
# 2. Calcular AUC por método y por año
# ----------------------------------------------------------------------------

resultados <- lapply(names(puntajes), function(m) {
  
  predicciones_metodo <- puntajes[[m]]
  
  fila <- data.table(metodo = m)
  
  for (a in anios_eval) {
    
    if (is.null(predicciones_metodo[[a]])) {
      stop(
        "No se encontraron predicciones del método '", m,
        "' para el año ", a, "."
      )
    }
    
    indices <- filas[[a]]
    
    if (length(indices) != length(predicciones_metodo[[a]])) {
      stop(
        "La cantidad de predicciones no coincide con la cantidad ",
        "de observaciones para el método '", m,
        "' en el año ", a, "."
      )
    }
    
    etiquetas <- y[indices]
    scores <- predicciones_metodo[[a]]
    
    if (anyNA(etiquetas) || anyNA(scores)) {
      stop(
        "Hay valores faltantes en etiquetas o predicciones: ",
        "método ", m, ", año ", a, "."
      )
    }
    
    if (length(unique(etiquetas)) < 2) {
      stop(
        "El año ", a,
        " no tiene las dos clases necesarias para calcular AUC."
      )
    }
    
    fila[, (a) := auc(scores, etiquetas)]
  }
  
  # Tiempo de ejecución registrado para cada método.
  tiempo_metodo <- segundos[[m]]
  
  if (is.null(tiempo_metodo)) {
    tiempo_metodo <- NA_real_
  }
  
  fila[, segundos := as.numeric(tiempo_metodo)]
  
  fila
})

tabla <- rbindlist(resultados, fill = TRUE)

# Guardar tabla de AUC.
archivo_auc <- file.path(
  carpeta_exp,
  "auc_metodos_desafio.csv"
)

fwrite(tabla, archivo_auc)

cat("\n=== AUC POR MÉTODO ===\n")
cat("La columna correspondiente al test es la referencia principal.\n\n")

print(
  tabla[
    ,
    lapply(.SD, function(x) {
      if (is.numeric(x)) round(x, 4) else x
    })
  ]
)

# ----------------------------------------------------------------------------
# 3. Construir curvas ROC para el año de test
# ----------------------------------------------------------------------------

test <- as.character(TS$test)

if (!test %in% anios_eval) {
  stop(
    "El año de test (", test,
    ") no está incluido entre los años evaluados."
  )
}

# Función para construir los puntos de la curva ROC.
roc <- function(score, yy, metodo) {
  
  if (length(score) != length(yy)) {
    stop("La cantidad de scores no coincide con las etiquetas.")
  }
  
  if (anyNA(score) || anyNA(yy)) {
    stop("Hay valores faltantes al construir la curva ROC.")
  }
  
  if (length(unique(yy)) < 2) {
    stop("Se necesitan las dos clases para construir una curva ROC.")
  }
  
  orden <- order(-score)
  
  score <- score[orden]
  yy <- yy[orden]
  
  positivos <- sum(yy == 1)
  negativos <- sum(yy == 0)
  
  tpr <- cumsum(yy == 1) / positivos
  fpr <- cumsum(yy == 0) / negativos
  
  # Se utilizan hasta 2000 puntos para representar la curva.
  k <- unique(
    round(seq(1, length(yy), length.out = min(2000, length(yy))))
  )
  
  data.table(
    metodo = metodo,
    fpr = c(0, fpr[k]),
    tpr = c(0, tpr[k])
  )
}

# El método 9 replica al método 8 en el año de test.
# Por eso se excluye de la gráfica ROC para evitar duplicar la curva.
dibujar <- names(puntajes)[
  !grepl("^9", names(puntajes))
]

curvas <- rbindlist(
  lapply(dibujar, function(m) {
    
    roc(
      score = puntajes[[m]][[test]],
      yy = y[te],
      metodo = m
    )
    
  })
)

# Etiquetas con el AUC correspondiente a cada método.
etiquetas <- setNames(
  sprintf(
    "%s (AUC %.3f)",
    tabla$metodo,
    tabla[[test]]
  ),
  tabla$metodo
)

curvas[, metodo_original := metodo]

curvas[
  ,
  metodo := factor(
    etiquetas[metodo_original],
    levels = unname(etiquetas[dibujar])
  )
]

# Identificar visualmente la estrategia completa y los métodos con FE.
curvas[
  ,
  completa := grepl("^8", metodo_original)
]

curvas[
  ,
  con_fe := grepl("^[234].*[+] FE", metodo_original)
]

# ----------------------------------------------------------------------------
# 4. Gráfico de curvas ROC
# ----------------------------------------------------------------------------

base <- ggplot(
  curvas,
  aes(
    x = fpr,
    y = tpr,
    colour = metodo,
    linewidth = completa,
    linetype = con_fe
  )
) +
  geom_abline(
    linetype = "dotted",
    colour = "grey50"
  ) +
  geom_line() +
  scale_linewidth_manual(
    values = c(
      `FALSE` = 0.6,
      `TRUE` = 1.3
    ),
    guide = "none"
  ) +
  scale_linetype_manual(
    values = c(
      `FALSE` = "solid",
      `TRUE` = "dashed"
    ),
    guide = "none"
  ) +
  scale_colour_viridis_d(end = 0.92) +
  labs(
    x = "Falsos positivos (alumnos que no abandonan y marcamos)",
    y = "Verdaderos positivos (de los que abandonan, cuántos marcamos)",
    colour = NULL
  ) +
  theme_minimal(base_size = 11)

g1 <- base +
  labs(
    title = sprintf(
      "Curva ROC en test (%s → abandono en %d)",
      test,
      as.integer(test) + 1
    )
  ) +
  theme(
    legend.position = c(0.68, 0.25),
    legend.text = element_text(size = 8),
    legend.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

g2 <- base +
  coord_cartesian(xlim = c(0, 0.2)) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Zoom: el 20 % de alumnos más marcados"
  )

archivo_roc <- file.path(
  carpeta_exp,
  "roc_test_desafio.png"
)

png(
  archivo_roc,
  width = 14,
  height = 6.2,
  units = "in",
  res = 130
)

grid::grid.newpage()

grid::pushViewport(
  grid::viewport(
    layout = grid::grid.layout(1, 2)
  )
)

print(
  g1,
  vp = grid::viewport(
    layout.pos.row = 1,
    layout.pos.col = 1
  )
)

print(
  g2,
  vp = grid::viewport(
    layout.pos.row = 1,
    layout.pos.col = 2
  )
)

invisible(dev.off())

# ----------------------------------------------------------------------------
# 5. Gráfico comparativo de AUC
# ----------------------------------------------------------------------------

# Se excluye el método 9 porque en el año de test
# utiliza las mismas predicciones que el método 8.
escalera <- tabla[
  !grepl("^9", metodo),
  .(
    metodo = factor(
      metodo,
      levels = rev(metodo)
    ),
    auc = get(test)
  )
]

g3 <- ggplot(
  escalera,
  aes(
    x = auc,
    y = metodo,
    fill = metodo
  )
) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.3f", auc)),
    hjust = -0.15,
    size = 3.3
  ) +
  scale_fill_viridis_d(
    end = 0.92,
    direction = -1
  ) +
  coord_cartesian(
    xlim = c(0.5, 1)
  ) +
  labs(
    x = sprintf("AUC en test (%s)", test),
    y = NULL,
    title = "¿Cuánto suma cada paso?"
  ) +
  theme_minimal(base_size = 11)

archivo_escalera <- file.path(
  carpeta_exp,
  "auc_escalera_desafio.png"
)

ggsave(
  archivo_escalera,
  g3,
  width = 9,
  height = 6,
  dpi = 130
)

# ----------------------------------------------------------------------------
# 6. Guardar importancia de variables
# ----------------------------------------------------------------------------

if (exists("importancia", inherits = TRUE)) {
  
  importancia_final <- copy(importancia)
  
  if (
    !all(c("Feature", "Gain") %in% names(importancia_final))
  ) {
    warning(
      "El objeto 'importancia' no contiene las columnas Feature y Gain. ",
      "No se generó el archivo de importancia."
    )
    
  } else {
    
    suma_gain <- sum(
      importancia_final$Gain,
      na.rm = TRUE
    )
    
    if (is.finite(suma_gain) && suma_gain > 0) {
      
      importancia_final[
        ,
        Gain := Gain / suma_gain
      ]
      
      fwrite(
        importancia_final[
          ,
          .(
            variable = Feature,
            ganancia = Gain
          )
        ],
        file.path(
          carpeta_exp,
          "importancia_desafio.csv"
        )
      )
      
    } else {
      
      warning(
        "La suma de Gain es cero o no es válida. ",
        "No se generó el archivo de importancia."
      )
    }
  }
  
} else {
  
  warning(
    "No existe el objeto 'importancia'. ",
    "Revisá que 03_metodos_desafio.R haya generado la importancia del modelo."
  )
}

# ----------------------------------------------------------------------------
# 7. Resumen final
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat(" EVALUACIÓN FINALIZADA\n")
cat("============================================================\n")

cat("\nArchivos generados:\n")
cat(" - auc_metodos_desafio.csv\n")
cat(" - roc_test_desafio.png\n")
cat(" - auc_escalera_desafio.png\n")

if (file.exists(file.path(carpeta_exp, "importancia_desafio.csv"))) {
  cat(" - importancia_desafio.csv\n")
}

cat("\nCarpeta de resultados:\n")
cat(carpeta_exp, "\n")

cat("\nIMPORTANTE:\n")
cat(
  "La foto 2021 no se evalúa mediante AUC porque no tiene ",
  "resultados observados de abandono en 2022.\n",
  sep = ""
)

cat(
  "Las predicciones para 2022 se encuentran en el archivo ",
  "generado por 03_metodos_desafio.R.\n",
  sep = ""
)

cat("\n============================================================\n")
