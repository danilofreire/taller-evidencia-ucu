# generar-beca.R
# Genera un conjunto de datos SINTÉTICO de un programa de becas de estudio,
# para la Tarea 2 del taller de la UCU (bloque de regresión discontinua).
#
# Un programa reparte becas a estudiantes de bajos recursos: quienes superan
# un puntaje de vulnerabilidad socioeconómica (>= 50) reciben la beca; los
# que quedan por debajo, no. Ese umbral administrativo crea un RDD sharp:
# cerca del corte, quedar de un lado o del otro es casi un sorteo, así que
# comparar a los que están apenas arriba con los que están apenas abajo
# estima el efecto causal de la beca.
#
# La variable de asignación es `puntaje_c` (el puntaje centrado en el corte,
# puntaje - 50). Cruzar el corte (`puntaje_c >= 0`) da la beca. Efectos
# verdaderos incorporados:
#   creditos     +6 créditos aprobados en el primer año
#   permanencia  salto de ~ +0.20 en la probabilidad de seguir matriculado
# Las covariables previas (edad, mujer, promedio_secundaria, ingreso_hogar)
# NO saltan en el corte: sirven para el chequeo de balance.
#
# Ojo con la dirección: un puntaje más alto significa MÁS vulnerabilidad, y
# los más vulnerables rinden algo menos por otras razones (la pendiente es
# negativa). La beca agrega un salto hacia arriba justo en el umbral.
#
# El generador usa fabricatr. La semilla fija (2718) garantiza datos
# idénticos a beca_puntaje.csv.

library(fabricatr)

set.seed(2718)

datos <- fabricate(
  N = 2000,
  puntaje   = round(runif(N, 10, 90), 2),   # índice de vulnerabilidad (0-100)
  puntaje_c = puntaje - 50,                  # centrado en el umbral
  beca      = as.integer(puntaje_c >= 0),    # la regla asigna, no un sorteo
  x         = puntaje_c / 10,                # tendencia, en decenas de puntos
  # --- covariables previas (no saltan en el corte) ---
  edad                = round(runif(N, 17, 20)),
  mujer               = rbinom(N, 1, 0.52),
  promedio_secundaria = round(pmin(10, pmax(4, 7.5 - 0.15 * x + rnorm(N, 0, 1))), 1),
  ingreso_hogar       = round(pmax(40, 380 - 22 * x + rnorm(N, 0, 55))),
  # --- resultados con el salto de la beca en el umbral ---
  creditos    = pmax(0, round(28 - 1.2 * x + 6 * beca + rnorm(N, 0, 5))),
  permanencia = as.integer(0.25 - 0.08 * x + 0.9 * beca + rnorm(N, 0, 1) > 0)
)
datos$estudiante <- sprintf("E%04d", seq_len(nrow(datos)))
datos <- datos[, c("estudiante", "puntaje", "puntaje_c", "beca",
                   "edad", "mujer", "promedio_secundaria", "ingreso_hogar",
                   "creditos", "permanencia")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "beca_puntaje.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Chequeo rápido con rdrobust y rddensity ------------------------------
suppressPackageStartupMessages({
  library(rdrobust); library(rddensity)
})
rc <- rdrobust(datos$creditos, datos$puntaje_c)
cat(sprintf("salto en creditos     = %+.2f (p = %.3f) [esperado ~ +6]\n",
            rc$coef[1], rc$pv[1]))
rp <- rdrobust(datos$permanencia, datos$puntaje_c)
cat(sprintf("salto en permanencia  = %+.3f (p = %.3f)\n", rp$coef[1], rp$pv[1]))
dd <- rddensity(datos$puntaje_c)
cat(sprintf("densidad (manipulación) p = %.3f  [esperado alto]\n", dd$test$p_jk))
cat("Balance en covariables (salto y p):\n")
for (v in c("promedio_secundaria", "ingreso_hogar", "edad", "mujer")) {
  m <- rdrobust(datos[[v]], datos$puntaje_c)
  cat(sprintf("  %-20s salto = %+.3f  p = %.3f\n", v, m$coef[1], m$pv[1]))
}
pl <- rdrobust(datos$creditos, datos$puntaje_c, c = -20)
cat(sprintf("corte placebo (c=-20) p = %.3f  [esperado alto]\n", pl$pv[1]))
