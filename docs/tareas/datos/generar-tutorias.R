# generar-tutorias.R
# Genera un conjunto de datos SINTÉTICO de un experimento de campo con un
# programa de tutorías escolares, para la Tarea 1 del taller de la UCU
# (bloque de experimentos).
#
# Un municipio sortea un programa de tutorías de refuerzo entre estudiantes
# de secundaria: la mitad recibe las tutorías (tratamiento), la otra mitad
# no (control). Como la asignación es aleatoria, los grupos quedan
# balanceados en expectativa y la diferencia de medias estima el efecto
# causal del programa sobre el rendimiento.
#
# Efecto verdadero incorporado en la simulación:
#   nota_final   +4 puntos para estudiantes no vulnerables
#                +14 puntos para estudiantes vulnerables (efecto heterogéneo)
#   => el efecto promedio (ATE) ronda +8 puntos sobre 100.
# Las covariables (nota_base, edad, mujer, vulnerable, asistencia_previa,
# zona) se miden antes del sorteo y no dependen del tratamiento: sirven para
# la tabla de balance (deben salir sin diferencias sistemáticas) y para el
# ajuste de covariables a la Lin.
#
# El generador usa fabricatr y randomizr, la misma "cocina" del bloque de
# experimentos. La semilla fija (4012) garantiza datos idénticos a
# tutorias_rct.csv.

library(fabricatr)
library(randomizr)

set.seed(4012)

datos <- fabricate(
  N = 500,
  # --- covariables previas (medidas antes del sorteo) ---
  nota_base         = round(pmin(pmax(rnorm(N, 58, 12), 20), 95)),
  edad              = sample(14:18, N, replace = TRUE),
  mujer             = rbinom(N, 1, 0.5),
  vulnerable        = rbinom(N, 1, 0.4),
  asistencia_previa = round(pmin(pmax(rnorm(N, 85, 8), 50), 100)),
  zona              = sample(c("urbana", "rural"), N, replace = TRUE,
                             prob = c(0.65, 0.35)),
  # --- el sorteo: 250 alumnos a tutorías, 250 al control ---
  tratamiento       = complete_ra(N, m = 250),
  # --- el efecto verdadero es más grande para los vulnerables ---
  efecto_i          = 4 + 10 * vulnerable,
  nota_final        = round(pmin(100, pmax(0,
                        0.6 * nota_base + 0.15 * asistencia_previa + 12
                        + efecto_i * tratamiento + rnorm(N, 0, 6)))),
  aprobo            = as.integer(nota_final >= 60)
)

datos$id_alumno <- sprintf("A%03d", seq_len(nrow(datos)))
datos <- datos[, c("id_alumno", "tratamiento", "nota_base", "edad", "mujer",
                   "vulnerable", "asistencia_previa", "zona",
                   "nota_final", "aprobo")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "tutorias_rct.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Chequeo rápido de los efectos ----------------------------------------
suppressPackageStartupMessages(library(estimatr))
ate <- difference_in_means(nota_final ~ tratamiento, data = datos)
cat(sprintf("ATE nota_final           = %+.2f (esperado ~ +8)\n",
            ate$coefficients["tratamiento"]))
p_aprobo <- lm_robust(aprobo ~ tratamiento, data = datos)
cat(sprintf("efecto sobre aprobo      = %+.3f\n",
            p_aprobo$coefficients["tratamiento"]))
het <- lm_robust(nota_final ~ tratamiento * vulnerable, data = datos)
cat(sprintf("efecto no vulnerables    = %+.2f (esperado ~ +4)\n",
            het$coefficients["tratamiento"]))
cat(sprintf("extra si vulnerable      = %+.2f (esperado ~ +10)\n",
            het$coefficients["tratamiento:vulnerable"]))
cat("Balance (dif. y p por covariable):\n")
for (v in c("nota_base", "edad", "mujer", "asistencia_previa")) {
  f <- lm_robust(reformulate("tratamiento", v), data = datos)
  cat(sprintf("  %-18s dif = %+.2f  p = %.3f\n", v,
              f$coefficients["tratamiento"], f$p.value["tratamiento"]))
}
