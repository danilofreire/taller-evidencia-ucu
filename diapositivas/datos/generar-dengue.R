# generar-dengue.R
# Genera un conjunto de datos SINTÉTICO de un experimento de campo con
# agentes comunitarios de salud, para enseñar experimentos aleatorizados
# en el taller de la UCU.
#
# Inspirado (a grandes rasgos) en el experimento de incentivos financieros
# para el control del Aedes aegypti en Brasil. Unidad: agente comunitario
# de salud. La mitad recibe un incentivo por desempeño (tratamiento),
# la otra mitad no (control), asignados al azar. Como la asignación es
# aleatoria, los grupos quedan balanceados en expectativa y la diferencia
# de medias estima el efecto causal.
#
# Efectos verdaderos incorporados en la simulación:
#   visitas domiciliarias   +6.0 por agente tratado
#   índice larvario (%)     -2.5 puntos por agente tratado
# Las covariables (edad, sexo, experiencia, zona, visitas previas) son
# predeterminadas y no dependen del tratamiento: sirven para la tabla de
# balance (deben salir sin diferencias sistemáticas).

set.seed(2026)

n <- 400

# --- Tratamiento: 200 tratados y 200 control, aleatorización simple ---
tratamiento <- sample(rep(c(0L, 1L), each = n / 2))

# --- Covariables predeterminadas (no dependen del tratamiento) ---
edad            <- pmin(pmax(round(rnorm(n, 38, 8)), 22), 60)
mujer           <- rbinom(n, 1, 0.7)
experiencia     <- rpois(n, 7)
zona            <- sample(c("urbana", "rural"), n, replace = TRUE, prob = c(0.6, 0.4))
visitas_previas <- pmax(round(rnorm(n, 35, 6)), 15)

# --- Resultados con efecto causal verdadero ---
visitas        <- pmax(round(visitas_previas + 6 * tratamiento + rnorm(n, 0, 8)), 0)
indice_larvario <- round(pmax(0, rnorm(n, 12, 4) - 2.5 * tratamiento), 1)

datos <- data.frame(
  id_agente       = seq_len(n),
  tratamiento     = tratamiento,
  edad            = edad,
  mujer           = mujer,
  experiencia     = experiencia,
  zona            = zona,
  visitas_previas = visitas_previas,
  visitas         = visitas,
  indice_larvario = indice_larvario,
  stringsAsFactors = FALSE
)

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "dengue_incentivos.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Verificación (descomentar para volver a chequear) --------------------
# library(estimatr)
# summary(lm_robust(visitas ~ tratamiento, data = datos))          # ~ +6, t > 5
# summary(lm_robust(indice_larvario ~ tratamiento, data = datos))  # ~ -2.5
# # Balance: ninguna covariable debería diferir sistemáticamente
# for (v in c("edad", "mujer", "experiencia", "visitas_previas")) {
#   f <- reformulate("tratamiento", v)
#   print(summary(lm_robust(f, data = datos))$coefficients["tratamiento", c("Estimate", "t value")])
# }
# # Modificación de clase: repetir sólo en zona rural (efecto similar, IC más ancho)
# summary(lm_robust(visitas ~ tratamiento, data = subset(datos, zona == "rural")))
