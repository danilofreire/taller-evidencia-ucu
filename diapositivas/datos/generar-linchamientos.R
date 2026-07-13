# generar-linchamientos.R
# Genera un conjunto de datos SINTÉTICO de un experimento de encuesta
# (viñeta) sobre actitudes hacia la justicia por mano propia, para enseñar
# experimentos de encuesta en el taller de la UCU.
#
# Inspirado (a grandes rasgos) en el estudio de actitudes hacia los
# linchamientos en Brasil. Unidad: persona encuestada. A cada una se le
# muestra al azar una de dos viñetas sobre un sospechoso de robo capturado
# por vecinos; la versión de tratamiento agrega que la policía había
# ignorado denuncias repetidas del barrio. Resultado: apoyo (0 a 10) a la
# acción de los vecinos.
#
# Efecto verdadero: el tratamiento sube el apoyo en promedio +1.4 puntos,
# PERO el efecto es heterogéneo: mucho mayor entre quienes desconfían de la
# policía (tau_i = 2.6 - 0.4 * confianza_policia). De ahí la interacción
# negativa condicion x confianza_policia que la práctica descubre.

set.seed(2026)

n <- 1200

# --- Condición experimental: 600 control, 600 tratamiento, al azar ---
condicion <- sample(rep(c("control", "tratamiento"), each = n / 2))

# --- Covariables predeterminadas ---
edad      <- pmin(pmax(round(rnorm(n, 40, 13)), 18), 80)
mujer     <- rbinom(n, 1, 0.52)
educacion <- factor(
  sample(c("primaria", "secundaria", "terciaria"), n, replace = TRUE,
         prob = c(0.25, 0.45, 0.30)),
  levels = c("primaria", "secundaria", "terciaria"), ordered = TRUE
)
confianza_policia <- sample(1:5, n, replace = TRUE,
                            prob = c(.15, .25, .30, .20, .10))

# --- Resultado con efecto heterogéneo ---
trat  <- as.integer(condicion == "tratamiento")
tau_i <- 2.6 - 0.4 * confianza_policia
apoyo <- 3.2 + tau_i * trat - 0.15 * (confianza_policia - 3) + rnorm(n, 0, 2.2)
apoyo <- pmin(pmax(round(apoyo), 0), 10)

datos <- data.frame(
  id                = seq_len(n),
  condicion         = condicion,
  edad              = edad,
  mujer             = mujer,
  educacion         = as.character(educacion),
  confianza_policia = confianza_policia,
  apoyo             = apoyo,
  stringsAsFactors  = FALSE
)

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "linchamientos.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Verificación (descomentar para volver a chequear) --------------------
# library(estimatr)
# summary(lm_robust(apoyo ~ condicion, data = datos))   # ATE ~ +1.4, significativo
# # Modificación de clase: interacción con confianza en la policía
# datos$trat <- as.integer(datos$condicion == "tratamiento")
# summary(lm_robust(apoyo ~ trat * confianza_policia, data = datos))
# # el coeficiente de la interacción trat:confianza_policia debería ser ~ -0.4:
# # el efecto se concentra entre quienes ya desconfían de las instituciones
