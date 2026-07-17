# generar-meta.R
# Genera un conjunto de datos SINTÉTICO de estudios sobre tamaño de la
# legislatura y gasto público, para enseñar meta-análisis y sesgo de
# publicación en el taller de la UCU.
#
# Unidad: estudio. 20 estudios (autores y años ficticios, con sabor regional).
# Cada uno reporta un efecto estandarizado (efecto) y su error estándar (ee).
# El efecto verdadero es 0.15 con heterogeneidad entre estudios (tau = 0.08).
#
# Truco pedagógico (sesgo de publicación): a los estudios chicos (ee > 0.12)
# les imponemos que sólo "se publican" si son significativos y positivos
# (efecto / ee > 1.64). Eso infla los efectos chicos y hace que:
#   - el estimador combinado suba por encima del 0.15 verdadero (~0.20 a 0.24)
#   - el funnel plot quede asimétrico y el test de Egger dé significativo
#   - al quedarnos sólo con los estudios grandes (ee <= 0.12), baje hacia 0.15

set.seed(2026)

n_est <- 20

# --- Tamaño muestral y error estándar ------------------------------------
# (Se simulan primero las columnas numéricas para fijar el flujo aleatorio.)
n  <- round(exp(runif(n_est, log(80), log(5500))))     # log-uniforme 80 a 5500
ee <- round(pmin(pmax(2.2 / sqrt(n), 0.03), 0.25), 3)  # ~ 1/sqrt(n), rango 0.03 a 0.25

# --- Efecto con heterogeneidad y selección por publicación ---------------
tau  <- 0.08
mu   <- 0.15
efecto <- numeric(n_est)
for (i in seq_len(n_est)) {
  sdi <- sqrt(tau^2 + ee[i]^2)
  e   <- rnorm(1, mu, sdi)
  if (ee[i] > 0.12) {                                  # estudio chico: se publica sólo si significativo
    while (e / ee[i] <= 1.64) e <- rnorm(1, mu, sdi)
  }
  efecto[i] <- round(e, 3)
}

# --- Etiquetas de estudio ficticias (no usar nombres reales) -------------
apellidos <- c("García", "Souza", "Lima", "Rojas", "Fernández", "Muñoz",
               "Silva", "Torres", "Vargas", "Herrera", "Castro", "Mendoza",
               "Ríos", "Acosta", "Núñez", "Ferreira", "Ortiz", "Cabrera",
               "Duarte", "Sosa")
anios <- sample(2005:2022, n_est, replace = TRUE)
dobles <- rbinom(n_est, 1, 0.35)                       # algunos con dos autores
segundo <- sample(apellidos)
estudio <- ifelse(dobles == 1,
                  sprintf("%s y %s (%d)", apellidos, segundo, anios),
                  sprintf("%s (%d)", apellidos, anios))

pais <- sample(c("Brasil", "México", "Chile", "Colombia", "Perú",
                 "Argentina", "Uruguay"), n_est, replace = TRUE)
diseno <- sample(rep(c("RDD", "DiD", "OLS"), c(8, 5, 7)))

datos <- data.frame(
  estudio          = estudio,
  pais             = pais,
  diseno           = diseno,
  n                = n,
  ee               = ee,
  efecto           = efecto,
  stringsAsFactors = FALSE
)

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "meta_estudios.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Verificación (descomentar para volver a chequear) --------------------
# library(metafor)
# m <- rma(yi = efecto, sei = ee, data = datos)
# summary(m)                       # estimador combinado ~ 0.20 a 0.24 (sesgado)
# regtest(m)                       # test de Egger, p < ~0.10 (asimetría)
# funnel(m); forest(m)             # el funnel se ve asimétrico
# # Modificación de clase: sólo estudios grandes -> baja hacia 0.15
# rma(yi = efecto, sei = ee, data = subset(datos, ee <= 0.12))
