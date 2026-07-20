# generar-transferencias.R
# Genera datos/transferencias.csv, la literatura simulada de la PRÁCTICA de
# 05-acumulacion.qmd: 24 estudios sobre el efecto de las transferencias
# condicionadas en la asistencia escolar.
#
# La gracia de estos datos: NO hay sesgo de publicación (nadie quedó en el
# cajón, el funnel sale simétrico y el test de Egger no detecta nada), pero
# el efecto verdadero es DISTINTO en dos contextos: 0.35 en zonas rurales y
# 0.10 en zonas urbanas. El promedio combinado (~0.24) no describe a ninguno
# de los dos. Al separar por contexto la heterogeneidad desaparece.
#
# Semilla fija (46) => el CSV es siempre el mismo.

library(fabricatr)

set.seed(46)

apellidos <- c("García", "Souza", "Lima", "Rojas", "Fernández",
               "Muñoz", "Silva", "Torres", "Vargas", "Herrera",
               "Castro", "Mendoza", "Ríos", "Acosta", "Núñez",
               "Ferreira", "Ortiz", "Cabrera", "Duarte", "Sosa")
paises <- c("Brasil", "México", "Colombia", "Perú",
            "Honduras", "Nicaragua", "Ecuador")

transferencias <- fabricate(
  N = 24,
  autor    = sample(apellidos, N, replace = TRUE),
  anio     = sample(2008:2023, N, replace = TRUE),
  estudio  = paste0(autor, " (", anio, ")"),
  pais     = sample(paises, N, replace = TRUE),
  # 12 estudios rurales y 12 urbanos
  contexto = rep(c("rural", "urbano"), each = 12),
  n        = round(exp(runif(N, log(200), log(2000)))),
  ee       = round(1.5 / sqrt(n), 3),
  # el efecto verdadero depende del contexto: 0.35 rural, 0.10 urbano
  efecto   = round(rnorm(N, ifelse(contexto == "rural", 0.35, 0.10),
                         sqrt(0.04^2 + ee^2)), 3)
)

transferencias <- transferencias[, c("estudio", "pais", "contexto",
                                     "n", "ee", "efecto")]

# --- Escribir el CSV al lado de este script ----------------------------------
args     <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
dir_base <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida   <- file.path(dir_base, "transferencias.csv")
write.csv(transferencias, salida, row.names = FALSE)
cat("Escrito:", normalizePath(salida), "\n")
cat("Estudios:", nrow(transferencias), "\n")
