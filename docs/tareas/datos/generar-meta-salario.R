# generar-meta-salario.R
# Genera un conjunto de datos SINTÉTICO de estudios sobre el efecto del
# salario mínimo en el empleo, para la Tarea 4 del taller de la UCU
# (bloque de acumulación de evidencia).
#
# La simulación arma primero la LITERATURA COMPLETA: 45 estudios (autores y
# años ficticios), cada uno con una elasticidad empleo-salario (`efecto`,
# negativa = el salario mínimo baja el empleo) y su error estándar (`ee`).
# El efecto verdadero depende del MÉTODO del estudio, un patrón real de esta
# literatura: los diseños más creíbles encuentran efectos cercanos a cero,
# los OLS simples encuentran efectos más negativos.
#   OLS          -0.10   panel   -0.04   experimento  -0.01
#
# Después aplica el FILTRO DE PUBLICACIÓN: los estudios grandes (ee <= 0.03)
# se publican con cualquier resultado; los chicos, sólo si encontraron un
# efecto negativo y significativo (efecto / ee < -1.64). El CSV guarda
# únicamente los publicados. Eso deja un funnel asimétrico (falta la esquina
# de los estudios chicos con efectos nulos o positivos), un test de Egger
# que lo detecta, y un promedio publicado más negativo que el verdadero.
#
# El generador usa fabricatr. La semilla fija (63) garantiza datos idénticos
# a meta_salario.csv.

library(fabricatr)
suppressPackageStartupMessages(library(dplyr))

set.seed(63)

apellidos <- c("García", "Souza", "Lima", "Rojas", "Fernández",
               "Muñoz", "Silva", "Torres", "Vargas", "Herrera",
               "Castro", "Mendoza", "Ríos", "Acosta", "Núñez",
               "Ferreira", "Ortiz", "Cabrera", "Duarte", "Sosa",
               "Smith", "Johnson", "Müller", "Rossi", "Kumar")
paises <- c("Estados Unidos", "Brasil", "México", "Reino Unido", "Alemania",
            "Chile", "Argentina", "Sudáfrica", "India", "España")

literatura <- fabricate(
  N = 60,
  autor   = sample(apellidos, N, replace = TRUE),
  anio    = sample(1995:2023, N, replace = TRUE),
  estudio = paste0(autor, " (", anio, ")"),
  pais    = sample(paises, N, replace = TRUE),
  metodo  = sample(c("OLS", "panel", "experimento"), N,
                   replace = TRUE, prob = c(0.4, 0.4, 0.2)),
  # muestras de 200 a 5000 observaciones
  n       = round(exp(runif(N, log(200), log(5000)))),
  # cuanto más grande el estudio, más chico su error
  ee      = round(0.9 / sqrt(n), 3),
  # el efecto verdadero depende del método; lo demás es ruido + heterogeneidad
  mu      = case_when(metodo == "OLS" ~ -0.10,
                      metodo == "panel" ~ -0.04,
                      TRUE ~ -0.01),
  efecto  = round(rnorm(N, mu, sqrt(0.05^2 + ee^2)), 3)
)

# el filtro de publicación: los grandes entran siempre;
# los chicos, sólo si salieron negativos y significativos
datos <- literatura |>
  filter(ee <= 0.03 | efecto / ee < -1.64)

datos <- datos[, c("estudio", "pais", "metodo", "n", "ee", "efecto")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "meta_salario.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas (de",
    nrow(literatura), "estudios simulados)\n")

# --- Chequeo rápido -------------------------------------------------------
suppressPackageStartupMessages(library(metafor))
m <- rma(yi = efecto, sei = ee, data = datos)
grandes <- rma(yi = efecto, sei = ee, data = subset(datos, ee <= 0.03))
completa <- rma(yi = efecto, sei = ee, data = literatura)
cat(sprintf("publicados = %.3f | grandes = %.3f | cajón abierto = %.3f\n",
            m$beta[1], grandes$beta[1], completa$beta[1]))
cat(sprintf("Egger p = %.3f  [esperado bajo] | I^2 = %.1f%%\n",
            regtest(m)$pval, m$I2))
cat("Por método (efecto combinado):\n")
for (mt in c("OLS", "panel", "experimento")) {
  sub <- subset(datos, metodo == mt)
  if (nrow(sub) > 1) {
    mm <- rma(yi = efecto, sei = ee, data = sub)
    cat(sprintf("  %-12s efecto = %+.3f  (k = %d)\n", mt, mm$beta[1], nrow(sub)))
  }
}
