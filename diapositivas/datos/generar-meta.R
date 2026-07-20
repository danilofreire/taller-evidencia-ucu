# generar-meta.R
# Genera un conjunto de datos SINTÉTICO de estudios sobre tamaño de la
# legislatura y gasto público, para enseñar meta-análisis y sesgo de
# publicación en el taller de la UCU.
#
# La simulación arma primero la LITERATURA COMPLETA: 40 estudios (autores y
# años ficticios, con sabor regional), cada uno con un efecto estandarizado
# (efecto) y su error estándar (ee). El efecto verdadero es 0.15 con
# heterogeneidad entre estudios (tau = 0.08).
#
# Después aplica el FILTRO DE PUBLICACIÓN: los estudios grandes (ee <= 0.12)
# se publican con cualquier resultado; los chicos, sólo si salieron
# significativos y positivos (efecto / ee > 1.64). El CSV guarda únicamente
# los publicados. Eso hace que (semilla 27):
#   - se publiquen 21 de los 40 (12 grandes + 9 chicos inflados)
#   - el estimador combinado suba a 0.20 (arriba del 0.15 verdadero)
#   - el funnel quede asimétrico y el test de Egger dé p = 0.004
#   - al quedarse sólo con los grandes, baje a 0.15
#   - la literatura completa (con el cajón abierto) dé 0.15
#
# El generador usa fabricatr y es EXACTAMENTE el mismo que aparece en
# 05-acumulacion.qmd (slide "Generá los datos", con los huecos completos)
# y en codigo/practica-05.R. La semilla fija garantiza datos idénticos.

library(fabricatr)
suppressPackageStartupMessages(library(dplyr))

set.seed(27)

apellidos <- c("García", "Souza", "Lima", "Rojas", "Fernández",
               "Muñoz", "Silva", "Torres", "Vargas", "Herrera",
               "Castro", "Mendoza", "Ríos", "Acosta", "Núñez",
               "Ferreira", "Ortiz", "Cabrera", "Duarte", "Sosa")
paises <- c("Brasil", "México", "Chile", "Colombia", "Perú",
            "Argentina", "Uruguay")

literatura <- fabricate(
  N = 40,
  autor   = sample(apellidos, N, replace = TRUE),
  anio    = sample(2005:2022, N, replace = TRUE),
  estudio = paste0(autor, " (", anio, ")"),
  pais    = sample(paises, N, replace = TRUE),
  diseno  = sample(c("RDD", "DiD", "OLS"), N, replace = TRUE),
  # muestras de 60 a 600 personas
  n       = round(exp(runif(N, log(60), log(600)))),
  # cuanto más grande el estudio, más chico su error
  ee      = round(2.2 / sqrt(n), 3),
  # el efecto verdadero es 0.15 para todos; lo demás es ruido
  efecto  = round(rnorm(N, 0.15, sqrt(0.08^2 + ee^2)), 3)
)

# el filtro de publicación: los grandes entran siempre;
# los chicos, sólo si salieron significativos
datos <- literatura |>
  filter(ee <= 0.12 | efecto / ee > 1.64)

datos <- datos[, c("estudio", "pais", "diseno", "n", "ee", "efecto")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "meta_estudios.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas (de",
    nrow(literatura), "estudios simulados)\n")

# --- Chequeo rápido -------------------------------------------------------
suppressPackageStartupMessages(library(metafor))
m <- rma(yi = efecto, sei = ee, data = datos)
cat(sprintf("combinado publicados = %.3f | Egger p = %.3f | grandes = %.3f\n",
            m$beta[1], regtest(m)$pval,
            rma(yi = efecto, sei = ee,
                data = subset(datos, ee <= 0.12))$beta[1]))
