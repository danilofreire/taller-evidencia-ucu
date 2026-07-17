# generar-municipios.R
# Genera un conjunto de datos SINTÉTICO de municipios brasileños para
# enseñar regresión discontinua (RDD) en el taller de la UCU.
#
# Basado en el diseño de Mignozzetti, Cepaluni y Freire (2025, AJPS,
# "Legislature Size and Welfare: Evidence from Brazil", doi:10.1111/ajps.12843).
#
# Institución que crea la discontinuidad: en marzo de 2004 el Tribunal
# Superior Electoral fijó el tamaño de los concejos municipales por tramos
# de población (9 bancas, más una cada 47.619 habitantes, hasta 21).
# Acá simulamos UN umbral (el primero: 9 -> 10 concejales) para un RDD
# sharp limpio. Los efectos por concejal replican los del paper.
#
# El generador usa fabricatr (la cocina de DeclareDesign) y es EXACTAMENTE
# el mismo que aparece en 03-rdd.qmd (slide "Generá los datos del caso",
# con los huecos completos) y en codigo/practica-03.R. La semilla fija
# garantiza que los tres produzcan datos idénticos a municipios.csv.

library(fabricatr)

set.seed(148)
datos <- fabricate(
  N = 2000,
  poblacion      = round(runif(N, 20000, 75000)),
  poblacion_c    = poblacion - 47619,
  x              = poblacion_c / 10000,            # tendencia, en decenas de miles
  concejo_grande = as.integer(poblacion_c >= 0),   # la regla asigna, no un sorteo
  n_concejales   = 9 + concejo_grande,
  pib_pc         = round(12 + 0.8 * x + rnorm(N, 0, 2), 1),
  prop_pobres    = round(pmin(pmax(0.34 - 0.02 * x + rnorm(N, 0, 0.05), 0.05), 0.75), 3),
  bancas_2000    = 9 + rbinom(N, 1, 0.15),
  mortalidad_infantil     = pmax(2,   round(20 - 0.5 * x - 2.01 * concejo_grande + rnorm(N, 0, 2.5), 1)),
  mortalidad_postneonatal = pmax(0.5, round(7 - 0.2 * x - 0.9 * concejo_grande + rnorm(N, 0, 1.3), 1)),
  matricula_primaria      = pmax(0,   round(24 + 0.4 * x + 2.58 * concejo_grande + rnorm(N, 0, 3), 1)),
  ideb           = pmin(10, pmax(0, round(4.5 + 0.1 * x + rnorm(N, 0, 0.5), 1))),
  burocratas     = pmax(0, round(480 + 12 * x + 104 * concejo_grande + rnorm(N, 0, 45))),
  proyectos_servicios = pmax(0, round(40 + 2 * x + 6 * concejo_grande + rnorm(N, 0, 7)))
)
datos$municipio <- sprintf("M%04d", seq_len(nrow(datos)))
datos <- datos[, c("municipio", "poblacion", "poblacion_c", "concejo_grande",
                   "n_concejales", "pib_pc", "prop_pobres", "bancas_2000",
                   "mortalidad_infantil", "mortalidad_postneonatal",
                   "matricula_primaria", "ideb", "burocratas",
                   "proyectos_servicios")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "municipios.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Chequeo rápido de los saltos (medias justo arriba vs justo abajo) ---
cerca  <- abs(datos$poblacion_c) < 4000
arriba <- cerca & datos$concejo_grande == 1L
abajo  <- cerca & datos$concejo_grande == 0L
resultados <- c("mortalidad_infantil", "mortalidad_postneonatal",
                "matricula_primaria", "ideb", "burocratas", "proyectos_servicios")
for (v in resultados) {
  salto <- mean(datos[[v]][arriba]) - mean(datos[[v]][abajo])
  cat(sprintf("  salto en %-24s = %+.2f\n", v, salto))
}
