# generar-tabaco.R
# Genera un panel SINTÉTICO país-año de prevalencia de tabaquismo, para la
# Tarea 3 del taller de la UCU (bloque de diferencias en diferencias y
# control sintético).
#
# Inspirado (a grandes rasgos) en la política de control del tabaco de
# Uruguay, que desde 2006 aplicó una de las regulaciones más fuertes de la
# región (ambientes libres de humo, advertencias gráficas, impuestos).
# Unidad: país-año, 12 países x 18 años (1996 a 2013), 216 filas.
# Uruguay es la unidad tratada: desde 2006 la política hace caer su
# prevalencia mientras los demás países (donantes) siguen su tendencia.
#
# El generador usa fabricatr (paneles con add_level), igual que el caso de
# los homicidios de São Paulo. La semilla fija (305) garantiza datos
# idénticos a tabaco_panel.csv.
#
# Números que la práctica reproduce (semilla 305):
#   - DiD 2x2 (Uruguay vs promedio de donantes, pre/post 2006): ~ -6
#   - control sintético: buen ajuste pre-2006 y una brecha que se abre
#     hasta ~ -8 puntos porcentuales hacia 2013
#   - placebo (otro país como "tratado"): brecha cercana a cero

library(fabricatr)

set.seed(305)

datos <- fabricate(
  unidad = add_level(
    N = 12,
    pais = c("Uruguay", "Argentina", "Brasil", "Chile", "Paraguay",
             "Bolivia", "Perú", "Colombia", "Ecuador", "México",
             "Costa Rica", "Panamá"),
    tratado = as.integer(pais == "Uruguay"),
    base    = ifelse(tratado == 1, 32, runif(N, 22, 40)),
    pib_pc  = round(rlnorm(N, log(11000), 0.3)),
    urbanizacion = round(runif(N, 0.55, 0.92), 3),
    gasto_salud  = round(runif(N, 4, 9), 1),
    precio_cigarrillos = round(runif(N, 1.5, 4), 2)
  ),
  periodo = add_level(
    N = 18,
    anio = 1996:2013,
    post = as.integer(anio >= 2006),
    efecto = -9 * (1 - exp(-(anio - 2005) / 2.5)) * tratado * post,
    prevalencia = pmax(5, round(base - 0.4 * (anio - 1996)
                                + efecto + rnorm(N, 0, 1), 1))
  )
)
datos <- datos[, c("pais", "anio", "prevalencia", "pib_pc", "urbanizacion",
                   "gasto_salud", "precio_cigarrillos", "tratado", "post")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "tabaco_panel.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Chequeo rápido: el 2x2 de DiD ----------------------------------------
agg <- aggregate(prevalencia ~ tratado + post, datos, mean)
did <- (agg$prevalencia[agg$tratado == 1 & agg$post == 1] -
        agg$prevalencia[agg$tratado == 1 & agg$post == 0]) -
       (agg$prevalencia[agg$tratado == 0 & agg$post == 1] -
        agg$prevalencia[agg$tratado == 0 & agg$post == 0])
cat("DiD 2x2 =", round(did, 2), " [esperado ~ -6]\n")
