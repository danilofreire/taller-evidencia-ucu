# generar-homicidios.R
# Genera un panel SINTÉTICO estado-año de tasas de homicidio, para enseñar
# diferencias en diferencias y control sintético en el taller de la UCU.
#
# Inspirado (a grandes rasgos) en la evaluación de estrategias de prevención
# de homicidios en São Paulo, Brasil (Freire, 2018, LARR, doi:10.25222/larr.334).
# Unidad: estado-año, 16 estados x 16 años (1990 a 2005), 256 filas.
# São Paulo es la unidad tratada: desde el año 2000 una política hace caer su
# tasa mientras los demás estados (donantes) siguen su tendencia.
#
# El generador usa fabricatr (paneles con add_level) y es EXACTAMENTE el
# mismo que aparece en 04-did-sintetico.qmd (slide "Generá los datos del
# caso", con los huecos completos) y en codigo/practica-04.R. La semilla
# fija garantiza que los tres produzcan datos idénticos a homicidios.csv.
#
# Números que la práctica reproduce (semilla 1):
#   - DiD 2x2 (SP vs promedio de donantes, pre/post 2000): -12,5
#   - control sintético: buen ajuste pre-1999 y brecha de ~ -15 en 2005
#   - placebo (Paraná como "tratado"): brecha cercana a cero

library(fabricatr)

set.seed(1)
datos <- fabricate(
  unidad = add_level(
    N = 16,
    estado  = c("São Paulo", "Rio de Janeiro", "Minas Gerais", "Bahia",
                "Paraná", "Rio Grande do Sul", "Pernambuco", "Ceará", "Pará",
                "Santa Catarina", "Goiás", "Maranhão", "Espírito Santo",
                "Paraíba", "Amazonas", "Mato Grosso"),
    tratado = as.integer(estado == "São Paulo"),
    base    = ifelse(tratado == 1, 30, runif(N, 15, 45)),
    pib_pc  = round(rlnorm(N, log(12000), 0.25)),
    gini    = round(runif(N, 0.48, 0.62), 3),
    poblacion_urbana = round(runif(N, 0.55, 0.95), 3),
    jovenes_pct      = round(runif(N, 0.16, 0.24), 3)
  ),
  periodo = add_level(
    N = 16,
    anio   = 1990:2005,
    post   = as.integer(anio >= 2000),
    efecto = -14.5 * (1 - exp(-(anio - 1999))) * tratado * post,
    tasa_homicidios = pmax(0, round(base + 0.8 * (anio - 1990)
                                    + efecto + rnorm(N, 0, 1.5), 1))
  )
)
datos <- datos[, c("estado", "anio", "tasa_homicidios", "pib_pc", "gini",
                   "poblacion_urbana", "jovenes_pct", "tratado", "post")]

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "homicidios.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Chequeo rápido: el 2x2 de DiD ----------------------------------------
agg <- aggregate(tasa_homicidios ~ tratado + post, datos, mean)
did <- (agg$tasa[agg$tratado == 1 & agg$post == 1] -
        agg$tasa[agg$tratado == 1 & agg$post == 0]) -
       (agg$tasa[agg$tratado == 0 & agg$post == 1] -
        agg$tasa[agg$tratado == 0 & agg$post == 0])
cat("DiD 2x2 =", round(did, 2), "\n")   # esperado ~ -12,5
