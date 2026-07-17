# generar-homicidios.R
# Genera un panel SINTÉTICO estado-año de tasas de homicidio, para enseñar
# diferencias en diferencias y control sintético en el taller de la UCU.
#
# Inspirado (a grandes rasgos) en la evaluación de estrategias de prevención
# de homicidios en São Paulo, Brasil. Unidad: estado-año, 16 estados x 16
# años (1990 a 2005), 256 filas. São Paulo es la unidad tratada: desde el
# año 2000 una política hace caer su tasa de homicidios, mientras los demás
# estados (donantes) siguen su tendencia. Todos los datos son simulados.
#
# Estructura del efecto: el tratamiento arranca en 2000 y crece durante los
# primeros años hasta estabilizarse, de modo que la brecha con el
# contrafactual se ensancha con el tiempo (justo lo que el control sintético
# muestra). Números que la práctica reproduce:
#   - DiD 2x2 (SP vs promedio de donantes, pre/post 1999): ~ -11
#   - control sintético: buen ajuste pre-1999 y brecha de ~ -15/-16 hacia 2005
#   - placebo (un donante como "tratado"): brecha cercana a cero

set.seed(2026)

estados <- c(
  "São Paulo",
  "Rio de Janeiro", "Minas Gerais", "Bahia", "Paraná", "Rio Grande do Sul",
  "Pernambuco", "Ceará", "Pará", "Santa Catarina", "Goiás", "Maranhão",
  "Espírito Santo", "Paraíba", "Amazonas", "Mato Grosso"
)
anios <- 1990:2005
n_est <- length(estados)          # 16
n_an  <- length(anios)            # 16

# --- Niveles y tendencia -------------------------------------------------
# Tendencia común suave de +0.8 por año. São Paulo arranca en un nivel medio
# (base 30) para que el control sintético lo pueda reconstruir con los
# donantes; los donantes toman una base Uniforme(15, 45).
base_sp     <- 30
base_donant <- runif(n_est - 1, 15, 45)
base        <- c(base_sp, base_donant)
tendencia   <- 0.8

# --- Efecto del tratamiento (sólo São Paulo, desde el año 2000) ----------
# Cae rápido y se estabiliza cerca de -14.5: brecha en torno a -15 en 2005,
# DiD 2x2 ~ -11. La brecha se ensancha en los primeros años post-2000.
efecto_sp <- function(anio) {
  ifelse(anio < 2000, 0, -14.5 * (1 - exp(-1.0 * (anio - 1999))))
}

# --- Construcción del panel con ruido AR(1) por estado -------------------
filas <- vector("list", n_est)
for (i in seq_len(n_est)) {
  t   <- anios - 1990
  mu  <- base[i] + tendencia * t
  if (i == 1) mu <- mu + efecto_sp(anios)   # São Paulo tratado

  # ruido AR(1) suave, sd ~ 1.5
  e <- numeric(n_an); e[1] <- rnorm(1, 0, 1.5)
  for (k in 2:n_an) e[k] <- 0.5 * e[k - 1] + rnorm(1, 0, 1.5 * sqrt(1 - 0.5^2))
  tasa <- pmax(round(mu + e, 1), 0)

  filas[[i]] <- data.frame(
    estado           = estados[i],
    anio             = anios,
    tasa_homicidios  = tasa,
    # covariables predeterminadas: invariantes en el tiempo + ruido chico
    pib_pc           = round(rlnorm(1, log(12000), 0.25) * exp(rnorm(n_an, 0, 0.02))),
    gini             = round(runif(1, 0.48, 0.62) + rnorm(n_an, 0, 0.004), 3),
    poblacion_urbana = round(runif(1, 0.55, 0.95) + rnorm(n_an, 0, 0.004), 3),
    jovenes_pct      = round(runif(1, 0.16, 0.24) + rnorm(n_an, 0, 0.003), 3),
    tratado          = as.integer(i == 1),
    post             = as.integer(anios >= 2000),
    stringsAsFactors = FALSE
  )
}
datos <- do.call(rbind, filas)

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "homicidios.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Verificación (descomentar para volver a chequear) --------------------
# # DiD 2x2 (São Paulo vs promedio de donantes, cortando en 1999/2000)
# agg <- aggregate(tasa_homicidios ~ tratado + post, datos, mean)
# did <- (agg$tasa[agg$tratado==1 & agg$post==1] - agg$tasa[agg$tratado==1 & agg$post==0]) -
#        (agg$tasa[agg$tratado==0 & agg$post==1] - agg$tasa[agg$tratado==0 & agg$post==0])
# cat("DiD 2x2 =", round(did, 1), "\n")   # esperado entre -10 y -15
#
# # Control sintético con tidysynth
# library(tidysynth); library(dplyr)
# sc <- datos |>
#   synthetic_control(outcome = tasa_homicidios, unit = estado, time = anio,
#                     i_unit = "São Paulo", i_time = 1999, generate_placebos = TRUE) |>
#   generate_predictor(time_window = 1990:1999,
#                      pib = mean(pib_pc), gini_m = mean(gini),
#                      urb = mean(poblacion_urbana), jov = mean(jovenes_pct)) |>
#   generate_predictor(time_window = 1990:1999, tasa_pre = mean(tasa_homicidios)) |>
#   generate_weights() |>
#   generate_control()
# sc |> grab_synthetic_control() |> tail(1)      # brecha ~ -14 en 2005
# sc |> plot_trends(); sc |> plot_differences()
