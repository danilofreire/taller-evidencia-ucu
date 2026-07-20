# generar-gsynth-ejemplo.R
# Genera la figura del control sintético GENERALIZADO (gsynth) que se muestra
# en 04-did-sintetico.qmd, en la slide "gsynth en acción".
#
# La idea es un panel donde el método CLÁSICO no sirve: hay DOS unidades
# tratadas y con fechas de adopción DISTINTAS (adopción escalonada). gsynth
# estima un solo efecto promedio (ATT) usando modelos de efectos fijos
# interactivos, y su gráfico de "gap" muestra la brecha promedio en el tiempo.
#
# El mismo generador (fabricatr, semilla 20) aparece, resumido, en la slide.
# El efecto es constante (-12 mientras la política está activa) para que el
# código se lea fácil. La semilla fija hace la figura reproducible.

library(fabricatr)
suppressPackageStartupMessages({
  library(gsynth)
  library(ggplot2)
})

set.seed(20)
panel <- fabricate(
  estado = add_level(
    N = 20,
    inicio = c(2004, 2007, rep(NA, 18)),   # año de la política; NA = nunca
    nivel  = runif(N, 15, 45)              # nivel base de homicidios
  ),
  periodo = add_level(
    N = 16,
    ano = 1998:2013,
    tratado = as.integer(!is.na(inicio) & ano >= inicio),  # 1 si ya tratado
    y = pmax(0, round(nivel + 0.6 * (ano - 1998) - 12 * tratado
                      + rnorm(N, 0, 1.5), 1))
  )
)

# --- gsynth: efectos fijos interactivos, varios tratados, adopción escalonada -
out <- gsynth(y ~ tratado, data = panel,
              index = c("estado", "ano"),
              force = "two-way",   # efectos fijos de estado y de año
              CV = TRUE, r = c(0, 3),  # elige el número de factores por validación cruzada
              se = TRUE, inference = "parametric",
              nboots = 200, parallel = FALSE)

cat("ATT promedio (post):", round(out$att.avg, 2), "\n")

# --- Gráfico del gap (brecha estimada, promedio de las tratadas) --------------
g <- plot(out, type = "gap",
          main = "",
          xlab = "Años desde el tratamiento",
          ylab = "Homicidios: tratado - contrafactual") +
  theme_minimal(base_size = 15)

args     <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
dir_base <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida   <- file.path(dir_base, "..", "figures", "gsynth-ejemplo.png")
ggsave(salida, g, width = 7.5, height = 4.6, dpi = 300)
cat("Escrito:", normalizePath(salida), "\n")
