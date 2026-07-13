# generar-municipios.R
# Genera un conjunto de datos SINTÉTICO de municipios brasileños para
# enseñar regresión discontinua (RDD) en el taller de la UCU.
#
# Basado en el diseño de Mignozzetti, Cepaluni y Freire (2024, AJPS,
# "Legislature Size and Welfare: Evidence from Brazil", doi:10.1111/ajps.12843).
#
# Institución que crea la discontinuidad: en marzo de 2004 el Tribunal
# Superior Electoral fijó el tamaño de los concejos municipales por tramos
# de población. Cada concejo arranca con 9 bancas y suma una banca cada
# 47.619 habitantes, hasta 21 (12 umbrales en total: 47.619; 95.238; ...).
# La variable de asignación es la población proyectada de 2003 (IBGE),
# predeterminada, así que los municipios no pueden autoseleccionarse.
#
# Acá simulamos UN umbral (el primero, 47.619 hab: 9 -> 10 concejales)
# para un RDD sharp limpio. Los efectos por concejal replican los del
# paper: mortalidad infantil -2,01/1000; postneonatal -0,90/1000;
# matrícula primaria +2,58 chicos/aula; sin cambio en calidad (IDEB);
# +104 burócratas designados; ~15% más de proyectos de servicios.
# Densidad suave (sin manipulación); covariables continuas en el corte
# (sirven de placebo y mejoran la precisión).

set.seed(2024)

n      <- 2000
umbral <- 47619   # primer corte del TSE: 9 -> 10 concejales

# --- Variable de asignación: población proyectada 2003, en torno al corte ---
poblacion   <- round(runif(n, 20000, 75000))
poblacion_c <- poblacion - umbral
x           <- poblacion_c / 10000   # en decenas de miles, para las tendencias

# --- Tratamiento (sharp): cruzar el umbral suma una banca (9 -> 10) ---
concejo_grande <- as.integer(poblacion >= umbral)
n_concejales   <- 9L + concejo_grande

# --- Covariables predeterminadas, continuas en el corte (placebos) ---
region <- sample(
  c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"),
  n, replace = TRUE, prob = c(.10, .30, .15, .30, .15)
)
nordeste    <- as.integer(region == "Nordeste")
pib_pc      <- round(12 + 0.8 * x + rnorm(n, 0, 2.0), 1)              # PIB per cápita (miles R$)
prop_pobres <- round(pmin(pmax(0.34 - 0.02 * x + rnorm(n, 0, 0.05), 0.05), 0.75), 3)
bancas_2000 <- 9L + rbinom(n, 1, 0.15)                               # bancas del período previo

# --- Tendencia suave compartida en la variable de asignación ---
# (el confusor que el RDD neutraliza al comparar sólo cerca del corte)
f <- function(x) x

# --- Resultados de bienestar (salto verdadero tau sólo en el umbral) ---
mortalidad_infantil     <- 20.0 - 0.50 * f(x) - 2.01 * concejo_grande + rnorm(n, 0, 2.5)
mortalidad_postneonatal <-  7.0 - 0.20 * f(x) - 0.90 * concejo_grande + rnorm(n, 0, 1.3)
matricula_primaria      <- 24.0 + 0.40 * f(x) + 2.58 * concejo_grande + rnorm(n, 0, 3.0)
ideb                    <-  4.5 + 0.10 * f(x) + 0.00 * concejo_grande + rnorm(n, 0, 0.5)
burocratas              <-  480 + 12.0 * f(x) + 104  * concejo_grande + rnorm(n, 0, 45)
proyectos_servicios     <-   40 +  2.0 * f(x) +   6  * concejo_grande + rnorm(n, 0, 7)

# --- Límites realistas y redondeo ---
mortalidad_infantil     <- pmax(2,  round(mortalidad_infantil, 1))
mortalidad_postneonatal <- pmax(0.5, round(mortalidad_postneonatal, 1))
matricula_primaria      <- pmax(0,  round(matricula_primaria, 1))
ideb                    <- pmin(10, pmax(0, round(ideb, 1)))
burocratas              <- pmax(0,  round(burocratas, 0))
proyectos_servicios     <- pmax(0,  round(proyectos_servicios, 0))

datos <- data.frame(
  municipio               = sprintf("M%04d", seq_len(n)),
  region                  = region,
  nordeste                = nordeste,
  poblacion               = poblacion,
  poblacion_c             = poblacion_c,
  umbral                  = umbral,
  concejo_grande          = concejo_grande,
  n_concejales            = n_concejales,
  pib_pc                  = pib_pc,
  prop_pobres             = prop_pobres,
  bancas_2000             = bancas_2000,
  mortalidad_infantil     = mortalidad_infantil,
  mortalidad_postneonatal = mortalidad_postneonatal,
  matricula_primaria      = matricula_primaria,
  ideb                    = ideb,
  burocratas              = burocratas,
  proyectos_servicios     = proyectos_servicios,
  stringsAsFactors        = FALSE
)

# --- Guardar junto al script, sin importar el directorio de trabajo ---
args       <- commandArgs(trailingOnly = FALSE)
file_arg   <- sub("^--file=", "", args[grep("^--file=", args)])
dir_salida <- if (length(file_arg)) dirname(normalizePath(file_arg)) else getwd()
salida     <- file.path(dir_salida, "municipios.csv")
write.csv(datos, salida, row.names = FALSE)

cat("Escrito:", salida, "con", nrow(datos), "filas\n")

# --- Chequeo rápido de los saltos (medias justo arriba vs justo abajo) ---
cerca  <- abs(poblacion_c) < 4000
arriba <- cerca & concejo_grande == 1L
abajo  <- cerca & concejo_grande == 0L
resultados <- c("mortalidad_infantil", "mortalidad_postneonatal",
                "matricula_primaria", "ideb", "burocratas", "proyectos_servicios")
for (v in resultados) {
  salto <- mean(datos[[v]][arriba]) - mean(datos[[v]][abajo])
  cat(sprintf("  salto en %-24s = %+.2f\n", v, salto))
}
