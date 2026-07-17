# Práctica del Bloque 2: regresión discontinua
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Estos son los chunks centrales de 03-rdd.qmd, en el mismo orden, más los
# tres ejercicios de la práctica. Corré cada bloque, mirá el resultado y
# después intentá los ejercicios (las soluciones están en el apéndice del deck).
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Paquetes ---------------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("tidyverse", "estimatr", "rdrobust", "rddensity", "fabricatr")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --- 2. Generá los datos -------------------------------------------------
# Los datos son simulados con fabricatr sobre el diseño del paper: un solo
# corte (47.619 habitantes, 9 -> 10 bancas) y efectos calcados de la
# investigación real. La semilla fija hace que salgan idénticos al CSV.
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

# ¿Preferís bajar el CSV ya armado, sin generarlo? Descomentá:
# datos <- read.csv("datos/municipios.csv")
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/municipios.csv")

# --- 3. Gemelos en el umbral ---------------------------------------------
# Cerca del corte, arriba y abajo son casi idénticos en lo predeterminado.
datos |>
  filter(abs(poblacion_c) < 4000) |>
  group_by(concejo_grande) |>
  summarise(across(c(pib_pc, prop_pobres, bancas_2000, n_concejales), mean))

# --- 4. Ver el salto con rdplot ------------------------------------------
# rdplot elige los bins de forma óptima y ajusta un polinomio a cada lado.
rdplot(datos$mortalidad_infantil, datos$poblacion_c,
       x.label = "Población centrada en el umbral",
       y.label = "Mortalidad infantil (por mil)", title = "")

# --- 5. Comparación ingenua vs local -------------------------------------
# La diferencia entre todos los grandes y todos los chicos arrastra la tendencia.
with(datos, mean(mortalidad_infantil[concejo_grande == 1]) -
            mean(mortalidad_infantil[concejo_grande == 0]))

cerca <- filter(datos, abs(poblacion_c) < 8000)
fit <- lm_robust(mortalidad_infantil ~ concejo_grande + poblacion_c +
                   concejo_grande:poblacion_c, data = cerca)
round(coef(summary(fit))["concejo_grande", 1:4], 3)

# --- 6. Estimarlo bien: rdrobust -----------------------------------------
# Ancho de banda óptimo y corrección del sesgo de los bordes.
rd <- rdrobust(datos$mortalidad_infantil, datos$poblacion_c)
round(c(efecto = rd$coef[1], se = rd$se[1],
        p = rd$pv[1], ancho = rd$bws[1, 1]), 3)

# --- 7. Los chequeos de validez ------------------------------------------
# a) manipulación: test de densidad (¿amontonamiento en el corte?)
dens <- rddensity(datos$poblacion_c)
round(c(T = dens$test$t_jk, p = dens$test$p_jk), 3)

# b) balance: las covariables previas no deberían saltar
# 1. un RDD por covariable; 2. juntamos salto y valor p en una tabla
b_pib    <- rdrobust(datos$pib_pc, datos$poblacion_c)
b_pobres <- rdrobust(datos$prop_pobres, datos$poblacion_c)

balance <- data.frame(
  variable = c("pib_pc", "prop_pobres"),
  salto    = round(c(b_pib$coef[1], b_pobres$coef[1]), 3),
  p_valor  = round(c(b_pib$pv[1], b_pobres$pv[1]), 3)
)
balance

# c) corte placebo: un umbral falso donde no debería pasar nada
rdrobust(datos$mortalidad_infantil, datos$poblacion_c, c = -15000)$pv[1]

# --- 8. Ejercicio 1: estimá otro resultado -------------------------------
# Repetí el análisis sobre la matrícula primaria (matricula_primaria):
# gráfico del salto (a mano y con rdplot) y efecto con rdrobust.
plot(datos$poblacion_c, datos$matricula_primaria, pch = 20, cex = 0.5)
abline(v = 0, col = "red")

rdplot(datos$matricula_primaria, datos$poblacion_c,
       x.label = "Población centrada en el umbral",
       y.label = "Matrícula primaria (chicos por aula)", title = "")

rd1 <- rdrobust(datos$matricula_primaria, datos$poblacion_c)
round(c(efecto = rd1$coef[1], p = rd1$pv[1]), 3)

# --- 9. Ejercicio 2: poné el diseño a prueba -----------------------------
# Densidad, corte placebo y balance, ahora para tu estimación.
c(densidad = round(rddensity(datos$poblacion_c)$test$p_jk, 3),
  placebo  = round(rdrobust(datos$matricula_primaria, datos$poblacion_c,
                            c = 12000)$pv[1], 3),
  balance  = round(rdrobust(datos$pib_pc, datos$poblacion_c)$pv[1], 3))
# ¿Probaste el placebo en -15000? Da p ~ 0,03: con muchos cortes falsos,
# alguno cae por azar. Un placebo aislado no voltea el diseño; un patrón sí.

# --- 10. Ejercicio 3: ingenua vs local -----------------------------------
# ¿Por qué difieren los números? ¿Cuál reportarías y por qué?
ingenua <- with(datos, mean(matricula_primaria[concejo_grande == 1]) -
                       mean(matricula_primaria[concejo_grande == 0]))

# regresión local a mano: * pide las dos pendientes más el salto
cerca <- filter(datos, abs(poblacion_c) < 8000)
local_lm <- lm_robust(matricula_primaria ~ concejo_grande * poblacion_c,
                      data = cerca)$coefficients[["concejo_grande"]]

local_rd <- rdrobust(datos$matricula_primaria, datos$poblacion_c)$coef[1]
round(c(ingenua = ingenua, local_lm = local_lm, local_rd = local_rd), 2)
