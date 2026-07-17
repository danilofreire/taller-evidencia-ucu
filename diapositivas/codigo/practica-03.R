# Práctica del Bloque 2: regresión discontinua
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Estos son los chunks centrales de 03-rdd.qmd, en el mismo orden, más los
# tres ejercicios de la práctica. Corré cada bloque, mirá el resultado y
# después intentá los ejercicios (las soluciones están en el apéndice del deck).
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Datos y paquetes -------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("estimatr", "rdrobust", "rddensity", "ggplot2")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

set.seed(2026)

# Los datos del caso de los concejos (simulados; un umbral en 47.619 hab)
datos <- read.csv("datos/municipios.csv")
# Leélo directo desde la web, sin descargar nada:
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/municipios.csv")

# --- 2. Gemelos en el umbral ---------------------------------------------
# Cerca del corte, arriba y abajo son casi idénticos en lo predeterminado.
cerca <- subset(datos, abs(poblacion_c) < 4000)
aggregate(cbind(pib_pc, prop_pobres, bancas_2000, n_concejales)
          ~ concejo_grande, data = cerca, FUN = mean)

# --- 3. Ver el salto con rdplot ------------------------------------------
# rdplot elige los bins de forma óptima y ajusta un polinomio a cada lado.
rdplot(datos$mortalidad_infantil, datos$poblacion_c,
       x.label = "Población centrada en el umbral",
       y.label = "Mortalidad infantil (por mil)", title = "")

# --- 4. Comparación ingenua vs local -------------------------------------
# La diferencia entre todos los grandes y todos los chicos arrastra la tendencia.
with(datos, mean(mortalidad_infantil[concejo_grande == 1]) -
            mean(mortalidad_infantil[concejo_grande == 0]))

cerca <- subset(datos, abs(poblacion_c) < 8000)
fit <- lm_robust(mortalidad_infantil ~ concejo_grande + poblacion_c +
                   concejo_grande:poblacion_c, data = cerca)
round(coef(summary(fit))["concejo_grande", 1:4], 3)

# --- 5. Estimarlo bien: rdrobust -----------------------------------------
# Ancho de banda óptimo y corrección del sesgo de los bordes.
rd <- rdrobust(datos$mortalidad_infantil, datos$poblacion_c)
round(c(efecto = rd$coef[1], se = rd$se[1],
        p = rd$pv[1], ancho = rd$bws[1, 1]), 3)

# --- 6. Los chequeos de validez ------------------------------------------
# a) manipulación: test de densidad (¿amontonamiento en el corte?)
dens <- rddensity(datos$poblacion_c)
round(c(T = dens$test$t_jk, p = dens$test$p_jk), 3)

# b) balance: las covariables previas no deberían saltar
sapply(c("pib_pc", "prop_pobres"), function(v) {
  m <- rdrobust(datos[[v]], datos$poblacion_c)
  round(m$pv[1], 3)
})

# c) corte placebo: un umbral falso donde no debería pasar nada
rdrobust(datos$mortalidad_infantil, datos$poblacion_c, c = -15000)$pv[1]

# --- 7. Ejercicio 1: estimá otro resultado -------------------------------
# Repetí el análisis sobre la matrícula primaria (matricula_primaria):
# gráfico del salto y efecto con rdrobust.
plot(datos$poblacion_c, datos$matricula_primaria, pch = 20, cex = 0.5)
abline(v = 0, col = "red")

rd1 <- rdrobust(datos$matricula_primaria, datos$poblacion_c)
round(c(efecto = rd1$coef[1], p = rd1$pv[1]), 3)

# --- 8. Ejercicio 2: poné el diseño a prueba -----------------------------
# Densidad, corte placebo y balance, ahora para tu estimación.
c(densidad = round(rddensity(datos$poblacion_c)$test$p_jk, 3),
  placebo  = round(rdrobust(datos$matricula_primaria, datos$poblacion_c,
                            c = -15000)$pv[1], 3),
  balance  = round(rdrobust(datos$pib_pc, datos$poblacion_c)$pv[1], 3))

# --- 9. Ejercicio 3: ingenua vs local ------------------------------------
# ¿Por qué difieren los dos números? ¿Cuál reportarías y por qué?
ingenua <- with(datos, mean(matricula_primaria[concejo_grande == 1]) -
                       mean(matricula_primaria[concejo_grande == 0]))
local   <- rdrobust(datos$matricula_primaria, datos$poblacion_c)$coef[1]
round(c(ingenua = ingenua, local = local), 2)
