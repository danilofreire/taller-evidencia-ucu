# Práctica del Bloque 1: experimentos
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Estos son exactamente los chunks de la práctica de 02-experimentos.qmd,
# en el mismo orden. Corré cada bloque, mirá el resultado y después
# probá las modificaciones que están al final.
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Paquetes ---------------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("tidyverse", "estimatr", "fabricatr", "randomizr")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --- 2. Generá los datos -------------------------------------------------
# Los datos son simulados con fabricatr (la cocina de DeclareDesign).
# La semilla fija hace que salgan idénticos al CSV del repo.
set.seed(3043)
datos <- fabricate(
  N = 400,
  edad            = round(rnorm(N, 38, 7.5)),
  mujer           = rbinom(N, 1, 0.7),
  experiencia     = round(rnorm(N, 7, 2.6)),
  zona            = sample(c("urbana", "rural"), N, replace = TRUE),
  visitas_previas = round(rnorm(N, 35, 6)),
  tratamiento     = complete_ra(N, m = 200),
  visitas = round(20 + 4.5 * tratamiento
                  + 3 * tratamiento * (zona == "urbana")
                  + 0.4 * visitas_previas + rnorm(N, 0, 9)),
  indice_larvario = round(pmax(0, 12 - 2.8 * tratamiento + rnorm(N, 0, 4)), 1)
)
datos$id_agente <- seq_len(nrow(datos))
datos <- datos[, c("id_agente", "tratamiento", "edad", "mujer", "experiencia",
                   "zona", "visitas_previas", "visitas", "indice_larvario")]

# ¿Preferís bajar el CSV ya armado, sin generarlo? Descomentá:
# datos <- read.csv("datos/dengue_incentivos.csv")
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/dengue_incentivos.csv")

# --- 3. Mirar los datos --------------------------------------------------
glimpse(datos)

datos |>
  group_by(tratamiento) |>
  summarise(n = n(),
            visitas = mean(visitas),
            indice_larvario = mean(indice_larvario))

# --- 4. La diferencia de medias ------------------------------------------
# El incentivo se sorteó, así que la diferencia de medias ya es el efecto.
difference_in_means(visitas ~ tratamiento, data = datos)

# --- 5. La regresión da lo mismo -----------------------------------------
# El coeficiente de tratamiento coincide con la diferencia de medias,
# pero la regresión escala: controles, interacciones, efectos fijos.
ajuste_visitas <- lm_robust(visitas ~ tratamiento, data = datos)
summary(ajuste_visitas)

# --- 6. Tabla de balance -------------------------------------------------
# Un buen sorteo deja los grupos parejos antes del tratamiento.
# 1. un modelo por covariable: ¿difiere entre tratados y control?
b_edad    <- lm_robust(edad ~ tratamiento, data = datos)
b_exp     <- lm_robust(experiencia ~ tratamiento, data = datos)
b_previas <- lm_robust(visitas_previas ~ tratamiento, data = datos)

# 2. juntamos la diferencia (coef. de tratamiento) y su valor p en una tabla
balance <- data.frame(
  variable = c("edad", "experiencia", "visitas_previas"),
  dif      = round(c(b_edad$coefficients["tratamiento"],
                     b_exp$coefficients["tratamiento"],
                     b_previas$coefficients["tratamiento"]), 2),
  p_valor  = round(c(b_edad$p.value["tratamiento"],
                     b_exp$p.value["tratamiento"],
                     b_previas$p.value["tratamiento"]), 3)
)
balance
# Mirá `experiencia`: sale significativa aunque el tratamiento se sorteó.
# ¿Cuántas covariables esperás que salgan significativas solo por azar?

# --- 7. Controles: precisión, no sesgo -----------------------------------
# Sumar covariables pre-tratamiento no corrige sesgo (no hay), pero puede
# achicar el error estándar del efecto.
ajuste_ctrl <- lm_robust(visitas ~ tratamiento + edad + visitas_previas,
                         data = datos)
summary(ajuste_ctrl)

# --- 8. Probá vos: ¿para quién funciona? ---------------------------------
# Completá el operador que pide la interacción entre tratamiento y zona.
# ¿Qué signo tiene la interacción y qué dice sobre dónde rinde más el incentivo?
ajuste_hetero <- lm_robust(visitas ~ tratamiento * zona, data = datos)
summary(ajuste_hetero)
