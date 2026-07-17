# Práctica del Bloque 3: diferencias en diferencias y control sintético
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Estos son los chunks centrales de 04-did-sintetico.qmd, en el mismo orden,
# más el ejercicio del placebo. Corré cada bloque, mirá el resultado y después
# intentá el ejercicio (la solución está en el apéndice del deck).
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Datos y paquetes -------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("dplyr", "ggplot2", "tidysynth")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

set.seed(2026)

# Los datos del caso de São Paulo (simulados; tratamiento desde el año 2000)
datos <- read.csv("datos/homicidios.csv")
# Leélo directo desde la web, sin descargar nada:
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/homicidios.csv")

# --- 2. El 2x2 a mano ----------------------------------------------------
# Cuatro promedios: São Paulo y los donantes, antes y después de 2000.
medias <- datos |>
  group_by(tratado, post) |>
  summarise(tasa = mean(tasa_homicidios), .groups = "drop")
medias

# La doble resta: (São Paulo después - antes) - (donantes después - antes)
(26.17 - 30.74) - (37.46 - 31.16)

# --- 3. El control sintético en un pipe ----------------------------------
# Cada paso hace una cosa: definir el problema, elegir los predictores
# pre-política, dejar que el método elija los pesos y armar el sintético.
sc <- datos |>
  synthetic_control(outcome = tasa_homicidios,   # resultado
                    unit = estado,               # unidad
                    time = anio,                 # tiempo
                    i_unit = "São Paulo",        # la unidad tratada
                    i_time = 1999,               # último año sin política
                    generate_placebos = TRUE) |> # placebos para inferencia
  generate_predictor(time_window = 1990:1999,    # promedios pre-política
                     pib = mean(pib_pc), gini_m = mean(gini),
                     urb = mean(poblacion_urbana), jov = mean(jovenes_pct)) |>
  generate_predictor(time_window = 1990:1999, tasa_pre = mean(tasa_homicidios)) |>
  generate_weights() |>                          # elige los pesos
  generate_control()                             # arma el sintético

# --- 4. Ver el resultado -------------------------------------------------
# Buen ajuste antes de 2000 y una brecha de unos 15-16 puntos hacia 2005.
sc |> plot_trends()

# Lo mismo, restando el sintético: la brecha, sola.
sc |> plot_differences()

# ¿Quiénes forman el São Paulo sintético? Los pesos son transparentes.
sc |> grab_unit_weights() |> arrange(desc(weight))

# --- 5. Ejercicio: el placebo --------------------------------------------
# Cambiá la unidad tratada por un donante cualquiera (probá "Paraná") y
# volvé a correr el pipe. ¿Qué le pasa a la brecha?
placebo <- datos |>
  filter(estado != "São Paulo") |>            # sacá al tratado real del pool
  synthetic_control(outcome = tasa_homicidios, unit = estado, time = anio,
                    i_unit = "Paraná", i_time = 1999,
                    generate_placebos = FALSE) |>
  generate_predictor(time_window = 1990:1999,
                     pib = mean(pib_pc), gini_m = mean(gini),
                     urb = mean(poblacion_urbana), jov = mean(jovenes_pct)) |>
  generate_predictor(time_window = 1990:1999, tasa_pre = mean(tasa_homicidios)) |>
  generate_weights() |>
  generate_control()

# La brecha queda cerca de cero: el efecto grande aparece sólo donde hubo política
placebo |> plot_trends()
