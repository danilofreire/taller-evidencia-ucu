# Práctica del Bloque 3: diferencias en diferencias y control sintético
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Estos son los chunks centrales de 04-did-sintetico.qmd, en el mismo orden,
# más el ejercicio del placebo. Corré cada bloque, mirá el resultado y después
# intentá el ejercicio (la solución está en el apéndice del deck).
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Paquetes ---------------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("dplyr", "ggplot2", "tidysynth", "fabricatr")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# --- 2. Generá los datos -------------------------------------------------
# El panel del caso (16 estados x 16 años), simulado con fabricatr usando
# add_level: un nivel para los estados, otro para los años. La semilla fija
# hace que salgan idénticos al CSV del repo.
set.seed(1)
datos <- fabricate(
  unidad = add_level(              # nivel 1: los 16 estados
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
  periodo = add_level(             # nivel 2: 16 años por estado
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

# ¿Preferís bajar el CSV ya armado, sin generarlo? Descomentá:
# datos <- read.csv("datos/homicidios.csv")
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/homicidios.csv")

# --- 3. El 2x2 a mano ----------------------------------------------------
# Cuatro promedios: São Paulo y los donantes, antes y después de 2000.
medias <- datos |>
  group_by(tratado, post) |>
  summarise(tasa = mean(tasa_homicidios), .groups = "drop")
medias

# La doble resta: (São Paulo después - antes) - (donantes después - antes)
(27.78 - 33.85) - (40.99 - 34.54)

# --- 4. El control sintético en un pipe ----------------------------------
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

# --- 5. Ver el resultado -------------------------------------------------
# Buen ajuste antes de 2000 y una brecha de unos 15 puntos hacia 2005.
sc |> plot_trends()

# Lo mismo, restando el sintético: la brecha, sola.
sc |> plot_differences()

# ¿Quiénes forman el São Paulo sintético? Los pesos son transparentes.
sc |> grab_unit_weights() |> arrange(desc(weight))

# --- 6. Ejercicio: el placebo --------------------------------------------
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
