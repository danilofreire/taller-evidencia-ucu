# Práctica de la Parte 5: meta-análisis y sesgo de publicación
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Dos partes:
#   A. la DEMO que hicimos juntos, con la literatura simulada de 40 estudios
#      (los mismos chunks del deck, en el mismo orden)
#   B. los TRES EJERCICIOS, con la literatura nueva de transferencias
#      condicionadas. Las respuestas están en el apéndice del deck.
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Paquetes ---------------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("dplyr", "metafor", "fabricatr")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# =========================================================================
# A. LA DEMO: una literatura con cajón
# =========================================================================

# --- 2. Generá los datos -------------------------------------------------
# Primero la LITERATURA COMPLETA: 40 estudios con efecto verdadero 0.15.
# Después el cajón: una línea de filter decide quién se publica. La semilla
# fija hace que los publicados salgan idénticos al CSV del repo.
set.seed(27)

apellidos <- c("García", "Souza", "Lima", "Rojas", "Fernández",
               "Muñoz", "Silva", "Torres", "Vargas", "Herrera",
               "Castro", "Mendoza", "Ríos", "Acosta", "Núñez",
               "Ferreira", "Ortiz", "Cabrera", "Duarte", "Sosa")
paises <- c("Brasil", "México", "Chile", "Colombia", "Perú",
            "Argentina", "Uruguay")

literatura <- fabricate(
  N = 40,
  autor   = sample(apellidos, N, replace = TRUE),
  anio    = sample(2005:2022, N, replace = TRUE),
  estudio = paste0(autor, " (", anio, ")"),
  pais    = sample(paises, N, replace = TRUE),
  diseno  = sample(c("RDD", "DiD", "OLS"), N, replace = TRUE),
  # muestras de 60 a 600 personas
  n       = round(exp(runif(N, log(60), log(600)))),
  # cuanto más grande el estudio, más chico su error
  ee      = round(2.2 / sqrt(n), 3),
  # el efecto verdadero es 0.15 para todos; lo demás es ruido
  efecto  = round(rnorm(N, 0.15, sqrt(0.08^2 + ee^2)), 3)
)

# el cajón: los grandes entran siempre;
# los chicos, sólo si salieron significativos
datos <- literatura |>
  filter(ee <= 0.12 | efecto / ee > 1.64)
datos <- datos[, c("estudio", "pais", "diseno", "n", "ee", "efecto")]

# se hicieron 40, se publicaron 21: los otros 19 quedaron en el cajón
nrow(literatura)
nrow(datos)

# ¿Preferís bajar el CSV ya armado, sin generarlo? Descomentá:
# datos <- read.csv("datos/meta_estudios.csv")
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/meta_estudios.csv")

# --- 3. El funnel de la literatura COMPLETA (el cajón abierto) ------------
# Simétrico, como debe ser: los estudios chicos se dispersan para los dos lados.
funnel(rma(yi = efecto, sei = ee, data = literatura),
       xlab = "Efecto estimado", ylab = "Error estándar")

# --- 4. El modelo: el efecto combinado -----------------------------------
# Efectos aleatorios por defecto: el efecto puede variar entre estudios.
m <- rma(yi = efecto,  # el efecto de cada estudio
         sei = ee,     # su error estándar
         data = datos)
m

# Cómo leer la salida:
#   k        = cuántos estudios entraron
#   tau^2    = cuánto varía el efecto REAL entre estudios
#   I^2      = ese desacuerdo, en % del total (0% = todos miden lo mismo)
#   Q        = el test formal de heterogeneidad
#   estimate = el efecto combinado (el rombo del forest plot)

# --- 5. El forest plot ----------------------------------------------------
forest(m, slab = datos$estudio, xlab = "Efecto estimado")

# --- 6. El funnel plot y el test de Egger ---------------------------------
# Ahora el de los 21 PUBLICADOS. La receta para leerlo, en tres pasos:
#   1. ubicá la línea del efecto combinado (el centro del embudo)
#   2. tapá la mitad de arriba: los estudios grandes casi no se mueven,
#      así que no dicen nada sobre el cajón
#   3. en la banda de abajo (los chicos, ee > 0.12), contá los puntos
#      de cada lado de la línea. Repartidos = sano; un lado vacío = cajón
funnel(m, xlab = "Efecto estimado", ylab = "Error estándar")
regtest(m)

# En la literatura completa la cuenta daba 13 y 15. Acá:
banda <- filter(datos, ee > 0.12)
c(izquierda = sum(banda$efecto <  coef(m)),
  derecha   = sum(banda$efecto >= coef(m)))

# --- 7. El chequeo: sólo los grandes --------------------------------------
# Los grandes (ee <= 0.12) se publican con cualquier resultado.
grandes <- filter(datos, ee <= 0.12)
round(c(publicados    = unname(coef(m)),
        solo_grandes  = unname(coef(rma(yi = efecto, sei = ee, data = grandes))),
        cajon_abierto = unname(coef(rma(yi = efecto, sei = ee, data = literatura)))), 3)

# 0.203 / 0.145 / 0.152: el promedio publicado estaba inflado por los estudios
# chicos que sobrevivieron al filtro de la significancia. La tercera columna
# no existe en la vida real: nadie ve el cajón.

# =========================================================================
# B. LOS EJERCICIOS: transferencias condicionadas y asistencia escolar
# =========================================================================
# 24 estudios, siete países, efecto en desvíos estándar.
url <- paste0("https://raw.githubusercontent.com/danilofreire/",
              "taller-evidencia-ucu/main/diapositivas/datos/",
              "transferencias.csv")
transferencias <- read.csv(url)

# Si estás sin internet, el CSV también está en el repo:
# transferencias <- read.csv("datos/transferencias.csv")

# --- Ejercicio 1: el efecto combinado -------------------------------------
# ¿Cuánto da el efecto combinado y qué dice su intervalo?
# ¿Cuánto da el I², y qué significa ese número acá?
# Mirando el forest plot: ¿las filas se parecen entre sí?
m_tr <- rma(yi = efecto, sei = ee, data = transferencias)
m_tr

forest(m_tr, slab = transferencias$estudio,
       xlab = "Efecto (desvíos estándar)")

# --- Ejercicio 2: ¿hay cajón en esta literatura? --------------------------
# ¿El embudo se parece al de la literatura completa o al de los 21 publicados?
# ¿Qué p-valor da Egger, y qué concluís?
# ¿Alcanza esto para afirmar que no hay sesgo de publicación?
funnel(m_tr, xlab = "Efecto estimado", ylab = "Error estándar")
regtest(m_tr)

# --- Ejercicio 3: un efecto, ¿o dos? --------------------------------------
# ¿Cuánto da cada subgrupo? ¿Qué pasó con el I² dentro de cada uno?
# Un ministro leyó "el efecto es 0,24". ¿Qué le decís?
rma(yi = efecto, sei = ee, data = filter(transferencias, contexto == "rural"))
rma(yi = efecto, sei = ee, data = filter(transferencias, contexto == "urbano"))
