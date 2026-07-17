# Práctica del Bloque 4: meta-análisis y sesgo de publicación
# Taller "Políticas públicas basadas en evidencia" (UCU)
#
# Estos son los chunks centrales de 05-acumulacion.qmd, en el mismo orden,
# más el ejercicio de los estudios grandes. Corré cada bloque, mirá el
# resultado y después intentá el ejercicio (la solución está en el apéndice
# del deck).
#
# Ejecutalo desde la carpeta diapositivas/ (así encuentra los datos en datos/).

# --- 1. Datos y paquetes -------------------------------------------------
# Instalar si es necesario (solo la primera vez)
paquetes <- c("metafor")
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

set.seed(2026)

# 20 estudios simulados sobre tamaño de la legislatura y gasto público
datos <- read.csv("datos/meta_estudios.csv")
# Leélo directo desde la web, sin descargar nada:
# datos <- read.csv("https://raw.githubusercontent.com/danilofreire/taller-evidencia-ucu/main/diapositivas/datos/meta_estudios.csv")

# --- 2. El modelo: el efecto combinado -----------------------------------
# Efectos aleatorios por defecto: el efecto puede variar entre estudios.
m <- rma(yi = efecto,   # el efecto de cada estudio
         sei = ee,      # su error estándar
         data = datos)
m

# --- 3. El forest plot ----------------------------------------------------
# Veinte filas, un rombo. ¿Qué estudios tiran el rombo para arriba?
forest(m, slab = datos$estudio, xlab = "Efecto estimado")

# --- 4. El funnel plot y el test de Egger ---------------------------------
# Sin sesgo, el embudo es simétrico. Acá falta la esquina de abajo a la
# izquierda: los estudios chicos con efectos chicos o negativos.
funnel(m, xlab = "Efecto estimado", ylab = "Error estándar")
regtest(m)

# --- 5. Ejercicio: sólo los estudios grandes ------------------------------
# Los estudios grandes (ee <= 0.12) se publican con cualquier resultado.
# ¿Hacia dónde se mueve el efecto combinado si te quedás sólo con ellos?
grandes <- subset(datos, ee <= 0.12)
rma(yi = efecto, sei = ee, data = grandes)

# El efecto baja de ~0,21 a ~0,16, pegado al 0,15 verdadero de la
# simulación: el promedio completo estaba inflado por los estudios chicos
# que sobrevivieron al filtro de la significancia
