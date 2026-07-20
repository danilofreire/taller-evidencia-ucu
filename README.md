# Políticas públicas basadas en evidencia

Materiales del taller "Políticas públicas basadas en evidencia: lecciones desde la investigación aplicada en América Latina", dictado en la Universidad Católica del Uruguay (Montevideo) el sábado 1 de agosto de 2026.

**Sitio del taller: <https://danilofreire.github.io/taller-evidencia-ucu/>**

Es un taller intensivo de un día, en español, para estudiantes de maestría con conocimientos básicos de R. Está organizado alrededor de dos preguntas. La mañana responde la primera: ¿de dónde sale tu contrafactual? Recorre tres respuestas (lo construís sorteando, lo encontrás en un umbral, lo armás con las trayectorias de otras unidades). La tarde responde la segunda: ¿cuándo un resultado se vuelve evidencia? Ahí entran el sesgo de publicación, el meta-análisis y la crisis de replicación.

Todos los casos vienen de investigación aplicada en América Latina, y una misma pregunta (el efecto del tamaño de las legislaturas) aparece dos veces: como regresión discontinua a la mañana y como meta-análisis a la tarde.

## Contenido

| Parte | Tema | Método | Duración |
| :-- | :-- | :-- | :-- |
| 1 | Apertura: el problema del contrafactual | | 30 min |
| 2 | Experimentos | Diferencia de medias, balance, `DeclareDesign` | 75 min |
| 3 | Regresión discontinua | `rdrobust`, `rddensity` | 75 min |
| 4 | Diferencias en diferencias y control sintético | `tidysynth`, `gsynth` | 45 min |
| 5 | Acumulación de evidencia | Meta-análisis con `metafor` | 60 min |
| | Cierre: el mapa completo | | 15 min |

Cada parte con práctica trae los datos, un script de R listo para correr y una página de referencia con el código explicado función por función.

## Referencia R

Además de las diapositivas, el sitio incluye una [referencia de R](https://danilofreire.github.io/taller-evidencia-ucu/referencia-r.html) con una página por parte: cada función que usamos, con un ejemplo ejecutable, su salida real y una nota sobre cuándo conviene usarla. Como las semillas están fijas, los números de esas páginas coinciden con los de las diapositivas.

También hay una [guía de reproducibilidad](https://danilofreire.github.io/taller-evidencia-ucu/reproducibilidad.html) con el kit mínimo para que tu trabajo lo pueda correr otra persona.

## Estructura del repositorio

```text
├── index.qmd                    inicio
├── programa.qmd                 programa detallado
├── materiales.qmd               diapositivas, datos y código
├── referencias.qmd              lecturas del taller
├── reproducibilidad.qmd         guía de reproducibilidad
├── referencia-r.qmd             índice de las referencias de R
├── referencia-experimentos.qmd  \
├── referencia-rdd.qmd            |  una página por parte,
├── referencia-did-sintetico.qmd  |  con el código explicado
├── referencia-acumulacion.qmd   /
├── diapositivas/
│   ├── 01-apertura.qmd          las seis presentaciones
│   ├── 02-experimentos.qmd         (formato clean-revealjs)
│   ├── 03-rdd.qmd
│   ├── 04-did-sintetico.qmd
│   ├── 05-acumulacion.qmd
│   ├── 06-cierre.qmd
│   ├── codigo/                  scripts de las prácticas
│   ├── datos/                   datos simulados y sus generadores
│   └── figures/                 imágenes de las diapositivas
├── _quarto.yml                  configuración del sitio
├── theme.scss, theme-dark.scss  tema visual
└── docs/                        sitio renderizado (GitHub Pages)
```

## Datos

Todos los datos son **simulados**. Reproducen la estructura y el orden de magnitud de los estudios reales, sin exponer datos originales. Cada base tiene su generador en `diapositivas/datos/`, con la semilla fija, así que el CSV del repositorio se puede reproducir exactamente.

| Archivo | Filas | Para qué |
| :-- | :-- | :-- |
| `dengue_incentivos.csv` | 400 | Experimento de campo sobre incentivos y dengue |
| `municipios.csv` | 2000 | Regresión discontinua sobre el tamaño del concejo |
| `homicidios.csv` | 256 | Panel estado-año para el control sintético |
| `meta_estudios.csv` | 21 | Literatura publicada, con sesgo de publicación adentro |
| `transferencias.csv` | 24 | Literatura de la práctica de meta-análisis |

## Reproducir el sitio

Hace falta [R](https://cran.r-project.org/) y [Quarto](https://quarto.org). Los paquetes de R que aparecen en el taller:

```r
paquetes <- c(
  # base de todas las prácticas
  "tidyverse", "estimatr", "fabricatr", "randomizr",
  # regresión discontinua
  "rdrobust", "rddensity",
  # diferencias en diferencias y control sintético
  "tidysynth", "gsynth",
  # meta-análisis
  "metafor",
  # simular diseños e inferencia por aleatorización
  "DeclareDesign", "DesignLibrary", "ri2"
)

for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
```

Los últimos cuatro se usan en las diapositivas y en las páginas de referencia, no en las prácticas que se corren en el taller.

Para previsualizar con recarga automática:

```bash
quarto preview
```

Para renderizar el sitio completo a `docs/`:

```bash
quarto render
```

## Los casos

Las presentaciones se apoyan en investigación propia y de colegas:

- **Experimentos.** Freire y Mignozzetti (2022), incentivos económicos y control del dengue en Brasil; y Freire, Galdino y Mignozzetti (2020), [un resultado nulo creíble](https://doi.org/10.1177/2053168020914444) sobre rendición de cuentas.
- **Regresión discontinua.** Mignozzetti, Cepaluni y Freire (2025), [tamaño del concejo y bienestar](https://doi.org/10.1111/ajps.12843), *American Journal of Political Science*.
- **Control sintético.** Freire (2018), [la caída de los homicidios en São Paulo](https://doi.org/10.25222/larr.334), *Latin American Research Review*.
- **Meta-análisis.** Freire, Mignozzetti, Roman y Alptekin (2023), [el tamaño de las legislaturas y el gasto público](https://doi.org/10.1017/S0007123422000552), *British Journal of Political Science*.

## Contacto

Danilo Freire, Department of Data and Decision Sciences, Emory University.
<danilofreire@gmail.com> · <https://danilofreire.github.io>

## Licencia

Contenido bajo licencia [MIT](https://opensource.org/licenses/MIT).
