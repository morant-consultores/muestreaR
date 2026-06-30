# =====================================================================================
# disenar_muestra_demo.R
# -------------------------------------------------------------------------------------
# Demostración PASO A PASO de cómo se diseña una muestra con muestreaR, equivalente
# a lo que se hace en las carpetas municipales (p.ej. Edomex), pero sobre un MARCO
# MUESTRAL SINTÉTICO para que corra en cualquier máquina, SIN los shapefiles ni la
# lista nominal reales del INE.
#
# Objetivo: poder ejecutar y observar cada etapa del diseño de forma aislada:
#   1. Construir la población (marco muestral)
#   2. Definir los niveles de muestreo (estratos y clusters)
#   3. Plan de muestra por nivel
#   4. Factores de corrección poblacional (fpc)
#   5. Extracción de la muestra
#   6. Cuotas de edad/sexo
#   7. (opcional) Inspección de resultados
#
# CÓMO CORRERLO:
#   devtools::load_all(".")            # cargar el paquete desde la raíz del repo
#   source("inst/ejemplos/disenar_muestra_demo.R")
#
# NOTA SOBRE REPRODUCIBILIDAD:
#   El sorteo de la muestra es aleatorio. El diseño se crea con `semilla = 123`, de
#   modo que cada etapa estocástica (extraer_muestra, calcular_cuotas) fija sola su
#   sub-semilla y el resultado es reproducible: dos corridas dan la misma muestra.
#   Si omites `semilla`, el comportamiento es el histórico (no reproducible).
# =====================================================================================

devtools::load_all()
library(dplyr)
library(tidyr)
library(purrr)

# Si corres este archivo de forma suelta (no vía devtools::test), descomenta:
# devtools::load_all(".")

set.seed(2025)  # semilla global para que el marco sintético sea siempre idéntico

# =====================================================================================
# PASO 0 — Construir un MARCO MUESTRAL sintético
# -------------------------------------------------------------------------------------
# En PRODUCCIÓN este marco lo construye muestreaR a partir de shapefiles + lista
# nominal del INE:
#
#     poblacion <- PoblacionINE$new(
#       nombre   = "Estado de México",
#       ln       = ln_real,            # lista nominal (xlsx INE)
#       electoral = electoral_real,    # resultados/padrón electoral por sección
#       shp_mza  = mza_shp,            # shapefile de manzanas
#       shp_loc  = loc_shp,            # shapefile de localidades
#       shp_mun  = mun_shp)            # shapefile de municipios
#
# Aquí lo fabricamos a mano. Cada FILA es una manzana (la unidad mínima). El marco
# replica las columnas que el muestreo realmente consume:
#   - id            : identificador único de manzana (unidad de muestreo de nivel 0)
#   - region        : estrato de primer nivel
#   - MUNICIPIO     : cluster de segundo nivel
#   - NOMBRE_MUN    : nombre del municipio (lo usan las cuotas)
#   - SECCION       : cluster de tercer nivel (sección electoral)
#   - lista_nominal : variable poblacional (tamaño) para el muestreo PPT
#   - LN22_*_F/M    : desglose de lista nominal por rango de edad y sexo (para cuotas)
# -------------------------------------------------------------------------------------

# Geografía sintética con anidamiento limpio  region > SECCION > manzana,
# igual que la topología de 2 niveles de las muestras reales de Edomex 2025.
# 2 regiones x 6 secciones x 4 manzanas = 48 manzanas.
geo <- tidyr::expand_grid(
  region    = c("Region 1", "Region 2"),
  seccion_n = 1:6,
  manzana_n = 1:4
) |>
  mutate(
    reg_id     = as.integer(factor(region)),
    # 2 municipios por región (las cuotas se reportan por municipio)
    MUNICIPIO  = sprintf("MUN_%d_%d", reg_id, ceiling(seccion_n / 3)),
    NOMBRE_MUN = paste("Municipio", MUNICIPIO),
    # SECCION única dentro de cada región (anidamiento estricto)
    SECCION    = as.character(reg_id * 100 + seccion_n + 1000),
    id         = as.character(row_number())   # id único por manzana
  )

# lista_nominal por manzana (tamaño poblacional) y su desglose edad x sexo.
# Repartimos lista_nominal en 8 celdas (4 rangos x 2 sexos) de forma determinista.
marco_muestral <- geo |>
  mutate(lista_nominal = 80 + (row_number() * 7) %% 120) |>   # 80..199, determinista
  mutate(
    LN22_18A24_F = round(lista_nominal * 0.13),
    LN22_18A24_M = round(lista_nominal * 0.12),
    LN22_25A39_F = round(lista_nominal * 0.16),
    LN22_25A39_M = round(lista_nominal * 0.15),
    LN22_40A59_F = round(lista_nominal * 0.15),
    LN22_40A59_M = round(lista_nominal * 0.14),
    LN22_60YMAS_F = round(lista_nominal * 0.08),
    LN22_60YMAS_M = round(lista_nominal * 0.07)
  ) |>
  select(id, region, MUNICIPIO, NOMBRE_MUN, SECCION, lista_nominal,
         starts_with("LN22_"))

message("PASO 0 — marco muestral sintético: ", nrow(marco_muestral), " manzanas, ",
        dplyr::n_distinct(marco_muestral$SECCION), " secciones, ",
        dplyr::n_distinct(marco_muestral$MUNICIPIO), " municipios.")

# Construimos un objeto `poblacion` mínimo compatible con DiseñoINE SIN pasar por
# crear_mm_ine (que requeriría shapefiles). DiseñoINE solo necesita poder leer y
# escribir poblacion$marco_muestral, y conocer poblacion$nombre.
poblacion <- new.env()
poblacion$nombre <- "Demo sintético"
poblacion$marco_muestral <- marco_muestral
poblacion$informacion_electoral <- NULL   # solo lo usa revisar_muestra() (opcional)

# =====================================================================================
# PASO 1 — Crear el DISEÑO e ir definiendo los NIVELES de muestreo
# -------------------------------------------------------------------------------------
# DiseñoINE$new define el nivel 0 (la unidad mínima = manzana, vía id_unidad_muestreo)
# y calcula su plan. Parámetros:
#   n                    : tamaño de muestra objetivo (nº de entrevistas)
#   n_0                  : entrevistas por unidad mínima (por manzana)
#   variable_poblacional : columna de tamaño para el muestreo PPT
#   unidad_muestreo      : etiqueta legible de la unidad mínima
#   id_unidad_muestreo   : columna que identifica la unidad mínima (nivel 0)
#   llave_muestreo       : etiqueta corta del nivel
# -------------------------------------------------------------------------------------

diseno <- DiseñoINE$new(
  poblacion            = poblacion,
  n                    = 240,          # 240 entrevistas objetivo
  n_0                  = 5,            # 5 entrevistas por manzana
  variable_poblacional = "lista_nominal",
  unidad_muestreo      = "Manzanas",
  id_unidad_muestreo   = "id",
  llave_muestreo       = "Man",
  semilla              = 123           # <-- diseño reproducible (Fase 1)
)

# Añadimos los niveles jerárquicos, igual que en producción (2 niveles):
#   nivel 1 = region   (ESTRATO)
#   nivel 2 = SECCION  (CLUSTER, último nivel)
# Las manzanas (id) son el nivel 0, definido al crear el diseño.
diseno$agregar_nivel("region",  tipo = "strata",  descripcion = "Regiones",             llave = "region")
diseno$agregar_nivel("SECCION", tipo = "cluster", descripcion = "Secciones electorales", llave = "SECCION")

message("PASO 1 — niveles definidos:")
print(diseno$niveles)

# =====================================================================================
# PASO 2 — PLAN DE MUESTRA por nivel
# -------------------------------------------------------------------------------------
# Cuántas unidades se reparten en cada nivel y bajo qué criterio:
#   "peso"  : las secciones se reparten proporcional al tamaño (lista_nominal)
#   nivel 2 : el último nivel (manzanas por sección) se calcula automáticamente
# -------------------------------------------------------------------------------------
diseno$plan_muestra(nivel = 1, criterio = "peso", unidades_nivel = 8)   # 8 secciones
diseno$plan_muestra(nivel = 2)                                          # manzanas (último)

message("PASO 2 — plan de muestra calculado. Unidades por nivel:")
print(diseno$niveles |> dplyr::select(nivel, tipo, descripcion, unidades, plan_muestra))

# =====================================================================================
# PASO 3 — FACTORES DE CORRECCIÓN POBLACIONAL (fpc)
# -------------------------------------------------------------------------------------
# IMPORTANTE: el orden requerido es 2, 0 (el último nivel cluster, luego el 0; los
# estratos no llevan fpc). Este orden no es obvio y es uno de los puntos que la
# Fase 4 (rediseño de API) busca volver explícito.
# -------------------------------------------------------------------------------------
diseno$fpc(nivel = 2)
diseno$fpc(nivel = 0)

message("PASO 3 — fpc calculados. Columnas fpc en el marco: ",
        paste(grep("^fpc", names(diseno$poblacion$marco_muestral), value = TRUE),
              collapse = ", "))

# =====================================================================================
# PASO 4 — EXTRAER LA MUESTRA (paso estocástico → fijamos semilla)
# -------------------------------------------------------------------------------------
# Se sortea nivel por nivel: secciones dentro de región, manzanas dentro de sección.
# Como el diseño se creó con semilla = 123, cada etapa fija sola su sub-semilla
# (123 + nivel) y el resultado es reproducible: vuelve a correr el script y
# obtendrás exactamente la misma muestra.
# -------------------------------------------------------------------------------------
diseno$extraer_muestra(nivel = 1)
diseno$extraer_muestra(nivel = 2)

muestra_final <- diseno$muestra |> purrr::pluck(length(diseno$muestra))
message("PASO 4 — muestra extraída. Manzanas seleccionadas: ",
        nrow(muestra_final |> tidyr::unnest(data)))

# Inspección: qué secciones y municipios quedaron en la muestra
diseno$muestra |>
  purrr::pluck(length(diseno$muestra)) |>
  tidyr::unnest(data) |>
  distinct(region, NOMBRE_MUN, MUNICIPIO, SECCION) |>
  arrange(region, MUNICIPIO, SECCION) |>
  print(n = Inf)

# =====================================================================================
# PASO 5 — CUOTAS de edad y sexo (paso estocástico → fijamos semilla)
# -------------------------------------------------------------------------------------
# Reparte las entrevistas de cada sección en celdas edad x sexo según el desglose
# LN22_*. El ajuste final ±1 para cuadrar exactamente es aleatorio, pero la semilla
# del diseño (123 + 1000) lo vuelve reproducible.
# -------------------------------------------------------------------------------------
diseno$calcular_cuotas(ajustar = TRUE)

message("PASO 5 — cuotas calculadas. Total de entrevistas en cuotas: ",
        sum(diseno$cuotas$n))
print(head(diseno$cuotas, 12))

# =====================================================================================
# PASO 6 (opcional) — Pruebas / inspección de la muestra
# -------------------------------------------------------------------------------------
# revisar_muestra() compara los totales estimados contra el marco usando el paquete
# survey. Requiere poblacion$informacion_electoral, que aquí no construimos, por lo
# que se deja comentado. En producción:
#     diseno$revisar_muestra(prop_vars = c("lista_nominal"), var_extra = NULL)
# -------------------------------------------------------------------------------------

# exportar() escribe mapas (Google Maps API), cuotas.csv y diseño.rda. Requiere
# cartografía real y API key, por lo que NO se ejecuta en el demo:
#     diseno$exportar(shp, carpeta = "Insumos", zoom = 16)

message("\n>>> Demo completado. Revisa `diseno$niveles`, `diseno$muestra` y `diseno$cuotas`.")

# =====================================================================================
# LIMITACIÓN CONOCIDA
# -------------------------------------------------------------------------------------
# Este script NO se pudo ejecutar en el entorno donde se escribió (sin R instalado).
# Es muy posible que requiera ajustes menores al correrlo por primera vez (nombres de
# columnas, tamaños de muestra vs. unidades disponibles). Reporta cualquier error y se
# corrige. La matemática de muestreo subyacente NO se modifica.
# =====================================================================================
