# =====================================================================================
# helper-fixture.R
# -------------------------------------------------------------------------------------
# Fixtures sintéticos deterministas para los tests de muestreaR. NO dependen de
# shapefiles ni lista nominal reales del INE: fabrican directamente el marco muestral
# (lo que crear_mm_ine produciría) para poder ejercitar toda la matemática de muestreo.
#
# Es la versión reutilizable del marco que arma inst/ejemplos/disenar_muestra_demo.R.
# =====================================================================================

# Marco muestral sintético: cada fila es una manzana (unidad mínima).
# 2 regiones x 2 municipios x n_secciones x n_manzanas. Determinista.
generar_marco_ine <- function(n_secciones = 3, n_manzanas = 4) {
  geo <- tidyr::expand_grid(
    region      = c("Region 1", "Region 2"),
    municipio_n = 1:2,
    seccion_n   = seq_len(n_secciones),
    manzana_n   = seq_len(n_manzanas)
  ) |>
    dplyr::mutate(
      MUNICIPIO  = sprintf("MUN_%s_%d",
                           ifelse(region == "Region 1", "A", "B"), municipio_n),
      NOMBRE_MUN = paste("Municipio", MUNICIPIO),
      SECCION    = as.character(dplyr::row_number() %/% n_manzanas + 1000),
      id         = as.character(dplyr::row_number())
    )

  geo |>
    dplyr::mutate(lista_nominal = 80 + (dplyr::row_number() * 7) %% 120) |>
    dplyr::mutate(
      LN22_18A24_F  = round(lista_nominal * 0.13),
      LN22_18A24_M  = round(lista_nominal * 0.12),
      LN22_25A39_F  = round(lista_nominal * 0.16),
      LN22_25A39_M  = round(lista_nominal * 0.15),
      LN22_40A59_F  = round(lista_nominal * 0.15),
      LN22_40A59_M  = round(lista_nominal * 0.14),
      LN22_60YMAS_F = round(lista_nominal * 0.08),
      LN22_60YMAS_M = round(lista_nominal * 0.07)
    ) |>
    dplyr::select(id, region, MUNICIPIO, NOMBRE_MUN, SECCION, lista_nominal,
                  dplyr::starts_with("LN22_"))
}

# Objeto `poblacion` mínimo compatible con DiseñoINE, sin pasar por crear_mm_ine.
# DiseñoINE solo necesita leer/escribir poblacion$marco_muestral y poblacion$nombre.
generar_poblacion_ine <- function(marco = generar_marco_ine()) {
  poblacion <- new.env()
  poblacion$nombre <- "Fixture sintético"
  poblacion$marco_muestral <- marco
  poblacion$informacion_electoral <- NULL
  poblacion
}

# Diseño INE completo y reproducible (semilla fija) listo para extraer muestra.
# Ejecuta: niveles -> plan_muestra -> fpc. Deja el diseño listo para extraer_muestra().
generar_diseno_ine <- function(n = 240, n_0 = 5, semilla = 123,
                               poblacion = generar_poblacion_ine()) {
  diseno <- DiseñoINE$new(
    poblacion            = poblacion,
    n                    = n,
    n_0                  = n_0,
    variable_poblacional = "lista_nominal",
    unidad_muestreo      = "Manzanas",
    id_unidad_muestreo   = "id",
    llave_muestreo       = "Man",
    semilla              = semilla
  )
  diseno$agregar_nivel("region",    tipo = "strata",  descripcion = "Regiones",             llave = "region")
  diseno$agregar_nivel("MUNICIPIO", tipo = "cluster", descripcion = "Municipios",           llave = "Mun")
  diseno$agregar_nivel("SECCION",   tipo = "cluster", descripcion = "Secciones electorales", llave = "SECCION")

  diseno$plan_muestra(nivel = 1, criterio = "peso",     unidades_nivel = 4)
  diseno$plan_muestra(nivel = 2, criterio = "uniforme", unidades_nivel = 8)
  diseno$plan_muestra(nivel = 3)

  diseno$fpc(nivel = 2)
  diseno$fpc(nivel = 3)
  diseno$fpc(nivel = 0)

  diseno
}
