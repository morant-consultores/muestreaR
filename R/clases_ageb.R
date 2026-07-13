# Clases del flujo censal por AGEB ----------------------------------------
#
# El diseño por AGEBs usa las MISMAS clases del flujo censal que el equipo
# ya opera: [Diseño] (la población vive adentro y el objeto se exporta como
# diseño.rda, insumo de AppAuditoria y encuestar) y [Cartografia] (los
# shapefiles se exportan como shp.rda y su método crear_mapas() imprime los
# mapas de Google por AGEB con las instrucciones del encuestador). Aquí
# solo se agrega la población censal-AGEB, que no necesita base de
# localidades ni shapefiles para armarse.

#' Población y marco muestral por AGEB (censo INEGI)
#'
#' Clase R6 que construye y almacena el marco muestral por manzana desde el
#' dataset "ageb_mza_urbana" del Censo 2020 (ver [crear_mm_ageb()]) y
#' permite clasificar las unidades en regiones. Es la población de un
#' [Diseño] cuyo conglomerado es el **AGEB urbano**; el resto del flujo
#' (niveles, plan, fpc, extracción, cuotas, exportar con mapas) es el de la
#' clase [Diseño] de siempre.
#'
#' @export
PoblacionAGEB <- R6::R6Class(
  "Poblacion",
  public = list(
    nombre = NULL,
    marco_muestral = NULL,
    #' @param nombre Nombre de la población (p. ej. `"Estado de México"`).
    #' @param censo `tibble` del CSV del censo leído como texto (ver
    #'   [crear_mm_ageb()]).
    initialize = function(nombre, censo) {
      self$nombre <- nombre
      self$marco_muestral <- crear_mm_ageb(censo)
    },
    #' @param variable Columna censal a totalizar (default `"P_18YMAS"`, la
    #'   medida de tamaño del diseño por AGEBs).
    #' @param na.rm Ignorar `NA` (enmascaramiento INEGI).
    calcular_poblacion = function(variable = "P_18YMAS", na.rm = TRUE) {
      sum(self$marco_muestral[[variable]], na.rm = na.rm)
    },
    #' @param id Columna que identifica la unidad a clasificar (p. ej.
    #'   `"MUN"`).
    #' @param regiones Lista nombrada región -> valores de `id` (ver
    #'   [regiones()]).
    regiones = function(id, regiones) {
      self$marco_muestral <- regiones(
        self$marco_muestral,
        id = id,
        regiones = regiones
      )
    }
  )
)
