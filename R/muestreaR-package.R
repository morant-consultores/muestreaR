#' muestreaR: diseño de muestras polietápicas para encuestas
#'
#' Herramientas para construir el marco muestral, definir un diseño polietápico
#' (estratos y clusters con muestreo proporcional al tamaño), extraer la muestra,
#' calcular cuotas de edad/sexo y exportar los insumos de campo.
#'
#' @details
#' Las dependencias que se usan SIN prefijo `::` dentro del paquete se declaran
#' aquí para que el NAMESPACE las importe explícitamente. Sin esto, varias
#' funciones solo corrían si el usuario tenía esos paquetes adjuntados
#' (p. ej. `library(sf)` / `library(tidyverse)`). El resto de dependencias se
#' invocan con prefijo `paquete::funcion()` y no necesitan importarse aquí.
#'
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @import leaflet
#' @import tibble
#' @import purrr
#' @import sf
#' @import tidyr
#' @importFrom rlang sym enquo :=
#' @importFrom glue glue
#' @importFrom stats var confint reorder na.omit
#' @importFrom utils head
#' @importFrom R6 R6Class
#' @importFrom grDevices topo.colors
"_PACKAGE"

# Variables usadas en evaluación no estándar (NSE) de dplyr/tidyr.
# Se declaran para silenciar los NOTE "no visible binding for global variable".
utils::globalVariables(c(
  ".", "2.5 %", "97.5 %", "AGEB", "ALTITUD",
  "AMBITO", "basura", "cantidad", "cluster_0",
  "color", "CVE_AGEB", "CVE_ENT", "CVE_LOC",
  "CVE_MUN", "CVE_MZA", "CVEGEO", "data",
  "descripcion", "DISTRITO", "DISTRITO_F", "DISTRITO_L",
  "edad", "ENTIDAD", "entrevistas", "entrevistas_a_levantar", "estrato", "fin",
  "fpc_0", "gini", "id", "LISTA",
  "LISTA NOMINAL", "lista_nominal", "llave", "LOC",
  "CLAVE ENTIDAD", "LISTA HOMBRES", "LISTA MUJERES",
  "LISTA_HOMBRES", "LISTA_HOMBRES_nuevo", "LISTA_MUJERES", "LISTA_MUJERES_nuevo",
  "value_nuevo",
  "Localidad", "LOCALIDAD", "longitud", "LONGITUD",
  "MANZANA", "manzanas", "manzanas_por_seccion", "MUN", "Municipio",
  "MUNICIPIO", "MZA", "n_0", "n_mza",
  "name", "nivel", "NOM_LOC", "NOM_MUN",
  "NOMBRE", "NOMBRE_MUN", "NOMGEO", "original",
  "P_18A24_F", "P_18A24_M", "P_18YMAS", "P_18YMAS_F", "P_18YMAS_M",
  "porcentaje", "resultado",
  "P_60YMAS_F", "P_60YMAS_M", "pct", "plan_muestra",
  "pobtot", "POBTOT", "porc", "puntual",
  "rango", "rango_sexo", "region", "Seccion",
  "SECCION", "secciones", "sector", "sexo", "STATUS",
  "strata_1", "tipo", "TIPOMZA", "total",
  "Total", "unidades", "value", "variable",
  "VPH_SINTIC"
))

