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
"_PACKAGE"
