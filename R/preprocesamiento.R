# =====================================================================================
# Funciones de preprocesamiento: insumos del INE -> población muestral.
# Encapsulan el preámbulo que antes se copiaba entre scripts, con un objetivo claro
# por función.
# =====================================================================================

#' Cargar la cartografía del INE
#'
#' Lee los shapefiles del INE necesarios para construir la muestra y devolverlos
#' como una lista de objetos `sf` en CRS 4326. Carga solo lo necesario: siempre
#' manzana, localidad, municipio y sección; los distritos solo si se estratifica
#' por ellos. No carga la mancha urbana (no se usa).
#'
#' @param carpeta Ruta a la carpeta con los shapefiles del INE (p. ej.
#'   `".../SHP/2023/15 MEXICO"`).
#' @param distrito Nivel de distrito a cargar, cuando se estratifica por distrito:
#'   `"ninguno"` (por defecto), `"local"` o `"federal"`. Es uno u otro.
#' @param encoding Codificación de los shapefiles (por defecto `"CP1252"`, la del
#'   INE).
#'
#' @return Lista con elementos `mza`, `loc`, `mun`, `sec` y, según `distrito`,
#'   `dl` o `df`.
#' @export
leer_cartografia_ine <- function(carpeta,
                                 distrito = c("ninguno", "local", "federal"),
                                 encoding = "CP1252") {
  distrito <- match.arg(distrito)
  leer <- function(archivo) {
    sf::st_read(file.path(carpeta, archivo), quiet = TRUE,
                options = paste0("ENCODING=", encoding)) |>
      sf::st_transform(4326)
  }
  cart <- list(
    mza = leer("MANZANA.shp"),
    loc = leer("LOCALIDAD.shp"),
    mun = leer("MUNICIPIO.shp"),
    sec = leer("SECCION.shp")
  )
  if (distrito == "local")   cart$dl <- leer("DISTRITO_LOCAL.shp")
  if (distrito == "federal") cart$df <- leer("DISTRITO_FEDERAL.shp")
  cart
}
