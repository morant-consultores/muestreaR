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

#' Leer la lista nominal del INE
#'
#' Lee el archivo de lista nominal por rango de edad y sexo (xlsx del INE),
#' normaliza los nombres de columna (quita saltos de línea) y la filtra a la
#' entidad indicada.
#'
#' @param ruta Ruta al archivo `.xlsx` de lista nominal.
#' @param entidad Clave de entidad a conservar (por defecto 15, Estado de México).
#'
#' @return `tibble` de lista nominal de la entidad.
#' @export
leer_lista_nominal_ine <- function(ruta, entidad = 15) {
  ln <- readxl::read_excel(ruta)
  ln <- purrr::set_names(ln, gsub("\r\n", " ", names(ln)))
  ln %>% filter(`CLAVE ENTIDAD` == entidad, SECCION != 0)
}

#' Corregir la lista nominal por edad y sexo
#'
#' Reconcilia la lista nominal por rango de edad y sexo (por sección) con la base
#' del INE por localidad, reescalando los conteos por sexo a los totales de la
#' base. Es el paso de preprocesamiento más delicado; reproduce exactamente la
#' lógica usada en producción.
#'
#' @param ln `tibble` de lista nominal (de [leer_lista_nominal_ine()]), con las
#'   columnas `SECCION`, `LISTA NOMINAL`, `LISTA HOMBRES`, `LISTA MUJERES` y los
#'   conteos por rango de edad y sexo.
#' @param base_ine `tibble` de la base del INE por localidad, con `SECCION` y las
#'   columnas `LISTA_HOMBRES` y `LISTA_MUJERES` (con guion bajo); son los totales
#'   por sexo a los que se reescala la lista nominal.
#'
#' @return `tibble` de lista nominal corregida, con `SECCION`, `LISTA NOMINAL` y
#'   los conteos corregidos por rango de edad y sexo.
#'
#' @section Supuesto: Se asume que cada sección tiene población **mayor que cero
#'   en ambos sexos** en la lista nominal original. Si un sexo suma cero en una
#'   sección, su población de la base no puede repartirse (división por cero) y se
#'   conserva el original — comportamiento heredado del flujo de producción. Con
#'   datos reales del INE este caso es raro.
#' @export
corregir_lista_nominal <- function(ln, base_ine) {
  auxi <- ln %>%
    select(SECCION, contains("LISTA")) %>%
    rename(LISTA = `LISTA NOMINAL`, LISTA_HOMBRES = `LISTA HOMBRES`, LISTA_MUJERES = `LISTA MUJERES`) %>%
    select(-contains(c("NO BINARIO", "NOBINARIO"))) %>%
    pivot_longer(-c(SECCION, LISTA, LISTA_HOMBRES, LISTA_MUJERES)) %>%
    mutate(sexo = if_else(grepl("HOMBRES", name), "HOMBRES", "MUJERES")) %>%
    left_join(
      base_ine %>% group_by(SECCION) %>%
        summarise(across(contains("LISTA"), ~sum(.x, na.rm = TRUE), .names = "{.col}_nuevo"))
    ) %>%
    group_by(SECCION, sexo) %>%
    mutate(pct = value / sum(value)) %>%
    ungroup() %>%
    mutate(value_nuevo = if_else(sexo == "HOMBRES", round(pct * LISTA_HOMBRES_nuevo), round(pct * LISTA_MUJERES_nuevo)),
           value_nuevo = if_else(is.na(value_nuevo), value, value_nuevo)) %>%
    select(SECCION, name, value_nuevo, sexo) %>%
    group_by(SECCION, sexo) %>%
    mutate(LISTA = sum(value_nuevo)) %>%
    ungroup()

  ln_sexo <- auxi %>%
    select(-name, -value_nuevo) %>%
    filter(sexo != "NO BINARIO") %>%
    distinct(SECCION, sexo, LISTA) %>%
    pivot_wider(names_from = "sexo", values_from = "LISTA") %>%
    purrr::set_names(c("SECCION", "LISTA_HOMBRES", "LISTA_MUJERES")) %>%
    mutate(`LISTA NOMINAL` = LISTA_HOMBRES + LISTA_MUJERES)

  ln_edad_sexo <- auxi %>%
    select(-sexo, -LISTA) %>%
    filter(!grepl("NO_BINARIO|NOBINARIO", name)) %>%
    pivot_wider(names_from = "name", values_from = "value_nuevo")

  ln_sexo %>% select(-contains("LISTA_")) %>% left_join(ln_edad_sexo)
}

#' Construir la población muestral (marco electoral INE)
#'
#' Envoltura declarativa de `PoblacionINE$new()` que arma la población a partir de
#' la lista nominal (corregida), la base electoral y la cartografía, dejando claro
#' qué insumo va en cada lugar.
#'
#' @param ln Lista nominal corregida (de [corregir_lista_nominal()]).
#' @param electoral Base electoral por sección (con columna `seccion`).
#' @param cartografia Lista de cartografías (de [leer_cartografia_ine()]); usa
#'   `mza`, `loc` y `mun` (que es lo que `crear_mm_ine` consume para construir el
#'   marco). Las capas `sec`/`dl`/`df` no se usan aquí; sirven para los mapas de
#'   campo vía [CartografiaINE].
#' @param nombre Nombre de la población (p. ej. `"Estado de México"`).
#'
#' @return Objeto [PoblacionINE].
#' @export
construir_poblacion_ine <- function(ln, electoral, cartografia, nombre = "Población") {
  PoblacionINE$new(
    nombre    = nombre,
    ln        = ln,
    electoral = electoral,
    shp_mza   = cartografia$mza,
    shp_loc   = cartografia$loc,
    shp_mun   = cartografia$mun
  )
}
