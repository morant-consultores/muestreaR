#' Formatear una clave a un ancho fijo
#'
#' Convierte un valor a caracter rellenándolo hasta un ancho fijo, útil para
#' normalizar claves geoestadísticas (entidad, municipio, localidad, etc.).
#'
#' @param var Valor o vector a formatear.
#' @param tamaño Entero con el ancho deseado de la cadena resultante.
#' @param bandera Bandera de `formatC()` para el relleno (por defecto `0`,
#'   que rellena con ceros a la izquierda).
#'
#' @return Vector de caracteres con el ancho fijo indicado.
#' @export
formato <- function(var, tamaño, bandera = 0){

  readr::parse_character(
    formatC({{var}}, width = tamaño, flag = bandera)
  )
}

#' Construir el marco muestral (marco censal INEGI)
#'
#' Arma el marco muestral a nivel manzana a partir de las bases de población del
#' censo (manzana y localidad) y la cartografía, normalizando las claves
#' geoestadísticas y uniendo localidades amanzanadas y puntuales rurales.
#'
#' @param mza Base de población por manzana (censo INEGI).
#' @param loc Base de población por localidad (censo INEGI).
#' @param loc_shp Cartografía de localidades (objeto `sf`).
#' @param lpr_shp Cartografía de localidades puntuales rurales (objeto `sf`).
#'
#' @return `tibble` del marco muestral con una fila por manzana y las claves
#'   geoestadísticas normalizadas.
#' @export
crear_mm <- function(mza,
                     loc,
                     loc_shp,
                     lpr_shp){
  poblacion <- mza
  parseN <- poblacion %>% select(POBTOT:last_col()) %>% select(where(is.character)) %>% names
  poblacion <- poblacion %>% mutate(across(all_of(parseN), ~readr::parse_double(.x,na = c("","NA","*","N/A"))))

  poblacion <- poblacion %>% mutate(MUN = formato(MUN, tamaño = 3),
                                    LOC = formato(LOC, tamaño = 4))

  murb <- poblacion %>% filter(!grepl("Total ",NOM_LOC)) %>% mutate(
    ENTIDAD = formato(ENTIDAD, tamaño = 2),
    MUN = formato(MUN, tamaño = 3),
    LOC = formato(LOC, tamaño = 4)
  )

  murb <- murb %>% left_join(loc_shp %>% as_tibble() %>%
                               transmute(ENTIDAD = formato(CVE_ENT, tamaño = 2),
                                         MUN =formato(CVE_MUN, tamaño = 3),
                                         LOC = formato(CVE_LOC, tamaño = 4),
                                         AMBITO))
  murb <- murb %>% mutate(tipo_localidad = "Localidad amanzanada",
                          ARLU = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}-LOC-{AMBITO}"), glue::glue("{ENTIDAD}{MUN}{AGEB}-AGEB-{AMBITO}")),
                          AULR = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}-AGEB-{AMBITO}"), glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}800-LOC-{AMBITO}")),
                          tipo = "Localidad amanzanada")



  localidad <- loc

  loc_no_murb <- localidad %>% filter(!grepl("Total de|Localidades",NOM_LOC)) %>%
    mutate(
      ENTIDAD = formato(ENTIDAD, tamaño = 2),
      MUN =formato(MUN, tamaño = 3),
      LOC = formato(LOC, tamaño = 4)) %>%
    anti_join(murb %>% distinct(MUN, LOC)) %>%
    mutate(AMBITO = "Rural", tipo_localidad = "Localidad puntual")

  parseN2 <- loc_no_murb %>% select(POBTOT:VPH_SINTIC) %>% select(where(is.character)) %>% names
  loc_no_murb <- loc_no_murb %>%
    select(-(LONGITUD:ALTITUD)) %>%
    mutate(
      across(all_of(parseN2), ~readr::parse_double(.x, na = c("","NA","*","N/A")))
    )


  loc_no_murb <- loc_no_murb %>% left_join(
    lpr_shp %>%
      tibble %>%
      transmute(ENTIDAD = formato(CVE_ENT,2),
                MUN = formato(CVE_MUN, 3),
                LOC = formato(CVE_LOC,4),
                AGEB = formato(CVE_AGEB,4),
                MZA = formato(CVE_MZA,3)
      )
  )

  loc_no_murb <- loc_no_murb %>% mutate(ARLU = glue::glue("{ENTIDAD}{MUN}{AGEB}-AGEB-Rural"),
                                        AULR = glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}800-LOC-Rural"),
                                        tipo = "Localidad puntual rural")


  final <- murb %>%
    bind_rows(
      loc_no_murb
    ) %>% mutate(ENTIDAD = formato(ENTIDAD, 2),
                 MZA = formato(MZA, tamaño = 3),
                 MUN = paste0(ENTIDAD, MUN),
                 LOC = paste0(MUN, LOC),
                 AGEBR = if_else(tipo == "Localidad puntual rural",paste0(MUN, AGEB),paste0(LOC, AGEB)),
                 AGEB = paste0(LOC, AGEB),
                 MZA = paste0(AGEB, MZA)) %>%
    tibble::rownames_to_column("id")



  return(final)
}


#' Construir la lista de cartografías (marco censal INEGI)
#'
#' Normaliza y agrupa los distintos shapefiles (municipio, localidad urbana y
#' rural, AGEB rural y urbana, manzana) en una lista lista para graficar mapas y
#' generar insumos de campo.
#'
#' @param mun Shapefile de municipios (`sf`).
#' @param locU Shapefile de localidades urbanas (`sf`).
#' @param agebR Shapefile de AGEB rurales (`sf`).
#' @param agebU Shapefile de AGEB urbanas (`sf`).
#' @param locR Shapefile de localidades rurales (`sf`).
#' @param mza Shapefile de manzanas (`sf`).
#'
#' @return Lista nombrada de cartografías (`MUN`, `ARLU`, `AULR`, `AGEB`, `MZA`).
#' @export
crear_shp <- function(mun, locU, agebR, agebU, locR, mza){
  mun <- mun %>% transmute(MUN = CVEGEO, NOM_MUN = NOMGEO)

  locU <- locU %>% transmute(ARLU = glue::glue("{CVEGEO}-LOC-{AMBITO}"), NOM_LOC = NOMGEO, AMBITO = AMBITO)

  agebR <- agebR %>% transmute(ARLU = glue::glue("{CVEGEO}-AGEB-Rural"))

  agebU <- agebU %>% transmute(AULR = glue::glue("{CVEGEO}-AGEB-Urbana"))

  locR <-   locR %>% transmute(AULR = glue::glue("{CVEGEO}-LOC-Rural"), Nombre = NOMGEO)

  mza <- mza %>% transmute(MZA = CVEGEO, TIPOMZA)

  return(list(MUN = mun,
              ARLU = bind_rows(locU %>%
                                 filter(AMBITO == "Urbana"), agebR),
              AULR = bind_rows(agebU,locR),
              AGEB = agebU,
              MZA = mza
  )
  )
}


#' Construir el marco muestral (marco electoral INE)
#'
#' Arma el marco muestral a nivel manzana a partir de la lista nominal del INE y
#' la cartografía electoral (manzanas, localidades y municipios). Reparte la
#' lista nominal por sección entre sus manzanas y clasifica el desglose por
#' rango de edad y sexo (`LN22_*`).
#'
#' @param ln Lista nominal del INE con columnas `SECCION`, los conteos por edad
#'   y sexo (`LISTA_*`) y `LISTA NOMINAL`.
#' @param shp_mza Shapefile de manzanas (`sf`), con columna `STATUS`.
#' @param shp_loc Shapefile de localidades (`sf`).
#' @param shp_mun Shapefile de municipios (`sf`).
#'
#' @return `tibble` del marco muestral con una fila por manzana, `lista_nominal`
#'   y el desglose `LN22_*` por edad y sexo.
#' @export
crear_mm_ine <- function(ln, shp_mza, shp_loc, shp_mun){
  shp_mza <- shp_mza %>% filter(sf::st_is_valid(.), STATUS == 1)
  aux <- sf::st_join(shp_loc, shp_mza %>% select(MANZANA))
  shp_lpr <- aux %>% filter(is.na(MANZANA)) %>% select(-MANZANA)

  if("LISTA" %in% colnames(shp_lpr)){
    shp_lpr <- shp_lpr %>% relocate(LISTA, .before = LOCALIDAD)
  }

  shp_lpr <- shp_lpr %>% as_tibble %>% rename(MANZANA = NOMBRE) %>%
    mutate(across(c(ENTIDAD:LOCALIDAD,-contains("LISTA")), ~as.character(.x))) %>%
    select(ENTIDAD:MANZANA) %>% mutate(TIPO = "rural")

  shp_mun <- shp_mun %>% as_tibble %>% rename(NOMBRE_MUN = NOMBRE) %>%
    mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN)

  if("LISTA" %in% colnames(shp_mza)){
    shp_mza <- shp_mza %>% relocate(LISTA, .before = MANZANA)
  }

  shp_mza <- shp_mza %>% as_tibble %>% select(ENTIDAD:MANZANA) %>%
    mutate(across(c(ENTIDAD:MANZANA,-contains("LISTA")), ~as.character(.x))) %>% mutate(TIPO = "urbana")

  shp_mza <- shp_mza %>% bind_rows(shp_lpr) %>% arrange(as.numeric(SECCION))

  ln <- ln %>% select(SECCION, contains("LISTA_")) %>% tidyr::pivot_longer(-SECCION, names_to = "sector",
                                                                    values_to = "n") %>%
    mutate(sector = gsub(pattern = "_18_",replacement =  "_18_18_",x =  sector),
           sector = gsub(pattern = "_19_",replacement =  "_19_19_",x =  sector),
           sector = gsub(pattern = "_Y_",replacement =  "_",x =  sector),
    ) %>%
    tidyr::separate(sector, into = c("lista","ini","fin","sexo")) %>%
    mutate(fin = as.numeric(fin),
           fin = if_else(is.na(fin),200,fin),
           rango = cut(as.numeric(fin), c(17,24,39,59,Inf),
                       labels = paste0("LN22_",c("18A24","25A39","40A59","60YMAS")))) %>%
    count(SECCION,rango,sexo, wt = n) %>% mutate(sexo = if_else(sexo == "HOMBRES","M","F")) %>%
    tidyr::unite(rango_sexo, rango:sexo) %>%
    tidyr::pivot_wider(id_cols = SECCION, names_from = rango_sexo, values_from = n) %>%
    left_join(ln %>% select(SECCION, `LISTA NOMINAL`)) %>%
    mutate(SECCION = as.character(SECCION)) %>% rename(lista_nominal = `LISTA NOMINAL`)

  ln_mza <- ln %>%
    left_join(
      shp_mza %>% count(SECCION, name = "n_mza")
    ) %>% mutate(across(2:lista_nominal, ~.x/n_mza)) %>% select(-n_mza)


  mza <- shp_mza %>% left_join(shp_mun) %>%
    left_join(ln_mza, by = "SECCION") %>%
    mutate(across(where(is.numeric), ~if_else(is.na(.x), 0, .x))) %>%
    rownames_to_column(var = "id")

  if("LISTA" %in% colnames(mza)){
    mza <- mza %>%
      mutate(lista_nominal = if_else(is.na(LISTA) | LISTA == 0, lista_nominal, LISTA)) %>%
      filter(lista_nominal > 0) |>
      select(-LISTA)

    pct_general <- mza |> tidyr::pivot_longer(starts_with("LN22")) |>
      filter(value != 0) |>
      count(name, wt = value) |>
      mutate(pct = n/sum(n)) |>
      select(-n)

    mza <- mza |>
      tidyr::pivot_longer(starts_with("LN22")) |>
      left_join(pct_general, join_by(name)) |>
      mutate(value = if_else(value == 0, lista_nominal*pct, value)) |>
      select(-pct) |>
      tidyr::pivot_wider(names_from = name, values_from = value)

  }

  return(mza)
}



#' Construir la lista de cartografías (marco electoral INE)
#'
#' Normaliza y agrupa los shapefiles electorales (distrito federal y local,
#' municipio, sección y manzana) en una lista lista para graficar mapas y
#' generar insumos de campo.
#'
#' @param df Shapefile de distritos federales (`sf`).
#' @param dl Shapefile de distritos locales (`sf`).
#' @param mun Shapefile de municipios (`sf`).
#' @param loc Shapefile de localidades (`sf`).
#' @param secc Shapefile de secciones electorales (`sf`).
#' @param mza Shapefile de manzanas (`sf`), con columna `STATUS`.
#'
#' @return Lista nombrada de cartografías (`DISTRITO_F`, `DISTRITO_L`,
#'   `MUNICIPIO`, `SECCION`, `MANZANA`).
#' @export
crear_shp_ine <- function(df, dl, mun, loc, secc, mza){
  df <- df %>% transmute(across(c(ENTIDAD,DISTRITO_F), ~as.character(.x))) %>% st_make_valid()

  dl <- dl %>% transmute(across(c(ENTIDAD,DISTRITO_L), ~as.character(.x))) %>% st_make_valid()

  mun <- mun %>% rename(NOMBRE_MUN = NOMBRE) %>%
    mutate(across(ENTIDAD:MUNICIPIO, ~as.character(.x))) %>% select(ENTIDAD:NOMBRE_MUN) %>% st_make_valid()

  loc <- loc %>% rename(MANZANA = NOMBRE) %>%
    mutate(across(ENTIDAD:LOCALIDAD, ~as.character(.x))) %>%
    select(ENTIDAD:MANZANA) %>% st_make_valid()

  secc <- secc %>% rename(DISTRITO_F = DISTRITO) %>%
    mutate(across(ENTIDAD:SECCION, ~as.character(.x))) %>%
    select(ENTIDAD:SECCION) %>% st_make_valid()

  mza <- mza %>% filter(st_is_valid(.), STATUS == 1) %>% select(ENTIDAD:MANZANA) %>%
    mutate(across(ENTIDAD:MANZANA, ~as.character(.x))) %>% st_make_valid()

  mza <- bind_rows(mza, loc) %>% arrange(as.numeric(SECCION))

  return(list(
    DISTRITO_F = df,
    DISTRITO_L = dl,
    MUNICIPIO = mun,
    SECCION = secc,
    MANZANA = mza
  ))
}
