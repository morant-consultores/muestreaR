#' Title
#'
#' @param var
#' @param tama
#' @param bandera
#'
#' @return
#' @export
#'
#' @examples
formato <- function(var, tamaño, bandera = 0){

  readr::parse_character(
    formatC({{var}}, width = tamaño, flag = bandera)
  )
}

#' Title
#'
#' @param a
#' @param b
#' @param cd
#'
#' @return
#' @export
#'
#' @examples
crear_mm <- function(mza,
                     loc,
                     loc_shp,
                     lpr_shp){
  # print(mza)
  # poblacion <- read_csv(mza,na = "*")
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

  # loc_shp <- readOGR(dsn=loc_shp,encoding = "CP1252") %>% spTransform(CRS("+init=epsg:4326")) %>% st_as_sf()
  murb <- murb %>% left_join(loc_shp %>% as_tibble() %>%
                               transmute(ENTIDAD = formato(CVE_ENT, tamaño = 2),
                                         MUN =formato(CVE_MUN, tamaño = 3),
                                         LOC = formato(CVE_LOC, tamaño = 4),
                                         AMBITO))
  murb <- murb %>% mutate(tipo_localidad = "Localidad amanzanada",
                          ARLU = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}-LOC-{AMBITO}"), glue::glue("{ENTIDAD}{MUN}{AGEB}-AGEB-{AMBITO}")),
                          AULR = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}-AGEB-{AMBITO}"), glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}800-LOC-{AMBITO}")),
                          # ARLU = if_else(AMBITO == "Urbana", "LU", "AR"),
                          # AULR = if_else(AMBITO == "Urbana", "AU", "LR"),
                          tipo = "Localidad amanzanada")



  # localidad <- read_csv(loc,na = "*")
  localidad <- loc
  # ageb_shp <- st_read(ageb_shp)%>%
  #   st_transform(4326)

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
      # LONGITUD=map_dbl(LONGITUD,~as.numeric(char2dms(.x,"°","'"))),
      # LATITUD =map_dbl(LATITUD,~as.numeric(char2dms(.x,"°","'"))),
      # ALTITUD = as.numeric(ALTITUD),
      across(all_of(parseN2), ~readr::parse_double(.x, na = c("","NA","*","N/A")))
    )

  # loc <- loc_no_murb %>% st_as_sf(coords = c("LONGITUD","LATITUD"), crs = 4326) %>%
  #   sf::st_join(ageb_shp %>% mutate(valid = sf::st_is_valid(geometry)) %>% filter(valid)) %>% as_tibble %>%
  #   select(MUN,LOC,AGEB = CVE_AGEB)

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
                                        # ARLU = "AR",
                                        # AULR = "LR",
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


  # yo <- yo %>% summarise(sum(POBTOT)) %>% pull(1)
  # urb <- localidad %>% slice(1) %>% pull(POBTOT)
  # list(
  #   identical(yo,urb),
  #   yo-urb,
  #   .x
  # )

  return(final)
}


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

