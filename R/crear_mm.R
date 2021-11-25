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
crear_mm <- function(mza, loc, ageb_shp, loc_shp){
  # print(mza)
  # poblacion <- read_csv(mza,na = "*")
  poblacion <- mza
  poblacion <- poblacion %>% mutate(across(POBTOT:VPH_SINTIC, ~as.numeric(.x)))
  str <- poblacion %>% select(MUN,LOC) %>% select_if(is.character) %>% ncol
  if(str != 2){
    poblacion <- poblacion %>% mutate(MUN = formato(MUN, tamaño = 3),
                                      LOC = formato(LOC, tamaño = 4))
  }

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
                          clave_ARLU = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}"), glue::glue("{ENTIDAD}{MUN}{AGEB}")),
                          clave_AULR = if_else(AMBITO == "Urbana", glue::glue("{ENTIDAD}{MUN}{LOC}{AGEB}"), glue::glue("{ENTIDAD}{MUN}{AGEB}{LOC}")),
                          ARLU = if_else(AMBITO == "Urbana", "LU", "AR"),
                          AULR = if_else(AMBITO == "Urbana", "AU", "LR"))



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
    # select(NOM_ENT,MUN,NOM_MUN,LOC,NOM_LOC,POBTOT) %>%
    mutate(AMBITO = "Rural", tipo_localidad = "Localidad puntual")

  loc_no_murb <- loc_no_murb %>%
    mutate(LONGITUD=map_dbl(LONGITUD,~as.numeric(char2dms(.x,"°","'"))),
           LATITUD =map_dbl(LATITUD,~as.numeric(char2dms(.x,"°","'"))),
           across(POBTOT:VPH_SINTIC, ~as.numeric(.x)),
           ALTITUD = as.numeric(ALTITUD))

  loc <- loc_no_murb %>% st_as_sf(coords = c("LONGITUD","LATITUD"), crs = 4326) %>%
    sf::st_join(ageb_shp %>% mutate(valid = sf::st_is_valid(geometry)) %>% filter(valid)) %>% as_tibble %>%
    select(MUN,LOC,AGEB = CVE_AGEB)

  loc_no_murb <- loc_no_murb %>% left_join(loc) %>% mutate(clave_ARLU = glue::glue("{ENTIDAD}{MUN}{AGEB}"),
                                                           clave_AULR = glue::glue("{ENTIDAD}{MUN}{AGEB}{LOC}"),
                                                           ARLU = "AR",
                                                           AULR = "LR")



  final <- murb %>%
    bind_rows(
      loc_no_murb
    ) %>% mutate(ENTIDAD = as.character(ENTIDAD),
                 MZA = formato(MZA, tamaño = 3),
                 MUN=paste0(ENTIDAD, MUN),
                 LOC=paste0(MUN, LOC),
                 AGEB=paste0(LOC, AGEB),
                 MZA=if_else(is.na(MZA), AGEB, paste0(AGEB, MZA))) %>%
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
