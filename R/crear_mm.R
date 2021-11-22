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
  print(mza)
  poblacion <- read_csv(mza,na = "*")
  poblacion <- poblacion %>% mutate(across(POBTOT:VPH_SINTIC, ~as.numeric(.x)))
  str <- poblacion %>% select(MUN,LOC) %>% select_if(is.character) %>% ncol
  if(str != 2){
    poblacion <- poblacion %>% mutate(MUN = formatC(MUN,width = 3,flag = "0"),
                                      LOC = formatC(LOC,width = 4,flag = "0"))
  }
  murb <- poblacion %>% filter(!grepl("Total ",NOM_LOC)) %>% mutate(
    ENTIDAD = formatC(ENTIDAD, width = 2, flag = 0),
    MUN =formatC(MUN, width = 3, flag = 0),
    LOC = formatC(LOC, width = 4, flag = 0)
  )

  loc_shp <- readOGR(dsn=loc_shp,encoding = "CP1252") %>% spTransform(CRS("+init=epsg:4326")) %>% st_as_sf()
  murb <- murb %>% left_join(loc_shp %>% as_tibble() %>%
                               transmute(ENTIDAD = formatC(CVE_ENT, width = 2, flag = 0),
                                         MUN =formatC(CVE_MUN, width = 3, flag = 0),
                                         LOC = formatC(CVE_LOC, width = 4, flag = 0),
                                         AMBITO))
  # loc_shp <- st_read(loc_shp)%>%
  #   st_transform(4326)


  localidad <- read_csv(loc,na = "*")

  ageb_shp <- st_read(ageb_shp)%>%
    st_transform(4326)

  loc_no_murb <- localidad %>% filter(!grepl("Total de|Localidades",NOM_LOC)) %>%
    mutate(
      ENTIDAD = formatC(ENTIDAD, width = 2, flag = 0),
      MUN =formatC(MUN, width = 3, flag = 0),
      LOC = formatC(LOC, width = 4, flag = 0)) %>%
    anti_join(murb %>% distinct(MUN, LOC)) %>%
    # select(NOM_ENT,MUN,NOM_MUN,LOC,NOM_LOC,POBTOT) %>%
    mutate(AMBITO = "Rural")

  loc_no_murb <- loc_no_murb %>%
    mutate(LONGITUD=map_dbl(LONGITUD,~as.numeric(char2dms(.x,"°","'"))),
           LATITUD =map_dbl(LATITUD,~as.numeric(char2dms(.x,"°","'"))),
           across(POBTOT:VPH_SINTIC, ~as.numeric(.x)),
           ALTITUD = as.numeric(ALTITUD))

  loc <- loc_no_murb %>% st_as_sf(coords = c("LONGITUD","LATITUD"), crs = 4326) %>%
    st_join(ageb_shp %>% mutate(valid = st_is_valid(geometry)) %>% filter(valid)) %>% as_tibble %>%
    select(MUN,LOC,AGEB = CVE_AGEB)

  loc_no_murb <- loc_no_murb %>% left_join(loc)



  yo <- murb %>%
    bind_rows(
      loc_no_murb
    ) %>% mutate(ENTIDAD = as.character(ENTIDAD),
                 MZA = formatC(MZA,width = 3,flag = "0"))


  # yo <- yo %>% summarise(sum(POBTOT)) %>% pull(1)
  # urb <- localidad %>% slice(1) %>% pull(POBTOT)
  # list(
  #   identical(yo,urb),
  #   yo-urb,
  #   .x
  # )

  return(yo)
}
