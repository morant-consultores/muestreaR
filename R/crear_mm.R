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
crear_mm <- function(a,b,cd){
  print(a)
  poblacion <- read_csv(a,na = "*")
  poblacion <- poblacion %>% mutate(across(POBTOT:VPH_SINTIC, ~as.numeric(.x)))
  str <- poblacion %>% select(MUN,LOC) %>% select_if(is.character) %>% ncol
  if(str != 2){
    poblacion <- poblacion %>% mutate(MUN = formatC(MUN,width = 3,flag = "0"),
                                      LOC = formatC(LOC,width = 4,flag = "0"))
  }
  murb <- poblacion %>% filter(!grepl("Total ",NOM_LOC)) %>%
    # select(NOM_ENT,MUN,NOM_MUN,LOC,NOM_LOC,AGEB,MZA,POBTOT) %>%
    mutate(tipo = "murb")

  localidad <- read_csv(b,na = "*")

  ageb_r <- st_read(cd)%>%
    st_transform(4326)

  loc_no_murb <- localidad %>% filter(!grepl("Total de|Localidades",NOM_LOC)) %>%
    anti_join(murb %>% distinct(MUN, LOC)) %>%
    # select(NOM_ENT,MUN,NOM_MUN,LOC,NOM_LOC,POBTOT) %>%
    mutate(tipo = "loc_no_murb")

  loc_no_murb <- loc_no_murb %>%
    mutate(LONGITUD=map_dbl(LONGITUD,~as.numeric(char2dms(.x,"°","'"))),
           LATITUD =map_dbl(LATITUD,~as.numeric(char2dms(.x,"°","'"))),
           across(POBTOT:VPH_SINTIC, ~as.numeric(.x)),
           ALTITUD = as.numeric(ALTITUD))

  loc <- loc_no_murb %>% st_as_sf(coords = c("LONGITUD","LATITUD"), crs = 4326) %>%
    st_intersection(ageb_r %>% mutate(valid = st_is_valid(geometry)) %>% filter(valid)) %>% as_tibble %>%
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
