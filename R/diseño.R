

#' Title
#'
#' @param bd
#' @param grupo
#' @param tipo
#' @param i
#'
#' @return
#' @export
#'
#' @examples
etiquetar <- function(bd, grupo, tipo, i){
  bd %>% group_by(across(all_of(grupo)), .add = T) %>% mutate(!!glue("{tipo}_{i}"):= cur_group_id())
}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
fpc <- function(bd){
  nivel <- bd %>% ungroup %>% select(last_col()) %>% names
  bd <- if(!grepl("strata",nivel)){
    bd %>% left_join(bd %>% summarise(n())%>% summarise(!!sym(glue("fpc_{parse_number(nivel)}")):=n()))
  } else{
    bd
  }
  return(bd)
}

#' Title
#'
#' @param bd
#' @param peso
#'
#' @return
#' @export
#'
#' @examples
poblacion <- function(bd, peso){
  nivel <- bd %>% ungroup %>% select(last_col()) %>% names
  aux <- bd %>% mutate(!!sym(glue("pob_{parse_number(nivel)}")) := sum({{peso}}))
  return(aux)
}

#' Title
#'
#' @param bd
#' @param metodo
#'
#' @return
#' @export
#'
#' @examples
probabilidad <- function(bd, metodo){
  bd <- if(metodo == "poblacion"){
    nivel <- bd %>% ungroup %>% select(last_col()-1) %>% names
    bd <- if(grepl("fpc",nivel)){
      id <- parse_number(nivel)
      bd %>% mutate(!!sym(glue("prob_{id}")):=!!sym(glue("pob_{id}"))/!!sym(glue("pob_{id-1}")))
    } else{
      bd
    }
  }
  return(bd)
}

#' Title
#'
#' @param bd
#' @param grupo
#' @param tipo
#' @param peso
#' @param metodo
#'
#' @return
#' @export
#'
#' @examples
empaquetar <- function(bd, grupo, tipo, peso_tamaño, metodo_prob){
  aux <- bd
  for(i in seq_along(tipo)){
    aux <- etiquetar(aux, grupo[i], tipo[i],i) %>%
      fpc() %>%
      poblacion({{peso_tamaño}}) %>%
      probabilidad(metodo = metodo_prob)
  }
  return(aux)
}


#
# etiquetar <- function(bd, grupo, i){
#   browser()
#   sep <- str_split(grupo,pattern = "/")
#   grupo <- sep %>% pluck(1,1)
#   tipo <- sep %>% pluck(1,2)
#   aux <- bd %>% group_by(across(all_of(grupo)), .add = T) %>% mutate(!!glue("{tipo}_{i}"):= cur_group_id())
#   return(aux)
# }
#
# accumulate2(.x = c("NOM_MUN/s"),.y = "1",.f = ~etiquetar(bd = ags %>% select(NOM_MUN), grupo = .x, i = .y))
