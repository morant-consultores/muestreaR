

#' Title
#'
#' @param bd
#' @param grupo
#' @param tipo
#' @param i
#'
#' @return
#' @export
#' @examples
etiquetar <- function(bd, grupo, tipo, i){
  bd %>% group_by(across(all_of(grupo)), .add = T) %>% mutate(!!glue::glue("{tipo}_{i}"):= cur_group_id())
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
    bd %>% left_join(bd %>% summarise(n())%>% summarise(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=n()))
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
  aux <- bd %>% mutate(!!rlang::sym(glue::glue("pob_{parse_number(nivel)}")) := sum({{peso}}))
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
      bd %>% mutate(!!rlang::sym(glue::glue("prob_{id}")):=!!rlang::sym(glue::glue("pob_{id}"))/!!rlang::sym(glue::glue("pob_{id-1}")))
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
  para_n <- tibble(nombres = names(aux)) %>% anti_join(tibble(nombres = names(bd)))
  aux <- aux %>% group_by(across(all_of(c(grupo,para_n %>% pluck(1))))) %>% nest() %>% ungroup
  return(aux)
}


