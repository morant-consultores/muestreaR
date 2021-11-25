

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
fpc <- function(bd, n, metodo_fpc = "probabilidad_inclusion"){
  nivel <- bd %>% ungroup %>% select(last_col()) %>% names
  if(tipo == "numero_unidades"){
    bd <- if(!grepl("strata",nivel)){
      bd %>% left_join(
        bd %>% summarise(n())%>% summarise(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=n())
      )
    } else{
      bd
    }
  }
  if(tipo == "probabilidad_inclusion"){
    bd <- if(!grepl("strata",nivel)){
      bd %>% left_join(
        bd %>% summarise(n()) %>% summarise(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=n())
        )
    } else{
      bd
    }
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
  aux <- bd %>% mutate(!!rlang::sym(glue::glue("pob_{parse_number(nivel)}")) := sum({{peso}}, na.rm = T))
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
#' @param id
#' @param regiones
#'
#' @return
#' @export
#'
#' @examples
regiones <- function(bd, id, regiones){
  aux <- regiones %>% enframe(name = "region", value = id) %>% unnest(all_of(id))
  faltan <- bd %>% anti_join(aux) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
  if(faltan != ""){
    warning(glue("No se ha clasificado los siguientes {id}: {faltan}. \n Favor de agregarlos a la lista regiones"))
  }

  error <- aux %>% anti_join(bd) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
  if(error != ""){
    warning(glue("Los siguientes {id} no existen en la base de datos: {error}. \n Favor de rectificar la lista de regiones"))
  }

  bd %>% left_join(
    aux
  ) %>% select(region, everything())
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
empaquetar <- function(bd, grupo, tipo, peso_tamaño, metodo_fpc){
  aux <- bd
  for(i in seq_along(tipo)){
    aux <- etiquetar(aux, grupo[i], tipo[i],i) %>%
      fpc(metodo_fpc = metodo_fpc) #%>%
      # poblacion({{peso_tamaño}}) %>%
    # probabilidad(metodo = metodo_prob)
  }
  # para_n <- tibble(nombres = names(aux)) %>% anti_join(tibble(nombres = names(bd)))
  # aux <- aux %>% group_by(across(all_of(c(grupo,para_n %>% pluck(1))))) %>% nest() %>% ungroup
  return(aux %>% ungroup)
}


#' Title
#'
#' @param base
#' @param variable_estrato
#' @param variable_estudio
#' @param estimador
#' @param n_k
#' @param base_n
#'
#' @return
#' @export
#'
#' @examples
calcular_varianza_estratificada <- function(base, variable_estrato, variable_estudio,estimador=NULL, n_k=NULL, base_n=NULL) {
  if(is.null(estimador)) {

    bd <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      group_by({{variable_estrato}}) %>%
      summarise(varianza=var({{variable_estudio}}, na.rm=T)) %>%
      summarise(suma_varianza=sum(varianza, na.rm=T)) %>%
      pull(suma_varianza)

  }

  else {

    if(estimador=="t") {
      bd <- base %>%
        filter(!is.na({{variable_estudio}})) %>%
        group_by({{variable_estrato}}) %>%
        summarise(varianza=var({{variable_estudio}}, na.rm=T),
                  N=n()) %>%
        left_join(base_n) %>%
        mutate(#n=ceiling(n*N/sum(N)),
          varianza=((varianza*(N^2))/n)*(1-n/N)) %>%
        summarise(suma_varianza=sum(varianza, na.rm=T)) %>%
        pull(suma_varianza)

    }

  }
  return(bd)


}



#' Title
#'
#' @param base
#' @param n
#' @param variable_estudio
#'
#' @return
#' @export
#'
#' @examples
calcular_varianza_mas <- function(base, n, variable_estudio) {
  bd <- base %>%
    filter(!is.na({{variable_estudio}})) %>%
    summarise(N=n(),
              varianza=var({{variable_estudio}}, na.rm=T)) %>%
    mutate(varianza_t=((N^2)*(1-n/N)*varianza)/n) %>%
    pull(varianza_t)

  return(bd)

}



#' Title
#'
#' @param base
#' @param variable_estrato
#' @param variable_estudio
#' @param num
#' @param tipo
#' @param variable_peso
#'
#' @return
#' @export
#'
#' @examples
criterio_N <- function(base, variable_estrato, variable_estudio, num, tipo = "unidades", variable_peso) {

  if(tipo == "unidades"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      group_by({{variable_estrato}}) %>% tally(name = "N") %>% mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
  }

  if(tipo == "peso"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      count({{variable_estrato}}, wt = {{variable_peso}},name = "N") %>% mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
  }

  if(tipo == "uniforme"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      distinct({{variable_estrato}}) %>% mutate(n = round(num/n()))
  }

  return(res)
}



