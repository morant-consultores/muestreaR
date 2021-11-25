

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
calcular_fpc <- function(bd, n_grupo, metodo_fpc = "probabilidad_inclusion", peso_tamaño){
  nivel <- bd %>% ungroup %>% select(last_col()) %>% names
  if(metodo_fpc == "numero_unidades"){
    bd <- if(!grepl("strata",nivel)){
      bd %>% left_join(
        bd %>% summarise(n())%>% summarise(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=n())
      )
    } else{
      bd
    }
  }
  if(metodo_fpc == "probabilidad_inclusion"){
    bd <- if(!grepl("strata",nivel)){
      nivel_anterior <- bd %>% ungroup %>% select(last_col()-1) %>% names
      if(grepl("fpc",nivel_anterior)) nivel_anterior <- bd %>% ungroup() %>% select(last_col()-2) %>% names

      grupos <- bd %>% group_vars

      aux <- bd %>% split(.[[nivel_anterior]]) %>%
        map2_df(.x=.,.y=n_grupo,
                .f = ~{
                  .x %>%
                    mutate(total = sum({{peso_tamaño}},na.rm = T)) %>%
                    group_by(total,.add = T) %>% nest() %>%
                    ungroup() %>%
                    mutate(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=
                             # Asumes que está muestreado con método de Tillé
                             sampling::inclusionprobabilities(n = .y, a = total)) %>%
                    unnest(data)

                }) %>% group_by(across(all_of(grupos)))
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
empaquetar <- function(bd, grupo, tipo, n, peso_tamaño, metodo_fpc, criterio_n = "peso"){
  aux <- bd
  for(i in seq_along(tipo)){

    aux <- etiquetar(aux, grupo[i], tipo[i],i)

    nivel <- aux %>% ungroup %>% select(last_col()) %>% names
    if(!grepl("strata",nivel)){
      nivel_anterior <- aux %>% ungroup %>% select(last_col()-1) %>% names
      if(grepl("fpc",nivel_anterior)) nivel_anterior <- aux %>% ungroup() %>% select(last_col()-2) %>% names
      base_n <- criterio_N(base = aux %>% ungroup, variable_estrato = !!sym(nivel_anterior),
                          variable_estudio = {{peso_tamaño}},num = n[i], tipo = criterio_n)

    } else{
      base_n <- NULL
    }
    aux <- aux %>% calcular_fpc(n_grupo = base_n$n, metodo_fpc = metodo_fpc, peso_tamaño = {{peso_tamaño}}) #%>%
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
criterio_N <- function(base, variable_estrato, variable_estudio, num, tipo = "unidades") {

  if(tipo == "unidades"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      group_by({{variable_estrato}}) %>% tally(name = "N") %>% mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
  }

  if(tipo == "peso"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      count({{variable_estrato}}, wt = {{variable_estudio}},name = "N") %>% mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
  }

  if(tipo == "uniforme"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      distinct({{variable_estrato}}) %>% mutate(n = round(num/n()))
  }

  return(res)
}



