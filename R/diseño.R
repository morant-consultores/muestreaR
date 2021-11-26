

#' Title
#'
#' @param bd
#' @param grupo
#' @param tipo
#' @param i
#'
#' @return
#' @export
#' @import dplyr ggplot2
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
calcular_fpc <- function(bd, n_grupo, peso_tamaño){
  nivel <- bd %>% ungroup %>% select(last_col()) %>% names

  bd <- if(!grepl("strata",nivel)){
    nivel_anterior <- bd %>% ungroup %>% select(last_col()-1) %>% names
    if(grepl("fpc",nivel_anterior)) nivel_anterior <- bd %>% ungroup() %>% select(last_col()-2) %>% names

    grupos <- bd %>% group_vars

    # browser()

# bd %>%
#   mutate(total = sum({{peso_tamaño}},na.rm = T)) %>%
#   group_by(total,.add = T) %>% nest() %>%
#   left_join(
#     tibble(region = unique(bd$region), n = n_grupo)
#   ) %>% mutate(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=
#            # Asumes que está muestreado con método de Tillé
#            sampling::inclusionprobabilities(n = unique(n), a = total)) %>%
#   count(fpc_2)

    aux <- bd %>% purrr::split(.[[nivel_anterior]]) %>%
      purrr::map2_df(.x=., .y=n_grupo,
              .f = ~{
                .x %>%
                  mutate(total = sum({{peso_tamaño}},na.rm = T)) %>%
                  group_by(total,.add = T) %>% nest() %>%
                  ungroup() %>%
                  mutate(!!rlang::sym(glue::glue("fpc_{parse_number(nivel)}")):=
                           # Asumes que está muestreado con método de Tillé
                           sampling::inclusionprobabilities(n = .y, a = total)) %>%
                  tidyr::unnest(data)

              }) %>% group_by(across(all_of(grupos)))
  } else{
    bd
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
  aux <- regiones %>% tibble::enframe(name = "region", value = id) %>% tidyr::unnest(all_of(id))
  faltan <- bd %>% anti_join(aux) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
  if(faltan != ""){
    warning(glue::glue("No se ha clasificado los siguientes {id}: {faltan}. \n Favor de agregarlos a la lista regiones"))
  }

  error <- aux %>% anti_join(bd) %>% distinct(!!sym(id)) %>% pull(1) %>% paste(collapse = ", ")
  if(error != ""){
    warning(glue::glue("Los siguientes {id} no existen en la base de datos: {error}. \n Favor de rectificar la lista de regiones"))
  }

  bd %>% left_join(
    aux
  ) %>% select(region, everything())
}

nivel <- function(aux, nivel, grupo, tipo, n, peso_tamaño, criterio_n){
  aux <- etiquetar(aux, grupo, tipo, nivel)

  nivel <- aux %>% ungroup %>% select(last_col()) %>% names
  if(!grepl("strata",nivel)){
    nivel_anterior <- aux %>% ungroup %>% select(last_col()-1) %>% names
    if(grepl("fpc",nivel_anterior)) nivel_anterior <- aux %>% ungroup() %>% select(last_col()-2) %>% names
    base_n <- criterio_N(base = aux %>% ungroup, variable_estrato = !!sym(nivel_anterior),
                         variable_estudio = {{peso_tamaño}},num = n, tipo = criterio_n)

  } else{
    base_n <- NULL
  }

  aux <- aux %>% calcular_fpc(n_grupo = base_n$n, peso_tamaño = {{peso_tamaño}})
  return(aux)
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
empaquetar <- function(bd, grupo, tipo, n, peso_tamaño, criterio_n = "peso"){
  aux <- bd
  for(i in seq_along(tipo)){

    aux <- nivel(aux, i, grupo[i], tipo[i], n[i], peso_tamaño, criterio_n = "peso")

  }

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
        mutate(
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
      count({{variable_estrato}}, wt = {{variable_estudio}},name = "N") %>% mutate(n = round(num*N/sum(N))) %>% select(-N)
  }

  if(tipo == "uniforme"){
    res <- base %>%
      filter(!is.na({{variable_estudio}})) %>%
      distinct({{variable_estrato}}) %>% mutate(n = round(num/n()))
  }

  return(res)
}



