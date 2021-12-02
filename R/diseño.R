

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
agregar_nivel <- function(bd, grupo, tipo, i){
  bd %>% group_by({{grupo}}, .add = T) %>%
    mutate(!!glue::glue("{tipo}_{i}"):= cur_group_id())
}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
calcular_fpc <- function(base, nivel = 1, n_grupo, ultimo_nivel = F){
  nombres <- names(base)
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
  if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
  if(ultimo_nivel) nivel_secundario <- "cluster_0"
  else{
    nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
                             value = T )
    if(length(nivel_secundario)==0) {
      warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
      nivel_secundario <- "cluster_0"
    }
  }
  aux <- base %>% ungroup %>% agrupar_nivel(nivel_principal) %>%
    summarise(total = sum(POBTOT,na.rm = T)) %>%
    left_join(n_grupo) %>%
    mutate(
      !!rlang::sym(glue::glue("fpc_{nivel+1}")) := sampling::inclusionprobabilities(n = unique(n), a = total)
    ) %>% ungroup %>% select(-total,-n)

  base <- base %>% left_join(aux)
  return(base)
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
#'
criterio_N <- function(base, nivel, variable_estudio=NULL, num, criterio = "unidades",
                       ultimo_nivel=F) {
  nombres <- names(base)
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
  if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
  if(ultimo_nivel) nivel_secundario <- "cluster_0"
  else{
    nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
                             value = T )
    if(length(nivel_secundario)==0) {
      warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
      nivel_secundario <- "cluster_0"
    }
  }
  base <- base %>%
    agrupar_nivel(nivel=nivel)
  if(criterio == "unidades"){
    res <- base %>%
      filter(!is.na(!!sym(variable_estudio))) %>%
      summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
      mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
  }

  if(criterio == "peso"){

    res <- base %>%
      filter(!is.na(!!sym(variable_estudio))) %>%
      summarise(N=sum(!!sym(variable_estudio))) %>%
      mutate(n = min(c(round(num*N/sum(N))),N)) %>% select(-N)
  }

  if(criterio == "uniforme"){
    res <- base %>%
      summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
      mutate(n = min(c(num, N)))
  }

  return(res)
}



#' Title
#'
#' @param base
#' @param nivel
#' @param variable_estudio
#' @param bd_n
#'
#' @return
#' @export
#'
#' @examples
muestrear <- function(base, nivel,variable_estudio, bd_n){

  nombres <- names(base)
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
  if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
  if(ultimo_nivel) nivel_secundario <- "cluster_0"
  else{
    nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
                             value = T )
    if(length(nivel_secundario)==0) {
      warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
      nivel_secundario <- "cluster_0"
    }
  }

  muestra_n2  <- base %>%
    agrupar_nivel(nivel_secundario) %>%
    mutate(total = sum({{variable_estudio}},na.rm = T)) %>%
    group_by(total, .add = T) %>%
    nest() %>%
    ungroup() %>%
    split(.[[nivel_principal]]) %>% map2_df(bd_n$n,~{
      .x %>% slice_sample(weight_by = total,n = .y)
    }) %>% tidyr::unnest(data)

  return(muestra_n2)

}


