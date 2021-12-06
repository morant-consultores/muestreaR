

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
#'
criterio_N <- function(base,
                       nivel,
                       variable_estudio=NULL,
                       num,
                       criterio = "unidades",
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
      filter(!is.na(!!variable_estudio)) %>%
      summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
      mutate(n = ceiling(num*N/sum(N))) %>% select(-N)
  }

  if(criterio == "peso"){

    res <- base %>%
      filter(!is.na(!!variable_estudio)) %>%
      summarise(N=sum(!!variable_estudio)) %>%
      mutate(n = min(c(round(num*N/sum(N))),N)) %>% select(-N)
  }

  if(criterio == "uniforme"){
    res <- base %>%
      summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
      mutate(n = min(c(num, N)))
  }

  return(res)
}

criterio_m <- function(base,    nivel,
                       criterio = "unidades",
                       variable_tamaño=NULL,
                       num,
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
      filter(!is.na(!!variable_tamaño)) %>%
      summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
      mutate(n = ceiling(m_i*N/sum(N))) %>%
      select(-N)
  }

  if(criterio == "peso"){

    res <- base %>%
      filter(!is.na(!!variable_estudio)) %>%
      summarise(N=sum(!!variable_estudio)) %>%
      mutate(n = min(c(round(num*N/sum(N))),N)) %>%
      select(-N)
  }

  if(criterio == "uniforme"){
    res <- base %>%
      summarise(N=n_distinct(!!sym(nivel_secundario))) %>%
      mutate(n = min(c(num, N)))
  }

  return(res)
}

criterio_n <- function(plan_muestreo,
                       nivel,
                       variable_tamaño,
                       marco_muestral,
                       n,
                       m_i,
                       n_0) {
  if(nivel==0){
    res <- marco_muestral %>%
      group_by(cluster_0) %>%
      summarise(n_0=n_0) %>%
      mutate(m_0=ceiling(n/n_0))
    res <- list(cluster_0=res)
  }
  if(nivel!=ultimo_nivel){
    if(nivel==1){
      clase <- "strata"
      print("Clase fija, cuidado")
      if(clase=="strata"){
        browser()
        ve <- "POBTOT"
        print("Un monton de cosas fijas! CUIDADO")
        res_2 <- criterio_N(base = marco_muestral,
                            nivel = nivel,
                            variable_estudio = sym(ve),
                            num = m_i, criterio = "unidades",
                            ultimo_nivel = F) %>%
          rename("m_{nivel}":=n)
        # Para n
        res <- marco_muestral %>%
          agrupar_nivel(nivel=nivel) %>%
          summarise(nn:=sum({{variable_tamaño}})) %>%
          mutate("n_{nivel}":=round(n*nn/sum(nn))) %>%
          select(-nn)
        res <- inner_join(res, res_2)

      }

      # Para m

      nombre <- grep(pattern = glue::glue("(cluster|strata)_{nivel}"),
                     x = names(res),value = T)
      # Falta la m=peso, uniforme, unidades

      res <- set_names(list(res), nombre)
    }
    if(nivel>1){

      pp <- plan_muestreo[grep(pattern = glue::glue("(cluster|strata)_{nivel-1}"),
                               names(plan_muestreo))]
      nivel_secundario <- plan_muestreo[grep(pattern = glue::glue("(cluster|strata)_{nivel+1}"),
                                             names(plan_muestreo))]
      if(length(nivel_secundario)==0) nivel_secundario="cluster_0"
      browser()
      marco_agrupado <- agrupar_nivel(marco_muestral, nivel = nivel)
      marco_agrupado <- marco_agrupado %>% nest()%>%
        left_join(pp[[1]])
      # res2 <- map(marco_agrupado$data,
      #             ~criterio_N(base = .x,
      #                         nivel = nivel,
      #                         variable_estudio = sym(ve),
      #                         num = 5,
      #                         criterio = "unidades",
      #                         ultimo_nivel = F) %>%
      #               rename("m_{nivel}":=n))
      res_2 <- criterio_N(base = marco_muestral,
                          nivel = nivel,
                          variable_estudio = sym(ve),
                          num = 5,
                          criterio = "uniforme",
                          ultimo_nivel = F) %>%
        rename("m_{nivel}":=n) %>% select(-N)

      res <- marco_muestral %>%
        agrupar_nivel(nivel=nivel) %>%
        summarise() %>%
        left_join(pp[[1]]) %>%
        mutate("n_{nivel}":=round(!!sym(glue::glue("n_{nivel-1}"))/
                                    !!sym(glue::glue("m_{nivel-1}")))) %>%
        agrupar_nivel(nivel = nivel) %>%
        select(matches(glue::glue("n_{nivel}")))
      res <- inner_join(res, res_2)

      nombre <- grep(pattern = glue::glue("(cluster|strata)_{nivel}"),
                     x = names(res),value = T)
      # Falta la m=peso, uniforme, unidades

      res <- set_names(list(res), nombre)


    }
  }
  else{
    plan_muestreo["cluster_0"]
    res <- marco_muestral %>%
      agrupar_nivel(nivel)
    mutate()
    res <- list(cluster_0=res)

  }


  return(c(plan_muestreo,res))
}

asignar_m <- function(diseño, criterio, unidades_nivel){
  un_muestreo <- diseño$niveles %>% filter(!plan_muestra) %>% slice_min(n=1, order_by = nivel)
  if(criterio=="uniforme"){
    anterior <- diseño$niveles %>% filter(nivel==un_muestreo$nivel-1)
    anterior_ni <- diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]]
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise() %>%
      left_join(anterior_ni) %>%
      mutate("m_{un_muestreo$nivel}":=unidades_nivel/!!sym(glue::glue("m_{anterior$nivel}"))) %>%
      select(starts_with("strata_"),
             starts_with("cluster_"),
             glue::glue("m_{un_muestreo$nivel}"))
  }
  if(criterio=="unidades"){
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>% {
        if(un_muestreo$nivel==diseño$ultimo_nivel){
          summarise(.,n=n_distinct(cluster_0))
        }
        else
          summarise(.,across(matches(glue::glue("(strata|cluster)_{un_muestreo$nivel+1}")),
                             ~n_distinct(.x),
                             .names = "n"))
      } %>%
      mutate("m_{un_muestreo$nivel}":=unidades_nivel*n/sum(n))
  }
  if(criterio=="peso"){
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
      mutate("m_{un_muestreo$nivel}":=unidades_nivel*n/sum(n)) %>%
      select(-n)

  }
  return(res)
}

asignar_n <- function(diseño){
  un_muestreo <- diseño$niveles %>%
    filter(!plan_muestra) %>%
    slice_min(n=1, order_by = nivel)
  if(un_muestreo$tipo=="strata"){
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
      mutate(n=n/sum(n))
    if(un_muestreo$nivel==1) res <- res %>% mutate(.,n_1=diseño$n*n) %>% select(-n)
    else{
      anterior <- diseño$niveles %>% filter(nivel==un_muestreo$nivel-1)
      diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]] %>%
        left_join(res)
    }

  }
  if(un_muestreo$tipo=="cluster"){
    anterior <- diseño$niveles %>% filter(nivel==un_muestreo$nivel-1)
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise()
    res <- res %>% left_join(diseño$n_i[[glue::glue("{anterior$tipo}_{anterior$nivel}")]] %>%
      mutate("n_{un_muestreo$nivel}":=!!sym(glue::glue("n_{anterior$nivel}"))/!!sym(glue::glue("m_{anterior$nivel}"))) %>%
      select(starts_with("cluster"),
             starts_with("strata"),
             glue::glue("n_{un_muestreo$nivel}")))
  }
  return(res)
}



