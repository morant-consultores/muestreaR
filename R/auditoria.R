#' Title
#'
#' @param dise
#'
#' @return
#' @export
#'
#' @examples
unidades_app <- function(diseño) {
  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  unidades <- u_nivel %>% pull(variable)
  u_nivel_tipo <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)

  aulr <- shp$shp[[unidades]] %>%
    inner_join(diseño$muestra[[diseño$ultimo_nivel]] %>%
                 unnest(data) %>%
                 distinct(!!rlang::sym(unidades), !!rlang::sym(u_nivel_tipo)))
}

entrevistas <- function(diseño, enc){
  cortes <- diseño$cuotas %>% count(parse_number(rango)-1) %>% pull(1) %>% append(Inf)
  texto_cortes <- diseño$cuotas %>% distinct(rango) %>% pull(1)

  hecho <- enc %>%
    mutate(edad = as.character(cut(as.integer(edad),cortes,
                                   texto_cortes)),
           cluster = as.numeric(cluster)) %>%
    count(cluster, edad, sexo, name = "hecho") %>%
    full_join(
      diseño$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
        rename(cuota = n, cluster = cluster_3, edad = rango)
    ) %>% replace_na(list(hecho = 0, faltan = 0)) %>%
    mutate(faltan = cuota - hecho) %>% filter(cluster %in% diseño$cuotas$cluster_3)

  por_hacer <- diseño$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
    rename(cuota = n, cluster = u_nivel_tipo, edad = rango) %>%
    left_join(
      enc %>%
        mutate(edad = as.character(cut(as.integer(edad),cortes,
                                       texto_cortes)),
               cluster = as.numeric(cluster)) %>%
        count(cluster, edad, sexo, name = "hecho")
    ) %>% replace_na(list(hecho = 0)) %>% mutate(por_hacer = cuota-hecho,
                                                 por_hacer2 = if_else(por_hacer < 0, 0, por_hacer)
    )
  return(list(hecho = hecho, por_hacer = por_hacer))
}
