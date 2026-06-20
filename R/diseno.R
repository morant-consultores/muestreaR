

#' Etiquetar un nivel de muestreo en el marco
#'
#' Agrupa el marco muestral por una variable y crea una columna identificadora
#' del conglomerado o estrato para ese nivel, con nombre `{tipo}_{i}`.
#'
#' @param bd `tibble` del marco muestral.
#' @param grupo Variable por la que se agrupa el nivel (sin comillas).
#' @param tipo Tipo de nivel: `"cluster"` o `"strata"`.
#' @param i Entero con el número de nivel.
#'
#' @return El marco muestral con una columna nueva `{tipo}_{i}` que identifica al grupo.
#' @export
#' @import dplyr ggplot2
agregar_nivel <- function(bd, grupo, tipo, i){
  bd %>% group_by({{grupo}}, .add = T) %>%
    mutate(!!glue::glue("{tipo}_{i}"):= cur_group_id()) %>% ungroup
}

#' Calcular el factor de corrección poblacional (fpc) de un nivel
#'
#' Calcula las probabilidades de inclusión de primer orden para el nivel
#' indicado y las agrega al marco muestral como columna `fpc_{nivel}`. Estas
#' probabilidades alimentan el diseño `survey` (muestreo proporcional al tamaño).
#'
#' @param diseño Objeto de la clase [Diseño] (o [DiseñoINE]).
#' @param nivel Entero con el nivel para el que se calcula el fpc. El nivel 0
#'   corresponde a la unidad mínima (manzana).
#'
#' @return El marco muestral del diseño con la columna `fpc_{nivel}` añadida.
#' @export
calcular_fpc <- function(diseño, nivel = 1){
  nombres <- names(diseño$poblacion$marco_muestral)
  nivel_anterior <- diseño$niveles %>% filter(nivel == (!!nivel - 1))
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")

  aux <- if(nivel == 0){
    nivel_principal <- diseño$niveles %>% filter(nivel == max(nivel)) %>% pull(nivel)
    diseño$poblacion$marco_muestral %>% agrupar_nivel(nivel_principal) %>%
      mutate(manzanas = n()) %>%
      left_join(diseño$n_i %>% .[[grep(nombres,pattern = glue::glue("(strata|cluster)_{diseño$ultimo_nivel}"),
                                       value = T )]]) %>%
      mutate(
        fpc_0= sampling::inclusionprobabilities(n = unique(!!sym(glue::glue("m_{diseño$ultimo_nivel}"))), a = manzanas)
      ) %>% distinct(fpc_0) %>% ungroup
  } else{

    diseño$poblacion$marco_muestral %>% agrupar_nivel(readr::parse_number(nivel_principal)) %>%
      summarise(total = sum(!!sym(diseño$variable_poblacional),na.rm = T)) %>%
      left_join(diseño$n_i %>% .[[glue::glue("{nivel_anterior$tipo}_{nivel_anterior$nivel}")]]) %>%
      mutate(
        !!rlang::sym(glue::glue("fpc_{nivel}")) := sampling::inclusionprobabilities(n = unique(!!sym(glue::glue("m_{nivel-1}"))), a = total)
      ) %>% ungroup %>% select(-total,-all_of(c(glue::glue("{c('m','n')}_{nivel-1}"))))
  }




  res <- diseño$poblacion$marco_muestral %>% left_join(aux)
  return(res)
}


#' Asignar regiones (estratos) al marco muestral
#'
#' Clasifica las unidades del marco muestral en regiones a partir de una lista
#' que asocia cada región con un conjunto de valores de la variable `id`. Avisa
#' si quedan unidades sin clasificar o si la lista incluye valores inexistentes.
#'
#' @param bd `tibble` del marco muestral.
#' @param id Nombre (caracter) de la variable que identifica a cada unidad
#'   (p. ej. `"SECCION"`).
#' @param regiones Lista nombrada: cada elemento es una región y contiene el
#'   vector de valores de `id` que la componen.
#'
#' @return El marco muestral con una columna `region` al inicio.
#' @export
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

asignar_m <- function(diseño, criterio, unidades_nivel, manual){
  # Se elige la unidad de muestreo
  un_muestreo <- diseño$niveles %>% filter(!plan_muestra) %>% slice_min(n=1, order_by = nivel)
  # Se cuentan las unidades secundarias de muestreo
  # Sirve para saber si hay suficientes unidades secundarias
  aux <- diseño$poblacion$marco_muestral %>%
    agrupar_nivel(un_muestreo$nivel) %>% {
      if(un_muestreo$nivel==diseño$ultimo_nivel){
        summarise(.,n=n_distinct(cluster_0))
      }
      else
        summarise(.,across(matches(glue::glue("(strata|cluster)_{un_muestreo$nivel+1}")),
                           ~n_distinct(.x),
                           .names = "n"))
    }
  if(un_muestreo$nivel==diseño$ultimo_nivel){
    warning("Por ser el último nivel el criterio de repartición está determinado")
    res <- diseño$poblacion$marco_muestral %>%
      agrupar_nivel(un_muestreo$nivel) %>%
      summarise() %>%
      left_join(diseño$n_i %>% last()) %>%
      # Aquí se fuerza al número de manzanas determinada
      mutate("m_{un_muestreo$nivel}":=round((!!sym(glue::glue("n_{un_muestreo$nivel-1}"))/
                                               !!sym(glue::glue("m_{un_muestreo$nivel-1}")) )/
                                              diseño$n_0)) %>%
      select(-all_of(c(glue::glue("n_{un_muestreo$nivel-1}"),
                       glue::glue("m_{un_muestreo$nivel-1}"))))
  }
  else{
    if(criterio=="uniforme"){
      res <- diseño$poblacion$marco_muestral %>%
        agrupar_nivel(un_muestreo$nivel) %>%
        summarise() %>%
        mutate("m_{un_muestreo$nivel}":= round(unidades_nivel/un_muestreo$unidades)) %>%
        select(starts_with("strata_"),
               starts_with("cluster_"),
               glue::glue("m_{un_muestreo$nivel}"))
    }
    if(criterio == "manual"){
      res <- diseño$poblacion$marco_muestral %>%
        agrupar_nivel(un_muestreo$nivel) %>%
        summarise() %>% mutate("m_{un_muestreo$nivel}":= manual)
    }
    if(criterio=="unidades"){
      res <- aux %>%
        mutate("m_{un_muestreo$nivel}":= repartir_cociente(unidades_nivel,unidades_nivel*n/sum(n)))
    }
    if(criterio=="peso"){
      # Se utiliza la variable poblacional de tamaño para repartir las unidades de nivel
      # según el peso. Hasta aquí no es relativo al grupo anterior, es una repartición global.
      res <- diseño$poblacion$marco_muestral %>%
        agrupar_nivel(un_muestreo$nivel) %>%
        summarise(n=sum(!!sym(diseño$variable_poblacional))) %>%
        # agrupar_nivel(un_muestreo$nivel) %>%
        mutate("m_{un_muestreo$nivel}":= repartir_cociente(unidades_nivel,unidades_nivel*n/sum(n))) %>%
        # mutate("m_{un_muestreo$nivel}":= round(unidades_nivel/n_groups(.)*n/sum(n))) %>%
        select(-n)
    }

  }
  # Se elije el mínimo entre las unidades previstas en el plan de muestreo y las posibles
  res <- res %>% left_join(aux) %>%
    mutate("m_{un_muestreo$nivel}":= if_else(!!sym(glue::glue("m_{un_muestreo$nivel}")) < n,
                                             !!sym(glue::glue("m_{un_muestreo$nivel}")), as.numeric(n))) %>%
    select(-n)
  # Se asigna el número total de unidades de muestreo
  # Cuando es strata se asigna así mismo y al siguiente
  if(un_muestreo$tipo=="strata"){
    diseño$niveles <-  diseño$niveles %>%
      # El número de estratos. Sirve para el nivel 1, hay que checar para otros niveles
      mutate(unidades=if_else(nivel==un_muestreo$nivel,
                              as.numeric(nrow(res)), unidades))
  }
  # Siempre se asigna al siguiente

  diseño$niveles <- diseño$niveles %>%
    mutate(unidades=if_else(nivel==un_muestreo$nivel+1,
                            if_else(un_muestreo$nivel==1,res %>%
                                      ungroup() %>%
                                      summarise(n=sum(!!sym(glue::glue("m_{un_muestreo$nivel}")))) %>%
                                      pull(n),
                                    res %>%
                                      ungroup() %>%
                                      summarise(n=mean(!!sym(glue::glue("m_{un_muestreo$nivel}")))) %>%
                                      pull(n) * un_muestreo$unidades),
                            unidades))
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



#' Sortear la muestra de un nivel
#'
#' Extrae aleatoriamente las unidades del nivel indicado dentro de cada unidad
#' del nivel anterior, con probabilidad proporcional al tamaño (variable
#' poblacional) salvo en el último nivel, donde el sorteo es equiprobable.
#' Para reproducibilidad, fija la semilla del diseño antes de llamar a esta
#' función (ver el campo `semilla` de [DiseñoINE]).
#'
#' @param diseño Objeto de la clase [Diseño] (o [DiseñoINE]) con el plan de
#'   muestra ya calculado.
#' @param nivel Entero con el nivel a sortear.
#'
#' @return Lista con la muestra acumulada hasta el nivel indicado; el último
#'   elemento contiene las unidades recién sorteadas (anidadas en `data`).
#' @export
muestrear <- function(diseño, nivel){

  nombres <- names(diseño$poblacion$marco_muestral)
  nivel_principal <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel}"),
                          value = T )
  if(length(nivel_principal)!=1) stop("El nivel seleccionado no se encuestra en el marco muestral")
  if(nivel_principal=="cluster_0") stop("Hay que arreglar esto")
  if(nivel == diseño$ultimo_nivel) nivel_secundario <- "cluster_0"
  else{
    nivel_secundario <- grep(nombres,pattern = glue::glue("(strata|cluster)_{nivel+1}"),
                             value = T ) %>% readr::parse_number()
    if(length(nivel_secundario)==0) {
      warning("El nivel posterior no se encuentra en el marco muestral, se utiliza en cambio el último nivel")
      nivel_secundario <- "cluster_0"
    }
  }

  bd <- if(is.null(diseño$muestra)) diseño$poblacion$marco_muestral else diseño$muestra %>% purrr::pluck(length(diseño$muestra)) %>% tidyr::unnest(data)
  muestra  <- bd %>%
    agrupar_nivel(nivel_secundario) %>%
    mutate(total = sum(!!sym(diseño$variable_poblacional),na.rm = T)) %>%
    group_by(total, .add = T) %>%
    tidyr::nest() %>%
    ungroup() %>%
    # semi_join(
    #   diseño$n_i %>% .[[nivel_principal]] %>% filter(!!rlang::sym(glue::glue("m_{nivel}")) >0)
    # ) %>%
    split(.[[nivel_principal]]) %>% purrr::map_df(~{
      n_nivel <- diseño$n_i %>% .[[nivel_principal]] %>%
        filter(!!sym(nivel_principal) == unique(.x[[nivel_principal]])) %>% pull(glue::glue("m_{nivel}"))

      if(nivel_secundario == "cluster_0") sorteado <- .x %>% slice_sample(n = n_nivel) else{
        if(is.null(diseño$sobre_muestra)){
          no_necesita_sm <- T
        } else{
          no_necesita_sm <- diseño$sobre_muestra %>% semi_join(.x) %>% nrow() == 0
        }

        if(no_necesita_sm){
          sorteado <-  .x %>% slice_sample(weight_by = total,n = n_nivel)
        } else{
          n_nivel_original <- diseño$sobre_muestra %>% semi_join(.x) %>% pull(m_1_vieja)
          sorteo_normal <- .x %>% slice_sample(weight_by = total,n = n_nivel_original)

          ya <- sorteo_normal %>% semi_join(
            diseño$poblacion$marco_muestral %>% semi_join(diseño$sobre_muestra)
          )

          n_nivel_sm <- diseño$sobre_muestra %>% semi_join(.x) %>% pull(m_sm) -nrow(ya)

          sorteo_sm <- .x %>% semi_join(
            diseño$poblacion$marco_muestral %>% semi_join(diseño$sobre_muestra)
          ) %>%
            anti_join(sorteo_normal)

          if(nrow(sorteo_sm) > n_nivel_sm) {
           sorteo_sm <- sorteo_sm %>%
             slice_sample(weight_by = total,n = n_nivel_sm)
          }

          sorteado <- bind_rows(sorteo_normal, sorteo_sm)

        }

      }


      return(sorteado)
    })

  nombre <- diseño$niveles %>% filter(nivel == !!nivel+1) %>% pull(variable)
  if(length(nombre) == 0) nombre <- "MZA"
  res <- diseño$muestra %>% append(list(muestra) %>% purrr::set_names(nombre))
  return(res)

}

#' Calcular cuotas de edad y sexo (marco censal INEGI)
#'
#' Reparte las entrevistas asignadas a cada conglomerado de la muestra en celdas
#' de rango de edad y sexo, proporcionalmente a la población del censo, y ajusta
#' al entero más cercano cuadrando el total por conglomerado.
#'
#' @param diseño Objeto de la clase [Diseño] con la muestra ya extraída.
#'
#' @return `tibble` con las cuotas por conglomerado, rango de edad y sexo
#'   (columna `n` con el número de entrevistas).
#' @export
cuotas <- function(diseño){
  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  muestra <- diseño$muestra %>% purrr::pluck(length(diseño$muestra))
  bd <- diseño$poblacion$marco_muestral %>%
    semi_join(muestra %>% distinct(!!rlang::sym(u_cluster)))

  ent <- muestra %>% left_join(diseño$n_i$cluster_0) %>% count(!!rlang::sym(u_cluster), wt = n_0, name = "entrevistas")
  cuotas <- bd %>% transmute(Municipio = NOM_MUN,
                             Localidad = NOM_LOC,
                             !!rlang::sym(u_cluster),
                             P_18A24_F,
                             P_18A24_M,
                             P_25A59_F = P_18YMAS_F - P_18A24_F - P_60YMAS_F,
                             P_25A59_M = P_18YMAS_M - P_18A24_M - P_60YMAS_M,
                             P_60YMAS_F,P_60YMAS_M) %>%
    group_by(Municipio, Localidad,!!rlang::sym(u_cluster)) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)),.groups =  "drop") %>%
    tidyr::pivot_longer(c(-Municipio,-Localidad,-!!sym(u_cluster)), names_to = "edad", values_to = "cantidad") %>%
    tidyr::separate(edad, c("basura","rango","sexo")) %>% select(-basura) %>%
    group_by(!!rlang::sym(u_cluster)) %>% mutate(pct = cantidad/sum(cantidad)) %>% ungroup %>%
    left_join(
      ent
    ) %>% mutate(n =  round(pct*entrevistas))

  revision <- cuotas %>% count(!!rlang::sym(u_cluster), wt = n)

  c <- revision %>% left_join(ent) %>% mutate(diff = entrevistas - n) %>%
    select(a = 1,b = 4) %>%
    purrr::pmap_df(function(a, b){
      aux <- cuotas %>% filter(!!rlang::sym(u_cluster) == !! a)
      ya <- abs(b)
      while(ya != 0){
        alea <- min(nrow(aux), ya)
        aleatorio <- sample(x = seq_len(nrow(aux)), size = alea)
        aux[aleatorio, "n"] <- aux[aleatorio, "n"] + sign(b)
        ya <- ya - alea
      }

      return (aux)
    })

  cool <- c %>% count(!!rlang::sym(u_cluster), wt = n) %>% left_join(ent) %>% filter(n != entrevistas)
  if(nrow(cool) == 0 & nrow(c) == nrow(cuotas)) print("Exacto")
  return(c %>% select(-cantidad,-pct,-entrevistas))
}

#' Calcular cuotas de edad y sexo (marco electoral INE)
#'
#' Versión para el marco del INE: reparte las entrevistas de cada conglomerado
#' en celdas de rango de edad y sexo según la lista nominal (`LN22_*`).
#'
#' @param diseño Objeto de la clase [DiseñoINE] con la muestra ya extraída.
#' @param ajustar `logical`. Si es `TRUE`, ajusta ±1 las cuotas redondeadas para
#'   cuadrar exactamente las entrevistas por conglomerado.
#'
#' @return `tibble` con las cuotas por conglomerado, rango de edad y sexo.
#' @export
cuotas_ine <- function(diseño, ajustar){
  u_nivel <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel)
  u_cluster <- u_nivel %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  muestra <- diseño$muestra %>% purrr::pluck(length(diseño$muestra))
  bd <- diseño$poblacion$marco_muestral %>%
    semi_join(muestra %>% distinct(!!rlang::sym(u_cluster)))

  ent <- muestra %>% left_join(diseño$n_i$cluster_0) %>% count(!!rlang::sym(u_cluster), wt = n_0, name = "entrevistas")

  cuotas <- bd %>% select(Municipio = NOMBRE_MUN,
                          Seccion = SECCION,
                          !!rlang::sym(u_cluster),
                          contains("LN22")) %>%
    group_by(Municipio, Seccion,!!rlang::sym(u_cluster)) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)),.groups =  "drop") %>%
    tidyr::pivot_longer(c(-Municipio, -Seccion,-!!sym(u_cluster)), names_to = "edad", values_to = "cantidad") %>%
    tidyr::separate(edad, c("basura","rango","sexo")) %>% select(-basura) %>%
    group_by(!!rlang::sym(u_cluster)) %>% mutate(pct = cantidad/sum(cantidad)) %>% ungroup %>%
    left_join(
      ent
    ) %>% mutate(n =  round(pct*entrevistas))

  if(ajustar){
    revision <- cuotas %>% count(!!rlang::sym(u_cluster), wt = n)

    ci <- revision %>% left_join(ent) %>% mutate(diff = entrevistas - n) %>%
      select(a = 1,b = 4) %>%
      purrr::pmap_df(function(a, b){
        aux <- cuotas %>% filter(!!rlang::sym(u_cluster) == !! a)
        ya <- abs(b)
        while(ya != 0){
          alea <- min(nrow(aux), ya)
          aleatorio <- sample(x = seq_len(nrow(aux)), size = alea)
          aux[aleatorio, "n"] <- aux[aleatorio, "n"] + sign(b)
          ya <- ya - alea
        }

        return (aux)
      })

    cool <- ci %>% count(!!rlang::sym(u_cluster), wt = n) %>% left_join(ent) %>% filter(n != entrevistas)
    if(nrow(cool) == 0 & nrow(ci) == nrow(cuotas)) print("Exacto")
    ci <- ci %>% select(-cantidad,-pct,-entrevistas,-Seccion)
  } else{
    ci <- cuotas
  }

  return(ci)
}
