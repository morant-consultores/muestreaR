#' Sustituir una unidad de la muestra (marco censal INEGI)
#'
#' Reemplaza una unidad seleccionada (p. ej. una manzana inviable en campo) por
#' otra del mismo conglomerado, recalculando la muestra y regenerando su mapa.
#'
#' @param diseño Objeto de la clase [Diseño] con la muestra extraída.
#' @param shp Lista de cartografías del diseño.
#' @param id Identificador de la unidad a sustituir.
#' @param zoom Nivel de zoom para el mapa de la unidad sustituta.
#' @param dir Carpeta donde se guardan los mapas.
#'
#' @return El diseño con la unidad sustituida.
#' @export
sustituir_muestra <- function(diseño, shp, id, zoom, dir){

# sólo se puede sustituir el último nivel
t_nivel <- diseño$niveles %>% filter(nivel == !!diseño$ultimo_nivel) %>% pull(variable)
nivel <- diseño$niveles %>% filter(variable == !!t_nivel) %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
# nivel del conjunto al que pertenece
nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel -1) %>%
  transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
# muestra del nivel
muestra <- diseño$muestra[[t_nivel]]
# conjnuto seleccionado
subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)
#nueva muestra
nuevo <- diseño$poblacion$marco_muestral %>%
  filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
  anti_join(muestra) %>%
  muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
  mutate(total = sum(!!rlang::sym(diseño$variable_poblacional))) %>%
  group_by(total, .add = T) %>%
  tidyr::nest() %>%
  ungroup %>%
  slice_sample(n = 1, weight_by = total)

# nuevo$data %>% pluck(1,"NOM_MUN")
####podría haber un error aquí#######
manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow
nuevas_manzanas <- nuevo %>% tidyr::unnest(data) %>% slice_sample(n = manzanas) %>%
  group_by(across(strata_1:total),cluster_0) %>% tidyr::nest()
#####################################
#recalcular n_i$cluster_0
cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
if(length(cl0_quitar) != nrow(nuevas_manzanas)) stop(glue::glue("Volver a correr. La {t_nivel} muestreada no tiene el mismo número de manzanas que la que desea sustituir."))
diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
  bind_rows(
    diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>%
      mutate(n_0 = enc_0)
  ) %>% arrange(cluster_0)
#sustituir muestra
diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)
#calcular cuota del nuevo

aux_cuotas <- muestreaR:::cuotas(diseño)
cuotas_nuevo <- aux_cuotas %>% anti_join(diseño$cuotas, by = nivel)

cuotas_viejo <- diseño$cuotas %>% filter(!!rlang::sym(nivel) == id)

diseño$cuotas <- diseño$cuotas %>% anti_join(cuotas_viejo, by = nivel) %>%
  bind_rows(
    cuotas_nuevo
  ) %>% arrange(!!rlang::sym(nivel))

# library(ggmap)
# ggmap::ggmap(ggmap::get_map())
muestreaR:::google_maps(diseño, shp = shp$shp, zoom = zoom, dir = dir)

return(diseño)
# diseño %>% readr::write_rds(glue::glue("auditoria/data/diseño_qro{i}.rda"))
}

#' Sustituir una unidad de la muestra (marco electoral INE)
#'
#' Versión para el marco del INE: reemplaza una unidad seleccionada por otra del
#' mismo conglomerado, con opción de reajustar las cuotas y regenerar el mapa.
#'
#' @param diseño Objeto de la clase [DiseñoINE] con la muestra extraída.
#' @param shp Lista de cartografías electorales del diseño.
#' @param id Identificador de la unidad a sustituir.
#' @param zoom Nivel de zoom para el mapa de la unidad sustituta.
#' @param dir Carpeta donde se guardan los mapas.
#' @param ajustar_cuotas `logical`. Si es `TRUE`, recalcula las cuotas tras la
#'   sustitución.
#' @param crear_mapa `logical`. Si es `TRUE`, genera el mapa de la nueva unidad.
#'
#' @return El diseño con la unidad sustituida.
#' @export
sustituir_muestra_ine <- function(diseño, shp, id, zoom, dir, ajustar_cuotas, crear_mapa){

  # sólo se puede sustituir el último nivel
  t_nivel <- diseño$niveles %>% filter(nivel == !!diseño$ultimo_nivel) %>% pull(variable)
  nivel <- diseño$niveles %>% filter(variable == !!t_nivel) %>% transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  # nivel del conjunto al que pertenece
  nivel_anterior <- diseño$niveles %>% filter(nivel == diseño$ultimo_nivel -1) %>%
    transmute(paste(tipo,nivel,sep = "_")) %>% pull(1)
  # muestra del nivel
  muestra <- diseño$muestra[[t_nivel]]
  # conjnuto seleccionado
  subcluster <- muestra %>% filter(!!rlang::sym(nivel) == id) %>% pull(nivel_anterior)
  #nueva muestra
  nuevo <- diseño$poblacion$marco_muestral %>%
    filter(!!rlang::sym(nivel_anterior) == subcluster) %>%
    anti_join(muestra) %>%
    muestreaR:::agrupar_nivel(readr::parse_number(nivel)) %>%
    mutate(total = sum(!!rlang::sym(diseño$variable_poblacional))) %>%
    group_by(total, .add = T) %>%
    tidyr::nest() %>%
    ungroup %>%
    slice_sample(n = 1, weight_by = total)

  # nuevo$data %>% pluck(1,"NOM_MUN")
  ####podría haber un error aquí#######
  manzanas <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% nrow
  nuevas_manzanas <- nuevo %>% tidyr::unnest(data) %>% slice_sample(n = manzanas) %>%
    group_by(across(strata_1:total),cluster_0) %>% tidyr::nest()
  #####################################
  #recalcular n_i$cluster_0
  cl0_quitar <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) == id) %>% pull(cluster_0)
  enc_0 <- diseño$n_i$cluster_0 %>% filter(cluster_0 %in% cl0_quitar) %>% pull(n_0)
  if(length(cl0_quitar) != nrow(nuevas_manzanas)) stop(glue::glue("Volver a correr. La {t_nivel} muestreada no tiene el mismo número de manzanas que la que desea sustituir."))
  diseño$n_i$cluster_0 <- diseño$n_i$cluster_0 %>% filter(!cluster_0 %in% nuevas_manzanas$cluster_0) %>%
    bind_rows(
      diseño$n_i$cluster_0 %>% filter(cluster_0 %in% nuevas_manzanas$cluster_0) %>%
        mutate(n_0 = enc_0)
    ) %>% arrange(cluster_0)
  #sustituir muestra
  diseño$muestra[[t_nivel]] <- diseño$muestra[[t_nivel]] %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevo)
  diseño$muestra$MZA <- diseño$muestra$MZA %>% filter(!!rlang::sym(nivel) != id) %>% bind_rows(nuevas_manzanas)
  #calcular cuota del nuevo

  aux_cuotas <- muestreaR:::cuotas_ine(diseño, ajustar = ajustar_cuotas)
  cuotas_nuevo <- aux_cuotas %>% anti_join(diseño$cuotas, by = nivel)

  cuotas_viejo <- diseño$cuotas %>% filter(!!rlang::sym(nivel) == id)

  diseño$cuotas <- diseño$cuotas %>% anti_join(cuotas_viejo, by = nivel) %>%
    bind_rows(
      cuotas_nuevo
    ) %>% arrange(!!rlang::sym(nivel))

  # library(ggmap)
  # ggmap::ggmap(ggmap::get_map())
  if(crear_mapa){
    muestreaR:::google_maps_ine(diseño, shp = shp$shp, zoom = zoom, dir = dir)
  }
  

  return(diseño)
  # diseño %>% readr::write_rds(glue::glue("auditoria/data/diseño_qro{i}.rda"))
}
