#' Title
#'
#' @param bd
#' @param grupo
#' @param tipo
#'
#' @return
#' @export
#'
#' @examples
etiquetar <- function(bd, grupo, tipo){
  aux <- bd
  for(i in seq_along(tipo)){
    aux <- aux %>% group_by(across(all_of(grupo[i])), .add = T) %>% mutate(!!glue("{tipo[i]}_{i}"):= cur_group_id())
  }
  return(aux)
}
