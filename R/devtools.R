agrupar_nivel <- function(bd, nivel){
  niveles <- grep(x = names(bd), pattern = glue::glue("(strata|cluster)_[1-{nivel}]"), value=T)
  indices <- stringr::str_extract(niveles, pattern = '(?<=_).*') %>% as.numeric() %>% order()
  niveles <- niveles[indices]
  bd <- bd %>% group_by(across(all_of(niveles)))
  return(bd)
}

#' Title
#'
#' @param n
#' @param x
#'
#' @return
#' @export
#' @examples
repartir_cociente <- function(n, x){
  if(sum(x)+length(x)-1<=n) stop("No es válido el vector propuesto")
  else{
    piso <- floor(x)
    dif= n-sum(piso)

    residuo <- x- piso

    nueva <- (order(residuo, decreasing = T)<=dif)+piso
  }
  return(nueva)
}
