agrupar_nivel <- function(bd, nivel){
  niveles <- grep(x = names(bd), pattern = glue::glue("(strata|cluster)_[1-{nivel}]"), value=T)
  indices <- stringr::str_extract(niveles, pattern = '(?<=_).*') %>% as.numeric() %>% order()
  niveles <- niveles[indices]
  bd <- bd %>% group_by(across(all_of(niveles)))
  return(bd)
}

#' Repartir un total entero por el método del mayor residuo
#'
#' Redondea un vector de asignaciones reales a enteros que suman exactamente `n`,
#' asignando las unidades sobrantes a los mayores residuos fraccionarios (método
#' de Hamilton / mayor residuo).
#'
#' @param n Entero con el total a repartir.
#' @param x Vector numérico de asignaciones reales (suma aproximadamente `n`).
#'
#' @return Vector de enteros, de la misma longitud que `x`, que suma `n`.
#' @export
#' @examples
#' repartir_cociente(10, c(2.4, 3.3, 4.3))
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
