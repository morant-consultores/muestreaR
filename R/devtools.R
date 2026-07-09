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
  piso <- floor(x)
  dif <- n - sum(piso)
  # a lo más se puede sumar +1 por unidad: si el vector propuesto queda a más
  # de eso del total (o lo excede), no es un redondeo válido
  if (is.na(dif) || dif < 0 || dif > length(x)) {
    stop("No es válido el vector propuesto")
  }
  residuo <- x - piso
  # los `dif` mayores residuos reciben la unidad extra. OJO: rank, no order —
  # `order(residuo) <= dif` marca posiciones equivocadas con vectores
  # desordenados (bug corregido: el sobrante se iba a residuos chicos)
  piso + (rank(-residuo, ties.method = "first") <= dif)
}
