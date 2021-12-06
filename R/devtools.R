agrupar_nivel <- function(bd, nivel){
  niveles <- grep(x = names(bd), pattern = glue::glue("(strata|cluster)_[1-{nivel}]"), value=T)
  indices <- stringr::str_extract(niveles, pattern = '(?<=_).*') %>% as.numeric() %>% order()
  niveles <- niveles[indices]
  bd <- bd %>% group_by(across(all_of(niveles)))
  return(bd)
}
