# Separar el título del mapa del nombre de su archivo.

# Etiqueta que se IMPRIME como título de la unidad en el mapa.
#
# El nombre del archivo y el título salían del mismo valor: `ggsave("{i}.png")` y
# `ggtitle("{u_cluster}: {i}")`. Eso obliga a elegir entre un archivo manejable y
# un título que campo pueda leer, y cuando el identificador del archivo no es la
# llave que el encuestador captura, el título MIENTE.
#
# El caso que lo motivó (Huehuetoca, ago-2026): los conglomerados grandes se
# parten en hojas de ruta y cada hoja necesita su propio PNG, así que el archivo
# se llama `1-A.png`. El título salía "cluster_2: 1-A" — pero `1-A` NO es un
# cluster: el encuestador debe capturar `1`. `encuestar` elimina lo que no casa
# con la llave real ("Cluster no existente") imprimiendo solo un mensaje, así que
# una cuadrilla que copia el título del mapa pierde sus entrevistas en silencio.
#
# Con `etiquetas_unidad` el archivo sigue siendo `1-A.png` y el título puede decir
# "Cluster 1 · hoja A de 4": el número que se captura, al frente.
.etiqueta_unidad <- function(i, u_cluster, etiquetas = NULL) {
  if (!is.null(etiquetas)) {
    # `match` y no `[[`: en un vector atómico con nombres, `x[["ausente"]]` lanza
    # "subíndice fuera de los límites" en vez de devolver NULL, así que etiquetar
    # solo algunas unidades reventaba en la primera sin entrada.
    pos <- match(as.character(i), names(etiquetas))
    if (!is.na(pos)) {
      valor <- etiquetas[[pos]]
      # una entrada vacía o NA cae al default: mejor el título de siempre que un
      # mapa sin título
      if (length(valor) == 1 && !is.na(valor) && nzchar(as.character(valor))) {
        return(as.character(valor))
      }
    }
  }
  as.character(glue::glue("{u_cluster}: {i}"))
}

# `etiquetas_unidad` debe poder resolverse por nombre; un vector sin nombres no
# tiene forma de saber a qué unidad corresponde cada texto.
.validar_etiquetas_unidad <- function(etiquetas) {
  if (is.null(etiquetas)) return(invisible(NULL))
  if (!(is.character(etiquetas) || is.list(etiquetas)) ||
      is.null(names(etiquetas)) || any(!nzchar(names(etiquetas)))) {
    stop("`etiquetas_unidad` debe ser un vector o lista CON NOMBRES, donde el ",
         "nombre es el identificador de la unidad (el mismo que nombra el ",
         "archivo) y el valor es el texto a imprimir.", call. = FALSE)
  }
  if (anyDuplicated(names(etiquetas))) {
    stop("`etiquetas_unidad` trae nombres repetidos: ",
         paste(unique(names(etiquetas)[duplicated(names(etiquetas))]),
               collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}
