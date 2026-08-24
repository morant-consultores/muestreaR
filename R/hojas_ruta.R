# Hojas de ruta: partir el recorrido de un conglomerado en asignaciones operables.

#' Partir la ruta de un conglomerado en hojas contiguas y balanceadas
#'
#' Un conglomerado grande no es una asignación operable. Con dosis proporcional al
#' tamaño, un conglomerado que concentra 9% del padrón se lleva 9% de la muestra:
#' correcto para la estimación, y en campo eso fueron 274 puertas —**siete días de
#' un encuestador**— con las manzanas repartidas en 5 km. Una asignación así
#' también invita a que la cuadrilla se retire por cansancio en vez de por
#' presupuesto, que es lo que el protocolo prohíbe.
#'
#' La **hoja de ruta** parte ese recorrido en tramos, cada uno con su parte de la
#' carga. Es una partición del MATERIAL: no toca la selección de unidades, ni la
#' dosis, ni la numeración de manzanas, así que el estimador queda intacto.
#'
#' @section Por qué el corte es contiguo y no geográfico:
#' Agrupar las manzanas por cercanía (k-means, `hclust` sobre los centroides)
#' parece lo natural y da un material peor:
#'
#' * las hojas **se traslapan** en el orden de ruta —la hoja B con las manzanas 2 a
#'   10 y la C con las 3 a 9— así que el papel deja de leerse en orden y dos
#'   cuadrillas pueden discutir de quién es una manzana;
#' * la carga queda **desbalanceada**, porque agrupar por geometría ignora las
#'   puertas: medido, una hoja con 162 puertas contra un objetivo de 90.
#'
#' Cortar el orden de ruta por carga acumulada arregla las dos cosas de una: cada
#' hoja es un rango (`manzanas 4 a 7`) y los tramos quedan parejos. Y la compacidad
#' geográfica sale gratis cuando el orden de ruta ya es geográfico —por localidad y
#' manzana, las consecutivas son vecinas.
#'
#' @section La frontera:
#' `frontera` fuerza que ninguna hoja cruce ese límite. El caso que lo motivó: sin
#' la regla, una hoja juntaba tres manzanas de una localidad con una de otra a 7 km,
#' y su extensión volvía a 5 km — el problema que las hojas venían a resolver. Con
#' la localidad como frontera, esa manzana aislada queda como su propia hoja: el
#' viaje hay que hacerlo igual, y explícito el supervisor lo puede planear.
#'
#' Es determinista y no usa semilla: es un recorrido, no un agrupamiento aleatorio.
#'
#' @param ruta data.frame con una fila por unidad a recorrer (una manzana).
#' @param carga_max Carga máxima por hoja, en las unidades de `carga` (p. ej.
#'   puertas). Es un objetivo, no un tope duro: una unidad indivisible más grande
#'   que `carga_max` se queda sola en su hoja.
#' @param cluster,orden,carga Nombres de las columnas del conglomerado, del orden
#'   de recorrido y de la carga de cada unidad.
#' @param frontera Nombre de una columna que ninguna hoja puede cruzar (p. ej.
#'   `"LOCALIDAD"`), o `NULL` para no imponer ninguna.
#'
#' @return `ruta` ordenada por conglomerado y orden de recorrido, con tres columnas
#'   nuevas: `hoja` (entero 1..k dentro del conglomerado), `n_hojas` y `id_hoja`
#'   (etiqueta de campo: `"12"` si el conglomerado no se partió, `"12-A"`,
#'   `"12-B"`, … si sí).
#'
#' @examples
#' ruta <- data.frame(
#'   cluster_2 = rep(c(1, 2), c(8, 3)),
#'   orden_ruta = c(1:8, 1:3),
#'   puertas_a_tocar = c(rep(34, 8), 9, 9, 8),
#'   LOCALIDAD = c(rep(1, 7), 21, rep(6, 3))
#' )
#' # el conglomerado 1 se parte por carga (272 puertas) y respeta la localidad;
#' # el 2 (26 puertas) se queda en una hoja
#' partir_en_hojas(ruta, carga_max = 120, frontera = "LOCALIDAD")
#' @export
partir_en_hojas <- function(ruta, carga_max,
                            cluster = "cluster_2",
                            orden = "orden_ruta",
                            carga = "puertas_a_tocar",
                            frontera = NULL) {
  faltan <- setdiff(c(cluster, orden, carga, frontera), names(ruta))
  if (length(faltan)) {
    stop("A `ruta` le faltan columnas: ", paste(faltan, collapse = ", "),
         call. = FALSE)
  }
  if (length(carga_max) != 1 || is.na(carga_max) || carga_max <= 0) {
    stop("`carga_max` debe ser un número positivo.", call. = FALSE)
  }
  if (anyNA(ruta[[carga]]) || any(ruta[[carga]] < 0)) {
    stop("`", carga, "` debe ser no negativa y sin NA.", call. = FALSE)
  }
  if (!nrow(ruta)) {
    return(cbind(ruta, hoja = integer(0), n_hojas = integer(0),
                 id_hoja = character(0)))
  }

  ruta <- ruta[order(ruta[[cluster]], ruta[[orden]]), , drop = FALSE]
  ruta$hoja <- NA_integer_

  for (idx_cl in split(seq_len(nrow(ruta)), ruta[[cluster]])) {
    # la frontera se respeta partiendo primero por ella y numerando las hojas de
    # corrido dentro del conglomerado
    bloques <- if (is.null(frontera)) list(idx_cl) else
      split(idx_cl, ruta[[frontera]][idx_cl])
    h_base <- 0L
    for (idx in bloques) {
      n <- length(idx)
      total <- sum(ruta[[carga]][idx])
      k <- min(max(ceiling(total / carga_max), 1L), n)
      if (k <= 1L) {
        ruta$hoja[idx] <- h_base + 1L
        h_base <- h_base + 1L
        next
      }
      # greedy sobre el orden de recorrido: se abre hoja nueva al llegar a la
      # parte proporcional del total, reservando una unidad para cada hoja que
      # falta (si no, las últimas hojas saldrían vacías)
      objetivo <- total / k
      h <- 1L; acum <- 0; asign <- integer(n)
      for (i in seq_len(n)) {
        asign[i] <- h
        acum <- acum + ruta[[carga]][idx[i]]
        if (h < k && (acum >= objetivo * h || (n - i) == (k - h))) h <- h + 1L
      }
      ruta$hoja[idx] <- h_base + asign
      h_base <- h_base + k
    }
  }

  n_hojas <- tapply(ruta$hoja, ruta[[cluster]], max)
  ruta$n_hojas <- as.integer(n_hojas[as.character(ruta[[cluster]])])
  ruta$id_hoja <- ifelse(ruta$n_hojas > 1,
                         paste0(ruta[[cluster]], "-", .letra_hoja(ruta$hoja)),
                         as.character(ruta[[cluster]]))

  stopifnot(
    "Quedaron unidades sin hoja" = !anyNA(ruta$hoja),
    "Las hojas de un conglomerado no son 1..k" =
      all(vapply(split(ruta$hoja, ruta[[cluster]]),
                 function(h) setequal(h, seq_len(max(h))), logical(1))),
    # la propiedad que hace legible el papel
    "Hay hojas no contiguas en el orden de recorrido" =
      all(vapply(split(ruta[[orden]], paste(ruta[[cluster]], ruta$hoja)),
                 function(o) identical(sort(o), seq(min(o), max(o))), logical(1))))
  if (!is.null(frontera)) {
    stopifnot("Hay hojas que cruzan la frontera" =
                all(vapply(split(ruta[[frontera]],
                                 paste(ruta[[cluster]], ruta$hoja)),
                           function(f) length(unique(f)) == 1L, logical(1))))
  }
  ruta
}

# A, B, ... Z, AA, AB, ... — para conglomerados con más de 26 hojas
.letra_hoja <- function(h) {
  vapply(h, function(i) {
    out <- character(0)
    while (i > 0) {
      r <- (i - 1L) %% 26L
      out <- c(LETTERS[r + 1L], out)
      i <- (i - 1L) %/% 26L
    }
    paste(out, collapse = "")
  }, character(1))
}
