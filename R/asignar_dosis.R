# Reparto de entrevistas entre unidades de muestreo.
#
# Complementa a [calcular_asignacion()]: ahí la dosis por sección es FIJA y la
# autoponderación la da el PPS; aquí la dosis es PROPORCIONAL al tamaño, que es lo
# que hace falta cuando el PPS no puede darla.

#' Reparto de entrevistas proporcional al tamaño, con piso por unidad
#'
#' Reparte `total` entrevistas entre unidades de muestreo (secciones, AGEBs)
#' proporcionalmente a su tamaño, con un piso por unidad, y devuelve enteros que
#' suman **exactamente** `total`.
#'
#' @section Cuándo hace falta:
#' El modelo operativo estándar reparte con dosis FIJA por unidad y consigue la
#' autoponderación por el lado del PPS: si la probabilidad de inclusión es
#' proporcional al tamaño (`pi_i = m * t_i / sum(t)`) y la dosis es constante
#' (`n_0`), la probabilidad de una persona es `n_0 * m / sum(t)` — la misma en
#' todo el estrato.
#'
#' Esa vía **se cierra cuando las unidades entran completas**. Si el estrato cabe
#' entero en el operativo y se seleccionan todas sus unidades, `pi_i` vale 1 para
#' todas y deja de ser proporcional al tamaño: con dosis fija, la probabilidad de
#' una persona pasa a ser `n_0 / t_i`, que varía tanto como varíen las unidades.
#' Medido en Huehuetoca (ago-2026), con 64 secciones de 287 a 10,685 en lista
#' nominal, la dosis fija abría los pesos en razón 37:1 y el efecto de diseño
#' atribuible a ellos llegaba a 2.22; con la dosis repartida por esta función bajó
#' a 1.03.
#'
#' La regla práctica: **dosis fija con PPS, o dosis proporcional con censo.**
#' Mezclarlas (censo con dosis fija) no da ninguna de las dos.
#'
#' @section El piso:
#' Sin piso, la proporcionalidad pura manda 2 o 3 entrevistas a las unidades más
#' chicas, y eso no es operable: una cuadrilla no se despacha por dos entrevistas
#' y media manzana rompe la regla de manzana completa del sistemático. El piso
#' natural es `n_0 * manzanas_por_seccion` (una manzana completa).
#'
#' Las unidades que quedan EN el piso terminan levemente sobre-representadas y su
#' peso baja. El efecto es acotado y conviene medirlo: el coeficiente de variación
#' de `tamano / dosis` es el que manda, y `1 + CV^2` es el efecto de diseño que
#' los pesos aportan por su cuenta.
#'
#' @section Método:
#' Mayores restos (Hamilton), vía [repartir_cociente()]. Primero se busca por
#' bisección el factor de proporcionalidad que, **con el piso ya aplicado**, hace
#' que el objetivo continuo sume `total`; luego se reparte la parte entera y los
#' enteros que falten se dan a las unidades con el resto fraccionario más grande.
#' Es determinista —mismo insumo, mismo reparto, sin semilla— y por eso
#' reproducible.
#'
#' @param tamano Vector numérico positivo con la medida de tamaño de cada unidad
#'   (típicamente la lista nominal).
#' @param total Entero: total de entrevistas a repartir.
#' @param piso Entero: mínimo de entrevistas por unidad (default 1).
#'
#' @return Vector entero de la misma longitud que `tamano`, que suma `total` y
#'   respeta `piso` en todas sus entradas.
#'
#' @seealso [calcular_asignacion()] para el reparto con dosis fija;
#'   [repartir_cociente()] para el redondeo por mayores restos.
#'
#' @examples
#' # tres unidades muy desiguales: la dosis sigue al tamaño
#' asignar_dosis_proporcional(c(287, 1090, 10685), total = 768, piso = 6)
#'
#' # con unidades iguales el reparto es uniforme
#' asignar_dosis_proporcional(rep(100, 8), total = 80)
#'
#' # el piso protege a las chicas y se respeta siempre
#' d <- asignar_dosis_proporcional(c(50, 50, 5000), total = 100, piso = 10)
#' sum(d)        # 100 exacto
#' min(d) >= 10  # TRUE
#' @export
asignar_dosis_proporcional <- function(tamano, total, piso = 1L) {
  if (!is.numeric(tamano) || !length(tamano)) {
    stop("`tamano` debe ser un vector numérico no vacío.", call. = FALSE)
  }
  if (anyNA(tamano) || any(tamano <= 0)) {
    stop("`tamano` debe ser positivo y sin NA: es una medida de tamaño.",
         call. = FALSE)
  }
  if (length(total) != 1 || is.na(total) || total != round(total) || total <= 0) {
    stop("`total` debe ser un entero positivo.", call. = FALSE)
  }
  if (length(piso) != 1 || is.na(piso) || piso != round(piso) || piso < 0) {
    stop("`piso` debe ser un entero no negativo.", call. = FALSE)
  }

  n <- length(tamano)
  if (total < n * piso) {
    stop("El total (", total, ") no alcanza para dar el piso de ", piso,
         " a las ", n, " unidades (haría falta ", n * piso, ").", call. = FALSE)
  }

  # Con el piso al tope no hay nada que repartir: cualquier factor daría lo mismo
  # y la bisección de abajo no tendría raíz.
  if (total == n * piso) {
    return(rep(as.integer(piso), n))
  }

  # Factor de proporcionalidad que, CON el piso aplicado, hace que el objetivo
  # continuo sume `total`. La función es monótona no decreciente en `p`, así que
  # la bisección converge; el extremo superior `total` es holgado porque
  # `p = total` ya asigna al menos `total` a la unidad más chica.
  f <- function(p) sum(pmax(piso, p * tamano)) - total
  p <- stats::uniroot(f, c(.Machine$double.eps, total), tol = 1e-12)$root
  objetivo <- pmax(piso, p * tamano)

  # Redondeo por mayores restos. Las unidades sujetas por el piso tienen residuo
  # 0, así que no reciben sobrante: crecer ahí las alejaría de su proporción en
  # vez de acercarlas.
  dosis <- repartir_cociente(n = total, x = objetivo)

  if (sum(dosis) != total || any(dosis < piso)) {
    stop("El reparto no cuadró (suma ", sum(dosis), " contra ", total,
         ", mínimo ", min(dosis), " contra un piso de ", piso,
         "). Reporta este caso: es un error del algoritmo, no del insumo.",
         call. = FALSE)
  }
  as.integer(dosis)
}
