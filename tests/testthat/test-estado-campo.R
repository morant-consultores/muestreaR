# Estado de campo contra la ruta y la reserva impresas.
#
# La aritmética que importa: una puerta que NO era vivienda (iglesia, baldío)
# no consumió presupuesto y no cuenta como fallo, así que la tasa se mide
# sobre puertas de VIVIENDA. Y una manzana sin acceso se sustituye por la
# siguiente de reserva, que ya está sorteada: no se toca el diseño.

ruta_min <- function() {
  tibble::tibble(
    cluster    = c(1L, 1L, 1L, 2L),
    orden_ruta = c(1L, 2L, 3L, 1L),
    manzana    = c(3L, 5L, 6L, 1L),
    seccion    = c("1957", "1957", "1957", "1958"),
    hoja       = c("1-A", "1-A", "1-A", "2-A"),
    puertas_a_tocar = c(12L, 12L, 12L, 10L),
    presupuesto_puertas_cluster = c(36L, 36L, 36L, 10L),
    entrevistas_meta_cluster    = c(12L, 12L, 12L, 6L)
  )
}
reserva_min <- function() {
  tibble::tibble(
    cluster    = c(1L, 1L, 2L),
    orden_ruta = c(4L, 5L, 2L),
    manzana    = c(7L, 8L, 4L),
    seccion    = c("1957", "1957", "1958"),
    puertas_esperadas_manzana = c(12L, 12L, 10L)
  )
}
cuotas_min <- function() {
  tibble::tibble(cluster_2 = c(1L, 2L), SECCION = c("1957", "1958"),
                 entrevistas = c(12L, 6L), puertas = c(36L, 10L))
}
toque <- function(cluster_2, manzana_num, resultado, clase_razon = NA_character_) {
  tibble::tibble(cluster_2 = as.integer(cluster_2),
                 manzana_num = as.integer(manzana_num),
                 resultado = resultado, clase_razon = clase_razon)
}

test_that("las puertas que no eran vivienda NO consumen presupuesto", {
  toques <- dplyr::bind_rows(
    toque(1, 1, "efectiva"), toque(1, 1, "rechazo"), toque(1, 1, "no_abrio"),
    # cuatro puertas que resultaron ser una iglesia: no son fallo ni gasto
    toque(1, 1, "sin_registro", "no_vivienda"),
    toque(1, 1, "sin_registro", "no_vivienda"),
    toque(1, 1, "sin_registro", "no_vivienda"),
    toque(1, 1, "sin_registro", "no_vivienda"))
  cl <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())$clusters
  c1 <- cl[cl$cluster_2 == 1L, ]
  expect_equal(c1$puertas_vivienda, 3L)
  expect_equal(c1$presupuesto_restante, 33L)   # 36 - 3, NO 36 - 7
  expect_equal(c1$tasa_medida, 1 / 3)          # 1 efectiva / 3 puertas de vivienda
})

test_that("una manzana sin acceso se manda a sustituir con la SIGUIENTE de reserva", {
  toques <- toque(1, 2, "sin_registro", "sin_acceso")
  res <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())
  m <- res$manzanas[res$manzanas$cluster_2 == 1L & res$manzanas$manzana_num == 2L, ]
  expect_equal(m$estado, "sin_acceso")
  expect_match(m$accion, "SUSTITUIR")
  expect_match(m$accion, "4")   # el primer orden_ruta libre de la reserva del cluster 1
})

test_that("dos manzanas sin acceso reciben reservas DISTINTAS", {
  # si las dos apuntaran a la misma, campo caminaría una manzana y creería
  # haber cubierto dos huecos
  toques <- dplyr::bind_rows(toque(1, 1, "sin_registro", "sin_acceso"),
                             toque(1, 2, "sin_registro", "sin_acceso"))
  m <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())$manzanas
  acc <- m$accion[m$cluster_2 == 1L & m$manzana_num %in% 1:2]
  expect_length(unique(acc), 2)
  expect_true(all(grepl("SUSTITUIR", acc)))
})

test_that("una reserva YA recorrida no se vuelve a asignar", {
  toques <- dplyr::bind_rows(
    toque(1, 4, "efectiva"),                              # ya se caminó la reserva 4
    toque(1, 1, "sin_registro", "sin_acceso"))
  m <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())$manzanas
  acc <- m$accion[m$cluster_2 == 1L & m$manzana_num == 1L]
  expect_match(acc, "5")            # brinca la 4
  expect_false(grepl("\\b4\\b", acc))
})

test_that("cuando la reserva se agota, la acción lo dice en vez de inventar", {
  toques <- dplyr::bind_rows(toque(2, 1, "sin_registro", "sin_acceso"),
                             toque(2, 2, "sin_registro", "sin_acceso"))
  m <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())$manzanas
  acc <- m$accion[m$cluster_2 == 2L & m$manzana_num %in% 1:2]
  expect_true(any(grepl("RESERVA AGOTADA", acc)))
})

test_that("'se recorrio y no dio entrevistas' CIERRA la manzana", {
  # el bug del 309: esto se leia igual que "nunca visitada" y campo regresaba
  toques <- toque(1, 3, "sin_registro", "recorrida_sin_entrevistas")
  m <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())$manzanas
  fila <- m[m$cluster_2 == 1L & m$manzana_num == 3L, ]
  expect_equal(fila$estado, "cerrada_por_recorrido")
  expect_equal(fila$accion, "CERRADA")
})

test_that("la manzana se cierra al agotar su presupuesto, con tope", {
  # tope_cierre 1.25 sobre 12 puertas de plan = 15
  toques <- dplyr::bind_rows(replicate(15, toque(1, 1, "no_abrio"), simplify = FALSE))
  m <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min(),
                       tope_cierre = 1.25)$manzanas
  fila <- m[m$cluster_2 == 1L & m$manzana_num == 1L, ]
  expect_equal(fila$estado, "cerrada_por_presupuesto")
})

test_that("una manzana sin tocar queda sin_iniciar y se manda a CONTINUAR", {
  res <- estado_de_campo(toque(1, 1, "efectiva"), ruta_min(), reserva_min(),
                         cuotas_min())
  fila <- res$manzanas[res$manzanas$cluster_2 == 1L & res$manzanas$manzana_num == 2L, ]
  expect_equal(fila$estado, "sin_iniciar")
  expect_equal(fila$accion, "CONTINUAR")
  expect_equal(res$clusters$mzas_sin_iniciar[res$clusters$cluster_2 == 1L], 2L)
})

test_that("un toque en manzana que no existe en el sorteo se marca y NO entra al presupuesto", {
  # el dropdown de manzana del Opinometro ofrece 1..45 igual para los 64
  # clusters, sin condicionar: nada impide capturar una manzana inexistente.
  # Esas puertas tienen probabilidad de seleccion DESCONOCIDA.
  toques <- dplyr::bind_rows(toque(1, 1, "efectiva"), toque(1, 40, "efectiva"))
  res <- estado_de_campo(toques, ruta_min(), reserva_min(), cuotas_min())
  expect_equal(res$clusters$toques_fuera_del_sorteo[res$clusters$cluster_2 == 1L], 1L)
  expect_equal(res$clusters$puertas_vivienda[res$clusters$cluster_2 == 1L], 1L)
  fuera <- res$manzanas[res$manzanas$manzana_num == 40L, ]
  expect_equal(fuera$origen, "fuera_del_sorteo")
  expect_match(fuera$accion, "FUERA DEL SORTEO")
})

test_that("ningun cluster del plan desaparece, aunque no tenga ni un toque", {
  res <- estado_de_campo(toque(1, 1, "efectiva"), ruta_min(), reserva_min(),
                         cuotas_min())
  expect_setequal(res$clusters$cluster_2, c(1L, 2L))
  expect_equal(res$clusters$efectivas[res$clusters$cluster_2 == 2L], 0L)
  expect_true(is.na(res$clusters$tasa_medida[res$clusters$cluster_2 == 2L]))
})

test_that("cuando ruta y cuotas cuadran, la función corre sin error", {
  # ruta_min() y cuotas_min() ya cuadran (12/36 y 6/10 por cluster); esto
  # deja el invariante explícito en vez de darlo por hecho en los demás
  # tests, que ya dependían de que la guarda no truene con estos fixtures
  expect_no_error(estado_de_campo(toque(1, 1, "efectiva"), ruta_min(),
                                  reserva_min(), cuotas_min()))
})

test_that("si `ruta` y `cuotas` discrepan, truena nombrando el cluster", {
  # el bug que evita: si el material impreso (ruta) y el plan con el que
  # se sortearon las cuotas divergen, el Excel reportaría avance contra
  # una meta ajena a ese cluster; mejor que truene ruidoso a que la invente
  cuotas_mala <- cuotas_min()
  cuotas_mala$entrevistas[cuotas_mala$cluster_2 == 1L] <- 99L
  expect_error(estado_de_campo(toque(1, 1, "efectiva"), ruta_min(),
                               reserva_min(), cuotas_mala),
              "cluster 1")
})
