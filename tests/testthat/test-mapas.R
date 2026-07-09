# La anotación de los mapas de campo. El equipo ya NO levanta por cuotas
# (la composición la corrige el rake), así que el subtítulo del mapa dejó de
# mostrar cuotas y ahora indica lo operativo: el zoom del mapa, las manzanas
# a visitar, los contactos (viviendas a levantar) y las entrevistas efectivas
# planeadas. Aplica a los dos flujos: censal INEGI (google_maps) y electoral
# INE (google_maps_ine), vía los helpers resumen_operativo()/etiqueta_mapa().

diseno_ageb_con_muestra <- function() {
  pob <- PoblacionAGEB$new("Fixture", censo_clase_prueba())
  pob$marco_muestral <- pob$marco_muestral |> dplyr::mutate(region = NOM_MUN)
  suppressWarnings(disenar_muestra_ageb(
    pob,
    estratos = tibble::tibble(estrato = c("Nezahualcóyotl", "Toluca"),
                              entrevistas = c(20, 20)),
    n_0 = 5, manzanas_por_ageb = 2,
    tasa_rechazo = 0.5, modo_rechazo = "manzanas",
    calcular_cuotas = FALSE, semilla = 7
  ))
}

test_that("resumen_operativo da manzanas, contactos y entrevistas por conglomerado", {
  diseno <- diseno_ageb_con_muestra()
  res <- resumen_operativo(diseno)

  expect_true(all(c("cluster_2", "manzanas", "contactos", "entrevistas")
                  %in% names(res)))
  # 2 AGEBs por estrato, 4 manzanas por AGEB (2 base x 2 del rechazo 0.5)
  expect_true(all(res$manzanas == 4))
  # contactos = manzanas x n_0 = 4 x 5 = 20 viviendas a levantar por AGEB
  expect_true(all(res$contactos == 20))
  # tasa 0.5 (de la asignación) => 10 efectivas planeadas por AGEB
  expect_true(all(res$entrevistas == 10))
})

test_that("etiqueta_mapa arma el subtítulo con zoom, manzanas, contactos y entrevistas", {
  res <- resumen_operativo(diseno_ageb_con_muestra())
  txt <- etiqueta_mapa(res[1, ], zoom = 16)

  expect_match(txt, "Zoom: 16")
  expect_match(txt, "Manzanas: 4")
  expect_match(txt, "Contactos planeados: 20")
  expect_match(txt, "Entrevistas planeadas: 10")
  # ya no lleva el desglose de cuotas por rango/sexo
  expect_false(grepl("rango|sexo|A24|60YMAS", txt, ignore.case = TRUE))
})

test_that("resumen_operativo opera igual sobre el flujo electoral INE", {
  diseno <- generar_diseno_ine()
  suppressWarnings({
    diseno$extraer_muestra(nivel = 1)
    diseno$extraer_muestra(nivel = 2)
  })

  res <- resumen_operativo(diseno)
  expect_true(all(c("cluster_2", "manzanas", "contactos", "entrevistas")
                  %in% names(res)))
  expect_true(all(res$contactos > 0))
  expect_true(all(res$manzanas > 0))
  # diseño hecho a mano, sin atributo "asignacion": sin tasa declarada, las
  # entrevistas efectivas planeadas igualan a los contactos (rechazo 0)
  expect_equal(res$entrevistas, res$contactos)
})

test_that("resumen_operativo usa la tasa de rechazo de la asignación si existe", {
  diseno <- diseno_ageb_con_muestra()
  # con la asignación (tasa 0.5) las entrevistas son la mitad de los contactos
  res <- resumen_operativo(diseno)
  expect_equal(res$entrevistas, round(res$contactos * 0.5))

  # si se borra el atributo, cae al default (tasa 0): entrevistas = contactos
  attr(diseno, "asignacion") <- NULL
  res0 <- resumen_operativo(diseno)
  expect_equal(res0$entrevistas, res0$contactos)
})
