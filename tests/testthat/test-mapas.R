# La anotación de los mapas de campo. El equipo ya NO levanta por cuotas
# (la composición la corrige el rake), así que el subtítulo del mapa dejó de
# mostrar cuotas y ahora indica lo operativo: el zoom del mapa, las manzanas
# a visitar, los contactos (viviendas a levantar) y las entrevistas efectivas
# planeadas. Aplica a los dos flujos: censal INEGI (google_maps) y electoral
# INE (google_maps_ine), vía los helpers resumen_operativo()/etiqueta_mapa().

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

test_that("clusters_dibujables descarta los conglomerados sin polígono", {
  # shp_mapa solo trae los conglomerados que sobreviven el join a la
  # cartografía; los sorteados sin polígono (p. ej. AGEBs sin marco 2025)
  # no deben entrar al loop de google_maps (centroide de geometría vacía).
  shp_mapa <- tibble::tibble(cluster_2 = c(1L, 2L, 4L))
  cluster <- c(1L, 2L, 3L, 4L)
  expect_equal(clusters_dibujables(cluster, shp_mapa, "cluster_2"),
               c(1L, 2L, 4L))
  # conserva el orden de `cluster` y no inventa conglomerados
  expect_equal(clusters_dibujables(c(4L, 1L), shp_mapa, "cluster_2"),
               c(4L, 1L))
  expect_length(clusters_dibujables(5L, shp_mapa, "cluster_2"), 0)
})

test_that("resumen_operativo numera los mapas (n de N) de forma estable", {
  res <- resumen_operativo(diseno_ageb_con_muestra())
  expect_true(all(c("mapa", "total_mapas") %in% names(res)))
  # numeración 1..N sin huecos y N = número de conglomerados
  expect_equal(sort(res$mapa), seq_len(nrow(res)))
  expect_true(all(res$total_mapas == nrow(res)))
  # estable: el número de cada conglomerado no depende del orden de dibujo
  res2 <- resumen_operativo(diseno_ageb_con_muestra())
  expect_equal(res$mapa[order(res$cluster_2)],
               res2$mapa[order(res2$cluster_2)])
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

test_that("numerar_manzanas asigna 1..k por conglomerado, estable y compartible", {
  diseno <- diseno_ageb_con_muestra()
  num <- numerar_manzanas(diseno)

  expect_true(all(c("cluster_2", "cluster_0", "manzana_num") %in% names(num)))
  # una fila por manzana sorteada
  bd <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  expect_equal(nrow(num), nrow(bd))
  # dentro de cada conglomerado: 1..k sin huecos
  por_cluster <- split(num$manzana_num, num$cluster_2)
  expect_true(all(vapply(por_cluster,
                         function(x) identical(sort(x), seq_along(x)),
                         logical(1))))
  # estable: misma numeración en llamadas repetidas (es el id del cuestionario)
  expect_identical(num, numerar_manzanas(diseno))
  # en el flujo censal la numeración sigue el orden de la clave MZA
  chk <- bd |>
    dplyr::left_join(num, by = c("cluster_2", "cluster_0")) |>
    dplyr::group_by(cluster_2) |>
    dplyr::arrange(MZA, .by_group = TRUE) |>
    dplyr::summarise(ok = identical(manzana_num, seq_len(dplyr::n())),
                     .groups = "drop")
  expect_true(all(chk$ok))
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
