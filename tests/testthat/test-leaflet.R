# Mapa interactivo (leaflet) de la muestra para planear rutas de campo.
# El helper capas_leaflet_ageb() arma las capas sf (municipios, AGEBs y
# manzanas sorteadas) con las columnas de popup; mapa_interactivo_ageb() las
# envuelve en un widget leaflet exportable a HTML. Fixtures en helper-fixture.R.

test_that("capas_leaflet_ageb arma manzanas y AGEBs con la info operativa", {
  diseno <- diseno_ageb_con_muestra()
  cart <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)
  capas <- capas_leaflet_ageb(diseno, cart)

  expect_named(capas, c("municipios", "agebs", "manzanas"))
  expect_s3_class(capas$agebs, "sf")
  expect_s3_class(capas$manzanas, "sf")

  # una fila por AGEB sorteado y una por manzana sorteada
  bd <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  expect_equal(nrow(capas$agebs), dplyr::n_distinct(bd$AGEB))
  expect_equal(nrow(capas$manzanas), dplyr::n_distinct(bd$MZA))

  # columnas para el popup / color de ruta
  expect_true(all(c("AGEB", "MZA", "NOM_MUN", "viviendas", "popup")
                  %in% names(capas$manzanas)))
  expect_true(all(c("AGEB", "manzanas", "contactos", "entrevistas", "mapa",
                    "popup") %in% names(capas$agebs)))

  # las viviendas por manzana salen de n_0 (5 en el fixture)
  expect_true(all(capas$manzanas$viviendas == 5))
  # el resumen del AGEB coincide con resumen_operativo
  res <- resumen_operativo(diseno)
  expect_equal(sort(capas$agebs$contactos), sort(res$contactos))
})

test_that("capas_leaflet_ageb agrega la capa municipal con totales y flag en_muestra", {
  diseno <- diseno_ageb_con_muestra()
  cart <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)
  # un municipio EXTRA que NO sale en la muestra (para el caso sin cobertura)
  extra <- cart$shp$MUN[1, ]
  extra$MUN <- "15999"
  extra$NOM_MUN <- "Fuera de muestra"
  cart$shp$MUN <- rbind(cart$shp$MUN, extra)

  capas <- capas_leaflet_ageb(diseno, cart)
  expect_true("municipios" %in% names(capas))
  mun <- capas$municipios
  expect_s3_class(mun, "sf")
  expect_true(all(c("NOM_MUN", "en_muestra", "agebs", "contactos", "entrevistas")
                  %in% names(mun)))

  bd <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  expect_setequal(mun$MUN[mun$en_muestra], unique(bd$MUN))

  # los totales de la capa municipal cuadran con el resumen por AGEB
  res <- resumen_operativo(diseno)
  expect_equal(sum(mun$contactos), sum(res$contactos))
  expect_equal(sum(mun$entrevistas), sum(res$entrevistas))

  # el municipio fuera de muestra: flag FALSE y totales en cero
  expect_false(mun$en_muestra[mun$MUN == "15999"])
  expect_equal(mun$contactos[mun$MUN == "15999"], 0)
  expect_equal(mun$agebs[mun$MUN == "15999"], 0)
})

test_that("capas_leaflet_ageb avisa cuando una manzana no tiene su AGEB dibujable", {
  diseno <- diseno_ageb_con_muestra()
  cart <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)
  bd <- diseno$muestra |> purrr::pluck(length(diseno$muestra)) |>
    tidyr::unnest(data)
  # marco NO reconciliado: un AGEB sorteado pierde su polígono (sus manzanas
  # sí tienen polígono, pero el contorno del AGEB no existe)
  aul_out <- unique(bd$AULR)[1]
  cart$shp$AGEB <- cart$shp$AGEB[cart$shp$AGEB$AULR != aul_out, ]

  expect_warning(capas_leaflet_ageb(diseno, cart), "reconciliar")
  # con cartografía completa NO avisa
  cart_ok <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)
  expect_no_warning(capas_leaflet_ageb(diseno, cart_ok))
})

test_that("mapa_interactivo_ageb devuelve un widget leaflet y exporta HTML", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("htmlwidgets")
  diseno <- diseno_ageb_con_muestra()
  cart <- cartografia_ageb_prueba(diseno$poblacion$marco_muestral)

  m <- mapa_interactivo_ageb(diseno, cart)
  expect_s3_class(m, "leaflet")

  archivo <- withr::local_tempfile(fileext = ".html")
  ruta <- mapa_interactivo_ageb(diseno, cart, archivo = archivo)
  expect_equal(ruta, archivo)
  expect_true(file.exists(archivo))
  expect_gt(file.info(archivo)$size, 0)
})
