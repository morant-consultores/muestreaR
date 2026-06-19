# crear_mm_ine() debe construir un marco muestral válido a partir de los shapefiles
# y la lista nominal. Aquí se ejercita con el fixture geoespacial sintético.

test_that("crear_mm_ine corre y produce las columnas esperadas", {
  fx <- generar_fixture_crear_mm()

  marco <- suppressWarnings(
    crear_mm_ine(ln = fx$ln, shp_mza = fx$shp_mza,
                 shp_loc = fx$shp_loc, shp_mun = fx$shp_mun)
  )

  # una fila por manzana del fixture (12)
  expect_equal(nrow(marco), nrow(fx$shp_mza))

  # columnas clave que el resto del flujo de muestreo consume
  expect_true(all(c("id", "ENTIDAD", "MUNICIPIO", "SECCION", "NOMBRE_MUN",
                    "lista_nominal") %in% names(marco)))

  # las 8 celdas de edad x sexo
  cols_ln22 <- grep("^LN22_", names(marco), value = TRUE)
  expect_length(cols_ln22, 8)
})

test_that("crear_mm_ine genera tamaños poblacionales positivos y consistentes", {
  fx <- generar_fixture_crear_mm()
  marco <- suppressWarnings(
    crear_mm_ine(ln = fx$ln, shp_mza = fx$shp_mza,
                 shp_loc = fx$shp_loc, shp_mun = fx$shp_mun)
  )

  expect_true(all(marco$lista_nominal > 0))                # tamaño positivo
  expect_false(any(is.na(marco$lista_nominal)))            # sin NA

  # la lista nominal se reparte entre las manzanas de cada sección: el total por
  # sección debe reconstruir el total original de la lista nominal del fixture.
  total_por_seccion <- marco |>
    dplyr::group_by(SECCION) |>
    dplyr::summarise(total = sum(lista_nominal), .groups = "drop") |>
    dplyr::arrange(SECCION)

  esperado <- fx$ln |>
    dplyr::transmute(SECCION = as.character(SECCION), total = `LISTA NOMINAL`) |>
    dplyr::arrange(SECCION)

  expect_equal(total_por_seccion$total, esperado$total)
})
