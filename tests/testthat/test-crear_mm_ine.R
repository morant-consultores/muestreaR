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

test_that("crear_mm_ine procesa la rama con columna LISTA (relleno proporcional)", {
  # Los marcos reales del INE traen una columna LISTA en shp_mza que activa la
  # rama de relleno proporcional al final de crear_mm_ine. Esta rama usa funciones
  # de tidyr: si no están prefijadas falla en producción aunque el fixture base
  # (sin LISTA) pase. Este test la ejercita explícitamente.
  fx <- generar_fixture_crear_mm(con_lista = TRUE)

  marco <- suppressWarnings(
    crear_mm_ine(ln = fx$ln, shp_mza = fx$shp_mza,
                 shp_loc = fx$shp_loc, shp_mun = fx$shp_mun)
  )

  expect_true(all(marco$lista_nominal > 0))
  expect_false("LISTA" %in% names(marco))            # la columna LISTA se consume
  expect_length(grep("^LN22_", names(marco), value = TRUE), 8)
})
