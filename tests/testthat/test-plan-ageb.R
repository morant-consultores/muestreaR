# Muestra por AGEB: motor UPM genérico detrás del plan seccional, cara
# planear_muestra_ageb, puente al contrato de encuestar y olas por unidad.
# El fixture marco_ageb_prueba() vive en helper-fixture.R (lo comparten los
# tests de manzanas).

# ---- asignar_potencia por unidad ----

test_that("asignar_potencia nombra las columnas de conteo por unidad", {
  asig <- asignar_potencia(marco_ageb_prueba(), n_total = 200,
                           m_por_seccion = 10,
                           variable_tamano = "pob18", unidad = "ageb")
  expect_true(all(c("agebs", "agebs_disponibles") %in% names(asig)))
  expect_false(any(c("secciones", "secciones_disponibles") %in% names(asig)))
  expect_equal(asig$entrevistas_plan, asig$agebs * 10)
})

# ---- aplicar_lista_negra por unidad ----

test_that("aplicar_lista_negra documenta con la llave y el tamaño de la unidad", {
  marco <- marco_ageb_prueba()
  res <- aplicar_lista_negra(marco, secciones = marco$ageb[1:2],
                             municipios = "15058",
                             llave_seccion = "ageb",
                             variable_tamano = "pob18",
                             unidad = "ageb")
  doc <- attr(res, "lista_negra")
  expect_true(all(c("ageb", "estrato", "pob18", "motivo") %in% names(doc)))
  expect_false("seccion" %in% names(doc))
  expect_setequal(unique(doc$motivo),
                  c("municipio en lista negra", "ageb en lista negra"))
  expect_equal(nrow(res), 18)
  expect_equal(nrow(doc), 22)
})
