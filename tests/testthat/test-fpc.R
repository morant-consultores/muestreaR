# Invariantes de los factores de corrección poblacional (fpc).
# Un fpc, al ser una probabilidad de inclusión, debe estar en (0, 1].

test_that("los fpc calculados son probabilidades válidas en (0, 1]", {
  diseno <- generar_diseno_ine()
  marco  <- diseno$poblacion$marco_muestral

  cols_fpc <- grep("^fpc_", names(marco), value = TRUE)
  expect_true(length(cols_fpc) >= 1)   # se generaron columnas fpc

  for (col in cols_fpc) {
    valores <- marco[[col]]
    valores <- valores[!is.na(valores)]
    expect_true(all(valores > 0),  info = paste(col, "tiene valores <= 0"))
    expect_true(all(valores <= 1), info = paste(col, "tiene valores > 1"))
  }
})
