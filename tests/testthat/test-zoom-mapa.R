# zoom_para_bbox(): el zoom del mapa de campo se calcula POR conglomerado ---
# Con zoom fijo, al crecer la muestra (p. ej. ampliación 4 -> 9 manzanas por
# AGEB) el encuadre corta manzanas. El helper elige el mayor zoom de Google
# Static Maps al que el bbox del conglomerado (con margen) cabe en el tile.

# píxeles que ocupa el bbox en un tile Web Mercator al zoom z
px_bbox <- function(bb, z, size = 640) {
  merc <- function(lat) log(tan(pi / 4 + lat * pi / 360))
  lon_px <- (bb[["xmax"]] - bb[["xmin"]]) / 360 * 256 * 2^z
  lat_px <- (merc(bb[["ymax"]]) - merc(bb[["ymin"]])) / (2 * pi) * 256 * 2^z
  max(lon_px, lat_px)
}

test_that("el bbox cabe al zoom elegido y ya no cabe al siguiente", {
  bb <- c(xmin = -99.05, ymin = 19.50, xmax = -99.00, ymax = 19.55)
  z <- zoom_para_bbox(bb)
  expect_lt(z, 16L)
  expect_lte(px_bbox(bb, z) * 1.15, 640)        # cabe con el margen
  expect_gt(px_bbox(bb, z + 1) * 1.15, 640)     # el siguiente ya no
})

test_that("conglomerados chicos se topan en zoom_max (máximo detalle)", {
  bb <- c(xmin = -99.001, ymin = 19.500, xmax = -99.000, ymax = 19.501)
  expect_identical(zoom_para_bbox(bb, zoom_max = 16), 16L)
  # un bbox degenerado (punto) no truena: máximo detalle
  expect_identical(zoom_para_bbox(c(xmin = -99, ymin = 19,
                                    xmax = -99, ymax = 19)), 16L)
})

test_that("a mayor extensión, menor zoom (monotonía) y respeta zoom_max", {
  chico <- c(xmin = -99.02, ymin = 19.50, xmax = -99.00, ymax = 19.52)
  grande <- c(xmin = -99.20, ymin = 19.40, xmax = -99.00, ymax = 19.60)
  expect_gt(zoom_para_bbox(chico), zoom_para_bbox(grande))
  expect_lte(zoom_para_bbox(chico, zoom_max = 14), 14L)
  expect_lte(zoom_para_bbox(c(xmin = -180, ymin = -60,
                              xmax = 180, ymax = 60)), 3L)
})
