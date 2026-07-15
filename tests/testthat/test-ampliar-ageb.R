# ampliar_manzanas_ageb(): sorteo complementario ADITIVO ---------------------
# La restricción que gobierna todo: la muestra que ya está en campo NO se
# toca — mismas manzanas, misma numeración corta. La ampliación solo agrega.

test_that("la ampliación conserva intactas las manzanas que ya están en campo", {
  d <- diseno_ageb_con_muestra()
  ultimo <- length(d$muestra)
  antes <- d$muestra[[ultimo]]

  d2 <- ampliar_manzanas_ageb(d, manzanas_por_ageb = 5, semilla = 99)
  despues <- d2$muestra[[length(d2$muestra)]]

  # todas las originales siguen en la muestra ampliada, con su fila idéntica
  expect_true(all(antes$cluster_0 %in% despues$cluster_0))
  expect_identical(
    despues |> dplyr::semi_join(antes, by = "cluster_0") |>
      dplyr::arrange(cluster_0) |> tidyr::unnest(data),
    antes |> dplyr::arrange(cluster_0) |> tidyr::unnest(data)
  )
  # el diseño ORIGINAL no se modifica (la ampliación regresa un clon)
  expect_identical(nrow(d$muestra[[ultimo]]), nrow(antes))
})

test_that("agrega hasta el objetivo por AGEB respetando la disponibilidad del marco", {
  d <- diseno_ageb_con_muestra()      # fixture: 6 manzanas por AGEB, 4 sorteadas
  u_cluster <- "cluster_2"
  marco <- d$poblacion$marco_muestral
  sorteados <- unique(d$muestra[[length(d$muestra)]][[u_cluster]])

  # objetivo alcanzable (5 de 6): todos los AGEBs quedan exactos
  d2 <- ampliar_manzanas_ageb(d, manzanas_por_ageb = 5, semilla = 99)
  conteo <- d2$muestra[[length(d2$muestra)]] |> dplyr::count(cluster_2)
  expect_true(all(conteo$n == 5))

  # objetivo mayor que el marco (9 > 6): entran todas las disponibles, sin error
  d3 <- ampliar_manzanas_ageb(d, manzanas_por_ageb = 9, semilla = 99)
  conteo3 <- d3$muestra[[length(d3$muestra)]] |> dplyr::count(cluster_2)
  disponibles <- marco |>
    dplyr::filter(cluster_2 %in% sorteados) |>
    dplyr::count(cluster_2, name = "en_marco")
  expect_identical(
    conteo3 |> dplyr::left_join(disponibles, by = "cluster_2") |>
      dplyr::mutate(ok = n == pmin(9, en_marco)) |> dplyr::pull(ok) |> all(),
    TRUE
  )

  # las nuevas salen del marco del MISMO AGEB y no repiten manzana
  nuevas <- d3$muestra[[length(d3$muestra)]] |>
    dplyr::anti_join(d$muestra[[length(d$muestra)]], by = "cluster_0")
  expect_true(all(nuevas$cluster_0 %in% marco$cluster_0))
  expect_identical(anyDuplicated(d3$muestra[[length(d3$muestra)]]$cluster_0), 0L)
})

test_that("la numeración de campo se conserva y las manzanas nuevas continúan", {
  d <- diseno_ageb_con_muestra()
  num_antes <- numerar_manzanas(d)

  d2 <- ampliar_manzanas_ageb(d, manzanas_por_ageb = 6, semilla = 99)
  num_despues <- numerar_manzanas(d2)

  # las manzanas en campo conservan EXACTAMENTE su número (1..4 impreso en mapas)
  expect_identical(
    num_despues |> dplyr::semi_join(num_antes, by = "cluster_0") |>
      dplyr::arrange(cluster_0),
    num_antes |> dplyr::arrange(cluster_0)
  )
  # las nuevas continúan la numeración: 5, 6, ... consecutivas por cluster
  nuevas <- num_despues |> dplyr::anti_join(num_antes, by = "cluster_0")
  rangos <- nuevas |>
    dplyr::left_join(
      num_antes |> dplyr::group_by(cluster_2) |>
        dplyr::summarise(tope = max(manzana_num), .groups = "drop"),
      by = "cluster_2") |>
    dplyr::group_by(cluster_2) |>
    dplyr::summarise(ok = identical(sort(manzana_num),
                                    seq(unique(tope) + 1L, length.out = dplyr::n())),
                     .groups = "drop")
  expect_true(all(rangos$ok))
})

test_that("es reproducible con semilla y actualiza n, asignación y plan", {
  d <- diseno_ageb_con_muestra()
  d2a <- ampliar_manzanas_ageb(d, manzanas_por_ageb = 6, semilla = 99)
  d2b <- ampliar_manzanas_ageb(d, manzanas_por_ageb = 6, semilla = 99)
  expect_identical(sort(d2a$muestra[[length(d2a$muestra)]]$cluster_0),
                   sort(d2b$muestra[[length(d2b$muestra)]]$cluster_0))

  # n = total a levantar = suma de n_0 sobre las manzanas de la muestra
  por_manzana <- d2a$muestra[[length(d2a$muestra)]] |>
    dplyr::left_join(d2a$n_i$cluster_0, by = "cluster_0")
  expect_identical(as.numeric(d2a$n), as.numeric(sum(por_manzana$n_0)))

  # asignación: entrevistas objetivo intactas, a levantar recalculado
  asig <- attr(d2a, "asignacion")
  expect_identical(asig$entrevistas, attr(d, "asignacion")$entrevistas)
  expect_identical(as.numeric(sum(asig$entrevistas_a_levantar)),
                   as.numeric(sum(por_manzana$n_0)))

  # plan versionado: pi_ageb intactas (mismos AGEBs, mismo sorteo de etapa I)
  # y contactos por AGEB = suma real de n_0 de sus manzanas
  plan_antes <- attr(d, "plan_ageb")
  plan2 <- attr(d2a, "plan_ageb")
  expect_identical(sort(plan2$ageb), sort(plan_antes$ageb))
  expect_identical(plan2 |> dplyr::arrange(ageb) |> dplyr::pull(pi_ageb),
                   plan_antes |> dplyr::arrange(ageb) |> dplyr::pull(pi_ageb))
  var_ageb <- d$niveles$variable[d$niveles$nivel == d$ultimo_nivel]
  reales <- por_manzana |>
    dplyr::left_join(d2a$poblacion$marco_muestral |>
                       dplyr::distinct(cluster_0, .data[[var_ageb]]),
                     by = "cluster_0") |>
    dplyr::group_by(.data[[var_ageb]]) |>
    dplyr::summarise(contactos_real = sum(n_0), .groups = "drop")
  expect_identical(
    plan2 |> dplyr::arrange(ageb) |> dplyr::pull(contactos),
    reales |> dplyr::arrange(.data[[var_ageb]]) |> dplyr::pull(contactos_real)
  )
})

test_that("acepta un objetivo DISTINTO por AGEB (tabla cluster -> objetivo)", {
  d <- diseno_ageb_con_muestra()      # fixture: 6 manzanas por AGEB, 4 sorteadas
  antes <- d$muestra[[length(d$muestra)]]
  clusters <- sort(unique(antes$cluster_2))
  # al primero pedirle 6, al segundo 5, el resto NO se toca (sin fila)
  objetivos <- tibble::tibble(cluster_2 = clusters[1:2], objetivo = c(6L, 5L))

  d2 <- ampliar_manzanas_ageb(d, manzanas_por_ageb = objetivos, semilla = 99)
  conteo <- d2$muestra[[length(d2$muestra)]] |> dplyr::count(cluster_2)

  expect_identical(conteo$n[conteo$cluster_2 == clusters[1]], 6L)
  expect_identical(conteo$n[conteo$cluster_2 == clusters[2]], 5L)
  # los AGEBs sin fila en la tabla conservan sus manzanas tal cual
  sin_objetivo <- setdiff(clusters, clusters[1:2])
  antes_conteo <- antes |> dplyr::count(cluster_2)
  for (cl in sin_objetivo) {
    expect_identical(conteo$n[conteo$cluster_2 == cl],
                     antes_conteo$n[antes_conteo$cluster_2 == cl])
  }
  # y las manzanas originales siguen intactas
  expect_identical(
    d2$muestra[[length(d2$muestra)]] |>
      dplyr::semi_join(antes, by = "cluster_0") |>
      dplyr::arrange(cluster_0) |> tidyr::unnest(data),
    antes |> dplyr::arrange(cluster_0) |> tidyr::unnest(data)
  )
})
