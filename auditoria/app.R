#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(readr)
library(purrr)
library(tibble)
library(sf)
library(readxl)
library(dplyr)
library(tidyr)
library(muestreaR)
library(tibble)
library(gt)
library(glue)
library(ggplot2)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinycssloaders)
library(colorRamps)
# Define UI for application that draws a histogram

# Lectura -----------------------------------------------------------------

diseño <- read_rds("data/diseño_qro5.rda")
shp <- read_rds("data/shp_qro.rda")
# list.files("R",full.names = T) %>% walk(~source(.x))
bd <- read_excel("data/bd.xlsx")
eliminadas <- read_excel("data/Eliminadas.xlsx") %>% rename(SbjNum = ID)
sin_z <- read_excel("data/Sin zona .xlsx") %>% mutate(Razón = "Zona equivocada")
eliminadas <- bind_rows(eliminadas, sin_z)
bbox_qro <- st_bbox(shp$shp$MUN)
# bd a shp ----------------------------------------------------------------

enc <- bd %>%
  anti_join(eliminadas) %>%
  filter(!is.na(Longitude)) %>%
  select(cluster = CLUSTER, edad = PB, sexo = P21,
         Encuestador = Srvyr, id = SbjNum, Latitude,Longitude) %>%
  mutate(Latitude = as.double(Latitude))
enc_shp <- enc %>%
  st_as_sf(coords = c("Longitude","Latitude"), crs = "+init=epsg:4326")

# aulr en muestra ---------------------------------------------------------

aulr <- shp$shp$AULR %>%
  inner_join(diseño$muestra[[3]] %>%
               unnest(data) %>% distinct(AULR,cluster_3))




# dentro y fuera ----------------------------------------------------------
#
# todas <- enc_shp %>%
#   st_join(aulr %>%
#             filter(stringr::str_detect(AULR,"Urbana")))
# fuera <-  todas %>%
#           filter(is.na(AULR)) %>%
#           mutate(color="black")
#
# dentro <-  todas %>%
#   filter(!is.na(AULR)) %>%
#   mutate(distinto=if_else(cluster_3!=cluster, 1, 0),
#          cluster_3=as.character(cluster_3),
#          cluster_3=if_else(distinto==1, cluster_3, cluster),
#          color="green")
#
# dentro %>% readr::write_rds("data/dentro.rda")
# fuera %>% readr::write_rds("data/fuera.rda")
dentro <- readr::read_rds("data/dentro.rda")
fuera <- readr::read_rds("data/fuera.rda")
enc_shp <- bind_rows(dentro %>% mutate(cluster_3 = as.numeric(cluster_3)), fuera %>% mutate(cluster_3 = as.numeric(cluster)))
# reordenar clusters ------------------------------------------------------

nuevos <- dentro %>%
  as_tibble() %>%
  filter(distinto==1) %>%
  select(id, cluster_3, distinto)

enc <- left_join(enc, nuevos, by="id")

enc$distinto[is.na(enc$distinto)] <- 0

enc<- enc %>%
  mutate(cluster=if_else(distinto==1, cluster_3, cluster)) %>%
  select(-cluster_3, -distinto)

enc <- enc %>% semi_join(diseño$muestra$AULR %>% mutate(cluster_3 = as.character(cluster_3)),
          by = c("cluster" = "cluster_3"))

enc_shp <- enc_shp %>% semi_join(diseño$muestra$AULR,
                         by = c("cluster_3"))

# cuotas ------------------------------------------------------------------

hecho <- enc %>%
  mutate(edad = as.character(cut(as.integer(edad),c(17,24,59,200),
                                 c("18A24","25A59","60YMAS"))),
         cluster = as.numeric(cluster)) %>%
  count(cluster, edad, sexo, name = "hecho") %>%
  full_join(
    diseño$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
      rename(cuota = n, cluster = cluster_3, edad = rango)
  ) %>% replace_na(list(hecho = 0, faltan = 0)) %>%
  mutate(faltan = cuota - hecho) %>% filter(cluster %in% diseño$cuotas$cluster_3)

por_hacer <- diseño$cuotas %>% mutate(sexo = if_else(sexo == "F", "Mujer", "Hombre")) %>%
  rename(cuota = n, cluster = cluster_3, edad = rango) %>%
  left_join(
    enc %>%
      mutate(edad = as.character(cut(as.integer(edad),c(17,24,59,200),
                                     c("18A24","25A59","60YMAS"))),
             cluster = as.numeric(cluster)) %>%
      count(cluster, edad, sexo, name = "hecho")
  ) %>% replace_na(list(hecho = 0)) %>% mutate(por_hacer = cuota-hecho,
                                               por_hacer2 = if_else(por_hacer < 0, 0, por_hacer)
  )





ui <-dashboardPage(
  dashboardHeader(title = diseño$poblacion$nombre),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Mapa", tabName = "mapa", icon = icon("map")
      ),
      menuItem(
        "Entrevistas", tabName = "entrevistas", icon = icon("poll")
      ),
      menuItem(
        "Auditoría", tabName = "auditoria", icon = icon("search")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("mapa",
              leafletOutput(outputId = "map", height = 600),

              # Shiny versions prior to 0.11 should use class = "modal" instead.
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                            width = 330, height = "auto",
                            HTML('<button data-toggle="collapse" data-target="#demo">Min/max</button>'),
                            tags$div(id = 'demo',  class="collapse",
                                     selectInput("cluster", "Cluster", c("Seleccione..."= "",sort(unique(diseño$muestra[[3]]$cluster_3)))
                                     ),
                                     actionButton("filtrar","Filtrar"),
                                     gt_output("faltantes"),
                                     hr(),
                                     actionButton("regresar", "Regresar")
                            )
              )
      ),
      tabItem("entrevistas",
              h2("Entrevistas realizadas"),

              fluidRow(
                column(12,
                       progressBar(id = "enc_hechas", value = nrow(enc),display_pct = T,striped = T,
                                   total = (diseño$niveles %>% filter(nivel == 0) %>% pull(unidades))*diseño$n_0,
                                   status = "success"
                       )
                )
              ),
              fluidRow(
                valueBox(width = 6,
                         value = por_hacer %>% filter(por_hacer < 0) %>% summarise(sum(por_hacer)) %>% pull(1) %>% abs(),
                         subtitle = "Entrevistas hechas de más según la cuota",color = "yellow",
                         icon = icon("plus")
                ),
                valueBox(width = 6,
                         value = nrow(bd) - nrow(enc),
                         subtitle = "Entrevistas eliminadas",
                         color = "red",
                         icon = icon("times")
                )
              ),
              h2("Entrevitas por hacer"),
              fluidRow(
                column(6,
                       withSpinner(plotOutput("por_hacer",height = 600)),
                       ),
                column(6,
                       withSpinner(plotOutput("por_hacer_cuotas",height = 600))
                       )
              ),
              h2("Eliminadas"),
              DTOutput("eliminadas")
      ),
      tabItem("auditoria",
              passwordInput("psw","Contraseña"),
              uiOutput("graficas")
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$map <- renderLeaflet({

    # leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
    shp$graficar_mapa(bd = diseño$poblacion$marco_muestral, nivel = "MUN") %>%
      shp$graficar_mapa(bd = diseño$muestra, nivel = "AULR") %>%
      addCircleMarkers(data = enc_shp, color = ~color, stroke = F, label = ~glue::glue("{cluster_3}-{Encuestador}-{id}")) %>%
      # addCircleMarkers(data = fuera, color = "black", stroke = F) %>%
      addLegend(position = "bottomright", colors = c("green", "black"), labels = c("dentro", "fuera"),
                title = "Entrevistas")
  })

  proxy <- leafletProxy("map")

  observeEvent(input$filtrar,{
    bbox <-  aulr %>% filter(cluster_3 == !!input$cluster) %>% sf::st_bbox()

    proxy %>% flyToBounds(bbox[[1]],bbox[[2]],bbox[[3]],bbox[[4]])
  })

  observeEvent(input$regresar,{
    proxy %>% flyToBounds(bbox_qro[[1]],bbox_qro[[2]],bbox_qro[[3]],bbox_qro[[4]])
  })

  output$faltantes <- render_gt({
    req(input$filtrar)
    validate(need(input$cluster != "",message =  "Escoja un cluster"))

    hecho %>% filter(cluster == !!input$cluster) %>% select(sexo,edad,faltan) %>%
      pivot_wider(names_from = sexo,values_from = faltan) %>%
      replace_na(list(Mujer = 0, Hombre = 0)) %>% gt() %>%
      tab_header(title = "Faltantes",subtitle = glue("Cluster: {input$cluster}"))
  })

  output$por_hacer <- renderPlot({
    aux <- por_hacer %>% count(cluster, wt = por_hacer, name = "encuestas") %>%# filter(encuestas != 0) %>%
      mutate(color = if_else(encuestas > 0, "#5BC0EB", "#C3423F"))
    aux %>%
      ggplot(aes(y = forcats::fct_reorder(factor(cluster), encuestas), x = encuestas)) +
      geom_col(aes(fill = color)) +
      geom_label(aes(label = encuestas)) +
      scale_fill_identity() +
      annotate("label", x = aux %>% filter(encuestas == max(encuestas)) %>% pull(encuestas),
               y = aux %>% filter(encuestas == min(encuestas)) %>% pull(cluster) %>% factor(),
               size = 9,
               label = glue::glue("{scales::comma(sum(aux$encuestas))} entrevistas por hacer"),
               hjust = "inward", vjust = "inward") +
      theme_minimal() + ylab("cluster") + xlab("entrevistas por hacer")
  })

  output$por_hacer_cuotas <- renderPlot({

    pal_p <- leaflet::colorNumeric("Blues", domain = hecho %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
    pal_n <- leaflet::colorNumeric("Reds", domain = hecho %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort)
    uno <- pal_p(hecho %>% filter(faltan>0) %>% pull(faltan) %>% unique %>% sort)
    dos <- pal_n(hecho %>% filter(faltan<0) %>% pull(faltan) %>% unique %>% sort %>% rev)

    hecho %>% mutate(grupo = glue::glue("{edad} {sexo}")) %>%
      group_by(cluster) %>% mutate(total = sum(faltan)) %>%
      ungroup %>% mutate(cluster = reorder(cluster, total)) %>%
      ggplot(aes(y = cluster, x = grupo,
                 fill = factor(faltan))) +
      geom_tile() +
      geom_text(aes(label = faltan ), alpha = .5) +
      scale_fill_manual(values = c(dos, "white", uno)) +
      labs(fill = "Entrevistas \n por hacer", y = NULL, x = NULL) + theme_minimal()
  })

  output$eliminadas <- renderDT({
    bd %>% inner_join(eliminadas) %>% select(SbjNum, Fecha= Date, Encuestador = Srvyr, Razón) %>%
      bind_rows(
        bd %>% filter(is.na(Longitude)) %>% select(SbjNum, Fecha = Date, Encuestador = Srvyr) %>%
          mutate(Razón = "GPS apagado")
      ) %>% arrange(desc(Fecha))
  }, options = list(dom = "ltpi",
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
}

# Run the application
shinyApp(ui = ui, server = server)
