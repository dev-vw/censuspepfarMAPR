# app.R
library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("Visualize and Compare Census and Pepfar Shapefiles"),

  sidebarLayout(
    sidebarPanel(
      h4("Pepfar Map"),
      selectInput("pepfar_country", "Select Pepfar Country:", choices = sort(c(names(cou_shp_lst), names(rou_shp_lst)))),
      uiOutput("padm_level_ui"),
      #uiOutput("plabel_col_ui"),
      uiOutput("ppoly_select_ui"),

      conditionalPanel(
        condition = "input.show_facilities == true && input.ppolygon_select != ''",
        h5("Highlight PEPFAR Facilities within 100km of PEPFAR polygon border"),
        shinyWidgets::materialSwitch(
          inputId = "phighlight_100km",
          label = "Show",
          status = "danger",
          value = FALSE),
        tags$hr(),
      ),

      h4("Census Map"),
      selectInput("census_country", "Select Census Country:", choices = sort(names(census_shp_lst))),
      uiOutput("cadm_level_ui"),
      #uiOutput("clabel_col_ui"),
      uiOutput("cpoly_select_ui"),

      conditionalPanel(
        condition = "input.show_facilities == true && input.cpolygon_select != ''",
        h5("Highlight PEPFAR Facilities within 100km of Census polygon border"),
        shinyWidgets::materialSwitch(
          inputId = "chighlight_100km",
          label = "Show",
          status = "danger",
          value = FALSE),
        tags$hr(),
      ),

      h4("Show all PEPFAR facilities in selected polygon"),
      shinyWidgets::materialSwitch(
        inputId = "show_facilities",
        label = "Show",
        status = "danger",
        value = FALSE
      )



    ),

    mainPanel(
      fluidRow(
        column(6, leafletOutput("pmap", height = "700px")),
        column(6, leafletOutput("cmap", height = "700px"))
      )
    )
  )
)

server <- function(input, output, session) {


# helper functions --------------------------------------------------------

  cou_or_rou <- function(country, adm_level) {
    if (country %in% names(cou_shp_lst)) {
      return(cou_shp_lst[[country]][[adm_level]])
    } else if (country %in% names(rou_shp_lst)) {
      return(rou_shp_lst[[country]][[adm_level]])
    } else {
      return(NULL)
    }
  }

# reactive objects --------------------------------------------------------

  pshp <- reactive({
    req(input$pepfar_country, input$padm_level)

    cou_or_rou(input$pepfar_country, input$padm_level)
  })

  cshp <- reactive({
    req(input$census_country, input$cadm_level)
    census_shp_lst[[input$census_country]][[input$cadm_level]]
  })

  ppoly <- reactive({
    req(pshp(), input$ppolygon_select)
    pshp()[pshp()[["name"]] == input$ppolygon_select, ]
  })

  cpoly <- reactive({
    req(cshp(), input$cpolygon_select)
    cshp()[cshp()[["AREA_NAME"]] == input$cpolygon_select, ]
  })

  ppoly_active <- reactive({
    !is.null(input$ppolygon_select) && !is.null(ppoly())
  })

  cpoly_active <- reactive({
    !is.null(input$cpolygon_select) && !is.null(cpoly())
  })


# reactive UIs ------------------------------------------------------------

  # adm level choices per country
  output$padm_level_ui <- renderUI({
    req(input$pepfar_country)

    shp <- if (input$pepfar_country %in% names(cou_shp_lst)) cou_shp_lst[[input$pepfar_country]] else rou_shp_lst[[input$pepfar_country]]

    selectInput("padm_level", "Select Admin Level (Pepfar):",
                choices = names(shp))
  })

  output$cadm_level_ui <- renderUI({
    req(input$census_country)

    shp <- census_shp_lst[[input$census_country]]

    selectInput("cadm_level", "Select Admin Level (Census):",
                choices = names(shp))
  })

  # polygon search selection
  output$ppoly_select_ui <- renderUI({
    req(pshp())
    selectizeInput("ppolygon_select", "Search Polygon (Pepfar):",
                   choices = c("", sort(pshp()[["name"]])),
                   selected = "",
                   options = list(placeholder = 'Select a polygon'))
  })

  output$cpoly_select_ui <- renderUI({
    req(cshp())
    selectizeInput("cpolygon_select", "Search Polygon (Census):",
                   choices = c("", sort(cshp()[["AREA_NAME"]])),
                   selected = "",
                   options = list(placeholder = 'Select a polygon'))
  })


# render leaflet maps -----------------------------------------------------

  # base maps
  output$pmap <- renderLeaflet({
    req(pshp())

    leaflet(pshp()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "darkred", fillOpacity = 1,
        weight = 1, color = "black",
        label = ~as.character(pshp()[["name"]])
      )
  })

  output$cmap <- renderLeaflet({
    req(cshp())

    leaflet(cshp()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "steelblue", fillOpacity = 1,
        weight = 1, color = "black",
        label = ~as.character(cshp()[["AREA_NAME"]])
      )
  })

  # polygon highlighting
  observeEvent(input$ppolygon_select, {
    req(ppoly())
    #poly <- pshp()[pshp()[["name"]] == input$ppolygon_select, ]
    bbox <- st_bbox(ppoly(), crs = st_crs(4326))
    #pts <- st_filter(facilities, poly)

    print("PEPFAR")
    print(bbox)

    if (nrow(ppoly()) > 0) {
      leafletProxy("pmap") %>%
        clearGroup("p_highlight") %>%
        clearGroup("cpoints") %>%
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
        addPolygons(
          data = ppoly(),
          fillOpacity = 1, fillColor = "#d10000",
          color = "black", weight = 2,
          group = "p_highlight",
          label = input$ppolygon_select
        )
    }
  })

  observeEvent(input$cpolygon_select, {
    req(cpoly())
    #poly <- cshp()[cshp()[["AREA_NAME"]] == input$cpolygon_select, ]
    bbox <- st_bbox(cpoly(), crs = st_crs(4326))

    print("Census")
    print(input$cpolygon_select)

    if (nrow(cpoly()) > 0) {
      leafletProxy("cmap") %>%
        clearGroup("c_highlight") %>%
        clearGroup("cpoints") %>%
        fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
        addPolygons(
          data = cpoly(),
          fillOpacity = 1, fillColor = "#29b1d1",
          color = "black", weight = 2,
          group = "c_highlight",
          label = input$cpolygon_select
        )
    }
  })

  # switching facilities on and off
  observe({
    if (input$show_facilities & ppoly_active()) {
      print("pepfar points")

      ppts <- st_filter(facilities, ppoly())

      leafletProxy("pmap") %>%
        clearGroup("ppoints") %>%
        addCircleMarkers(
          data = ppts,
          radius = 1,
          color = "#ffe6e6",
          #fillColor = "#e74c3c",
          opacity = 1,
          #stroke = TRUE,
          #weight = 0,
          group = "ppoints"
        )
    } else {
      leafletProxy("pmap") %>% clearGroup("ppoints")
    }

    if (input$show_facilities & cpoly_active()) {
      print("census points")
      cpts <- st_filter(facilities, cpoly())

      leafletProxy("cmap") %>%
        clearGroup("cpoints") %>%
        addCircleMarkers(
          data = cpts,
          radius = 1,
          color = "#edf3f8",
          #fillColor = "#e74c3c",
          opacity = 1,
          #stroke = TRUE,
          #weight = 0,
          group = "cpoints"
        )
    } else {
      leafletProxy("cmap") %>% clearGroup("cpoints")
    }
  })

}

shinyApp(ui, server)
