# app.R
library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("Visualize and Compare Census and Pepfar Shapefiles"),

  sidebarLayout(
    sidebarPanel(
      width = 3, # set width of sidebar

      h4("Pepfar Shapefile Set"),
      selectInput("pepfar_country", "Select Pepfar Country:", choices = sort(c(names(cou_shp_lst), names(rou_shp_lst)))),
      uiOutput("padm_level_ui"),
      uiOutput("ppoly_select_ui"),

      conditionalPanel(
        condition = "input.show_facilities == true && input.ppolygon_select != ''",
        sliderInput(
          inputId = "pbuffer_dist",
          label = "Buffer distance (km) - PEPFAR",
          min = 1,
          max = 100,
          value = 50,
          step = 1
        ),
        tags$hr()
      ),

      h4("Census Shapefile Set"),
      selectInput("census_country", "Select Census Country:", choices = sort(names(census_shp_lst))),
      uiOutput("cadm_level_ui"),
      uiOutput("cpoly_select_ui"),
      conditionalPanel(
        condition = "input.show_facilities == true && input.cpolygon_select != ''",
        sliderInput(
          inputId = "cbuffer_dist",
          label = "Buffer distance (km) - Census",
          min = 1,
          max = 100,
          value = 50,
          step = 1
        ),
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
        column(6,
               leafletOutput("pmap", height = "600px"),
               conditionalPanel(
                 condition = "input.show_facilities == true && input.ppolygon_select != ''",
                 absolutePanel(
                   bottom = 30,
                   left = 30,
                   style = "background:white; padding:8px 12px; border-radius:6px; box-shadow:0 1px 5px rgba(0,0,0,0.3); font-size:14px;",
                   textOutput("pinfo")
                 )
               )
        ),
        column(6,
               leafletOutput("cmap", height = "600px"),
               conditionalPanel(
                 condition = "input.show_facilities == true && input.cpolygon_select != ''",
                 absolutePanel(
                   bottom = 30,
                   left = 30,
                   style = "background:white; padding:8px 12px; border-radius:6px; box-shadow:0 1px 5px rgba(0,0,0,0.3); font-size:14px;",
                   textOutput("cinfo")
                 )
               )
        )
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

  pmax_buffer_width <- reactive({
    req(ppoly())

    # since sf_use_s2 is set to FALSE, being specific about projections is especially important
    ppoly_proj <- st_transform(ppoly(), crs = 3857)

    bbox <- st_bbox(ppoly_proj)
    min(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]) / 2
  })

  cmax_buffer_width <- reactive({
    req(cpoly())

    # since sf_use_s2 is set to FALSE, being specific about projections is especially important
    cpoly_proj <- st_transform(cpoly(), crs = 3857)

    bbox <- st_bbox(cpoly_proj)
    min(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"]) / 2
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

  # dynamically update the slider input according to the subnat polygon displayed
  observe({
    req(ppoly())
    max_dist <- round(pmax_buffer_width() / 1000) # convert to km
    updateSliderInput(session,
                      inputId = "pbuffer_dist",
                      max = max_dist,
                      value = round(max_dist / 2))
  })

  observe({
    req(cpoly())
    max_dist <- round(cmax_buffer_width() / 1000) # convert to km
    updateSliderInput(session,
                      inputId = "cbuffer_dist",
                      max = max_dist,
                      value = round(max_dist / 2))
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
    bbox <- st_bbox(ppoly(), crs = st_crs(4326))

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
    bbox <- st_bbox(cpoly(), crs = st_crs(4326))

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
    #browser()
    if (input$show_facilities && ppoly_active()) {
      print("pepfar points")
      ppts <- st_filter(facilities, ppoly())
      pbuffer <- st_difference(
        ppoly(),
        st_buffer(ppoly(), dist = -input$pbuffer_dist / 111.32))
      ppts_buffer <- st_intersection(
        pbuffer, ppts
      )

      leafletProxy("pmap") %>%
        clearGroup("ppoints") %>%
        addCircleMarkers(
          data = ppts,
          radius = 1,
          color = "#ffe6e6",
          opacity = 1,
          #stroke = TRUE,
          #weight = 0,
          group = "ppoints"
        ) %>%
        addPolygons(
          data = pbuffer,
          fillOpacity = 0.2,
          color = "black", weight = 2,
          group = "ppoints",
        ) %>%
        addCircleMarkers(
          data = ppts_buffer,
          radius = 1,
          color = "#D1D100",
          opacity = 1,
          #stroke = TRUE,
          #weight = 0,
          group = "ppoints"
        )
    } else {
      leafletProxy("pmap") %>% clearGroup("ppoints")
    }
  })

  observe({
    if (input$show_facilities && cpoly_active()) {
      print("census points")
      cpts <- st_filter(facilities, cpoly())
      cbuffer <- st_difference(
        cpoly(),
        st_buffer(cpoly(), dist = -input$cbuffer_dist / 111.32))
      cpts_buffer <- st_intersection(
        cbuffer, cpts
      )

      leafletProxy("cmap") %>%
        clearGroup("cpoints") %>%
        addCircleMarkers(
          data = cpts,
          radius = 1,
          color = "#edf3f8",
          opacity = 1,
          #stroke = TRUE,
          #weight = 0,
          group = "cpoints"
        ) %>%
        addPolygons(
          data = cbuffer,
          fillOpacity = 0.2,
          color = "black", weight = 2,
          group = "cpoints",
        ) %>%
        addCircleMarkers(
          data = cpts_buffer,
          radius = 1,
          color = "#D129B2",
          opacity = 1,
          #stroke = TRUE,
          #weight = 0,
          group = "cpoints"
        )
    } else {
      leafletProxy("cmap") %>% clearGroup("cpoints")
    }
  })

# info box ----------------------------------------------------------------

  output$pinfo <- renderText({
    req(input$show_facilities, ppoly_active())

    ppts <- st_filter(facilities, ppoly())

    total <- nrow(ppts)

    pbuffer <- st_difference(
      ppoly(),
      st_buffer(ppoly(), dist = -input$pbuffer_dist / 111.32))
    ppts_buffer <- st_intersection(
      pbuffer, ppts
    )

    within <- nrow(ppts_buffer)
    percent <- if (total > 0) round((within/total) * 100, 1) else 0

    paste0("Facilities within ", input$pbuffer_dist, "km buffer: ", within, " / ", total, " (", percent, "%)")
  })

  output$cinfo <- renderText({
    req(input$show_facilities, cpoly_active())

    cpts <- st_filter(facilities, cpoly())

    total <- nrow(cpts)

    cbuffer <- st_difference(
      cpoly(),
      st_buffer(cpoly(), dist = -input$cbuffer_dist / 111.32))
    cpts_buffer <- st_intersection(
      cbuffer, cpts
    )

    within <- nrow(cpts_buffer)
    percent <- if (total > 0) round((within/total) * 100, 1) else 0

    paste0("Facilities within ", input$cbuffer_dist, "km buffer: ", within, " / ", total, " (", percent, "%)")
  })
}

shinyApp(ui, server)
