function(input, output, session) {

  output$topicvar <- renderUI({
     radioButtons("topicvar_sel", label = NULL,
                  choices = switch(input$topic,
                                   it = c(#"No Computer" = "it_nocomputer",
                                          "No Internet" = "it_nointernet",
                                          "No Broadband" = "it_nobroadband",
                                          "Has Broadband" = "it_broadband"),
                                   ed = c("Less than HS" = "ed_lths",
                                          "HS or more" = "ed_hsplus",
                                          "BA or more" = "ed_baplus"),
                                   fs = c("Two Married Parents" = "fs_marriedpar",
                                          "Single Parent" = "fs_singlepar"
                                          #"Other HH Type" = "fs_nonfamily"
                                          ),
                                   hi = c("Has Insurance" = "hi_yesinsure",
                                          "Not Insured" = "hi_notinsure"),
                                   hh = c("Owner Occupied" = "hh_owner",
                                          "Renter Occupied" = "hh_renter",
                                          "Vacant" = "hh_vacant"),
                                   lf = c("Unemployed" = "lf_unemployed",
                                          "Not in Labor Force" = "lf_nilf",
                                          "Disconnected" = "lf_disconnected"),
                                   la = c("Any language (not English)" = "la_anynoteng"
                                          # "Any Spanish" = "la_anyspanish",
                                          # "Any other Indo-european" = "la_anyindoeuro",
                                          # "Any Asian/Pacific" = "la_anyasianpac",
                                          # "Any other language" = "la_anyother",
                                          # "Speak English 'not well'" = "la_notwellnoteng",
                                          # "Speak English 'not well': Spanish" = "la_notwellspanish",
                                          # "Speak English 'not well': Other Indo-European" = "la_notwellindoeuro",
                                          # "Speak English 'not well': Asian/Pacific" = "la_notwellasianpac",
                                          # "Speak English 'not well': Other language" = "la_notwellother"
                                          ),
                                   pb = c("Born in same state" = "pb_samestate",
                                          "Foriegn born" = "pb_foreignborn"),
                                   rm = c("Lived in same house 1 year ago" = "rm_samehouse",
                                          "Lived in different county 1 year ago" = "rm_outcounty",
                                          "Lived in different state 1 year ago" = "rm_outstate"
                                          # "Lived outside the USA 1 year ago" = "rm_outusa"
                                          ),
                                   dr = c("Average Commuting Time" = "dr_avgcommute",
                                          "Commute at least 45 minutes" = "dr_45minplus",
                                          "Commute at least 60 minutes" = "dr_60minplus")
                                   ))
  })

  distMap <- reactive({
    if (input$distType) {
      dplyr::filter(contextualDataTool::dist_maps, DISTRICT_TYPE != "Secondary")
    } else {
      dplyr::filter(contextualDataTool::dist_maps, DISTRICT_TYPE != "Elementary")
    }
  })

  cenMap <- reactive({
    if (input$topic %in% c("ed", "fs", "rm", "pb", "hi")) {
      contextualDataTool::wiTract
    } else {
      contextualDataTool::wiBlkGrp
    }
  })

  output$distList <- renderUI({
    selectizeInput("dist", label = "Zoom to District", multiple = FALSE,
                   choices = as.character(distMap()$Display)[order(as.character(distMap()$Display))])
  })

  baseDistMap <- reactive({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = distMap(), weight = 1, color = "gray", fill = FALSE)
  })

  myColors <- reactive({
    myPal(cenMap()[, input$topicvar_sel, drop=TRUE])(cenMap()[, input$topicvar_sel, drop=TRUE])
  })

  stateMap <- eventReactive(input$makemap, {
    baseDistMap() %>%
      addPolygons(data = cenMap(), stroke = FALSE, fillOpacity = .5,
                  fillColor = myColors())
  })

  focaldist <- reactive({
    #THIS IS NO GOOD  FOR EXAMPLE IT PULLS BELOIT AND BELOIT TURNER
    dplyr::filter(distMap(), Display == input$dist)
  })

  #MAP
  custom_bbox <- reactive({
    expand_bbox(sf::st_bbox(focaldist()), 1)
  })

  output$dispMap <- renderLeaflet({
    if (is.null(stateMap())) return()
    stateMap()
  })

  output$custom_legend <- renderPlot(height = 100, {
    if (is.null(stateMap())) return()
    myLeg(cenMap()[, input$topicvar_sel, drop=TRUE])
  })

  observeEvent(input$zoomin, {
    bounds <- unlist(custom_bbox())
    names(bounds) <- NULL

    leafletProxy("dispMap") %>%
      addPolygons(data = focaldist(), weight = 2, color = "black", fill = FALSE, layerId = "focalDist") %>%
      fitBounds(lng1 = bounds[1], lat1 = bounds[3],
                lng2 = bounds[2], lat2 = bounds[4])

    # v$filename <- input$dist
    #
    # v$map <- stateMap() %>%
    #   addPolygons(data = focaldist(), weight = 2, color = "black", fill = FALSE, layerId = "focalDist") %>%
    #   flyToBounds(lng1 = bounds[1], lat1 = bounds[3],
    #               lng2 = bounds[2], lat2 = bounds[4])
  })

  observeEvent(input$zoomout, {
    leafletProxy("dispMap") %>%
      removeShape("focalDist") %>%
      setView(-89.8, 44.83, zoom = 7)
#
#     v$filename <- "Statewide"
#     v$map <- stateMap()
  })

  # #This will not save the final version of the map ... check answer below for inspiration
  # #https://stackoverflow.com/questions/35384258/save-leaflet-map-in-shiny
  # #Doesn't really work yet
  # captureMap4Download <- reactive({
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #
  #   v$map %>% setView(lng = (lngRng[1]+lngRng[2])/2,
  #                     lat = (latRng[1]+latRng[2])/2,
  #                     zoom = input$map_zoom)
  # })
  #
  # output$downloadMap <- downloadHandler(
  #   filename = function() {
  #     glue::glue(trimws(v$filename), "_", input$topicvar_sel, '.png')
  #   },
  #   content = function(file) {
  #     mapview::mapshot(captureMap4Download(), file = file,
  #                      remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"))
  #   }
  # )

  #District/School Info
  output$distScore <- renderText(
    contextualDataTool::dist_reportcard %>%
      filter(`District Name` == input$dist) %>%
      mutate(
        out = paste(`Overall Accountability Rating`, " (",
                    round(as.numeric(`Overall Accountability Score`), 1),
                    ")", sep = "")
      ) %>%
      pull(out)
  )

  #stop App on browser close
  session$onSessionEnded(stopApp)
}
