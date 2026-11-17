

# libraries
library(shiny)
library(bslib)
library(rsconnect)
library(sf)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(htmltools)
library(tigris)
library(tidygeocoder)


options(tigris_use_cache = TRUE)


# ui creation -------------------------------------------------------------


ui <- page_navbar(
  title = "Top 12 worst evictors in Binghamton",
  
  navbar_options = navbar_options(
    position = "static-top",
    bg = NULL,
    theme = "auto",
    collapsible = TRUE,
    underline = TRUE
  ),
  
  sidebar = sidebar(
    width = 340,
    
    helpText(
      "These landlords have sued the most tenants for eviction in Binghamton since the Emergency Eviction Moratorium ended in January 2022 (as of 9/30/2024). Here we have their known housing locations."
    ),
    
    tags$style(
      HTML(
        ".landlord-card > .card-header {
           color: #fff;
           background: #8FBC8F;
         }
         div.a {
           font-size: 13px;
           font-style: italic;
           color: gray;
         }"
      )
    ),
    
    selectizeInput(
      "locselect",
      label = "Location",
      choices = NULL,
      selected = NULL
    ),
    
    selectizeInput(
      "addselect",
      label = "Search for an Address",
      choices = NULL,
      selected = "",
      options = list(placeholder = "Enter an Address")
    ),
    
    selectizeInput(
      "llordselect",
      label = "Filter by landlord",
      choices = NULL,
      selected = NULL,
      options = list(placeholder = "Enter a landlord")
    ),
    
    card(
      class = "landlord-card",
      card_header("Landlord Information"),
      card_body(htmlOutput("landlord_text"))
    )
  ),
  
  nav_panel(
    title = "Map",
    icon = icon("location-dot"),
    
    card(
      card_title("Selected Address: "),
      card_body(htmlOutput("address_text1")),
      fill = FALSE
    ),
    
    card(leafletOutput(outputId = "parcel")),
    
    div(
      style = "text-align: center; padding-top: 10px; width: 100%;",
      actionButton(
        "action",
        " Zoom to Full Map View",
        icon = icon("search"),
        style = "color: gray; background-color: #91cbef; border-color: gray;"
      )
    )
  ),
  
  nav_panel(
    title = "About Us",
    icon = icon("circle-info"),
    
    card(
      card_title("Binghamton Housing Justice: Worst Evictors Project"),
      card_body(htmlOutput("bginfo")),
      fill = FALSE
    )
  )
)


# server function ---------------------------------------------------------

server <- function(input, output, session) {
  
  # ---- DATA ----------------------------------------------------------------
  llord = read.csv("files/EDIT of Copy of Top Eviction Rates (Oct_1_24-Sep_30_25) - Sheet1.csv")
  hdat = read.csv("files/total - Sheet1.csv") %>% filter(Latitude < 43)
  
  llord$landlord = trimws(llord$landlord)
  hdat$Landlord_Group = trimws(hdat$Landlord_Group)
  
  hdat = hdat %>% distinct(actaddress, .keep_all = TRUE)
  
  updateSelectizeInput(
    session,
    'locselect',
    choices = c("Binghamton" = "Binghamton", "Broome County" = "Broome_County"),
    selected = "Binghamton",
    server = TRUE
  )
  
  updateSelectizeInput(
    session,
    'addselect',
    choices = c(" " = "", hdat$actaddress),
    selected = "",
    server = TRUE
  )
  
  updateSelectizeInput(
    session,
    'llordselect',
    choices = c("All" = "All", llord$landlord),
    selected = "All",
    server = TRUE
  )
  
  
  RV <-
    reactiveValues(llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>",
                   add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>")
  
  
  # ---- GEOGRAPHY -----------------------------------------------------------
  counties <-
    counties(cb = TRUE, year = 2023, class = "sf") %>% st_transform(4326)
  st_crs(counties) = 4326
  broome = counties %>% filter(STATE_NAME == "New York" &
                                 NAMELSAD == "Broome County")
  broome_bbox <- st_bbox(broome)
  
  
  places = places(
    state = "NY",
    cb = T,
    year = 2023,
    class = "sf"
  ) %>%
    st_transform(4326)
  st_crs(places) = 4326
  bing = places %>% filter(NAME == "Binghamton")
  bing_bbox <- st_bbox(bing)
  
  
  hdat = st_as_sf(hdat,
                  coords = c('Longitude', 'Latitude'),
                  crs = 4326)
  hdat$bing <- lengths(st_within(hdat, bing)) > 0
  
  broome_streets <- st_read("files/broome_streets/broome_streets.shp") %>%
    st_transform(4326)
  
  broome_streets <- st_filter(broome_streets, broome)
  bing_streets   <- st_intersection(broome_streets, bing)
  
  
  # ---- LANDLORD INFO  ---------------------------------------------------
  
  llord_html <- function(name) {
    row <- llord %>% filter(landlord == name)
    n_props <- sum(hdat$Landlord_Group == name)
    
    if (nrow(row) == 0) {
      return(
        paste0(
          "<b>Landlord:</b> ", name,
          "<br><br>There are ", n_props,
          " known properties associated with this landlord in Broome County."
        )
      )
    }
    
    paste0(
      "<b>Landlord:</b> ", name, "<hr>",
      "<b>Ranking: </b>", row$ranking[1], " on our Worst Evictors List<br>",
      "<b>Evictions Filed (Oct 2024 to Sep 2025): </b>", row$yearlyfilings[1], "<br>",
      "<b>Total Complaints for Landlord: </b>", row$code_violations[1], "<br><br>",
      "There are ", n_props,
      " known properties associated with this landlord in Broome County."
    )
  }
  
  
  # ---- BASE MAP ------------------------------------------------------------
  output$parcel <- renderLeaflet({
    leaflet() %>%
      addPolygons(
        data = bing,
        fillColor = "#e5e5e5",
        color = "#333333",
        weight = 4,
        opacity = 1
      ) %>%
      addPolylines(
        data = bing_streets,
        weight = 2,
        color = "#333333"
      ) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas)
  })
  
  
  # ---- ADDRESS SELECT ----------------------------------------------------
  observeEvent(input$addselect, {
    req(input$addselect)
    selection <- hdat %>% filter(actaddress == input$addselect)
    
    leafletProxy("parcel") %>% clearGroup("SearchResult") %>%
      addMarkers(
        data = selection,
        group = "SearchResult",
        icon = list(iconUrl = "files/red-loc.png",
                    iconSize = c(25, 25))
      ) %>%
      setView(
        lng = st_coordinates(selection$geometry)[1],
        lat = st_coordinates(selection$geometry)[2],
        zoom = 16
      )
    
    RV$llord_click <- llord_html(selection$Landlord_Group)
    
    if ("code_violations" %in% names(selection) &&
        length(selection$code_violations) > 0 &&
        !is.na(selection$code_violations[1]) &&
        nzchar(as.character(selection$code_violations[1]))) {
      RV$llord_click <- paste0(
        RV$llord_click,
        "<br><br><b>Complaints at this address: </b>",
        selection$code_violations[1]
      )
    }
    
    RV$add = selection$actaddress
  })
  
  
  # ---- LOCATION SELECTED (Binghamton / Broome County) ----------------------
  observeEvent(input$locselect, {
    req(input$locselect)
    
    if (input$locselect == "Binghamton") {
      disp_dat = hdat %>% filter(bing)
      
      leafletProxy("parcel") %>% clearMarkers() %>%
        addCircleMarkers(
          data = disp_dat,
          layerId = ~actaddress,
          radius = 5,
          stroke = TRUE, color = "white", weight = 1,
          fillColor = "#3388dd", fillOpacity = 0.7,
          label = ~actaddress
        ) %>%
        clearShapes() %>%
        addPolygons(
          data = bing,
          fillColor = "#e5e5e5",
          color = "#333333",
          weight = 4,
          opacity = 1
        )  %>%
        addPolylines(
          data = bing_streets,
          weight = 2,
          color = "#333333"
        )
      
      bbox = bing_bbox
      
      RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
      
      RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
      
    } else {
      disp_dat = hdat
      
      leafletProxy("parcel") %>% clearMarkers() %>%
        addCircleMarkers(
          data = disp_dat,
          layerId = ~actaddress,
          radius = 5,
          stroke = TRUE, color = "white", weight = 1,
          fillColor = "#3388dd", fillOpacity = 0.7,
          label = ~actaddress
        ) %>%
        clearShapes() %>%
        addPolygons(
          data = broome,
          fillColor = "#e5e5e5",
          color = "#333333",
          weight = 4,
          opacity = 1
        )  %>%
        addPolylines(data = broome_streets,
                     weight = 2,
                     color = "#333333")
      
      bbox = broome_bbox
      
      updateSelectizeInput(session,
                           "addselect",
                           selected = "")
      
      RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
      
      RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
      
    }
    
    
    # ---- LANDLORD SELECT -------------------------------------------------
    observeEvent(input$llordselect, {
      req(input$llordselect)
      
      if (input$llordselect == "All") {
        leafletProxy("parcel") %>% clearMarkers() %>%
          addCircleMarkers(
            data = disp_dat,
            layerId = ~actaddress,
            radius = 5,
            stroke = TRUE, color = "white", weight = 1,
            fillColor = "#3388dd", fillOpacity = 0.7,
            label = ~actaddress
          )
        
        updateSelectizeInput(
          session,
          'addselect',
          choices = c(" " = "", disp_dat$actaddress),
          selected = "",
          server = TRUE
        )
        
        RV$llord_click = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get landlord information</span>"
        
        RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
        
      } else {
        llordgroup <-
          disp_dat %>% filter(Landlord_Group == input$llordselect)
        
        leafletProxy("parcel") %>% clearMarkers() %>%
          addCircleMarkers(
            data = llordgroup,
            layerId = ~actaddress,
            radius = 5,
            stroke = TRUE, color = "white", weight = 1,
            fillColor = "#3388dd", fillOpacity = 0.7,
            label = ~actaddress
          )
        
        updateSelectizeInput(
          session,
          'addselect',
          choices = c(" " = "", llordgroup$actaddress),
          selected = "",
          server = TRUE
        )
        
        RV$llord_click <- llord_html(input$llordselect)
        
        updateSelectizeInput(session,
                             "addselect",
                             selected = "")
        
        RV$add = "<span style = 'color: gray;'>Click a marker on the map or search for an address to get housing information</span>"
        
      }
      
    })
    
    
    observeEvent(input$action, {
      leafletProxy("parcel") %>%
        flyToBounds(
          lng1 = bbox[1] %>% unname(),
          lat1 = bbox[2] %>% unname(),
          lng2 = bbox[3] %>% unname(),
          lat2 = bbox[4] %>% unname(),
          options = list(duration = 0.25)
        )
    })
    
  })
  
  
  # ---- MARKER CLICKED ------------------------------------------------------
  observeEvent(input$parcel_marker_click, {
    id <- input$parcel_marker_click$id
    req(id)
    updateSelectizeInput(session, "addselect", selected = id)
  })
  
  
  # ---- TEXT OUTPUTS --------------------------------------------------------
  output$landlord_text <- renderText({
    print(RV$llord_click)
  })
  
  output$bginfo = renderText({
    print(
      paste0(
        '<li style="display:inline-block;">
      In 2019, the NYS Housing Stability & Tenant Protections Act significantly increased the rights and protections of renter households against landlord profiteering and exploitation. While these expansions were hard fought, tenants throughout the City of Binghamton continue to face discrimination - source of income, disability status, eviction history, household composition, etc. - when attempting to rent available units. Illegal practices are also pervasive in the form of “self-help evictions” as local landlords do everything they can to avoid submitting to legal court proceedings in order to carry out more immediate displacement.
      <br><br>    
      Housing justice advocates in Binghamton have identified low enforcement rates of RPAPL §768, related to unlawful eviction attempts, by Binghamton Police and Broome County Sheriff Departments. Similarly, enforcement of the expanded protections within Landlord Tenant proceedings at City Court is often inconsistent. Local tenants cannot simply rely on the implementation of the 2019 changes when seeking to obtain or maintain rental housing. 
      <br><br>
      While New York State offers a Statewide Landlord Tenant Eviction Dashboard, the accessible data only represents aggregate eviction filings and offers neither information that can be used to assist tenants in making informed decisions regarding their residential opportunities nor resources should they be subject to potential violations by their landlord.
      <br><br>
      This dashboard is intended to fill that gap and will be updated + expanded annually to reflect landlords within the City who most frequently sue their tenants for eviction - a practice that, when serial or chronic, is commonly associated with poor property conditions and tenants rights violations.
      <br><hr>
      This project was completed in collaboration with the Fair Housing Advisory Board of Broome County, Binghamton Tenants Union, and the Digital and Data Studies program at Binghamton University. More information on local housing justice efforts may be found <a href="https://www.binghamtonhousingjustice.com/">here</a>.
      <br><br>
      The source code for this project can be found <a href="https://github.com/lyraeal/FHAB-Dashboard">here</a>.'
      )
    )
  })
  
}

# run app -----------------------------------------------------------------

shinyApp(ui = ui, server = server)

rsconnect::writeManifest()
getwd()
setwd("C:/Users/jessi/Documents/myapp")
