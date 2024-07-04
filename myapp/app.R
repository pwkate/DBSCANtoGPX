library(shiny)
library(shinyWidgets)
library(leaflet)
library(reactable)
library(tidyverse)
library(bslib)
library(sf)
library(httr2)
library(httr)
library(jsonlite)
library(stringr)
library(fpc)

# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)

readRenviron(".Renviron")

ui <- page_sidebar(

    titlePanel("DBSCAN Clustering for Field Programs"),

    # Sidebar with a slider input for number of bins 
    sidebar = sidebar(  
      width = 280,
        fileInput("file", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        fluidRow(
          column(width = 12,
                 selectInput("id", "Points Unique Identifier:", ""),
          ),
          column(width = 6,
                 selectInput("x", "X/Longitude:", ""),
          ),
          column(width = 6,
                 selectInput("y", "Y/Latitude:", ""),
          ),
          column(width = 12,
                 helpText("Find best UTM of your area by entering your country below")
          ),
          column(width = 9,
                 textInput("country","Country:"),
          ),
          column(width = 3,
            actionBttn(inputId = "search",icon = icon("globe",lib ="glyphicon"))
          ),
          column(width = 12,
                 selectInput("crs","CRS Code:","")
          ),
          column(width = 12,
                 uiOutput("hint",style = "color: red;")
          ),
          h5("Parameters for DBSCAN Clustering"),
          numericInput(inputId = "casenum",
                       label = "Clustering Case Numbers:",
                       value = 3),
          numericInput(inputId = "distance",
                       label="Maximum Clustering Distance: (meter)",
                       value=300),
          h5("Parameters for Buffering"),
          numericInput(inputId = "buffer_d",
                       label="Buffer Areas of Cluster Cases: (meter)",
                       value=300),
          actionBttn(inputId = "go",label = "Ready"),
        )
    ),
      # Show a plot of the generated distribution
    
    navset_card_underline(
      nav_panel("Clustering Map",
                leafletOutput("dbscanmap"),
                tableOutput("summary")
                ),
      nav_panel("GPX Configuration",
                h6("Select the displsy elements form the datasets (each 3 max) ; multiple variables will be joined with semicolon, e.g, ID: John; Cluster_ID: 3"),
                   fluidRow(
                     column(width = 4,
                            h5("GPX of Clustered Index Cases"),
                            selectInput("type","Output Options:",
                                        choices = c("ONLY clustered cases"=1,
                                                    "All cases"=2),
                                        selected = 1),
                            helpText("Name and Description will be displayed on the marker pop-up label."),
                            selectizeInput("name", "Name (Required)",choices="",multiple=T,options=list(maxItems=3)),
                            selectizeInput("desc", "Description (Recommended)",choices="",multiple=T,options=list(maxItems=3)),
                            selectizeInput("cmt", "Comment",choices="",multiple=T,options=list(maxItems=3)),
                            selectizeInput("src", "Source",choices="",multiple=T,options=list(maxItems=3)),
                            downloadButton("download", "Download")
                     ),
                     column(width = 3,
                            h5("GPX of Buffer Areas"),
                            p("The outlines of buffer areas will be dissolved as a single line object."),
                            textInput("name1", "Name", value = "Screening Zone"),
                            textInput("desc1", "Description",value = "Buffer with: 300 meters"),
                            textInput("cmt1", "Comment",placeholder = "Optional"),
                            textInput("src1", "Source",placeholder = "Optional"),
                            downloadButton("download1", "Download")
                     ),
                     column(width = 5,
                            a(href = "https://play.google.com/store/apps/details?id=com.vecturagames.android.app.gpxviewer&hl=en_US&pli=1",
                              img(src = "gpx.png", height = 576, width = 600))
                     )
                   )),
      nav_panel("Data Table",
                fluidRow(
                  column(width = 12,
                         uiOutput("dbscan"),
                         downloadButton("downloadtb", "Download"))
                  )
      )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  #############################
  data <- reactiveVal(NULL)
  summary_dt <-reactiveVal(NULL)
  buffer_re<-reactiveVal(NULL)
  dt1_re<-reactiveVal(NULL)
  dt0_re<-reactiveVal(NULL)
  db_re<-reactiveVal(NULL)
  ##############################
  
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath)
    updateSelectInput(session, "id", choices = names(df))
    updateSelectInput(session, "x", choices = names(df))
    updateSelectInput(session, "y", choices = names(df))
    data(df)
  })
  
  ############################
  crs_search <- reactiveVal(FALSE)
 
   observeEvent(input$search,{
     
     crs_search(FALSE)
     
     if (!crs_search()) {
      
      ###search CRS list
      url<-paste0("https://api.maptiler.com/coordinates/search/",
                  input$country,
                  "%20WGS%2084",
                  "%20kind:CRS",
                  ".json?key=",
                  Sys.getenv("API_KEY"),
                  "&limit=50")
      req <- httr::GET(url)
      
      response <- httr::content(req, "text", encoding = "UTF-8")
      crs_list <- jsonlite::fromJSON(response)$results
      ###search UTM ZONE with data center
      dt<-data()
      dt[, input$x] <- as.numeric(dt[, input$x])
      dt[, input$y] <- as.numeric(dt[, input$y])
      meanx <- mean(dt[, input$x], na.rm = TRUE)
      meany <- mean(dt[, input$y], na.rm = TRUE)

      url<-paste0("https://www.latlong.net/c/?lat=",
                  as.character(meany),
                  "&long=",
                  as.character(meanx))
      req <- httr::GET(url)
      
      response <- httr::content(req, "text", encoding = "UTF-8")

      html <- as.character(response)

      pattern <- "<td>([0-9,.]+)</td>\\s*<td>([0-9,.]+)</td>\\s*<td>([0-9A-Za-z]+)</td>"
      
      matches <- regmatches(html, gregexpr(pattern, html, perl = TRUE))
      text<-matches[[1]]
      #print(text)
      
      result <- str_extract(text, "\\d+(\\.\\d+)?[A-Za-zA-Za-z]")
  
      zone0 <- str_extract(result, "\\d+")
      
      if (any(dt[, input$y] < 0, na.rm = TRUE)){sphere<-'S'}else{sphere<-'N'}
      
      zone<-paste0(zone0,sphere)
      
      results <- str_extract_all(crs_list$name, paste0("UTM zone\\s*", zone))
      
      results <- lapply(results, function(x) ifelse(length(x) == 0, "0", x))
      
      indices <- which(unlist(sapply(results, function(x) x != "0")))
      
      if (length(indices)==0){
        updateSelectInput(session, "crs", choices = c("4326","UTM NOT FOUND"), selected = "UTM NOT FOUND")
      }else{
        updateSelectInput(session, "crs", choices = crs_list$id[,"code"], selected = crs_list$id[indices,"code"])
      }
      
      output$hint <- renderUI({
        if (input$crs=="UTM NOT FOUND") {
          div(style = "color: red;","UTM NOT FOUND: entered country mismatched with data coordinates. Try Again:)")
        }
      })

      crs_search(TRUE)}
  })
  
  ##############################################
  
  
  #############################
  output$dbscan <- renderUI({
    if (is.null(data())) {
      tagList(
        h3("Waiting for data..."),
        p("Data will be displayed here once data upload is complete."),
        br()
      )
    } else {
      reactable(data(),
                showPageInfo = TRUE,
                defaultPageSize = 20,
                filterable = TRUE,
                compact = TRUE,
                searchable = TRUE)
    }
  })
  
  ############################
  analysis <- reactiveVal(FALSE)
  
  observeEvent(input$go,{
    
    analysis(FALSE)
    
    if (!analysis()) {
    
    dtsf<-st_as_sf(data(),coords = c(input$x,input$y),crs=4326)
    
    
    print(input$crs)
    
    t<-as.integer(input$crs)

    dtsf_t<-st_transform(dtsf,crs = t)

    radius<-input$distance
    pts<-input$casenum
    
    set.seed(123)
    db <- fpc::dbscan(st_coordinates(dtsf_t), eps = radius, MinPts = pts)
    names(db)
    
    #set.seed(123)
    
    #0.000833 degree as 100 meter
    #radius<-input$distance/100*0.000833
    #pts<-input$casenum
    
    #db <- fpc::dbscan(dt[,c("X","Y")], eps = radius, MinPts = pts)
    
    #result <- qgis_run_algorithm(
    #  "native:dbscanclustering",
    #  INPUT = dtsf,
    #  MIN_SIZE = pts,
    #  EPS = radius, 
    #  `DBSCAN*` = 0,
    #  FIELD_NAME = "CLUSTER_ID",
    #  SIZE_FIELD_NAME = "CLUSTER_SIZE",
    #  OUTPUT = qgis_tmp_vector()
    #)
    
    #db <- sf::st_as_sf(result)
    
    
    #db_re(db)
    
    dtsf$CLUSTER_ID<-db$cluster
    dtsf<-dtsf %>% 
      group_by(CLUSTER_ID) %>% 
      mutate(CLUSTER_SIZE=ifelse(CLUSTER_ID==0,NA,n())) %>% 
      ungroup()
    dtsf<-dtsf %>% 
      mutate(CLUSTER_ID:=ifelse(CLUSTER_ID==0,NA,CLUSTER_ID))
    
    db_re(dtsf)
    
    db0<-filter(dtsf,is.na(CLUSTER_ID))
    db1<-filter(dtsf,!is.na(CLUSTER_ID))
    
    rand_colors<-c("red","darkred","lightred","orange", "beige",
                   "green", "darkgreen", "lightgreen", "blue", 
                   "darkblue", "lightblue", "purple", "darkpurple",
                   "pink", "cadetblue","gray", "lightgray", "black","white")
    
    
    colors <- as.data.frame(matrix(nrow = length(unique(db1$CLUSTER_ID)), ncol = 2))
    names(colors) <- c("CLUSTER_ID", "color")
    colors$CLUSTER_ID <- sort(unique(db1$CLUSTER_ID))
    colors$color <- c(rand_colors[sample(length(rand_colors),length(unique(db1$CLUSTER_ID)),replace = T)])
    db1 <- left_join(db1, colors,by="CLUSTER_ID")
    
    icons1 <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = db1$color
    )
    
    icons0 <- awesomeIcons(
      icon = "close-circle-outline",
      iconColor = 'black',
      library = 'ion',
      markerColor = "white"
    )
    
    
    db1_c<-db1 %>% 
      group_by(CLUSTER_ID) %>% 
      summarise(geom = st_union(geometry))
    
    buf_dist=input$buffer_d
    
    
    buffer<-st_buffer(db1,input$buffer_d) %>% 
      st_union()
    
    buffer_re(buffer)

    dt1<-st_drop_geometry(db1) %>% 
      bind_cols(st_coordinates(db1))
    dt1_re(dt1)
    
    dt0<-st_drop_geometry(db0) %>% 
      bind_cols(st_coordinates(db0))
    dt0_re(dt0)
    
    dt<-data()
    dt[, input$x] <- as.numeric(dt[, input$x])
    dt[, input$y] <- as.numeric(dt[, input$y])
    meanx <- mean(dt[, input$x], na.rm = TRUE)
    meany <- mean(dt[, input$y], na.rm = TRUE)
    
    #print(meanx)
    #print(meany)
    
    output$dbscanmap <- renderLeaflet({
      
    leaflet() %>%
      setView(lng = meanx, lat = meany, zoom = 12) %>% 
      addTiles(group = "OpenStreetMap") %>% 
      addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>% 
      addAwesomeMarkers(
        data = dt1_re(), lng = ~X, lat = ~Y,
        label = ~get(input$id),
        icon = icons1,
        group = ~CLUSTER_ID
      ) %>% 
      addCircleMarkers(
        data = dt0_re(), lng = ~X, lat = ~Y,
        label = ~get(input$id), popup= ~get(input$id), color="red",stroke = F,fillOpacity = 0.5, radius = 6
      ) %>% 
      addPolygons(
        data = buffer_re(), fillOpacity = 0, weight = 2, color = "deepskyblue", opacity = 1
      ) %>% 
      addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"))
      
    })
    
    su<-data.frame(clusters=length(unique(db1$CLUSTER_ID)),
                   cases=nrow(db1),
                   noises=nrow(db0))
    names(su)<-c("Clusters","Clustered Cases","Non-Clustered Cases")
    summary_dt(su)
    
    output$summary<-renderTable({
      summary_dt()
    })
    
    update<-st_drop_geometry(dtsf) %>% 
      bind_cols(st_coordinates(dtsf))%>% 
      mutate(CLUSTER_ID:=ifelse(is.na(CLUSTER_ID),0,CLUSTER_ID))
    
    data(update)
    
    ###update Data Table with Cluster result
    updateReactable("dbscan", data = data())
    
    ###update the selection of GPX
    updateSelectizeInput(session, "name", choices = names(data()))
    updateSelectizeInput(session, "desc", choices = names(data()))
    updateSelectizeInput(session, "cmt", choices = names(data()))
    updateSelectizeInput(session, "src", choices = names(data()))
    #updateSelectizeInput(session, "name1", choices = names(data()))
    #updateSelectizeInput(session, "desc1", choices = names(data()))
    #updateSelectizeInput(session, "cmt1", choices = names(data()))
    #updateSelectizeInput(session, "src1", choices = names(data()))
  
    analysis(TRUE)}
    })
  
  ####################################################################
  
  
  output$downloadtb <- downloadHandler(
    filename = function() {
      paste("dbscan_table_", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
  create_concatenated_string  <- function(df, col1,col2,col3,col4) {
    df %>%
      rowwise() %>%
      mutate(name = paste(sapply(col1, function(col) paste(col, get(col), sep = ": ")), collapse = "; ")) %>%
      mutate(desc = paste(sapply(col2, function(col) paste(col, get(col), sep = ": ")), collapse = "; ")) %>%
      mutate(cmt = paste(sapply(col3, function(col) paste(col, get(col), sep = ": ")), collapse = "; ")) %>%
      mutate(src = paste(sapply(col4, function(col) paste(col, get(col), sep = ": ")), collapse = "; ")) %>%
      ungroup()%>% 
      select(name,desc,cmt,src)
  }
  
  all_case<-reactive({
    all<-db_re() %>% 
      mutate(CLUSTER_ID:=ifelse(is.na(CLUSTER_ID),0,CLUSTER_ID))
    
    return(create_concatenated_string(all,input$name,input$desc,input$cmt,input$src))
    })
  
  cluster_case<-reactive({
    cluster<-st_as_sf(dt1_re(),coords = c("X","Y"),crs=4326)
    
    return(create_concatenated_string(cluster,input$name,input$desc,input$cmt,input$src))
    })
  
  
  output$download<-downloadHandler(
    filename = function() {
      if (input$type == 1) {
        paste("cluster_dbscan_", Sys.Date(), ".gpx", sep = "")
      } else {
        paste("all_dbscan_", Sys.Date(), ".gpx", sep = "")
      }
    },
    content = function(file) {
      if (input$type == 1) {
        st_write(cluster_case(), driver = "GPX", file)
      } else {
        st_write(all_case(), driver = "GPX", file)
      }
    }
  )
  
  buffer<-reactive({
    buffer_re() %>% 
    st_boundary() %>% 
    st_sf() %>% 
    mutate(name=input$name1,
           desc=input$desc1,
           cmt=input$cmt1,
           src=input$src1) %>% 
    select(name,desc,cmt,src)})
  
  output$download1<-downloadHandler(
    filename = function() {
      paste("buffer_zone_", Sys.Date(),".gpx", sep = "")
    },
    content = function(file) {
      st_write(buffer(), driver = "GPX", file)
    }
  )
}

shinyApp(ui = ui, server = server)
