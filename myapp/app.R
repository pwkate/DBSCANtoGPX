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
library(shinyjs)

# Specify the application port
#options(shiny.host = "0.0.0.0")
#options(shiny.port = 7860)

readRenviron(".Renviron")

ui <- page_sidebar(

    titlePanel("DBSCAN Clustering for Field Programs"),

    # Sidebar with a slider input for number of bins 
    sidebar = sidebar(  
      width = 360,
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
          h5("Parameters for Cluster Definition:"),
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "width: 100%; margin-right: 3px;",
              numericInput(inputId = "clusternum",
                           label = "Number of cases",
                           value = 3)
            ),
            div(
              style = "width: 100%; margin-right: 3px;",
              numericInput(inputId = "clusterdist",
                           label = "Within (meters)",
                           value = 300)            )
          ),
          h5("Parameters for Buffering (meter):"),
          fluidRow(
            column(12,
                   numericInput(inputId = "buffer_d",
                                label = "Buffer Areas of Cluster Cases:",
                                value = 300)
            ),
            column(12,
                   checkboxInput(inputId = "enable_buffer_d0",
                                 label = "Enable Buffer for Non-Clusters",
                                 value = TRUE)
            ),
            column(12,
                   conditionalPanel(
                     condition = "input.enable_buffer_d0 == true",
                     numericInput(inputId = "buffer_d0",
                                  label = "Buffer Areas of Non-Cluster Cases:",
                                  value = 150)
                   )
            )
          ),
          div(
            style = "display: flex; align-items: center;",
            div(
              style = "width: 100%; margin-right: 3px;",
              actionBttn(inputId = "go",label = "Run")
            ),
            div(
              style = "width: 100%; margin-right: 3px;",
              actionBttn(inputId = "cluclear",label = "Reset")
            )
          )
          
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
  
  summary_dt <-reactiveVal(NULL)###SUMMARY DATATABEL
  
  buffer_re<-reactiveVal(NULL)
 
  dt1_re<-reactiveVal(NULL)
  dt0_re<-reactiveVal(NULL)
  db_re<-reactiveVal(NULL)
  
  dbscan_l<-reactiveVal(NULL) ###OUTPUT_LEAFLET
  ##############################
  
  observeEvent(input$file, {
    df <- read.csv(input$file$datapath)
    
    x_columns <- names(df)[grepl("X|longitude", names(df), ignore.case = TRUE)]
    y_columns <- names(df)[grepl("y|latitude", names(df), ignore.case = TRUE)]
    
    updateSelectInput(session, "id", choices = names(df))
    updateSelectInput(session, "x", 
                      choices = names(df),
                      selected = x_columns)
    updateSelectInput(session, "y", choices = names(df),
                      selected = y_columns)
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
   
   reset_trigger <- reactiveVal(0)
   
   observeEvent(input$cluclear, {
       dbscan_l(NULL)
       summary_dt(NULL)
       reset_trigger(reset_trigger() + 1)
   }) 
   
  ############################
   
  ############################
  ### set DBSCANMAP
  ############################
   output$dbscanmap <- renderLeaflet({
     
     reset_trigger()
     
     if (is.null(dbscan_l()) && is.null(data())) {
       leaflet() %>%
         setView(lng = 0, lat = 0, zoom = 3) %>%
         addTiles(group = "OpenStreetMap") %>%
         addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
         addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
         addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
     } else if (is.null(dbscan_l()) && !is.null(data())) {
       req(data(), input$x, input$y)  # Validate inputs and data
       
       meanx <- mean(data()[, input$x], na.rm = TRUE)
       meany <- mean(data()[, input$y], na.rm = TRUE)
       
       leaflet() %>%
         setView(lng = meanx, lat = meany, zoom = 12) %>%
         addTiles(group = "OpenStreetMap") %>%
         addProviderTiles("Esri.WorldImagery", group = "Esri.WorldImagery") %>%
         addCircleMarkers(
           data = data(),
           lng = ~as.numeric(get(input$x)),
           lat = ~as.numeric(get(input$y)),
           label = ~get(input$id),
           color = "red", stroke = F,
           fillOpacity = 0.5, radius = 6
         ) %>%
         addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
         addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
     } else {
       req(dbscan_l())
       dbscan_l() 
     }
   })
   
   
   
     ###########################
   
   observeEvent(input$go, {
     
     req(data(), input$x, input$y, input$clusterdist, input$clusternum)
    
     #shinyjs::disable("go")
     #showNotification("Conducting Cluster Detection...", type = "message", duration = NULL, id = "dbscan_note")
     
     dtsf<-st_as_sf(data(),coords = c(input$x,input$y),crs=4326)
     
     t<-as.integer(input$crs)
     
     dtsf_t<-st_transform(dtsf,crs = t)
     

     radius<-input$clusterdist
     pts<-input$clusternum
     
     #st_coordinates(dtsf)
     set.seed(123)
     db <- fpc::dbscan(st_coordinates(dtsf_t), eps = radius, MinPts = pts)
     
     dtsf$CLUSTER_ID<-db$cluster
     
     dtsf<-dtsf %>% 
       group_by(CLUSTER_ID) %>% 
       mutate(CLUSTER_SIZE=ifelse(CLUSTER_ID==0,NA,n())) %>% 
       ungroup() #### organize original data for CLUSTER SIZE (NA -->non-cluster)
     
     dtsf<-dtsf %>% 
       mutate(CLUSTER_ID:=ifelse(CLUSTER_ID==0,NA,CLUSTER_ID)) #### organize original data for CLUSTER ID: NA -->non-cluster
     
     db_re(dtsf)
     
     db0<-filter(dtsf,is.na(CLUSTER_ID)) # CLUSTER SF DATASET
     db1<-filter(dtsf,!is.na(CLUSTER_ID)) # non-CLUSTER SF DATASET
     
     rand_colors<-c("red","darkred","lightred","orange", "beige",
                    "green", "darkgreen", "lightgreen", "blue", 
                    "darkblue", "lightblue", "purple", "darkpurple",
                    "pink", "cadetblue","gray", "lightgray", "black","white")
     
     colors <- as.data.frame(matrix(nrow = length(unique(db1$CLUSTER_ID)), ncol = 2))
     names(colors) <- c("CLUSTER_ID", "color")
     colors$CLUSTER_ID <- sort(unique(db1$CLUSTER_ID))
     colors$color <- c(rand_colors[sample(length(rand_colors),length(unique(db1$CLUSTER_ID)),replace = T)])
     
     db1 <- left_join(db1, colors,by="CLUSTER_ID") ### cluster set with color variable!
     
     icons1 <- awesomeIcons(
       icon = 'ios-close',
       iconColor = 'black',
       library = 'ion',
       markerColor = db1$color
     )
     
     clu_buf<-input$buffer_d ###BUFFER DISTANCE OF CLUSTER CASE
     non_clu<-if (input$enable_buffer_d0) {input$buffer_d0} else {0} ###BUFFER DISTANCE OF non-CLUSTER CASE
     
     clu_convex <- db1 %>%
       group_by(CLUSTER_ID) %>%
       summarize(geometry = st_union(geometry)) %>%  # Combine geometries
       mutate(geometry = st_convex_hull(geometry))  # Compute convex hull and set as geometry column
     
    
     if(non_clu>0){
       buffer1<-st_buffer(clu_convex,clu_buf) %>% select(CLUSTER_ID)
       
       buffer0<-st_buffer(db0,non_clu) %>% select(CLUSTER_ID)
       
       buffer_combined <- rbind(buffer1, buffer0)
       
       buffer_union <- st_union(buffer_combined)
       
       buffer_union <- st_collection_extract(buffer_union, "POLYGON")
       
       buffer <- st_sf(geometry = buffer_union) %>% st_make_valid()
       
       buffer_re(buffer) ### Buffer reactive value!!!
       
     }else{
       buffer1<-st_buffer(clu_convex,clu_buf)
       buffer_union <- st_union(buffer1)
       buffer_union <- st_collection_extract(buffer_union, "POLYGON")
       buffer <- st_sf(geometry = buffer_union) %>% st_make_valid()
       
       buffer_re(buffer) ### Buffer reactive value!!!
     }
     
     if(nrow(db1)>0){
       dt1<-st_drop_geometry(db1) %>% 
         bind_cols(st_coordinates(db1))
     } ### DT1: DATASET OF CLUSTER CASES
     dt1_re(dt1)
     
     dt0<-st_drop_geometry(db0) %>% 
       bind_cols(st_coordinates(db0))
     ### DT0: DATASET OF NON-CLUSTER CASES
     dt0_re(dt0)
     
     ###SUMMARY DATATABLE
     su<-data.frame(clusters=length(unique(db1$CLUSTER_ID)),
                    cases=nrow(db1),
                    noises=nrow(db0))
     names(su)<-c("Clusters","Clustered Cases","Non-Clustered Cases")
     summary_dt(su)
     #############################
     
     
     dt<-data()
     dt[, input$x] <- as.numeric(dt[, input$x])
     dt[, input$y] <- as.numeric(dt[, input$y])
     meanx <- mean(dt[, input$x], na.rm = TRUE)
     meany <- mean(dt[, input$y], na.rm = TRUE)
     
     library(htmltools)
     legendHtml <- HTML(paste(
       "<link rel='stylesheet' href='https://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css'>",
       "<div style='line-height: 1.5;'>",
       "<i class='ion ion-ios-close' style='color:black; font-size: 16px;'></i> Clustered Cases<br>",
       "<svg width='16' height='16'><circle cx='8' cy='8' r='6' fill='red' fill-opacity='0.5' /></svg> Non-clustered Cases<br>",
       "</div>"
     ))
     
     if(nrow(db1)>0){
       l<-leaflet() %>%
         setView(lng = meanx, lat = meany, zoom = 12) %>% 
         addTiles(group = "OpenStreetMap") %>% 
         addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>% 
         addAwesomeMarkers(
           data = dt1, lng = ~X, lat = ~Y,
           #label = ~get(input$id),
           label = ~Seq,
           icon = icons1,
           group = ~CLUSTER_ID
         ) %>% 
         addCircleMarkers(
           data = dt0, lng = ~X, lat = ~Y,
           #label =  ~get(input$id), 
           label = ~Seq,
           color="red",stroke = F,
           fillOpacity = 0.5, radius = 6
         ) %>% 
         addPolygons(
           data = buffer, fillOpacity = 0, weight = 2, color = "deepskyblue", opacity = 1
         ) %>% 
         addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
         addControl(html = legendHtml, position = "bottomleft") %>% 
         addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
       
     }else{
       l<-leaflet() %>%
         setView(lng = meanx, lat = meany, zoom = 12) %>% 
         addTiles(group = "OpenStreetMap") %>% 
         addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>% 
         addCircleMarkers(
           data = dt0, lng = ~X, lat = ~Y,
           #label =  ~get(input$id), 
           label = ~Seq,
           color="red",stroke = F,
           fillOpacity = 0.5, radius = 6
         ) %>% 
         addPolygons(
           data = buffer, fillOpacity = 0, weight = 2, color = "deepskyblue", opacity = 1
         ) %>% 
         addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery")) %>%
         addControl(html = legendHtml, position = "bottomleft") %>% 
         addScaleBar(position = "bottomright", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
     }
     
     dbscan_l(l)
     
     output$summary<-renderTable({
       summary_dt()
     })
     
     update<-st_drop_geometry(dtsf) %>% 
       bind_cols(st_coordinates(dtsf))%>% 
       mutate(CLUSTER_ID:=ifelse(is.na(CLUSTER_ID),0,CLUSTER_ID))
     

     data(as.data.frame(update))
     
     ###update Data Table with Cluster result
     updateReactable("dbscan", data = data())
     
     ###update the selection of GPX
     updateSelectizeInput(session, "name", choices = names(data()))
     updateSelectizeInput(session, "desc", choices = names(data()))
     updateSelectizeInput(session, "cmt", choices = names(data()))
     updateSelectizeInput(session, "src", choices = names(data()))
     
     
     #shinyjs::enable("go")
     #removeNotification("dbscan_note")
   })
  
  ####################################################################
  
  ###########
  ###GPX FILE
  ###########
  
  #observe({req(data(),input$x)
  #  print(is.null(dbscan_l()) & !is.null(data()))
  #  print(class(data()[,input$x]))
  #})
  
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
