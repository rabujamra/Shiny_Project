library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(leaflet)
library(googleVis)
library(maps)
library(geojsonio)
library(RColorBrewer)
library(stats)
library(shinyWidgets)
library(gdata)

# loading dfs
df_Counties = read.csv("Data/Counties.csv", stringsAsFactors = F)
st = geojson_read('Data/NC_County.json', what = "sp")

#sort array based on NC json file (for map)
df_Counties$County <- reorder.factor(df_Counties$County, new.order=st$NAME)
df_Counties <- df_Counties %>%
  arrange(County)

# Assigning variables
geo_choices = list(
        "Population" = names(df_Counties)[[37]],
        "Diabetes Prevalence" = names(df_Counties)[[12]],
        #"Physical Acitivity" = names(df_Counties)[[36]],
        "Adult Obesity" = names(df_Counties)[[3]],
        "Physicians" = names(df_Counties)[[18]],
        "Uninsured Adults" = names(df_Counties)[[46]],
        #"Metropolitan" = names(df_Counties)[[31]],
        "Tier" = names(df_Counties)[[41]]
)

xcol_choices = list(
  "Population" = names(df_Counties)[[37]],
  #"Physical Acitivity" = names(df_Counties)[[36]],
  "Adult Obesity" = names(df_Counties)[[3]],
  "Physicians" = names(df_Counties)[[18]],
  "Uninsured Adults" = names(df_Counties)[[46]],
  #"Metropolitan" = names(df_Counties)[[31]],
  "Tier" = names(df_Counties)[[41]]
)

intro_str = "Hello....."

intrdata_str = "Hello1...."


ui <- fluidPage(theme = "style.css",
                shinyUI(
                        dashboardPage(
                                skin = "blue",
                                dashboardHeader(title = "NC County: Diabetes Prevalence", titleWidth = 350),
                                #dashboardHeader(title = "NC Diabetes Prevalence"),
                                dashboardSidebar(sidebarMenu(
                                        menuItem("Introduction",tabName = "intr",icon = icon("align-justify")),
                                        menuItem("Geographic", tabName = "geo", icon = icon("map")),
                                        menuItem("Correlations", tabName = "cor", icon = icon("line-chart")),
                                        menuItem("Categories",tabName = "cat",icon = icon("dashboard"))
                          
                                )),
                                dashboardBody(
                                        tags$style(type = "text/css", "#geo {height: calc(100vh - 80px) !important;}"),
                                        tabItems(
                                          tabItem(tabName = "intr",
                                                  fluidRow(column(
                                                    width = 12,
                                                    box(
                                                      title = "Introduction",
                                                      solidHeader = T,
                                                      width = NULL,
                                                      status = "info",
                                                      id = "intro",
                                                      tags$h1("About This Project"),
                                                      tags$h3(
                                                        "Half the money I spend on advertising is wasted; the trouble is, I dont't know which half' -- John Wanamaker(1838-1922)"
                                                      ),
                                                      tags$h4(intro_str),
                                                      tags$h2("The Dataset"),
                                                      tags$h4(intrdata_str),
                                                      tags$img(
                                                        #src = "HRhd2Y0.png",
                                                        width = 1100,
                                                        height = 660
                                                      )
                                                    )
                                                    
                                                  ))),
                                                tabItem(tabName = "geo",
                                                        fluidRow(
                                                                column(
                                                                        width = 9,
                                                                        box(
                                                                                title = "Map",
                                                                                solidHeader = T,
                                                                                status = "info",
                                                                                leafletOutput("geo", height = 800),
                                                                                width = NULL,
                                                                                height = "auto"
                                                                        )
                                                                ),
                                                                column(
                                                                        width = 3,
                                                                        box(
                                                                                title = "Select to Plot",
                                                                                solidHeader = T,
                                                                                width = NULL,
                                                                                status = "info",
                                                                                selectizeInput("geoin", label = NULL, geo_choices)
                                                                        )
                                                                )
                                                        )),
                                                tabItem(tabName = "cor",
                                                         fluidRow(column(7,
                                                                         selectizeInput(inputId = 'xcol',
                                                                                        label = "X Variable",
                                                                                        choices = xcol_choices,
                                                                                        selected = xcol_choices[[1]]),
                                                         #),
                                                         # column(width=7,
                                                                         box(
                                                                          title = "Data",
                                                                          solidHeader = T,
                                                                          collapsible = T,
                                                                          width = NULL,
                                                                          status = "info",
                                                                          htmlOutput("scatter"),
                                                                          tags$h4(textOutput("cor"))
                                                                        )
                                                                        ),
                                                                 column(width=5,
                                                                        # fluidRow(column(6,
                                                                        #                 selectizeInput(inputId = 'xcol',
                                                                        #                                label = "X Variable",
                                                                        #                                choices = xcol_choices,
                                                                        #                                selected = xcol_choices[[1]])
                                                                        # ),
                                                                        # column(6,
                                                                          pickerInput(inputId = "Metro",
                                                                                    label = "Metro",
                                                                                    choices = c("Metro", "Nonmetro"),
                                                                                    selected = c("Metro", "Nonmetro"),
                                                                                    options = list(`actions-box` = TRUE),
                                                                                    multiple = TRUE),
                                                                          pickerInput(inputId = "Tier",
                                                                                    label = "Tier",
                                                                                    choices = c(1, 2, 3),
                                                                                    selected = c(1, 2, 3),
                                                                                    options = list(`actions-box` = TRUE),
                                                                                    multiple = TRUE),
                                                                          
                                                                        #   )
                                                                        # ),
                                                                        box(
                                                                          title = "Data",
                                                                          solidHeader = T,
                                                                          collapsible = T,
                                                                          width = NULL,
                                                                          status = "info",
                                                                          DT::dataTableOutput({
                                                                            "dattable"})
                                                                          )
                                                                        )
                                                        )
                                                        ),
                                          tabItem(tabName = "cat",
                                                  fluidRow(column(6,
                                                                  dropdownButton(
                                                                    tags$h3("List of Input"),
                                                                    selectInput(
                                                                      inputId = 'Cat',
                                                                      label = 'Category',
                                                                      choices = c(Metro=names(df_Counties)[[31]], Tier=names(df_Counties)[[53]]),
                                                                      selected = names(df_Counties)[[31]]
                                                                    ),
                                                                    circle = TRUE,
                                                                    status = "info",
                                                                    icon = icon("gear"),
                                                                    width = "250px",
                                                                    tooltip = tooltipOptions(title = "Choose BoxPlot Criteria")
                                                                    )
                                                                  ),
                                                           column(6,
                                                                  pickerInput(inputId = "Metro1",
                                                                                label = "Metro",
                                                                                choices = c("Metro", "Nonmetro"),
                                                                                selected = c("Metro", "Nonmetro"),
                                                                                options = list(`actions-box` = TRUE),
                                                                                multiple = TRUE),
                                                                    pickerInput(inputId = "Tier1",
                                                                                label = "Tier",
                                                                                choices = c(1, 2, 3),
                                                                                selected = c(1, 2, 3),
                                                                                options = list(`actions-box` = TRUE),
                                                                                multiple = TRUE)
                                                                    )
                                                           ),
                                                           fluidRow(column(10,
                                                             box(
                                                             title = "Data",
                                                             solidHeader = T,
                                                             collapsible = T,
                                                             width = NULL,
                                                             status = "info",
                                                             plotOutput("BoxPlot")
                                                           )))
                                                  )
                )))))

server <- function(input, output, session) {
        bins = reactiveValues()
        labtxt = reactiveValues()
        
        
        # Reactive Data For scatter Plot
        geo_df_scat = reactive({
                req(input$xcol, names(df_Counties)[[12]]) 
                
                df_Counties %>%
                  select(input$xcol, names(df_Counties)[[12]]) %>%
                  filter(df_Counties$Metropolitan.or.Nonmetropolitan %in% input$Metro &
                           df_Counties$Tier %in% input$Tier)
                
        })
        
        # Reactive Data For Correlation computing
        geo_corx = reactive({
                req(input$xcol)
                
          df_Counties %>%
            filter(df_Counties$Metropolitan.or.Nonmetropolitan %in% input$Metro &
                     df_Counties$Tier %in% input$Tier) %>%
                        select(input$xcol) 
        })
   
        # Reactive Data For Data Table
        datatable = reactive({
          req(input$xcol, names(df_Counties)[[12]]) 
          
          df_Counties %>%
            select(County, input$xcol, names(df_Counties)[[12]]) %>%
            filter(df_Counties$Metropolitan.or.Nonmetropolitan %in% input$Metro &
                     df_Counties$Tier %in% input$Tier)
          
        })
        
        # Reactive Data For Boxplot
        boxplot_tbl = reactive({
          req(input$Cat)
          df_Counties %>%
            select(Cat=input$Cat,Diab=Diabetes.Prevalence) %>%
            filter(df_Counties$Metropolitan.or.Nonmetropolitan %in% input$Metro1 &
                     df_Counties$Tier %in% input$Tier1) #%>%
            #filter('Metropolitan.or.Nonmetropolitan' %in% c('Metro', 'Nonmetro'))
        })
        
        # Switching labels for map
        observe({ 
                if (input$geoin == "Population") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Population:</strong> %s"
                        bins$y = 6
                } else if (input$geoin == "Diabetes.Prevalence") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Diabetes Prevalence:</strong> %s"
                        bins$y = 5
                } else if (input$geoin == "Adult.Obesity") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Score:</strong> %s"
                        bins$y = 4
                } else if (input$geoin == "Health Care Workforce - Primary Care Physicians") {
                        labtxt$x = "<strong>%s</strong><br/><strong>Physician Access:</strong> %s"
                        bins$y = 5
                } else if (input$geoin %in% c("Uninsure.Adults")) {
                        labtxt$x = "<strong>%s</strong><br/><strong>Uninsured Adults:</strong> %s"
                        bins$y = 5
                } else if (input$geoin %in% c("Tier")) {
                        labtxt$x = "<strong>%s</strong><br/><strong>Tiers:</strong> %s"
                        bins$y = 3
                }
                
        })
        
        # Graph for Map
        output$geo = renderLeaflet({
                pal = colorBin("Greens",
                               df_Counties[, input$geoin],
                               bins = bins$y,
                               pretty = F)
                labels = sprintf(labtxt$x,
                                 st$NAME,
                                 format(
                                    df_Counties[, input$geoin],
                                         scientific = F,
                                         big.mark = ","
                                 )
                                 ) %>% lapply(htmltools::HTML)
                
                geo = leaflet(st) %>%
                        addTiles() %>%
                        addPolygons(
                                fillColor = ~ pal(df_Counties[, input$geoin]),
                                weight = 2,
                                opacity = 1,
                                color = "white",
                                dashArray = "3",
                                fillOpacity = 0.7,
                                highlight = highlightOptions(
                                        weight = 5,
                                        color = "#666",
                                        dashArray = "",
                                        fillOpacity = 0.7,
                                        bringToFront = TRUE
                                ),
                                label = labels,
                                labelOptions = labelOptions(
                                        style = list(
                                                "font-weight" = "normal",
                                                padding = "3px 8px"
                                        ),
                                        textsize = "15px",
                                        direction = "auto"
                                )
                        )
                geo %>%
                        addLegend(
                                pal = pal,
                                values = df_Counties[, input$geoin],
                                opacity = 0.7,
                                title = NULL,
                                position = "bottomright"
                        )
        })
        
        #Geo scatter plot
        output$scatter = renderGvis({
                gvisScatterChart(
                        geo_df_scat(),
                        options = list(
                                width = "700px",
                                height = "600px",
                                legend = "none"
                        )
                )
        })
        
        # Printing correlation
        output$cor = renderText({
                paste("Correlation:", round(cor(geo_corx(), df_Counties %>%
                                                  filter(df_Counties$Metropolitan.or.Nonmetropolitan %in% input$Metro &
                                                           df_Counties$Tier %in% input$Tier) %>%
                                                  select(names(df_Counties)[[12]]))[[1]], 2), sep = " ")
        })
        
        output$table = renderTable({
          head(boxplot_tbl(), 6)
        },
        striped = T,
        spacing = 'l',
        width = '100%',
        colnames = F,
        digits = 2)
        
        output$BoxPlot <- renderPlot(
          boxplot_tbl() %>%
            ggplot(mapping = aes(x = Cat, y = Diab)) +
            geom_boxplot(aes(fill = Cat)) + ggtitle(paste("Diabetes Prevalence by ", input$Cat)) +
            xlab(paste(input$Cat)) + ylab("Diabetes Prevalence") +
            scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +
            theme(plot.title = element_text(size = 24, face = "bold"),
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold"))
        )
        
        # Categories Table
        output$dattable = DT::renderDataTable({
          datatable()
        }, rownames = F)
        
        
}

shinyApp(ui = ui, server = server)
