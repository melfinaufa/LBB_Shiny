ui <- dashboardPage(title = "www.baktisiregar.com",
                    
                    # Header
                    dashboardHeader(title = "Bencana Alam",titleWidth = 200),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                      
                      # Side menu of the Dashboard  
                      sidebarMenu(
                        
                        selectInput(
                          inputId = "input_bulan",
                          label = "Bulan",
                          choices = levels(disaster_clean$Bulan),
                          selected = "May"),
                          size = 13,
                        # selectize = FALSE),
                        
                        selectInput(
                          inputId = "input_kejadian",
                          label = "Kejadian Bencana",
                          choices = levels(disaster_clean$Kejadian),
                          selected = "Banjir",
                          selectize = FALSE),
                        
                        
                        menuItem(text = "Plot",
                                 icon = icon("chart-line"),
                                 tabName = "page1",
                                 badgeLabel = "Numeric",
                                 badgeColor = "green"),
                        
                        menuItem(text = "Graph",
                                 icon = icon("magnifying-glass-chart"),
                                 tabName = "page3",
                                 badgeLabel = "Proporsi",
                                 badgeColor = "red"),
                        
                        # actionLink("remove", icon = icon("sync-alt"),"Remove detail tabs"),
                        
                        menuItem("Data Table", tabName = "page2", icon = icon("table-list")),
                        
                        menuItem("Source Code", icon = icon("github"), href = "https://github.com/Bakti-Siregar/Flights-Dashboard"))
                        
                      ),
                      
          
                    
                    # The body of the dashboard
                    
                  dashboardBody(
                    tabItems(
                      # --- PAGE 1 (Dashboard) --- 
                      
                      
                      tabItem(tabName = "page1",
                              h2("Report Kejadian Bencana Menurut Bulan dan Jenis Kejadian Bencana di Indonesia Tahun 2023"),
                              br(),
                              
                              
                              tabsetPanel(id = "tabs",
                                          tabPanel(title = "Dashboard",
                                                   value = "page1",
                              

                                           fluidRow(valueBoxOutput("sum_meninggal"),
                                                    valueBoxOutput("sum_hilang"),
                                                    valueBoxOutput("sum_terluka"),
                                                    valueBoxOutput("sum_rusak"),
                                                    valueBoxOutput("sum_terendam"),
                                                    valueBoxOutput("sum_fasum")),
                                            
                                           fluidRow(
                                             box(width = 6, height = 480, plotlyOutput("plot_trend", height = 460)),
                                             box(width = 6, height = 480, plotlyOutput("plot_rangking", height = 460)))
                                  ))),
                      
                      tabItem(tabName = "page3",
                              h2("Report Proporsi Jumlah Kejadian Bencana Menurut Provinsi dan Bulan"),
                              br(),
                              
                              fluidRow(
                                box(width = 2,
                                    solidHeader = TRUE,
                                    status = "warning",
                                    title = "Provinsi",
                                    radioButtons(inputId = "input_prov",
                                                 label = NULL,
                                                 choiceNames = levels(disaster_clean$Provinsi),
                                                 choiceValues = levels(disaster_clean$Provinsi))),
                                
                                box(width = 5, height = 500, plotlyOutput(outputId = "prop_dis", height = 480)),
                                box(width = 5, height = 500, plotlyOutput(outputId = "prop_sum", height = 480))
                              )
                              
                      ),
                    
                      
                      # --- PAGE 3 (Data Table) ---
                      tabItem(tabName = "page2",
                              fluidRow(
                                box(width = 12, dataTableOutput(outputId = "table_data"))  
                              )
                      
                     )
                      )
)
)