## ui.R ##
library(shinydashboard)


header <- dashboardHeader(title = "VI Solutions",
            dropdownMenu(type = "messages",
                messageItem(
                from = "Sales Dept",
                message = "Sales are steady this month."
                ),
                messageItem(
                from = "New User",
                message = "How do I register?",
                icon = icon("question"),
                time = "13:45"
                ),
                messageItem(
                from = "Support",
                message = "The new server is ready.",
                icon = icon("life-ring"),
                time = Sys.Date()#"2017-12-01"
                )
            ),
            dropdownMenu(type = "notifications",
                notificationItem(
                text = "5 new users today",
                icon("users")
                ),
                notificationItem(
                text = "12 items delivered",
                icon("truck"),
                status = "success"
                ),
                notificationItem(
                text = "Server load at 86%",
                icon = icon("exclamation-triangle"),
                status = "warning"
                )
            ),
            dropdownMenu(type = "tasks", badgeStatus = "success",
                taskItem(value = 90, color = "green",
                "Documentation"
                ),
                taskItem(value = 17, color = "aqua",
                "Project X"
                ),
                taskItem(value = 75, color = "yellow",
                "Server deployment"
                ),
                taskItem(value = 80, color = "red",
                "Overall project"
                )
            )
        )


sidebar <- dashboardSidebar(
            sidebarMenu(
                menuItem("Sales", tabName = "sales", icon = icon("bar-chart-o")
                ),
                menuItem("Geolocalize", tabName = "geoloc",
                    icon = icon("location-arrow")
                ),
                menuItem("Stocks", tabName="stock", icon = icon("line-chart")),
                menuItem("Stocks", tabName="stock", icon = icon("line-chart")),
                menuItem("Source Code", icon = icon("file-code-o"),
                    href = "https://github.com/arcnewuss/demo_dashboard/blob/master/server.r"
                )
                    
            )
        )


body <- dashboardBody(
            tabItems(
                # Geoloc
                tabItem(tabName="geoloc",
                    fluidRow(
                        box(
                            tags$style(HTML(".progress-text { height: 48px; }")),
                            textInput("region", label = h3("Region"), 
                                value = "Shanghai"),
                            sliderInput("zoom", "Zoom:",
                                        min = 3, max = 13,  value = 12),
                            actionButton("show","Show Map", width = 200)
                        ),
                        box( plotOutput("myImage") )
                    )
                ),
                # Sales
                tabItem(tabName="sales"
                ),
                
                # Stocks
                tabItem(tabName="stock",
                    fluidRow(
                        box(
                            title="Sandhu's analytics",
                            sliderInput( inputId = "days", 
                                label="Lag length", 
                                value = 00, min = 1, max = 200
                            )
                        ),
                        box(
                            dateInput(inputId= "startdate", 
                                label="Enter the Start date", value = "2016-09-01", min = 2016-09-01, max = NULL,
                                    format = "yyyy-mm-dd", 
                                        startview = "month", weekstart = 0,
                                    language = "en", width = NULL
                            ),
                            dateInput(inputId= "enddate", 
                                label="Enter the End Date (A minimum of 30 days)", 
                                value = NULL, min = NULL, max = NULL,
                                    format = "yyyy-mm-dd",
                                         startview = "month", weekstart = 0,
                                    language = "en", width = NULL
                            )
                        ),
                        box(
                            selectInput(inputId = "stock", 
                                label= "Select Stock",
                                    choices = c( "Accenture","Alibaba","Amazon","Apple", "At&T", 
                                                "Bank of America", "Coca-Cola", "Exxon Mobil",
                                                "Ford Motor", "General Electric","Google",
                                                "JP Morgan Chase", "Microsoft", "Morgan Stanley",
                                                "Pfizer", "Tesla",  "Top Hundred Stocks",  "Twitter", 
                                                "Verizon", "Walt Disney"), 
                                    selected="Top Hundred Stocks"
                            ),
                            selectInput(inputId = "ana", 
                                label= "Select Analysis",
                                choices = c("Moving Average", 
                                    "Moving Average Convergence Divergence (MACD)",
                                    "Rate of Change",
                                    "Price Envelop and Relative Strength Index"), 
                                selected="Moving Average"
                            )
                        )
                    ),
                    fluidRow(
                            plotOutput(outputId = "graph"),
                            tags$h4("For any feedback or questions please email Sandhu at: prabhjot.viu@gmail.com")
                    )
                    
                )
            )
        )


shinyUI(
    dashboardPage(skin="yellow",
        header,
        sidebar,
        body
    )
)# Closing ShinyUI
