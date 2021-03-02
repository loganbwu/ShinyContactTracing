library(shiny)
library(DT)
library(visNetwork)
library(leaflet)
library(plotly)
source("functions.R")

navbarPage(
    title = "ContactTracer",
    inverse = T,
    position = "fixed-top",
    tabPanel(
        "Contacts",
        sidebarLayout(
            sidebarPanel(
                style = "position:fixed; width:33%; margin-top:64px; height:calc(100vh - 80px); overflow-y: auto;",
                p("Data is generated from a random name generator, random Melbourne addresses, and a simple infection model. This has no connection with real events or the Victorian DHHS."),
                h3("Profile"),
                textInput("PID", "PID", 1),
                tableOutput("table_pid"),
                h3("Infectious periods"),
                plotlyOutput("timeline")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                style = "margin-top:64px; padding-left:32px",
                fluidRow(
                    column(
                        6,
                        sliderInput("graph_order", "Degrees of separation", 1, min=1, max=10, step=1, ticks=F),
                        visNetworkOutput("subgraph_vis")
                    ),
                    column(
                        6,
                        leafletOutput("map", height="485px")
                    )
                ),
                tabsetPanel(
                    type = "tabs",
                    tabPanel("Potential upstream", br(), DTOutput("table_upstream")),
                    tabPanel("Potential downstream", br(),  DTOutput("table_downstream"))
                )
            )
        )
    ),
    tabPanel.about
)

