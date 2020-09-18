#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with S2hiny here:
#
#    http://shiny.rstudio.com/
#

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
                style = "position:fixed; width:33%; margin-top:64px; margin-right: 16px; height:90%",
                p("Data is generated from a random name generator, random Melbourne addresses, and a simple infection model. This has no connection with real events or the Victorian DHHS."),
                h3("Profile"),
                textInput("PID", "PID", 1),
                tableOutput("table_pid"),
                h3("Infectious periods"),
                plotlyOutput("timeline")
            ),
            
            # Show a plot of the generated distribution
            mainPanel(
                style = "margin-top:64px",
                fluidRow(
                    column(
                        6,
                        sliderInput("graph_order", "Graph order", 1, min=1, max=10, step=1),
                        visNetworkOutput("subgraph_vis")
                    ),
                    column(
                        6,
                        leafletOutput("map", height="505px")
                    )
                ),
                h3("Potential upstream"),
                DTOutput("table_upstream"),
                h3("Potential downstream"),
                DTOutput("table_downstream")
            )
        )
    ),
    tabPanel.about
)

