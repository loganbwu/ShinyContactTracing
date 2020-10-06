library(sf)
library(tidygraph)
library(shiny)
library(leaflet)

nodes_as_sf <- function(g) {
  stopifnot(inherits(g, "tbl_graph"))
  g %>% activate(nodes) %>% as_tibble %>% st_as_sf
}

edges_as_sf <- function(g) {
  stopifnot(inherits(g, "tbl_graph"))
  g %>% activate(edges) %>% as_tibble
}

pal = function (x, colors, na.col="grey30") {
  p = colorRamp(colors)
  x.rs = scales::rescale(as.numeric(x))
  cols.mat = p(x.rs)
  cols.is.na = which(is.na(cols.mat[,1]))
  cols.mat[cols.is.na,] = 0
  cols.rgb = rgb(cols.mat[,1], cols.mat[,2], cols.mat[,3], maxColorValue=255)
  cols.rgb[cols.is.na] = na.col
  cols.rgb
}

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

# viral shedding as a function of infection day
infectivity = approxfun(c(0, 1, 3, 4, 5, 7, 12, 14), c(0, 0.1, 0.8, 1, 1, 0.7, 0.1, 0))

colorIcons <- iconList(
  blue = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
                  "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-blue.png",
                  shadowUrl="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-shadow.png"),
  red = makeIcon("https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
                 "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-red.png",
                 shadowUrl="https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.7.1/images/marker-shadow.png")
)

tabPanel.about <- tabPanel(
  "About",
  div(
    style = "text-align: center; padding-top: 64px",
    div(
      style = "width: 800px; display: inline-block; text-align: left",
      h3("For demonstration purposes only"),
      p("This proof of concept presents a synthetic agent-based infection dataset. It uses visualisations involving graph theory, statistical likelihood, and geographic information systems. Reactive elements are used where possible for navigation."),
      tags$b("No real data have been used in this app."),
      p("Names are generated with the randomNames package. Addresses are randomly sampled from the dataset at https://data.melbourne.vic.gov.au/dataset/Address-Points/imwx-szwr. Infection is simulated via a stochastic process."),
      p("Assumptions:"),
      tags$li("..."),
      br(),
      p("Potential experiments include:"),
      tags$li("..."),
      br(),
      hr(),
      p("For more information, contact Logan Wu at loganbwu@gmail.com."),
      p("https://github.com/loganbwu/ShinyContactTracing")
    ),
  ),
  tags$footer(
    p("Logan Wu 2020"),
    align = "center",
    style = "position:absolute; bottom:0; width:100%; padding: 10px;"
  )
  
)