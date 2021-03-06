library(shiny)
library(tidyverse)
library(sf)
library(visNetwork)
library(tmap)
library(DT)
library(tidygraph)
library(leaflet)
library(RSQLite)
library(plotly)
library(leafpop)
source("functions.R")

data.dir = "data-processed"
gpkg.path = file.path(data.dir, "data.gpkg")
con = dbConnect(SQLite(), gpkg.path)

# Load data
people = st_read(gpkg.path, "people") %>% st_transform(4326)
people$`First.name`[1] = "Logan"
people$`Last.name`[1] = "Wu"
people$Workplace[1] = "Arup, 699 Collins Street"
people$Infection.acquired = Sys.Date() - people$Infection.acquired[1] + people$Infection.acquired
names(people) = names(people) %>% str_replace("\\.", " ")
popup.cols = c("PID", "First name", "Last name", "Address", "Workplace", "Infection acquired")
people$popuptable = popupTable(people, popup.cols, row.numbers=F, feature.id=F)
n = nrow(people)
infections = dbReadTable(con, "infections")

# calculate infectivity at time of acquisition
x.distance = exp(-st_distance(people) %>% as.numeric / 10000)
x.workplace = outer(people$Workplace, people$Workplace, function (a, b) a==b) %>% as.numeric
x.infectivity = outer(people$`Infection acquired`, people$`Infection acquired`, function(a, b) infectivity(b - a)) * (1 - diag(n)) # Do not use NA otherwise colsum may be NA

# importance parameters and likelihood function - in theory subject to machine learning!
b.distance = 5e-3
b.workplace = 5e-3
likelihood = (b.distance * x.distance + b.workplace * x.workplace) * x.infectivity

# Read graph from database and create edge geometry by joining to people
edges = infections %>%
    slice(-1) %>%
    left_join(people %>% select(PID), by=c("from"="PID")) %>%
    left_join(people %>% select(PID), by=c("to"="PID"), suffix=c(".from", ".to"))
coords = cbind(st_coordinates(edges$geom.from), st_coordinates(edges$geom.to))
linestrings = st_sfc(
    lapply(1:nrow(coords),
           function(i){
               st_linestring(matrix(coords[i,],ncol=2,byrow=TRUE))
           }))
edges = edges %>% select(from, to)
graph = tbl_graph(nodes=people, edges=edges, node_key="PID") %>%
    activate(edges) %>%
    mutate(geom = linestrings) %>%
    activate(nodes) %>%
    mutate(`Infection end` = `Infection acquired` + 14,
           PID = PID %>% as.character %>% fct_inorder,
           label = paste(paste("PID:", PID),
                         paste("Name:", `First name`, `Last name`),
                         paste("Address:", Address),
                         paste("Workplace:", Workplace),
                         paste("Acquired:", format(`Infection acquired`, "%e %b, %Y")),
                         sep="<br>"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    pid = reactiveVal()
    
    # ways to change active PID
    observeEvent(input$PID, {
        pid(as.numeric(input$PID))
    })
    observeEvent(input$current_node_id, {
        node.pid = person.subgraph() %>%
            nodes_as_sf %>%
            slice(input$current_node_id) %>%
            pull(PID)
        updateTextInput(session, "PID", value=node.pid)
    })
    observeEvent(input$us_select_button, {
        row = input$us_select_button[1] %>% str_extract("[0-9]+$") %>% as.numeric
        row.pid = person.upstream()$PID[row]
        updateTextInput(session, "PID", value=row.pid)
    })
    observeEvent(input$ds_select_button, {
        row = input$ds_select_button[1] %>% str_extract("[0-9]+$") %>% as.numeric
        row.pid = person.downstream()$PID[row]
        updateTextInput(session, "PID", value=row.pid)
    })
    
    person = reactive({
        people %>% filter(PID == pid())
    })
    
    person.subgraph = reactive({
        graph %>%
            filter(node_distance_from(pid()) <= input$graph_order | node_distance_to(pid()) <= input$graph_order)
    })
    
    person.upstream = reactive({
        cat("Person PID", person()$PID, "\n")
        upstream.likelihood = likelihood[,pid()]
        upstream.contacts = people %>%
            mutate(likelihood = upstream.likelihood,
                   Relation = ifelse(PID %in% infections$from[infections$to==pid()], "Upstream", NA)) %>%
            select(-StrName, -Suburb) %>%
            st_drop_geometry %>%
            arrange(Relation, desc(likelihood))
    })
    
    person.downstream = reactive({
        downstream.likelihood = likelihood[pid(),]
        downstream.contacts = people %>%
            mutate(likelihood = downstream.likelihood,
                   Relation = ifelse(PID %in% infections$to[infections$from==pid()], "Downstream", NA)) %>%
            select(-StrName, -Suburb) %>%
            st_drop_geometry %>%
            arrange(Relation, desc(likelihood))
    })
    
    confirmed.contacts = reactive({
        people %>%
            filter(PID %in% c(infections$to[infections$from==pid()], infections$from[infections$to==pid()], pid())) %>%
            arrange(desc(`Infection acquired`)) %>%
            mutate(`Infection end` = `Infection acquired` + 14,
                   PID = PID %>% as.character %>% fct_inorder,
                   label = paste(paste("PID:", PID),
                                 paste("Name:", `First name`, `Last name`),
                                 paste("Acquired:", format(`Infection acquired`, "%e %b, %Y")),
                                 sep="<br>"),
                   color = ifelse(PID == pid(), "case", "contact"))
    })
    
    output$table_pid = renderTable(
        person() %>% select(-popuptable) %>% st_drop_geometry %>% t %>% as.data.frame,
        rownames=T, colnames=F, striped=T, hover=T, width="100%"
    )
    
    output$timeline = renderPlotly({
        g = person.subgraph() %>%
            nodes_as_sf %>%
            arrange(desc(`Infection acquired`)) %>%
            mutate(PID = PID %>% as.character %>% fct_inorder,
                   color = ifelse(PID == pid(), "case", "contact")) %>%
            ggplot(aes(text = label, color=color)) +
            geom_vline(xintercept = as.numeric(person()$`Infection acquired`), alpha=0.5) +
            geom_segment(aes(x = `Infection acquired`, y = PID, xend = `Infection end`, yend = PID)) +
            geom_point(aes(x = `Infection acquired`, y = PID)) +
            geom_text(aes(x = `Infection acquired`, y = PID, label = PID), hjust=1, nudge_x = -1) +
            scale_x_date() +
            scale_color_manual(values=c("case"="firebrick", "contact"="steelblue")) +
            theme_bw() +
            theme(legend.position = "none",
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank()) +
            labs(x = NULL, y = NULL)
        n.contacts = nrow(confirmed.contacts())
        g.height = 30 * n.contacts + 40  # 30px per bar + 40 for y labels
        ggplotly(g, tooltip = c("text")) %>%
            layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                   paper_bgcolor = "rgba(0, 0, 0, 0)",
                   fig_bgcolor   = "rgba(0, 0, 0, 0)",
                   margin = list(t=0, l=0, r=0, b=0),
                   height = g.height) %>%
            config(displayModeBar = F)
    })
    
    output$subgraph_vis = renderVisNetwork({
        pids = person.subgraph() %>% nodes_as_sf %>% pull(PID)
        dates = person.subgraph() %>% nodes_as_sf %>% pull(`Infection acquired`)
        colors = pal(dates, c("steelblue", "grey"))
        colors[pids == pid()] <- "firebrick"
        visNetwork(
            person.subgraph() %>%
                nodes_as_sf %>%
                mutate(id = row_number(),
                       title = popuptable,
                       label = PID, # visNetwork looks for these fields by default
                       color = colors),
            person.subgraph() %>%
                edges_as_sf %>%
                mutate(arrows = "to")) %>%
            visEvents(select = "function(nodes) { Shiny.onInputChange('current_node_id', nodes.nodes); }") %>%
            visOptions(highlightNearest = list(enabled=T, hover=T))
    })
    
    output$map = renderLeaflet({ leaflet() %>% addProviderTiles(providers$CartoDB.Positron) })
    
    observe({
        # update map on person change
        person.sf = person.subgraph() %>%
            nodes_as_sf %>%
            st_transform(4326) %>%
            mutate(color = ifelse(PID == person()$PID, "red", "blue"))
        box = person.sf %>% st_bbox %>% as.numeric
        leafletProxy("map") %>%
            clearMarkers() %>%
            clearMarkerClusters() %>%
            clearShapes() %>%
            addPolylines(data = person.subgraph() %>% edges_as_sf %>% pull(geom),
                         weight = 1,
                         color = "#555") %>%
            addMarkers(
                data = person.sf %>% filter(PID != person()$PID),
                icon = colorIcons$blue,
                label = ~PID,
                popup = ~popuptable,
                clusterOptions = markerClusterOptions(maxClusterRadius=10)
            ) %>%
            addMarkers(
                data = person.sf %>% filter(PID == person()$PID),
                icon = colorIcons$red,
                label = ~PID,
                popup = ~popuptable
            ) %>%
            fitBounds(lng1 = box[1], lat1 = box[2], lng2 = box[3], lat2 = box[4],
                      options = list(padding = c(100, 100)))
    })
    
    output$table_upstream = renderDT(
        person.upstream() %>%
            mutate(` ` = shinyInput(actionButton, nrow(.), 'button_', label="Go", onclick='Shiny.onInputChange(\"us_select_button\",  this.id)')) %>%
            select(` `, everything(), -popuptable) %>%
            datatable(style="bootstrap", rownames=F, selection="single",
                      escape = -which(colnames(person.upstream()) == " ")) %>%
            formatStyle("likelihood",
                        background = styleColorBar(person.upstream()$likelihood, "lightblue"),
                        backgroundSize = "95% 50%",
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = "left") %>%
            formatStyle("Relation",
                        target = "row",
                        fontWeight = styleEqual("Upstream", "bold")) %>%
            formatRound(columns="likelihood", digits=3),
        server = T
    )
    
    output$table_downstream = renderDT(
        person.downstream() %>%
            mutate(` ` = shinyInput(actionButton, nrow(.), 'button_', label="Go", onclick='Shiny.onInputChange(\"ds_select_button\",  this.id)')) %>%
            select(` `, everything(), -popuptable) %>%
            datatable(style="bootstrap", rownames=F, selection="single",
                      escape = -which(colnames(person.downstream()) == " ")) %>%
            formatStyle("likelihood",
                        background = styleColorBar(person.downstream()$likelihood, "lightblue"),
                        backgroundSize = "95% 50%",
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = "left") %>%
            formatStyle("Relation",
                        target = "row",
                        fontWeight = styleEqual("Downstream", "bold")) %>%
            formatRound(columns="likelihood", digits=3),
        server=T
    )
    
})
