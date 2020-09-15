# Shiny Contact Tracing

This is a demo dashboard to experiment with user interfaces for epidemiological contact linking.

## Getting Started

[Download](https://rstudio.com/products/rstudio/download/) and install [RStudio](https://rstudio.com/). You can access RStudio primers from the [rstudio.cloud website](https://rstudio.cloud/learn/primers).

### Prerequisites

Install R and any required packages:

```{r}
install.packages(c("shiny", "dplyr", "stringr", "magrittr", "sf", "leaflet.extras" "leafgl", "igraph", "tidygraph", "forcats"))
```

### Installing

Open the project `ContactTracing.Rproj` in RStudio. With `server.R` or `ui.R` open, launch a local version of the app with "Run app" or `shiny::runApp()`.


## Deployment

Use RStudio to deploy to any R Server instance or a free account from [shinyapps.io](https://www.shinyapps.io/).

Existing deployment in progress via ShinyApps.io.


## Authors

* **Logan Wu** Epidemiologist Support Officer, Victorian Department of Health and Human Services - [Github](https://github.com/loganbwu)

