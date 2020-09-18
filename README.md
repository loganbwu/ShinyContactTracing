# Shiny Contact Tracing

This is a demo dashboard to experiment with user interfaces for epidemiological contact linking.

This does **not** involve any confidential, private, or commercial data, and does not contain any information from real-world events or the Victorian DHHS.

## Getting Started

[Download](https://rstudio.com/products/rstudio/download/) and install [RStudio](https://rstudio.com/). You can access RStudio primers from the [rstudio.cloud website](https://rstudio.cloud/learn/primers).

### Prerequisites

Install R and any required packages:

```{r}
install.packages(c("shiny", "tidyverse", "sf", "leaflet", "tmap", "visNetwork", "igraph", "tidygraph", "DT", "RSQLite"))
```

### Installing

Open the project `ContactTracing.Rproj` in RStudio. With `server.R` or `ui.R` open, launch a local version of the app with "Run app" or `shiny::runApp()`.


## Deployment

Use RStudio to deploy to any R Server instance or a free account from [shinyapps.io](https://www.shinyapps.io/).

Existing deployment available at https://loganwu.shinyapps.io/ContactTracing/.


## Authors

* **Logan Wu** Epidemiologist Support Officer, Victorian Department of Health and Human Services - [Github](https://github.com/loganbwu)

