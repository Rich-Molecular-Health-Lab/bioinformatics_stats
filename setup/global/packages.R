install_missing_packages <- function(required_packages) {
  installed_packages <- rownames(installed.packages())
  missing_packages   <- dplyr::setdiff(required_packages, installed_packages)
  
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  } else {
    message("All required packages are already installed.")
  }
}


required_packages <- c(
  "knitr",
  "ape",
  "bibtex",
  "BiocManager",
  "bookdown",
  "bsicons",
  "bslib",
  "bsplus",
  "conflicted",
  "crosstalk",
  "data.table",
  "devtools",
  "fontawesome",
  "glue",
  "ggtext",
  "gt",
  "gtExtras",
  "gtable",
  "here",
  "htmltools",
  "htmlwidgets",
  "kableExtra",
  "MASS",
  "lmerTest",
  "lubridate",
  "paletteer",
  "pander",
  "pandoc",
  "philr",
  "phyloseq",
  "png",
  "rcompanion",
  "reactable",
  "reactablefmtr",
  "rmarkdown",
  "sass",
  "scales",
  "shiny",
  "shinydashboard",
  "shinyjs",
  "shinyMatrix",
  "shinyTime",
  "showtext",
  "thematic",
  "tidyverse",
  "tippy",
  "usethis",
  "utf8",
  "rmdformats"
)

if (!exists("packages_checked")) {
  install_missing_packages(required_packages)
  packages_checked <- TRUE
}

library(knitr)
library(ape)
library(bibtex)
library(BiocManager)
library(bookdown)
library(bsicons)
library(bslib)
library(bsplus)
library(conflicted)
library(crosstalk)
library(data.table)
library(devtools)
library(fontawesome)
library(glue)
library(ggtext)
library(gt)
library(gtExtras)
library(gtable)
library(here)
library(htmltools)
library(htmlwidgets)
library(kableExtra)
library(MASS)
library(lmerTest)
library(lubridate)
library(paletteer)
library(pander)
library(pandoc)
library(philr)
library(phyloseq)
library(png)
library(rcompanion)
library(reactable)
library(reactablefmtr)
library(rmarkdown)
library(sass)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyMatrix)
library(shinyTime)
library(showtext)
library(thematic)
library(tidyverse)
library(tippy)
library(usethis)
library(utf8)
library(rmdformats)