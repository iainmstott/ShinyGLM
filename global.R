#packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(ggfortify)

datafiles <- (list.files("data"))
data <- lapply(datafiles, function(dat){ read_csv(paste("data/", dat, sep = "")) })
datanames <- sapply(datafiles, function(dat){ gsub(".csv", "", dat) })
names(datanames) <- datanames
names(data) <- datanames

# Prism syntax highlight
prismCodeBlock <- function(code) {
  tagList(
    HTML(code),
    tags$script("Prism.highlightAll()")
  )
}


