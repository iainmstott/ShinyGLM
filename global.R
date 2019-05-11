#packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)

datafiles <- (list.files("data"))
for(i in 1:length(datfiles)){
    read_csv(paste("data/", datafiles, sep = ""))
}
