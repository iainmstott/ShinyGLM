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

allThemesHack <- function() {
  themes <- dir(
    system.file("shinythemes/css", package = "shinythemes"),
    "*.min.css"
  )
  sub(".min.css", "", themes)
}

themeSelectorHack <- function() {
  shiny::absolutePanel(
    bottom = "20px",
    left = "20px",
    draggable = FALSE,
    fixed = FALSE,
    style = "width: 250px; z-index: 100000;",
    div(class = "panel-body",
      selectInput("shinytheme-selector", "Colour scheme:",
        c("default", allThemesHack()),
        selectize = FALSE
      )
    ),
    tags$script(
"$('#shinytheme-selector')
  .on('change', function(el) {
    var allThemes = $(this).find('option').map(function() {
      if ($(this).val() === 'default')
        return 'bootstrap';
      else
        return $(this).val();
    });
    // Find the current theme
    var curTheme = el.target.value;
    if (curTheme === 'default') {
      curTheme = 'bootstrap';
      curThemePath = 'shared/bootstrap/css/bootstrap.min.css';
    } else {
      curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
    }
    // Find the <link> element with that has the bootstrap.css
    var $link = $('link').filter(function() {
      var theme = $(this).attr('href');
      theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
      return $.inArray(theme, allThemes) !== -1;
    });
    // Set it to the correct path
    $link.attr('href', curThemePath);
  });"
    )
  )
}