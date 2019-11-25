### Notes...
### Make matrix update on population selection

# install packages if necessary
install.packages("shiny")
install.packages("shinythemes")
install.packages("shinyjs")
install.packages("ggplot2")
install.packages("readr")
install.packages("DT")
install.packages("ggfortify")

# Make sure the directory is correct
setwd("C:/Dropbox/Work/Software/RShiny/ShinyGLiM")


# Run the application
shiny::shinyAppDir(appDir = "C:/Dropbox/Work/Software/Rshiny/ShinyGLiM",
                   options = list(launch.browser=
                                        TRUE)
                                        #rstudioapi::viewer)
                   )



# Deploy the application to shinyapps.io
rsconnect::deployApp(appDir = "C:/Dropbox/Work/Software/Rshiny/ShinyGLiM",
                     appName = "ShinyGLiM", appId = "1249945",
                     account = "iainmstott", upload = TRUE)


