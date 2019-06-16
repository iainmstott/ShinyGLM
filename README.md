## Shiny apps for learning stats with `R`  

This folder contains a shiny app which is designed to help novice learners
understand the nature of data, how to visualise data using 2D plots, and how 
to execute generalized linear models (GLiMs). I advocate an approach of 
data -> plots -> analyses. 
 
The tabs are to be taken in order. First, understand individual variables 
(data). Second, understand relationships between multiple variables (plots). 
Third, determine significance of and parameters for these relationships 
(analyses). Start with Gaussian data and progress from there. 

The apps utilise [`R`](https://www.R-project.org), and in particular the 
[`ggplot2`](https://en.wikipedia.org/wiki/Ggplot2) 
package, which follows the "grammar of graphics" framework for data 
visualisation. The apps provide a GUI for learning theories of data, data 
visualisation and GLiMs, but display corresponding `R` code to implement these, 
which (I hope) is a softer approach to introducing learners to coding for the 
first time. Accompanying exercises and 
lecture slides supplement learning by reinforcing the lessons learned through
the app, and encouraging students to go beyond the foundation of the app to work 
with new data, new variables, new plots, new analyses, unknown arguments, 
new funtions, etc., usw., osv.  

Many of the data examples are of a biological / ecological / conservation / 
evolution nature, because that's what I do and that's what I teach. If you 
want to fork and replace with your own data, I think that should work 
provided they're in the correct format for working with 
[read_csv](https://readr.tidyverse.org/reference/read_delim.html).  

I teach the course using the excellent 
[Getting Started With `R`](https://global.oup.com/academic/product/getting-started-with-r-9780198787846?cc=us&lang=en&)
textbook by
[Andrew Beckerman](https://github.com/andbeck),
[Dylan Childs](https://github.com/dzchilds) and 
[Owen Petchey](https://github.com/opetchey).
I recommend it.

The app is available here:  
[ShinyGLiM](https://iainmstott.shinyapps.io/ShinyGLiM)  
