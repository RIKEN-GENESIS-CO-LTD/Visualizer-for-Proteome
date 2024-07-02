######### Package Loading ##########
library(shiny)



########## Viewer launch ##########
shiny::runApp("app", launch.browser=TRUE)
# options(shiny.reactlog=TRUE) # https://shiny.posit.co/r/articles/improve/debugging/
# In the Shiny app, press Ctrl+F3 to launch the reactive log visualization. It’ll look something like this: