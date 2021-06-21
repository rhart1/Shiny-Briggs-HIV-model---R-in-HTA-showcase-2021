#### HIV-AIDS R Shiny version  ####
#' The model is designed to be reactive() until the point of the parameters, but the process of taking the parameters list
#' and running the model to produce the ICER and graphs is dependent on a button. Interacting with the model, you will find that the
#' prints to console and to the model which are dependent on the parameters list will update whenever any of the values are changed.
#' However, the ICER and QC prints will only update when the button is selected. 
#' 
#' Reactive and observe are different from eventReactive and observeEvent in that they do not require a specific event
#' for them to render, they will look for changes in every loop of the model. Reactive is an object and makes named elements
#' observe executes actions.
#' 
#' As a general rule eventReactive and reactive are things and observeEvent and observe are actions. Whatever happens in observEevent
#' will be generated (graphs, processes, printing messaged etc.) but the things that happen within the observe() or or observeEvent()
#' that cannot be referenced elsewhere in the model. eventReactive is a thing which can be called later in the model, so can be described
#' as an object and not a process. A key thing here is to look at how they are called in the script and what is inside them.
#' 
#' This app also is laid out in 3 separate scripts, app.R, server.R and ui.R
#' This naming format allows the scripts to be identified by R as an application, and allows the runApp() function to
#' be used along with its associates arguments for activating the app
#' 
#' HTML styles have also been added to this script to customize the theme of the application. See below to view how alternative
#' colours have been applied 

library("shiny")

App_location <- "./Briggs et al. HIV model/Shiny app/"

########################## RUN THE APP ############################################
message(" *** Starting up model, a window will open in default browser ***")

runApp(appDir = file.path(getwd(),App_location),  # This looks for the ui.R, server.R and www folder within the wd (if it exists). Do not rename these.
       launch.browser = TRUE,   # This line will open the application in the user's default browser
       quiet = FALSE,   
       display.mode = "normal") # Can equal "showcase"


