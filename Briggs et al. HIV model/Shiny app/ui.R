# The UI.R script contains the ui, this is the layout of the graphical user interface (GUI) of your app
# This can be read linearly, going through content positioning tab by tab

Packageloader <- function(packs) {
  system.time(if (length(setdiff(packs, rownames(
    installed.packages()
  ))) > 0) {
    print("you need the following packages to run this script")
    print(setdiff(packs, rownames(installed.packages())))
    print("Installing them now...")
    install.packages(setdiff(packs, rownames(installed.packages())))
    print("Now loading libraries...")
    sapply(packs, require, character.only = TRUE)
  } else {
    print("All packages are installed already")
    print("Loading the specified libraries...")
    sapply(packs, require, character.only = TRUE)
  })
}


# ~ Loading packages  ----------------------------------------------------------
# Loading packages  ----------------------------------------------------------
#' Packages used: shiny uses the main shiny functions
#'               shinydashboard contains the functions for the main UI layout
#'               DT is the package used for producing data tables
#'               shinymaterial contains symbols/icons used in the UI layout
#'               rhandsontable creates the rhandsontable element
#'               shinyjs is so that we can use the java function 'click'
#'               matpow calculates the power of a matrix
#'               expm does the same, useful matrix operators
#'               tidyverse is good for managing data
Packageloader(c("shiny", "shinydashboard", "DT","shinymaterial","rhandsontable","shinyjs",
                "matpow", "expm", "tidyverse"))



ui <-
  # ~ Header ------------------------------------------------------------------
dashboardPage(
  #dashboardHeader(title = "HIV AIDS model"),
  #dashboardHeader can be customised and the title realigned if required using the following code:
  dashboardHeader(
    title = "HIV AIDS model", #This is kept, even though it is not displayed, because it names the tab
    titleWidth = 0 ,
    tags$li(class = "dropdown", tags$a(("HIV AIDS model")))
  ),
  
  # ~ Sidebar -----------------------------------------------------------------
  # Laying out the sidebar of the UI, this defines all the tabs which are used within this app
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem(
        "Model settings",
        tabName = "tab_Settings",
        icon = icon("home", lib = "glyphicon") # glyphicon is a library within shinymaterials which has the little logos,
        # there is an entire library online
      ),
      menuItem(
        "Clinical data",
        tabName = "tab_Data",
        icon = icon("stats", lib = "glyphicon")
      ),
      menuItem(
        "Cost inputs",
        tabName = "tab_Costs",
        icon = icon("pencil", lib = "glyphicon")
      ),
      menuItem(
        "Parameters",
        tabName = "tab_Parameters",
        icon = icon("list-ul"),
        menuSubItem("Settings", tabName = "tab_Param_setting"),
        menuSubItem("Transition probabilities", tabName = "tab_Param_TP"),
        menuSubItem("Costs", tabName = "tab_Param_Costs")
      ),
      menuItem("Model results", tabName = "tab_Model_res", icon = icon("table")),
      br(),br(),
      column(12, align="center",
      actionButton(inputId = "Run_model", label = "Run the model", style = "color: #000000; background-color: #26b3bd; border-color: #2e6da4"))
    )
  ),
  
  
  # ~ Tab layouts -------------------------------------------------------------
  # The inputs, outputs and general layout of each of the tabs. The order is irrelevant but it is good practice to
  ## keep it in the order it was stated in the sidebar layout
  dashboardBody(
    tags$head(
      tags$style(
      HTML(
        '
        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
        background-color: #002C5D;
        font-size: 16px;
        font-weight: bold;
        }
        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
        background-color: #46B8D2;
        }
        /* main sidebar */
        .skin-blue .main-sidebar {
        background-color: #222D32;
        }
        /* other links in the sidebarmenu when hovered */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        background-color: #002C5D;
        }
        /* toggle button when hovered  */
        .skin-blue .main-header .navbar .sidebar-toggle:hover{
        background-color: #002C5D;
        } '
      )
    )),
    useShinyjs(),
    tabItems(
      tabItem(
        # ~~ Model settings --------------------------------------------------------
        tabName = "tab_Settings",
        tags$h2("Model settings"),
        fluidRow(
          # R Shiny normally renders vertically. fluidrow allows horizontal layering
          column(
            # Columns within fluidrow. the width of the whole page is always 12
            4,
            box(
              title = "General settings",
              width = 12,
              # This '12' refers to the whole space within the column
              solidHeader = TRUE,
              # Whether you want a solid colour border
              collapsible = FALSE,
              # Collapsible box
              status = "info",
              # This is the reference to the colour of the box
              numericInput(
                inputId = "Dr_C",
                label = "Drug costs discount",
                value = 0.06,
                max = 1,
                min = 0,
                step = 0.01
              ),
              numericInput(
                inputId = "Dr_Ly",
                label = "Drug LY discount",
                value = 0,
                max = 1,
                min = 0,
                step = 0.01
              ),
              numericInput(
                inputId = "THorizon",
                label = "Time horizon (years)",
                value = 20,
                max = 100,
                min = 10,
                step = 1
              )
            )
          ),
          column(
            4,
            box(
              title = "Other settings",
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              numericInput(
                inputId = "combo_HR",
                label = "Hazard Ratio (HR) for Combo",
                value = 0.509,
                max = 2,
                min = 0.001,
                step = 0.001
              )
            )
          ),
          box(
            title = "Testing values",
            width = 12,
            solidHeader = TRUE,
            tags$b(
              "These are the values that are being read in the server, this is to test the reactivity of the input interaction."
            ),
            #there is an entire shiny HTML tags glossary to make your UI look the way you want. alternatively you can render text by
            # using the htmlOutput() function.
            br(),
            br(),
            textOutput(outputId = "test_setting"),
            br(),
            #textOutput(outputId = "test_Monotrans"), #this is commented out for this model version
            #br(),
            textOutput(outputId = "test_Combtrans"),
            br(),
            textOutput(outputId = "test_ICER")
          )
        )
      ),
      
      # ~~ Data -----------------------------------------------------------------
      tabItem(tabName = "tab_Data", h2("Data"),
              fluidRow(
                box(
                  title = "Observed trial data",
                  width = 5,
                  solidHeader = TRUE,
                  status = "primary",
                  "Observed trial data state transitions:",
                  dataTableOutput(outputId = "Matrix")
                )
              )),
      
      # ~~ Drug costs -----------------------------------------------------------
      tabItem(tabName = "tab_Costs", h2("Cost inputs"),
              fluidRow(
                box(
                  title = "State costs",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  rHandsontableOutput(outputId = "DM_CC_costs_table", width = "90%")
                ),
                box(
                  title = "Drug costs",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  rHandsontableOutput(outputId = "Drug_costs_table", width = "90%")
                )
              )),
      
      
      # ~~ Parameters -----------------------------------------------------------
      tabItem(tabName = "tab_Param_setting",
              h2("Settings"),
              fluidRow(
                box(
                  title = "Active model settings",
                  width = 6,
                  solidHeader = TRUE,
                  status = "primary",
                  dataTableOutput(outputId = "Active_settings")
                )
              )),
      
      tabItem(
        tabName = "tab_Param_TP",
        h2("Transition probabilities"),
        fluidRow(
          box(
            title = "tp_mono",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            dataTableOutput(outputId = "tp_mono_DF")
          ),
          box(
            title = "tp_combo",
            width = 6,
            solidHeader = TRUE,
            status = "info",
            dataTableOutput(outputId = "tp_combo_DF")
          ),
          box(
            title = "tp_QC",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            uiOutput(outputId = "TP_QC")
          )
        )
      ),
      
      tabItem(tabName = "tab_Param_Costs", h2("Active costs"),
              fluidRow(
                box(
                  title = "Disease management costs",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  status = "primary",
                  tags$b("Disease management costs per state"),
                  # tags refers to HTML tags which influence font and layout.
                  # b is bold. u is underlined. There is a list online for all tags
                  dataTableOutput(outputId = "Param_DM_drugcost")
                ),
                box(
                  title = "Drug costs per cycle",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  solidHeader = TRUE,
                  status = "info",
                  tags$b("Drug costs per cycle"),
                  dataTableOutput(outputId = "Param_drugcost")
                ),
                box(
                  title = "Total costs per state per cycle",
                  width = 12,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  solidHeader = TRUE,
                  status = "success",
                  tags$b("State cost per cycle"),
                  dataTableOutput(outputId = "Param_statecost")
                )
              )),
      
      # ~~ Model results --------------------------------------------------------
      tabItem(
        tabName = "tab_Model_res",
        h2("Model results"),
        conditionalPanel("input.Run_model > 0",   # Conditional panels are used to render the UI depending on
                         # particular values in the inputs, including action buttons
                         # This "input.Run_model > 0" is only true if "input.Run_model"
                         # has been pressed
                         fluidRow(
                           box(
                             title = "Model summary",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             tags$u("Discounted results:"),
                             dataTableOutput(outputId = "disc_Results_table"),
                             br(),
                             tags$u("Undiscounted results:"),
                             dataTableOutput(outputId = "undisc_Results_table")
                           ),
                           box(
                             title = "Model graphs",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             tags$u("Monotherapy trace:"),
                             plotOutput(outputId = "mono_trace"),
                             br(),
                             tags$u("Combination trace:"),
                             plotOutput(outputId = "combo_trace"),
                             br(),
                             tags$u("Comparative trace:"),
                             plotOutput(outputId = "comparative_markov")
                           )
                         )),
        conditionalPanel(
          "input.Run_model == 0",
          # If Run_model has not been selected
          tags$h3("Select 'Run the model' button in Model settings to run the model")
        )
      )
      
    )
  )
)