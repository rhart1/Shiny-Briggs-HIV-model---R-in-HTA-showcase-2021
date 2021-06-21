#### HIV-AIDS R version  ####
# This R script is the Briggs et al. HIV model replicated in R. This is the model code without the shiny interface

# Model set up -------------------

# ~ Location in project folder -------------
File_location <- "./Briggs et al. HIV model/"

# ~ Load in the functions --------------
source(file.path(File_location,"functions.R"), local = TRUE)

# ~ Loading packages  ----------------------------------------------------------
#' Packages used: matpow calculates the power of a matrix
#'               expm does the same, useful matrix operators
#'               tidyverse is useful for working with data frames
#'               ggplot2 creates graphs - it is good practice to design graphs, and datatables (using DT) in the script
#'               before wrapping in shiny.
#' Packageloader loads in all the required packages, but also installs the packages that are needed but are not currently downloaded
Packageloader(c("matpow", "expm", "tidyverse", "ggplot2","DT","rhandsontable"))

#' Informing the model
#' Informing the model can happen in many different ways. Here, because it is a small model without complex analysis,
#' all inputs are defined in the model. For models with complex analysis and large data sets, it is recommended that
#' the inputs are defined, managed and stored externally to this model (in Excel CSV, workbook, SAS, or SQL for example)
#' and are called in when the model starts (you can practice by moving these inputs into external files and calling them in)
#' and then need to be sorted into lists for the programmer to be able to easily find and use them.
#' 
#' In the shiny app, the input boxes create an `input` list, this can be created here to mimic how these inputs are used
#' when this is made into a shiny app. It can then be populated with all the inputs that will be used in the model.
#' In this model, the `input` list only includes the variables that I want to be user-amendable in the shiny app. 
#' All information that I do not want as interactive/changable is not sorted into a list (but can be in complex models)
#' 
#' All inputs are taken from Briggs et al. 


# Informing model --------------
# ~ User interactive inputs --------------
#' It is important to design a shiny app with interactivity in mind. In the app to be coded, I would like the following
#' variables to be user amendable using the standard shiny inputs https://shiny.rstudio.com/tutorial/written-tutorial/lesson3/
input <- list(
  combo_HR = 0.509,                   # This is treatment HR
  Dr_C = 0.06,                        # Drug cost discount
  Dr_Ly = 0,                          # Life-year discount
  THorizon = 20                       # Time horizon
)

# ~~ Input state transition costs ---------------------------------------------

#' Defining costs in list for creating inputs
#' These are the annual costs exactly as recorded in table 2.2 of Briggs et al.
#' enter direct medical and CC costs. These are time invariant, so do not need additional logic.
#' It may be neater to make these vectors of length THorizon, using the rep() "repeat" command
#' 
#' These are being kept apart from the inputs list because they are not being made user amendable via
#' the standard shiny inputs. The inputs here are going to be amendable via rhandsontable https://jrowen.github.io/rhandsontable/
#' which behaves differently (it is rendered as an output, not an input - see server.R for example details) 

#' Medical and community costs:
DM_CC_costs = list(
  state_A = list(
    direct_medical = 1701,
    community      = 1055
  ),
  state_B = list(
    direct_medical = 1774,
    community      = 1278
  ),
  state_C = list(
    direct_medical = 6948,
    community      = 2059
  ),
  state_D = list(
    direct_medical = 0   ,
    community      = 0   
  )
)

DM_CC_costs_df = data.frame(
  unlist(DM_CC_costs$state_A),
  unlist(DM_CC_costs$state_B),
  unlist(DM_CC_costs$state_C),
  unlist(DM_CC_costs$state_D)
)

colnames(DM_CC_costs_df) <- c("state_A","state_B","state_C","state_D") 

#' Drug unit costs:
Drug_unit_costs = data.frame(rbind(2278,2086.50))
row.names(Drug_unit_costs) = c("AZT_mono","lamivudine")

#view dataframe
Drug_unit_costs


# ~ Static inputs --------------
#These can be named here or within the inputs
state_names = c("A", "B", "C", "D")

# Observation matrix - again, this can be defined here or in an inputs list
obs_mat = t(matrix(
  c(1251, 350 , 116 , 17  ,
    0   , 731 , 512 , 15  ,
    0   , 0   , 1312, 437 ,
    0   , 0   , 0   , 1),
  ncol = 4,
  nrow = 4
))

# Transition probabilities and processing data ----------------------------------------------------------
# ~ View data ----------------------------------------------------------
#Create datatable for user to view that can be used in the shiny front-end
datatable(
  data = obs_mat,
  colnames = state_names,
  rownames = state_names,
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)


# ~ Intervention transition probabilities ---------------------------------------------
#R can handle many types of data structure. The most useful data structure for CE modeling is a list.
#This is essentially a "family tree" of data, which follows a hierarchy. Here, I use this to make a list
# containing transition calculations for each arm. Notice the shift to using = instead of <-.
#The list structure means all the parameters are in the same place, so makes things very easy to QC
# These values are passed into the parameters list later on to keep everything together
# NOTE: These do 1-sum() for the diagonal elements, as is a required assumption when applying a HR to all states
tp_mono = list(
  # To and from state A
  tAA = obs_mat[1, 1] / sum(obs_mat[1, ]),
  tAB = obs_mat[1, 2] / sum(obs_mat[1, ]),
  tAC = obs_mat[1, 3] / sum(obs_mat[1, ]),
  tAD = obs_mat[1, 4] / sum(obs_mat[1, ]),
  # To and from state B
  #tBB is defined after
  tBA = 0,
  tBB = obs_mat[2, 2] / sum(obs_mat[2, ]),
  tBC = obs_mat[2, 3] / sum(obs_mat[2, ]),
  tBD = obs_mat[2, 4] / sum(obs_mat[2, ]),
  # To and from state C
  #tCC is defined after
  tCA = 0,
  tCB = 0,
  tCC = obs_mat[3, 3] / sum(obs_mat[3, ]),
  tCD = obs_mat[3, 4] / sum(obs_mat[3, ]),
  # To and from state D
  tDA = 0,
  tDB = 0,
  tDC = 0,
  tDD = 1
)
#This is added to a matrix in the parameters list section below


# ~ Comparator transition probabilities ---------------------------------------------
#' Note that in this case a HR is being applied to the matrix, which necessitates the assumption that the remaining probability
#' that is, the diagonal elements are equal to 1-sum(all other tps). This is a crude assumption, but matches its use in the Briggs example!
#' One way to streamline the model here, or add complication would be to make this list consist of vectors of length THorizon rather than individual elements
#' thus building in time-dependency of transition probabilities!

#' Make a list of transition probabilities. diagonal elements added after. This is a two stage calculation which is why it is in two lists
#' and then combined

tp_combo_a <- list(
    # To and from state A
    #tAA is defined after
    tAB = obs_mat[1, 2] / sum(obs_mat[1, ]) * input$combo_HR,
    tAC = obs_mat[1, 3] / sum(obs_mat[1, ]) * input$combo_HR,
    tAD = obs_mat[1, 4] / sum(obs_mat[1, ]) * input$combo_HR,
    # To and from state B
    #tBB is defined after
    tBA = 0,
    tBC = obs_mat[2, 3] / sum(obs_mat[2, ]) * input$combo_HR,
    tBD = obs_mat[2, 4] / sum(obs_mat[2, ]) * input$combo_HR,
    # To and from state C
    #tCC is defined after
    tCA = 0,
    tCB = 0,
    tCD = obs_mat[3, 4] / sum(obs_mat[3, ]) * input$combo_HR,
    # To and from state D
    tDA = 0,
    tDB = 0,
    tDC = 0,
    tDD = 1
  )
  #Now add in the diagonal elements as per the example. tDD is fixed at 1
tp_combo_b <- list(
    tAA = 1 - sum(tp_combo_a$tAB, tp_combo_a$tAC, tp_combo_a$tAD),
    tBB = 1 - sum(tp_combo_a$tBA, tp_combo_a$tBC, tp_combo_a$tBD),
    tCC = 1 - sum(tp_combo_a$tCA, tp_combo_a$tCB, tp_combo_a$tCD)
  )
tp_combo <- c(tp_combo_a,tp_combo_b)


#~ QC on transition probabilities ----------------------------------------
#R gives you an advantage of automatically coding QC checks. the code will give an error and stop running if the probabilities do not sum to 1
# this means that if you make an error and build and appropriate net to catch that error, the model will check itself before letting you run it.

#tp_mono is not reactive and tp_combo has been wrapped in a reactive function. Therefore, tp_combo requires brackets
if (any(c(
  tp_combo$tAA + sum(tp_combo$tAB, tp_combo$tAC, tp_combo$tAD),
  sum(tp_combo$tBA, tp_combo$tBB, tp_combo$tBC, tp_combo$tBD),
  sum(tp_combo$tCA, tp_combo$tCB, tp_combo$tCC, tp_combo$tCD),
  tp_mono$tAA + sum(tp_mono$tAB, tp_mono$tAC, tp_mono$tAD),
  sum(tp_mono$tBA, tp_mono$tBB, tp_mono$tBC, tp_mono$tBD),
  sum(tp_mono$tCA, tp_mono$tCB, tp_mono$tCC, tp_mono$tCD)
) != 1)) {
  warning("WARNING: Transition probabilities do not sum to 1")
  # If the text to render is not standard text, HTML rendering is required. This works in the 
  ## Shown format above and below and works similar to the tags
} else{
  print("Note: Transition probabilities sum to 1")
}


# Costs in the model ---------------
#' In the design stage, it was decided that the costs will be user-amendable by rhandsontable
#' which is an Excel-like table that allows copying and pasting and feels more natural to the Excel user
#' 
#' In this SHiny script, this will mean that the DM_CC_costs_table and Drug_unit_costs will be 
#' taken from the back-end base case data frames, rendered as a front-end table that the user can interact with and then when 
#' a button is clicked the table will be then taken and used back-end. These tables can be re-named to
#' distinguish them from the base case inputs.
#' 
#' In big models, it can be beneficial to sort the Base case inputs into a list, then the user-reactive lists into
#' another list. This means that 'reset to base case' functionality can be managed and that there are input

# ~ Create tables for presenting to user ---------------------
# Experiment how you would like this set up in your application
# State costs:
rhandsontable(
  data = DM_CC_costs_df,
  rowHeaders = c("direct_medical", "community"),
  colHeaders = c("state_A", "state_B", "state_C", "state_D"),
  rowHeaderWidth = 250
) %>%
  hot_col(
    renderer  = color_renderer_Edit,
    col       = 1:3,
    type      = "numeric",
    readOnly  = NULL,
    colWidths = 100,
    format    = '0,0.00'
  ) %>%   
  hot_col(
    col       = 4,
    type      = "numeric",
    readOnly  = TRUE,
    colWidths = 100,
    format    = '0,0.00'
  ) %>% 
  #The hot_validate_numeric function validated cells within RHandsontable to prevent values being entered outside acceptable range
  hot_validate_numeric(col = 1:3,
                       min = 0,
                       max = 20000)

#Drug unit costs:
rhandsontable(
  data = Drug_unit_costs,
  rowHeaders = c("AZT_mono","lamivudine"),
  colHeaders = "Cost per cycle",
  rowHeaderWidth = 250
) %>%
  hot_col(
    renderer  = color_renderer_Edit,
    col       = 1,
    type      = "numeric",
    readOnly  = NULL,
    colWidths = 100,
    format    = '0,0.00'
  )  %>%      #The hot_validate_numeric function validated cells within RHandsontable to prevent values being entered outside acceptable range
  hot_validate_numeric(col = 1,
                       min = 0,
                       max = 10000)


# Name the user-reactive tables
#' The user will change the values via the rhandsontable and then these will be used back-end
#' They therefore need to be re-named in ths shiny script, this can be simulated here
 
DM_CC_cost_table_react <- DM_CC_costs_df
Drug_cost_table <- Drug_unit_costs

# ~ Intermediate costs calculations --------------
# Combining the costs with the time horizon and the regimen stated in Briggs et al.
#These are time-sensitive, but follow a very simple pattern
#For mono, it's the same cost for each cycle
#for others we can generate some logic here. This logic can easily be expanded into a whole section of code, in the same way as excel
# by linking cycle numbers to events occurring (i.e. making a dosing schedule). As with Excel, we can add extra states for specific circumstances
# (e.g. combo_A_OffTrt), but here it is so simple that we don't need to make more Markov states. Let's just make it have the extra drug cost for the
# first two years, and then drop down:
drug_costs <- list(
    AZT_mono = rep(Drug_cost_table[1,1],input$THorizon),
    lamivudine = c(rep(Drug_cost_table[2,1],2),rep(0,input$THorizon - 2)),
    combo = rep(Drug_cost_table[1,1],input$THorizon) + c(rep(Drug_cost_table[2,1],2),rep(0,input$THorizon - 2)) 
  )

#calculate final cost in each state, remembering that these are essentially patient flow elements.
# This shows that you can actually add up columns in a data-frame just as with excel.
state_costs <- list(
    #mono state costs
    mono = list(
      A_mono = rep(sum(DM_CC_cost_table_react$state_A),input$THorizon) + drug_costs$AZT_mono,
      B_mono = rep(sum(DM_CC_cost_table_react$state_B),input$THorizon) + drug_costs$AZT_mono,
      C_mono = rep(sum(DM_CC_cost_table_react$state_C),input$THorizon) + drug_costs$AZT_mono,
      D_mono = rep(0,input$THorizon)
    ),
    #combo state costs
    combo = list(
      A_combo = rep(sum(DM_CC_cost_table_react$state_A),input$THorizon) + drug_costs$combo,
      B_combo = rep(sum(DM_CC_cost_table_react$state_B),input$THorizon) + drug_costs$combo,
      C_combo = rep(sum(DM_CC_cost_table_react$state_C),input$THorizon) + drug_costs$combo,
      D_combo = rep(0,input$THorizon)
    )
  )

# Utilities ---------------------------------------------------------------
#There are no utilities in this model, it is only concerned with life years, but it would follow a similar principal to costs above
# we would just multiply the life years by the U in each state. N.B. these utilities could easily be made time-dependent


# Parameters list -----------------------------------------------------------
#now that we have a few R objects in memory, things can get confusing and hard to follow. Let's clean things up a bit, using a list!

# Make a list structure for all the parameters. From this point onward, all calculations should use parameters items. Notice how I use subsections here to subdivide the code into easy to read chunks that are clearly denoted.
# To see the sections either click the button in the top right of this pane, (that looks a bit like a paragraph icon in word) or the bottom left 
# with the little up and down arrows next to it. I use a tilde (~) to indent, creating subsections similarly to word.

# parameters is kept as one big list for converting into a reactive expression

parameters <- list(
    # ~ Model settings --------------------------------------------------------
    #These are laid out as an embedded list to show off what you can do with a list
    settings = list(
      #state names
      state_names = state_names,
      #Discount rate: costs
      Dr_C = input$Dr_C,
      #Discount rate: life years
      Dr_Ly = input$Dr_Ly,
      #Time horizon
      THorizon = input$THorizon,
      #starting distribution of states
      state_BL_dist = c(1, 0, 0, 0)
    ),
    
    # ~ Transition probability matrices (TPMs) ------------------------------------------
    transitions = list(#Feed the information calculated above into a set of transition probability matrices.
      #Control arm
      TPM_mono = t(matrix(
        ncol = 4,
        nrow = 4,
        c(
          tp_mono$tAA,tp_mono$tAB,tp_mono$tAC,tp_mono$tAD,
          tp_mono$tBA,tp_mono$tBB,tp_mono$tBC,tp_mono$tBD,
          tp_mono$tCA,tp_mono$tCB,tp_mono$tCC,tp_mono$tCD,
          tp_mono$tDA,tp_mono$tDB,tp_mono$tDC,tp_mono$tDD
        )
      )),
      #Intervention arm
      TPM_combo = t(matrix(
        ncol = 4,
        nrow = 4,
        c(
          tp_combo$tAA,tp_combo$tAB,tp_combo$tAC,tp_combo$tAD,
          tp_combo$tBA,tp_combo$tBB,tp_combo$tBC,tp_combo$tBD,
          tp_combo$tCA,tp_combo$tCB,tp_combo$tCC,tp_combo$tCD,
          tp_combo$tDA,tp_combo$tDB,tp_combo$tDC,tp_combo$tDD
        )
      ))),
    
    # ~ Costs -----------------------------------------------------------------
    Costs = list(
      DM_CC_costs = DM_CC_cost_table_react,
      drug_costs  = drug_costs,
      state_costs = state_costs
    )
  )

# ~ View parameters -----------------
# ~~ Make datatable outputs ------------
#' Users will likely want to see the parameters in the app to confirm the inputs going through 
#' like they would in an Excel model

# Settings:
datatable(
  data = t(
    data.frame(
      parameters$settings$Dr_C,
      parameters$settings$Dr_Ly,
      parameters$settings$THorizon
    )
  ),
  rownames = c("Dr_C", "Dr_Ly", "THorizon"),
  colnames = "Value",
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)

# Transitions (monotherapy):
datatable(
  data = parameters$transitions$TPM_mono,
  rownames = state_names,
  colnames = state_names,
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)  %>% formatRound(1:4,digits = 3)

# Transitions (combination therapy):
datatable(
  data = parameters$transitions$TPM_combo,
  rownames = state_names,
  colnames = state_names,
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)  %>% formatRound(1:4,digits = 3)

# Costs inputs
datatable(
  data = parameters$Costs$DM_CC_costs,
  rownames = c("Direct medical costs", "Comminuty costs"),
  colnames = state_names,
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)  %>%   formatCurrency(
  1,
  currency = "",
  interval = 3,
  mark = ","
)

# Costs (drug costs by year)
datatable(
  data = data.frame(parameters$Costs$drug_costs),
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
)   %>%   formatCurrency(
  1:3,
  currency = "",
  interval = 3,
  mark = ","
)


# Costs (state costs by year)
datatable(
  data = data.frame(parameters$Costs$state_costs),
  rownames = NULL,
  colnames = c("Mon_A","Mon_B","Mon_C","Mon_D","Com_A","Com_B","Com_C","Com_D"),
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
) %>%   formatCurrency(1:8,currency = "", interval = 3, mark = ",")


#So now, all parameters are ready to go, and in a nice neat structure.


# Run the model -----------------------------------------------------------
#Set up a list containing patient flow items like discount factor, state populations
patient_flow <- list()

#' In this model example I have set up two further lists within patient_flow to separate the 
#' calculations and results. There are infinite ways that these outputs can be stored and sorted,
#' and there are many better ways than this for large models. It is important to experiment
#' As long as it is well commented and things are easy to find when you need them then it is okay
patient_flow$mono <- list()
patient_flow$combo <- list()

# ~ Markov state populations ----------------------------------------------
#This is the model engine, iteratively applying the transition matrix i times and recording the result
#The nature of sapply makes this in columns rather than rows, so the result needs to be transposed (t())
# Simply put, this generates a function, which gets applied at the index within that function. This requires that
#  the elements to be calculated are independent. You therefore need to calculate the cell from first principals
#  The way we do that here is by calculating the nth power of the TPM for mono. For combo later on it's more complicated,
#  as we need to calculate a time dependent set of populations. We simply use an if for this, exactly like in excel, except
#  wrapped in a function within sapply


# ~~ Mono -----------------------------------------------------------------
#Calculate state populations using vectorised (i.e. non-loop) approach

#apply from 1:time horizon a function index i calculating the cycle pop (nth power of M), starting from a starting pop of P
#neaten up and rename columns.

#The following elements have been listed as separate functions instead of as one big list (like parameters)
# it does not really matter how you split up/combine separate functions as long as you remember how to reference them later

StatePopMono <-
  sapply(1:parameters$settings$THorizon, function(i) {
    calc_cycle(
      P = parameters$settings$state_BL_dist,
      M = parameters$transitions$TPM_mono,
      n = i
    )
  }) %>%
  t() %>%
  as.data.frame()
#name the states so we can keep track nicely
names(StatePopMono) <- parameters$settings$state_names
#add a time column for reference
StatePopMono$time <- 1:parameters$settings$THorizon
data.frame(StatePopMono)

patient_flow$mono$StatePop <- StatePopMono

# ~~ Combo -----------------------------------------------------------------
#A little more complicated as it's time-dependent. this requires more annotation when you build a model, to make it understandable.
#use sapply to avoid a loop, defining an outer-function with index i. This index essentially cycles through the states, however,
# this time, the state population in cycle i depends on i. This can still be calculated from first principals, but will always be
# to some degree bespoke. Here, in the first 2 cycles, apply the TPM for combo, in the 3rd cycle switch to mono, and subsequently
# use the mono one, taking into consideration the cycle where the TPM switched from combo to mono (i.e. cycle 2-3). In short after i == 2:
#  P = state populations in cycle 2, M = mono transitions as patient is now not on combo

#calculate state populations, taking into consideration treatment duration of combo of 2 years. reformat into nice data.frame().
StatePopCombo <- sapply(1:parameters$settings$THorizon, function(i) {
  if (i <= 2) {
    #if we're still in the treatment stage (first 2 years), just calculate the populations as normal
    calc_cycle(
      P = parameters$settings$state_BL_dist,
      M = parameters$transitions$TPM_combo,
      n = i
    )
  } else if (i == 3) {
    #if patients are now off in their first year off treatment, calculate the new state population
    newTP1 <-
      calc_next_cycle(
        s_t1 = calc_cycle(
          P = parameters$settings$state_BL_dist,
          M = parameters$transitions$TPM_combo,
          n = 2
        ),
        M = parameters$transitions$TPM_mono
      )
    calc_next_cycle(
      s_t1 = calc_cycle(
        P = parameters$settings$state_BL_dist,
        M = parameters$transitions$TPM_combo,
        n = 2
      ),
      M = parameters$transitions$TPM_mono
    )
  } else {
    #If the patients are now in the years after coming off treatment, generate the "starting dist" when they came off treatment,
    # and feed that into our calc_cycle command
    newTP1 <-
      calc_next_cycle(
        s_t1 = calc_cycle(
          P = parameters$settings$state_BL_dist,
          M = parameters$transitions$TPM_combo,
          n = 2
        ),
        M = parameters$transitions$TPM_mono
      )
    #calculate state populations extrapolating out from the first cycle off treatment
    calc_cycle(P = newTP1,
               M = parameters$transitions$TPM_mono,
               n = i - 3)
  }
}) %>%
  t() %>%
  as.data.frame()

names(StatePopCombo) <- parameters$settings$state_names
StatePopCombo$time <- 1:parameters$settings$THorizon

data.frame(StatePopCombo)
patient_flow$combo$StatePop <- StatePopCombo

# ~ Time in state calculations --------------------------------------------------------------
#Discount factor can be calculated using sapply instead of a loop
patient_flow$disc$cost <- sapply(1:parameters$settings$THorizon, function(n) {1 / ((1 + parameters$settings$Dr_C) ^ n)})

patient_flow$disc$ly   <- sapply(1:parameters$settings$THorizon, function(n) {1 / ((1 + parameters$settings$Dr_Ly) ^ n)})

# Results -------------------

#Now we have all the elements which are necessary to work out the ICER. Make a list to contain all the results

#make a list structure for results
results <- list()

#~ Cost by arm -------------------------------------------------------------

# ~~ Mono -----------------------------------------------------------------
#put the state populations into the cost element, so that it's the same size
#results$costs$UnDisc$mono <- patient_flow$mono$StatePop

#replace the data with the per cycle costs by half cycle corrected time in state for that state
#use sapply to avoid looping through states individually. Note that multiplying 2 vectors gives you the individual elements multiplied
# also note that [[1]] is the 1st item in a list (i.e. when state == 1 in this sapply, it is pulling parameters$Costs$state_costs$mono[[1]], which
#  is the mono costs for state A. Make sure that your parameters list is in the correct order, or that you're referring more specifically to the state you want!)

results$costs <- list()

# Lists to separate undiscounted and discounted costs
results$costs$UnDisc <- list()
results$costs$Disc <- list()

results$costs$UnDisc$mono <-
  sapply(1:length(parameters$settings$state_names),
         function(state) {
           patient_flow$mono$StatePop[, state] * parameters$Costs$state_costs$mono[[state]]
         })

#Discounted version, using discount column. normal multiplication applies equivalent to multiplying the cell by its corresponding row value:
# A QC check of this is below the command
results$costs$Disc$mono <-
  results$costs$UnDisc$mono * patient_flow$disc$cost

#QC: Check that it's multiplying the discount correctly in a couple of arbitrary cells:
# if any of them do not match up (!= is "not equal to", and any() is...well...any), else do nothing
if (any(
  results$costs$Disc$mono[1, 1] != results$costs$UnDisc$mono[1, 1] * patient_flow$disc$cost[1],
  results$costs$Disc$mono[4, 2] != results$costs$UnDisc$mono[4, 2] * patient_flow$disc$cost[4]
)) {
  print("ERROR: Mono cost discounts applied incorrectly")
} else {
  print("Mono cost discounts applied correctly")
}

#lifetime expected costs in mono: These can be checked against your excel implementation of this model
results$costs$E_LT_cost <- list()
results$costs$E_LT_dcost <- list()
results$costs$E_LT_cost$mono <- sum(results$costs$UnDisc$mono)
results$costs$E_LT_dcost$mono <- sum(results$costs$Disc$mono)
  
  # ~~ Combo -----------------------------------------------------------------
  #Note that this time we have already built the time dependency into the state costs, so we do not need to add a bunch of logic into the sapply
  #  This illustrates that you can avoid having to do that with good preparation when setting up your parameters list!
  #put the time in state in there as a placeholder to make it the correct dimensions
  #replace the data with the per cycle costs by half cycle corrected time in state for that state
  
results$costs$UnDisc$combo <-
  sapply(1:length(parameters$settings$state_names),
         function(state) {
           patient_flow$combo$StatePop[, state] * parameters$Costs$state_costs$combo[[state]]
         })


#Discounted version, using discount column. normal multiplication applies equivalent to multiplying the cell by its corresponding row value:
# A QC check of this is below the command
results$costs$Disc$combo <-
  results$costs$UnDisc$combo * patient_flow$disc$cost

#QC: Check that it's multiplying the discount correctly in a couple of cells:
if (any(
  results$costs$Disc$combo[1, 1] != results$costs$UnDisc$combo[1, 1] * patient_flow$disc$cost[1],
  results$costs$Disc$combo[4, 2] != results$costs$UnDisc$combo[4, 2] * patient_flow$disc$cost[4]
)) {
  print("ERROR: Combo cost discounts applied incorrectly")
} else {
  print("Combo cost discounts applied correctly")
}
#lifetime expected costs in combo
results$costs$E_LT_cost$combo <- sum(colSums(results$costs$UnDisc$combo))
results$costs$E_LT_dcost$combo <- sum(colSums(results$costs$Disc$combo))

#~ LY by arm -------------------------------------------------------------
# Because LY is going to be reactive to discounting because of the input, the LY by arm and the applied discounting needs to be calculated
# It is important to be aware of this when making elements reactive or making inputs Just because the calculation is not
## required in the base-case scenario does not mean that the functionality should not be included if it changes.

results$LY <- list()

# Expected discounted and undiscounted life years
results$LY$E_LT_LY  <- list()
results$LY$E_LT_dLY <- list()

#lifetime expected LY in mono
results$LY$E_LT_LY$mono <-
  sum(patient_flow$mono$StatePop[, parameters$settings$state_names[1:3]])

results$LY$E_LT_dLY$mono <-
  sum(patient_flow$mono$StatePop[, parameters$settings$state_names[1:3]] * patient_flow$disc$ly)

#lifetime expected LY in combo
results$LY$E_LT_LY$combo <-
  sum(patient_flow$combo$StatePop[, parameters$settings$state_names[1:3]])
results$LY$E_LT_dLY$combo <-
  sum(patient_flow$combo$StatePop[, parameters$settings$state_names[1:3]] * patient_flow$disc$ly)

# ~~ QC ------------------------------------------------------------
#Use dplyr syntax to generate some row sums (summing across rows of a data frame). This uses pipes,
#  which let you pass along information rather than putting everything as nested in something else!
#  This is really helpful when performing some really complicated output.

#QC check all state populations sum to 1
QC_list <- list()

QC_list$mono <- patient_flow$mono$StatePop %>%
  select(-time) %>%
  (function(x)
    rowSums(x))

QC_list$combo <- patient_flow$combo$StatePop %>%
  select(-time) %>%
  (function(x)
    rowSums(x))

QC_list
  
# ~ ICER ------------------------------------------------------------------
# We're all very familiar with this, delta(cost)/delta(LY) in this case.

ICER <- list()

#Calculate the lifetime costs discounted and undiscounted (no 1/2 cycle correction to match the example)
ICER$inc_costs <-
  results$costs$E_LT_cost$combo - results$costs$E_LT_cost$mono
ICER$inc_dcosts <-
  results$costs$E_LT_dcost$combo - results$costs$E_LT_dcost$mono

#Directly from the patient flow list, calculate the total life years:
#This uses all of the columns which include living patients.
#The same answer can be derived using 1 - dead
ICER$inc_ly <-
  results$LY$E_LT_LY$combo - results$LY$E_LT_LY$mono
ICER$inc_dly <-
  results$LY$E_LT_dLY$combo - results$LY$E_LT_dLY$mono

#undiscounted ICER
ICER$ICERs$undiscounted <- ICER$inc_costs / ICER$inc_ly

#Discounted ICER
ICER$ICERs$discounted <- ICER$inc_dcosts / ICER$inc_dly


ICER$discounted <-
  data.frame(c(ICER$inc_dcosts, ICER$inc_dly, ICER$ICERs$discounted))
ICER$undiscounted <-
  data.frame(c(ICER$inc_costs, ICER$inc_ly, ICER$ICERs$undiscounted))


# Output ICER if we want to see it
paste("Results - ICER (discounted):", ICER$discounted[3,1])




# ~ Tables ------------------------------------------------------------------
# These are the outputs that I would like to output front-end into the shiny code

datatable(
  data = ICER$discounted,
  rownames = c("Incremental cost", "Incremental QALY", "ICER"),
  colnames =  NULL,
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
) %>% formatRound(1, digits = 2) %>% formatCurrency(1,
                                                    currency = "",
                                                    interval = 3,
                                                    mark = ",")


datatable(
  data = ICER$undiscounted,
  rownames = c("Incremental cost", "Incremental QALY", "ICER"),
  colnames =  NULL,
  options = list(
    lengthChange = FALSE,
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE
  )
) %>%
  formatRound(1, digits = 2) %>%
  formatCurrency(1,
                 currency = "",
                 interval = 3,
                 mark = ",")
  
  
  
  
  # ~ Graphs --------------------------------------------------------------
  # As with the tables, the graphs can also be used in the same was as without shiny, but inserted inside the shiny functions
  #draw the Markov traces of each arm
draw_trace(patient_flow$mono$StatePop %>% select(-time) , "Monotherapy")

draw_trace(patient_flow$combo$StatePop %>% select(-time),
           "Combination therapy")


#plot of markov traces next to each other for reference
plotelements <- list()

plotelements$x <- patient_flow$mono$StatePop %>%
  gather(time) %>%
  rename(state = time) %>%
  group_by(state) %>%
  mutate(lab = "Mono", time = row_number() - 1)
plotelements$y <- patient_flow$combo$StatePop %>%
  gather(time) %>%
  rename(state = time) %>%
  group_by(state) %>%
  mutate(lab = "Combo", time = row_number() - 1)

rbind(plotelements$x, plotelements$y) %>%
  ggplot(aes(
    x = time,
    y = value,
    colour = state,
    linetype = lab
  )) +
  geom_line() +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Years since baseline",
       y = "%",
       title = "Comparison")

