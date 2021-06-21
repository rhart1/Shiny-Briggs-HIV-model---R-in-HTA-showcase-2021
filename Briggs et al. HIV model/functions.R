# ~ UDF to install and load packages--------------------------------------------------------------
# A user-defined function to automatically check that you have installed the packages that are required
#setdiff finds the difference between two vectors, the rest is very simple logic
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

# ~ UDFs to calculate state populations -----------------------
#This is a very simple function: it simply raises a matrix M to a power n, according to
#a starting population variable P. this uses the matrix power (matpow) package, for the %^% command!
calc_cycle <- function(M, n, P) {
  P %*% (M %^% n)
}
#version for transition probabilities which change over time (for combo arm)
#M = matrix to apply, s_t1 = s(t-1)
calc_next_cycle <- function(M,s_t1){
  s_t1 %*% M
}

# ~ UDF to draw some plots --------------------------------------------------------------
#simple function to draw graphs. This shows that you can make a function and use it so that you only have ot change one thing
# to affect all the uses. For example, if you delete the theme_classic() from this command and then run the graphs, they will all
# look different when you've run the lines of code again.
draw_trace <- function(patient_flow_data, arm_name) {
  patient_flow_data %>%
    gather() %>%
    group_by(key) %>%
    mutate(t = 1:max(n())) %>%
    ggplot(aes(x = t, y = value, colour = key)) +
    geom_line() +
    theme_classic() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(legend.position = "bottom") +
    labs(x = "Years since baseline",
         y = "%",
         title = arm_name)
}


# ~ Javascript UDF --------------------------------------------------------------
# The rhandsontable elements work through javascript, and so to format them javascript needs to be used. 
# This is used to colour all the cells which can be edited in lightblue - this is optional, but shows that such options
# do exists to customize the items within the shiny interface
color_renderer_Edit <- "
  function(instance, td) {
Handsontable.renderers.NumericRenderer.apply(this, arguments);
td.style.background = 'lightblue';
  }
"

