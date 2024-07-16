# Load packages
library(ragg)
library(patchwork)

# Load functions needed
source("functions_final.R")

################################################################################
################################# INSTRUCTIONS #################################
################################################################################
# To run the exploratory simulation, only the following parameters need to be 
# changed. All code after simulation should remain the same.

# Variables that contain the following structure c(.., ..., ..) refer to 
# the number of effects / regression coefficients per depth level

# Example:
# number_effects = c(10, 5) means 10 main effects and 5 first order interactions are generated
# unique_beta_per_complexity = c(10, 1) means 10 unique regression coefficients for the main effects are generated
# but only 1 regression coefficient for the first-order interactions, which means that all first-order interactions
# have the same regression coefficient



# Define the model name
model = "E-86" # E- refers to the exploratory model and the number corresponds to those given in our paper


# Complexity parameters that can be changed
number_effects = c(10, 10)                  # The number of main effects and interactions structured as follows c(# main effects, # first-order interactions, # second-order interactions, # third order interactions, ......)
unique_beta_per_complexity = c(5, 5)        # The number of unique regression coefficients per depth level, structured as follows c(# number of unique regression coefficients main effects, # number of unique regression coefficients first-order effects, .....)
                                            # Specifying 1 will indicate all regression coefficients are identical at that depth level
complexity_depth = length(number_effects)   # The interaction depth, this can either be specified manually or, as is done here, extract automatically from the number of effects

target_var = 0.10                           # The variation in p(x), minimum = 0 and maximum = 0.25
how_many_continuous = 0                     # The number of continuous covariates, a number that should not exceed number of main effects specified in number_effects
formula_complexity = 1                      # The formula complexity, to indicate if only main effects should be included (=1) or higher order interactions (>=2)




################################################################################
################################################################################
################################################################################
# The remainder of the code should not be changed.
# These are all variables set in line with our simulation study
################################################################################
################################################################################
################################################################################

################################## SIMULATION ##################################

nmax = 50000 # N, the total number of observations
n_obs = sapply(seq(4, 44, by = 2), function(x) x^2) # n, the sample size

number_outcome_repeats = 10 # m, the number of replications

# Methods that were used in the exploratory study
methods = c("logistic", "lasso", "knn", "rf")


# This will create the path in which results are stored
dir_path = file.path("results_final", "Exploratory-analysis", model)
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE) 
  "Folder created"
} 

# Create the file name, which informs about all complexity parameters and date and time
file_name = paste(dir_path, "/E_", format(Sys.time(), "%d-%m-%Y_%H-%M-%S"), "_numeffects=", paste(number_effects, collapse = "_"),
                  "_complexity=", complexity_depth, "_uniquebetas=", paste(unique_beta_per_complexity, collapse = "_"), 
                  "_formulacomplex=", formula_complexity, "_targetvar=", target_var, "_repeats=", number_outcome_repeats, "_continuous=", how_many_continuous,  sep = "")

# Code that initializes the entire simulation
# Run simulation
average_df = average_experiments(seeds = c(1, 2, 3, 4, 5),                                   # The random seeds allocated to the exploratory analysis
                                 number_effects = number_effects, 
                                 complexity_depth = complexity_depth, 
                                 unique_beta_per_complexity = unique_beta_per_complexity, 
                                 formula_complexity = formula_complexity, 
                                 n_obs = n_obs, 
                                 nmax = nmax, 
                                 target_var = target_var, 
                                 number_outcome_repeats = number_outcome_repeats,
                                 how_many_continuous = how_many_continuous, 
                                 methods = methods,
                                 file_name = paste(file_name, ".txt", sep = ""))

# To be able to access the data, the output file with averages is saved
saveRDS(object = average_df, file = paste(file_name, ".Rds", sep = ""))



################################################################################
################################ CREATE FIGURES ################################
################################################################################

# In this section the figures are created which were used in our paper

## Accuracy
average_plot_zoom_accuracy = visualization_obs_zoom_accuracy(average_df = average_df, methods = methods)  # create the base plot

avg_acc = average_plot_zoom_accuracy +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    plot.margin = margin(t = 5.5,
                         r = 7.5,
                         b = 5.5,
                         l = 5.5)
  ) +
  guides(color = guide_legend(
    override.aes = list(size = 1.5),
    title.position = "top",
    ncol = 3,
    byrow = TRUE)
  )

ggsave(filename = paste(file_name, "_accuracy.pdf", sep = ""), plot = avg_acc, width = 5, height = 6, units = "in") # pdf version
ggsave(filename = paste(file_name, "_accuracy.png", sep = ""), plot = avg_acc, width = 5, height = 6, units = "in") # png version



## Sensitivity
average_plot_zoom_specificity = visualization_obs_zoom_specificity(average_df = average_df, methods = methods) # create the base plot


avg_sens = average_plot_zoom_sensitivity +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    plot.margin = margin(t = 5.5,
                         r = 7.5, 
                         b = 5.5,
                         l = 5.5)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5),
      title.position = "top",
      ncol = 3,
      byrow = TRUE)
  )

ggsave(filename = paste(file_name, "_sensitivity.pdf", sep = ""), plot = avg_sens, width = 5, height = 6, units = "in") # pdf version
ggsave(filename = paste(file_name, "_sensitivity.png", sep = ""), plot = avg_sens, width = 5, height = 6, units = "in") # png version

## Specificity
average_plot_zoom_sensitivity = visualization_obs_zoom_sensitivity(average_df = average_df, methods = methods)

avg_spec = average_plot_zoom_specificity +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 10),
    plot.margin = margin(t = 5.5,
                         r = 7.5,  
                         b = 5.5,
                         l = 5.5)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(size = 1.5),
      title.position = "top",
      ncol = 3,
      byrow = TRUE)
  )

ggsave(filename = paste(file_name, "_specificity.pdf", sep = ""), plot = avg_spec, width = 5, height = 6, units = "in") # pdf version
ggsave(filename = paste(file_name, "_specificity.png", sep = ""), plot = avg_spec, width = 5, height = 6, units = "in") # png version





# A combined figure, in which all three figures are put into one figure
combined_plot = avg_acc + avg_sens + avg_spec +
  plot_layout(ncol = 3, nrow = 1, guides = "collect") & theme(legend.position = "bottom")

ggsave(file = paste(file_name, "_combined.pdf", sep = ""), plot = combined_plot, dpi = 100, width = 10, height = 5, units = "in") # pdf version
ggsave(file = paste(file_name, "_combined.png", sep = ""), plot = combined_plot, dpi = 100, width = 10, height = 5, units = "in") # png version



