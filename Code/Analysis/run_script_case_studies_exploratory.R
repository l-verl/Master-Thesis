# Load packages
library(ragg)
library(patchwork)

# Load functions needed
source("functions_for_case_studies.R")


################################################################################
################################# INSTRUCTIONS #################################
################################################################################
# To run the exploratory analysis on the case studies, only two parameters need to be changed.

formula_complexity = 1    # The formula complexity, to indicate if only main effects should be included (=1) or higher order interactions (>=2)
case_study = "framingham" # The case study to run (Options: framingham, 50k, diabetes)






################################################################################
################################################################################
################################################################################
# The remainder of the code should not be changed.
# These are all variables set in line with our simulation study
################################################################################
################################################################################
################################################################################


number_outcome_repeats = 10
methods = c("logistic", "lasso", "knn", "rf")
scaled = "yes" # we do not vary this
balanced = "yes" # we do not vary this

# Create the folder in which the results will be saved
dir_path = file.path("results_case_studies", case_study)
if (!dir.exists(dir_path)) {
  dir.create(dir_path, recursive = TRUE) 
  "Folder created"
} 


if (case_study == "framingham") {
  if (balanced == "yes") {
    # If balanced data is required, there are less observations available in the test data (max. 1618)
    n_obs = sapply(seq(4, 40, by = 2), function(x) x^2)
  } else {
    n_obs = sapply(seq(4, 44, by = 2), function(x) x^2)
  }
} else {
  n_obs = sapply(seq(4, 44, by = 2), function(x) x^2)
}


file_name = paste("results_case_studies/", case_study, "/Run_", format(Sys.time(), "%d-%m-%Y_%H-%M-%S"), 
                  "_formulacomplex=", formula_complexity, "_repeats=", number_outcome_repeats, "_scaled=", scaled, "_balanced=", balanced,  sep = "")

average_df = average_experiments(seeds = c(9, 10, 11, 12, 13), 
                                 formula_complexity = formula_complexity, 
                                 n_obs = n_obs, 
                                 number_outcome_repeats = number_outcome_repeats, 
                                 scaled = scaled, 
                                 balanced = balanced,
                                 methods = methods, 
                                 case_study = case_study,
                                 file_name = paste(file_name, ".txt", sep = ""))


# Save the data under same name as the log file
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
                         r = 7.5,  # 22 for depth
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



