# Load packages
library(tibble)
library(tidyverse)
library(stringr)
library(randomForest)
library(caret)
library(tidyr)
library(glmnet)




generate_input_data = function(number_effects, complexity_depth, unique_beta_per_complexity) {
  # Function that puts the complexity parameters in a workable dataframe
  
  # number_effects (vector):              input should be c() that gives the number of main effects and interactions structured 
  #                                       as follows c(# main effects, # first-order interactions, # second-order interactions, # third order interactions, ......)
  # complexity_depth (numeric):           number indicating the interaction depth of the data, matching the entries in number_effects
  # unique_beta_per_complexity (vector):  input should be c() that gives the number of unique regression coefficients per depth level, structured 
  #                                       as follows c(# number of unique regression coefficients main effects, # number of unique regression coefficients first-order effects, .....)
  
  # Output is a tibble containing the complexity parameters used as input data
  
  
  
  # Check whether the given parameters match what their input should be
  if (length(number_effects) != complexity_depth | length(unique_beta_per_complexity) != complexity_depth) {
    stop("The number(s) you have given for the number_effects and/or unique_beta_per_complexity might not correspond with the complexity depth")
  }
  
  # Create the tibble containing all data generation information
  trial_data = tibble(
    Complexity_level = 1:complexity_depth,
    number_covariates = number_effects,
    beta_coeff_per_depth = vector("list", length = complexity_depth)
  )
  
  # Loop that generates the regression coefficients
  for (b in 1:complexity_depth) {
    
    # Generate regression coefficients from the uniform distribution and fill 
    # out a vector containing as many coefficients as covariates
    beta_list = rep(runif(unique_beta_per_complexity[b]), length.out = trial_data$number_covariates[[b]])
    trial_data$beta_coeff_per_depth[[b]] = beta_list
  }
  
  return(trial_data)
}








average_experiments = function(seeds, number_effects, complexity_depth, unique_beta_per_complexity, formula_complexity, n_obs, nmax, target_var, number_outcome_repeats, how_many_continuous, methods, file_name) {
  # Function that runs the entire simulation experiment for one model
  
  # seeds (vector):                      input should be c(), where the numbers indicate the random seed
  # number_effects (vector):             input should be c() that gives the number of main effects and interactions structured 
  #                                      as follows c(# main effects, # first-order interactions, # second-order interactions, # third order interactions, ......)
  # complexity_depth (numeric):          number indicating the interaction depth of the data, matching the entries in number_effects
  # unique_beta_per_complexity (vector): input should be c() that gives the number of unique regression coefficients per depth level, structured 
  #                                      as follows c(# number of unique regression coefficients main effects, # number of unique regression coefficients first-order effects, .....)
  # formula_complexity (numeric):        a number to indicate if only main effects should be included (=1) or higher order interactions (>=2) in the statistical methods (indicates the complexity depth)
  # n_obs (vector):                      input should be c(), where the numbers indicate the number of observations that will be sampled (n)
  # nmax (numeric):                      a number indicating the total number of observations (N) that is used to generate the data frame used
  #                                      This is different from n_obs, which samples from this parameter
  # target_var (numeric):                a number that indicates how much variance can be present in p(x) for the generated Y data
  # number_outcome_repeats (numeric):    a number that indicates how many replications, m, must be run within one seed. This indicates 
  #                                      the amount of y-vectors that are generated and used to calculate an average performance measure
  # how_many_continuous (numeric):       number of continuous covariates
  # methods (vector):                    input should be c(), where the string input are the methods chosen to run 
  #                                      Options (input in function) are: logistic regression (logistic), lasso (lasso), ridge (ridge), 
  #                                      elastic net (elastic), random forest (rf), k-nearest neighbors (knn), support vector machine linear kernel (svm_linear),
  #                                      support vector machine polynomial kernel (svm_poly), support vector machine radial kernel (svm_radial),
  #                                      gradient boosting machine (gbm), and XGBoost (xgboost)
  # file_name (character):               a character string that contains the file name where everything will be saved
  
  
  # Output is a data frame containing the average performance measures per method and n_obs
  
  
  # Set up the sink function to save all printed output to a log file
  sink(file_name, type = c("output", "message"), append = TRUE)
  cat("\nStart of experiment run at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
  cat("\nUsing the following information", "\nFormula complexity = ", formula_complexity, 
      "\nn_obs = ", n_obs, "\nnmax = ", nmax, '\ntarget_var = ', target_var, "\nnumber_outcome_repeats = ", number_outcome_repeats,
      "\nnumber of continuous covariates = ", how_many_continuous, 
      "\nselected methods = ", methods)
  
  # To calculate the performance for different seeds lapply is used to save all results in a list
  # Averaged results are returned for all three performance measures
  results_from_all_experiments = lapply(
    X = seeds, 
    FUN = 
      function(x) {
        cat("\nStart of experiment, using seed = ", x, "\n")
        run_single_exp(
          seed = x, 
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
          file_name = file_name
        )
      }
  )
  
  # Add the nested lists for different random seeds together in one data frame
  results_from_all_experiments_df = do.call(rbind, results_from_all_experiments)
  
  # Save all results in the log file
  cat("\nResults are \n")
  
  print(results_from_all_experiments_df)
  
  
  # List of all possible methods to calculate 
  all_methods_list = list(
    logistic = c("Accuracy_log", "Sensitivity_log", "Specificity_log"),
    lasso = c("Accuracy_lasso", "Sensitivity_lasso", "Specificity_lasso"),
    ridge = c("Accuracy_ridge", "Sensitivity_ridge", "Specificity_ridge"),
    elastic = c("Accuracy_elastic", "Sensitivity_elastic", "Specificity_elastic"),
    rf = c("Accuracy_rf", "Sensitivity_rf", "Specificity_rf"),
    knn = c("Accuracy_knn", "Sensitivity_knn", "Specificity_knn"), 
    svm_linear = c("Accuracy_svm_linear", "Sensitivity_svm_linear", "Specificity_svm_linear"),
    svm_radial = c("Accuracy_svm_radial", "Sensitivity_svm_radial", "Specificity_svm_radial"),
    svm_poly = c("Accuracy_svm_poly", "Sensitivity_svm_poly", "Specificity_svm_poly"),
    gbm = c("Accuracy_gbm", "Sensitivity_gbm", "Specificity_gbm"),
    xgboost = c("Accuracy_xgboost", "Sensitivity_xgboost", "Specificity_xgboost")
  )
  
  # Only extract the columns that match the methods selected (different for exploratory and in-depth analysis)
  selected_columns = c("Accuracy_null", "Sensitivity_null", "Specificity_null", sapply(methods, function(m) all_methods_list[[m]], simplify = "character")) %>% unlist()
  
  # Generate a dataframe in which the averages are calculated from different seeds for each performance measure, method, and n_obs combination
  # Averages are grouped by observations
  average_df = results_from_all_experiments_df %>%
    group_by(Observations) %>%
    summarize_at(vars(selected_columns),
                 funs(mean = mean))
  
  cat("\nThis results in the following dataframe with average accuracies\n")
  print(as.data.frame(average_df))
  
  sink() 
  
  return(average_df)
  
}







run_single_exp = function(seed, number_effects, complexity_depth, unique_beta_per_complexity, formula_complexity, n_obs, nmax, target_var, number_outcome_repeats, how_many_continuous, methods, file_name) {
  # Function that runs the simulation for the specified seed
  
  # seed (numeric):                      a number indicating the random seed
  # number_effects (vector):             input should be c() that gives the number of main effects and interactions structured 
  #                                      as follows c(# main effects, # first-order interactions, # second-order interactions, # third order interactions, ......)
  # complexity_depth (numeric):          number indicating the interaction depth of the data, matching the entries in number_effects
  # unique_beta_per_complexity (vector): input should be c() that gives the number of unique regression coefficients per depth level, structured 
  #                                      as follows c(# number of unique regression coefficients main effects, # number of unique regression coefficients first-order effects, .....)
  
  # formula_complexity (numeric):        a number to indicate if only main effects should be included (=1) or higher order interactions (>=2) in the statistical methods (indicates the complexity depth)
  # n_obs (vector):                      input should be c(), where the numbers indicate the number of observations that will be sampled (n)
  # nmax (numeric):                      a number indicating the total number of observations (N) that is used to generate the data frame used
  #                                      This is different from n_obs, which samples from this parameter
  # target_var (numeric):                a number that indicates how much variance can be present in p(x) for the generated Y data
  # number_outcome_repeats (numeric):    a number that indicates how many replications, m, must be run within one seed. This indicates 
  #                                      the amount of y-vectors that are generated and used to calculate an average performance measure
  # how_many_continuous (numeric):       number of continuous covariates
  # methods (vector):                    input should be c(), where the string input are the methods chosen to run
  #                                      Options (input in function) are: logistic regression (logistic), lasso (lasso), ridge (ridge), 
  #                                      elastic net (elastic), random forest (rf), k-nearest neighbors (knn), support vector machine linear kernel (svm_linear),
  #                                      support vector machine polynomial kernel (svm_poly), support vector machine radial kernel (svm_radial),
  #                                      gradient boosting machine (gbm), and XGBoost (xgboost)
  # file_name (character):               a character string that contains the file name where everything will be saved
  
  # Output is a data frame containing the performance measures estimates per method (averaged over number replications) and n_obs
  
  
  # Set the seed for reproducibility
  set.seed(seed)
  
  # Generate the input data for this seed
  input_data = generate_input_data(number_effects = number_effects, complexity_depth = complexity_depth, unique_beta_per_complexity = unique_beta_per_complexity)
  
  cat("\nUsing the following data input: \n")
  print(input_data %>%  unnest(beta_coeff_per_depth) %>% as.data.frame())
  
  # Extract the needed formulas for data generation, machine learning methods, and statistical methods
  formulas = create_formulas(input_data = input_data, formula_complexity = formula_complexity)
  
  # Generate the simulation data
  dfs = generate_data(input_data = input_data, nmax = nmax, formulas = formulas, target_var = target_var, number_outcome_repeats = number_outcome_repeats, how_many_continuous = how_many_continuous)
  
  # Set up the result data frame
  exp_results_df = data.frame(Observations = numeric(), Seed = numeric(), Accuracy_null = numeric(), Sensitivity_null = numeric(), Specificity_null = numeric())
  
  # Assign names to chosen methods for column names
  available_methods = list(
    logistic = c("Accuracy_log", "Sensitivity_log", "Specificity_log"),
    lasso = c("Accuracy_lasso", "Sensitivity_lasso", "Specificity_lasso"),
    ridge = c("Accuracy_ridge", "Sensitivity_ridge", "Specificity_ridge"),
    elastic = c("Accuracy_elastic", "Sensitivity_elastic", "Specificity_elastic"),
    rf = c("Accuracy_rf", "Sensitivity_rf", "Specificity_rf"),
    knn = c("Accuracy_knn", "Sensitivity_knn", "Specificity_knn"), 
    svm_linear = c("Accuracy_svm_linear", "Sensitivity_svm_linear", "Specificity_svm_linear"),
    svm_radial = c("Accuracy_svm_radial", "Sensitivity_svm_radial", "Specificity_svm_radial"),
    svm_poly = c("Accuracy_svm_poly", "Sensitivity_svm_poly", "Specificity_svm_poly"),
    gbm = c("Accuracy_gbm", "Sensitivity_gbm", "Specificity_gbm"),
    xgboost = c("Accuracy_xgboost", "Sensitivity_xgboost", "Specificity_xgboost")
  )
  
  
  # Extract the columns of methods that need to be added
  evaluation_chr = character()
  for (m in methods) {
    evaluation_chr = c(evaluation_chr, available_methods[[m]])

  }
  
  # Add the columns for all methods selected to the results data frame
  for (c in evaluation_chr) {
    exp_results_df[c] = numeric()
  }
  print(exp_results_df)
  
  
  
  # Print the statements about this experiment
  cat("\nThe formula used for (penalized) Logistic Regression = ", formulas$LR)
  cat("\nThe formula used in machine learning methods = ", formulas$ML, "\n")
  
  cat("\nIn the simulated data, these are the column means of the outcomes: \n")
  print(colMeans(dfs$Y_data))
  
  # For loop that runs through each n_obs entry and run all methods
  for (n in n_obs) {
    
    row_idx = list() # List containing the randomly sampled row indexes for each outcome
    
    # For loop that will go through Y_data and randomly sample n rows for each y to be used
    # Y_data contains y's, generated based on number_outcome_repeats
    for (col in 1:ncol(dfs$Y_data)) {
      
      # To make sure randomly sampled rows don't only contain 0's or 1' repeat is used
      # to run until at least 1 observation of each are present in the sample
      repeat {
        idx = sample(nrow(dfs$Y_data), size = n) # Sample n rows
        
        # Check if the sampled subset contains at least one observations of both 0 and 1
        if (sum(dfs$Y_data[idx, col] == 0) >= 1 & sum(dfs$Y_data[idx, col] == 1) >= 1) {
          row_idx[[col]] = idx
          break
        }
      }
      
    }
    
    
    # Add the first entries to the list that contains information true for all methods
    exp_results_df_methods = list(Observations = n, Seed = seed)
    
    cat("\nSeed = ", seed, " at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"), " and observations = ", n)
    
    
    # Run the null model which serves as the baseline
    cat("\nStart of Null Model at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
    null_model_results = null_model(all_data = dfs, index = row_idx)
    exp_results_df_methods[["Accuracy_null"]] = null_model_results$Accuracy
    exp_results_df_methods[["Sensitivity_null"]] = null_model_results$Sensitivity
    exp_results_df_methods[["Specificity_null"]] = null_model_results$Specificity
    
    cat("\nEnd of Null Model at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
    cat("\n")
    
    
    if ("logistic" %in% methods) {
      
      # Run logistic model
      cat("\nStart of Logistic Regression at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      log_results = logistic(all_data = dfs, formula_LR = formulas$LR, index = row_idx)
      exp_results_df_methods[["Accuracy_log"]] = log_results$Accuracy
      exp_results_df_methods[["Sensitivity_log"]] = log_results$Sensitivity
      exp_results_df_methods[["Specificity_log"]] = log_results$Specificity
      
      cat("\nEnd of Logistic Regression at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
    }
    if ("lasso" %in% methods) {
      
      # Run lasso model
      cat("\nStart of Lasso at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      lasso_results = lasso(all_data = dfs, formula_LR = formulas$LR, index = row_idx)
      exp_results_df_methods[["Accuracy_lasso"]] = lasso_results$Accuracy
      exp_results_df_methods[["Sensitivity_lasso"]] = lasso_results$Sensitivity
      exp_results_df_methods[["Specificity_lasso"]] = lasso_results$Specificity
      
      cat("\nEnd of Lasso at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
      
    }
    if ("ridge" %in% methods) {
      
      # Run ridge model
      cat("\nStart of Ridge at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      ridge_results = ridge(all_data = dfs, formula_LR = formulas$LR, index = row_idx)
      exp_results_df_methods[["Accuracy_ridge"]] = ridge_results$Accuracy
      exp_results_df_methods[["Sensitivity_ridge"]] = ridge_results$Sensitivity
      exp_results_df_methods[["Specificity_ridge"]] = ridge_results$Specificity
      
      cat("\nEnd of Ridge at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
      
    }
    if ("elastic" %in% methods) {
      
      # Run elastic net model
      cat("\nStart of Elastic Net at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      elastic_results = elastic_net(all_data = dfs, formula_LR = formulas$LR, index = row_idx)
      exp_results_df_methods[["Accuracy_elastic"]] = elastic_results$Accuracy
      exp_results_df_methods[["Sensitivity_elastic"]] = elastic_results$Sensitivity
      exp_results_df_methods[["Specificity_elastic"]] = elastic_results$Specificity
      
      cat("\nEnd of Elastic Net at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
      
    }
    if ("rf" %in% methods) {
      
      # Run random forest model
      cat("\nStart of Random Forest at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      rf_results = random_forest(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_rf"]] = rf_results$Accuracy
      exp_results_df_methods[["Sensitivity_rf"]] = rf_results$Sensitivity
      exp_results_df_methods[["Specificity_rf"]] = rf_results$Specificity
      
      cat("\nEnd of Random Forest at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
      
    }
    if ("knn" %in% methods) {
      
      # Run knn model
      cat("\nStart of kNN at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      knn_results = knn_function(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_knn"]] = knn_results$Accuracy
      exp_results_df_methods[["Sensitivity_knn"]] = knn_results$Sensitivity
      exp_results_df_methods[["Specificity_knn"]] = knn_results$Specificity
      
      cat("\nEnd of kNN at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
      
    }
    if ("svm_linear" %in% methods) {
      
      # Run linear support vector machine
      cat("\nStart of linear SVM at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      svm_linear_results = svm_linear_function(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_svm_linear"]] = svm_linear_results$Accuracy
      exp_results_df_methods[["Sensitivity_svm_linear"]] = svm_linear_results$Sensitivity
      exp_results_df_methods[["Specificity_svm_linear"]] = svm_linear_results$Specificity
      
      cat("\nEnd of linear SVM at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
    }
    if ("svm_radial" %in% methods) {
      
      # Run radial support vector machine
      cat("\nStart of radial SVM at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      svm_radial_results = svm_radial_function(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_svm_radial"]] = svm_radial_results$Accuracy
      exp_results_df_methods[["Sensitivity_svm_radial"]] = svm_radial_results$Sensitivity
      exp_results_df_methods[["Specificity_svm_radial"]] = svm_radial_results$Specificity
      
      cat("\nEnd of radial SVM at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
    }
    if ("svm_poly" %in% methods) {
      
      # Run polynomial support vector machine
      cat("\nStart of polynomial SVM at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      svm_poly_results = svm_polynomial_function(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_svm_poly"]] = svm_poly_results$Accuracy
      exp_results_df_methods[["Sensitivity_svm_poly"]] = svm_poly_results$Sensitivity
      exp_results_df_methods[["Specificity_svm_poly"]] = svm_poly_results$Specificity
      
      cat("\nEnd of polynomial SVM at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
    }
    if ("gbm" %in% methods) {
      
      # Run gbm
      cat("\nStart of gbm at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      gbm_results = gbm_function(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_gbm"]] = gbm_results$Accuracy
      exp_results_df_methods[["Sensitivity_gbm"]] = gbm_results$Sensitivity
      exp_results_df_methods[["Specificity_gbm"]] = gbm_results$Specificity
      
      cat("\nEnd of gbm at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
    }
    if ("xgboost" %in% methods) {
      
      # Run xgboost
      cat("\nStart of xgboost at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      
      xgboost_results = xgboost_function(all_data = dfs, formula_ML = formulas$ML, index = row_idx)
      exp_results_df_methods[["Accuracy_xgboost"]] = xgboost_results$Accuracy
      exp_results_df_methods[["Sensitivity_xgboost"]] = xgboost_results$Sensitivity
      exp_results_df_methods[["Specificity_xgboost"]] = xgboost_results$Specificity
      
      cat("\nEnd of xgboost at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
      cat("\n")
    }
    
    # Add the results of all chosen methods to the results data frame
    exp_results_df = rbind(exp_results_df, data.frame(exp_results_df_methods))
    
  }
  
  return(exp_results_df)
}








create_formulas = function(input_data, formula_complexity) {
  # Function that generates the formulas that will be used in the data generating process, statistical methods, and machine learning methods
  
  # input_data (tibble):          the input data for the experiment
  # formula_complexity (numeric): a number to indicate if only main effects should be included (=1) or higher order interactions (>=2) in the statistical methods (indicates the complexity depth)
  
  
  # Output is a list containing the formula for statistical methods (LR), machine learning methods (ML) and for the data generating process (data_generation_formula) 
  
  
  # Extract how many main effects are wanted from input_data
  number_main_effects = as.numeric(input_data[input_data$Complexity_level == 1, "number_covariates"])
  
  # Name all possible main effects
  covariates = paste0("x", 1:number_main_effects) 
  
  # Put the main effects into a formula string
  all_main = paste(covariates, collapse = " + ")
  
  # Define formulas for the methods where the dependent variable is given as a binary factor
  LR_formula = "as.factor(y) ~" # LR is used for statistical methods and can be (penalized) logistic regression
  ML_formula = "as.factor(y) ~"
  
  # Add the main effects to the machine learning formula 
  ML_formula = paste(ML_formula, all_main, sep = " ")
  
  # Define the basis for the data generation formula which are main effects
  data_formula = all_main 
  
  # Create a list that will contain all possible combinations of covariates, i.e., interactions at each complexity level
  effects_per_complexity_depth = list()
  
  # Complexity level 1 are main effects, these are added before the loop
  effects_per_complexity_depth[[1]] = covariates
  
  c = 2 # Index for effects_per_complexity_depth in the loop
  
  # For loop that iterates over each complexity level from input_data to find
  # the possible combinations and sample the effects used to generate the data
  for (row in 1:nrow(input_data)) {
    
    # The loop is only run for complexity depth other than main effects
    if (input_data$Complexity_level[row] != 1) {
      # Will generate all possible combinations of complexities and save these to the list
      
      # Generate all possible combinations of covariates, using complexity_level as guide for how many elements per combination
      possible_int = combn(covariates, input_data$Complexity_level[row], simplify = FALSE)
      
      # Paste the possible combinations together with * such that in a formula both main effects and interactions are used
      possible_comb = sapply(possible_int, function(x) paste(x, collapse = "*"))
      
      # Save all possible combinations to the effects_per_complexity_depth list
      effects_per_complexity_depth[[c]] = possible_comb
      c = c + 1 # Add 1 to the index such that in the next loop a new element in effects_per_complexity_depth is made
      
      # To effectively generate the data it is specified how many effects per complexity level are
      # used to generate the data. Here those effects are randomly sampled and added to the formula
      # for data generation
      which_int_effect = sample(possible_comb, input_data$number_covariates[row])
      all_int = paste(which_int_effect, collapse = " + ") # add interaction effects together
      data_formula = paste(data_formula, all_int, sep = " + ")
    }
  }
  
  # The LR formula is normally formed based on the given formula_complexity, which corresponds to a given input from 
  # all possible combinations that we generated for effects_per_complexity_depth
  # These can be extracted if formula complexity <= complexity_depth
  # If this is not the case, we need to generate additional combinations for the higher formula complexity
  if (formula_complexity > max(input_data$Complexity_level)) {
    
    cat("\nThe formula complexity is given as ", formula_complexity, "while the highest complexity depth is ", max(input_data$Complexity_level), "\n")
    
    # Generate all possible combinations of covariates, using the formula complexity as guide for how many elements per combination
    possible_int = combn(covariates, formula_complexity, simplify = FALSE)
    
    # Paste the possible combinations together with * such that in a formula both main effects and interactions are used
    possible_comb = sapply(possible_int, function(x) paste(x, collapse = "*"))
    
    # Form the LR formula
    LR_formula = paste(LR_formula, paste(possible_comb, collapse = " + "))
    
  } else {
    # Extract the effects from effects_per_complexity_depth if formula_complexity <= complexity_depth
    LR_formula = paste(LR_formula, paste(effects_per_complexity_depth[[formula_complexity]], collapse = " + "))
  }
  
  return(list(LR = LR_formula, ML = ML_formula, data_generation_formula = data_formula))
  
}








generate_data = function(input_data, nmax, formulas, target_var, number_outcome_repeats, how_many_continuous) {
  # Function that generates the simulation data. 
  # The X data (covariates) is only generated once, while different y's are 
  # generated based on the number given by number_outcome_repeats

  
  # input_data (tibble):                contains the complexity parameters used as input data
  # nmax (numeric):                     a number indicating the total number of observations (N) that is used to generate the data frame used
  #                                     This is different from n_obs, which samples from this parameter
  # formulas (list):                    a list that contains the formulas for the statistical methods, machine learning methods, and data generating process
  # target_var (numeric):               a number that indicates how much variance can be present in p(x) for the generated Y data
  # number_outcome_repeats (numeric):   a number that indicates how many replications, m, must be run within one seed. This indicates 
  #                                     the amount of y-vectors that are generated and used to calculate an average performance measure
  # how_many_continuous (numeric):      number of continuous covariates
  
  
  # Output is a list: data frame X_data which contains generated covariates, a data frame Y_data which contains generated outcomes, and a data frame 
  # test which contains generated test data (both X and y)
  
  
  # Extract how many main effects are required from input_data
  number_main_effects = as.numeric(input_data[input_data$Complexity_level == 1, "number_covariates"])
  
  # Extract the generated regression coefficients from input_data                             
  betas = input_data %>% unnest(beta_coeff_per_depth) %>% select(beta_coeff_per_depth)
  
  
  #############################################################################
  ######################  DATA GENERATION (TRAINING SET) ######################
  #############################################################################
  
  ### FIXED PART ###
  # In this part a fixed data set X is generated. This is done based on parameter nmax and the 
  # number of main effects that is required extracted from input_data
  
  
  # Generate an empty data frame 
  X = as.data.frame(matrix(ncol = number_main_effects, nrow = nmax))
  colnames(X) = paste0("x", 1:number_main_effects)
  
  #### FIXED PART: MAIN EFFECTS ####
  # In this part the data is generated and added to the empty data frame X.
  # Only the main effects are generated, as interaction effects are derived from these
  # Note: the data is generated using a binomial distribution, subsequently
  # 0's are changed into -1 in order for data to become more balanced once 
  # complexity increases
  
  # Generate main effects
  for (x in 1:number_main_effects) {
    
    # Calculate continuos covariates with standard normal distribution
    if (x <= how_many_continuous) {
      covariate_x = rnorm(n = nmax, mean = 0, sd = 1)
    } else {
      # Each variable x is generated from a binomial distribution, where the covariate is balanced
      # by using prob = 0.5
      covariate_x = rbinom(n = nmax, size = 1, prob = 0.5)
      
      # The generated 0's are replaced by -1's
      covariate_x = ifelse(covariate_x == 0, -1, 1)
    }
    
    # Add the generated covariate to the dataframe X
    X[x] = covariate_x
  }
  
  cat("\nA small piece of the generated data X")
  print(head(X))
  
  # Make a new data frame where interactions will be added
  X_interaction = X
  
  # Extract the formula to generate the data
  all_effects = formulas$data_generation_formula
  
  cat("\nThe data is generated using the following formula: as.factor(y) ~ ", all_effects)
  
  # Extract from the formula only the interaction effects (leave out main effects)
  which_interaction_effect = str_extract_all(all_effects, "\\b\\w+(\\*\\w+)+\\b")[[1]]
  
  #### FIXED PART: INTERACTION EFFECTS ####
  # In this part a loop will calculate the interaction effects specified in the data generation formula
  # Interactions are calculated at this stage in order to calculate probabilities needed for the generation of Y
  
  # A condition that checks whether there were any interaction effects extracted from the data generation formula
  # If no interactions are found, the loop can be skipped entirely
  if (!(is_empty(which_interaction_effect))) {
    
    # For loop that will iterate over all interaction effects specified in the data generation formula
    for (interaction_name in which_interaction_effect) {
      
      # Calculate the interaction based on the data generated in X
      interaction_calc = eval(parse(text = interaction_name), envir = X)
      
      # To not confuse any methods replace the * by _ in the interaction name
      interaction_name = str_replace_all(                  
        string = interaction_name,
        pattern = "[*]", 
        replacement = "_"
      )
      
      # Add the calculated interaction as a new column to X_interaction
      X_interaction[[interaction_name]] = interaction_calc
      
    }
  }
  
  #### FIXED PART: BISECTION ALGORITHM ####
  # In this part the bisection algorithm is used to scale the regression coefficients in such a way that 
  # the variance of the logit_pattern, used for generating Y
  # is close to the target_var specified 
  
  # Run bisection algorithm to find the constant that will result in a logit_pattern with target_var as variance
  k = bisection(f = f, a = 0, b = 1000, var = target_var, data = X_interaction, betas = betas)
  
  # Calculate the linear predictor with the final constant
  linear_pred = as.matrix(X_interaction) %*% (k * as.matrix(betas))
  
  # Transform the log-odds from the linear predictor into probabilities
  logit_pattern = plogis(linear_pred)
  
  cat("\nThe Bisection Algorithm found the following constant k = ", k, "\nThis constant will make sure the target variance is = ", target_var, "\n")
  
  ### RANDOM PART ###
  # In this part some randomness is added to the data. This is done by
  # generating outcomes (y)'s for each observation, as many as specified by number_outcome_repeats, i.e., replications m
  
  # To make sure the models can run, a condition is set that no generated y can contain only one 
  # level, instead each y should contain at least one 0 and 1, n_distinct  makes sure that both classes are present
  Y = as.data.frame(
    replicate(n = number_outcome_repeats, 
              expr = 
                {
                  repeat{
                    y = rbinom(n = nrow(X), size = 1, prob = logit_pattern)
                    
                    if (n_distinct(y) > 1) {
                      return(y)
                    }
                  }
                }
    )
  )
  colnames(Y) = paste0("y", 1:number_outcome_repeats)
  
  
  #############################################################################
  #################################  TEST SET #################################
  #############################################################################
  
  ### TEST SET ###
  # In this part a test data set is generated. This is done according to the 
  # same principle as generating the training data
  
  
  #### TEST SET: FIXED PART ####
  # In this part the test data is generated and added to the empty data frame X_test.
  # Only the main effects are generated, as interaction effects are derived from these
  # Note: the data is generated using a binomial distribution, subsequently
  # 0's are changed into -1 in order for data to become more balanced once complexity increases
  
  # Generate an empty data frame 
  X_test = as.data.frame(matrix(ncol = number_main_effects, nrow = 2000))
  colnames(X_test) = paste0("x", 1:number_main_effects)
  
  # Generate main effects
  for (t in 1:number_main_effects) {
    
    # Calculate continuos covariates with standard normal distribution
    if (t <= how_many_continuous) {
      covariate_t = rnorm(n = 2000, mean = 0, sd = 1)
    } else {
      # Each variable t is generated from a binomial distribution, where the covariate is balanced
      # by using prob = 0.5
      covariate_t = rbinom(n = 2000, size = 1, prob = 0.5) 
      
      # The generated 0's are replaced by -1's
      covariate_t = ifelse(covariate_t == 0, -1, 1)
    }
    
    # Add the generated covariate to the dataframe X
    X_test[t] = covariate_t
  }
  
  
  # Make a new data frame where interactions will be added
  X_test_interaction = X_test
  
  #### TEST SET - FIXED PART: INTERACTION EFFECTS ####
  # In this part a loop will calculate the interaction effects in the test data set as specified in the data generation formula
  # Interactions are calculated at this stage to calculate probabilities needed for the generation of Y_test
  
  # A condition that checks whether there were any interaction effects extracted from the data generation formula
  # If no interactions are found, the loop can be skipped entirely
  if (!(is_empty(which_interaction_effect))) {
    
    # For loop that will iterate over all interaction effects specified in the data generation formula
    for (interaction_name in which_interaction_effect) {
      
      # Calculate the interaction based on the data generated in X_test
      interaction_calc = eval(parse(text = interaction_name), envir = X_test)
      
      # To not confuse any methods replace the * by _ in the interaction name
      interaction_name = str_replace_all(                  
        string = interaction_name,
        pattern = "[*]", 
        replacement = "_"
      )
      
      # Add the calculated interaction as a new column to X_test_interaction
      X_test_interaction[[interaction_name]] = interaction_calc
      
    }
  }
  
  
  #### TEST SET: FIXED PART - BISECTION ALGORITHM ####
  # In this part the bisection algorithm is used to scale the regression coefficients in 
  # such a way that the variance of the logit_pattern, used for generating Y_test
  # is close to the target_var specified 
  
  # Run bisection algorithm to find the constant that will result in a logit_pattern with target_var as variance
  k = bisection(f = f, a = 0, b = 1000, var = target_var, data = X_test_interaction, betas = betas)
  
  # Calculate the linear predictor with the final constant
  linear_pred_test = as.matrix(X_test_interaction) %*% (k * as.matrix(betas))
  
  # Transform the log-odds from the linear predictor into probabilities
  logit_pattern_test = plogis(linear_pred_test)
  
  cat("\nThe Bisection Algorithm for the test data found the following constant k = ", k, "\nThis constant will make sure the target variance is = ", target_var)
  
  #### TEST SET: RANDOM PART ####
  # In the test data set only one y is randomly generated for each observation.
  
  
  # To make sure the models can run, a condition is set that no generated y can contain only one 
  # level, instead each y should contain at least one 0 and 1
  Y_test = as.data.frame(
    replicate(n = 1, 
              expr = 
                {
                  repeat{
                    y = rbinom(n = nrow(X_test), size = 1, prob = logit_pattern_test)
                    
                    if (n_distinct(y) > 1) {
                      return(y)
                    }
                  }
                }
    )
  )
  colnames(Y_test) = c("y")
  
  # Make one test data frame
  test_df = cbind(X_test, Y_test)
  
  # Store all generated data in a nested list
  data = list(X_data = X, Y_data = Y, test = test_df)
  
  
  return(data)
}









f = function(k, data, betas, var) {
  # Function that calculates the following formula:
  # 1/n \sum_{n = 1}^n p * (1 - p) - var 
  # for the bisection method function
  
  # k (number):         a number representing the constant k in the bisection algorithm
  # data (dataframe):   a data frame that contains the generated covariates including the interaction terms
  # betas (tibble):     a tibble that contains the regression coefficients generated for input_data
  # var (numeric):      a number specifying the specified variance the probabilities can have
  
  
  # Output is of class numeric, the outcome of the formula
  
  
  # Calculate the linear predictor with the current k (constant) from the bisection algorithm
  linear_pred = as.matrix(data) %*% as.matrix(k * betas)
  
  # Calculate the probabilities from the log_odds in linear_pred
  logit_pattern = plogis(linear_pred)
  
  # Calculate the variance each observation has, according to the binomial variance formula
  binomial_variance = logit_pattern * (1 - logit_pattern) 
  
  # Calculate the average variance
  avg_var = mean(binomial_variance) 
  
  # Calculate the formula needed
  return_c = avg_var - var
  
  return(return_c)
}









bisection = function(f, a, b, var, data, betas) {
  # Function that is tasked with running the bisection algorithm. The goal is to find a 
  # value for constant k that will result in a certain variance in the probabilities.
  # The formula used in the bisection method to find the root is:
  # 1/n \sum_{n = 1}^n p * (1 - p) - var = average variance - var
  
  # f (function):                 a function that is used to calculate for the bisection method
  # a (numeric):                  a number specifying the lower bound of the bisection algorithm, i.e., this is the lowest value the constant can take
  # b (numeric):                  a number specifying the upper bound of the bisection algorithm, i.e., this is the highest value the constant can take
  # var (numeric):                a number specifying the specified variance the probabilities can have
  # data (dataframe):             a data frame that contains the generated covariates including the interaction terms
  # betas (tibble):               a tibble that contains the regression coefficients generated for input_data
  
  
  # Output is of class numeric, specifying the constant k. 
  
  
  # Specifying some starting values
  avg_var = Inf  # Extreme value representing the outcome of the formula (average variance - var)
  tol = 0.001    # The tolerance, i.e., the largest difference between the var and the actual variance that we have deemed acceptable
  
  # While loop that will run until the output value of f is no longer larger than the tolerance value
  while (abs(avg_var) > tol) {
    
    # Calculate constant c
    c = (a + b) / 2 
    
    # Calculate the value from the formula in the function
    avg_var = f(k = c, data = data, betas = betas, var = var)
    
    # Check whether there is no difference anymore, i.e., the root is found
    if (avg_var == 0) {
      return(c)
    }
    else if (f(k = a, data = data, betas = betas, var = var) * f(k = c, data = data, betas = betas, var = var) < 0) {
      b = c
    }
    else if (f(k = a, data = data, betas = betas, var = var) * f(k = c, data = data, betas = betas, var = var) > 0) {
      a = c
    }
  }
  
  return(c)
}










null_model = function(all_data, index) {
  # Function that estimate the null model, i.e. intercept-only model using cross-validation
  
  # all_data (list):  list that contains all data generated: X_data, Y_data, and test data
  # index (list):     list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the null model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  test = as.data.frame(cbind(y = test$y, intercept = rep(1, nrow(test))))
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ] # Only use sampled rows
    
    cat("\nThe proportion of Y in the train data = ", mean(train_data$y))
    
    # Run 10-fold cross validation, this is done by using only the train_data y
    # and introducing an intercept of 1
    train_data = as.data.frame(cbind(y = train_data$y, intercept = rep(1, nrow(train_data))))
    
    null_model = train(form = as.factor(y) ~ intercept,
                       data = train_data,
                       method = "glm",
                       family = binomial,
                       trControl = trainControl(method = "cv", number = 10))
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(null_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
}









logistic = function(all_data, formula_LR, index) {
  # Function that runs a logistic regression using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the logistic regression model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  
  # Extract the test data set
  test = all_data$test
  
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ] # Only use sampled rows
    
    
    # Run a binary logistic regression model with 10-fold cross validation
    log_model = train(form = as.formula(formula_LR),
                      data = train_data,
                      method = "glm",
                      family = "binomial",
                      trControl = trainControl(method = "cv", number = 10))
    
    # Print the final model
    cat("\nThe trained model is the following: ")
    print(log_model$finalModel)
    cat("\n")
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(log_model, newdata = test)
    
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









random_forest = function(all_data, formula_ML, index) {
  # Function that runs random forest using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the random forest model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  # Extract how many main effects there are
  number_main_effects_rf = ncol(all_data$X_data)
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    # Define the grid used for tuning the model
    # First max_try is defined which uses the number of main effects that are included in the formula
    # to determine the size of the grid
    max_mtry = min(max(1, number_main_effects_rf), floor(sqrt(number_main_effects_rf))) 
    grid = expand.grid(mtry = 1:max_mtry)
    
    cat("\nUsing a grid parameter mtry is tuned, where the grid is: \n")
    print(grid)
    
    
    # Run a random forest model with 10-fold cross validation
    rf_model = train(form = as.formula(formula_ML),
                     data = train_data,
                     method = "rf",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = grid)
    cat("\nBy tuning the following mtry parameter is found to be optimal: \n")
    print(rf_model$bestTune)
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(rf_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
    
    
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









knn_function = function(all_data, formula_ML, index) {
  # Function that runs k-nearest neighbors using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the knn model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  # Extract how many main effects there are
  number_main_effects_knn = ncol(all_data$X_data)
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    # Define the grid used for tuning the model
    # First max_k is defined which uses the number of main effects that are included in the formula
    # to determine the size of the grid
    max_k = min(15, number_main_effects_knn)
    grid = expand.grid(k = 1:max_k)
    
    cat("\nUsing a grid parameter k is tuned, where the grid is: \n")
    print(grid)
    
    
    # Run a knn model with 10-fold cross validation
    knn_model = train(form = as.formula(formula_ML),
                      data = train_data,
                      method = "knn",
                      trControl = trainControl(method = "cv", number = 10),
                      preProcess = c("center", "scale"), 
                      tuneGrid = grid)
    cat("\nBy tuning the following k parameter is found to be optimal: \n")
    print(knn_model$bestTune)
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(knn_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
    
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
}









lasso = function(all_data, formula_LR, index) {
  # Function that runs a lasso model using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the lasso model
  
  
  # Make a variable to save accuracy values
  accuracy = rep(0.5, ncol(all_data$Y_data))
  sensitivity = rep(0.5, ncol(all_data$Y_data))
  specificity = rep(0.5, ncol(all_data$Y_data))
  
  # Define the formula that will be used and leave out the intercept that is created in the model.matrix
  formula_lasso = paste0(formula_LR, " -1")
  
  # Extract the test data set and transform into a model matrix
  test = all_data$test
  test_df = model.matrix(as.formula(formula_lasso), data = test)
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    # Define a new X matrix (excluding intercept) for the lasso model and extract the y_df
    X_df = model.matrix(as.formula(formula_lasso), data = train_data)
    y_df = as.factor(train_data$y)
    
    # To avoid running into errors this method is written within a tryCatch function
    # that will try to run the model and make predictions, but if this 
    # cannot be done within the loop a baseline accuracy will be provided
    tryCatch({
      
      # Run a lasso model with 10-fold cross validation
      lasso_model = cv.glmnet(x = X_df, y = y_df, alpha = 1, family = "binomial", type.measure = "class")
      
      cat("\nThe optimal tuning parameter lambda is found to be = ", lasso_model$lambda.min, "\n") 
      
      # Use the trained model to make predictions for new data (test)
      preds = predict(lasso_model, s = "lambda.min", newx = test_df, type = "class")
      
      # Get estimates from performance measures from the confusion matrix function
      cf_matrix = confusionMatrix(data = as.factor(preds),
                                  reference = as.factor(test$y), positive = "1")
      
      print(cf_matrix)
      
      accuracy[b] = cf_matrix$overall['Accuracy']
      sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
      specificity[b] = cf_matrix$byClass[["Specificity"]]
      cat("\n")
      print(cf_matrix$table)
      
      
    }, error = function(e) {
      print(paste("Error occurred at b = ", b, "and obs ", print(nrow(X_df)), "The error: ", e))
      
      # Assign baseline estimate as value, which is already done at the start
    }
    )
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









ridge = function(all_data, formula_LR, index) {
  # Function that runs a ridge model using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the ridge model
  
  
  # Make a variable to save accuracy values
  accuracy = rep(0.5, ncol(all_data$Y_data))
  sensitivity = rep(0.5, ncol(all_data$Y_data))
  specificity = rep(0.5, ncol(all_data$Y_data))
  
  # Define the formula that will be used
  formula_ridge = paste0(formula_LR, " -1")
  
  # Extract the test data set and transform into a model matrix
  test = all_data$test
  test_df = model.matrix(as.formula(formula_ridge), data = test)
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication 
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    # Define a new X matrix (excluding intercept) for the ridge model and extract the y_df
    X_df = model.matrix(as.formula(formula_ridge), data = train_data)
    y_df = as.factor(train_data$y)
    
    # To avoid running into errors this method is written within a tryCatch function
    # that will try to run the model and make predictions, but if this 
    # cannot be done within the loop a baseline accuracy will be provided
    tryCatch({
      
      # Run a ridge model with 10-fold cross validation
      ridge_model = cv.glmnet(x = X_df, y = y_df, alpha = 0, family = "binomial", type.measure = "class")
      
      cat("\nThe optimal tuning parameter lambda is found to be = ", ridge_model$lambda.min, "\n") 
      
      # Use the trained model to make predictions for new data (test)
      preds = predict(ridge_model, s = "lambda.min", newx = test_df, type = "class")
      
      # Get estimates from performance measures from the confusion matrix function
      cf_matrix = confusionMatrix(data = as.factor(preds),
                                  reference = as.factor(test$y), positive = "1")
      
      print(cf_matrix)
      
      accuracy[b] = cf_matrix$overall['Accuracy']
      sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
      specificity[b] = cf_matrix$byClass[["Specificity"]]
      cat("\n")
      print(cf_matrix$table)
      
      
    }, error = function(e) {
      print(paste("Error occurred at b = ", b, "and obs ", print(nrow(X_df)), "The error: ", e))
      
      # Assign baseline estimate as value, which is already done at the start
    }
    )
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









elastic_net = function(all_data, formula_LR, index) {
  # Function that runs an elastic net model using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the elastic net model
  
  
  # Make a variable to save accuracy values
  accuracy = rep(0.5, ncol(all_data$Y_data))
  sensitivity = rep(0.5, ncol(all_data$Y_data))
  specificity = rep(0.5, ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication 
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    
    # To avoid running into errors this method is written within a tryCatch function
    # that will try to run the model and make predictions, but if this 
    # cannot be done within the loop a baseline accuracy will be provided
    tryCatch({
      
      # Run an elastic net model with 10-fold cross validation
      elastic_model = train(form = as.formula(formula_LR),
                            data = train_data,
                            method = "glmnet",
                            trControl = trainControl(method = "cv", number = 10),
                            tuneLength = 20)
      
      cat("\nThe optimal tuning parameter alpha = ", elastic_model$bestTune$alpha, " and lambda = ", elastic_model$bestTune$lambda, "\n") 
      
      # Use the trained model to make predictions for new data (test)
      preds = predict(elastic_model, newdata = test, type = "raw")
      
      # Get estimates from performance measures from the confusion matrix function
      cf_matrix = confusionMatrix(data = as.factor(preds),
                                  reference = as.factor(test$y), positive = "1")
      
      print(cf_matrix)
      
      accuracy[b] = cf_matrix$overall['Accuracy']
      sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
      specificity[b] = cf_matrix$byClass[["Specificity"]]
      cat("\n")
      print(cf_matrix$table)
      
      
    }, error = function(e) {
      print(paste("Error occurred at b = ", b, "and obs ", print(nrow(train_data)), "The error: ", e))
      
      # Assign baseline estimate as value, which is already done at the start
      
    }
    )
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b])  
    
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









svm_linear_function = function(all_data, formula_ML, index) {
  # Function that runs a linear SVM using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the linear kernel svm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    
    cat("\nUsing a random search parameter C is tuned")
    
    
    # Run a linear svm model with 10-fold cross validation
    svm_linear_model = train(form = as.formula(formula_ML),
                             data = train_data,
                             method = "svmLinear",
                             trControl = trainControl(method = "cv", number = 10, search = "random"),
                             tuneLength = 15,
                             preProcess = c("center", "scale"))
    
    cat("\nThe optimal tuning parameter C = ", svm_linear_model$bestTune$C) 
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(svm_linear_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









svm_radial_function = function(all_data, formula_ML, index) {
  # Function that runs a radial SVM using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the radial svm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    cat("\nUsing a random search parameters C and sigma are tuned")
    
    
    # Run a radial svm model with 10-fold cross validation
    svm_radial_model = train(form = as.formula(formula_ML),
                             data = train_data,
                             method = "svmRadial",
                             trControl = trainControl(method = "cv", number = 10, search = "random"),
                             tuneLength = 15,
                             preProcess = c("center", "scale")) 
    
    cat("\nThe optimal tuning parameter sigma = ", svm_radial_model$bestTune$sigma, " and C = ", svm_radial_model$bestTune$C) 
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(svm_radial_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









svm_polynomial_function = function(all_data, formula_ML, index) {
  # Function that runs a polynomial SVM using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the polynomial kernel svm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    cat("\nUsing a random search parameters C, scale, and degree are tuned")
    
    
    # Run a polynomial svm model with 10-fold cross validation
    svm_poly_model = train(form = as.formula(formula_ML),
                           data = train_data,
                           method = "svmPoly",
                           trControl = trainControl(method = "cv", number = 10, search = "random"),
                           tuneLength = 15,
                           preProcess = c("center", "scale"))

    cat("\nThe optimal tuning parameter scale = ", svm_poly_model$bestTune$scale, ", C = ", svm_poly_model$bestTune$C, "and degree = ", svm_poly_model$bestTune$degree) 
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(svm_poly_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









gbm_function = function(all_data, formula_ML, index) {
  # Function that runs gradient boosting machine using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the gbm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    
    # Define the grid used for tuning the model
    grid = expand.grid(
      n.trees = c(50, 100, 200, 300, 400, 500, 1000),
      interaction.depth = 1:4,
      shrinkage = c(0.005, 0.001, 0.05, 0.01, 0.1),
      n.minobsinnode = 1
    )
    
    cat("\nUsing a grid, parameters n.trees, interaction.depth, shrinkage, n.minobsinnode are tuned")
    
    
    # Run a gbm model with 10-fold cross validation
    gbm_model = train(form = as.formula(formula_ML), verbose = FALSE,
                      data = train_data,
                      method = "gbm",
                      trControl = trainControl(method = "cv", number = 10),
                      tuneGrid = grid)
    cat("\nOptimal tuning parameters found are n.trees = ", gbm_model$bestTune$n.trees, "interaction.depth = ", gbm_model$bestTune$interaction.depth, "shrinkage = ", gbm_model$bestTune$shrinkage, "n.minobsinnode = ", gbm_model$bestTune$n.minobsinnode)
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(gbm_model, newdata = test)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test$y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
    
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}









xgboost_function = function(all_data, formula_ML, index) {
  # Function that runs XGBoost using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data generated: X_data, Y_data, and test data
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the xgboost model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(ncol(all_data$Y_data))
  sensitivity = numeric(ncol(all_data$Y_data))
  specificity = numeric(ncol(all_data$Y_data))
  
  # Extract the test data set
  test = all_data$test
  test_data_x = model.matrix(y ~ . -1, data = test)
  test_data_y = test$y
  
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:ncol(all_data$Y_data)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the X data frame and  Y data frame of bth replication
    train_data = cbind(all_data$X_data, y = all_data$Y_data[, b])
    train_data = train_data[row_idx, ]
    train_data_x = model.matrix(y ~ . -1, data = train_data)
    train_data_y = train_data$y
    
    print(head(train_data_x))
    
    cat("\nUsing a random search parameters n.trees, interaction.depth, shrinkage, n.minobsinnode are tuned")
    
    
    # Run a xgboost model with 10-fold cross validation
    xgboost_model = train(
      x = train_data_x, y = as.factor(train_data_y), verbose = 0,
      method = "xgbTree",
      tuneLength = 15,
      trControl = trainControl(method = "cv", number = 10, search = "random"))

    cat("\nOptimal tuning parameters found are nrounds = ", xgboost_model$bestTune$nrounds, "max_depth = ", xgboost_model$bestTune$max_depth, "eta = ", xgboost_model$bestTune$eta, "gamma = ", xgboost_model$bestTune$gamma, "colsample_bytree = ", xgboost_model$bestTune$colsample_bytree, "min_child_weight = ", xgboost_model$bestTune$min_child_weight, "subsample = ", xgboost_model$bestTune$subsample)
    
    # Use the trained model to make predictions for new data (test)
    preds = predict(xgboost_model, newdata = test_data_x)
    
    # Get estimates from performance measures from the confusion matrix function
    cf_matrix = confusionMatrix(data = as.factor(preds),
                                reference = as.factor(test_data_y), positive = "1")
    
    accuracy[b] = cf_matrix$overall['Accuracy']
    sensitivity[b] = cf_matrix$byClass[["Sensitivity"]]
    specificity[b] = cf_matrix$byClass[["Specificity"]]
    cat("\n")
    print(cf_matrix$table)
    cat("\nDataset ", b, " Accuracy = ", accuracy[b], " Sensitivity = ", sensitivity[b], " Specificity = ", specificity[b]) 
    
  }
  cat("\nAll accuracies = ", accuracy)
  cat("\nAll sensitivities = ", sensitivity)
  cat("\nAll specificities = ", specificity)
  
  
  return(list(Accuracy = mean(accuracy), Sensitivity = mean(sensitivity), Specificity = mean(specificity)))
  
}
































visualization_obs_zoom_accuracy = function(average_df, methods) {
  # Function that will visualize the accuracies output by average_experiments
  # It zooms in on range [0.5, 1]
  
  # average_df (dataframe): dataframe containing the accuracies per method and observations
  # methods (character):    character vector containing chosen methods
  
  
  # Output is a ggplot figure plotting the accuracies per method and observation
  
  
  methods_in_plot = c(
    "logistic" = "Logistic Regression",
    "lasso" = "LASSO Regression",
    "ridge" = "Ridge Regression",
    "elastic" = "Elastic Net Regression",
    "rf" = "Random Forest",
    "knn" = "KNN",
    "svm_linear" = "Linear SVM",
    "svm_radial" = "Radial SVM",
    "svm_poly" = "Polynomial SVM",
    "gbm" = "GBM",
    "xgboost" = "XGBoost"
  )
  
  
  # Define the colors for each method
  colors_per_method = c(
    "Null model" = "#565656",
    "Logistic Regression" = "#1F78C8",
    "LASSO Regression" = "#ff0000",
    "Ridge Regression" = "#33a02c",  
    "Elastic Net Regression" = "#fb9a99",  
    "Random Forest" = "#ff7f00",
    "KNN" = "#00FF00",
    "Linear SVM" = "#a6cee3",  
    "Radial SVM" = "#b15928",  
    "Polynomial SVM" = "#cab2d6", 
    "GBM" = "#6a3d9a",
    "XGBoost" = "#ff6ec7" 
  )
  
  # Define the order of the legend
  levels_order = c(
    "Null model",
    "Logistic Regression",
    "LASSO Regression",
    "Ridge Regression",
    "Elastic Net Regression",
    "Random Forest",
    "KNN",
    "Linear SVM",
    "Radial SVM",
    "Polynomial SVM",
    "GBM",
    "XGBoost"
  )
  
  # Extract methods used
  methods_2 = methods_in_plot[methods]
  
  # Set up the base plot which includes the null model that is always included
  p = ggplot(data = average_df) +
    geom_line(aes(x = Observations, y = Accuracy_null_mean, color = "Null model"), linewidth = 0.45, alpha = 0.5) + 
    geom_point(aes(x = Observations, y = Accuracy_null_mean, color = "Null model"),  size = 0.3, alpha = 0.5)
  
  
  
  if ("logistic" %in% methods) {
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_log_mean, color = "Logistic Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_log_mean, color = "Logistic Regression"),  size = 0.3, alpha = 0.5)
  }
  
  if ("lasso" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_lasso_mean, color = "LASSO Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_lasso_mean, color = "LASSO Regression"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("ridge" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_ridge_mean, color = "Ridge Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_ridge_mean, color = "Ridge Regression"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("elastic" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_elastic_mean, color = "Elastic Net Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_elastic_mean, color = "Elastic Net Regression"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("rf" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_rf_mean, color = "Random Forest"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_rf_mean, color = "Random Forest"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("knn" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_knn_mean, color = "KNN"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_knn_mean, color = "KNN"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_linear" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_svm_linear_mean, color = "Linear SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_svm_linear_mean, color = "Linear SVM"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_radial" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_svm_radial_mean, color = "Radial SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_svm_radial_mean, color = "Radial SVM"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_poly" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_svm_poly_mean, color = "Polynomial SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_svm_poly_mean, color = "Polynomial SVM"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("gbm" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_gbm_mean, color = "GBM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_gbm_mean, color = "GBM"),  size = 0.3, alpha = 0.5)
    
  }
  
  if ("xgboost" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Accuracy_xgboost_mean, color = "XGBoost"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Accuracy_xgboost_mean, color = "XGBoost"), size = 0.3, alpha = 0.5)
    
  }
  
  
  p = p +
    ylim(0.5, 1) +
    scale_color_manual(values = colors_per_method, limits = c("Null model", levels_order[levels_order %in% methods_2])) +
    theme_minimal() + 
    theme(plot.caption = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10)) + 
    labs(color = "Method", y = "Accuracy")
  
  return(p)
  
}















visualization_obs_zoom_sensitivity = function(average_df, methods) {
  # Function that will visualize the sensitivity output by average_experiments
  # It zooms in on range [0.50, 1]
  
  # average_df (dataframe): dataframe containing the sensitivity estimates per method and observations
  # methods (character):    character vector containing chosen methods
  
  
  # Output is a ggplot figure plotting the sensitivity estimates per method and observation
  
  
  methods_in_plot = c(
    "logistic" = "Logistic Regression",
    "lasso" = "LASSO Regression",
    "ridge" = "Ridge Regression",
    "elastic" = "Elastic Net Regression",
    "rf" = "Random Forest",
    "knn" = "KNN",
    "svm_linear" = "Linear SVM",
    "svm_radial" = "Radial SVM",
    "svm_poly" = "Polynomial SVM",
    "gbm" = "GBM",
    "xgboost" = "XGBoost"
  )
  
  
  # Define the colors for each method
  colors_per_method = c(
    "Null model" = "#565656",
    "Logistic Regression" = "#1F78C8",
    "LASSO Regression" = "#ff0000",
    "Ridge Regression" = "#33a02c",  
    "Elastic Net Regression" = "#fb9a99",  
    "Random Forest" = "#ff7f00",
    "KNN" = "#00FF00",
    "Linear SVM" = "#a6cee3",  
    "Radial SVM" = "#b15928",  
    "Polynomial SVM" = "#cab2d6", 
    "GBM" = "#6a3d9a",
    "XGBoost" = "#ff6ec7" 
  )
  
  # Define the order of the legend
  levels_order = c(
    "Null model",
    "Logistic Regression",
    "LASSO Regression",
    "Ridge Regression",
    "Elastic Net Regression",
    "Random Forest",
    "KNN",
    "Linear SVM",
    "Radial SVM",
    "Polynomial SVM",
    "GBM",
    "XGBoost"
  )
  
  # Extract methods used
  methods_2 = methods_in_plot[methods]
  
  
  
  # Set up the base plot which includes the null model that is always included
  p = ggplot(data = average_df) +
    geom_line(aes(x = Observations, y = Sensitivity_null_mean, color = "Null model"), linewidth = 0.45, alpha = 0.5) + 
    geom_point(aes(x = Observations, y = Sensitivity_null_mean, color = "Null model"), size = 0.3, alpha = 0.5)
  
  
  
  if ("logistic" %in% methods) {
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_log_mean, color = "Logistic Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_log_mean, color = "Logistic Regression"), size = 0.3, alpha = 0.5)
  }
  
  if ("lasso" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_lasso_mean, color = "LASSO Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_lasso_mean, color = "LASSO Regression"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("ridge" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_ridge_mean, color = "Ridge Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_ridge_mean, color = "Ridge Regression"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("elastic" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_elastic_mean, color = "Elastic Net Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_elastic_mean, color = "Elastic Net Regression"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("rf" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_rf_mean, color = "Random Forest"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_rf_mean, color = "Random Forest"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("knn" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_knn_mean, color = "KNN"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_knn_mean, color = "KNN"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_linear" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_svm_linear_mean, color = "Linear SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_svm_linear_mean, color = "Linear SVM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_radial" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_svm_radial_mean, color = "Radial SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_svm_radial_mean, color = "Radial SVM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_poly" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_svm_poly_mean, color = "Polynomial SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_svm_poly_mean, color = "Polynomial SVM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("gbm" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_gbm_mean, color = "GBM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_gbm_mean, color = "GBM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("xgboost" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Sensitivity_xgboost_mean, color = "XGBoost"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Sensitivity_xgboost_mean, color = "XGBoost"), size = 0.3, alpha = 0.5)
    
  }
  
  
  p = p +
    ylim(0.50, 1) +
    scale_color_manual(values = colors_per_method, limits = c("Null model", levels_order[levels_order %in% methods_2])) +
    theme_minimal() + 
    theme(plot.caption = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10)) + 
    labs(color = "Method", y = "Sensitivity")
  
  return(p)
  
}







visualization_obs_zoom_specificity = function(average_df, methods) {
  # Function that will visualize the specificity output by average_experiments
  # It zooms in on range [0.50, 1]
  
  # average_df (dataframe): dataframe containing the specificity estimates per method and observations
  # methods (character):    character vector containing chosen methods
  
  
  # Output is a ggplot figure plotting the specificity estimates per method and observation
  
  
  methods_in_plot = c(
    "logistic" = "Logistic Regression",
    "lasso" = "LASSO Regression",
    "ridge" = "Ridge Regression",
    "elastic" = "Elastic Net Regression",
    "rf" = "Random Forest",
    "knn" = "KNN",
    "svm_linear" = "Linear SVM",
    "svm_radial" = "Radial SVM",
    "svm_poly" = "Polynomial SVM",
    "gbm" = "GBM",
    "xgboost" = "XGBoost"
  )
  
  
  # Define the colors for each method
  colors_per_method = c(
    "Null model" = "#565656",
    "Logistic Regression" = "#1F78C8",
    "LASSO Regression" = "#ff0000",
    "Ridge Regression" = "#33a02c",  
    "Elastic Net Regression" = "#fb9a99",  
    "Random Forest" = "#ff7f00",
    "KNN" = "#00FF00",
    "Linear SVM" = "#a6cee3",  
    "Radial SVM" = "#b15928",  
    "Polynomial SVM" = "#cab2d6", 
    "GBM" = "#6a3d9a",
    "XGBoost" = "#ff6ec7" 
  )
  
  # Define the order of the legend
  levels_order = c(
    "Null model",
    "Logistic Regression",
    "LASSO Regression",
    "Ridge Regression",
    "Elastic Net Regression",
    "Random Forest",
    "KNN",
    "Linear SVM",
    "Radial SVM",
    "Polynomial SVM",
    "GBM",
    "XGBoost"
  )
  
  # Extract methods used
  methods_2 = methods_in_plot[methods]
  
  
  
  # Set up the base plot which includes the null model that is always included
  p = ggplot(data = average_df) +
    geom_line(aes(x = Observations, y = Specificity_null_mean, color = "Null model"), linewidth = 0.45, alpha = 0.5) + 
    geom_point(aes(x = Observations, y = Specificity_null_mean, color = "Null model"), size = 0.3, alpha = 0.5) 
  
  
  
  if ("logistic" %in% methods) {
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_log_mean, color = "Logistic Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_log_mean, color = "Logistic Regression"), size = 0.3, alpha = 0.5)
  }
  
  if ("lasso" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_lasso_mean, color = "LASSO Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_lasso_mean, color = "LASSO Regression"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("ridge" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_ridge_mean, color = "Ridge Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_ridge_mean, color = "Ridge Regression"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("elastic" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_elastic_mean, color = "Elastic Net Regression"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_elastic_mean, color = "Elastic Net Regression"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("rf" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_rf_mean, color = "Random Forest"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_rf_mean, color = "Random Forest"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("knn" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_knn_mean, color = "KNN"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_knn_mean, color = "KNN"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_linear" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_svm_linear_mean, color = "Linear SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_svm_linear_mean, color = "Linear SVM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_radial" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_svm_radial_mean, color = "Radial SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_svm_radial_mean, color = "Radial SVM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("svm_poly" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_svm_poly_mean, color = "Polynomial SVM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_svm_poly_mean, color = "Polynomial SVM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("gbm" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_gbm_mean, color = "GBM"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_gbm_mean, color = "GBM"), size = 0.3, alpha = 0.5)
    
  }
  
  if ("xgboost" %in% methods) {
    
    p = p + 
      geom_line(aes(x = Observations, y = Specificity_xgboost_mean, color = "XGBoost"), linewidth = 0.45, alpha = 0.5) +
      geom_point(aes(x = Observations, y = Specificity_xgboost_mean, color = "XGBoost"), size = 0.3, alpha = 0.5)
    
  }
  
  
  p = p +
    ylim(0.50, 1) +
    scale_color_manual(values = colors_per_method, limits = c("Null model", levels_order[levels_order %in% methods_2])) +
    theme_minimal() + 
    theme(plot.caption = element_text(size = 10),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10)) + 
    labs(color = "Method", y = "Specificity")
  
  return(p)
  
}
