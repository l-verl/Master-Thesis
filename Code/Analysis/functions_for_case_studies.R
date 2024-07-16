# Load packages
library(tibble)
library(tidyverse)
library(stringr)
library(randomForest)
library(caret)
library(tidyr)
library(glmnet)
library(riskCommunicator)
library(psych)








average_experiments = function(seeds, formula_complexity, n_obs, number_outcome_repeats, scaled, balanced, methods, case_study, file_name) {
  # Function that runs the entire case study for one model
  
  # seeds (vector):                     input should be c(), where the numbers indicate the random seed
  # formula_complexity (numeric):       a number to indicate if only main effects should be included (=1) or higher order interactions (>=2) in the statistical methods (indicates the complexity depth)
  # n_obs (vector):                     input should be c(), where the numbers indicate the number of observations that will be sampled (n)
  # number_outcome_repeats (numeric):   a number that indicates how many replications, m, must be run within one seed. This indicates 
  #                                     the amount of y-vectors that are generated and used to calculate an average performance measure
  # scaled (character):                 a character string that indicates to scale the data if "yes" is input
  # balanced (character):               a character string that indicates whether the data will be balanced
  # methods (vector):                   input should be c(), where the string input are the methods chosen to run 
  #                                     Options (input in function) are: logistic regression (logistic), lasso (lasso), ridge (ridge), 
  #                                     elastic net (elastic), random forest (rf), k-nearest neighbors (knn), support vector machine linear kernel (svm_linear),
  #                                     support vector machine polynomial kernel (svm_poly), support vector machine radial kernel (svm_radial),
  #                                     gradient boosting machine (gbm), and XGBoost (xgboost)
  # case_study (character)              a character string indicating which case study to run (framingham, diabetes, 50k)
  # file_name (character):              a character string that contains the file name where everything will be saved
  
  
  # Output is a data frame containing the average performance measures per method and n_obs
  
  
  # Set up the sink function to save all printed output to a log file
  sink(file_name, type = c("output", "message"), append = TRUE)
  cat("\nStart of experiment run at ", format(Sys.time(), "%d-%m-%Y %H:%M:%S"))
  cat("\nCase Study: ", case_study)
  cat("\nUsing the following information", "\nFormula complexity = ", formula_complexity, 
      "\nn_obs = ", n_obs, "\nnumber_outcome_repeats = ", number_outcome_repeats, "\nscaled = ", scaled, "\nbalanced = ", balanced,
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
          formula_complexity = formula_complexity, 
          n_obs = n_obs, 
          number_outcome_repeats = number_outcome_repeats,
          scaled = scaled, 
          balanced = balanced,
          methods = methods,
          case_study = case_study,
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
  
  # Only extract the columns that match the methods selected
  selected_columns = c("Accuracy_null", "Sensitivity_null", "Specificity_null", sapply(methods, function(m) all_methods_list[[m]], simplify = "character")) %>% unlist()
  
  # Calculate a dataframe in which the averages are calculated from different seeds for each performance measure, method, and n_obs combination
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







run_single_exp = function(seed, formula_complexity, n_obs, number_outcome_repeats, scaled, balanced, methods, case_study, file_name) {
  # Function that runs the case study for the specified seed
  
  # seed (numeric):                     a number indicating the random seed
  # formula_complexity (numeric):       a number to indicate if only main effects should be included (=1) or higher order interactions (>=2) in the statistical methods (indicates the complexity depth)
  # n_obs (vector):                     input should be c(), where the numbers indicate the number of observations that will be sampled (n)
  # number_outcome_repeats (numeric):   a number that indicates how many replications, m, must be run within one seed. This indicates 
  #                                     the amount of y-vectors that are generated and used to calculate an average accuracy
  # scaled (character):                 a character string that indicates to scale the data if "yes" is input
  # balanced (character):               a character string that indicates whether the data will be balanced.
  # methods (vector):                   input should be c(), where the string input are the methods chosen to run 
  #                                     Options (input in function) are: logistic regression (logistic), lasso (lasso), ridge (ridge), 
  #                                     elastic net (elastic), random forest (rf), k-nearest neighbors (knn), support vector machine linear kernel (svm_linear),
  #                                     support vector machine polynomial kernel (svm_poly), support vector machine radial kernel (svm_radial),
  #                                     gradient boosting machine (gbm), and XGBoost (xgboost)
  # case_study (character)              a character string indicating which case study to run (framingham, diabetes, 50k)
  # file_name (character):              a character string that contains the file name where everything will be saved
  
  
  # Output is a data frame containing the performance measures estimates per method (averaged over number replications) and n_obs
  
  
  # Set the seed for reproducibility
  set.seed(seed)
  
  # Extract the right case study data, create formula, and prepare the data
  if (case_study == "framingham") {
    cat("\nProcessing the framingham data... \n")
    
    # Load the framingham data
    data("framingham")
    framingham = framingham %>% 
      select(SEX, TOTCHOL, AGE, SYSBP, DIABP, CURSMOKE, CIGPDAY, BMI,  
             DIABETES, BPMEDS, HEARTRTE, GLUCOSE, educ, PREVCHD, PREVAP, PREVMI, 
             PREVSTRK, PREVHYP, CVD) %>%
      rename(y = CVD)
    
    # Extract the needed formulas for machine learning methods and statistical methods
    formulas = create_formulas(input_data = framingham, formula_complexity = formula_complexity)
    
    # Generate data
    dfs = framingham_data_function(scaled = scaled, balanced = balanced)
  }
  if (case_study == "diabetes") {
    cat("\nProcessing the diabetes data... \n")
    
    # Load the diabetes data
    diabetes = read.csv("case_study_data/diabetes_cleaned.csv")
    
    # Extract the needed formulas for machine learning methods and statistical methods
    formulas = create_formulas(input_data = diabetes, formula_complexity = formula_complexity)
    
    # Generate data
    dfs = diabetes_data_function(scaled = scaled, balanced = balanced)
  }
  if (case_study == "50k") {
    cat("\nProcessing the census income data... \n")
    
    # Load the adult 50k data
    adult_50 = read.csv("case_study_data/adult_cleaned.csv")
    
    # Extract the needed formulas for machine learning methods and statistical methods
    formulas = create_formulas(input_data = adult_50, formula_complexity = formula_complexity)
    
    # Generate data
    dfs = adult_50_data_function(scaled = scaled, balanced = balanced)
    
  }

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
  
  
  # Print the statements about this experiment
  cat("\nThe formula used for (penalized) Logistic Regression = ", formulas$LR)
  cat("\nThe formula used in machine learning methods = ", formulas$ML, "\n")
  
  cat("\nIn the case study data, this is the mean of the total train data set: \n")
  print(mean(as.numeric(as.character(dfs$train_data$y))))
  
  cat("\nIn the case study data, this is the mean of the total test data set: \n")
  print(mean(as.numeric(as.character(dfs$test$y))))
  
  # For loop that runs through each n_obs entry and runs all methods
  for (n in n_obs) {
    
    row_idx = list() # List containing the randomly sampled row indexes for each outcome
    
    # For loop that will go through the number_outcome_repeats and randomly sample rows for each y to be used
    for (col in 1:number_outcome_repeats) {
      
      # Count the number of rows present in the data
      number_rows = nrow(dfs$train_data)
      
      # To make sure randomly sampled rows don't only contain 0's or 1' repeat is used
      # to run until at least 1 observation of each are present in the sample
      repeat {
        idx = sample(number_rows, size = n) # Sample n rows
        
        # Check if the sampled subset contains at least one observation of both 0 and 1
        if (sum(as.numeric(as.character(dfs$train_data$y[idx])) == 0) >= 1 & sum(as.numeric(as.character(dfs$train_data$y[idx])) == 1) >= 1) {
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
      
      xgboost_results = xgboost_function(all_data = dfs, index = row_idx)
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
  # Function that generates the formulas that will be used in the statistical methods and machine learning methods
  
  # input_data (tibble):          the case study data
  # formula_complexity (numeric): a number to indicate if only main effects should be included (=1) or higher order interactions (>=2) in the statistical methods (indicates the complexity depth)
  
  
  # Output is a list containing the formula for statistical methods (LR) and machine learning methods (ML) 
  
  
  # Extract how many main effects are wanted from input_data
  number_main_effects = ncol(input_data) - 1
  
  # Name all possible main effects
  covariates = colnames(input_data)
  covariates = covariates[!covariates %in% c("y")]
  
  # Put the main effects into a formula string
  all_main = paste(covariates, collapse = " + ")
  
  # Define formulas for the methods where the dependent variable is given as a binary factor
  LR_formula = "as.factor(y) ~" # LR is used for statistical methods and can be (penalized) logistic regression
  ML_formula = "as.factor(y) ~"
  
  # Add the main effects to the machine learning formula 
  ML_formula = paste(ML_formula, all_main, sep = " ")
  
  # Create a list that will contain all possible combinations of covariates, i.e., interactions at each complexity level
  effects_per_complexity_depth = list()
  
  # Complexity level 1 are main effects, these are added before the loop
  effects_per_complexity_depth[[1]] = covariates
  
  # If formula complexity was not 1, we extract the formula for the other formula complexity
  if (formula_complexity != 1) {
    
    # Generate all possible combinations of covariates, using complexity_level as guide for how many elements per combination
    possible_int = combn(covariates, formula_complexity, simplify = FALSE)
    
    # Paste the possible combinations together with * such that in a formula both main effects and interactions are used
    possible_comb = sapply(possible_int, function(x) paste(x, collapse = "*"))
    
    # Save all possible combinations to the effects_per_complexity_depth list
    effects_per_complexity_depth[[2]] = possible_comb
    
  }
  
  # The LR formula is formed based on the given formula_complexity
  # This formula complexity corresponds to an element from effects_per_complexity_depth which can be
  # extracted and then added in the formula
  if (formula_complexity == 1) {
    LR_formula = paste(LR_formula, paste(effects_per_complexity_depth[[1]], collapse = " + "))
  } else {
    LR_formula = paste(LR_formula, paste(effects_per_complexity_depth[[2]], collapse = " + "))
  }
  
  return(list(LR = LR_formula, ML = ML_formula))
  
}







framingham_data_function = function(scaled, balanced) {
  # Function that will prepare the Framingham dataset from the riskCommunicator package in R
  
  # scaled (character):                 a character string that indicates to scale the data if "yes" is input
  # balanced (character):               a character string that indicates whether the data will be balanced
  
  # Output is a list containing the training data set and test data set
  
  
  # Import the data from the riskCommunicator package
  data("framingham")
  
  # Select variables of interest from period 1
  data_framingham = framingham %>%
    # Filter period 1
    filter(PERIOD == 1) %>%
    
    # Select only the relevant variables
    select(SEX, TOTCHOL, AGE, SYSBP, DIABP, CURSMOKE, CIGPDAY, BMI,  
           DIABETES, BPMEDS, HEARTRTE, GLUCOSE, educ, PREVCHD, PREVAP, PREVMI, 
           PREVSTRK, PREVHYP, CVD) %>%
    
    # Rename the outcome CVD to y
    rename(y = CVD) %>%
    
    # Use a complete case analysis
    filter(complete.cases(.))
  
  # Put variables as factor
  # Both binary and categorical factors are included (we forgot to include CURSMOKE, DIABETES, BPMEDS)
  numeric_var = c("SEX", "educ", "PREVCHD", 
                  "PREVAP", "PREVMI", "PREVSTRK", "PREVHYP", "y") # CURSMOKE, DIABETES, BPMEDS should also have been added (but was forgotten), it did not make any difference in the analysis as they were binary
  data_framingham[numeric_var] = lapply(data_framingham[numeric_var], as.factor)
  
  # Print the summary function
  print(describe(data_framingham))
  
  cat("\n The dimensions of the data are: ")
  print(dim(data_framingham))
  
  # Condition that will only run if user wants the data scaled, to be more comparable
  # to the standard normal distribution that was used in the simulation 
  if (scaled == "yes") {
    data_framingham = data_framingham %>% mutate_if(is.numeric, ~ as.vector(scale(.)))
  }
  
  print(head(data_framingham))
  
  cat("\nThe mean of y in the entire data is: ", mean(as.numeric(as.character(data_framingham$y))))
  
  # Condition that runs depends on whether the data should be balanced or not
  if (balanced == "yes") {
    # Due to the fact this data is much smaller and it is imbalanced, it is difficult 
    # to create balanced train and test data and some negative data is left out 
    
    positive_framingham = data_framingham %>% filter(y == 1)
    negative_framingham = data_framingham %>% filter(y == 0)
    
    # Match the number of positive cases to a random sample of the same number of negative cases
    balanced_framingham = rbind(
      positive_framingham,
      negative_framingham[sample(x = nrow(negative_framingham),
                                 size = nrow(positive_framingham)), ]
    )
    
    # Create indices for an 80-20 train-test split
    idx = createDataPartition(y = balanced_framingham$y, p = 0.8, list = FALSE)
    
    data_framingham_train = balanced_framingham[idx, ]
    data_framingham_test = balanced_framingham[-idx, ]

    
      
  } else {
    
    idx = createDataPartition(y = data_framingham$y, p = 0.8, list = FALSE)
    data_framingham_train = data_framingham[idx, ]
    data_framingham_test = data_framingham[-idx, ]
  }
  
  cat("\nThe mean of y in the train part of the data is: ", mean(as.numeric(as.character(data_framingham_train$y))))
  cat("\nThe mean of y in the test part of the data is: ", mean(as.numeric(as.character(data_framingham_test$y))))
  
  rownames(data_framingham_train) = 1:nrow(data_framingham_train)
  rownames(data_framingham_test) = 1:nrow(data_framingham_test)
  
  data = list(train_data = data_framingham_train, test = data_framingham_test)
  
  
  return(data)
}




adult_50_data_function = function(scaled, balanced) {
  # Function that will prepare the adult 50k dataset
  
  # scaled (character):                 a character string that indicates to scale the data if "yes" is input
  # balanced (character):               a character string that indicates whether the data will be balanced
  
  # Output is a list containing the training data set and test data set
  
  
  # Import the data
  data_adult_50 = read.csv("case_study_data/adult_cleaned.csv")
  
  # Put variables as factor
  # Both binary and categorical factors are included
  factor_variables = c("y", "sex", "workclass", "education", "marital_status", "occupation", "relationship", "race", "native_country")
  data_adult_50[factor_variables] = lapply(data_adult_50[factor_variables], as.factor)
  
  # Print the summary function
  print(describe(data_adult_50))
  
  cat("\n The dimensions of the data are: ")
  print(dim(data_adult_50))
  
  # Condition that will only run if user wants the data scaled, to be more comparable
  # to the standard normal distribution that was used in the simulation 
  if (scaled == "yes") {
    data_adult_50 = data_adult_50 %>% mutate_if(is.numeric, ~ as.vector(scale(.)))
  }

  
  print(head(data_adult_50))
  
  cat("\nThe mean of y in the entire data is: ", mean(as.numeric(as.character(data_adult_50$y))))
  
  # Condition that runs depends on whether the data should be balanced or not
  if (balanced == "yes") {
    # Due to the fact this data is imbalanced, it is difficult 
    # to create balanced train and test data and some negative data is left out 
    
    positive_data_adult_50 = data_adult_50 %>% filter(y == 1)
    negative_data_adult_50= data_adult_50 %>% filter(y == 0)
    
    # Match the number of positive cases to a random sample of the same number of negative cases
    balanced_adult_50 = rbind(
      positive_data_adult_50,
      negative_data_adult_50[sample(x = nrow(negative_data_adult_50),
                                    size = nrow(positive_data_adult_50)), ]
    )
    
    # Create indices for an 80-20 train-test split
    idx = createDataPartition(y = balanced_adult_50$y, p = 0.8, list = FALSE)
    
    data_adult_50_train = balanced_adult_50[idx, ]
    data_adult_50_test = balanced_adult_50[-idx, ]
    
    
    
  } else {
    
    idx = createDataPartition(y = data_adult_50$y, p = 0.8, list = FALSE)
    data_adult_50_train = data_adult_50[idx, ]
    data_adult_50_test = data_adult_50[-idx, ]
  }
  
  cat("\nThe mean of y in the train part of the data is: ", mean(as.numeric(as.character(data_adult_50_train$y))))
  cat("\nThe mean of y in the test part of the data is: ", mean(as.numeric(as.character(data_adult_50_test$y))))
  
  
  rownames(data_adult_50_train) = 1:nrow(data_adult_50_train)
  rownames(data_adult_50_test) = 1:nrow(data_adult_50_test)
  
  
  data = list(train_data = data_adult_50_train, test = data_adult_50_test)
  
  
  return(data)
}



diabetes_data_function = function(scaled, balanced) {
  # Function that will prepare the diabetes dataset
  
  # scaled (character):                 a character string that indicates to scale the data if "yes" is input
  # balanced (character):               a character string that indicates whether the data will be balanced
  
  # Output is a list containing the training data set and test data set
  
  
  # Import the data
  data_diabetes = read.csv("case_study_data/diabetes_cleaned.csv")
  
  # Put variables as factor
  # Both binary and categorical factors are included
  factors_variables = c("race", "gender", "age", "admission_type_id", "discharge_disposition_id", 
                        "admission_source_id", "max_glu_serum", "A1Cresult", "metformin", 
                        "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide", 
                        "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", 
                        "acarbose", "miglitol", "troglitazone", "tolazamide", 
                        "insulin", "glyburide.metformin", "glipizide.metformin", 
                        "metformin.pioglitazone", "change", "diabetesMed", "y", 
                        "diag_1_group", "diag_2_group", "diag_3_group")
  data_diabetes[factors_variables] = lapply(data_diabetes[factors_variables], as.factor)
  
  # Print the summary function
  print(describe(data_diabetes))
  
  cat("\n The dimensions of the data are: ")
  print(dim(data_diabetes))
  
  # Condition that will only run if user wants the data scaled, to be more comparable
  # to the standard normal distribution that was used in the simulation 
  if (scaled == "yes") {
    data_diabetes = data_diabetes %>% mutate_if(is.numeric, ~ as.vector(scale(.)))
  }
  
  
  print(head(data_diabetes))
  
  cat("\nThe mean of y in the entire data is: ", mean(as.numeric(as.character(data_diabetes$y))))
  
  # Condition that runs depends on whether the data should be balanced or not
  if (balanced == "yes") {
    # Due to the fact this data is imbalanced, it is difficult 
    # to create balanced train and test data and some negative data is left out 
    
    positive_data_diabetes = data_diabetes %>% filter(y == 1)
    negative_data_diabetes = data_diabetes %>% filter(y == 0)
    
    # Match the number of positive cases to a random sample of the same number of negative cases
    balanced_diabetes = rbind(
      positive_data_diabetes,
      negative_data_diabetes[sample(x = nrow(negative_data_diabetes),
                                    size = nrow(positive_data_diabetes)), ]
    )
    
    # Create indices for an 80-20 train-test split
    idx = createDataPartition(y = balanced_diabetes$y, p = 0.8, list = FALSE)
    
    data_diabetes_train = balanced_diabetes[idx, ]
    data_diabetes_test = balanced_diabetes[-idx, ]
    
  } else {
    
    idx = createDataPartition(y = data_diabetes$y, p = 0.8, list = FALSE)
    data_diabetes_train = data_diabetes[idx, ]
    data_diabetes_test = data_diabetes[-idx, ]
  }
  
  cat("\nThe mean of y in the train part of the data is: ", mean(as.numeric(as.character(data_diabetes_train$y))))
  cat("\nThe mean of y in the test part of the data is: ", mean(as.numeric(as.character(data_diabetes_test$y))))
  
  rownames(data_diabetes_train) = 1:nrow(data_diabetes_train)
  rownames(data_diabetes_test) = 1:nrow(data_diabetes_test)
  
  data = list(train_data = data_diabetes_train, test = data_diabetes_test)
  
  
  return(data)
}

















null_model = function(all_data, index) {
  # Function that estimate the null model, i.e. intercept-only model using cross-validation
  
  # all_data (list):  list that contains all data from the case study
  # index (list):     list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the null model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  # Extract the test data set
  test = all_data$test
  test = as.data.frame(cbind(y = test$y, intercept = rep(1, nrow(test))))
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    cat("\nThe mean is: ", mean(as.numeric(as.character(train_data$y))))
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the logistic regression model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  
  # Extract the test data set
  test = all_data$test
  
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the random forest model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  # Extract the test data set
  test = all_data$test
  
  # Extract how many main effects there are
  # Since we use the test data, we subtract 1 for the outcome 
  number_main_effects_rf = ncol(test) - 1
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the knn model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  # Extract the test data set
  test = all_data$test
  
  # Extract how many main effects there are
  number_main_effects_knn = ncol(test) - 1
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the lasso model
  
  
  # Make a variable to save accuracy values
  accuracy = rep(0.5, length(index))
  sensitivity = rep(0.5, length(index))
  specificity = rep(0.5, length(index))
  
  # Define the formula that will be used and leave out the intercept that is created in the model.matrix
  formula_lasso = paste0(formula_LR, " -1")
  
  # Extract the test data set and transform into a model matrix
  test = all_data$test
  test_df = model.matrix(as.formula(formula_lasso), data = test)
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the ridge model
  
  
  # Make a variable to save accuracy values
  accuracy = rep(0.5, length(index))
  sensitivity = rep(0.5, length(index))
  specificity = rep(0.5, length(index))
  
  # Define the formula that will be used
  formula_ridge = paste0(formula_LR, " -1")
  
  # Extract the test data set and transform into a model matrix
  test = all_data$test
  test_df = model.matrix(as.formula(formula_ridge), data = test)
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication 
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the elastic net model
  
  
  # Make a variable to save accuracy values
  accuracy = rep(0.5, length(index))
  sensitivity = rep(0.5, length(index))
  specificity = rep(0.5, length(index))
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication 
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the linear kernel svm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the radial svm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the polynomial kernel svm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  
  # Extract the test data set
  test = all_data$test
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
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
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the gbm model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  
  # Extract the test data set
  test = all_data$test
  
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use sampled rows
    
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









xgboost_function = function(all_data, index) {
  # Function that runs XGBoost using 10-fold cross-validation 
  
  # all_data (list):         list that contains all data from the case study
  # formula_LR (character):  character string that contains the formula the model will use
  # index (list):            list that contains indexes that are used to filter from the data
  
  
  # Output is a list, containing estimates from performance measures, averaged for m replications of the xgboost model
  
  
  # Make a variable to save accuracy values
  accuracy = numeric(length(index))
  sensitivity = numeric(length(index))
  specificity = numeric(length(index))
  
  
  # Extract the test data set and prepare for xgboost
  test = all_data$test
  test_data_x = model.matrix(y ~ . -1, data = test)
  test_data_y = test$y
  
  
  # For loop that will run over all outcomes (number_outcome_repeats)
  for (b in 1:length(index)) {
    
    # Extract the sampled rows for the bth replication
    row_idx = index[[b]]
    
    # Extract the data frame of bth replication
    train_data = all_data$train_data
    train_data = train_data[row_idx, ] # Only use on sampled rows
    
    # The case study data can contain categorical variables with more than 2 categories
    # which means all categories need to be dummy coded (excluding intercept). 
    # This was not the case in the simulation study, as the data was simulated as
    # numeric (and if categorical, was only 2 classes)
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
  # It zooms in on range [0.50, 1]
  
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
