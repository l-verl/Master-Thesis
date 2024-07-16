# Master Thesis

This repository contains supplementary material for my master thesis.

## Contents
 The repository contains the following:
 - Code
 - Figures


### Code
The code is divided into separate files for clarity.

We provide some general instructions to make it work.

**Exploratory Analysis**  
To run the exploratory analysis, the following documents are needed:

- The script that will run the whole analysis: [run_script_exploratory.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Analysis/run_script_exploratory.R)
- The script containing the underlying functions used: [functions_final.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Analysis/functions_final.R), this should not be changed. This contains all functions used.


**In-Depth Analysis**  
To run the in-depth analysis, the following documents are needed:

- The script that will run the whole analysis: [run_script_in_depth.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Analysis/run_script_in_depth.R)
- The script containing the underlying functions used: [functions_final.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Analysis/functions_final.R), this should not be changed. This contains all functions used.

**Case Studies**  
To run the case studies, the following documents are needed:

- The script that will run the whole exploratory analysis: [run_script_case_studies_exploratory.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Case_Studies/run_script_case_studies_exploratory.R) or in-depth analysis: [run_script_case_studies_depth.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Case_Studies/run_script_case_studies_depth.R)
- The script containing the underlying functions used: [functions_for_case_studies.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Case_Studies/functions_for_case_studies.R), this should not be changed. This contains all functions used.

*The case study data is not included, but is available in the sources described. The code used to run the analysis on the case studies is included. Additionally, we do include the steps we took during pre-processing for the diabetes and census income dataset in [preprocessing_case_studies.Rmd](https://github.com/l-verl/Master-Thesis/blob/main/Code/Case_Studies/preprocessing_case_studies.Rmd), while for the Framingham study this is located in the [functions_for_case_studies.R](https://github.com/l-verl/Master-Thesis/blob/main/Code/Case_Studies/functions_for_case_studies.R) file.*

*We should also note that during the pre-processing of the Framingham data, not all binary covariates were recoded to factors. However, since they were binary covariates, this had no effect.*


### Figures

The complete results are documented in figures. They have been divided into folders for each analysis:
- [Exploratory Analysis](https://github.com/l-verl/Master-Thesis/tree/main/Figures/Exploratory%20Analysis)
- [In-Depth Analysis](https://github.com/l-verl/Master-Thesis/tree/main/Figures/In-Depth%20Analysis)
- [Case Study](https://github.com/l-verl/Master-Thesis/tree/main/Figures/Case%20Studies)
    - [Framingham](https://github.com/l-verl/Master-Thesis/tree/main/Figures/Case%20Studies/Framingham)
    - [Diabetes](https://github.com/l-verl/Master-Thesis/tree/main/Figures/Case%20Studies/Diabetes)
    - [Census Income](https://github.com/l-verl/Master-Thesis/tree/main/Figures/Case%20Studies/Census%20Income%20(50k))

