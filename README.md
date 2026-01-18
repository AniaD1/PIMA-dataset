# PRIMA

## Overview
This project analyses the Pima Indian Diabetes Dataset to predict diabetes status, addressing the problem of a high level of questionable data entries. Many clinical variables in the dataset contain biologically implausible zero values, which are treated as missing for the purpose of this analysis and addressed using Multiple Imputation by Chained Equations (MICE).

## Structure of the analysis
* Exploratory data analysis and identification of implausible zero values
* Visualization of missingness patterns and co-occurrence
* Assessment of missing data mechanisms (MCAR rejected; MAR plausible)
* Multiple imputation using MICE with predictive mean matching
* Predictive modeling using:Logistic Regression (GLM), Random Forest (RF)
* Model evaluation using ROC curves and AUC

## Dataset
* Source: Kaggle (Smith et al., 1988)
* Population: Female patients ≥21 years old of Pima Indian heritage
* Outcome: Diabetes status (Outcome)

## Main Packages
mice, randomForest, naniar, visdat, ggplot2, tidyverse, pROC

## Files included
* PRIMA_dataset.csv - orginal PRIMA dataset obtaied from Kaggle
* PRIMA_imputation.Rmd - RMarkdown file with the code for analysis with extended description
* Prima_imputation_code.R - R code with minimal description

## Reference
Smith, J.W. et al. (1988). Using the ADAP learning algorithm to forecast the onset of diabetes mellitus.
