
# BMI Prediction Using Blood Pressure — Linear Regression in R

## Overview

This project explores the relationship between Body Mass Index (BMI) and blood pressure using statistical analysis and linear regression models in R.

The main objective is to investigate whether systolic and diastolic blood pressure can help explain BMI variations and evaluate the performance and limitations of different regression models.

## Objectives

- Explore and clean the dataset.
- Perform descriptive statistical analysis and correlation analysis.
- Study the linear relationship between BMI and blood pressure.
- Build simple and multiple linear regression models.
- Compare models using statistical criteria.
- Apply variable selection methods, including stepwise regression and Lasso.
- Evaluate model assumptions through residual analysis and diagnostic tests.
- Generate a BMI prediction for a hypothetical observation.

## Dataset

The dataset contains **7,132 observations** and three main variables:

| Variable | Description |
|---|---|
| `BMI` | Body Mass Index |
| `BP_Sy` | Systolic blood pressure |
| `BP_Di` | Diastolic blood pressure |

The dataset file (`BMI_Dataset.RData`) is not included in this repository because it is currently unavailable. The original download source has not yet been identified.

## Methods and Technologies

**Language:** R

**Statistical methods:**

- Descriptive statistics and data visualization
- Correlation analysis
- Simple and multiple linear regression
- Stepwise variable selection
- Model comparison using R², adjusted R², AIC, BIC and Mallows' Cp
- Lasso regression
- ANOVA and Tukey's test
- Residual diagnostics and statistical hypothesis testing

**R packages:**

- `lmtest`
- `leaps`
- `glmnet`

## Key Results

The multiple linear regression model achieved:

- **R²:** 0.1127
- **Adjusted R²:** 0.1124

The model explains approximately **11.27% of the variability in BMI**, indicating limited explanatory power.

For a hypothetical observation with a systolic blood pressure of 130 mmHg and a diastolic blood pressure of 85 mmHg, the model predicted a BMI of **30.90**.

## Limitations

The model has limited explanatory power, and some linear regression assumptions are not perfectly satisfied. Additional variables, such as age, sex, diet and physical activity, could improve the analysis.

**This project is for educational and statistical purposes only. It is not intended for medical diagnosis or clinical decision-making.**

## Repository Contents

- `README.md` — Project description and documentation
- `regression.R` — R source code for data analysis, regression modeling and diagnostics *(if this is the name of your code file)*

**Note:** The dataset must be obtained separately to reproduce the analysis. The R code may not run until the required dataset file is available.

## Authors

- Malek Louati
- Hafssa El Hdour
- Abdou Dieng

**Course:** Linear Regression  
**Academic Year:** 2025–2026  
**Institution:** Polytech Nice Sophia

