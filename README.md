# 🫀 Heart Failure Prediction using Machine Learning


## 📂 Project Overview

This project applies machine learning techniques to predict heart failure events using clinical data. Cardiovascular diseases are the leading cause of death worldwide. Predicting heart failure can lead to timely interventions and improved patient outcomes. This project explores various machine learning models to assess their effectiveness in predicting heart failure based on 12 key clinical features.

## 📊 Dataset Description

The dataset contains medical records of **299 patients** collected from the **Faisalabad Institute of Cardiology and Allied Hospital (Pakistan)** in 2015.

## 🔧 Data Processing

- Converted binary numeric variables into factors for better interpretation.
- Checked and removed any missing or duplicate entries.
- Cleaned the data to ensure quality before modeling.

## 📈 Exploratory Data Analysis

Visualizations were used to analyze:
- Distributions of each feature
- Relationships between features and the DEATH_EVENT
- Correlation matrix to identify significant predictors

## 🤖 Models Built

### 1. Multiple Linear Regression
- Identified significant predictors using p-values and ANOVA.
- Applied stepwise selection to refine the model.

### 2. Regression Tree (rpart)
- Performed pruning and complexity parameter tuning.
- Highlighted key splits like follow-up time and ejection fraction.

### 3. Random Forest (Ensemble)
- Applied to improve accuracy by reducing overfitting.
- Selected as the top-performing model based on NMSE.

## 📏 Evaluation Metrics

- **MAE** (Mean Absolute Error)
- **MSE** (Mean Squared Error)
- **NMSE** (Normalized MSE)
- **K-Fold Cross-Validation** for robust model validation

### Best Model: `rpartXse.v3` (pruned regression tree)
- Achieved lowest cross-validated NMSE of **0.578**

## 🧪 Predictions

Predictions were made on a holdout test set. The best model produced an NMSE of **0.577**, indicating decent prediction quality with room for further improvement.

## 🧾 Summary

- Cleaned and visualized the data to uncover meaningful patterns.
- Built multiple models to compare predictive power.
- Regression trees outperformed linear models in predicting mortality.
- Ensemble methods like Random Forests provided additional accuracy gains.


