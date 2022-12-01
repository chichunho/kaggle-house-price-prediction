# House price prediction in Kaggle

This is a Kaggle House Price Competition. The goal is to predict the house price through
the given attributes.

This repository contains:
1. Description
2. Dependencies
3. Model selection and scores

## Description

Year: 2020

House price are changing every day, we need a system to predict the house price in the future,
which can help either home buyers or householders to understand how they can predict the sale price
of it or how to set the price respectively.

## Dependencies

Language: R

Following R packages are used:
- e1071
- car
- MASS
- xgboost
- randomForest
- glmnet

## Model selection and scores

Five models are selected to predict the output. Their name and scores are shown as below:
- Linear regression: 0.13577
- Xgboost: 0.15885
- LASSO regression: 0.59756
- Random Forest: 0.13905
- Ridge regression: 0.13052
