# House Price Prediction

## Overview

This project forms the final part of the [Professional Certificate in Data Science by HarvardX](https://online-learning.harvard.edu/series/professional-certificate-data-science) offered by [edX](https://www.edx.org). The aim of this project was to use different data science and machine learning (ML) regression techniques to predict house prices using the **Kaggle** [House Prices: Advanced Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques) dataset.

The following ML models were fitted and used to make predictions of house sale prices:

* Support vector machine with linear kernel (*svmLinear*)
* Boosted generalized linear model (*glmboost*)
* Linear regression (*lm*)
* Penalized linear regression (*penalized*)
* Generalized additive model using LOESS (*gamLoess*)
* Stochastic gradient boosting (*gbm*)
* K-Nearest neighbour (*kknn*)
* eXtreme gradient boosting (*xgbLinear*)
* Random forest (*rf*)
* eXtreme gradient boosting (*xgbTree*)
* Boosted tree (*bstTree*)

More information about each model can be found in the *Classification and Regression Training* page of R's [**caret package**](https://rdrr.io/cran/caret/man/models.html).

More information about the methodology used and the results obtained can be found in the *house_price_prediction.pdf* report while *sale_price_prediction.R* contains the code for testing, developing and evaluating the model.

## Repository contents

* **house_price_prediction.pdf**
    * The final PDF report knitted from an R Markdown file (see *house_price_prediction.Rmd*)

* **house_price_prediction.Rmd**
    * An R Markdown document including the code to generate all the plots, tables and results reported in *house_price_prediction.pdf*
    * Detailed instructions about how to reproduce the analysis are included

* **sale_price_prediction.R**
    * An R script that will build the ML regression models, fit them and then make predictions on a test set
    * At the end, a data frame is produced that contains the final predictions ready to be submitted to Kaggle

---
