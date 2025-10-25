# Adult Census Income Prediction 📊

This project analyzes the "Adult" dataset from the 1994 US Census database to predict whether an individual's annual income exceeds $50,000. The analysis involves data cleaning, comprehensive exploratory data analysis (EDA), and the implementation and evaluation of several machine learning classification models.

---

## Dataset Overview

The dataset provides a rich collection of socio-economic and demographic features for predicting income levels.

* **Source**: 1994 US Census Bureau Database
* **Total Instances**: 48,842 records, split into a training set (32,561) and a testing set (16,281).
* **Cleaned Instances**: After removing records with unknown values, 45,222 instances remain.
* **Features**: The dataset contains a mix of continuous and categorical attributes, including: `age`, `workclass`, `education-num`, `marital-status`, `occupation`, `relationship`, `race`, `sex`, `capital-gain`, `capital-loss`, and `hours-per-week`.

---

## Analysis and Findings

The project followed a structured workflow from data preparation to modeling, yielding key insights into the factors that correlate with income.

### Exploratory Data Analysis (EDA) 📈

Visual exploration of the data revealed several significant trends:

* **Education & Income**: A strong positive correlation exists between higher education levels and earning over $50K. Individuals in the higher-income bracket consistently have more years of education.
* **Age & Income**: Those earning over $50K are, on average, older than those earning less. The analysis showed that the prime earning years appear to be between the late 30s and 50s.
* **Demographic Factors**: The data shows notable income disparities across different demographic groups. A significantly larger proportion of males earn over $50K compared to females. Similarly, individuals identified as "White" or "Asian-Pac-Islander" have a higher representation in the `>50K` income category.

### Predictive Modeling and Results 🤖

Several classification models were trained and evaluated. The performance was measured by prediction accuracy on the test set.

| Model                 | Accuracy  |
| --------------------- | :-------: |
| Decision Tree         | 84.49%    |
| K-Nearest Neighbors   | 82.50%    |
| **Random Forest** | **85.30%**|

The **Random Forest** model was the top performer, achieving an accuracy of **85.3%**. This result is highly competitive with benchmark models documented for this dataset, such as C4.5 (84.46%) and NBTree (85.90%).

---

## Conclusion

The analysis successfully identified key predictors of income and built an effective classification model.

* **Best Model**: The **Random Forest** model provided the highest accuracy and is the recommended model for this prediction task.
* **Key Predictors**: The most influential factors in predicting income were determined to be an individual's **relationship status**, **capital gains**, **education level**, and **age**.

---

## How to Run This Project

To replicate this analysis, follow these steps.

### Prerequisites

* **R**: Ensure you have a working installation of R.
* **RStudio**: An IDE like RStudio is recommended.

### Required R Libraries

Install the necessary R packages by running the following command in your R console:

```R
install.packages(c("dplyr", "ggplot2", "tidyverse", "GGally", "tree", "class", "caret", "randomForest", "factoextra"))
