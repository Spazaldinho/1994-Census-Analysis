######## LOADING LIBRARIES ######## 

library(dplyr)
library(ggplot2)
library(tidyverse)
library(GGally)
library(tree)
library(class)
library(caret)
library(randomForest)
library(factoextra)


######## READING DATA ######## 

adult_data <- read.csv("adult.data", header = FALSE)
adult_test <- read.csv("adult.test", header = FALSE, skip = 1)

column_names <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                  "marital_status", "occupation", "relationship", "race", 
                  "sex", "capital_gain", "capital_loss", "hours_per_week", 
                  "native_country", "income")

colnames(adult_data) <- column_names
colnames(adult_test) <- column_names

adult_data <- adult_data %>%
  mutate(across(where(is.character), trimws))

adult_test <- adult_test %>%
  mutate(across(where(is.character), trimws))

adult_data[] <- lapply(adult_data, as.character)
rows_with_question_mark <- apply(adult_data, 1, function(row) any(row == "?"))
cleaned_train <- adult_data[!rows_with_question_mark, ]

cleaned_train$age <- as.integer(cleaned_train$age)
cleaned_train$fnlwgt <- as.integer(cleaned_train$fnlwgt)
cleaned_train$education_num <- as.integer(cleaned_train$education_num)
cleaned_train$capital_gain <- as.integer(cleaned_train$capital_gain)
cleaned_train$capital_loss <- as.integer(cleaned_train$capital_loss)
cleaned_train$hours_per_week <- as.integer(cleaned_train$hours_per_week)

cleaned_train <- cleaned_train %>%
  mutate(income = sub("\\.$", "", income))


adult_test[] <- lapply(adult_test, as.character)
rows_with_question_mark <- apply(adult_test, 1, function(row) any(row == "?"))
cleaned_test <- adult_test[!rows_with_question_mark, ]

cleaned_test$age <- as.integer(cleaned_test$age)
cleaned_test$fnlwgt <- as.integer(cleaned_test$fnlwgt)
cleaned_test$education_num <- as.integer(cleaned_test$education_num)
cleaned_test$capital_gain <- as.integer(cleaned_test$capital_gain)
cleaned_test$capital_loss <- as.integer(cleaned_test$capital_loss)
cleaned_test$hours_per_week <- as.integer(cleaned_test$hours_per_week)

cleaned_test <- cleaned_test %>%
  mutate(income = sub("\\.$", "", income))

complete_dataset <- rbind(cleaned_train, cleaned_test)


######## PRELIMINARY EXPLORATION ######## 

age_counts <- as.data.frame(table(complete_dataset$age))

ggplot(age_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#FFAB91", color = "#BF360C", width = 0.8) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(face="bold", color="#3E2723", size=12),
    axis.title.y = element_text(face="bold", color="#3E2723", size=12),
    axis.text.x = element_text(angle=45, vjust=1, hjust=1)
  ) +
  labs(
    title = "Distribution of Ages",
    x = "Age",
    y = "Frequency"
  ) +
  scale_fill_manual(values = c("#FFCCBC", "#FFAB91", "#FF8A65", "#FF7043", "#FF5722")) 

ggplot(complete_dataset, aes(x = fnlwgt)) +
  geom_density(fill = "salmon", alpha = 0.5) +
  labs(title = "Density Plot of fnlwgt", x = "fnlwgt", y = "Density") +
  theme_minimal()

workclass_counts <- as.data.frame(table(complete_dataset$workclass))

ggplot(workclass_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#F4A582", color = "black") +
  theme_minimal() +
  labs(x = "Workclass", y = "Frequency", title = "Distribution of Workclass") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

education_counts <- as.data.frame(table(complete_dataset$education))

ggplot(education_counts, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Education Level Proportions", fill = "Education Level")

marital_status_counts <- as.data.frame(table(complete_dataset$marital_status))

ggplot(marital_status_counts, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#B39DDB", color = "black") +  
  theme_minimal() +
  labs(x = "Marital Status", y = "Frequency", title = "Distribution of Marital Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

occupation_counts <- as.data.frame(table(complete_dataset$occupation))

ggplot(occupation_counts, aes(x = reorder(Var1, -Freq), y = Freq)) +  
  geom_bar(stat = "identity", fill = "#B79FAD", color = "black") +
  theme_minimal() +
  labs(x = "Occupation", y = "Frequency", title = "Distribution of Occupation") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))

relationship_counts <- complete_dataset %>%
  group_by(relationship) %>%
  summarise(count = n())

ggplot(relationship_counts, aes(x = "", y = count, fill = relationship)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +  
  labs(fill = "Relationship", title = "Distribution of Relationship Status") +
  scale_fill_brewer(palette = "Pastel1")

ggplot(complete_dataset, aes(x = race)) +
  geom_bar(fill = "#495290", color = "black") + 
  theme_minimal() +
  labs(x = "Race", y = "Count", title = "Distribution of Race in the Dataset") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(complete_dataset %>% filter(capital_loss > 0), aes(x = capital_loss)) +
  geom_histogram(binwidth = 500, fill = "#66DDCC", color = "#108070") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Histogram of Non-Zero Capital Loss", x = "Capital Loss", y = "Frequency")

ggplot(complete_dataset %>% filter(capital_gain > 0), aes(x = capital_gain)) +
  geom_histogram(binwidth = 500, fill = "#F6DDCC", color = "#6E2C00") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(title = "Histogram of Non-Zero Capital Gain", x = "Capital Gain", y = "Frequency")

ggplot(complete_dataset, aes(x = hours_per_week)) +
  geom_density(fill = "#F79FAD", color = "black") +
  theme_minimal() +
  labs(x = "Hours per Week", y = "Density", title = "Density of Hours Worked per Week")

country_counts <- complete_dataset %>%
  count(native_country) %>%
  arrange(desc(n))

ggplot(country_counts, aes(x = reorder(native_country, -n), y = n, fill = native_country)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Native Country", y = "Number of Entries", title = "Entries by Native Country", fill = "Country")

income_counts <- complete_dataset %>%
  count(income)

ggplot(income_counts, aes(x = "", y = n, fill = income)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#108070", "#495290")) +
  theme_void() +
  theme(legend.title = element_blank()) +
  labs(title = "Income Distribution")


# Want to see how education_num, the absolute level of education, relates to 
# income class

ggplot(cleaned_train, aes(x = income, y = education_num)) +
  geom_boxplot() +
  labs(x = "Income class", y = "Education Level", title = "Box Plot of Integer Quantity by Binary Classification")

ggplot(cleaned_train, aes(x = education_num, fill = income)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ income) +
  labs(x = "Education Level", y = "Density", title = "Density Plot of Education Level by Income Class")

ggplot(cleaned_train, aes(x = education_num, fill = income)) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~ income) +
  labs(x = "Education Level", y = "Count", title = "Histogram of Education Level by Income Class")

count_data <- cleaned_train %>%
  group_by(education_num, income) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(count_data, aes(x = education_num, y = count, group = income, color = income)) +
  geom_line() +
  scale_x_continuous(breaks = unique(cleaned_train$education_num)) +  
  labs(x = "Education Number", y = "Number of People", title = "Line Graph of People Count by Education Number and Income") +
  theme_minimal()

total_counts <- cleaned_train %>%
  group_by(income) %>%
  summarise(total = n(), .groups = 'drop')

count_data <- cleaned_train %>%
  group_by(education_num, income) %>%
  summarise(count = n(), .groups = 'drop') %>%
  left_join(total_counts, by = "income") %>%
  mutate(percentage = (count / total) * 100)

ggplot(count_data, aes(x = education_num, y = percentage, group = income, color = income)) +
  geom_line() +
  scale_x_continuous(breaks = unique(cleaned_train$education_num)) +  
  labs(x = "Education Number", y = "Percentage of People", title = "Line Graph of Education Level Percentage by Income Class") +
  theme_minimal()

count_data <- cleaned_train %>%
  group_by(education_num, income) %>%
  summarise(count = n(), .groups = 'drop')

total_counts <- cleaned_train %>%
  group_by(income) %>%
  summarise(total = n(), .groups = 'drop')

count_data <- count_data %>%
  left_join(total_counts, by = "income") %>%
  mutate(percentage = (count / total) * 100) %>%
  arrange(income, education_num) %>%
  group_by(income) %>%
  mutate(cum_percentage = cumsum(percentage)) %>%
  ungroup()

ggplot(count_data, aes(x = education_num, y = cum_percentage, group = income, color = income)) +
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = unique(cleaned_train$education_num)) + 
  labs(x = "Education Number", y = "Cumulative Percentage of People", title = "Cumulative Percentage Line Graph by Education Number and Income") +
  theme_minimal()

# Now lets see the same thing for age

ggplot(cleaned_train, aes(x = income, y = age)) +
  geom_boxplot() +
  labs(x = "Income class", y = "Age", title = "Box Plot of Age by Binary Classification")

ggplot(cleaned_train, aes(x = age, fill = income)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ income) +
  labs(x = "Age", y = "Density", title = "Density Plot of Age by Income Class")

ggplot(cleaned_train, aes(x = age, fill = income)) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~ income) +
  labs(x = "Age", y = "Count", title = "Histogram of Age by Income Class")

count_data <- cleaned_train %>%
  group_by(age, income) %>%
  summarise(count = n()) %>%
  ungroup()

ggplot(count_data, aes(x = age, y = count, group = income, color = income)) +
  geom_line() +
  scale_x_continuous(breaks = unique(cleaned_train$age)) +  
  labs(x = "Age", y = "Number of People", title = "Line Graph of People Count by Age and Income")

total_counts <- cleaned_train %>%
  group_by(income) %>%
  summarise(total = n())

count_data <- cleaned_train %>%
  group_by(age, income) %>%
  summarise(count = n()) %>%
  left_join(total_counts, by = "income") %>%
  mutate(percentage = (count / total) * 100)

ggplot(count_data, aes(x = age, y = percentage, group = income, color = income)) +
  geom_line() +
  scale_x_continuous(breaks = unique(cleaned_train$age)) +  
  labs(x = "Age", y = "Percentage of People", title = "Line Graph of Age Percentage by Income Class")

count_data <- cleaned_train %>%
  group_by(age, income) %>%
  summarise(count = n())

total_counts <- cleaned_train %>%
  group_by(income) %>%
  summarise(total = n())

count_data <- count_data %>%
  left_join(total_counts, by = "income") %>%
  mutate(percentage = (count / total) * 100) %>%
  arrange(income, age) %>%
  group_by(income) %>%
  mutate(cum_percentage = cumsum(percentage)) %>%
  ungroup()

ggplot(count_data, aes(x = age, y = cum_percentage, group = income, color = income)) +
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = unique(cleaned_train$age)) + 
  labs(x = "Age", y = "Cumulative Percentage of People", title = "Cumulative Percentage Line Graph by Age and Income") +
  theme_minimal()

race_income_proportion <- complete_dataset %>%
  count(race, income) %>%
  group_by(race) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

ggplot(race_income_proportion, aes(x = "", y = proportion, fill = income)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(~race, scales = "free_y") +
  theme_void() +
  labs(title = "Proportion of Income Classes by Race", fill = "Income Class")

sex_income_proportion <- complete_dataset %>%
  count(sex, income) %>%
  group_by(sex) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup()

ggplot(sex_income_proportion, aes(x = "", y = proportion, fill = income)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(~sex, scales = "free_y") +
  theme_void() +
  labs(title = "Proportion of Income Classes by sex", fill = "Income Class")



######## Some Classification! ######## 

# Needed to factor string data, and remove variables with >32 factors.
# Decided to change native country to a binary group
cleaned_train_tree <- cleaned_train %>%
  mutate(native_country = ifelse(native_country == "United-States", "United-States", "Other")) %>%
  mutate_if(is.character, as.factor) 

cleaned_test_tree <- cleaned_test %>%
  mutate(native_country = ifelse(native_country == "United-States", "United-States", "Other")) %>%
  mutate_if(is.character, as.factor) 

tree_test <- tree(income ~. , data = cleaned_train_tree)
plot(tree_test)
text(tree_test, pretty = 0)

predictions <- predict(tree_test, newdata = cleaned_test_tree, type = "class")
confusionMatrix(predictions, as.factor(cleaned_test_tree$income))

cleaned_test_factorized <- lapply(cleaned_test, function(x) if(is.character(x)) factor(x) else x)
cleaned_train_factorized <- lapply(cleaned_train, function(x) if(is.character(x)) factor(x) else x)

numeric_train <- cleaned_train[, sapply(cleaned_train, is.numeric)]
numeric_test <- cleaned_test[, sapply(cleaned_test, is.numeric)]

train_features_scaled <- scale(numeric_train)
test_features_scaled <- scale(numeric_test, center = attr(train_features_scaled, "scaled:center"), scale = attr(train_features_scaled, "scaled:scale"))

train_target <- cleaned_train_factorized$income
test_target <- cleaned_test_factorized$income

# K (7) nearest neighbors
k <- 7
knn_model <- knn(train = train_features_scaled, test = test_features_scaled, cl = train_target, k = k)
confusionMatrix(knn_model, test_target)

cross_knn <- knn.cv(train = train_features_scaled, cl = train_target, k = k, l = 0, prob = FALSE, use.all = TRUE)
confusionMatrix(cross_knn, train_target)


# PCA with KNN
pca_result <- prcomp(train_features_scaled, center = TRUE, scale. = TRUE)

# Decide on the number of principal components to retain
variance_explained <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
num_components <- which(variance_explained >= 0.95)[1]  # keep 95% of the variance

train_pca <- predict(pca_result, train_features_scaled)[, 1:num_components]
test_pca <- predict(pca_result, test_features_scaled)[, 1:num_components]

k <- 7
knn_model <- knn(train = train_pca, test = test_pca, cl = train_target, k = k)

confusionMatrix(knn_model, as.factor(test_target))

# Random Forest
rf_fit <- randomForest(income ~., data = cleaned_train_tree, 
                       mtry = (ncol(cleaned_train_tree) - 1)/ 3, ntree = 1000, importance = TRUE)
rf_pred <- predict(rf_fit, cleaned_test_tree)

confusionMatrix(rf_pred, as.factor(test_target))
