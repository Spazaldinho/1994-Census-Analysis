# Install and load necessary package
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)

# Fetch dataset from UCI Repository (adjust URL as needed)
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
adult <- fread(url, header = FALSE, na.strings = "?")

# Define column names based on dataset description
colnames(adult) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                     "marital_status", "occupation", "relationship", "race", "sex",
                     "capital_gain", "capital_loss", "hours_per_week", "native_country", "income")

# Split data into features and targets
X <- adult[, 1:(ncol(adult)-1), with = FALSE] # All but the last column
y <- adult$income # Target variable

# Metadata and variable information isn't directly fetched in this process.
# Typically, you'd refer to the dataset's documentation for this information.

# For demonstration, here's how you could manually create a simple metadata structure
metadata <- list(description = "Adult Dataset from UCI ML Repository",
                 url = "https://archive.ics.uci.edu/ml/datasets/adult",
                 size = dim(adult),
                 variables = colnames(adult))

# Print metadata
print(metadata)

# Variable information
variables_info <- sapply(adult, class) # This gives the data type of each column
print(variables_info)
