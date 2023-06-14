# Load required libraries
library(dplyr)
library(tidyr)
library(lubridate)

# Set seed for reproducibility
set.seed(123)

# Generate dummy data
data1 <- data.frame(id = 1:10, var1 = runif(10), stringsAsFactors = FALSE)
data2 <- data.frame(id = sample(1:10), var2 = runif(10), stringsAsFactors = FALSE)
data3 <- data.frame(id = sample(1:10), var3 = runif(10), stringsAsFactors = FALSE)
data4 <- data.frame(id = sample(1:10), var4 = runif(10), stringsAsFactors = FALSE)
data5 <- data.frame(id = sample(1:10), var5 = runif(10), stringsAsFactors = FALSE)

# Perform transformations on the data
transformed_data <- data1 %>%
  inner_join(data2, by = "id") %>%
  inner_join(data3, by = "id") %>%
  inner_join(data4, by = "id") %>%
  inner_join(data5, by = "id") %>%
  mutate(total = var1 + var2 + var3 + var4 + var5) %>%
  filter(total > 2) %>%
  select(-total) %>%
  arrange(id) %>%
  pivot_longer(cols = starts_with("var"), names_to = "variable", values_to = "value") %>%
  mutate(value_squared = value^2) %>%
  group_by(variable) %>%
  summarize(mean_value = mean(value), sum_squared = sum(value_squared)) %>%
  arrange(desc(mean_value)) %>%
  mutate(rank = row_number()) %>%
  select(variable, rank, mean_value, sum_squared) %>%
  mutate(date = ymd("2023-05-14")) %>%
  mutate(month = month(date), quarter = quarter(date), year = year(date))

# Save transformed_data as CSV file
write.csv(transformed_data, file = "transformed_data.csv", row.names = FALSE)

# Print the transformed_data
print(transformed_data)
