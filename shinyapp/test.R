# Create a sample data frame
df <- data.frame(
  ID = c(1, 2, 3, 4, 5),
  Category = c("A", "B", "A", "C", "B"),
  Value = c(10, 20, 15, 15, 30)
)

# Find the rows where Category is "A"
rows_to_keep <- which(df$Value == 15)

# Print the row numbers
print(rows_to_keep) # Output: 1 3

# Subset the data frame to include only those rows
subset_df <- df[rows_to_keep, ]

# Print the subsetted data frame
print(subset_df)
