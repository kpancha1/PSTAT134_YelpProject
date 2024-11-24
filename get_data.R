rm(list=ls())

library(jsonlite)
library(dplyr)
library(ndjson)
library(jsonlite)
library(data.table)
library(parallel)

setwd('/Users/yujisu/Desktop/Final project')

## Review.json
# Open a connection to the JSON file and Create a list to store parsed data
file_connection <- file("data/business.json", "r")

# Initialize counters and storage
line_count <- 0
chunk_count <- 1
chunk_size <- 1000000  # Save every 1,000,000 lines
parsed_data <- list()

# Process lines in chunks
while (TRUE) {
  # Read a chunk of lines (e.g., 1000 at a time)
  lines <- readLines(file_connection, n = 1000)
  if (length(lines) == 0) break  # Exit loop if no lines left
  
# Parse each line as JSON and add to the list
  #parsed_chunk <- lapply(lines, function(line) fromJSON(line, simplifyVector = TRUE))

  #For the business data
  parsed_chunk <- lapply(lines, function(line) {
    data <- fromJSON(line, simplifyVector = TRUE)
    data$hours <- NULL  
    data$attributes <- NULL
    return(data)
  })
  parsed_data <- append(parsed_data, parsed_chunk)
  
  # Increment line counter
  line_count <- line_count + length(lines)
  cat("Processed", line_count, "lines...\n")
  if (line_count %% chunk_size == 0) {
    saveRDS(parsed_data,paste0("/Users/yujisu/Desktop/Final project/parsed_data_business", chunk_count, ".rds"))
    print(chunk_count)
    # Reset the chunk storage
    parsed_chunk <- list()
    parsed_data <- list()
    # Increment the chunk counter
    chunk_count <- chunk_count + 1
  }
}
close(file_connection)
chunk_count
saveRDS(parsed_data,paste0("/Users/yujisu/Desktop/Final project/parsed_data_business", chunk_count, ".rds"))

gc()
i=1
parsed_data <- readRDS(paste0("/Users/yujisu/Desktop/Final project/parsed_data_business", i, ".rds")); gc()
#a <- Sys.time()
flattened_list <- lapply(parsed_data, function(x) as.data.frame(x, stringsAsFactors = FALSE)); gc()
# Combine all the flattened data frames into one using rbindlist
final_df <- rbindlist(flattened_list, fill = TRUE); head(final_df)
fwrite(final_df, paste0("./business_df", i, ".csv")); print(i); gc()
#b <- Sys.time()



##For business data
problematic_elements <- sapply(parsed_data, function(x) is.null(x) || length(x) == 0)
filtered_data <- parsed_data[sapply(parsed_data, function(x) !is.null(x) && length(x) > 0)]
# Remove problematic elements
filtered_data <- filtered_data[problematic_indices]
# Ensure all elements are non-empty
filtered_data <- filtered_data[sapply(filtered_data, function(x) !is.null(x) && length(x) > 0)]
# Find all unique column names
all_columns <- unique(unlist(lapply(filtered_data, function(x) if (!is.null(x)) names(x))))
standardize_element <- function(x) {
  tryCatch({
    # Convert to data frame
    df <- as.data.frame(x, stringsAsFactors = FALSE)
    
    # Add missing columns
    missing_cols <- setdiff(all_columns, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    
    # Return standardized data frame
    return(df)
  }, error = function(e) {
    # Return an empty data frame if standardization fails
    return(data.frame(matrix(ncol = length(all_columns), nrow = 0, dimnames = list(NULL, all_columns))))
  })
}
standardized_list <- lapply(filtered_data, standardize_element)
final_df <- rbindlist(standardized_list, fill = TRUE)





### Get a list of all CSV files in the folder
file_list <- list.files(path = "./data/business/", pattern = "*.csv", full.names = TRUE)

# Read and combine all CSV files into one data frame
combined_data <- file_list %>%
  lapply(read.csv, stringsAsFactors = TRUE) %>%  # Read each CSV file
  bind_rows()                                    # Combine them into one data frame

# Save the combined data frame into a single CSV file
write.csv(combined_data, "business_data.csv", row.names = FALSE)

