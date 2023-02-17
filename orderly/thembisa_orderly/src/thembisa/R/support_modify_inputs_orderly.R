
create_data_dictionary <- function(data) {
  ## Split each line into a vector, using tabs as separators
  data <- strsplit(data, "\t")
  ## Grab first element of each line so we can test if it is data or text
  first_element <- sapply(data, "[", 1)
  ## Identify the lines that contain numerical data
  idx_data <- grepl("^[0-9]", first_element)

  # Extract descriptions of data, which are stored in first element of vector
  descriptions <- first_element[!idx_data]

  ## Identify which descriptions refer to which data (some are repeated)
  ## This gives a vector the same length as the data with entries indicating
  ## which description is used for that data
  idx_description <- cumsum(!idx_data)[idx_data]

  ## Output a data dictionary, giving the line numbers in the text file referring to
  ## - line number of the data itself
  ## - line number of the description of the data
  ## - name of data - defaults to X1, ..., Xn
  ## - text string describing the data
  dictionary <- data.frame(line_data = which(idx_data),
                           line_description = which(!idx_data)[idx_description],
                           name = paste0("X", seq_len(sum(idx_data))),
                           description = descriptions[idx_description])
  dictionary
}


save_dictionary <- function(dictionary, file) {
  write.csv(dictionary, file, row.names = FALSE)
  
}

## Convert Thembisa inputs into easily modifiable format
format_data <- function(data, dictionary, start_year = 1985, end_year = 2100) {
  ## Split each line into a vector, using tabs as separators
  data <- strsplit(data, "\t")
  ## Convert data from characters to numbers
  x <- as.numeric(unlist(data[dictionary$line_data]))
  ## Arrange in a matrix with one column per data set
  ret <- matrix(x, ncol = nrow(dictionary))
  colnames(ret) <- dictionary$name

  ## Add column indicating year of data
  year <- seq(start_year, end_year)
  if (length(year) != nrow(ret)) {
    stop("number of data entries does not match year range provided")
  }
  list(year = year,
       data = ret)
}



convert_to_thembisa_format <- function(formatted_data, data, dictionary) {
  values <- apply(formatted_data$data, MARGIN = 2,
                  function(x) paste(sprintf("%.4f", x), collapse = "\t"))
  ret <- replace(data, list = dictionary$line_data, values)
  ret
}

