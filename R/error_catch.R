if (any(is.na(data))) {
  warning("Data contains NA values. These will be removed.")
  data <- na.omit(data)
}

if (any(!is.finite(unlist(data)))) {
  stop("Data contains non-finite values")
}

if (nrow(data) != length(residuals(model))) {
  stop("Number of rows in data does not match the model")
}
