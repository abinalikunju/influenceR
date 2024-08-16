#' Calculate and Plot Influence Measures
#'
#' @param data A data frame containing the variables in the model
#' @param model An object of class lm
#' @param measures Character vector of influence measures to calculate.
#'   Options are "cook", "dffits", and "hadi". Default is all three.
#'
#' @return A ggplot object with the influence measures
#' @export
#'
#' @importFrom stats cooks.distance
#' @importFrom stats dffits
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap theme_minimal
influence_plot <- function(data, model, measures = c("cook", "dffits", "hadi")) {
  if (!inherits(model, "lm")) {
    stop("model must be an object of class lm")
  }

  if (!all(measures %in% c("cook", "dffits", "hadi"))) {
    stop("Invalid measure specified")
  }

  results <- data.frame(index = seq_len(nrow(data)))

  if ("cook" %in% measures) {
    results$cook <- cooks.distance(model)
  }

  if ("dffits" %in% measures) {
    results$dffits <- dffits(model)
  }

  if ("hadi" %in% measures) {
    results$hadi <- calculate_hadi(model)  # We'll implement this function later
  }

  results_long <- tidyr::pivot_longer(results, -index, names_to = "measure", values_to = "value")

  ggplot(results_long, aes(x = index, y = value)) +
    geom_point() +
    facet_wrap(~ measure, scales = "free_y") +
    theme_minimal() +
    labs(x = "Observation Index", y = "Influence Measure")
}
