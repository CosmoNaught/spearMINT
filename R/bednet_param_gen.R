# spearMINT/R/bednet_param_gen.R

# Load necessary libraries
library(dplyr)

#' Load and transform ITN data
#'
#' @param file_path The path to the CSV file.
#' @param net_type The type of net to label the data.
#' @return A data frame with the loaded and transformed data.
#' @export
load_itn_data <- function(file_path, net_type) {
  data <- read.csv(file_path)
  data <- data %>%
    dplyr::mutate(net_type = net_type) %>%
    dplyr::rename(dn0 = ERG_d_ITN0, rn0 = ERG_r_ITN0) %>%
    dplyr::select(dn0, rn0, gamman, bioassay_surv, net_type)
  return(data)
}

#' Combine ITN datasets
#'
#' @param file_paths A named list of file paths to the CSV files.
#' @return A combined data frame of all ITN data.
#' @export
combine_itn_data <- function(file_paths) {
  combined_data <- dplyr::bind_rows(
    lapply(names(file_paths), function(net_type) {
      load_itn_data(file_paths[[net_type]], net_type)
    })
  )
  return(combined_data)
}

#' Save ITN parameters to an RDS file
#'
#' @param data The data frame to save.
#' @param output_path The path to the output RDS file.
#' @export
save_itn_data <- function(data, output_path) {
  saveRDS(data, output_path)
}
