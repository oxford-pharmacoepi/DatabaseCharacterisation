filterData <- function(data, prefix, input) {
  cols <- colnames(data)
  cols <- cols[paste0(prefix, "_", cols) %in% names(input)]
  for (col in cols) {
    data <- data %>%
      dplyr::filter(.data[[col]] %in% .env$input[[paste0(prefix, "_", col)]])
  }
  validate(need(nrow(data) > 0, "No results for selected inputs"))
  return(data)
}
selectors <- function(data, prefix, columns, multiple = TRUE, default = list()) {
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col) {
    data[[col]] %>% unique() %>% sort()
  }
  purrr::map(columns, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choic(.),
    selected = def(.),
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE, 
    width = "200px"
  ))
}

bindestimates <- function(...) {
  # initial checks
  results <- list(...)
  omopgenerics:::assertList(results, class = "summarised_result")
  
  settings <- lapply(results, omopgenerics::settings) |>
    dplyr::bind_rows(.id = "list_id")
  results <- results |>
    dplyr::bind_rows(.id = "list_id")
  
  dic <- settings |>
    dplyr::select("result_id", "list_id") |>
    dplyr::distinct() |>
    dplyr::mutate("new_result_id" = as.integer(dplyr::row_number()))
  
  settings <- settings |>
    dplyr::inner_join(dic, by = c("result_id", "list_id")) |>
    dplyr::select(-c("result_id", "list_id")) |>
    dplyr::rename("result_id" = "new_result_id")
  results <- results |>
    dplyr::inner_join(dic, by = c("result_id", "list_id")) |>
    dplyr::select(-c("result_id", "list_id")) |>
    dplyr::rename("result_id" = "new_result_id")
  
  attr(results, "settings") <- settings
  class(results) <- c("summarised_result", class(results))
  
  return(results)
}

separator <- function(x){
 format(as.numeric(x), big.mark = ",", decimal.mark = ".")
}