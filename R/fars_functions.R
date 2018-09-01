#' Read dataset
#'
#' This is a function that loads the csv dataset if it exists, and
#' prints the data frame form of the dataset. You can specify the
#' name of the dataset (using the \code{filename} argument).
#'
#' @param filename A character string giving the name of the
#'                 csv dataset.
#'
#' @return This function prints the data frame form of a csv dataset.
#'         If the filename doesn't exist then prints an error message.
#'
#' @examples
#' fars_read(accident_2015.csv)
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
#'

#' Make filename
#'
#' This is a function that makes the dataset names. You can insert
#' the year of the dataset into the file name
#' (using the \code{year} argument).
#'
#' @param year A numeric variable giving the year of the file name.
#'
#' @return This function prints different dataset name by
#'         different year.
#'
#' @examples
#' make_filename(2018)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Select months and years of interest
#'
#' This is a function that selects the months and years in specified years
#' from datasets of these years (using the \code{years} argument).
#'
#' @param years A list of numbers giving the years of interest.
#'
#' @return This function prints months and years of interst. If datasets
#'         of one or more years do not exist in the datasset then generates
#'         a warning message.
#'
#' @examples
#' fars_read_years(list(2017,2018))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#'

#' Summarize datasets of interest
#'
#' This is a function that counts the number of months in each years
#' of interest (using the \code{years} argument).
#'
#' @param years A list of numbers giving the years of interest.
#'
#' @return This function prints the number of months in each years
#'         of interest.
#'
#' @examples
#' fars_summarize_years(list(2017,2018))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Draw state map of the accidents in a year
#'
#' This is a function that draws the locations of the accidents in
#' the specified state in the specified year.
#'
#' @param state.num A numeric variable specifies the state of
#'                  interest by number.
#' @param year a numeric variable specifies the year of interest.
#'
#' @return This function prints state map of the accidents in a year.
#'         If the state doesn't exist then prints an error message.
#'         If there were no accidents in the state and year specified,
#'         then prints "no accidents to plot".
#'
#' @examples
#' fars_map_state(1, 2018)
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
