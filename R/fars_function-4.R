#' Reading csv file for Fatality Analysis Reporting System
#'
#' @description the /code{fars_read(filename)} function is used for reading CSV file in the US National Highway Traffic Safety Administration's Fatality Analysis Reporting System
#'
#' @usage fars_read(filename)
#'
#' @param filename a character vector of file names or a single character string of file name to csv file/files
#'
#' @details This function will stop if file name provided does not exist.
#' The function utilizes \code{readr::read_csv()} to read in file.  Any messages from the underlying \code{readr::read_csv()} method is suppressed
#' file names are generally in the format: "accident_YEAR.csv.bz2" \code{fars_read_year(YEAR)} can be used to read csv files for different years
#'
#' @return A tbl_df, tidyverse tibble using \code{tbl_df()}.  Note: the returned tbl_df contains not row names
#'
#' @examples \dontrun{fars_read('accident_2013.csv.bz2')}
#'
#' @import readr dplyr
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

#' Create file name given a specific year
#'
#' @description create a character vector of file name in the specific format "accident_YEAR.csv.bz2
#'
#' @usage make_filename(year)
#'
#' @param year numeric value for the year required or a list/vector of years required.
#'
#' @details This funciton utilizes the \code{sprintf(fmt, ...)} function, which is a wrapper function for the C function sprintf that returns a character vector containing text in specifc format
#'
#' @return a character string of a file name or a character vector of file names
#'
#' @examples \dontrun{make_filename(2013) # returns 'accident_2013.csv.bz2'}
#'
#' @import dplyr
make_filename <- function(year) {
 year <- as.integer(year)
 sprintf("accident_%d.csv.bz2", year)
}

#' Reading csv file for Fatality Analysis Reporting System for given year/years
#'
#' @description similar to \code{fars_read(filename)} for reading csv file but \code{fars_read_years(years)} take numerical values of years for which data is required as input
#'
#' @usage fars_read_years(years)
#'
#' @param years numeric value for the year required or a list/vector of years required.
#'
#' @details This funciton utilizes the \code{make_filename(YEAR)} function to turn the years for which data is required into the specific format.  Then utilizes \code{fars_read(fmt, ...)} function to read the csv file.
#' If a csv file for the given year does not exist the function with produce a warning message
#'
#' @return a tbl_df, tidyverse tibble.  Note: the returned tbl_df contains no row names
#'
#' @examples \dontrun{fars_read_years(2013)}
#' # returns data.frame containing data from "accident_2013.csv.bz2"

#' @import dplyr
#' @importFrom magrittr %>%
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

#' Summarizing the Number of reports
#'
#' @description the function summarizes the number of reports by years
#'
#' @usage fars_summarize_years(years)
#'
#' @param years numeric value for the year required or a list/vector of years required.
#'
#' @details the function uses functions in the dplyr package to summarize the number of reports in given years. \code{years} is provided as a numeric vector of the years required.
#' If a single year is provided the function will just sum up the number of report in that single year
#'
#' @return a data frame of summary table showing number of accidents by year (columns) and months (rows)
#'
#' @examples \dontrun{fars_summarize_years(c(2013:2015))}
#' # returns a summary of number of accidents by year and months between 2013 and 2015
#'
#' @import dplyr tidyr
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' A map showing location of accidents by year
#'
#' @description The accident reports contain the location information (longitude and latitude).  This function takes \code{state.num} representing paritcular states and plots the locations of the accidents reported in those states in a given year (or given years)
#'
#' @usage fars_map_state(state.num, year)
#'
#' @param state.num integer or integer vector representing a state or a number of states
#' @param year a numeric stating the year from which data is required
#'
#' @details This function reads data in csv file from given years and plots the accidents report on a map.
#' If function will convert the numeric values provided in \code{year} as integer.  The function returns an error if the state.num provided does not represent a state
#' If no data could be obtained.  The function will stop and return "no accidents to plot"
#'
#' @return draws a map to provide a visual output and returns nothing
#'
#' @examples \dontrun{fars_map_state(1, 2013)}
#' # plots the location of accidents in state_num 1 and year 2013
#'
#' @import dplyr maps
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
