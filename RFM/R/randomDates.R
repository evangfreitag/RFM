#' randomDates
#'
#' This function generates random dates.
#' @param N_Dates integer Number of random dates to draw.
#' @param startDate character Beginning of date range (for example "2015/01/01").
#' @param endDate character End of date range (for example "2016/12/31").
#' @keywords Random Dates
#' @export
#' @examples randomDates(10,"2016/01/01","2016/12/31")
#' randomDates()

randomDates <- function(N_Dates, startDate, endDate) {
     startDate <- as.POSIXct(as.Date(startDate))
     endDate <- as.POSIXct(as.Date(endDate))
     diffDate <- as.numeric(difftime(endDate,startDate,unit="sec"))
     nDays <- sort(runif(N_Dates, 0, diffDate))
     outDates <- startDate + nDays
}