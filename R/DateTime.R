#' Date conversion 
#'
#'Convert dates from the DD/MM/YYYY format to day of the year (DOY)
#' @param dates Vector with dates to convert.
#' @param format Format of the date (support DD/MM/YYYY MM/DD/YYYY and YYYY/MM/DD).
#' @return Return a vector containing the corresponding DOY.
#' @examples 
#' dates = c("01/01/2000", "03/03/2000", "03/03/1999")
#' date.to.DOY(dates=dates)
#' @export
date.to.DOY <- function (dates, format="dd/mm/yyyy") {
  if (format=="dd/mm/yyyy") {
    DOY <- strptime(dates, "%d/%m/%Y")$yday+1 
  } else if (format=="mm/dd/yyyy") {
    DOY <- strptime(dates, "%m/%d/%Y")$yday+1 
  } else if (format=="yyyy/mm/dd") {
    DOY <- strptime(dates, "%Y/%m/%d")$yday+1 
  }
  return(DOY)
}

#' Time conversion
#'
#'Convert time from the HH:MM:SS format to a numerical 
#' @param Time Vector with time to convert.
#' @param sep  Character element containing regular expression(s) to use to splitting.
#' @return Return a vector containing the corresponding time.
#' @details time vector should be in the HH:MM:SS format.
#' @examples 
#' Time = c("14:30:00", "20:45:00", "05:00:00")
#' timecont(Time=Time)
#' @export
timecont <- function(Time, sep = ":") {
  timecont <- sapply(strsplit(as.character(Time),split=":"),
                      function(x) {
                        x <- as.numeric(x)
                        x[1]+x[2]/60
                      }
  )
  return(timecont)
}

#' Time & dates conversion
#'
#'Convert DOY and time into a single numerical variable
#' @param dates Vector with dates in the DOY format.
#' @param Time  Vector with time
#' @return Return a vector containing DOY and time as a single numerical variable
#' @details time vector should be numerical (e.g. as outputed by the time.to.cont function)
#' @examples 
#' dates = c(102,102,102,102,103,103,103,103)
#' Time = c(22, 22.5, 23, 23.5, 0, 0.5, 1, 1.5)
#' datetime(dates=dates, Time=Time)
#' @export
datetime <- function(dates, Time) {
    datetime <- as.numeric(dates)+as.numeric(Time)/24
  return(datetime)
}
