#' Get Relative Dates
#' 
#' This function takes a target date as a well formatted character, e.g. '2019-01-01'
#' and returns a dataframe of relative dates, e.g. yesterday, last week ending, 
#' last quarter start, etc.
#' @param targetDate The date of interest as a well formatted character string, 
#' e.g. '2019-01-01'
#' @keywords Dates, yesterday, last, week, month, previous, quarter, year
#' @export
#' @examples 
#' getRelativeDates()

getRelativeDates <- function(targetDate = Sys.time(), tz = 'America/New_York'){
  
  # Make sure target date is a character for consistency
  targetDate <- as.character(
    as.Date(
      format(
        Sys.time(), '%Y-%m-%d', tz= tz)))
  targetDate <- as.character(targetDate)
  
  # Get today's date
  today <- targetDate 
  
  # Get today's date
  todayStart <- as.POSIXct(today, tz = 'America/New_York') + .0001 
  
  # Get today's date
  todayEnd <- as.POSIXct(today, tz = 'America/New_York') + lubridate::days(1) - 1
  
  # Get yesterday's date
  yesterday <- as.Date(today) - 1
  
  # This Week Start
  thisWeekStart <- as.Date(lubridate::floor_date(as.POSIXct(todayStart, tz = 'America/New_York') - 1, 
                                                 unit = "week")) + lubridate::days(1)
  # This Week Ending
  thisWeekEnd <- as.Date(lubridate::ceiling_date(as.POSIXct(todayStart, tz = 'America/New_York') - 1, 
                                                 unit = "week")) 
  # Last Week Start
  lastWeekStart <- thisWeekStart - lubridate::weeks(1)
  
  # Last Week Ending
  lastWeekEnd <- thisWeekEnd - lubridate::weeks(1)
  
  # Previous Week Start 2 weeks ago
  previousWeekStart <- lastWeekStart - lubridate::weeks(1)
  
  # Previous Week Ending 2 weeks ago
  previousWeekEnd <- lastWeekEnd - lubridate::weeks(1)
  
  # This Month Ending
  thisMonthStart <- as.Date(lubridate::floor_date(as.POSIXct(todayStart, tz = 'America/New_York'), 
                                                  unit = "month"))
  # This Month Ending
  thisMonthEnd <- as.Date(lubridate::ceiling_date(as.POSIXct(todayEnd, tz = 'America/New_York'), 
                                                  unit = "month")) - 1
  
  # Last Month Ending
  lastMonthStart <- thisMonthStart - months(1)
  
  # Last Month Ending
  lastMonthEnd <- as.Date(lubridate::ceiling_date(lastMonthStart, 
                                                  unit = "month")) - 1
  
  # Previous Month Start 2 months ago
  previousMonthStart <- lastMonthStart - months(1) 
  
  # Previous Month Ending 2 months ago
  previousMonthEnd <- as.Date(lubridate::ceiling_date(previousMonthStart, 
                                                      unit = "month")) - 1
  
  # This Quarter Start
  thisQuarterStart <- as.Date(lubridate::floor_date(as.POSIXct(todayStart, tz = 'America/New_York'), 
                                                    unit = "quarter"))
  
  # This Quarter Ending
  thisQuarterEnd <- as.Date(lubridate::ceiling_date(as.POSIXct(todayEnd, tz = 'America/New_York'), 
                                                    unit = "quarter")) - 1
  
  # Last Quarter Ending
  lastQuarterEnd <- as.Date(lubridate::floor_date(as.POSIXct(todayEnd, tz = 'America/New_York'), 
                                                  unit = "quarter")) - 1
  
  # Last Quarter Start
  lastQuarterStart <- as.Date(lubridate::floor_date(lastQuarterEnd, 
                                                    unit = "quarter"))
  
  # Previous Quarter Ending 2 quarters ago
  previousQuarterEnd <- as.Date(lubridate::floor_date(lastQuarterEnd, 
                                                      unit = "quarter")) - 1
  
  # Previous Quarter Start
  previousQuarterStart <- as.Date(lubridate::floor_date(previousQuarterEnd, 
                                                        unit = "quarter"))
  
  # This Year Start
  thisYearStart <- as.Date(lubridate::floor_date(as.POSIXct(todayStart, tz = 'America/New_York'), 
                                                 unit = "year")) 
  
  # This Year Ending
  thisYearEnd <- as.Date(lubridate::ceiling_date(as.POSIXct(todayEnd, tz = 'America/New_York'), 
                                                 unit = "year")) - 1
  # Last Year Start
  lastYearStart <-  thisYearStart - lubridate::years(1)
  
  # Last Year Ending
  lastYearEnd <- thisYearEnd - lubridate::years(1)
  
  # Previous Year Start
  previousYearStart <-  lastYearStart - lubridate::years(1)
  
  # Previous Year Ending
  previousYearEnd <- lastYearEnd - lubridate::years(1)
  
  # Create data-frame of all of the dates
  dates <- data.frame(today,
                      todayStart,
                      todayEnd,
                      yesterday,
                      thisWeekStart,
                      thisWeekEnd,
                      lastWeekStart,
                      lastWeekEnd,
                      previousWeekStart,
                      previousWeekEnd,
                      thisMonthStart,
                      thisMonthEnd,
                      lastMonthStart,
                      lastMonthEnd,
                      previousMonthStart,
                      previousMonthEnd,
                      thisQuarterStart,
                      thisQuarterEnd,
                      lastQuarterStart,
                      lastQuarterEnd,
                      previousQuarterStart,
                      previousQuarterEnd,
                      thisYearStart,
                      thisYearEnd,
                      lastYearStart,
                      lastYearEnd,
                      previousYearStart,
                      previousYearEnd,
                      stringsAsFactors = FALSE)
  # Return the dates                  
  return(dates)
}