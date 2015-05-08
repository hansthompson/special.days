#' Find holidays in your date object
#' 
#' @param my_datetime An object of class POSIXct/POSIXt. 
#' @return A character vector of holidays.
#' @export
#' @examples
#' require(lubridate)
#' get_holidays(ymd_hms("12-15-00 13:00:00"))
get_holidays <- function(my_datetime) {
    library(lubridate)
    
    first_day <- function(dates, event_day = event_day) {
        dates <- wday(dates)
        x <- rep(NA, length(dates))
        day_of_the_week <- event_day
        day_of_the_week_ahead <- event_day + 7
        x[dates < day_of_the_week] <- day_of_the_week - dates[dates < day_of_the_week]
        x[dates == day_of_the_week] <- 0
        x[dates > day_of_the_week] <- day_of_the_week_ahead - dates[dates > day_of_the_week]
        days(x)
    }
    
    last_day <- function(dates, event_day = event_day) {
        dates <- wday(dates)
        x <- rep(NA, length(dates))
        day_of_the_week <- event_day
        day_of_the_week_before <- event_day - 7
        x[dates < day_of_the_week] <- day_of_the_week_before - dates[dates < day_of_the_week]
        x[dates == day_of_the_week] <- 0
        x[dates > day_of_the_week] <- day_of_the_week - dates[dates > day_of_the_week]
        days(x)
    }
    
    just_dates <- my_datetime - hours(hour(my_datetime))
    years_in_data <- unique(year(just_dates))
    #Jan 1st
    New_Years_Day <- ymd(paste0(years_in_data, "-01-01"), tz = "America/Anchorage")
    #Dec. 31
    New_Years_Eve <- New_Years_Day - days(1)
    #third monday of jan
    MLKJR <- New_Years_Day + first_day(New_Years_Day, 2) + weeks(3)
    #third monday of feb
    year_and_month <- New_Years_Day + months(1)
    Presidents <- year_and_month + first_day(year_and_month, 2) + weeks(3)
    #last monday of may
    year_and_month <- New_Years_Day + months(5) - days(1)
    Memorial <- year_and_month + last_day(year_and_month, 2)
    #Self explanitory
    July4 <- New_Years_Day + months(6) + days(4)
    #first monday of september
    Labor <- New_Years_Day + months(8) + first_day(New_Years_Day, 2)
    #fourth thursday of novemeber
    year_and_month <- New_Years_Day + months(11) - days(1)
    Thanksgiving   <- year_and_month + last_day(year_and_month, 5)
    #day after thanksgiving
    BlackFri <- Thanksgiving + days(1)
    #Dec. 18 - Dec. 23
    Week_Before_Christmas_start <- New_Years_Day + months(11) + days(17)  
    Week_Before_Christmas <- c(Week_Before_Christmas_start, 
                               Week_Before_Christmas_start + days(1), 
                               Week_Before_Christmas_start + days(2),
                               Week_Before_Christmas_start + days(3),
                               Week_Before_Christmas_start + days(4),
                               Week_Before_Christmas_start + days(5),
                               Week_Before_Christmas_start + days(6))
    #Dec. 24
    Christmas_Eve <- New_Years_Day + months(11) + days(23)
    #Dec. 25
    Christmas <- New_Years_Day + months(11) + days(24)
    #Dec. 26 - Dec. 31
    Week_After_Christmas_start <- New_Years_Day + months(11) + days(25)
    Week_After_Christmas <- c(Week_After_Christmas_start, 
                              Week_After_Christmas_start + days(1), 
                              Week_After_Christmas_start + days(2),
                              Week_After_Christmas_start + days(3),
                              Week_After_Christmas_start + days(4),
                              Week_After_Christmas_start + days(5),
                              Week_After_Christmas_start + days(6))
    #Oct. 18
    Alaska <- New_Years_Day + months(9) + days(17)
    #Last monday in march
    year_and_month <- New_Years_Day + months(2) - days(1)
    Seward <- year_and_month - last_day(year_and_month, 2)
    
    Holidays <- rep("None", length(just_dates))
    Holidays[just_dates %in% New_Years_Day] <- "New_Years_Day"
    Holidays[just_dates %in% New_Years_Eve] <- "New_Years_Eve"
    Holidays[just_dates %in% MLKJR] <- "MLKJR"
    Holidays[just_dates %in% Presidents] <- "Presidents"
    Holidays[just_dates %in% Memorial] <- "Memorial"
    Holidays[just_dates %in% July4] <- "July4"
    Holidays[just_dates %in% Labor] <- "Labor"
    Holidays[just_dates %in% Thanksgiving] <- "Thanksgiving"
    Holidays[just_dates %in% BlackFri] <- "BlackFri"
    Holidays[just_dates %in% Week_Before_Christmas] <- "Week_Before_Christmas"
    Holidays[just_dates %in% Christmas_Eve] <- "Christmas_Eve"
    Holidays[just_dates %in% Christmas] <- "Christmas"
    Holidays[just_dates %in% Week_After_Christmas] <- "Week_After_Christmas"
    Holidays[just_dates %in% Alaska] <- "Alaska"
    Holidays[just_dates %in% New_Years_Day] <- "New_Years_Day"
    Holidays[just_dates %in% Seward] <- "Seward"
    return(factor(Holidays))
}