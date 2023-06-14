# Libraries :
{
  
  library(RPostgreSQL)
  # List of required libraries
  required_libraries <- c("pushoverr", "rjson", "jsonlite", "httr", "gmodels",
                          "foreach", "hms", "iterators", "chron",
                          "sqldf", "dplyr", "stringr", "psych", "tm",
                          "tidyr", "tidyverse", "RSQLite", "readr", "pastecs",
                          "DBI", "RODBC", "odbc", "pryr", "readxl", "lubridate",
                          "sqldf", "data.table", "purrr", "plyr")
  
  # Function to check if a package is installed and install it if it's not
  install_if_not_installed <- function(package) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    }
  }
  
  # Loop through the required libraries and install them if needed
  for (library in required_libraries) {
    install_if_not_installed(library)
  }
  
  drv <- dbDriver("PostgreSQL")
  
  
  pacman::p_load(pushoverr,rjson,jsonlite,httr,gmodels,foreach,hms,iterators,chron,sqldf,dplyr, 
                 stringr,psych, tm,tidyr,tidyverse,RSQLite,readr,pastecs
                 ,DBI,RODBC,DBI,odbc,pryr,readxl,lubridate,sqldf);
  library(data.table)
  library(jsonlite)
  library(purrr)
  library(plyr)
  library(httr)
}

# Functions : general
{
  hexa_to_text <- function(arg) {
    s <- paste0(arg, sep = " ")
    h <- sapply(seq(1, 2, by = 2), function(x)
      substr(s, x, x + 1))
    y <- rawToChar(as.raw(strtoi(h, 16L)))
    return(jsonlite::prettify(y))
  }
}

# Functions data :
{
  # General:
  {
    char_to_dateTime  <- function(x) {
      z <- as.POSIXlt(x, tz = Sys.timezone())
      return(z)
    }
    
    char2Todate       <- function(x) {
      z <- as.Date(x)
      return(z)
    }
    
    char_to_date      <- function(x) {
      z <-
        as.Date(paste0(substr(x, 1, 4), '-', substr(x, 5, 6), '-', substr(x, 7, 8)))
    }
    
    char_toTime       <- function(x) {
      padleft0_4 <- function(x) {
        z <- str_pad(x, 4, pad = c("0"))
        return(z)
      }
      
      separatetime0 <- function(x) {
        z <- paste0(substr(x, 1, 2), "h", substr(x, 3, 4))
        return(z)
      }
      z <- separatetime0(padleft0_4(x))
      return(z)
    }
    
    char2_toTime      <- function(x) {
      z <- paste0(substr(x, 1, 2), "h", substr(x, 4, 5))
    }
    
    char3_toTime      <- function(x) {
      z <- paste0(substr(x, 1, 2), ":", substr(x, 4, 5))
    }
    
    char4_toTime      <- function(x) {
      x <- as.character(x)
      z <- paste0(substr(x, 1, 2), ":", substr(x, 4, 5))
      return(z)
    }
    
    datetime_to_date  <- function(x) {
      z <- as.Date(substr(x, 1, 10))
      return(z)
    }
    
    dt_form           <- function(dt, ts) {
      return(ifelse(is.na(dt), NA, as.character(ymd_hms(
        paste0(as.character(dt), " ", as.character(ts))
      ))))
    }
    
    dt_form2          <- function(dt, ts) {
      return(as.POSIXct(ifelse(
        is.na(dt), NA, as.character(ymd_hm(
          paste(as.character(dt), as.character(ts))
        ))
      )))
    }
    
    na_to_charempty   <- function(x) {
      return(ifelse(is.na(x), "", x))
    }
    
    fix_nahnah        <- function(x) {
      x <- ifelse(x == "NAhNA", "00:00", x)
    }
  }
  
  # functions utiles : values clean for labs ..
  {
    auto_affecGreaterThan <- function(var, max) {
      res <- ifelse(var > max, max, var)
    }
    auto_affecSmallerThan <- function(var, min) {
      res <- ifelse(var < min, min, var)
    }
    auto_bothLimits <- function(var, min, max) {
      res <- auto_affecGreaterThan(auto_affecSmallerThan(var, min), max)
    }
    
    divisionBy100 <- function(x) {
      y <- ifelse(is.na(x), NA, as.numeric(x) / 100)
      return(y)
    }
    
    na__1_func <- function(x) {
      y <- ifelse(is.na(x), "1", "")
      return(y)
    }
    
    which.nonnum <- function(x) {
      badNum <- is.na(suppressWarnings(as.numeric(as.character(x))))
      which(badNum & !is.na(x))
    }
    
    cleanValue <- function(x) {
      ind <- unlist(gregexpr('@', x))
      y <-
        ifelse(ind == -1L, gsub("[><]", "", x), gsub("[><]", "", substr(x, 1, ind -
                                                                          1)))
      y <- gsub("[mL/h]", "", gsub("[+]", "", x))
    }
    
    less_func <- function(var, min) {
      res <- ifelse(var < min , 1, NA)
      return(res)
    }
    
    out_func <- function(var, max) {
      res <- ifelse(var > max , 1, NA)
      return(res)
    }
    
    isBetweenTwoDates <- function(date, arrival, discharge) {
      discharge <- ifelse(is.na(discharge), Sys.Date(), discharge)
      z <- ifelse(date >= arrival & date <= discharge, TRUE, FALSE)
      return(z)
    }
    
  }
}

# Functions diverse :
{
  
  # set ta timer
  {
    set_timer <- function(duration_in_seconds) {
      library(tcltk)
      timer = duration_in_seconds
      pb <- tkProgressBar("Timer")
      start = Sys.time()
      while (TRUE) {
        elapsed = as.numeric(difftime(Sys.time(), start, units = 'secs'))
        remaining = timer - elapsed
        Sys.sleep(0.1)
        setTkProgressBar(pb,
                         remaining / timer,
                         label = sprintf("Starting in %i seconds", round(remaining)))
        if (remaining <= 0)
          break
      }
      Sys.sleep(1)
      close(pb)
    }
  }
  

  
 
  
  
  # Functions : notifications
  {
    push_notification <- function(message) {
      set_pushover_app(token = "a9czybjrpcj5tovv2sj3m9547fvx2h")
      pushover(message ,
               user = "utu2v9dmo21vi7rghrbb1j8imufo84")
    }
    
  }
}