##    This module loads all URL information to call previous vintage from FRED API.
##    Improve documentation universality, and naming conventions.
##

## Base url setters and getters
set.BaseURL <- function() {

  return("http://api.stlouisfed.org/fred")

}

get.BaseURL <- function() {

  return(set.BaseURL())

}


## Series directory setter and getter
set.Dir <- function() {

  return('series')

}

get.Dir <- function() {

  return(set.Dir())

}

##Type, i.e. 'observations', setter and getter
set.Type <- function() {

  return('observations')

}

get.Type <- function() {

  return(set.Type())

}

## Units setter and getter. Set to 'change' by default/
set.Units <- function() {

  base <- "units"
  units <- "lin"
  return(paste(base,units,sep="="))

}

get.Units <- function() {

  return(set.Units())

}

## File type setter and getter
set.fileType <- function() {

  base <- "file_type"
  type <- "json"
  return(paste(base,type,sep="="))

}

get.fileType <- function() {

  return(set.fileType())

}

## API key setter and getter
set.APIKey <- function() {

  base <- "api_key"
  key <- "76bb1186e704598b725af0a27159fdfc"
  return(paste(base,key,sep="="))

}

get.APIKey <- function() {

  return(set.APIKey())

}

## Directory url setter and getter <<- http://api.stlouisfed.org/fred/series
set.dirURL <- function() {

  base <- get.BaseURL()
  dir <- get.Dir()

  return(paste(base,dir,sep="/"))
}

get.dirURL <- function() {

  return(set.dirURL())

}

## Type url setter and getter <<- http://api.stlouisfed.org/fred/series/observations
set.typeURL <- function() {

  base <- get.dirURL()
  type <- get.Type()

  return(paste(base,type,sep="/"))

}

get.typeURL <- function() {

  return(set.typeURL())

}

# ## Pull metadata JSON file to determine appropriate window size.
# get.metadata <- function(series) {

#   # pulls information about series from API
#   # needs restructured
#   base <- get.dirURL()
#   key <- get.APIKey()
#   api <- paste(base,key,sep="?")
#   series.base <- "series_id"
#   series.id <- paste(series.base,series,sep="=")
#   file.type <- get.fileType()
#   meta.url <- paste(api,series.id,file.type,sep="&")

#   return(fromJSON(meta.url))

# }

# get.allvintages <- function(series) {

#   base <- get.dirURL()
#   type <- "vintagedates"
#   vints <- paste(base,type,sep="/")
#   key <- get.APIKey()
#   api <- paste(vints,key,sep="?")
#   series.base <- "series_id"
#   series.id <- paste(series.base,series,sep="=")
#   file.type <- get.fileType()
#   vint.query <- paste(api,series.id,file.type,sep="&")

#   object <- fromJSON(vint.query)

#   return(data.frame(object$vintage_dates))

# }


## Url to pull series observations
set.finalURL<- function() {

  base <- get.typeURL()
  key <- get.APIKey()
  api <- paste(base,key,sep="?")
  file.type <- get.fileType()
  units <- get.Units()
  url <- paste(api,file.type,units,sep="&")

  return(url)

}

get.finalURL <- function() {

  return(set.finalURL())

}

# get.vintage <- function(date) {

#   base <- get.finalURL()
#   param <- "vintage_dates"
#   vint <- date
#   query <- paste(param,vint,sep="=")

#   return(paste(base,query,sep="&"))

# }

get.JSON <- function(id) {

  base<- get.finalURL()
  series<- paste(base,'&series_id=',id,sep="")

  # Get json from url. Error out if url is not successful.
  if (url_success(series) == TRUE) {

    return(fromJSON(series))

  } else if (url_success(series) == FALSE) {

    print("There was an issue when calling the API. Please check that the series id is correct.")
    # Insert error trapping
    quit()

  } else {

    return("something has gone horribly awry. turn back, now.")
    quit()
  }

}

## Convert JSON file to data frame and scale values
get.data <- function(object) {

  # Convert JSON object to data frame for processing
  data <- data.frame(object$observations)

  # Drop realtime start and end
  drops <- c("realtime_start","realtime_end")
  data <- data[,!(names(data) %in% drops)]

  # Convert observation date to appropriate format for time-series analysis
  data$date <- strptime(data$date, format="%Y-%m-%d")

  # Convert to numeric type, ignore NA coercion warning
  data$value <- suppressWarnings(as.numeric(data$value))

  # Drop NA values.
  data <- na.omit(data)

  # Standardize data in order to detect outliers
  data$value <- scale(data$value)

  return(data)

  ###########################################################################################
  ##### The data frame should be immutable at this point! Do not attempt to change it! ######
  ###########################################################################################
}