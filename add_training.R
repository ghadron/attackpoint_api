
#' Fills out the "Add Training" form on Attackpoint
#'
#' @param duration A Number 
#' @param date A Date
#' @param time A Number between 0 and 23
#' @param act A String
#' @param workout A String of a Valid Attackpoint Workout
#' @param intensity A Number between 0 and 5
#' @param dist A Number
#' @param climb A Number
#' @param avg_hr A Number
#' @param max_hr A Number
#' @param desc A String with simple HTML 
#'
#' @examples
#' 
try_add_training <- 
  function(duration, date = NULL, time = NULL, act = NULL, workout = "Training", 
           intensity = 3, dist = NULL, climb = NULL, avg_hr = NULL, 
           max_hr = NULL, desc = NULL) {
    remDr$navigate("https://www.attackpoint.org/newtraining.jsp")
    
    set_duration(duration)
    set_date(date)
    set_time(time)
    set_act(act)
    set_workout(workout)
    set_intensity(intensity)
    set_dist(dist)
    set_climb(climb)
    set_avg_hr(avg_hr)
    set_max_hr(max_hr)
    set_desc(desc)
    
    remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/p[3]/input")$
      clickElement()
}

#' Inputs the time of the session
#'
#' @param duration A Number
#'
set_duration <- function(duration) {
  remDr$findElement("name", "sessionlength")$
    sendKeysToElement(list(toString(duration)))
}

#' Inputs the date of the session
#' 
#' @param date A Date
#' 
set_date <- function(date) {
  if(is.null(date))return()
  
  month_option_xpath <- 
    paste0("//select[@id = 'session-month']/option[@value = '", 
           format(as.Date(date), "%m"), "']", sep = "")
  remDr$findElement("xpath", month_option_xpath)$clickElement()
  
  day_option_xpath <-
    paste0("//select[@id = 'session-day']/option[@value = '", 
           format(as.Date(date), "%d"), "']", sep = "")
  remDr$findElement("xpath", day_option_xpath)$clickElement()
  
  remDr$findElement("name", "session-year")$
    sendKeysToElement(list(format(as.Date(date), "%Y")))
}

#' Inputs the time of day the session took place
#' 
#' @param time A Number between 0 and 23
#' 
set_time <- function(time) {
  if(is.null(time))return()
  
  time_option_xpath <- 
    paste0("//select[@id = 'sessionstarthour']/option[@value = '", 
           toString(time), "']", sep = "")
  remDr$findElement("xpath", time_option_xpath)$clickElement()
}

#' Inputs the activity/sport of the session
#'
#' @param act A String
#'
set_act <- function(act) {
  if(is.null(act))return()
  
  option_pos <- 1
  act_select_xpath <- "//select[@id = 'activitytypeid']/option["
  act_type <- 
    remDr$findElement("xpath", paste(act_select_xpath, option_pos, "]"))
  
  while (act_type$getElementText() != "New Type (enter here-->)") {
    
    if (act_type$getElementText() == act) {
      act_type$clickElement()
      return()
    }
    
    option_pos <- option_pos + 1
    act_type <- 
      remDr$findElement("xpath", paste(act_select_xpath, option_pos, "]"))
  }
  
  stop(paste(act, "is not a set activity"))
}

#' Inputs the workout type of the session
#' 
#' @param workout A String of a Valid Attackpoint Workout 
#' 
set_workout <- function(workout) {
  workout_option_xpath <-
    paste0("//select[@id = 'workouttypeid']/option[@value = '",
           get_workout_id(workout), "']", sep = "")
  remDr$findElement("xpath", workout_option_xpath)$clickElement()
}

#' Maps the given workout name to the HTML Select value
#' 
#' @param workout A String of a Valid Attackpoint Workout
#' 
get_workout_id <- function(workout) {
  workout <- tolower(workout)
  if(workout == "training")return("1")
  else if(workout == "race")return("2")
  else if(workout == "long")return("3")
  else if(workout == "intervals")return("4")
  else if(workout == "hills")return("5")
  else if(workout == "tempo")return("6")
  else if(workout == "warm up/down")return("7")
  else if(workout == "none" || workout == "[none]")return("null")
  else stop(paste(workout, "is an invalid workout type"))
}

#' Inputs the intensity of the session
#' 
#' @param intensity A Number between 0 and 5
#' 
set_intensity <- function(intensity) {
  intensity_option_xpath <- 
    paste0("//select[@id = 'intensity']/option[@value = '", intensity, "']", 
           sep = "")
  remDr$findElement("xpath", intensity_option_xpath)$clickElement()
}

#' Inputs the distance of the session
#'
#' @param dist A Number
#'
set_dist <- function(dist) {
  remDr$findElement("name", "distance")$
    sendKeysToElement(list(toString(dist)))
}

#' Inputs the climb of the session
#' 
#' @param climb A Number
#' 
set_climb <- function(climb) {
  remDr$findElement("name", "climb")$
    sendKeysToElement(list(toString(climb)))
}

#' Inputs the average heart rate of the session
#' 
#' @param avr_hr A Number
#' 
set_avg_hr <- function(avg_hr) {
  remDr$findElement("name", "ahr")$
    sendKeysToElement(list(toString(avg_hr)))
}

#' Inputs the max heart rate of the session
#' 
#' @param max_hr A Number
#' 
set_max_hr <- function(max_hr) {
  remDr$findElement("name", "mhr")$
    sendKeysToElement(list(toString(max_hr)))
}

#' Inputs the description of the session
#' 
#' @param desc A String with simple HTML
#' 
set_desc <- function(desc) {
  remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/textarea")$
    sendKeysToElement(list(toString(desc)))
}

