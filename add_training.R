#' Fills out the "Add Training" form on Attackpoint
#'
#' @param duration A Number 
#' @param date A Date with formatting "%Y-%m-%d"
#' @param time A Number between 0 and 23
#' @param act A String
#' @param workout A String of a Valid Attackpoint Workout
#' @param intensity A Number between 0 and 5
#' @param dist A Number
#' @param climb A Number
#' @param avg_hr A Number
#' @param max_hr A Number
#' @param rest_hr A Number
#' @param sleep A Number
#' @param weight A Number
#' @param is_inj A Boolean
#' @param is_sick A Boolean
#' @param is_rest_day A Boolean
#' @param desc A String with simple HTML 
#'
#' @examples
#' 
try_add_training <- function(remDr,
                             duration, 
                             date = NULL, 
                             time = NULL, 
                             act = NULL, 
                             workout = "Training",
                             intensity = 3, 
                             dist = NULL, 
                             climb = NULL, 
                             avg_hr = NULL, 
                             max_hr = NULL, 
                             rest_hr = NULL, 
                             sleep = NULL, 
                             weight = NULL, 
                             is_inj = FALSE, 
                             is_sick = FALSE, 
                             is_rest_day = FALSE, 
                             desc = NULL) {
  ap_training_url <- "https://www.attackpoint.org/newtraining.jsp"
  remDr$navigate(ap_training_url)
  
  set_duration(remDr, duration)
  set_date(remDr, date)
  set_time(remDr, time)
  set_act(remDr, act)
  set_workout(remDr, workout)
  set_intensity(remDr, intensity)
  set_dist(remDr, dist)
  set_climb(remDr, climb)
  set_avg_hr(remDr, avg_hr)
  set_max_hr(remDr, max_hr)
  set_rest_hr(remDr, rest_hr)
  set_sleep(remDr, sleep)
  set_weight(remDr, weight)
  set_injured(remDr, is_inj)
  set_sick(remDr, is_sick)
  set_rest_day(remDr, is_rest_day)
  set_desc(remDr, desc)
    
  remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/p[3]/input")$
      clickElement()
}

#' Inputs the time of the session
#'
#' @param duration A Number
#'
set_duration <- function(remDr, duration) {
  remDr$findElement("name", "sessionlength")$
    sendKeysToElement(list(toString(duration)))
}

#' Inputs the date of the session
#' 
#' @param date A Date with the formatting "%Y-%m-%d"
#' 
set_date <- function(remDr, date = NULL) {
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
set_time <- function(remDr, time = NULL) {
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
set_act <- function(remDr, act) {
  if(is.null(act))return()
  
  option_pos <- 1
  act_select_xpath <- "//select[@id = 'activitytypeid']/option["
  act_type <- 
    remDr$findElement("xpath", paste(act_select_xpath, option_pos, "]"))
  
  end_of_acts <- "New Type (enter here-->)"
  while (act_type$getElementText() != end_of_acts) {
    s
    
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
set_workout <- function(remDr, workout = "Training") {
  workout_option_xpath <-
    paste0("//select[@id = 'workouttypeid']/option[@value = '",
           get_workout_id(workout), "']", sep = "")
  remDr$findElement("xpath", workout_option_xpath)$clickElement()
}

#' Maps the given workout name to the HTML Select value
#' 
#' @param workout A String of a Valid Attackpoint Workout
#' 
get_workout_id <- function(remDr, workout = "Training") {
  workout <- tolower(workout)
  if(workout == "training" | is.null(workout))return("1")
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
set_intensity <- function(remDr, intensity) {
  intensity_option_xpath <- 
    paste0("//select[@id = 'intensity']/option[@value = '", intensity, "']", 
           sep = "")
  remDr$findElement("xpath", intensity_option_xpath)$clickElement()
}

#' Inputs the distance of the session
#'
#' @param dist A Number
#'
set_dist <- function(remDr, dist) {
  remDr$findElement("name", "distance")$sendKeysToElement(list(toString(dist)))
}

#' Inputs the climb of the session
#' 
#' @param climb A Number
#' 
set_climb <- function(remDr, climb) {
  remDr$findElement("name", "climb")$sendKeysToElement(list(toString(climb)))
}

#' Inputs the average heart rate of the session
#' 
#' @param avr_hr A Number
#' 
set_avg_hr <- function(remDr, avg_hr) {
  remDr$findElement("name", "ahr")$sendKeysToElement(list(toString(avg_hr)))
}

#' Inputs the max heart rate of the session
#' 
#' @param max_hr A Number
#' 
set_max_hr <- function(remDr, max_hr) {
  remDr$findElement("name", "mhr")$sendKeysToElement(list(toString(max_hr)))
}

#' Inputs the resting heart rate
#'
#' @param rest_hr A Number
#'
set_rest_hr <- function(remDr, rest_hr) {
  remDr$findElement("name", "rhr")$sendKeysToElement(list(toString(rest_hr)))
}

#' Inputs of the sleep on the day of the session
#' 
#' @param sleep A Number
#' 
set_sleep <- function(remDr, sleep) {
  remDr$findElement("name", "sleep")$sendKeysToElement(list(toString(sleep)))
}

#' Inputs of the weight on the day of the session
#' 
#' @param weight A Number
#' 
set_weight <- function(remDr, weight) {
  remDr$findElement("name", "weight")$sendKeysToElement(list(toString(weight)))
}

#' Checks off injured checkbox if true
#' 
#' @param is_inj A Boolean
#' 
set_injured <- function(remDr, is_inj) {
  if (isTRUE(is_inj))remDr$findElement("name", "injured")$clickElement()
}

#' Checks off sick checkbox if true
#' 
#' @param is_sick A Boolean
#' 
set_sick <- function(remDr, is_sick) {
  if (isTRUE(is_sick))remDr$findElement("name", "sick")$clickElement()
}

#' Checks off rest day checkbox if true
#' 
#' @param is_rest_day A Boolean
#' 
set_rest_day <- function(remDr, is_rest_day) {
  if (isTRUE(is_rest_day))remDr$findElement("name", "restday")$clickElement()
}

#' Inputs the description of the session
#' 
#' @param desc A String with simple HTML
#' 
set_desc <- function(remDr, desc) {
  remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/textarea")$
    sendKeysToElement(list(toString(desc)))
}

