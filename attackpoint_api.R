library(RSelenium)
library(magrittr)

source("ap_selenium.R")
source("add_training.R")
source("add_shoes.R")

#' username: Hadron Helper
#' password: copper12

#' Initializes a session in a user's account on Attackpoint by attempting to 
#' login. If this succedes the username is returned if it fails then "Login
#' Fail" is returned
#' 
#' @param username A String
#' @param password A String
#'
#' @return A String
#' @export
#' 
#' @examples
init_attackpoint <- function(username, password, docker_port = 4445L,
                             browser_name = "firefox") {
  if(missing(username) || is.null(username)) error("username required")
  if(missing(password) || is.null(password)) error("password required")
  
  init_remDr(docker_port)
  
  if(try_login(username, password)) username
  else "Login Fail"
}

#' Fills out the "Add Training" form on Attackpoint
#'
#' @param duration A Number 
#' @param date A Date
#' @param time A Number between 0 and 23
#' @param activity A String
#' @param workout A String of a Valid Attackpoint Workout
#' @param intensity A Number between 0 and 5
#' @param distance A Number
#' @param climb A Number
#' @param avg_heart_rate A Number
#' @param max_heart_rate A Number
#' @param rest_heart_rate A Number
#' @param sleep A Number
#' @param weight A Number
#' @param is_injured A Boolean
#' @param is_sick A Boolean
#' @param is_rest_day A Boolean
#' @param description A String with simple HTML
#'
add_training <- function(duration, date = NULL, time = NULL, activity = NULL, 
                         workout = "Training", intensity = 3, distance = NULL, 
                         climb = NULL, avg_heart_rate = NULL, 
                         max_heart_rate = NULL, rest_heart_rate = NULL, 
                         sleep = NULL, weight = NULL, is_injured = FALSE, 
                         is_sick = FALSE, is_rest_day = FALSE, 
                         description = NULL) {
  
  try_add_training(duration, date = date, time = time, act = activity, 
                   workout = workout, intensity = intensity, dist = distance, 
                   climb = climb, avg_hr = avg_heart_rate, 
                   max_hr = max_heart_rate, rest_hr = rest_heart_rate, 
                   sleep = sleep, weight = weight, is_injured = is_injured, 
                   is_sick = is_sick, is_rest_day = is_rest_day, 
                   desc = description)
}

#' Fills out the "Add/Edit Shoes" form for adding a new pair of shoes
#'
#' @param shoe_name A String
#' @param new_date A Date
#' @param init_miles A Number >= 0
#' @param is_retired A Boolean
#'
#' @examples 
#'
add_shoes <- function(shoe_name, new_date = NULL, init_miles = 0, 
                      is_retired = FALSE) {
  try_add_shoes(shoe_name = shoe_name, new_date = new_date, 
                init_miles = init_miles, is_retired = is_retired)
  
}

