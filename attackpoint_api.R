library(RSelenium)
library(magrittr)

source("ap_selenium.R")
source("add_training.R")

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

#' username: Hadron Helper
#' password: copper12
#'

add_training <-
  function(duration, date = NULL, time = NULL, activity = NULL, 
           workout = "Training", intensity = 3, distance = NULL, climb = NULL, 
           avg_heart_rate = NULL, max_heart_rate = NULL, description = NULL) {
  try_add_training(duration, date = date, time = time, act = activity, 
                   workout = workout, intensity = intensity, dist = distance, 
                   climb = climb, avg_hr = avg_heart_rate, 
                   max_hr = max_heart_rate, desc = description)
}

