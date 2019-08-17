library(RSelenium)
library(magrittr)

source("ap_selenium.R")

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
init_attackpoint <- function(username, password, docker_port = 4445L) {
  if(missing(username) || is.null(username)) error("username required")
  if(missing(password) || is.null(password)) error("password required")
  
  init_remDr(docker_port)
  
  if(try_login(username, password)) username
  else "Login Fail"
}

#' username: Hadron Helper
#' password: copper12
#'

add_training <- function(time, date = NULL, session = NULL, workout = "Training", intensity = 3, distance = NULL, climb = NULL, avg_hr = NULL, max_hr = NULL, description = NULL) {
  do_add_training(time, workout = workout)
}

