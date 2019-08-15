library(RSelenium)

#' Initializes a session in a user's account on Attackpoint
#' 
#'
#' @param username A String
#' @param password A String
#'
#' @return
#' @export
#' 
#' @examples
init_attackpoint <- function(username, password, docker_port = 4445L) {
  if(missing(username) || is.null(username)) error("username required")
  if(missing(password) || is.null(password)) error("password required")
  
  init_rem_dr <- remoteDriver(remoteServerAddr = "localhost", 
                              port = docker_port, browserName = "firefox")
  init_rem_dr$open()
  rem_dr <<- init_rem_dr
  
  if(try_ap_login(username, password)) username
  else "Login Fail"
}

try_ap_login <- function(username, password) {
  rem_dr$navigate("https://www.attackpoint.org/login.jsp?returl=https%3A%2F%2Fwww.attackpoint.org%2F")

  rem_dr$findElement(using = "name", "username")$
    sendKeysToElement(list(username))
  rem_dr$findElement(using = "name", "password")$
    sendKeysToElement(list(password, key = "enter"))
  
  rem_dr$getCurrentUrl() == "https://www.attackpoint.org/"
}

#' username: Hadron Helper
#' password: copper12
#'

close_attackpoint <- function() {
  
}

