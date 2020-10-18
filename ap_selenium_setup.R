#' Creates and open a remote driver on docker through the given port that is 
#' defaulted to 4445 and on a browser defualted to firefox. This is set to the 
#' global environment in remDr for use in all other function
#'
#' @param docker_port A Port Number
#' @param browser_name A String
#' 
init_remDr <- function(docker_port = 4445L, 
                       browser_name = "firefox") {
  # init_remDr <- RSelenium::remoteDriver(
  #   browserName = browser_name,
  #   remoteServerAddr = "localhost", 
  #   port = docker_port,
  #   extraCapbilities = RSelenium::makeFirefoxProfile(list(
  #     browser.download.dir = "~/Projects/Programs/attackpoint_api/training_csv/",
  #     browser.helperApps.neverAsk.saveToDisk = "csv"
  #   ))
  # )
  
  init_remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = docker_port,
    browserName = browser_name
  )
  
  init_remDr$open()
  return(init_remDr)
}


#' Tries to log into Attackpoint using the given username and password
#'
#' @param username A String
#' @param password A String
#'
#' @return A Logical
#' @export
#'
#' @examples
try_login <- function(remDr, usr, psw) {
  login_url <- "https://www.attackpoint.org/login.jsp?returl=https%3A%2F%2Fwww.attackpoint.org%2F"
  remDr$navigate(login_url)

  
  remDr$findElement("name", "username")$sendKeysToElement(list(usr))
  remDr$findElement("name", "password")$
    sendKeysToElement(list(psw, key = "enter"))
  
  ap_home_url = c("www.attackpoint.org", "https://www.attackpoint.org/")
  
  remDr$getCurrentUrl() %in% ap_home_url
}


