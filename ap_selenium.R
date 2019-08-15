init_rem_dr <- function(docker_port = 4445L) {
  init_rem_dr <- remoteDriver(remoteServerAddr = "localhost", 
                              port = docker_port, browserName = "firefox")
  init_rem_dr$open()
  rem_dr <<- init_rem_dr
}

try_login <- function(username, password) {
  rem_dr$navigate("https://www.attackpoint.org/login.jsp?returl=https%3A%2F%2Fwww.attackpoint.org%2F")
  
  rem_dr$findElement(using = "name", "username")$
    sendKeysToElement(list(username))
  rem_dr$findElement(using = "name", "password")$
    sendKeysToElement(list(password, key = "enter"))
  
  rem_dr$getCurrentUrl() == "https://www.attackpoint.org/"
}

do_add_training <- function(time, date = NULL, session = NULL, workout = "Training", intensity = 3, distance = NULL, climb = NULL, avg_hr = NULL, max_hr = NULL, desc = NULL) {
  rem_dr$navigate("https://www.attackpoint.org/newtraining.jsp")
  
  rem_dr$findElement(using = "name", "sessionlength")$
    sendKeysToElement(list(toString(time)))
  rem_dr$findElement(using = "xpath", "/html/body/div[1]/div[4]/form/p[3]/input")$
    clickElement()
}