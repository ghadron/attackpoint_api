
init_remDr <- function(docker_port = 4445L) {
  init_remDr <- remoteDriver(remoteServerAddr = "localhost", 
                              port = docker_port, browserName = "firefox")
  init_remDr$open()
  remDr <<- init_remDr
}

try_login <- function(username, password) {
  remDr$navigate("https://www.attackpoint.org/login.jsp?returl=https%3A%2F%2Fwww.attackpoint.org%2F")
  
  remDr$findElement("name", "username")$
    sendKeysToElement(list(username))
  remDr$findElement("name", "password")$
    sendKeysToElement(list(password, key = "enter"))
  
  remDr$getCurrentUrl() == "https://www.attackpoint.org/"
}

