library(RSelenium)

init_attackpoint <- function(username, password) {
  if(missing(username) || is.null(username)) error("username required")
  if(missing(password) || is.null(password)) error("password required")
  
  system("docker run --name attackpoint_container -d -p 4445:4444 selenium/standalone-firefox:2.53.1")
  
  rem_dr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, 
                          browserName = "firefox")
  rem_dr$open()
  
  rem_dr$navigate("https://www.attackpoint.org/login.jsp?returl=https%3A%2F%2Fwww.attackpoint.org%2F")

  print(rem_dr$getCurrentUrl())  
}

close_attackpoint <- function() {
  
}

