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

do_add_training <- function(time, date = NULL, session = NULL, workout = "Training", intensity = 3, distance = NULL, climb = NULL, avg_hr = NULL, max_hr = NULL, desc = NULL) {
  remDr$navigate("https://www.attackpoint.org/newtraining.jsp")
  
  # Set the time of the session
  remDr$findElement("name", "sessionlength")$
    sendKeysToElement(list(toString(time)))
  
  # Set the session workout type
  workout_option <- paste0("//select[@id = 'workouttypeid']/option[@value = '", 
                           get_workout_id(workout), "']", sep = "")
  remDr$findElement("xpath", workout_option)$clickElement()
  
  # Set the distance of the session
  remDr$findElement("name", "distance")$
    sendKeysToElement(list(toString(distance)))
  
  remDr$findElement(using = "xpath", "/html/body/div[1]/div[4]/form/p[3]/input")$
    clickElement()
}

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

