try_add_training <- 
  function(duration, date = NULL, time = NULL, workout = "Training",
           intensity = 3, dist = NULL, climb = NULL, avg_hr = NULL, 
           max_hr = NULL, desc = NULL) {
    remDr$navigate("https://www.attackpoint.org/newtraining.jsp")
    
    set_duration(duration)
    set_date(date)
    set_time(time)
    set_workout(workout)
    set_intensity(intensity)
    set_dist(dist)
    set_climb(climb)
    set_avg_hr(avg_hr)
    set_max_hr(max_hr)
    set_desc(desc)
    
    remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/p[3]/input")$
      clickElement()
}

set_duration <- function(duration) {
  remDr$findElement("name", "sessionlength")$
    sendKeysToElement(list(toString(duration)))
}

set_date <- function(date) {
  if(is.null(date))return()
  month_option_xpath <- 
    paste0("/html/body/div[1]/div[4]/form/select[1]/option[@value = '", 
           format(as.Date(date), "%m"), "']", sep = "")
  remDr$findElement("xpath", month_option_xpath)$clickElement()
  day_option_xpath <-
    paste0("/html/body/div[1]/div[4]/form/select[2]/option[@value = '", 
           format(as.Date(date), "%d"), "']", sep = "")
  remDr$findElement("xpath", day_option_xpath)$clickElement()
  remDr$findElement("name", "session-year")$
    sendKeysToElement(list(format(as.Date(date), "%Y")))
}

set_time <- function(time) {
  if(is.null(time))return();
  time_option_xpath <- 
    paste0("//select[@id = 'sessionstarthour']/option[@value = '", 
           toString(time), "']", sep = "")
  remDr$findElement("xpath", time_option_xpath)$clickElement()
}

set_workout <- function(workout) {
  workout_option_xpath <-
    paste0("//select[@id = 'workouttypeid']/option[@value = '", 
           get_workout_id(workout), "']", sep = "")
  remDr$findElement("xpath", workout_option_xpath)$clickElement()
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

set_intensity <- function(intensity) {
  intensity_option_xpath <- 
    paste0("//select[@id = 'intensity']/option[@value = '", intensity, "']", 
           sep = "")
  remDr$findElement("xpath", intensity_option_xpath)$clickElement()
}

set_dist <- function(distance) {
  remDr$findElement("name", "distance")$
    sendKeysToElement(list(toString(distance)))
}

set_climb <- function(climb) {
  remDr$findElement("name", "climb")$
    sendKeysToElement(list(toString(climb)))
}

set_avg_hr <- function(avg_hr) {
  remDr$findElement("name", "ahr")$
    sendKeysToElement(list(toString(avg_hr)))
}

set_max_hr <- function(max_hr){
  remDr$findElement("name", "mhr")$
    sendKeysToElement(list(toString(max_hr)))
}

set_desc <- function(desc) {
  remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/textarea")$
    sendKeysToElement(list(toString(desc)))
}

