get_training_csv <- function(remDr,
                             start_date = NULL, 
                             end_date = NULL) {
  if (is.null(start_date)) start_date <- as.Date(Sys.Date())
  if (is.null(end_date)) end_date <- as.Date(Sys.Date())
  
  remDr$navigate("https://www.attackpoint.org/reports.jsp?")
  
  set_start_date(remDr, start_date)
  set_end_date(remDr, end_date)
  
  remDr$findElement("xpath", "/html/body/div[1]/div[4]/form[2]/p/input")$
    clickElement()
}

set_start_date <- function(remDr, start_date) {
  month_option_xpath <- 
    paste0("//select[@id = 'from-month']/option[@value = '", 
           format(as.Date(start_date), "%m"), "']", sep = "")
  remDr$findElement("xpath", month_option_xpath)$clickElement()
  
  day_option_xpath <-
    paste0("//select[@id = 'from-day']/option[@value = '", 
           format(as.Date(start_date), "%d"), "']", sep = "")
  remDr$findElement("xpath", day_option_xpath)$clickElement()
  
  remDr$findElement("name", "from-year")$
    sendKeysToElement(list(format(as.Date(start_date), "%Y")))
}

set_end_date <- function(remDr, end_date) {
  month_option_xpath <- 
    paste0("//select[@id = 'to-month']/option[@value = '", 
           format(as.Date(end_date), "%m"), "']", sep = "")
  remDr$findElement("xpath", month_option_xpath)$clickElement()
  
  day_option_xpath <-
    paste0("//select[@id = 'to-day']/option[@value = '", 
           format(as.Date(end_date), "%d"), "']", sep = "")
  remDr$findElement("xpath", day_option_xpath)$clickElement()
  
  remDr$findElement("name", "to-year")$
    sendKeysToElement(list(format(as.Date(end_date), "%Y")))
}



