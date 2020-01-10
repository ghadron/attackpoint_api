#' Fills out the "Add/Edit Shoes" form for adding a new pair of shoes
#'
#' @param shoe_name A String
#' @param new_date A Date
#' @param init_miles A Number >= 0
#' @param is_retired A Boolean
#'
#' @examples 
#'
try_add_shoes <- function(remDr, 
                          shoe_name,
                          new_date = NULL, 
                          init_miles = 0, 
                          is_retired = FALSE) {
  remDr$navigate("https://www.attackpoint.org/editshoes.jsp")
  remDr$findElement("name", "name")$
    sendKeysToElement(list(toString(shoe_name)))
  set_new_date(remDr, new_date)
  set_init_miles(remDr, init_miles)
  if(isTRUE(is_retired))remDr$findElement("name", "retired")$clickElement()
  remDr$findElement("xpath", "/html/body/div[1]/div[4]/form/p/input")$
    clickElement()
}

#' Sets the shoes new date
#' 
#' @param new_date A Date
#' 
set_new_date <- function(remDr, new_date) {
  if(is.null(new_date) | is.na(new_date))return()
  
  month_option_xpath <- 
    paste0("//select[@id = 'datenew-month']/option[@value = '", 
           format(as.Date(new_date), "%m"), "']", sep = "")
  remDr$findElement("xpath", month_option_xpath)$clickElement()
  
  day_option_xpath <- 
    paste0("//select[@id = 'datenew-day']/option[@value = '",
           format(as.Date(new_date), "%m"), "']", sep = "")
  remDr$findElement("xpath", day_option_xpath)$clickElement()
  
  remDr$findElement("name", "datenew-year")$
    sendKeysToElement(list(toString(format(as.Date(new_date), "%Y"))))
}

#' Sets the initial miles of the shoes
#'
#' @param init_miles A Number >= 0
#'
set_init_miles <- function(remDr, init_miles) {
  if (!(length(init_miles) > 0 & is.numeric(init_miles))) {
    stop(paste(init_miles, "is not a number"))
  } else if (init_miles < 0)stop(paste(init_miles, "is not positive"))

  remDr$findElement("name", "initialmiles")$
    sendKeysToElement(list(toString(init_miles)))
}

