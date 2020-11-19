library(magrittr)
library(stringr)

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
  ap_shoes_url <- "https://www.attackpoint.org/editshoes.jsp" 
  remDr$navigate(ap_shoes_url)
  
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
set_new_date <- function(remDr, new_date = NULL) {
  if(is.null(new_date))return()
  
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

get_shoes <- function(remDr, 
                      shoe_name = NULL,
                      new_date = NULL, 
                      init_miles = NULL, 
                      is_retired = NULL) {
  ap_shoes_url <- "https://www.attackpoint.org/shoes.jsp"
  remDr$navigate(ap_shoes_url)
  
  shoe_table <- readHTMLTable(remDr$getPageSource()[[1]])[[1]] 
  shoe_table$V1 <- NULL
  shoe_table <- shoe_table[-1,]
  
  shoe_col_names <- c("name", "date_new", "miles", "kilometers", "total_time",
                      "climb", "sessions", "controls")
  index = 2
  for (name in shoe_col_names) {
    shoe_table_col_name <- paste(c("V", index), collapse = "")
    names(shoe_table)[names(shoe_table) == shoe_table_col_name] <- name 
    index <- index + 1
  }
  
  shoe_table
}

get_shoes_id <- function(remDr) {
  ap_shoes_url <- "https://www.attackpoint.org/shoes.jsp"
  remDr$navigate(ap_shoes_url)
  
  pg_html <- remDr$getPageSource()[[1]]
  shoesid_pos <- data.frame(str_locate_all(pg_html, "shoesid"))
  
  shoes_id <- str_sub(pg_html, shoesid_pos$end + 2, shoesid_pos$end + 6)
  
  shoes_id
}

get_shoe_data <- function(remDr, shoe_id) {
  ap_edit_shoes_url <- 
    paste("https://www.attackpoint.org/editshoes.jsp?shoesid=", shoe_id, 
          sep = "")
  remDr$navigate(ap_edit_shoes_url)
  
  # print(remDr$findElement("name", "name")$getElementText())

  retired_checkbox <- remDr$findElement("name", "retired")
  is_retired <- retired_checkbox$isElementSelected()
  
  
  ap_edit_shoes_url
}

