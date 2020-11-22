
get_profile_data <- function(remDr) {
    ap_profile_url <- "https://www.attackpoint.org/editprofile.jsp"
    remDr$navigate(ap_profile_url)
    
    usr_name <- remDr$findElement("name", "username")$
        getElementAttribute("value")[[1]]
    
    first_name <- remDr$findElement("name", "firstname")$
        getElementAttribute("value")[[1]]
    
    last_name <- remDr$findElement("name", "lastname")$
        getElementAttribute("value")[[1]]
    
    birth_year <- remDr$findElement("name", "birth-year")$
        getElementAttribute("value")[[1]]
    
    sportid_number <- remDr$findElement("name", "sicardno")$
        getElementAttribute("value")[[1]]
    
    height <- remDr$findElement("name", "height")$
        getElementAttribute("value")[[1]]
    
    gender <- remDr$findElement("name", "gender")$
        getElementAttribute("value")[[1]]
    
    print(gender)
    
    # first_name_public <- remDr$findElement("name", "showflags")$
    #     isElementSelected()[[1]]
    
    profile_data <- c(usr_name)
}