library(stringr)

#' Returns a list of profile data of user logged into remDr
#'
#' @return A List
#'
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
    
    primay_sportid <- remDr$findElement("name", "primarysportid")$
        getElementAttribute("value")[[1]]
    
    pg_html <- remDr$getPageSource()[[1]]
    prim_sportid_pos <- str_locate(pg_html, "name=\"primarysportid\"")
    
    # first_name_public <- remDr$findElement("name", "showflags")$
    #     isElementSelected()[[1]]
    
    profile_data <- list(usr_name = usr_name,
                         first_name = first_name,
                         last_name = last_name,
                         birth_year = birth_year,
                         sportid_number = sportid_number,
                         height = height,
                         gender = gender)
    
    profile_data
}

get_activity_types <- function(remDr) {
    ap_act_type_url <- "https://www.attackpoint.org/editactivitytypes.jsp"
    remDr$navigate(ap_act_type_url)

    next_act <- TRUE
    index <- 0
    acts <- c()
    
    while (next_act) {
        index <- index + 1
        name <- paste("name", index, sep = "")
        act <- tryCatch({
                remDr$findElement("name", name)
            }, error = function(e) {
                return (NA)
            }, warning = function(w){}
        )
        
        if (is.na(act)) {
            next_act = FALSE
        } else {
            acts <- append(acts, act$getElementAttribute("value")[[1]])
        }
    }
    
    return(acts)
}

