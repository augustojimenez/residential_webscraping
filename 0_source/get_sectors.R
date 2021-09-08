# Libraries
library(dplyr)
library(readr)
library(rvest)
library(stringi)

# Creating empty data frame
df <- data.frame(province = character(),
                 id = character(),
                 descripcion = character())

# There are 31 provinces in the Dominican Republic, but supercasas.com considers
# municipalities as provinces, depending on its importance. The province ID does
# not increases orderly (some numerals have no province assigned to it).
for (province in 1:50) {
    # URL to iterate looking for all neighborhoods in each province
    base_url <- "https://www.supercasas.com/assets/js/autocomplete-search-location-sectors.js?text=&val=&province="
    list_url <- paste0(base_url,
                    province)
    
    # Extracting HTML
    list_html <- read_html(list_url)
    
    # Verifying whether the URL contains a list of neighborhoods, the loop will
    # continue only if it contains a list
    empty_list <- stri_detect_fixed(html_text(list_html), "Selecciona")
    if(empty_list){
        next
    }
    
    sector <- 1
    
    repeat{
        links <- html_nodes(list_html,
                            paste0("body > div > ul > li:nth-child(",
                                   sector,
                                   ")"))
        len <- length(links)
        if(len == 0){
            break
        } else {
            df <- rbind(df, data.frame(province = province,
                                       id = html_attrs(links)[[1]][3],
                                       description = html_attrs(links)[[1]][4]))
            sector = sector + 1
        }
    }
}

# Removing the option "All neighborhoods" from the data set
df <- df %>%
    filter(description != "Todos los sectores") %>%
    distinct(id, .keep_all = TRUE)

write.csv(df, "./1_data/0_raw/neighborhoods.csv",  row.names = FALSE)
