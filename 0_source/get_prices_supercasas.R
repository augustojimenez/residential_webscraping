rm(list = ls())

library(dplyr)
library(readr)
library(rvest)
library(stringi)
library(purrr)

get_listings <- function(locations){
  cat(paste("Working on neighborhod:", locations, "\n"))
  # Each location correspond to a neighborhood
  date <- Sys.Date()
  
  # Base URL for searching within the web page
  base.url <- c("https://www.supercasas.com/buscar/?do=1&ObjectType=",
                "&Locations=",
                "&PriceType=",
                "&PagingPageSkip=")
  
  # Search parameters
  object.type <- 123 # Apartments
  price.type <- 400 # In USD dollars (but this param does not work properly)
  
  # Get the amount of listings
  no.listing <- parse_number(html_text(html_nodes(read_html(paste0(base.url[1],
                                                                   object.type,
                                                                   base.url[2],
                                                                   locations,
                                                                   base.url[3],
                                                                   price.type,
                                                                   base.url[4],
                                                                   0)), 
                                                  "#LowerCounter2")))
  
  if(length(no.listing) == 0) {
    cat(paste("    No listing on location", locations, ".\n"))
    cat(paste("Skipping location.\n"))
    return(NULL)
  }
  # Calculating the number of pages. Note: there are at most 24 listings per page
  max.pages <- floor((no.listing - 1) / 24)
  
  # Information about the amount of bedrooms, parkings and bathrooms is stored
  # always in the same panel, but not always in the same order. `choices` variable
  # is created as a point of comparison and used on line 98
  choices <- c("habitaciones", "parqueos", "baños")
  
  # Empty data frame to store data
  df <- tibble(date = as.Date(as.numeric()),
               id = as.numeric(),
               parking = as.character(),
               bathrooms = as.character(),
               bedrooms = as.character(),
               price = as.character(),
               seller = as.character(),
               location = as.character(),
               status = as.character(),
               area = as.character(),
               usage = as.character(),
               story = as.character(),
               planta = as.logical(),
               lift = as.logical(),
               pool = as.logical(),
               pozo = as.logical(),
               terraza = as.logical(),
               lobby = as.logical(),
               balcon = as.logical(),
               jacuzzi = as.logical(),
               gimnasio = as.logical())
  
  
  # Loop 
  for(page in 0:max.pages){
    cat(paste("  Page", page, "of", max.pages, ".\n"))
    complete.url <- paste0(base.url[1],
                           object.type,
                           base.url[2],
                           locations,
                           base.url[3],
                           price.type,
                           base.url[4],
                           page)
    
    pages_data <- read_html(complete.url)
    
    # Getting all listings' URL
    links <- html_nodes(pages_data, 
                        "#bigsearch-results-inner-results > ul > li > div > a")
    # Get the ending position for every link
    ending.points <- gregexpr(">", links) # Links are delimited by <>
    
    listing.id <- enlaces <- character()
    
    for(i in 1:length(links)){
      listing.id[i] <- substr(links[i], 10, ending.points[[i]][1]-2)
      enlaces[i] <- paste0("https://www.supercasas.com",
                           listing.id[i])
    }
    
    for(i in 1:length(enlaces)) {
      cat(paste("    Page:", page, ". Listing", i, ". ID:", listing.id[i], "\n"))
      listings_data <- read_html(enlaces[i])
      
      bedrooms <- parking <- bathrooms <- NA
      
      for(j in 1:3){
        x_paths <- paste0('//*[@id="detail-ad-info-specs"]/div[3]/div[', 
                          j,
                          ']/span')
        
        div <- html_nodes(listings_data,
                          xpath = x_paths) %>%
          html_text()
        which <- which(stri_detect_fixed(div, choices))
        if(length(which) == 0){
          next
        } else if(which == 1){
          bedrooms <- div
        } else if(which == 2){
          parking <- div
        } else if(which == 3){
          bathrooms <- div
        }
      }
      price <- html_text(html_nodes(listings_data, 
                                    "#detail-ad-header > h3"))
      seller <- html_text(html_nodes(listings_data, 
                                     "#detail-right > h3"))[1]
      location <- html_table(listings_data)[[1]][[1, 2]]
      status <- html_table(listings_data)[[1]][[2, 2]]
      area <- html_table(listings_data)[[1]][[3, 2]]
      usage <- html_table(listings_data)[[1]][[2, 4]]
      story <- html_table(listings_data)[[1]][[4, 2]]
      comodidades <- html_nodes(listings_data,
                                '#detail-ad-info-specs > div:nth-child(6)') %>%
        html_text()
      
      if(length(comodidades) == 0){
        planta <- lift <- pool <- pozo <- terraza <- NA
        jacuzzi <- gimnasio <- lobby <- balcon <- NA
      } else {
        planta <- stri_detect_fixed(comodidades, "Planta Eléctrica")
        lift <- stri_detect_fixed(comodidades, "Ascensor")
        pool <- stri_detect_fixed(comodidades, "Piscina")
        pozo <- stri_detect_fixed(comodidades, "Pozo")
        terraza <- stri_detect_fixed(comodidades, "Terraza")
        lobby <- stri_detect_fixed(comodidades, "Lobby")
        balcon <- stri_detect_fixed(comodidades, "Balcón")
        jacuzzi <- stri_detect_fixed(comodidades, "Jacuzzi")
        gimnasio <- stri_detect_fixed(comodidades, "Gimnasio")
      }
      
      df <- rbind(df, tibble(date = date,
                             id = listing.id[i],
                             parking,
                             bathrooms,
                             bedrooms,
                             price,
                             seller,
                             location,
                             status,
                             area,
                             usage,
                             story,
                             planta,
                             lift,
                             pool,
                             pozo,
                             terraza,
                             lobby,
                             balcon,
                             jacuzzi,
                             gimnasio))
    }
    Sys.sleep(15)
  }
  
  # Performing some minor transformations to data
  df <- tidyr::separate(df,
                        price,
                        c("currency", "price"),
                        sep = " ") %>%
    tidyr::separate(location,
                    c("neighborhood", "city", "province"),
                    sep = ",", fill = "left") %>%
    mutate(price = parse_number(as.character(price)),
           price.usd = ifelse(currency == "US$", price, price/58.5),
           bedrooms = parse_number(as.character(bedrooms)),
           bathrooms = parse_number(as.character(bathrooms)),
           parking = parse_number(as.character(parking)),
           area = parse_number(as.character(area)),
           area = ifelse(area == 0, NA, area),
           story = ifelse(story == 0, NA, parse_number(story)),
           status = ifelse(status == "N/D", NA, status),
           status = as.factor(status)) %>%
    as_tibble()
  
  #return(df)
  write_csv(df, file = paste0("../1_data/0_raw/housing_",
                              locations,
                              "_",
                              date,
                              ".csv"))
  
  cat("File was successfully saved.\n")
}

neighborhoods <- read_csv("../1_data/0_raw/neighborhoods.csv",
                          col_types = cols(.default = col_character()))

walk(neighborhoods$id, get_listings)
