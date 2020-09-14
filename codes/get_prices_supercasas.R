library(dplyr)
library(readr)
library(rvest)
library(xml2)
library(stringi)

hora_inicio <- Sys.time()
# Base URL for searching within the web page
base.url <- c("https://www.supercasas.com/buscar/?do=1&ObjectType=",
              "&Locations=",
              "&PriceType=",
              "&PagingPageSkip=")

# Search parameters
object.type <- 123 # Apartamento
locations <- "226,350" # 226 = Bella Vista; 350 = Bella Vista Norte
# 226%2c350 = 226,350
price.type <- 400

# number of listings
no.listing <- parse_number(html_text(html_nodes(read_html(paste0(base.url[1],
                                                                 object.type,
                                                                 base.url[2],
                                                                 locations,
                                                                 base.url[3],
                                                                 price.type,
                                                                 base.url[4],
                                                                 0)), 
                                                "#LowerCounter2")))
# number of pages. Note: there are 24 listings per page
max.pages <- round(no.listing/24 - 1)

df <- data.frame(id = as.numeric(),
                 parking = as.character(),
                 bathrooms = as.character(),
                 bedrooms = as.character(),
                 price = as.character(),
                 #seller = as.character(),
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

for(page in 0:max.pages){
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
  links <- html_nodes(pages_data, "#bigsearch-results-inner-results > ul > li > div > a")
  ending.points <- gregexpr(">", links)
  
  listing.id <- character()
  enlaces <- character()
  for(i in 1:length(links)){
    listing.id[i] <- substr(links[i], 10, ending.points[[i]][1]-2)
    enlaces[i] <- paste0("https://www.supercasas.com",
                         listing.id[i])
  }
  
  for(i in 1:length(enlaces)) {
    print(paste("Page:", page, ". Listing", i))
    listings_data <- read_html(enlaces[i])
    
    which <- length(html_text(html_nodes(listings_data,
                                         xpath = '//*[@id="detail-ad-info-specs"]/div[3]/div[3]/span')))
    if(which) {
      parking.div = 3
      bedrooms.div = 1
      # Bathrooms
      bathrooms <- html_text(html_nodes(listings_data, 
                                        xpath = paste('//*[@id="detail-ad-info-specs"]/div[3]/div[',
                                                      2,
                                                      ']/span')))
      if(length(bathrooms) == 0) bathrooms = NA
    } else {
      parking.div = 2
      bedrooms.div = 1
      bathrooms <- NA
    }
    parking <- html_text(html_nodes(listings_data, 
                                    xpath = paste('//*[@id="detail-ad-info-specs"]/div[3]/div[',
                                                  parking.div,
                                                  ']/span')))
    if(length(parking) == 0) parking = NA
    bedrooms <- html_text(html_nodes(listings_data, 
                                     xpath = paste('//*[@id="detail-ad-info-specs"]/div[3]/div[',
                                                   bedrooms.div,
                                                   ']/span')))
    if(length(bedrooms) == 0) bedrooms = NA
    price <- html_text(html_nodes(listings_data, 
                                  "#detail-ad-header > h3"))
    #seller <- html_text(html_nodes(listings_data, 
    #                               "#detail-right > h3"))
    location <- html_table(listings_data)[[1]][1, 2]
    status <- html_table(listings_data)[[1]][2, 2]
    area <- html_table(listings_data)[[1]][3, 2]
    usage <- html_table(listings_data)[[1]][2, 4]
    story <- html_table(listings_data)[[1]][4, 2]
    
    comodidades <- html_text(html_nodes(listings_data,
                                        '#detail-ad-info-specs > div:nth-child(6)'))
    
    if(length(comodidades) == 0){
      planta <- lift <- pool <- pozo <- terraza <- lobby <- balcon <- jacuzzi <- gimnasio <- NA
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
    
    df <- rbind(df, data.frame(id = listing.id[i],
                               parking,
                               bathrooms,
                               bedrooms,
                               price,
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
}

df <- tidyr::separate(df, price, c("currency", "price"), sep = " ") %>%
  tidyr::separate(location, c("neighborhood", "city", "province"), sep = ",") %>%
  mutate(price = parse_number(price),
         price.usd = ifelse(currency == "US$", price, price/58.5),
         bedrooms = parse_number(bedrooms),
         bathrooms = parse_number(bathrooms),
         parking = parse_number(parking),
         area = parse_number(area),
         area = ifelse(area == 0, NA, area),
         story = ifelse(story == 0, NA, parse_number(story)),
         status = ifelse(status == "N/D", NA, as.factor(status))) %>%
  as_tibble()

saveRDS(df, file = "./data/housing.rds")
print("data saved in 'data' dirtectory")
hora_final <- Sys.time()
print(hora_final - hora_inicio)
rm(list = ls())