# Resetting environment
rm(list = ls())

# Loading libraries
library(dplyr)
library(readr)
library(rvest)
library(stringi)

start_time <- Sys.time()

# Base URL for searching within the web page
base.url <- c("https://www.supercasas.com/buscar/?do=1&ObjectType=",
              "&Locations=",
              "&PriceType=",
              "&PagingPageSkip=")

# Search parameters
object.type <- 123 # Searching by apartment

# 226 = Bella Vista; 350 = Bella Vista Norte; 1330 = Bella Vista Sur
# 229 = Mirador Norte; 227 = Mirador Sur
locations <- "226,350,1330,229,227" 

# Prices in US Dollars
price.type <- 400

# Number of listings
no.listing <- parse_number(html_text(html_nodes(read_html(paste0(base.url[1],
                                                                 object.type,
                                                                 base.url[2],
                                                                 locations,
                                                                 base.url[3],
                                                                 price.type,
                                                                 base.url[4],
                                                                 0)), 
                                                "#LowerCounter2")))

# Number of pages. Note that there are 24 listings per page
max.pages <- round(no.listing/24 - 1)

choices <- c("habitaciones", "parqueos", "baños")

df <- data.frame(id = as.numeric(),
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
    print(paste("Page:", page, ". Listing", i, ". ID:", listing.id[i]))
    listings_data <- read_html(enlaces[i])
    
    bedrooms <- parking <- bathrooms <- NA
    
    for(j in 1:3){
      div <- html_text(html_nodes(listings_data,
                                  xpath = paste0('//*[@id="detail-ad-info-specs"]/div[3]/div[', 
                                                 j,
                                                 ']/span')))
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
                               parking = parking,
                               bathrooms = bathrooms,
                               bedrooms = bedrooms,
                               price = price,
                               seller = seller,
                               location = location[[1]],
                               status = status[[1]],
                               area = area[[1]],
                               usage = usage[[1]],
                               story = story[[1]],
                               planta = planta,
                               lift = lift,
                               pool = pool,
                               pozo = pozo,
                               terraza = terraza,
                               lobby = lobby,
                               balcon = balcon,
                               jacuzzi = jacuzzi,
                               gimnasio = gimnasio))
  }
}

df <- tidyr::separate(df, price, c("currency", "price"), sep = " ") %>%
  tidyr::separate(location, c("neighborhood", "city", "province"), sep = ",") %>%
  mutate(price = parse_number(price),
         price.usd = ifelse(currency == "US$", price, price / 58.5),
         bedrooms = parse_number(bedrooms),
         bathrooms = parse_number(bathrooms),
         parking = parse_number(parking),
         area = parse_number(area),
         area = ifelse(area == 0, NA, area),
         story = ifelse(story == 0, NA, parse_number(story)),
         usage = ifelse(usage == "N/D", NA, usage),
         status = ifelse(status == "N/D", NA, status),
         status = as.factor(status)) %>%
  as_tibble()

# Brief data cleaning
df[df$id == "/apartamentos-venta-mirador-sur/1252980/", "area"] = 170

df[df$id == "/apartamentos-venta-y-alquiler-mirador-norte/1234486/", "currency"] = "RD$"
df[df$id == "/apartamentos-venta-y-alquiler-mirador-norte/1234486/", "price.usd"] = df[df$id == "/apartamentos-venta-y-alquiler-mirador-norte/1234486/", "price"]/58.5

df[df$id == "/apartamentos-venta-bella-vista/1234385/", "currency"] = "RD$"
df[df$id == "/apartamentos-venta-bella-vista/1234385/", "price.usd"] = df[df$id == "/apartamentos-venta-bella-vista/1234385/", "price"]/58.5

df[df$id == "/apartamentos-venta-mirador-norte/1253741/", "price.usd"] = df[df$id == "/apartamentos-venta-mirador-norte/1253741/", "price.usd"] * 100
df[df$id == "/apartamentos-venta-mirador-norte/1253741/", "price"] = df[df$id == "/apartamentos-venta-mirador-norte/1253741/", "price"] * 100

df[df$id == "/apartamentos-venta-bella-vista/1252978/", "currency"] = "US$"
df[df$id == "/apartamentos-venta-bella-vista/1252978/", "price.usd"] = df[df$id == "/apartamentos-venta-bella-vista/1252978/", "price"]

df[df$id == "/apartamentos-venta-bella-vista-norte/1208666/", "currency"] = "US$"
df[df$id == "/apartamentos-venta-bella-vista-norte/1208666/", "price.usd"] = df[df$id == "/apartamentos-venta-bella-vista-norte/1208666/", "price"]

saveRDS(df, file = "./data/housing_140721(2).rds")
print("data saved in 'data' dirtectory")
end_time <- Sys.time()
print(end_time - start_time)
