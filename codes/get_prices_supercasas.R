library(dplyr)
library(rvest)
library(xml2)
library(stringr)

# Base URL for seaching within the webpage
base.url <- c("https://www.supercasas.com/buscar/?do=1&ObjectType=",
              "&PriceType=",
              "&Locations=",
              "&PriceFrom=",
              "&PriceTo=",
              "&RoomsFrom=",
              "&RoomsTo=", 
              "&Elevator=",
              "&Pool=",
              "&OpenSpace=",
              "&Generator=")

# Search parameters
object.type <- 123 # Apartamento
price.type <- 400
locations <- "226,350" # 226 = Bella Vista; 350 = Bella Vista Norte
price.range <- c(0, 15000000)
rooms.range <- c(2, 3)
elevator <- 1
pool <- 1
open.space <- 1
generator <- 1

complete.url <- paste0(base.url[1],
                       object.type,
                       base.url[2],
                       price.type,
                       base.url[3],
                       locations,
                       base.url[4],
                       price.range[1],
                       base.url[5],
                       price.range[2],
                       base.url[6],
                       rooms.range[1],
                       base.url[7],
                       rooms.range[2],
                       base.url[8],
                       elevator,
                       base.url[9],
                       pool,
                       base.url[10],
                       open.space,
                       base.url[11],
                       generator)

pages_data <- read_html(complete.url) 
html_nodes(pages_data, "#bigsearch-results-inner-results > ul > li > div > a") # Getting url of all listings
