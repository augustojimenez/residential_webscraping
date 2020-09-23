library(rvest)
library(tidyverse)
library(stringi)

base.url <- paste0("https://mudate.net/propiedades?category=", 
              "1", 
              "&city=", 
              "156", 
              "&currency=", 
              "US", 
              "&listing_type=", 
              "1", "&page=",
              "1",
              "&sector=",
              "1691",
              "&sort=",
              "us_saleprice")

html <- read_html(base.url)

lol <- html_nodes(html, '#__next > div.container > div.card-deck.property-holder.row')
html_attr(lol, "href")
