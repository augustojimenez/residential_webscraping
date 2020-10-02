library(dplyr)
library(readr)
library(rvest)
library(stringi)

df <- data.frame(id = character(),
                 descripcion = character())
for (ciudad in 1:3) {
    lista <- paste0("https://www.supercasas.com/assets/js/autocomplete-search-location-sectors.js?text=&val=&province=",
                    ciudad)
    lista_html <- read_html(lista)
    sector <- 1
    repeat{
        links <- html_nodes(lista_html, paste0("body > div > ul > li:nth-child(",
                                               sector,
                                               ")"))
        len <- length(links)
        if(len == 0){
            break
        } else {
            df <- rbind(df, data.frame(ciudad = ciudad,
                                       id = html_attrs(links)[[1]][3],
                                       descripcion = html_attrs(links)[[1]][4]))
            sector = sector + 1
        }
    }
}
  