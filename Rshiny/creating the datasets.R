library(tidyverse)
library(urbnmapr)
library(gsubfn)
library(RSQLite)
library(sqldf)



#### Loading the data set ####
beer_awards <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')
beer_awards$medal <- factor(beer_awards$medal, 
                            c("Gold", "Silver", "Bronze"),
                            levels = c("Gold", "Silver", "Bronze"),
                            ordered = TRUE)

#### Adding a new variable "new_cat" a more general category based on beer categories ####
beer_awards$category <- tolower(beer_awards$category)

gen_cat <- function(x){
  ifelse(str_detect(x,'india pale ale'),'India pale ale',
         ifelse(str_detect(x,'impe'),'Imperial',
                ifelse(str_detect(x,'pale ale'),'Pale ale',
                       ifelse(str_detect(x,'stout'),'Stouts',
                              ifelse(str_detect(x,'lager'),'Lager',
                                     ifelse(str_detect(x,'pils'),'Pilsner', 
                                            ifelse((str_detect(x,'fruit beer'))|(str_detect(x,'ras')),'Fruit beer',
                                                   ifelse(str_detect(x,'hefeweizen'),'Wheat beer',
                                                          ifelse(str_detect(x,'witbier'),'Wheat beer',
                                                                 ifelse(str_detect(x,'weizen'),'Wheat beer',
                                                                        ifelse((!str_detect(x,'india pale ale') | !str_detect(x,'pale ale'))&
                                                                                 (str_detect(x,'ale')),'Other ale',
                                                                               ifelse(str_detect(x,'wheat'),'Wheat beer',
                                                                                      ifelse(str_detect(x,'bock'),'Bock',
                                                                                             ifelse(str_detect(x,'porte'),'Porter',
                                                                                                    ifelse(str_detect(x,'bitter'),'Bitter','Other'
                                                                                                    )))))))))))))))
}
states_sf <- get_urbn_map("states", sf = TRUE)
beer_awards <- beer_awards %>% rowwise() %>% mutate(new_cat = gen_cat(category))

