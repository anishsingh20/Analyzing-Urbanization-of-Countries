#Analyzing Urbanization in Countries


#Scraping data from Wikipedia

require(rvest)
require(dplyr)
require(ggplot2)
require(maps)
require(mapproj)




#scraping the data from wikipedia
urban<-read_html("https://en.wikipedia.org/wiki/Urbanization_by_country")

#Converting to a Data frame
urban = urban %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  .[[1]] %>%
  html_table(fill=T)

#Data cleaning
str(urban)


#removing Rank column
urban$Rank<-NULL

#Making Columns Numeric
for(i in 2:4) {
  urban[,i] = as.numeric(urban[,i])
}

str(urban)

#Changing Names

names(urban)<-make.names(names(urban))
names(urban)

#making a new data frame ordered with Urbanization Rate

#rate of urbanization, describes the projected average rate of change of 
#the size of the urban population over the given period of time.

urbanRate<- urban %>% group_by(Nation) %>%
  summarise(Mean.Urban.Rate = mean(Urbanization.Rate....)) %>%
  top_n(15)





