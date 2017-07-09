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


ggplot(aes(x = reorder(Nation,Mean.Urban.Rate) , y = Mean.Urban.Rate),data = urbanRate) +
  geom_col(color="black",fill="blue",alpha=0.5) +
  coord_flip() +
  labs(x = "Countries" , y = "Mean Urbanization Rate",title="Urbanization Rates of Top 15 Countries")


#Now creating a World Map

#Loading the World data in a data frame with Latitudes and Longitudes
world<-map_data("world")

#Creating a new Column in Urban df to merge it with world df 
urban$region<-urban$Nation


#Merging the urban and World Data set
Urbanworld<-merge(world,urban,by="region",all.x=T)
Urbanworld$subregion<-NULL
