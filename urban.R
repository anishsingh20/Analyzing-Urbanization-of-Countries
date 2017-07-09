#Analyzing Urbanization in Countries


#Scraping data from Wikipedia

require(rvest)
require(dplyr)
require(ggplot2)
require(maps)
require(mapproj)
require(scales)




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

str(Urbanworld)

#A data frame for adding Names to the Plot
countryname = Urbanworld %>% 
  group_by(region) %>%
  summarise(
    long = mean(range(long)), 
    lat = mean(range(lat)), 
    group = mean(group), 
    UrbanPop = mean(Urban.Population....), 
    UrbanRate = mean(Urbanization.Rate....)
  )


#Generating a Map
gg <- ggplot()
gg<-legend(position="left")

gg <- gg + geom_map(data=world, map=world, 
      aes(map_id=region, x=long, y=lat), fill="white", 
      colour="black", size=0.25)
  
gg <- gg + geom_map(data=Urbanworld, map=world, aes(map_id=region, fill=Urban.Population....), color="white", size=0.25) +
            scale_fill_gradient(name = "Percentage of Urban Population", low = "#FAB8D2", high = "#F91C74", 
                      guide = "colorbar", na.value="white", breaks = pretty_breaks(n = 5)) + 
  geom_text(data=countryname, aes(x = long, y = lat, label = region), size=2.5)
