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

#Changing Names of the variables

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



#A data frame consisting of top 40 countries with highest Urban Population
TopUrbanPopulation= Urbanworld %>% 
  group_by(region) %>%
  summarise(
    long = mean(range(long)), 
    lat = mean(range(lat)), 
    group = mean(group), 
    UrbanPop = mean(Urban.Population....)
  ) %>%
  arrange(desc(UrbanPop)) %>%
  top_n(40)


#Generating a World Map filled by Percentage of Urban Population
gg <- ggplot()
#Adding legend

#Adding world map
gg <- gg + geom_map(data=world, map=world, 
      aes(map_id=region, x=long, y=lat), fill="white", 
      colour="black", size=0.25)

#Adding another layer which fills with Per of Urban Population
gg <- gg + geom_map(data=Urbanworld, map=world, aes(map_id=region, fill=Urban.Population....), color="white", size=0.25) +
            scale_fill_gradient(name = "Percentage of Urban Population", low = "#D7F6F7", high = "#177B7F", 
                      guide = "colorbar", na.value="white", breaks = pretty_breaks(n=6)) +
  labs(title="Percentage of Urban population in Countries",subtitle="White Regions are having NA values") + 
  geom_text(aes(x = long , y = lat , label = region),data = TopUrbanPopulation,size=3)






#enerating a World Map filled by Percentage of Urbanization Rate
#rate of urbanization, describes the projected average rate of change of 
#the size of the urban population over the given period of time
#In this data from 2015-2010

#A data frame for adding Names to the Plot
TopcountryRate= Urbanworld %>% 
  group_by(region) %>%
  summarise(
    long = mean(range(long)), 
    lat = mean(range(lat)), 
    group = mean(group), 
    UrbanRate = mean(Urbanization.Rate....)
  ) %>%
  arrange(desc(UrbanRate)) %>%
  top_n(20)



gg1<-ggplot()

#Adding world map
gg1 <- gg1 + geom_map(data=world, map=world, 
                      aes(map_id=region, x=long, y=lat), fill="white", 
                      colour="black", size=0.25)
gg1 <- gg1 + geom_map(data=Urbanworld, map=world, aes(map_id=region, fill=Urbanization.Rate....), color="white", size=0.25) +
  scale_fill_gradient(name = "Urbanization Rate", low = "#FEA5B1", high = "#CA0E27", 
                      guide = "colorbar", na.value="white", breaks = pretty_breaks(n=6)) +
  geom_text(data=TopcountryRate,aes(x = long, y = lat,label=region),size = 3) +
  labs(title="Urbanization Rate in Countries",subtitle="White areas have NA values")

#Oman has highest Urbanization Rate


#barplot of top 20 Countries with highest Urbanization Rate
ggplot(aes(x = reorder(region,UrbanRate), y = UrbanRate),data = TopcountryRate) +
  geom_col(color="black" , fill="#9BDE28" ,alpha = 0.6) + 
  coord_flip() + 
  labs(x = "Country" , y = "Urbanization Rate")

#Oman ,Qatar and some Gulf countries have highest Urbanization Rates







