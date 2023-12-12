library(tidyverse) # metapackage of all tidyverse packages
library(ggplot2)
library(dplyr)
library(reshape2) # Melt
library(plyr)

library(scales) # visualisation
library(corrplot) # visualisation
library(GGally) # visualisation
library(ggthemes) # visualisation

# Interactivity
library(crosstalk)
library(plotly)
library(htmlwidgets)
#Date
library(scales)
library(zoo)
library(lubridate)

library(ggalt)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(plotly)
library(ggplot2)
library(ggthemes)
library(gganimate)
#read file
Emission<-read.csv(choose.files())
Temperature<-read.csv(choose.files())
Temperature[Temperature$Country=='United Kingdom','Country']<-'uk'
Temperature[Temperature$Country=='United States','Country']<-'usa'
Emission_clean<- Emission %>% filter(country_or_area!='european union')
Emission_clean[Emission_clean$country_or_area=='Russian Federation','country_or_area']<-'Russia'
Emission_clean[Emission_clean$country_or_area=='United Kingdom','country_or_area']<-'uk'
Emission_clean[Emission_clean$country_or_area=='United States of America','country_or_area']<-'usa'
##select 1990-01-01 above
Data<-unique(Emission_clean$country_or_area)
unique(Temperature$Country)
Temperature_1990 <- Temperature %>%
  filter(Country %in% Data) %>%
  filter(dt > as.Date("1990-01-01")) %>%
  select(dt, AverageTemperatureUncertainty, Country,AverageTemperature) %>%
  mutate(year = year(dt)) %>%
  group_by(year, Country) %>%
  summarise(mean_value_uncertainty = mean(AverageTemperatureUncertainty),mean_value = mean(AverageTemperature)) %>%
  ungroup() %>%
  complete(year, Country, fill = list(c(mean_value_uncertainty = NA,mean_value = NA))) %>%
  arrange(year, Country) %>%
  fill(mean_value) %>%
  na.omit()
sort(unique(Temperature$Country))
Temperature_1990_iceland <- Temperature_1990 %>%
  filter(Country =="Iceland")
Temperature_1990_usa <- Temperature_1990 %>%
  filter(Country =="usa")

##Non europe country
ocenia<-c("Australia","New Zealand")
Asia<-"Japan"
America_continent<-c("United States of America","Canada")
library(dplyr)

Emission <- Emission %>%
  mutate(region = case_when(
    country_or_area %in% ocenia ~ "Oceania",
    country_or_area %in% Asia ~ "Asia",
    country_or_area %in% America_continent ~ "America",
    TRUE ~ "Europe"
  ))
##Change name in variable category
Emission<-Emission %>% mutate(category=recode(category, 
                                  carbon_dioxide_co2_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='CO2',
                                  greenhouse_gas_ghgs_emissions_including_indirect_co2_without_lulucf_in_kilotonne_co2_equivalent='GHG-indirect-CO2',
                                  greenhouse_gas_ghgs_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='GHG',
                                  hydrofluorocarbons_hfcs_emissions_in_kilotonne_co2_equivalent='HFC',
                                  methane_ch4_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='CH4',
                                  nitrogen_trifluoride_nf3_emissions_in_kilotonne_co2_equivalent='HF3',
                                  nitrous_oxide_n2o_emissions_without_land_use_land_use_change_and_forestry_lulucf_in_kilotonne_co2_equivalent='N2Os',
                                  perfluorocarbons_pfcs_emissions_in_kilotonne_co2_equivalent='PFCs',
                                  sulphur_hexafluoride_sf6_emissions_in_kilotonne_co2_equivalent='SF6',
                                  unspecified_mix_of_hydrofluorocarbons_hfcs_and_perfluorocarbons_pfcs_emissions_in_kilotonne_co2_equivalent='HFC-PFC-mix'))
##variable category
unique(Emission$category)
Co2<-Emission%>%filter(category=='CO2')
GHG_indirect_CO2<-Emission%>%filter(category=='GHG-indirect-CO2')
GHG<-Emission%>%filter(category=='GHG')
HFC<-Emission%>%filter(category=='HFC')
CH4<-Emission%>%filter(category=='CH4')
HF3<-Emission%>%filter(category=='HF3')
N2Os<-Emission%>%filter(category=='N2Os')
SF6<-Emission%>%filter(category=='SF6')
HFC_PFC_mix<-Emission%>%filter(category=='HFC-PFC-mix')
PFCs<-Emission%>%filter(category=='PFCs')
#1------------------------------------
#density graph on different country
options(repr.plot.width=12, repr.plot.height=7)
Emission %>%
  group_by(year,region) %>% summarise(total_value = sum(value)) %>%
  ggplot(aes(x=year,color=region)) + 
  geom_line(aes(y=total_value)) + 
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))+
        labs(title='International Greenhouse gas Emissions in 1990-2015',
       subtitle= 'in kilotons equivalent, summed by all region per year')+
        xlab('')+ylab('')
##2-------------------------------------
#Facet Bar plot first graph
Emission%>%group_by(year, category) %>% filter(year>=2010)%>%
  summarise(total_value = sum(value)) %>%top_n(3,total_value)%>%
  ggplot(aes(x=year,y = total_value, fill = year)) + 
  geom_bar(stat="identity", width = 0.5)+
  labs(x="Category",
       y="Total Value", 
       title="Distribution of Total Value per Category in between 2010 to 2014",
       subtitle= 'in kilotons equivalent, summed by all category per year')+ 
  facet_wrap(~category)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 90),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 18)
  )
#Facet Bar plot first graph
Emission%>%group_by(year, category) %>% filter(year>=2010)%>%
  summarise(total_value = sum(value)) %>%top_n(4,total_value)%>%
  ggplot(aes(x=year,y = total_value, fill = year)) + 
  geom_bar(stat="identity", width = 0.5)+
  labs(x="Category",
       y="Total Value", 
       title="Distribution of Total Value per Category in between 2010 to 2014",
       subtitle= 'in kilotons equivalent, summed by all category per year')+ 
  facet_wrap(~category)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 90),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 18)
  )
Emission%>%group_by(year, category) %>% filter(year>=2010)%>%
  summarise(total_value = sum(value)) %>%filter(category=="CH4"|category=="N2Os"|category=="HFC")%>%
  ggplot(aes(x=year,y = total_value, fill = year)) + 
  geom_bar(stat="identity", width = 0.5)+
  labs(x="Category",
       y="Total Value", 
       title="Distribution of Total Value per Category in between 2010 to 2014",
       subtitle= 'in kilotons equivalent, summed by all category per year')+ 
  facet_wrap(~category)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 90),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 18)
  )
Emission%>%group_by(year, category) %>% filter(year>=2010)%>%
  summarise(total_value = sum(value)) %>% top_n(-4,total_value)%>%
  ggplot(aes(x=year,y = total_value, fill = year)) + 
  geom_bar(stat="identity", width = 0.5)+
  labs(x="Category",
       y="Total Value", 
       title="Distribution of Total Value per Category in between 2010 to 2014",
       subtitle= 'in kilotons equivalent, summed by all category per year')+ 
  facet_wrap(~category)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 22),
    axis.text.x = element_text(size = 15, angle = 90),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 18)
  )
##3---------------------------------------------------------------------------
Category<-data.frame(Emission$category,Emission$value)
colnames(Category)<-c('Type_of_emission',"Value")  

#calculate all value
sum_by_gas <- aggregate(Value ~ Type_of_emission, data = Category, FUN = sum)

custom_col <- c("red", "green","blue","yellow","orange","purple","pink", "brown","gray","cyan") 
#second graph pie chart
ggplot(sum_by_gas, aes(x = "", y= Value, fill = factor(Type_of_emission))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5,size=22)) + 
  labs(fill="Type of emission", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Green house emission from 1990-2015",subtitle= 'in kilotons equivalent, summed by all country') + coord_polar(theta = "y", start=0)+
  scale_fill_manual(values=custom_col)
##4
#boxplot
Emission %>%
  group_by(year,category) %>% summarise(total_value = sum(value)) %>%
  ggplot(aes(x=year,color=category)) + 
  geom_line(aes(y=total_value)) + 
  labs(x="year",
       y="Total value emission", 
       title='International Greenhouse type gas Emissions in 1990-2014',
              subtitle= 'in kilotons equivalent, summed by all region per year')+ 
  theme_bw()+
  theme(plot.title = element_text(size=22)
        ,axis.text.x= element_text(size=15),
        axis.text.y= element_text(size=15),
        axis.title=element_text(size=18))
##5
#barplot plot
Emission %>%
  group_by(year, country_or_area) %>%
  summarise(total_value = sum(value)) %>%
  top_n(10, total_value) %>%
  ggplot(aes(reorder(country_or_area, total_value), total_value)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "green", high = "red") +
  labs(x = "Country/Area", y = "Total Value", title = "Top 10 Distribution of Emissions From 1990 to 2015") +
  theme_bw() +
  theme(plot.title = element_text(size = 22),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16))

Emission %>%
  group_by(year, country_or_area) %>%
  summarise(total_value = sum(value)) %>%
  top_n(-10, total_value) %>%
  ggplot(aes(reorder(country_or_area, total_value), total_value)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_gradient(low = "green", high = "red") +
  labs(x = "Country/Area", y = "Total Value per year", title = "Top 10 Distribution of least Emissions From 1990 to 2015") +
  theme_bw() +
  theme(plot.title = element_text(size = 22),
        axis.text.x = element_text(size = 12, angle = 90),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 16))

##8
#word map
Map<-map_data("world")
colnames(Map)[5]<-'country_or_area'
Map$country_or_area<-tolower(Map$country_or_area)
Emission_clean$country_or_area<-tolower(Emission_clean$country_or_area)
#remove this country since it does not fit within a map
year1990<-Emission_clean%>%filter(year==1990)%>%group_by(country_or_area)%>%
  summarise(total_value = sum(value))
year2014<-Emission_clean%>%filter(year==2014)%>%group_by(country_or_area)%>%
  summarise(total_value = sum(value))
year1990_map<-left_join(Map,year1990,by='country_or_area')
year2014_map<-left_join(Map,year2014,by='country_or_area')
ggplot() + geom_polygon(data=year1990_map,aes(x = long, y = lat, group = group, fill = total_value),colour = "black", size = 0.1) + 
  scale_fill_gradientn(name="",colours=rev(viridis::inferno(20)),na.value="gray100") + theme_fivethirtyeight() 
  + theme(legend.position='right',legend.direction='vertical',axis.text=element_blank()) + labs(title='Gas Emission')

ggplot() + 
  geom_polygon(data=year2014_map,aes(x = long, y = lat, group = group, fill = total_value),colour = "black", size = 0.1) + 
  scale_fill_gradientn(name="",colours=rev(viridis::inferno(20)),na.value="gray100") + theme_fivethirtyeight() 
  + theme(legend.position='right',legend.direction='vertical',axis.text=element_blank()) + labs(title='Gas Emission',subtitle='in kiloton, year 2014')
##mutiple regression
ggplot1<-Emission %>%
  group_by(year) %>%
  summarise(total_value = sum(value)) %>%
  ggplot(aes(y=total_value,x=year)) + geom_point(shape=21,fill="darkgreen",color="green") +geom_smooth(method="lm",se=FALSE)+
  xlab("year")+ylab("Gas Emission in Kilotons")
ggplot1_iceland<-Emission_clean %>%filter(country_or_area=="iceland")%>%group_by(year) %>%
  summarise(total_value = sum(value)) %>%
  ggplot(aes(y=total_value,x=year)) + geom_point(shape=21,fill="darkgreen",color="green") +geom_smooth(method="lm",se=FALSE)+
  xlab("year")+ylab("Gas Emission in Kilotons")
ggplot1_usa<-Emission_clean %>%filter(country_or_area=="usa")%>%group_by(year) %>%
  summarise(total_value = sum(value)) %>%
  ggplot(aes(y=total_value,x=year)) + geom_point(shape=21,fill="darkgreen",color="green") +geom_smooth(method="lm",se=FALSE)+
  xlab("year")+ylab("Gas Emission in Kilotons")
#all country
ggplot2_Temperature<-Temperature_1990%>%group_by(year)%>%
  summarise(total_value = mean(sum(mean_value_uncertainty)))%>%ggplot(aes(y=total_value,x=year)) + geom_point(shape=21,fill="darkblue",color="blue") +geom_smooth(method="lm",se=FALSE)+
  xlab("year")+ylab("The average temperature") + ggtitle("Temperature_uncertainty vs years")
#temperature
ggplot2_iceland<-Temperature_1990_iceland%>%ggplot(aes(y=mean_value_uncertainty,x=year)) + geom_point(shape=21,fill="darkblue",color="blue") +geom_smooth(method="lm",se=FALSE)+
  xlab("year")+ylab("The average temperature") + ggtitle("Temperature_uncertainty vs years ")
ggplot2_usa<-Temperature_1990_usa%>%ggplot(aes(y=mean_value_uncertainty,x=year)) + geom_point(shape=21,fill="darkblue",color="blue") +geom_smooth(method="lm",se=FALSE)+
  xlab("year")+ylab("The average temperature") + ggtitle("Temperature_uncertainty vs years")
#plotly
Interactive1<-ggplotly(ggplot1)
Interactive1_a<-ggplotly(ggplot1_iceland)
Interactive1_b<-ggplotly(ggplot1_usa)
Interactive2<-ggplotly(ggplot2_Temperature)
Interactive2_a<-ggplotly(ggplot2_iceland)
Interactive2_b<-ggplotly(ggplot2_usa)
saveWidget(Interactive1,"plot1.html")
#world map climate change
p<-ggplot(Temperature_1990, aes(x = mean_value_uncertainty,y = mean_value, group=Country,color = factor(Country),size=mean_value_uncertainty)) +
  geom_point() +
  guides(colour=F, size=F)+
  ggtitle("Temperature_uncertainty vs. Mean Tempeture  for Year: {frame_time}") +
  xlab("Temperature_uncertainty") +
  ylab("Mean Tempeture")+theme_bw()+
  transition_time(year)+guides(colour = "legend", size = "none")
anim_save("animated_plot.gif", animate(p))
# gif time series plot
options(repr.plot.width=20, repr.plot.height=20)
p <- Emission %>%
  group_by(year,country_or_area) %>%
  summarise(total_value = sum(value)) %>%
  ggplot(aes(year, total_value, group=country_or_area, color = factor(country_or_area))) +
  geom_line() +
  scale_color_viridis_d() + theme_minimal()+
  theme(text=element_text(size=10),legend.text= element_text(size=8))+
  labs(x = "Year", y = "Total Value",title='International Greenhouse gas Emissions',
       subtitle= 'in kilotons CO2 equivalent, summed by all countries per year') +
  theme(legend.position = "top")+ transition_reveal(year)+geom_point()

anim_save("animated_plot1.gif", animate(p))