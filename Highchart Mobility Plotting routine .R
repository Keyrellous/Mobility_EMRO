library(highcharter)
library(tidyverse)
library(dplyr)

WHOCases<-read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv", header = T)
names(WHOCases)[names(WHOCases) == "Country"] <- "Country0"
names(WHOCases)[names(WHOCases) == "Date_reported"] <- "Date"
WHOCases$Date<-as.Date(WHOCases$Date)


Mobility<-read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=6ec44f00b5b4f6ad",header = T)
names(Mobility)[names(Mobility) == "date"] <- "Date"
names(Mobility)[names(Mobility) == "country_region"] <- "Country0"
Mobility$Date<-as.Date(Mobility$Date)


country_00<-"Egypt" # Type the selected Country Here 
MobCas<-full_join(WHOCases, Mobility,by=c("Country0","Date" ))
Mob_country<-filter(MobCas, Country0==country_00 & sub_region_1=="" & metro_area=="")
MobilityPlot<-hchart(Mob_country, "column", name="Daily Cases", hcaes(x=Date, y=New_cases), yAxis=0, color="grey")%>%
  
  hc_yAxis_multiples(
    list(title = list(text = "Number of Daily Cases"),opposite=FALSE),
    list(title = list(text = "Percent Change in Mobility (%)"),opposite=TRUE))%>% 
  
  hc_add_series(Mob_country, type = "line", name="Retail and Recreation", hcaes(x=Date, y=retail_and_recreation_percent_change_from_baseline), id="retail", color="red", yAxis=1)%>%
  hc_add_series(Mob_country, type = "line", name="Groceries and Pharmacies", hcaes(x=Date, y=grocery_and_pharmacy_percent_change_from_baseline), id="", color="blue", yAxis=1)%>%
  hc_add_series(Mob_country, type = "line", name="Workplace", hcaes(x=Date, y=workplaces_percent_change_from_baseline), id="work", color="orange", yAxis=1)%>%
  hc_add_series(Mob_country, type = "line", name="Transit Stations", hcaes(x=Date, y=transit_stations_percent_change_from_baseline), id="transit", color="green", yAxis=1)%>%
  hc_add_series(Mob_country, type = "line", name="Resdential", hcaes(x=Date, y=residential_percent_change_from_baseline), id="residential", color="purple", yAxis=1)%>%
  hc_tooltip(shared = TRUE)%>%
  hc_title(text = country_00 )

MobilityPlot
