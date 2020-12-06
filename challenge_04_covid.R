library(tidyverse)
covid_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# sort the data accoridng the date
covid_tbl <- arrange(covid_tbl, countriesAndTerritories, year, month,day)

# convert date as "Date" datatype
covid_tbl[,'dateRep'] <- as.Date(covid_tbl$dateRep, "%d/%m/%Y")

#calculate cumulated cases and deaths
covid_tbl <- mutate(group_by(covid_tbl, countriesAndTerritories), cum_death=cumsum(deaths))
covid_tbl <- mutate(group_by(covid_tbl, countriesAndTerritories), cum_cases=cumsum(cases))

#calculate death rate by cumulated deaths/population
covid_tbl <- mutate(covid_tbl, mortality_rate = cum_death/(popData2019+1e-8))

# function that filters for data of only one state
get_cases_from_state <- function(state){
  cases=covid_tbl[covid_tbl$countriesAndTerritories == state,]
  return(cases)
}
  
#get cases of different states
cases_germany = get_cases_from_state('Germany')
cases_usa = get_cases_from_state('United_States_of_America')
cases_uk = get_cases_from_state('United_Kingdom')
cases_spain = get_cases_from_state('Spain')
cases_france = get_cases_from_state('France')

# plot cumulative cases for some states

p=ggplot()+ 
  geom_line(data = cases_germany, aes(y = cum_cases, x = dateRep, color='Germany')) +
  geom_line(data = cases_usa, aes(y = cum_cases, x = dateRep, color = 'United_States_of_America')) +
  geom_line(data = cases_uk, aes(y = cum_cases, x = dateRep,color = 'United_Kingdom')) +
  geom_line(data = cases_spain, aes(y = cum_cases, x = dateRep, color = 'Spain')) +
  geom_line(data = cases_france, aes(y = cum_cases,x = dateRep, color = 'France')) +
  labs(title = 'Cumulative Cases', x = 'date', y = 'cases', color = 'Legend')

plot(p)

############################# task 2 ################################

# get most actual mortality rates
last_date <- max(covid_tbl$dateRep)
covid_last <- covid_tbl[covid_tbl$dateRep == last_date,]

# adjust the data
covid_last$countriesAndTerritories <- lapply(covid_last$countriesAndTerritories, gsub, pattern = "_", replacement = " ")
covid_last$countriesAndTerritories <- lapply(covid_last$countriesAndTerritories, gsub, pattern = "United Kingdom", replacement = "UK")
covid_last$countriesAndTerritories <- lapply(covid_last$countriesAndTerritories, gsub, pattern = "United States of America", replacement = "USA")
covid_last$countriesAndTerritories <- lapply(covid_last$countriesAndTerritories, gsub, pattern = "Czechia", replacement = "Czech Republic")
covid_last=unnest(covid_last,cols=c(countriesAndTerritories))


# load data
world <- map_data("world")

#  join the lat/long data and the covid data
joined <- right_join(world, covid_last, by = c('region'='countriesAndTerritories'))

# drop states where no data is available (e.g vatican)
joined <- joined[!is.na(joined$mortality_rate),]


# visualize mortality rates
p = ggplot() +
    geom_map(
      data = joined, map = world, aes(long, lat, map_id = region,fill = mortality_rate), color = "black", size = 0.1)+
      labs(title = 'Mortality Rate',color='Legend')+
      scale_fill_gradient(low = "red", high = "black",labels=scales::percent)
    
plot(p)




