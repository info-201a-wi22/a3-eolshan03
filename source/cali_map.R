library("maps")
library("dplyr")
library("tidyverse")
library("ggplot2")
library("stringr")

prison_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = F)

cali_counties <- prison_trends %>%
  filter(year == 2014) %>%
  unite(county_ca, county_name, state, sep = ", ") %>%
  select(fips, county_ca, black_prison_pop, total_prison_pop) %>%
  mutate(percentage = black_prison_pop / total_prison_pop)

cali <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

cali_per <- left_join(cali, cali_counties, by = "fips") %>%
  filter(str_detect(county_ca, "CA") == T)

blank_theme <- theme_bw() +
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),       
        axis.title = element_blank(),       
        plot.background = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank())

cali_map <- ggplot(cali_per) +
  geom_polygon(aes(long, lat, group = group, fill = percentage), color = "black") +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(cali_per$percentage)), 
                        na.value = "white", low = "lightblue2", 
                        high = "navyblue") +
  blank_theme +
  labs(title = "Percentage of Black Incarcerated \n Individuals in CA in 2014") 

