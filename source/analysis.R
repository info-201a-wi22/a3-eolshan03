library("dplyr")
library("ggplot2")

setwd("~/Documents/Info201code/a3-eolshan03")

prison_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = F)

prison_subset <- prison_trends %>%
  select(year, state, county_name, total_prison_pop,
         female_prison_pop, male_prison_pop, aapi_prison_pop,
         black_prison_pop, latinx_prison_pop, native_prison_pop,
         other_race_prison_pop, white_prison_pop, aapi_female_prison_pop,
         aapi_male_prison_pop, black_female_prison_pop, black_male_prison_pop,
         latinx_female_prison_pop, latinx_male_prison_pop, 
         native_female_prison_pop, native_male_prison_pop, other_race_female_prison_pop,
         other_race_male_prison_pop, white_female_prison_pop, 
         white_male_prison_pop)

total_female_pop_2014 <- prison_subset %>%
  select(female_prison_pop, state, year) %>%
  filter(year == 2014) %>% 
  group_by(state) %>%
  summarize(total_state = sum(female_prison_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)

total_female_pop_2004 <- prison_subset %>%
  select(female_prison_pop, state, year) %>%
  filter(year == 2004) %>% 
  group_by(state) %>%
  summarize(total_state = sum(female_prison_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)

total_male_pop_2014 <- prison_subset %>%
  select(male_prison_pop, state, year) %>%
  filter(year == 2014) %>% 
  group_by(state) %>%
  summarize(total_state = sum(male_prison_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)

total_male_pop_2004 <- prison_subset %>%
  select(male_prison_pop, state, year) %>%
  filter(year == 2004) %>% 
  group_by(state) %>%
  summarize(total_state = sum(male_prison_pop, na.rm = T)) %>%
  summarize(total = sum(total_state, na.rm = T)) %>%
  pull(total)
  
black_2015 <- prison_subset %>%
  select(state, year, black_prison_pop) %>%
  filter(year == 2015) %>%
  group_by(state) %>%
  summarize(total = sum(black_prison_pop, na.rm = T)) %>%
  filter(total == max(total)) %>%
  pull(state)
  
white_2015 <- prison_subset %>%
   select(state, year, white_prison_pop) %>%
   filter(year == 2015) %>%
   group_by(state) %>%
   summarize(total = sum(white_prison_pop, na.rm = T)) %>%
   filter(total == max(total)) %>%
   pull(state)

 aapi_2015 <- prison_subset %>%
   select(state, year, aapi_prison_pop) %>%
   filter(year == 2015) %>%
   group_by(state) %>%
   summarize(total = sum(aapi_prison_pop, na.rm = T)) %>%
   filter(total == max(total)) %>%
   pull(state)

 native_2015 <- prison_subset %>%
   select(state, year, native_prison_pop) %>%
   filter(year == 2015) %>%
   group_by(state) %>%
  summarize(total = sum(native_prison_pop, na.rm = T)) %>%
   filter(total == max(total)) %>%
  pull(state)
        
        
latinx_2015 <- prison_subset %>%
   select(state, year, latinx_prison_pop) %>%
  filter(year == 2015) %>%
  group_by(state) %>%
  summarize(total = sum(latinx_prison_pop, na.rm = T)) %>%
  filter(total == max(total)) %>%
  pull(state)
