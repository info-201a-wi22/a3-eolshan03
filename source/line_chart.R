library("dplyr")
library("ggplot2")
library("tidyverse")

prison_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = F)

over_time <- prison_trends %>%
  select(year, white_prison_pop, black_prison_pop, aapi_prison_pop,
         native_prison_pop, other_race_prison_pop, latinx_prison_pop) %>%
  filter(year == 1984 | year == 1994 | year == 2004 | year == 2014) %>%
  group_by(year)  %>%
  summarize(across(white_prison_pop:latinx_prison_pop, sum, na.rm = T))


race_totals <- cbind(over_time[1], stack(over_time[2:7])) %>%
  rename(race = ind)
  
line_chart <- ggplot(race_totals, aes(x = year, y = values, group = race)) +
  geom_line(aes(color = race)) + 
  geom_point(aes(color = race), size = 1) +
  labs(title = "Prison Population over Time \n based on Race", fill = "Race") +
  ylab("Prison Population") +
  xlab("Year") +
  scale_color_hue(labels = c("White", "Black", 
                             "Asian American/\nPacific Islander", 
                             "Native", "Other/Mixed", "Latinx"))


