library("dplyr")
library("tidyverse")
library("ggplot2")
library("stringr")

prison_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = F)

prison_gender <- prison_trends %>%
  select(year, white_female_prison_pop, white_male_prison_pop, black_female_prison_pop,
         black_male_prison_pop, aapi_female_prison_pop, aapi_male_prison_pop, 
         native_female_prison_pop, native_male_prison_pop, latinx_female_prison_pop, 
         latinx_male_prison_pop, other_race_female_prison_pop, other_race_male_prison_pop) %>%
  filter(year == 2014) %>%
  group_by(year) %>%
  summarize(across(white_female_prison_pop:other_race_male_prison_pop, sum, na.rm = T))


gender_totals <- cbind(prison_gender[1], stack(prison_gender[2:13])) %>%
  rename(race = ind) %>%
  mutate(gender = if_else(str_detect(race, "female") == T, "female", "male"))

gender_bar <- ggplot(gender_totals) +
  geom_col(aes(race, values, fill = gender), position = "dodge") +
  scale_x_discrete(labels = c("white_female_prison_pop" = "White", "black_female_prison_pop" = "Black", 
                              "aapi_female_prison_pop" = "Asian\nAmerican", "native_female_prison_pop" = "Native",  
                              "latinx_female_prison_pop" = "Latinx", "other_race_female_prison_pop" = "Other/\nMixed"), 
                  breaks = c("white_female_prison_pop", "black_female_prison_pop",
                             "aapi_female_prison_pop", "native_female_prison_pop",
                             "latinx_female_prison_pop", "other_race_female_prison_pop")) +
  ylab("Prison Population") +
  labs(title = "Prison Population in 2014\n by Race and Gender")

