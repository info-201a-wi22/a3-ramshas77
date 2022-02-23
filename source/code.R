
data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
library(dplyr)
library(tidyr)
library(styler)
library(lintr)
library(ggplot2)
library(usmap)
View(data)

# varibles - county with highest number of black people incarcerated
variable1 <- data %>%
  select(county_name, black_jail_pop) %>%
  na.omit() %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)
# reference https://tidyr.tidyverse.org/reference/drop_na.html
# state with highest mean ratio of black women incarcerated vs the entire incarcerated population. 
variable2 <- data %>%
  select(state, black_female_prison_pop, total_prison_pop) %>%
  na.omit() %>%
  mutate(bw_to_entire_prison = black_female_prison_pop/total_prison_pop) %>%
  group_by(state) %>%
  summarize(mean_ratio = mean(bw_to_entire_prison)) %>%
  na.omit() %>%
  filter(mean_ratio == max(mean_ratio)) %>%
  pull(state)
# state with the highest mean disparity of black men in prison vs white men in prison 
variable3 <- data %>%
  select(state, black_male_prison_pop, white_male_prison_pop) %>%
  na.omit() %>%
  mutate(bm_to_wm_incarcerated = black_male_prison_pop - white_male_prison_pop) %>%
  group_by(state) %>% summarize(mean_disparity = mean(bm_to_wm_incarcerated)) %>%
  filter(mean_disparity == max(mean_disparity)) %>%
  pull(state)
# relocate reference https://dplyr.tidyverse.org/reference/relocate.html

# state with the highest ratio of black people vs the entire incarcerated population. 
variable4 <- data %>%
  select(state, black_prison_pop, total_prison_pop) %>%
  na.omit() %>% mutate(black_to_total_prison = black_prison_pop/total_prison_pop) %>%
  group_by(state) %>%
  summarize(mean_black_prison_poportion = mean(black_to_total_prison)) %>%
  na.omit %>%
  filter(mean_black_prison_poportion == max(mean_black_prison_poportion)) %>%
  pull(state)
# what state that compares black people incarcerated as a percentage of the entire black population.  
variable5 <- data %>% select(state, black_prison_pop, black_pop_15to64) %>% na.omit() %>% mutate(black_to_total_population = black_prison_pop/black_pop_15to64) %>% group_by(state) %>% summarize(mean_black_to_total_population = mean(black_to_total_population)) %>% na.omit %>% filter(mean_black_to_total_population == max(mean_black_to_total_population)) %>% pull(state)
# trends over time - black population (or black pop percentage) incarcerated over time 
trend_over_time <- data %>% select(year, black_jail_pop) %>% na.omit() %>% group_by(year) %>% summarize(nationwide_black_jail_population = sum(black_jail_pop))

display_chart1 <- function(data) {
  trend_over_time <- data %>% select(year, black_jail_pop) %>% 
    na.omit() %>% group_by(year) %>% summarize(nationwide_black_jail_population = sum(black_jail_pop)) %>% filter (year >= 1998, year<= 2008 )
  chart1 <- ggplot(trend_over_time) +
  geom_line(mapping = aes(x=year, y=nationwide_black_jail_population))
  return(chart1)
}


#variable comparison chart 
display_chart2 <- function(data) {
  national_black_jail_population <- data %>% select(year, black_jail_pop) %>% 
    na.omit() %>% group_by(year) %>% summarize(nationwide_black_jail_population = sum(black_jail_pop))
  national_white_jail_population <- data %>% select(year, white_jail_pop) %>% 
    na.omit() %>% group_by(year) %>% summarize(nationwide_white_jail_population = sum(white_jail_pop))
  white_vs_black_comparison <- full_join(national_black_jail_population, national_white_jail_population, by = "year")
  chart2 <- ggplot(white_vs_black_comparison) + 
    geom_line(mapping = aes(x=year, y=nationwide_black_jail_population, 
                            color = "nationwide black jail population")) + 
    geom_line(mapping = aes(x=year, y=nationwide_white_jail_population, 
                            color ="nationwide white jail population"))
  return(chart2)
}

# reference https://www.r-graph-gallery.com/279-plotting-time-series-with-ggplot2.html
# https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
# map - these rates don't mean much without proportions. 
#states with the highest proportion of incarcerated black people to the entire black population. 
# darker with higher, lighter with lower
# ggplot map 
display_map <- function(data) {
  black_incarcerated_proportion <- data %>% 
    mutate(black_proportion_in_prison = black_prison_pop/black_pop_15to64) %>%
    select(state, black_prison_pop, black_pop_15to64, black_proportion_in_prison) %>%
    na.omit() %>%
    group_by(state) %>%
    summarize(average_black_incarcerated_proportion = mean(black_proportion_in_prison))
  map <- plot_usmap(data = black_incarcerated_proportion, 
             values = "average_black_incarcerated_proportion", 
             color = "red") +
    scale_fill_continuous(low = "white", high = "red", name = "ratio") +
    labs(title = "Ratio of the black prison population to the black over the entire black population")
  return(map)
}
#adjust? 
# there a minimum for what's considered substantial available data? 
# reference 
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html


