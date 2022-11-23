library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>

black_jail_max2018 <- incarceration %>%
  select(black_jail_pop, year) %>%
  filter(year == 2018) %>%
  replace(is.na(.), 0) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(black_jail_pop)


white_jail_max2018 <- incarceration %>%
  select(white_jail_pop, year) %>%
  filter(year == 2018) %>%
  replace(is.na(.), 0) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(white_jail_pop)
  

black_jail_max2000 <- incarceration %>%
  select(black_jail_pop, year) %>%
  filter(year == 2000) %>%
  replace(is.na(.), 0) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(black_jail_pop)


white_jail_max2000 <- incarceration %>%
  select(white_jail_pop, year) %>%
  filter(year == 2000) %>%
  replace(is.na(.), 0) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(white_jail_pop)


#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

# This function returns dataframe with year and total_pop
get_year_jail_pop <- function() {
  year_total_pop <- incarceration %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))  
  return(year_total_pop)   
}

get_year_jail_pop()

# This function return barchart of year vs total_pop in nation level
plot_jail_pop_for_us <- function()  {
  year_total_pop <- get_year_jail_pop()
  year_total_bar <- ggplot(data = year_total_pop) +
    geom_col(aes(x=year, y=total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         x = "Year", y="Total Jail Population")
  return(year_total_bar)   
} 

plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# input: states - name of state e.g. Washington as WA
# This function returns dataframe of year and total population according to the given state

states <- c("WA", "NY", "CA")

get_jail_pop_by_states <- function(states) {

  state1 <- incarceration %>%
    filter(state == states[1]) %>%
    replace(is.na(.) , 0) %>%
    group_by(year) %>%
    summarize("WA" = sum(total_jail_pop))
  
  state2 <- incarceration %>%
    filter(state == states[2]) %>%
    replace(is.na(.) , 0) %>%
    group_by(year) %>%
    summarize("NY" = sum(total_jail_pop))
  
  state3 <- incarceration %>%
    filter(state == states[3]) %>%
    replace(is.na(.) , 0) %>%
    group_by(year) %>%
    summarize("CA" = sum(total_jail_pop))
  
  combined_state123 <- left_join(state1, state2, by = "year") %>%
    left_join(state3, by="year") 
  
  return(combined_state123)
}

get_jail_pop_by_states(states)

# input: states - name of state e.g. Washington as WA
# This function returns line chart of year vs population in given state level
plot_jail_pop_by_states <- function(states_data) {
  state_data <- get_jail_pop_by_states(states_data)
  state_line <- ggplot(data = state_data) +
    geom_line(mapping = aes(x = year, y = WA, colour="WA")) +
    geom_line(mapping = aes(x = year, y = NY, colour="NY")) +
    geom_line(mapping = aes(x = year, y = CA, colour="CA")) +
    labs(title = paste("Increase of Jail Population (1970-2018) in Three States"),
         x = "Year", y = "Total Jail Population") +
    scale_color_manual(name = "States", values = c("Red", "Blue", "Green")) 
    
  return(state_line)
}

plot_jail_pop_by_states(states)


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# This function returns dataframe of jail_pop for each race (aapi, black, latinx, native, white)

race_rate_by_states <- function() {
  aapi_by_states <- incarceration %>%
    select(state, aapi_jail_pop) %>%
    replace(is.na(.), 0) %>%
    group_by(state) %>%
    summarise(jail_pop = sum(aapi_jail_pop)) %>%
    mutate(group = "Asian American / Pacific Islander")
  
  
  black_by_states <- incarceration %>%
    select(state, black_jail_pop) %>%
    replace(is.na(.), 0) %>%
    group_by(state) %>%
    summarise(jail_pop = sum(black_jail_pop)) %>%
    mutate(group = "Black")
  
  latinx_by_states <- incarceration %>%
    select(state, latinx_jail_pop) %>%
    replace(is.na(.), 0) %>%
    group_by(state) %>%
    summarise(jail_pop = sum(latinx_jail_pop)) %>%
    mutate(group = "Latinx")
  
  native_by_states <- incarceration %>%
    select(state, native_jail_pop) %>%
    replace(is.na(.), 0) %>%
    group_by(state) %>%
    summarise(jail_pop = sum(native_jail_pop)) %>%
    mutate(group = "Native American")
  
  white_by_states <- incarceration %>%
    select(state, white_jail_pop) %>%
    replace(is.na(.), 0) %>%
    group_by(state) %>%
    summarise(jail_pop = sum(white_jail_pop)) %>%
    mutate(group = "White")

  combined_race_pop <- bind_rows(aapi_by_states, black_by_states, latinx_by_states, 
                                 native_by_states, white_by_states) %>%
    filter(jail_pop > 0)
  
  combined_race_pop

}

a <- race_rate_by_states()
  
# this function creates stacked bar chart of race rate of jail population 
race_stacked_bar <- function() {
  race_data <- race_rate_by_states()
  
  stacked_bar <- ggplot(race_data) +
    geom_col(mapping=(aes(fill=group, x=state, y=jail_pop)),
             position = "dodge") +
    labs(title="Jail Population by Race in States", 
         x = "State", y="Race Rate of Jail Population")
  
  stacked_bar
}

race_stacked_bar()

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# input : shortcut state name e.g. washington as WA
# This function returns the dataframe of subregion and black_jail_pop columns to be used in map function



black_jail_pop_county <- function(state_name) {
  black_df <- incarceration %>%
   filter(state == state_name) %>%
   group_by(county_name) %>%
   summarise(black_jail_pop = sum(black_jail_pop, na.rm = TRUE)) %>%
   mutate(county = tolower(county_name)) %>%
   select(county, black_jail_pop) 
   
  black_df 
}


# input: state name
# This function returns the map of black jail population in state divided in counties

create_map <- function(state_name) {
  
  ca <- black_jail_pop_county(state_name)

  state_shape <- map_data("county") %>%
    filter(region == "california") %>%
    group_by(subregion) %>%
    summarise(long = mean(long),
              lat = mean(lat),
              group = mean(group)) 
  
  final_df <- bind_cols(state_shape, ca) %>%
    select(subregion, long, lat, group, black_jail_pop)
    
  
  map <- ggplot(final_df) +
    geom_polygon(
      mapping = aes(x=long, y = lat, group = group, fill = black_jail_pop),
      color = "white",
      linewidth = .1
    ) +
    coord_map() +
  scale_fill_continuous(
    low = "#ff7276", high = "darkred",
    limits = c(min(final_df$black_jail_pop),
               max(final_df$black_jail_pop)),
    na.value = "grey50") +
    labs(fill = "Black Jail Population", 
         titles = paste("Black Jail Population in", state_name),
         x = "", y="") 
   
  map
}

create_map("CA")

## Load data frame ---- 


