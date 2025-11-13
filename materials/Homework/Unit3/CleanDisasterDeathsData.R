## clean data for poisson assignment
library(tidyverse)
library(stringr)
library(countrycode)

disaster_deaths <- read.csv("/Users/andrealane/Documents/Teaching/ids702-fall25/materials/Homework/Unit3/deaths-natural-disasters-csv-1.csv")
population_counts <- read.csv("/Users/andrealane/Documents/Teaching/ids702-fall25/materials/Homework/Unit3/CountryPopulations.csv")

# remove "X" from the start of the year variable names
names(disaster_deaths) <- stringr::str_replace(names(disaster_deaths),
                                      "X","")

# rename the country code variable because we will remove the index
disaster_deaths <- disaster_deaths |>
  dplyr::rename(Country.Code = index.Country.Code)

# remove the index by taking the last 3 characters in the country code string
disaster_deaths <- disaster_deaths |>
  dplyr::mutate(Country.Code.clean = stringr::str_sub(Country.Code, 
                                              str_length(Country.Code)-2,
                                              str_length(Country.Code)))

# once we confirmed that the country code variable was cleaned correctly, we can
# remove the original variable for simplicity
disaster_deaths <- disaster_deaths |>
  dplyr::select(-Country.Code) |>
  dplyr::rename(CountryCode = Country.Code.clean)

# use the countrycode package to categorize countries into regions
disaster_deaths <- disaster_deaths |>
  dplyr::mutate(Region = countrycode(CountryCode,
                              origin="iso3c",
                              destination="un.region.name"))
  
# get rid of years 1900-1959 to match population data. This will also reorder
# the first 3 variables
disaster_deaths_sub <- disaster_deaths |>
  dplyr::select("Country.Name","CountryCode","Region",
                  '2014':'1960')

# sum the rows with duplicated country code names (this will also replace NA with 0)
disaster_deaths_sub2 <- disaster_deaths_sub |>
  dplyr::group_by(CountryCode) |>
  dplyr::summarize(
    Country.Name = dplyr::first(Country.Name),
    Region = dplyr::first(Region),
    across('2014':'1960', ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# pivot data long so each row corresponds to a country-year combination
disaster_deaths_long <- tidyr::pivot_longer(disaster_deaths_sub2,
                                     cols='2014':'1960',
                                     names_to="Year",
                                     values_to="Deaths")
  
# remove "X" from the start of the year variable names
names(population_counts) <- stringr::str_replace(names(population_counts),
                                               "X","")

# remove indicator variables and years 2015-2024
population_counts_sub <- population_counts |>
  dplyr::select("Country.Name","Country.Code",
                '1960':'2014')

# pivot longer
population_counts_long <- tidyr::pivot_longer(population_counts_sub,
                                              cols='1960':'2014',
                                              names_to="Year",
                                              values_to="Population")

# rename to match disaster deaths data
population_counts_long <- population_counts_long |>
  rename(CountryCode = Country.Code)

# merge population data with disaster deaths
disaster_deaths_combined <- left_join(disaster_deaths_long, 
                                      population_counts_long[,c("CountryCode",
                                                                "Year",
                                                                "Population")], 
                                      by = c("CountryCode", "Year"))
# convert Year variable to numeric
disaster_deaths_combined <- disaster_deaths_combined |>
  mutate(Year = as.numeric(Year))



disaster_deaths_mean <- disaster_deaths_combined |>
  group_by(Region, Year) |>
  summarise(mean_deaths = mean(Deaths),
            .groups = "drop")

disaster_deaths_combined_nomiss <- disaster_deaths_combined |>
  drop_na(Population)

disaster_deaths_rate <- disaster_deaths_combined_nomiss |>
  mutate(Death.rate = Deaths/Population *100000)

disaster_deaths_rate_mean <- disaster_deaths_rate |>
  group_by(Region, Year) |>
  summarise(mean_deaths_rate = mean(Death.rate),
            .groups = "drop")



