library("tidyverse")
library("lubridate")
library("scales")
library("ggthemes")
library("tidytuesdayR")


# Loading Data for the First Time
# tuesdata <- tidytuesdayR::tt_load('2021-04-20')
# netflix <- tuesdata$netflix
# write_csv(netflix, 'netflix.csv')

netflix <- read_csv('netflix.csv')


## WHAT GENRES OF SHOWS ARE ADDED TO THE NETFLIX PLATFORM MOST OFTEN ? ## 

netflix_shows_added_by_genre = netflix %>%
  
  # Keep only TV shows
  select(type, listed_in, date_added) %>%
  filter(type == "TV Show") %>%
  
  # Create an entry for each TV Show/Genre pairing
  mutate(listed_in = str_split(listed_in, ",")) %>%
  unnest(listed_in) %>%
  mutate(listed_in = str_trim(listed_in)) %>%
  
  # Retain TV Show specific categories
  filter(str_starts(listed_in, "TV"), listed_in != 'TV Shows') %>%
  mutate(listed_in = str_replace(listed_in, "TV ", "")) %>%
  
  # Parse year from date_added and remove NA dates
  mutate(year = year(parse_date_time(date_added, "b d, y"))) %>% 
  filter(!is.na(year)) %>%
  
  # Get number of of shows added for each genre-year pair
  group_by(listed_in, year) %>%
  summarise(n = n()) %>%
  
  # Convert year to POSIXct date for axis formatting
  mutate(year = parse_date_time(year, "y"))

  
# Plot number of shows added per year, by genre
netflix_shows_added_by_genre %>%
ggplot(aes(x=year)) +
geom_line(aes(y=n, color = listed_in), size=1.2) +
labs(title = "TV Shows Added to   Netflix   Over Time, by Genre",
     x = "Date Show Was Added",
     y = "# of Shows Introduced",
     color = "Genre") + 

# Additional formatting/themes
scale_x_datetime(labels = date_format("%Y")) +
theme_few() + 
scale_color_tableau() +
theme(plot.title = element_text(size=20, face="bold"))
  
