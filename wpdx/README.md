Water Sources
================
Developed by Anthony Lipphardt

# TidyTuesday

Join the R4DS Online Learning Community in the weekly
[\#TidyTuesday](https://github.com/rfordatascience/tidytuesday) event\!

Every week we post a raw dataset, a chart or article related to that
dataset, and ask you to explore the data.

While the dataset will be “tamed”, it will not always be tidy\! As such
you might need to apply various R for Data Science techniques to wrangle
the data into a true tidy format. The goal of TidyTuesday is to apply
your R skills, get feedback, explore other’s work, and connect with the
greater \#RStats community\! As such we encourage everyone of all skills
to participate\!

# Loading the Weekly Dataset

Download the weekly data and make available in the `water` object.

[Water
Sources](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-04/readme.md)

``` r
# Loading Data for the First Time
# tuesdata <- tidytuesdayR::tt_load(2021, week = 19)
# water <- tuesdata$water
# write_csv(water, 'water.csv')

water <- read_csv('water.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   row_id = col_double(),
    ##   lat_deg = col_double(),
    ##   lon_deg = col_double(),
    ##   report_date = col_character(),
    ##   status_id = col_character(),
    ##   water_source = col_character(),
    ##   water_tech = col_character(),
    ##   facility_type = col_character(),
    ##   country_name = col_character(),
    ##   install_year = col_double(),
    ##   installer = col_character(),
    ##   pay = col_character(),
    ##   status = col_character()
    ## )

# Sample Data

Take an initial look at the format of the data available.

``` r
head(glimpse(water), 10)
```

    ## Rows: 473,293
    ## Columns: 13
    ## $ row_id        <dbl> 3957, 33512, 35125, 37760, 38118, 38501, 46357, 46535...
    ## $ lat_deg       <dbl> 8.0731360, 7.3737842, 0.7734576, 0.7805757, 0.7792664...
    ## $ lon_deg       <dbl> 38.61704, 40.50382, 34.92951, 34.96364, 34.97112, 34....
    ## $ report_date   <chr> "04/06/2017", "08/04/2020", "03/18/2015", "03/18/2015...
    ## $ status_id     <chr> "y", "y", "y", "y", "y", "y", "y", "y", "y", "y", "y"...
    ## $ water_source  <chr> NA, "Protected Spring", "Protected Shallow Well", "Bo...
    ## $ water_tech    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Hand Pum...
    ## $ facility_type <chr> NA, "Improved", "Improved", "Improved", "Improved", "...
    ## $ country_name  <chr> "Ethiopia", "Ethiopia", "Kenya", "Kenya", "Kenya", "K...
    ## $ install_year  <dbl> NA, 2019, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 201...
    ## $ installer     <chr> "Private-CRS", "WaterAid", NA, NA, NA, NA, NA, NA, NA...
    ## $ pay           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ status        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "Yes pump...

    ## # A tibble: 10 x 13
    ##    row_id lat_deg lon_deg report_date status_id water_source water_tech
    ##     <dbl>   <dbl>   <dbl> <chr>       <chr>     <chr>        <chr>     
    ##  1   3957   8.07     38.6 04/06/2017  y         <NA>         <NA>      
    ##  2  33512   7.37     40.5 08/04/2020  y         Protected S~ <NA>      
    ##  3  35125   0.773    34.9 03/18/2015  y         Protected S~ <NA>      
    ##  4  37760   0.781    35.0 03/18/2015  y         Borehole     <NA>      
    ##  5  38118   0.779    35.0 03/18/2015  y         Protected S~ <NA>      
    ##  6  38501   0.308    34.1 03/19/2015  y         Borehole     <NA>      
    ##  7  46357   0.419    34.3 05/19/2015  y         Unprotected~ <NA>      
    ##  8  46535   0.444    34.3 05/19/2015  y         Protected S~ <NA>      
    ##  9  46560   0.456    34.3 05/19/2015  y         Protected S~ <NA>      
    ## 10  46782   0.467    34.3 05/20/2015  y         Protected S~ <NA>      
    ## # ... with 6 more variables: facility_type <chr>, country_name <chr>,
    ## #   install_year <dbl>, installer <chr>, pay <chr>, status <chr>

# Wrangling

Explore the data and process it into prepared dataframes for
visualization.

``` r
water_africa = water %>%
  
  select(row_id, 
         lat_deg, 
         lon_deg, 
         report_date, 
         water_source, 
         install_year, 
         country_name) %>%
  
  mutate(date = parse_date_time(report_date, "m/d/Y"),
         water_source = case_when(
           str_detect(water_source, "Spring") ~ "Spring",
           str_detect(water_source, "Shallow Well") ~ "Shallow Well",
           str_detect(water_source, "Surface Water") ~ "Surface Water",
           TRUE ~ water_source
           ),
         year = year(date)) %>%
  
  filter(!is.na(country_name), 
         !is.na(water_source),
         
         # Exclude Peru
         country_name != "Peru",
         
         # Limit to after 2000 and records that were in the year of install
         year == install_year,
         year >= 2000,
         
         # Restrict to bounding box around continental Africa and madagascar
         lat_deg < 55,
         lat_deg > -25,
         lon_deg > -38,
         lon_deg < 40)
```

# Visualization(s)

Using your processed dataset, create your unique visualization(s).

``` r
plot <- ggplot() +
  
  # Only show shapes in world map contained in specified bounding box
  borders("world", 
          colour = "gray85", 
          fill = "gray80",
          ylim = c(-38,40), # longitude
          xlim = c(-25,55)) + # latitude
  
  theme_map() +
  
  # Show points colored by water source type. Grouping variable is set to row_id so that
  # gganimate treats each record as distinct point without tween animations
  geom_point(aes(x = lon_deg, y = lat_deg, color = water_source, group=row_id),
             data = water_africa, 
             alpha = 1,
             size = 1,
             show.legend = TRUE) +

  # Points should be added to map by year, fading in upon enter, and once added
  # should not be removed
  transition_time(year) +
  enter_fade() +
  shadow_mark(past = TRUE, future=FALSE) +  
  ggtitle('Water Sources in Africa Installed Over Time: {as.integer(frame_time)}') +
  
  # Extra formatting to adjust titles and size of point in legend
  labs(color = "Water Source") + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12)) +
   guides(color = guide_legend(override.aes = list(size=5))) +
  
  # Force Aspect Ratio
  coord_fixed()


# Render a PNG for each unique frame and stitch together into animated GIF
animate(plot, renderer = gifski_renderer(), units = "px", width = 500, height = 500)

# Save to GIF file for immediate display
anim_save('africa-water-installations.gif')
```

![‘Water sources installed in Africa between 2000 and 2020. Bore hole
installations are by far the most frequent
type’](africa-water-installations.gif)
