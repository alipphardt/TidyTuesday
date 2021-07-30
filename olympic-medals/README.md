Olympic Medals
================
Developed by Anthony Lipphardt

# TidyTuesday

Join the R4DS Online Learning Community in the weekly
[\#TidyTuesday](https://github.com/rfordatascience/tidytuesday) event\!

Every week we post a raw dataset, a chart or article related to that
dataset, and ask you to explore the data.

While the dataset will be “tamed”, it will not always be tidy\! As such
you might need to apply various R for Data Science techniques to wrangle
the data into a true tidy format.

The goal of TidyTuesday is to apply your R skills, get feedback, explore
other’s work, and connect with the greater \#RStats community\!

As such we encourage everyone of all skills to participate\!

``` r
knitr::opts_chunk$set(echo = TRUE)

library(tidytuesdayR)
library(tidyverse)

library(lubridate)
library(scales)
library(glue)
library(ggtext)
library(Cairo)


library(extrafont)
loadfonts(device = "win", quiet = TRUE)
```

# Loading the Weekly Dataset

Download the weekly data and make it available in the `olympics` object.

[Olympic
Games](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-27/readme.md)

``` r
# Loading Data for the First Time
#tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
#olympics <- tuesdata$olympics
#write_csv(olympics, 'olympics.csv')

olympics <- read_csv('olympics.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   id = col_double(),
    ##   name = col_character(),
    ##   sex = col_character(),
    ##   age = col_double(),
    ##   height = col_double(),
    ##   weight = col_double(),
    ##   team = col_character(),
    ##   noc = col_character(),
    ##   games = col_character(),
    ##   year = col_double(),
    ##   season = col_character(),
    ##   city = col_character(),
    ##   sport = col_character(),
    ##   event = col_character(),
    ##   medal = col_character()
    ## )

# Sample Data

Take an initial look at the format of the data available.

``` r
glimpse(olympics)
```

    ## Rows: 271,116
    ## Columns: 15
    ## $ id     <dbl> 1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, ~
    ## $ name   <chr> "A Dijiang", "A Lamusi", "Gunnar Nielsen Aaby", "Edgar Lindenau~
    ## $ sex    <chr> "M", "M", "M", "M", "F", "F", "F", "F", "F", "F", "M", "M", "M"~
    ## $ age    <dbl> 24, 23, 24, 34, 21, 21, 25, 25, 27, 27, 31, 31, 31, 31, 33, 33,~
    ## $ height <dbl> 180, 170, NA, NA, 185, 185, 185, 185, 185, 185, 188, 188, 188, ~
    ## $ weight <dbl> 80, 60, NA, NA, 82, 82, 82, 82, 82, 82, 75, 75, 75, 75, 75, 75,~
    ## $ team   <chr> "China", "China", "Denmark", "Denmark/Sweden", "Netherlands", "~
    ## $ noc    <chr> "CHN", "CHN", "DEN", "DEN", "NED", "NED", "NED", "NED", "NED", ~
    ## $ games  <chr> "1992 Summer", "2012 Summer", "1920 Summer", "1900 Summer", "19~
    ## $ year   <dbl> 1992, 2012, 1920, 1900, 1988, 1988, 1992, 1992, 1994, 1994, 199~
    ## $ season <chr> "Summer", "Summer", "Summer", "Summer", "Winter", "Winter", "Wi~
    ## $ city   <chr> "Barcelona", "London", "Antwerpen", "Paris", "Calgary", "Calgar~
    ## $ sport  <chr> "Basketball", "Judo", "Football", "Tug-Of-War", "Speed Skating"~
    ## $ event  <chr> "Basketball Men's Basketball", "Judo Men's Extra-Lightweight", ~
    ## $ medal  <chr> NA, NA, NA, "Gold", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~

# Wrangling and Exploration

Unique cities for Olympics

``` r
categorical_count = function(tbl, column){
  tbl %>% count({{column}}, sort = TRUE)
}

print.data.frame(categorical_count(olympics, city))
```

    ##                      city     n
    ## 1                  London 22426
    ## 2                  Athina 15556
    ## 3                  Sydney 13821
    ## 4                 Atlanta 13780
    ## 5          Rio de Janeiro 13688
    ## 6                 Beijing 13602
    ## 7               Barcelona 12977
    ## 8             Los Angeles 12423
    ## 9                   Seoul 12037
    ## 10                 Munich 10304
    ## 11               Montreal  8641
    ## 12            Mexico City  8588
    ## 13               Helsinki  8270
    ## 14                   Roma  8119
    ## 15                  Tokyo  7702
    ## 16                 Moskva  7191
    ## 17                  Paris  7169
    ## 18                 Berlin  6506
    ## 19              Amsterdam  4992
    ## 20                  Sochi  4891
    ## 21              Melbourne  4829
    ## 22              Vancouver  4402
    ## 23                 Torino  4382
    ## 24              Stockholm  4338
    ## 25              Antwerpen  4292
    ## 26         Salt Lake City  4109
    ## 27              Innsbruck  3639
    ## 28                 Nagano  3605
    ## 29            Albertville  3436
    ## 30            Lillehammer  3160
    ## 31                Calgary  2639
    ## 32               Sarajevo  2134
    ## 33            Lake Placid  2098
    ## 34               Grenoble  1891
    ## 35           Sankt Moritz  1657
    ## 36                Sapporo  1655
    ## 37      Cortina d'Ampezzo  1307
    ## 38              St. Louis  1301
    ## 39           Squaw Valley  1116
    ## 40                   Oslo  1088
    ## 41 Garmisch-Partenkirchen   895
    ## 42               Chamonix   460

``` r
write_csv(categorical_count(olympics, city), 'olympic-cities.csv')

categorical_count(olympics, team)
```

    ## # A tibble: 1,184 x 2
    ##    team              n
    ##    <chr>         <int>
    ##  1 United States 17847
    ##  2 France        11988
    ##  3 Great Britain 11404
    ##  4 Italy         10260
    ##  5 Germany        9326
    ##  6 Canada         9279
    ##  7 Japan          8289
    ##  8 Sweden         8052
    ##  9 Australia      7513
    ## 10 Hungary        6547
    ## # ... with 1,174 more rows

Let’s focus on the top 10 medal earning teams and see which of those
teams have hosted an Olympic Games.

``` r
# Loading a file of city and home team pairs (source: wikipedia)
home_team_pairs = read_csv('city-home-team-pairs.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   city = col_character(),
    ##   home_team = col_character(),
    ##   flag = col_double()
    ## )

``` r
# Recoding select team names

olympics = olympics %>%
  mutate(team_clean = case_when(
    str_detect(team, "United States") ~ "United States",
    str_detect(team, "France") ~ "France",
    str_detect(team, "Great Britain") ~ "Great Britain",
    str_detect(team, "Italy") ~ "Italy",
    str_detect(team, "Germany") ~ "Germany",
    str_detect(team, "Canada") ~ "Canada",
    str_detect(team, "Japan") ~ "Japan",
    str_detect(team, "Sweden") ~ "Sweden",
    str_detect(team, "Australia") ~ "Australia",
    str_detect(team, "Hungary") ~ "Hungary",
    TRUE ~ team
  ))

print.data.frame(categorical_count(olympics %>% filter(team != team_clean), team))
```

    ##                                team    n
    ## 1                      West Germany 3199
    ## 2                      East Germany 2543
    ## 3                   United States-1  277
    ## 4                   United States-2  277
    ## 5      United States Virgin Islands  270
    ## 6                   Great Britain-1  191
    ## 7                   Great Britain-2  189
    ## 8                         Germany-1  162
    ## 9                         Germany-2  161
    ## 10                          Italy-2  156
    ## 11                          Italy-1  155
    ## 12                         Canada-1  151
    ## 13                         Canada-2  150
    ## 14                         France-1  135
    ## 15                         France-2  121
    ## 16                          Japan-1   66
    ## 17                          Japan-2   66
    ## 18                   West Germany-1   56
    ## 19                   West Germany-2   56
    ## 20                   East Germany-1   48
    ## 21                   East Germany-2   48
    ## 22                         Sweden-2   46
    ## 23                  United States-3   46
    ## 24                         Sweden-1   44
    ## 25                      Australia-1   41
    ## 26                      Australia-2   40
    ## 27                         Canada-3   26
    ## 28                        Germany-3   24
    ## 29                  Great Britain-3   18
    ## 30                        Hungary-2   16
    ## 31                        Hungary-1   14
    ## 32                         France-3   12
    ## 33   United States Virgin Islands-1   12
    ## 34   United States Virgin Islands-2   12
    ## 35            Racing Club de France   11
    ## 36 United States Golf Association-3   10
    ## 37                  Great Britain-4    8
    ## 38                         Sweden-3    8
    ## 39                   Denmark/Sweden    6
    ## 40                   East Germany-3    6
    ## 41      United States/Great Britain    4
    ## 42                   West Germany-3    4
    ## 43                      Australia-3    2
    ## 44          Australia/Great Britain    2
    ## 45            Bohemia/Great Britain    2
    ## 46                         France-4    2
    ## 47             France/Great Britain    2
    ## 48            Germany/United States    2
    ## 49            Great Britain/Germany    2
    ## 50                        Hungary-3    2
    ## 51                          Italy-3    2
    ## 52                          Japan-3    2
    ## 53                         Sweden-4    2
    ## 54                 United States-10    2
    ## 55                 United States-11    2
    ## 56                 United States-12    2
    ## 57                 United States-13    2
    ## 58                 United States-14    2
    ## 59                  United States-4    2
    ## 60                  United States-5    2
    ## 61                  United States-6    2
    ## 62                  United States-7    2
    ## 63                  United States-8    2
    ## 64                  United States-9    2
    ## 65             United States/France    2

``` r
# Get top 5 teams
top5_teams = categorical_count(olympics, team) %>% slice_max(n = 5, n, with_ties = FALSE) %>% pull(team)

# Join on home team pairs data frame and flag medals where it was a home team win

olympics = olympics %>%
  left_join(home_team_pairs) %>%
  mutate(flag = case_when(
    team_clean == home_team ~ "Yes",
    TRUE ~ "No"
  ))
```

    ## Joining, by = "city"

``` r
# How many medals were home team wins?
olympics %>% count(flag)
```

    ## # A tibble: 2 x 2
    ##   flag       n
    ##   <chr>  <int>
    ## 1 No    251364
    ## 2 Yes    19752

# Visualization

Now, lets visualize home team medals for top 10 countries:

``` r
olympics = olympics %>%
  filter(team_clean %in% top5_teams,
         !is.na(medal))

host_wins = olympics %>%
  filter(team_clean %in% top5_teams) %>%
  select(year, team_clean, flag) %>%
  mutate(flag = as.factor(flag),
         flag = fct_rev(flag)) %>%
  ggplot(aes(x = year, fill = flag)) +
  geom_bar() +
  facet_grid(rows = vars(team_clean)) +
  theme_minimal() +
  scale_fill_manual(values = c("#168c39","#ee2f4d")) +
  labs(title = "Is there a home team advantage for Olympic host countries?",
       subtitle = "Total medals (gold, silver, bronze) are indicated for the top 5 countries. Until 1992, the Winter and Summer games were held in the same year.",
       caption = "<strong>Source:</strong> Kaggle | <strong>Visualization:</strong> @a_lipphardt",
       y = "Number of Medals",
       x = "",
       fill = "Home Team Win?") +
  theme(legend.position = "top",
        plot.title = element_textbox_simple(family = "Bahnschrift",
                                            halign = 0,
                                            size = 24,
                                            margin = margin(t=10,b=10)),
        
        plot.subtitle = element_textbox_simple(family = "Segoe UI",
                                            halign = 0,
                                            size = 16,
                                            margin = margin(b=20, 
                                                            r = 20)),
        
        plot.caption = element_textbox_simple(family = "Segoe UI Semilight",
                                              size = 16,
                                              halign = 1,
                                              margin = margin(r = 0, t = 20, b = 5)),
        legend.title = element_text(family = "Segoe UI Semilight", 
                                    size = 12),
        
        legend.text = element_text(family = "Segoe UI Semilight",
                                   size = 12),
        
        axis.text = element_text(family = "Segoe UI Semilight",
                                 size = 10),   
        
        axis.title.y = element_text(family = "Segoe UI Semilight",
                                 size = 14,
                                 margin = margin(r = 10)),
        
        strip.text = element_text(family = "Segoe UI Semilight",
                                  size = 14),
        
        plot.background = element_rect(fill = "#F9F8EF", color = "#F9F8EF")
        )
```

# Saving Image(s)

Save your image for sharing. Be sure to use the `#TidyTuesday` hashtag
in your post on twitter\!

``` r
ggsave(plot = host_wins,
       filename = "host-wins.png",
       width=16,
       height=9,
       type = "cairo",
       device = "png",
       dpi = 300)
```

![Is there a home team advantage for Olympic host countries. Total
medals (gold, silver, bronze) are indicated for the top 5 countries.
Until 1992, the Winter and Summer games were held in the same
year.](host-wins.png)
