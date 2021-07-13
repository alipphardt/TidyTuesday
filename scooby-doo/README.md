Scooby Doo Episodes
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

library(colormap)
library(viridis)
library(ggforce)
library(igraph)
library(ggraph)
```

# Loading the Weekly Dataset

Download the weekly data and make it available in the `scoobydoo`
object.

[Scooby Doo
Episodes](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-13/readme.md)

``` r
# Loading Data for the First Time
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
```

    ## --- Compiling #TidyTuesday Information for 2021-07-13 ----

    ## --- There is 1 file available ---

    ## --- Starting Download ---

    ## 
    ##  Downloading file 1 of 1: `scoobydoo.csv`

    ## --- Download complete ---

``` r
scoobydoo <- tuesdata$scoobydoo
write_csv(scoobydoo, 'scoobydoo.csv')

scoobydoo <- read_csv('scoobydoo.csv') %>%
  mutate(monster_name = str_replace(monster_name, "Agent,4", "Agent 4,"),
         monster_name = str_replace(monster_name, "Agent,8", "Agent 8,"),
         monster_name = str_replace(monster_name, "Agent,9", "Agent 9,"), 
         monster_name = str_replace_all(monster_name, "Bogle", "Bogel"), 
         monster_name = str_replace_all(monster_name, "Demons", "Demon"),          
         monster_name = str_replace_all(monster_name, "Phantoms", "Phantom"),          
         monster_name = str_replace_all(monster_name, "Zombies", "Zombie"), 
         monster_name = str_replace_all(monster_name, "Frankenstein's Monster", "Frankenstein"),
         monster_name = str_replace_all(monster_name, "Evil Trees", "Evil Tree"), 
         monster_name = str_replace_all(monster_name, "Dr Trebla", "Dr. Trebla"),
         monster_name = str_replace_all(monster_name, "Trebal", "Trebla"), 
         monster_name = str_replace_all(monster_name, pattern = regex("Agent \\d+"), replacement = "Agent X"))
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character(),
    ##   index = col_double(),
    ##   date_aired = col_date(format = ""),
    ##   run_time = col_double(),
    ##   monster_amount = col_double(),
    ##   unmask_other = col_logical(),
    ##   caught_other = col_logical(),
    ##   caught_not = col_logical(),
    ##   suspects_amount = col_double(),
    ##   culprit_amount = col_double(),
    ##   door_gag = col_logical(),
    ##   batman = col_logical(),
    ##   scooby_dum = col_logical(),
    ##   scrappy_doo = col_logical(),
    ##   hex_girls = col_logical(),
    ##   blue_falcon = col_logical()
    ## )
    ## i Use `spec()` for the full column specifications.

# Sample Data

Take an initial look at the format of the data available.

``` r
glimpse(scoobydoo)
```

    ## Rows: 603
    ## Columns: 75
    ## $ index                    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14~
    ## $ series_name              <chr> "Scooby Doo, Where Are You!", "Scooby Doo, Wh~
    ## $ network                  <chr> "CBS", "CBS", "CBS", "CBS", "CBS", "CBS", "CB~
    ## $ season                   <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", ~
    ## $ title                    <chr> "What a Night for a Knight", "A Clue for Scoo~
    ## $ imdb                     <chr> "8.1", "8.1", "8", "7.8", "7.5", "8.4", "7.6"~
    ## $ engagement               <chr> "556", "479", "455", "426", "391", "384", "35~
    ## $ date_aired               <date> 1969-09-13, 1969-09-20, 1969-09-27, 1969-10-~
    ## $ run_time                 <dbl> 21, 22, 21, 21, 21, 21, 21, 21, 21, 21, 21, 2~
    ## $ format                   <chr> "TV Series", "TV Series", "TV Series", "TV Se~
    ## $ monster_name             <chr> "Black Knight", "Ghost of Cptn. Cuttler", "Ph~
    ## $ monster_gender           <chr> "Male", "Male", "Male", "Male", "Female", "Ma~
    ## $ monster_type             <chr> "Possessed Object", "Ghost", "Ghost", "Ancien~
    ## $ monster_subtype          <chr> "Suit", "Suit", "Phantom", "Miner", "Witch Do~
    ## $ monster_species          <chr> "Object", "Human", "Human", "Human", "Human",~
    ## $ monster_real             <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ monster_amount           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 2, 1, 1, ~
    ## $ caught_fred              <chr> "FALSE", "FALSE", "FALSE", "TRUE", "FALSE", "~
    ## $ caught_daphnie           <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ caught_velma             <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ caught_shaggy            <chr> "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "F~
    ## $ caught_scooby            <chr> "TRUE", "FALSE", "TRUE", "FALSE", "TRUE", "FA~
    ## $ captured_fred            <chr> "FALSE", "TRUE", "FALSE", "FALSE", "FALSE", "~
    ## $ captured_daphnie         <chr> "FALSE", "TRUE", "FALSE", "FALSE", "FALSE", "~
    ## $ captured_velma           <chr> "FALSE", "TRUE", "FALSE", "FALSE", "FALSE", "~
    ## $ captured_shaggy          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ captured_scooby          <chr> "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "~
    ## $ unmask_fred              <chr> "FALSE", "TRUE", "TRUE", "TRUE", "FALSE", "TR~
    ## $ unmask_daphnie           <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ unmask_velma             <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ unmask_shaggy            <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ unmask_scooby            <chr> "TRUE", "FALSE", "FALSE", "FALSE", "TRUE", "F~
    ## $ snack_fred               <chr> "TRUE", "FALSE", "TRUE", "FALSE", "FALSE", "T~
    ## $ snack_daphnie            <chr> "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "F~
    ## $ snack_velma              <chr> "FALSE", "TRUE", "FALSE", "FALSE", "FALSE", "~
    ## $ snack_shaggy             <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ snack_scooby             <chr> "FALSE", "FALSE", "FALSE", "FALSE", "FALSE", ~
    ## $ unmask_other             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ caught_other             <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ caught_not               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ trap_work_first          <chr> "NULL", "FALSE", "FALSE", "TRUE", "NULL", "TR~
    ## $ setting_terrain          <chr> "Urban", "Coast", "Island", "Cave", "Desert",~
    ## $ setting_country_state    <chr> "United States", "United States", "United Sta~
    ## $ suspects_amount          <dbl> 2, 2, 0, 2, 1, 2, 1, 2, 1, 1, 1, 1, 2, 2, 1, ~
    ## $ non_suspect              <chr> "FALSE", "TRUE", "TRUE", "FALSE", "FALSE", "F~
    ## $ arrested                 <chr> "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE~
    ## $ culprit_name             <chr> "Mr. Wickles", "Cptn. Cuttler", "Bluestone th~
    ## $ culprit_gender           <chr> "Male", "Male", "Male", "Male", "Male", "Male~
    ## $ culprit_amount           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, ~
    ## $ motive                   <chr> "Theft", "Theft", "Treasure", "Natural Resour~
    ## $ if_it_wasnt_for          <chr> "NULL", "NULL", "NULL", "NULL", "NULL", "NULL~
    ## $ and_that                 <chr> "NULL", "NULL", "NULL", "NULL", "NULL", "NULL~
    ## $ door_gag                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ number_of_snacks         <chr> "2", "1", "3", "2", "2", "4", "4", "0", "1", ~
    ## $ split_up                 <chr> "1", "0", "0", "1", "0", "0", "1", "0", "0", ~
    ## $ another_mystery          <chr> "1", "0", "0", "0", "1", "0", "0", "0", "0", ~
    ## $ set_a_trap               <chr> "0", "0", "0", "0", "0", "0", "1", "1", "0", ~
    ## $ jeepers                  <chr> "0", "0", "0", "0", "0", "1", "0", "0", "0", ~
    ## $ jinkies                  <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", ~
    ## $ my_glasses               <chr> "1", "0", "0", "0", "1", "0", "0", "1", "0", ~
    ## $ just_about_wrapped_up    <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", ~
    ## $ zoinks                   <chr> "1", "3", "1", "2", "0", "2", "1", "0", "0", ~
    ## $ groovy                   <chr> "0", "0", "2", "1", "0", "0", "1", "0", "0", ~
    ## $ scooby_doo_where_are_you <chr> "0", "1", "0", "0", "1", "0", "0", "1", "0", ~
    ## $ rooby_rooby_roo          <chr> "1", "0", "0", "0", "0", "1", "1", "1", "1", ~
    ## $ batman                   <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ scooby_dum               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ scrappy_doo              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ hex_girls                <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ blue_falcon              <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FAL~
    ## $ fred_va                  <chr> "Frank Welker", "Frank Welker", "Frank Welker~
    ## $ daphnie_va               <chr> "Stefanianna Christopherson", "Stefanianna Ch~
    ## $ velma_va                 <chr> "Nicole Jaffe", "Nicole Jaffe", "Nicole Jaffe~
    ## $ shaggy_va                <chr> "Casey Kasem", "Casey Kasem", "Casey Kasem", ~
    ## $ scooby_va                <chr> "Don Messick", "Don Messick", "Don Messick", ~

# Wrangling and Exploration

Counts for several categoricals.

``` r
categorical_count = function(tbl, column){
  tbl %>% count({{column}}, sort = TRUE)
}

categorical_count(scoobydoo, series_name)
```

    ## # A tibble: 29 x 2
    ##    series_name                                    n
    ##    <chr>                                      <int>
    ##  1 Scooby-Doo and Scrappy-Doo (second series)    86
    ##  2 Be Cool, Scooby-Doo!                          53
    ##  3 Scooby-Doo Mystery Incorporated               52
    ##  4 Laff-a-Lympics                                48
    ##  5 Warner Home Video                             42
    ##  6 What's New Scooby-Doo?                        42
    ##  7 Scooby-Doo and Guess Who?                     41
    ##  8 The Scooby-Doo Show                           40
    ##  9 A Pup Named Scooby-Doo                        30
    ## 10 Shaggy & Scooby-Doo Get a Clue!               26
    ## # ... with 19 more rows

``` r
categorical_count(scoobydoo, network)
```

    ## # A tibble: 11 x 2
    ##    network                  n
    ##    <chr>                <int>
    ##  1 ABC                    281
    ##  2 Cartoon Network         84
    ##  3 Boomerang               74
    ##  4 CBS                     49
    ##  5 The WB                  41
    ##  6 Warner Home Video       39
    ##  7 The CW                  27
    ##  8 Syndication              3
    ##  9 Warner Bros. Picture     3
    ## 10 Adult Swim               1
    ## 11 TBC                      1

``` r
categorical_count(scoobydoo, format)
```

    ## # A tibble: 5 x 2
    ##   format                    n
    ##   <chr>                 <int>
    ## 1 TV Series               374
    ## 2 TV Series (segmented)   175
    ## 3 Movie                    43
    ## 4 Crossover                 8
    ## 5 Movie (Theatrical)        3

``` r
categorical_count(scoobydoo, monster_gender)
```

    ## # A tibble: 44 x 2
    ##    monster_gender                         n
    ##    <chr>                              <int>
    ##  1 Male                                 319
    ##  2 NULL                                  87
    ##  3 Male,Male                             47
    ##  4 Female                                30
    ##  5 Male,Male,Male                        23
    ##  6 Male,Male,Male,Male                   15
    ##  7 Male,Female                           11
    ##  8 Female,Male                            5
    ##  9 Male,Male,Male,Male,Male,Male,Male     5
    ## 10 Male,Female,Male                       4
    ## # ... with 34 more rows

``` r
categorical_count(scoobydoo, monster_real)
```

    ## # A tibble: 3 x 2
    ##   monster_real     n
    ##   <chr>        <int>
    ## 1 FALSE          404
    ## 2 TRUE           112
    ## 3 NULL            87

``` r
categorical_count(scoobydoo, setting_terrain)
```

    ## # A tibble: 15 x 2
    ##    setting_terrain     n
    ##    <chr>           <int>
    ##  1 Urban             267
    ##  2 Rural             109
    ##  3 Forest             48
    ##  4 Desert             40
    ##  5 Island             25
    ##  6 Snow               25
    ##  7 Swamp              19
    ##  8 Coast              17
    ##  9 Jungle             15
    ## 10 Ocean              13
    ## 11 Cave                9
    ## 12 Mountain            9
    ## 13 Space               5
    ## 14 Air                 1
    ## 15 Moon                1

``` r
categorical_count(scoobydoo, setting_country_state)
```

    ## # A tibble: 79 x 2
    ##    setting_country_state     n
    ##    <chr>                 <int>
    ##  1 United States           309
    ##  2 California               75
    ##  3 England                  12
    ##  4 Mexico                   10
    ##  5 Louisiana                 8
    ##  6 New York                  8
    ##  7 Canada                    7
    ##  8 Greece                    7
    ##  9 France                    6
    ## 10 Japan                     6
    ## # ... with 69 more rows

``` r
categorical_count(scoobydoo, non_suspect)
```

    ## # A tibble: 3 x 2
    ##   non_suspect     n
    ##   <chr>       <int>
    ## 1 FALSE         397
    ## 2 NULL          160
    ## 3 TRUE           46

``` r
categorical_count(scoobydoo, arrested)
```

    ## # A tibble: 3 x 2
    ##   arrested     n
    ##   <chr>    <int>
    ## 1 TRUE       381
    ## 2 NULL       155
    ## 3 FALSE       67

``` r
categorical_count(scoobydoo, culprit_gender)
```

    ## # A tibble: 24 x 2
    ##    culprit_gender       n
    ##    <chr>            <int>
    ##  1 Male               263
    ##  2 NULL               163
    ##  3 Female              61
    ##  4 Male,Male           47
    ##  5 Male,Female         20
    ##  6 Female,Male         15
    ##  7 Male,Male,Male       7
    ##  8 Female,Female        4
    ##  9 Female,Male,Male     3
    ## 10 Male,Female,Male     3
    ## # ... with 14 more rows

``` r
categorical_count(scoobydoo, if_it_wasnt_for)
```

    ## # A tibble: 108 x 2
    ##    if_it_wasnt_for                                        n
    ##    <chr>                                              <int>
    ##  1 NULL                                                 414
    ##  2 you meddling kids                                     65
    ##  3 you pesky kids                                         9
    ##  4 those meddling kids                                    4
    ##  5 you meddling-                                          3
    ##  6 you                                                    2
    ##  7 you meddling grown-ups                                 2
    ##  8 you meddling kids who like an idiot I invited here     2
    ##  9 you meddling kids, eh                                  2
    ## 10 you meddling mainlanders                               2
    ## # ... with 98 more rows

``` r
categorical_count(scoobydoo, and_that)
```

    ## # A tibble: 65 x 2
    ##    and_that                                                                    n
    ##    <chr>                                                                   <int>
    ##  1 NULL                                                                      528
    ##  2 puppy                                                                       9
    ##  3 dog                                                                         4
    ##  4 actress, singer, dancer and comedian Sandy Duncan                           1
    ##  5 and                                                                         1
    ##  6 and country music sensation Kacey Musgraves                                 1
    ##  7 and famed TV personality and game show host, Alex Trebek                    1
    ##  8 and famour Justice Leaguer and superhero speedster, The Flash               1
    ##  9 and famous singer/award-winning, multi-platinum, alternate pop maveric~     1
    ## 10 and siner-songwriter, musician and rock god, Axl Rose                       1
    ## # ... with 55 more rows

``` r
categorical_count(scoobydoo, door_gag)
```

    ## # A tibble: 2 x 2
    ##   door_gag     n
    ##   <lgl>    <int>
    ## 1 FALSE      544
    ## 2 TRUE        59

``` r
categorical_count(scoobydoo, split_up)
```

    ## # A tibble: 4 x 2
    ##   split_up     n
    ##   <chr>    <int>
    ## 1 0          270
    ## 2 NULL       219
    ## 3 1           99
    ## 4 2           15

``` r
categorical_count(scoobydoo, another_mystery)
```

    ## # A tibble: 5 x 2
    ##   another_mystery     n
    ##   <chr>           <int>
    ## 1 0                 320
    ## 2 NULL              219
    ## 3 1                  61
    ## 4 2                   2
    ## 5 3                   1

``` r
categorical_count(scoobydoo, batman)
```

    ## # A tibble: 2 x 2
    ##   batman     n
    ##   <lgl>  <int>
    ## 1 FALSE    599
    ## 2 TRUE       4

**monster\_name** look’s like it contains all monsters that appear in a
given episode and from the looks of it there are some common monsters
that have appeared in the series. Let’s see whether we can unpack this
column to get monsters that have appeared together.

NOTE: Plural versions of monster names have been resolved to singular
(e.g. Demons to Demon, Zombies to Zombie, etc) and mispellings have been
resolved. There are also listings for each of the individual Agent
characters, which have been collapsed into a single Agent X for a more
interesting graph.

``` r
# Unpack monster_name
monster_names = scoobydoo %>% select(index, monster_name) %>% mutate(monster_name = str_split(monster_name, ",")) %>% unnest(monster_name) %>% distinct(index, monster_name)

# Number of episodes monster has appeared in
monster_name_counts = monster_names %>% filter(!(monster_name %in% c("NULL", ""))) %>% count(monster_name, sort=TRUE)

# Monsters that have appears more than once
monster_name_counts %>% filter(n > 1)
```

    ## # A tibble: 66 x 2
    ##    monster_name           n
    ##    <chr>              <int>
    ##  1 Dr. Phineus Phibes    26
    ##  2 Agent X               25
    ##  3 Dr. Trebla            24
    ##  4 Mark                  12
    ##  5 Ricky                 12
    ##  6 Bogel                  9
    ##  7 Mummy                  9
    ##  8 Weerd                  9
    ##  9 Zombie                 9
    ## 10 Phantom                7
    ## # ... with 56 more rows

``` r
# Monsters that were paired together
monster_pairs = monster_names %>% inner_join(monster_names, by = "index") %>% 
  filter(monster_name.x != monster_name.y,
         monster_name.x < monster_name.y) %>% 
  unique()

# Top monster pairings
monster_pair_counts = monster_pairs %>% count(monster_name.x, monster_name.y, sort=TRUE)

# Unique monster names in pairings
unique_names = unique(c(monster_pairs$monster_name.x, monster_pairs$monster_name.y))

# Pull the top 50 monsters by number of appearances. 
unique_names_top = monster_name_counts %>% 
  filter(monster_name %in% unique_names) %>% 
  slice_max(n=50, n, with_ties = FALSE) %>%
  pull(monster_name)

# Now pull all pairings for those top 50 monsters
monster_pair_counts_top = monster_pair_counts %>% 
  filter( (monster_name.x %in% unique_names_top) & (monster_name.y %in% unique_names_top) ) %>%
  arrange(monster_name.x)
```

# Visualization

Create a chord diagram, with monster names arranged in a circle. Each
point around the circle should relate to the number of episodes each
monster appeared in.

``` r
# Scooby Doo Palette
scooby_blue = "#8FEAFC"
scooby_green = "#CAFF4D"
scooby_orange = "#FF4400"
scooby_brown = "#6F440F"


# Monster Info for Vertices - Sort in Alphabetical Order

monster_info = monster_name_counts %>% 
  filter(monster_name %in% unique_names_top) %>%
  mutate(strength = case_when(
    n < 3 ~ "Low",
    n < 10 ~ "Medium",
    TRUE ~ "High"),
    strength = as.factor(strength)) %>%
  arrange(desc(monster_name))


# Create graph based on pairings

monster_graph = monster_pair_counts_top %>% 
  graph_from_data_frame(directed = FALSE, 
                        vertices = monster_info)


# Number of vertices/monsters in graph
number_of_monsters = gorder(monster_graph)


# Calculate angle and hjust for each text label
angle = 360 * (seq(1, number_of_monsters) - 0.5) / number_of_monsters
angle_adj = ifelse(angle > 90 & angle<270, angle+180, angle)
hjust = ifelse(angle > 90 & angle<270, 1, 0)


# Create chord graph to show monster pairings and number of appearances
chord_graph = ggraph(monster_graph, layout = "circle") +
  
  # Green circle on inside of graph
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), fill = scooby_green, color = "black", size = 1) +
  
  # Brown connections between monster pairs, more appearances result in thicker line
  geom_edge_link(aes(edge_width = log(n)), 
                 edge_colour=scooby_brown, 
                 edge_alpha = 1, 
                 show.legend = FALSE) +  
  
  # Orange circle/node for each monster
  geom_node_point(size = 8, fill = scooby_orange, colour = "black", pch = 21, 
                  show.legend = FALSE) +
  

  # Number of appearances for a given monster
  geom_node_text(aes(label=glue("{n}")), 
                 size=4, 
                 color="black", 
                 show.legend = FALSE,
                 family = "Segoe UI") +
  
  # Name of monster around outside of graph  
  geom_node_text(aes(label=glue("     {name}     "), 
                     angle = angle_adj, 
                     hjust = hjust), 
                 size=5, 
                 color="black", 
                 show.legend = FALSE,
                 family = "Rockwell") +
  
  # Connections/edges should fall within this range scaled by number of appearances
  scale_edge_width(range = c(1, 3)) +
  
  theme_void() +
  
  # Expand limits to add padding for node labels
  expand_limits(x = c(-1.5, 1.5), y = c(-1.5, 1.5)) +
  
  labs(title = "The Monsters of Scooby Doo",
       subtitle = "With more than 600 mysteries in the history of the franchise, the gang has frequently encountered the same monster on multiple occasions. The following graph shows the Top 50 monsters by number of appearances (indicated in the orange nodes) and whether they've appeared alongside other monsters (indicated by brown edges). Thicker edges in the graph represent more appearances for a given pair of monsters.",
       caption = "<strong>Source:</strong> ScoobyPedia | <strong>Visualization:</strong> @a_lipphardt") +
  
  theme(plot.background = element_rect(fill = scooby_blue, color = scooby_blue),
        
        plot.title = element_textbox_simple(family = "Cooper Black",
                                            halign = 0.5,
                                            size = 20,
                                            margin = margin(t=10,b=10)),
        
        plot.subtitle = element_textbox_simple(family = "Segoe UI",
                                            halign = 0.5,
                                            size = 16,
                                            margin = margin(b=20, 
                                                            l = 20, 
                                                            r = 20)),
        
        plot.caption = element_textbox_simple(family = "Segoe UI Semilight",
                                              size = 14,
                                              halign = 1,
                                              margin = margin(r = 10, t = 20, b = 5)))
```

# Saving Image(s)

Save your image for sharing. Be sure to use the `#TidyTuesday` hashtag
in your post on twitter\!

``` r
ggsave(plot = chord_graph,
       filename = "scooby-doo.png",
       width=12,
       height=12,
       type = "cairo",
       device = "png",
       dpi = 300)
```

![The Monsters of Scooby Doo. With more than 600 mysteries in the
history of the franchise, the gang has frequently encountered the same
monster on multiple occasions. The following graph shows the Top 50
monsters by number of appearances (indicated in the orange nodes) and
whether they’ve appeared alongside other monsters (indicated by brown
edges). Thicker edges in the graph represent more appearances for a
given pair of monsters.](scooby-doo.png)
