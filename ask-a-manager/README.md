Ask a Manager Survey
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

Download the weekly data and make available in the `survey` object.

[Ask a Manager Survey](https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-11/readme.md)

``` r
# Loading Data for the First Time
# tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
# survey <- tuesdata$survey
# write_csv(survey, 'survey.csv')

survey <- read_csv('survey.csv')
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   timestamp = col_character(),
    ##   how_old_are_you = col_character(),
    ##   industry = col_character(),
    ##   job_title = col_character(),
    ##   additional_context_on_job_title = col_character(),
    ##   annual_salary = col_double(),
    ##   other_monetary_comp = col_character(),
    ##   currency = col_character(),
    ##   currency_other = col_character(),
    ##   additional_context_on_income = col_character(),
    ##   country = col_character(),
    ##   state = col_character(),
    ##   city = col_character(),
    ##   overall_years_of_professional_experience = col_character(),
    ##   years_of_experience_in_field = col_character(),
    ##   highest_level_of_education_completed = col_character(),
    ##   gender = col_character(),
    ##   race = col_character()
    ## )

# Sample Data

Take an initial look at the format of the data available.

``` r
head(glimpse(survey), 10)
```

    ## Rows: 26,232
    ## Columns: 18
    ## $ timestamp                                <chr> "4/27/2021 11:02:10", "4/2...
    ## $ how_old_are_you                          <chr> "25-34", "25-34", "25-34",...
    ## $ industry                                 <chr> "Education (Higher Educati...
    ## $ job_title                                <chr> "Research and Instruction ...
    ## $ additional_context_on_job_title          <chr> NA, NA, NA, NA, NA, NA, NA...
    ## $ annual_salary                            <dbl> 55000, 54600, 34000, 62000...
    ## $ other_monetary_comp                      <chr> "0", "4000", NA, "3000", "...
    ## $ currency                                 <chr> "USD", "GBP", "USD", "USD"...
    ## $ currency_other                           <chr> NA, NA, NA, NA, NA, NA, NA...
    ## $ additional_context_on_income             <chr> NA, NA, NA, NA, NA, NA, NA...
    ## $ country                                  <chr> "United States", "United K...
    ## $ state                                    <chr> "Massachusetts", NA, "Tenn...
    ## $ city                                     <chr> "Boston", "Cambridge", "Ch...
    ## $ overall_years_of_professional_experience <chr> "5-7 years", "8 - 10 years...
    ## $ years_of_experience_in_field             <chr> "5-7 years", "5-7 years", ...
    ## $ highest_level_of_education_completed     <chr> "Master's degree", "Colleg...
    ## $ gender                                   <chr> "Woman", "Non-binary", "Wo...
    ## $ race                                     <chr> "White", "White", "White",...

    ## # A tibble: 10 x 18
    ##    timestamp how_old_are_you industry job_title additional_cont~ annual_salary
    ##    <chr>     <chr>           <chr>    <chr>     <chr>                    <dbl>
    ##  1 4/27/202~ 25-34           Educati~ Research~ <NA>                     55000
    ##  2 4/27/202~ 25-34           Computi~ Change &~ <NA>                     54600
    ##  3 4/27/202~ 25-34           Account~ Marketin~ <NA>                     34000
    ##  4 4/27/202~ 25-34           Nonprof~ Program ~ <NA>                     62000
    ##  5 4/27/202~ 25-34           Account~ Accounti~ <NA>                     60000
    ##  6 4/27/202~ 25-34           Educati~ Scholarl~ <NA>                     62000
    ##  7 4/27/202~ 25-34           Publish~ Publishi~ <NA>                     33000
    ##  8 4/27/202~ 25-34           Educati~ Librarian High school, FT          50000
    ##  9 4/27/202~ 45-54           Computi~ Systems ~ Data developer/~        112000
    ## 10 4/27/202~ 35-44           Account~ Senior A~ <NA>                     45000
    ## # ... with 12 more variables: other_monetary_comp <chr>, currency <chr>,
    ## #   currency_other <chr>, additional_context_on_income <chr>, country <chr>,
    ## #   state <chr>, city <chr>, overall_years_of_professional_experience <chr>,
    ## #   years_of_experience_in_field <chr>,
    ## #   highest_level_of_education_completed <chr>, gender <chr>, race <chr>

# Wrangling

Explore the data and process it into prepared dataframes for
visualization.

``` r
# Filter to records for self-identified data scientists
data_science = survey %>%
  filter(str_detect(job_title, regex('data scien', ignore_case = T)) | str_detect(additional_context_on_job_title, regex('data scien', ignore_case = T)))

# Percent breakdown for years experience in data science
ds_experience = data_science %>%
  count(years_of_experience_in_field) %>%
  ungroup() %>%
  mutate(percent = n/sum(n)*100,
         year_order = str_split(years_of_experience_in_field, regex('\\s|-')) %>% 
           map_chr(., 1) %>% 
           as.integer()) %>%
  arrange(year_order)

# Percent breakdown for level of education in data science
ds_education = data_science %>%
  group_by(highest_level_of_education_completed) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(percent = count/sum(count) * 100,
         ed_order = case_when(
           highest_level_of_education_completed == "College degree" ~ 2,
           highest_level_of_education_completed == "High School" ~ 1,
           highest_level_of_education_completed == "Master's degree" ~ 3,
           highest_level_of_education_completed == "PhD" ~ 4,
    
           TRUE ~ 5
  )) %>%
  arrange(ed_order)

# Percent breakdown by gender for all survey responses
all_sex = survey %>%
  filter(gender %in% c("Woman", "Man")) %>%
  count(gender) %>%
  ungroup() %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))

# Percent breakdown by gender for data scientist responses
ds_sex = data_science %>%
  filter(gender %in% c("Woman", "Man")) %>%
  count(gender) %>%
  ungroup() %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))

# Select Top 3 Industries Represented for Data Science
ds_industry = data_science %>%
  group_by(industry) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  slice_max(count, n=3) %>%
  pull(industry)
  
# Salary data among data scientists in top 3 represented industries
ds_top3 = data_science %>%
  filter(industry %in% ds_industry,
         currency == "USD",
         annual_salary > 1000) %>%
  mutate(year_order = str_split(years_of_experience_in_field, regex('\\s|-')) %>% 
           map_chr(., 1) %>% 
           as.integer())
```

# Visualization(s)

Using your processed dataset, create your unique visualization(s).

``` r
font.family = "Segoe UI Light"

# Pie charts for gender breakdown among all responses vs data scientist responses

plot_sex_all = all_sex %>%
  mutate(ypos = cumsum(percent)- 0.5*percent,
         gender = case_when(
           gender == "Man" ~ "Men",
           TRUE ~ "Women"
         )) %>%
  ggplot(aes(x="", y=percent, fill=gender)) + 
  geom_bar(stat="identity", width=1, show.legend = FALSE) +
  geom_text(aes(y = ypos, label = gender), color = "white", size=3) +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1") +
  labs(title = "All Respondents",
       fill = "Gender:") +
  theme(
    text = element_text(family = font.family),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

plot_sex_ds = ds_sex %>%
  mutate(ypos = cumsum(percent)- 0.5*percent,
         gender = case_when(
           gender == "Man" ~ "Men",
           TRUE ~ "Women"
         )) %>%  
  ggplot(aes(x="", y=percent, fill=gender)) + 
  geom_bar(stat="identity", width=1, show.legend = FALSE) +
  geom_text(aes(y = ypos, label = gender), color = "white", size=3) +  
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1") +  
  labs(title = "Data Scientists",
       fill = "") +
  theme(
    text = element_text(family = font.family),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# Bar Chart for Highest Level of Education

plot_education = ds_education %>%
  mutate(highest_level_of_education_completed = fct_reorder(highest_level_of_education_completed, ed_order)) %>%
  ggplot(aes(x=highest_level_of_education_completed, y=percent)) +
  geom_col(fill = "#26418f") +
  labs(title = "Highest Level of Education Completed",
       y = "",
       x = "") +
  scale_y_continuous(labels = function(x) paste0(x, '%')) +  
  theme(
    text = element_text(family = font.family),
    plot.title = element_text(size = 12, face = "bold")
  )

# Horizontal Bar Chart for Years of Experience in Field

plot_experience = ds_experience %>% 
  mutate(years_of_experience_in_field = fct_reorder(years_of_experience_in_field, year_order)) %>%
  ggplot(aes(x=years_of_experience_in_field, y=percent, fill = log(year_order))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Years of Experience in Field",
       y = "",
       x = "") + 
  scale_y_continuous(labels = function(x) paste0(x, '%'), limits = c(0,40)) +
  scale_fill_viridis() +
  theme(
    text = element_text(family = font.family),
    plot.title = element_text(size = 12, face = "bold")
  ) +
  coord_flip()

# Swarm plot for annual salary, colored by years of experience

plot_salary = ds_top3 %>%
  ggplot(aes(x=industry, y=annual_salary, color = log(year_order))) +
  geom_beeswarm(size = 3, show.legend = FALSE, priority = "density") + 
  labs(title = "Annual Salary of Respondents for Top 3 Industries",
       y = "",
       x = "") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_color_viridis() +
  theme(
    text = element_text(family = font.family),
    plot.title = element_text(size = 12, face = "bold")
  )

# Using patchwork for grid layout

layout = "
#AAABBB#
#AAABBB#
#AAABBB#
CCCCCCCC
CCCCCCCC
CCCCCCCC
DDDDEEEE
DDDDEEEE
DDDDEEEE
"

final_plot = wrap_plots(
  A = plot_sex_all,
  B = plot_sex_ds,
  C = plot_education,
  D = plot_experience,
  E = plot_salary, 
  design = layout) + 
  plot_annotation(title = "Data Scientist Responses",
                  subtitle = "Of more than <strong>24,000</strong> survey respondents, <strong>161</strong> self-identified as a <strong>Data Scientist</strong>",
                  caption = "Source: Ask a Manager Survey, 2021",
                  theme = theme(text = element_text(family = font.family),
                                plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
                                plot.subtitle = element_markdown(size = 16, hjust = 0.5),
                                plot.caption = element_text(size = 12)))
```

# Saving Image(s)

Save your image for sharing. Be sure to use the `#TidyTuesday` hashtag
in your post on twitter\!

``` r
#This will save your most recent plot
ggsave(plot = final_plot,
  filename = "data-science-responses.png",
  width=16,
  height=9,
  device = "png")
```

![‘Survey responses for those that self identified as a Data
Scientist’](data-science-responses.png)
