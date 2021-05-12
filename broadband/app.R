library("tidytuesdayR")
library("tidyverse")
library("glue")
library("ggtext")
library("ggalt")
library("ggthemes")

library("shiny")

broadband <- read_csv('broadband.csv')


ui <- fluidPage(
      titlePanel("County Disparities between Broadband Internet Availability and Usage"),
      p(tags$strong("Code on GitHub"), a("https://github.com/alipphardt/TidyTuesday/tree/main/broadband", href="https://github.com/alipphardt/TidyTuesday/tree/main/broadband"), align = "center"),
      sidebarPanel(selectInput(inputId = "selectedState",
                  choices = broadband$ST %>% unique(),
                  label = "Select State"),
                  p(tags$strong("Availability"), " is defined as the percent of people per county with access to fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps as of the end of 2017. "), 
                  p(tags$strong("Usage"), " is defined as the percent of people per county that use the internet at broadband speeds based on fixed terrestrial broadband at speeds of 25 Mbps/3 Mbps as of the end of 2017."),
                  p(tags$strong("NOTE: "), "Counties with missing availability or usage data are not shown.")
      ),
      mainPanel(
        uiOutput("statePlot")
      )
      

)

server <- function(input, output){
  
  plotHeight = reactive({
    req(input$selectedState)
    
    number_of_counties = length(broadband %>% 
                                  filter(ST == input$selectedState, 
                                         `BROADBAND AVAILABILITY PER FCC` != '-',
                                          `BROADBAND USAGE` != '-') %>% 
                                  pull(`COUNTY NAME`))
    
    if(number_of_counties > 20){
      return(2)
    }
    else{
      return(1)
    }
    
  })    
  
  output$plot <- renderPlot({
    
    state_broadband = broadband %>% 
      filter(ST == input$selectedState) %>%
      rename(COUNTY_ID = `COUNTY ID`,
             COUNTY_NAME = `COUNTY NAME`,
             AVAILABILITY = `BROADBAND AVAILABILITY PER FCC`,
             USAGE = `BROADBAND USAGE`) %>%
      filter(USAGE != '-', AVAILABILITY != '-') %>%
      mutate(COUNTY_NAME = str_remove(COUNTY_NAME, " County"),
             AVAILABILITY = as.numeric(AVAILABILITY) * 100,
             USAGE = as.numeric(USAGE) * 100,
             DISPARITY = AVAILABILITY - USAGE,
             COUNTY_NAME = fct_reorder(COUNTY_NAME, desc(DISPARITY)))
    
    color1 <- "#58AF4A"
    color2 <- "#0171CE"
    
    state_title = glue("<span style='color:{color1}; font-weight:bold;'>AVAILABILITY</span> vs. <span style='color:{color2}'>USAGE</span> of Broadband in {state_broadband[1,]$ST} Counties")
    
    highest = state_broadband %>% slice_max(DISPARITY, n=1)
    lowest = state_broadband %>% slice_min(DISPARITY, n=1)
    
    plot1 = state_broadband %>% ggplot() +
      
      geom_segment(aes(y = COUNTY_NAME, yend = COUNTY_NAME, x = 0, xend = 100),
                   color = "#CDCDCD",
                   size = 0.2) +
      
      geom_dumbbell(aes(y = COUNTY_NAME, x = AVAILABILITY, xend = USAGE),
                    size = 2, 
                    color = "#CDCDCD",
                    size_x = 3,
                    size_xend = 3,
                    colour_x = color1,
                    colour_xend = color2,
                    show.legend = TRUE) +
      
      labs(title = state_title,
           x = "Percent of People within County",
           y = "") +
      
      scale_x_continuous(breaks=seq(0,100,10), limits=c(0,100)) + 
      scale_y_discrete(limits=rev) +
      
      scale_color_fivethirtyeight() +
      theme_fivethirtyeight() +  
      
      theme(  
        axis.text.y = element_text(size = 12, face = "bold"),
        axis.title = element_text(size=12, face = "bold", color = "#444444"),
        plot.title = element_markdown(size = 18, hjust = 0.5)
      ) 
    
    return(plot1)
    
  })
  
  output$statePlot <- renderUI({
    reduce_by = 0.7
    plotOutput("plot", width = 1200 * reduce_by, height = 900 * reduce_by * plotHeight())
  })

    
    
} 


shinyApp(ui, server)