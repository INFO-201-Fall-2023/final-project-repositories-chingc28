# loads library

library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)
library(tidyr)

theme_set(theme_minimal())
##### Loading the data 
main_df <- read.csv("sum_df.csv")

# gets rid of the X in the year columns 
colnames(main_df) <- gsub("^X", "", colnames(main_df))

### additional summary table needed 

region_sum <- main_df %>%
  mutate(Sum = rowSums(.[2:28])) %>%
  select(State, Sum, region_name)

region_sum <- as.data.frame(region_sum)

####### Code for scrollytelling 

intro <- tabPanel("Introduction",
                  imageOutput("title_img"),
                  br(),
                  h2("Understanding Climate Change: Weather, Impacts, and the Economy"),
                  br(),
                  p("Climate change is a pressing global issue that has far-reaching consequences
                    for our planet. The gradual rise in global surface temperatures has already
                    begun to impact weather patterns, resulting in a higher frequency of droughts
                    and more intense storms. Impacts of climate change are numerous, ranging from ocean acidification, weather, lung disease, to
                    crops yield. This interactive website focuses on weather patterns and 
                    aims to help visualize effects of climate
                    change on weather, carbon output, and its potential impact on our state, region,
                    and nationwide economy."),
                  br(),
                  h2("How Are Climate Change and Weather Related?"),
                  p("One of the key ways climate change influences weather events is through
                    the increased availability of water vapor in the atmosphere. As temperatures
                    rise, more water is evaporated into the air, creating a potent fuel source for
                    the formation of powerful storms. Warmer ocean surface temperatures and
                    heightened atmospheric heat contribute to the intensification of wind speeds
                    in tropical storms, leading to more destructive hurricanes and cyclones.
                    Furthermore, rising sea levels expose coastal regions to greater risks,
                    causing unprecedented erosion and posing a threat to vulnerable ecosystems."),
                  br(),
                  h4("Difference Between Weather and Climate"),
                  p("It is crucial to distinguish between weather and climate. Weather refers to the day-to-day atmospheric conditions,
                    such as temperature, precipitation, and wind patterns. On the other hand, climate
                    encompasses the long-term average of weather patterns in a specific region over a
                    considerable period. Climate change refers to the sustained alterations in climate
                    patterns over time, often resulting from human activities and natural factors."),
                  br(),
                  p("For example, 95% chance of rain on Wednesday evening over the city of Seattle refers to weather.
                    The city of Seattle receiving average 10 inches of rain in the month of October for the past 10 years
                    refers to climate."),
                  br(),
                  h2("Climate Change Indicators"),
                  p("To better understand and monitor climate change, the Environmental Protection Agency
                    (EPA) has developed over 50 climate change indicators that track changes over time. These
                    indicators cover various aspects, including the shrinking of glaciers, greenhouse gas
                    emissions, wildfires, and fluctuations in lake temperatures, among others. These indicators
                    provide valuable insights into the ongoing shifts in our climate and help inform policies
                    and actions to mitigate its impacts. More can be found on the", a("EPA site", href = "https://www.epa.gov/climate-indicators/view-indicators", ".")),
                  br(),
                  imageOutput("carbon_img"),
                  h2("Role of Carbon Dioxide (CO2) in Emissions"),
                  p("Human activities play a significant role in climate change, primarily through the
                    emission of greenhouse gases. Carbon dioxide (CO2) is the primary greenhouse gas
                    released as a result of human activities. In 2021, CO2 accounted for 79% of all U.S.
                    greenhouse gas emissions attributed to human activities. While carbon dioxide occurs
                    naturally in the atmosphere as part of the Earth's carbon cycle, human actions have
                    disrupted this balance. Industrial processes and the burning of fossil fuels have led to a
                    significant increase in CO2 levels, affecting the planet's ability to regulate its climate."),
                  h2("Climate Change Impact on the Economy"),
                  p("Rising temperatures and the effects of climate change could have significant economic
                  implications globally.According to a report made by the White House in 2023, the cost of climate
                  and weather disasters in the United States last year (2022) totaled more than $165 billion—the third most
                  costly year on record.  Swiss Re, a major insurance company, warned that by 2050, crop yield reductions,
                    the spread of diseases, and rising sea levels could lead to a significant decrease in global wealth.
                    The company projected that climate change could reduce global economic output by 11% to 14% compared to
                    growth levels without climate change, which amounts to as much as $23 trillion annually.
                    If the goal of holding global temperature increase to less than 2 degree Celsius above preindustrial levels,
                    economic losses by 2050 would be marginal. However,", tags$b("current emissions levels are far from the target 2 degrees.
                    In fact, it is projected that global temperatures are likely to increase as much as 2.6 degrees based on current
                    calculations"), ". If that were to occur, the economy of the United States would be as much as 7% smaller than what it
                    could be without climate change impact. For reference 7% of the U.S. 2022 GDP is $1.78 trillion. It is crucial
                    to mitigate climate change as much as possible to minimize effects on economic output, which would then
                    cascade into people's livelihoods and quality of living"), 
                  br(),
                  tags$strong("Sources"),
                  p("New York Times:", a("Climate Change Could Cut World Economy by $23 Trillion in 2050, Insurance Giant Warns",
                                         href = "https://www.nytimes.com/2021/04/22/climate/climate-change-economy.html")),
                  p("The White House:", a("The Importance of Measuring the Fiscal and Economic Costs of Climate Change",
                                          href = "https://www.whitehouse.gov/omb/briefing-room/2023/03/14/the-importance-of-measuring-the-fiscal-and-economic-costs-of-climate-change/")),
                  p("USGS, US Dept of Interior: ", a("How can climate change affect natural disasters?",
                                                     href = "https://www.usgs.gov/faqs/how-can-climate-change-affect-natural-disasters")),
                  p("EPA:", a("Overview of Greenhouse Gases", href = "https://www.epa.gov/ghgemissions/overview-greenhouse-gases")),
                  br()
                  )

state_level <- tabPanel("State Level",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              inputId = "state",
                              label = "Select State",
                              choices = unique(main_df$State),
                              selected = "Washington"
                            ),
                            h3("Overview"),
                            br(), 
                            h4("Storms"),
                            textOutput("state_storm_text"
                            ),
                            h4("GDP"),
                            textOutput("gdp_state_text"),
                            h4("Carbon"),
                            textOutput("carbon_state_text")
                          ),
                          mainPanel(
                            plotlyOutput(
                              outputId = "line"
                            ),
                            plotlyOutput(
                              outputId = "shaded_area"
                            ),
                            plotlyOutput(
                              outputId = "carbon_scatter_state"
                            )
                          )
                        ))

regional_level <- tabPanel("Regional Level",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput(
                                 inputId = "regions",
                                 label = "Select Region",
                                 choices = unique(main_df$region_name),
                                 selected = "West"
                               )
                             ),
                             mainPanel(
                               plotlyOutput(
                                 outputId = "scatter"
                               ),
                               plotlyOutput(
                                 outputId = "bar"
                               ),
                               plotlyOutput(
                                 outputId = "area_reg"
                               ),
                               plotlyOutput(
                                 outputId = "line_carb"
                               )
                             )
                           ))

national_level <- tabPanel("National Level",
                           mainPanel(
                             plotlyOutput(
                               outputId = "line_g"
                             ),
                             plotlyOutput(
                               outputId = "line_carbon"
                             ),
                             plotlyOutput(
                               outputId = "line_gdp"
                             )
                           ))

about_me <- tabPanel("About",
                     h1("About Me"),
                     h4("This project was inspired by ATMS111- Global Warming: Understanding the Issues, a
                        class taught at the University of Washington- Seattle. The course presented a broad overview
                        of impact of climate change on a global, regional, and local scale. As well as the impact
                        on social, societal, economic, and health impact. It was a wonderful course that taught me
                        different ways to think about climate change. The most crucial part of mitigating climate
                        change is lowering greenhouse gas emission, especially CO2 released by humans."),
                     h4("I hope that after analyzing storm events, weather output, and GDP on a state, regional, and national level,
                        people can have a better idea of where we as a country currently are when it comes to our impact on
                        CO2 output."),
                     br(), 
                     h4("A question that often arises when it comes to reducing GHG or climate change is clean energy.
                        Although this app doesn't include information about energy use, I have previously created a visualization
                        regarding clean energy sources (e.g. hydroelectric) vs. GHG-releasing energy sources (e.g. coal) and 
                        an analysis of energy output by state vs. nationwide. The visualizer can be found here on the",
                        a("Tableau", href = "https://public.tableau.com/app/profile/ching.chiu5208/viz/US_Energy/Story1"), "webpage
                        It is interactive as well, feel free to click through to satisfy your curiosities"),
                     h5("Here is the GitHub Repo:", a("Link- Ching Chiu", href = "https://github.com/INFO-201-Fall-2023/final-project-repositories-chingc28")),
                     br(),
                     h4("In the introduction, it mentioned that the Paris Climate Agreement urges nations to help
                        keep rising global temperature by maximum 2 degrees by the year 2050. It is projected to rise
                        by 2.6 degrees by most experts. However, it is hard to comprehend how even a 0.1 degree Celsius
                        difference can change the world. This excellent video by", a("The Economist", href = "https://www.youtube.com/watch?v=uynhvHZUOOo&ab_channel=TheEconomist"),
                        "talks about a world under a 3 degree global warming."),
                     br(),
                     h4("Here is another great article that talks about the difference between 1.5, 2, and 3 degree global warming. The
                        article is by", a("Global Citizen", href = "https://www.globalcitizen.org/en/content/the-difference-in-global-warming-levels-explained/")),
                     br(),
                     imageOutput("EPA_img"),
                     tags$em("This graph shows 2100 global temperature rise projection under different degree warming scenarios. Best case scenario,
                             at RCP2.5 warming, global temperature would only rise by 0.5C. At worst case scenario, RCP 8.5, also known as 'business as
                             usual' scenario where no climate change policy are adopted, the global temperature rise would be from 3 to 5 degrees
                             Celsius. Source(", a("EPA, Future of Climate Change", href = "https://climatechange.chicago.gov/climate-change-science/future-climate-change"), ")"),
                     h4("To learn further more about climate change projections, impact, and future outlooks, visit the IPCC webpage. IPCC
                        is a division of the UN made for assessing science related to climate change. They release a report every year, since 2016, where they
                        assess scenarios under different warming conditions and address the likelihoods of these scenarios. The yearly reports are dense and long,
                        however there are summaries that are easy to understand."),
                     h4("Here is the", a("IPCC report for the year of 2022", href = "https://www.ipcc.ch/report/ar6/wg2/")),
                     br(),
)



####
ui <- navbarPage("US Storms vs. Economy Over the Years",
                 intro,
                 state_level,
                 regional_level,
                 national_level,
                 about_me)

server <- function(input, output) {
  
  output$state_storm_text <- renderText({
  
  selected_state <- input$state
  filtered_df <- filter(df, LineCode == 1, GeoName == selected_state, incident_type != 'Biological', incident_type != "Dam/Levee Break", incident_type != "Terrorist", !is.na(incident_type))
  storm_events <- nrow(filtered_df)
  freq_table <- table(filtered_df$incident_type)
  max_storm_type <- names(freq_table)[which.max(freq_table)]
  storm_list <- unique(filtered_df[,"incident_type"])
  max_storm_num <- nrow(filter(df, GeoName == selected_state, incident_type == max_storm_type, LineCode == 1))
  
    # nrow(df[df$GeoName == selected_state & df$incident_type == max_storm_type & df$LineCode == 1, ])
  
  # Generate the text
  text <- paste("From 1997 to 2022, ", selected_state, " has experienced ", storm_events,
                " total storm events. This included ",
                paste(storm_list, collapse = ", "),
                ". The most common storm event that occurred was ", max_storm_type, "at", max_storm_num, ' instances over the last 25 years.')
  
  return(text)
  
  })
  
  output$gdp_state_text <- renderText({
    
    selected_state <- input$state
    gdp_start <- unique(df[df$GeoName == input$state & df$LineCode == 1 & !is.na(df$`1997`), '1997'])
    gdp_end <- unique(df[df$GeoName == input$state & df$LineCode == 1 & !is.na(df$`2022`), '2022'])
    gdp_change <- unique(df[df$GeoName == input$state & df$LineCode == 1 & !is.na(df$gdp_change), 'gdp_change'])
    
    text <- paste("In 1997, the state's GDP started at", gdp_start, "million dollars and reached", gdp_end, " million dollars in 2022. This is an increase of",
    gdp_change, "million dollars within 25 years.")
    
  })
  
  output$carbon_state_text <- renderText({
    
    selected_state <- input$state
    carbon_df <- filter(main_df, State == input$state, category == "carbon")
    max_value <- apply(carbon_df[2:25], 1, max)
    max_col <- apply(carbon_df[2:25], 1, function(row) colnames(carbon_df)[which.max(carbon_df[2:25])])
    min_values <- apply(carbon_df[2:25], 1, min)
    min_col <- apply(carbon_df[2:25], 1, function(row) colnames(carbon_df)[which.min(carbon_df[2:25])])
    carbon_2020 <- carbon_df[carbon_df$X2020 == 2020,]
    
    
    text <- paste("The trendline in", selected_state, "state's carbon emissions reveals insightful
    patterns. The peak year for carbon output occurred in", max_col,"reaching a significant",
    max_value, "megatons, while the lowest point was observed in", min_col, "with", 
    min_values, "megatons. Over the years, carbon emissions have undoubtedly decreased compared
    to 25 years ago, potentially attributed to the transition towards cleaner and renewable
    energy sources in recent times.")
    
    return(text)
    
  })
  
  output$line <- renderPlotly({
    
    # filtering the data
    state_df <- filter(main_df, State == input$state & main_df$category == 'storm')
    state_df <- state_df[, -c(29:30)]
    
    # Reshape the data to be longer 
    state_reshape_df <- pivot_longer(
      data = state_df,
      cols = -State,
      names_to = "Year",
      values_to = "Storm")
    
    # change 'Year' to numeric values so the graph can load
    state_reshape_df$Year <- gsub("^X", "", state_reshape_df$Year)
    state_reshape_df$Year <- as.numeric(state_reshape_df$Year)

    # plotting the line graph
    line <- ggplot(state_reshape_df, aes(x = Year, y = Storm)) + 
      geom_line(col = 'lightblue', fill = 'lightblue') +
      labs(
        title = paste(input$state,"'s Storm Events Over Time"),
        x = "Years",
        y = "Total Storm Events")
    
  })
  
  output$shaded_area <- renderPlotly({
    # filtering the data
    state_df <- filter(main_df, State == input$state & main_df$category == 'gdp')
    state_df <- state_df[, -c(29:30)]
    
    # Reshape the data to be longer 
    state_reshape_df <- pivot_longer(
      data = state_df,
      cols = -State,
      names_to = "Year",
      values_to = "Storm")
    
    # change 'Year' to numeric values so the graph can load
    state_reshape_df$Year <- gsub("^X", "", state_reshape_df$Year)
    state_reshape_df$Year <- as.numeric(state_reshape_df$Year)
    
    # plotting the line graph
    shaded_area <- ggplot(state_reshape_df, aes(x = Year, y = Storm)) + 
      geom_area(fill = 'lightpink') +
      labs(
        title = paste(input$state,"'s GDP Over Time"),
        x = "Years",
        y = "GDP in 2012 US Dollar value (in Millions)")
  })
  
  output$scatter <- renderPlotly({
    
    # plotting the scatterplot 
  
    scatter <- ggplot(region_sum, aes(x = State, y = Sum)) + 
      geom_point(aes(size = Sum, col = region_name)) +
      coord_flip() +
      labs(
        title = "Regional Weather Total",
        x = 'State',
        y = "Total Weather Events"
      )
  })
  
  output$bar <- renderPlotly({
    
    region_fil <- filter(main_df, category == "storm" & region_name == input$regions)
   
    region_filt <- region_fil %>%
      mutate(Sum = rowSums(.[2:28])) %>%
      select(State, Sum)
    
    region_filt <- as.data.frame(region_filt)
    
    ### plot the graph
    bar <- ggplot(region_filt, aes(x = State, y = Sum)) + 
      geom_bar(stat = "identity", aes(fill = State)) +
      labs(
        title = paste(input$region, "Total Storms"),
        x = 'State',
        y = "Total Weather Events"
      )
  })
  output$line_g <- renderPlotly({
    
    nation_fil <- filter(main_df, category == "storm")
    
    # change from wide to long
    nation_long <- pivot_longer(nation_fil, cols = c(2:28), names_to = "Year", values_to = "Value")
    
    # Convert Year column to numeric
    nation_long$Year <- as.numeric(nation_long$Year)
    
    # Create a boxplot
    ggplot(nation_long, aes(x = Year, y = Value)) +
      geom_boxplot(col = 'orange2') + 
      labs(
        title = "Total Number of Storms Each Year", 
        x = "Year", 
        y = "Total Storms"
      )
  })
  
  output$line_carbon <- renderPlotly({
    
    nation_fil <- filter(main_df, category == "carbon")
    
    # change from wide to long
    carbon_long <- pivot_longer(nation_fil, cols = c(2:25), names_to = "Year", values_to = "Value")
    
    # Calculate the sum of values for each year
    sum_values <- carbon_long %>%
      group_by(Year) %>%
      summarise(Value = sum(Value))
    
    # put data as a dataframe and turns Year into numeric

    sum_values <- as.data.frame(sum_values)
    sum_values$Year <- as.numeric(sum_values$Year)
    
    # Create a lollipop chart
    ggplot(sum_values, aes(x = Year, y = Value)) +
      geom_segment(aes(x = Year, xend = Year, y = 0, yend = Value), color = "maroon3") +
      geom_point(col = 'orchid1', size = 4, alpha = 0.6) +
      theme_light() +
      coord_flip() +
      scale_x_reverse() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()
        ) +
      labs(
        title = "Total Carbon Output Per Year", 
        x = "Year", 
        y = "Total Carbon Output (in gigatons)"
      )
  })
  
  output$line_gdp <- renderPlotly({
    
    gdp_fil <- filter(main_df, category == "gdp")
    
    # Reshape the dataframe to long format
    gdp_long <- pivot_longer(gdp_fil, cols = c(2:28), names_to = "Year", values_to = "Value")
    
    # Calculate the sum of values for each year
    sum_values <- gdp_long %>%
      group_by(Year) %>%
      summarise(Sum = sum(Value))
    
    # put data as numeric
    
    sum_values$Year <- as.numeric(sum_values$Year)

    # Create an area plot
    ggplot(sum_values, aes(x = Year, y = Sum)) +
      geom_area(fill = 'lightblue') + 
      labs(
        title = "Total GDP per Year", 
        x = "Year", 
        y = "GDP in 2012 US Dollars (in Millions)"
      )
  })
    
  output$area_reg <- renderPlotly({

    gdp_fil <- filter(main_df, category == "gdp")
    
    # Reshape the dataframe to long format
    gdp_long <- pivot_longer(gdp_fil, cols = c(2:28), names_to = "Year", values_to = "Value")
    
    # Calculate the sum of values for each year
    sum_values <- gdp_long %>%
      group_by(Year, region_name) %>%
      summarise(Sum = sum(Value))
    
    # put data as numeric
    
    sum_values$Year <- as.numeric(sum_values$Year)
    
    # Create an area plot
    ggplot(sum_values, aes(x = Year, y = Sum, fill = region_name)) +
      geom_area() + 
      labs(
        title = "Total GDP per Year", 
        x = "Year", 
        y = "GDP in 2012 US Dollars (in Millions)"
      )
    })
  
  output$line_carb <- renderPlotly({
    
    nation_fil <- filter(main_df, category == "carbon", region_name == input$regions)
    
    # change from wide to long
    carbon_long <- pivot_longer(nation_fil, cols = c(2:25), names_to = "Year", values_to = "Value")
    
    # Calculate the sum of values for each year
    sum_values <- carbon_long %>%
      group_by(Year) %>%
      summarise(Value = sum(Value))
    
    # put data as a dataframe and turns Year into numeric
    
    sum_values <- as.data.frame(sum_values)
    sum_values$Year <- as.numeric(sum_values$Year)
    
    # plotting the line graph
    line <- ggplot(sum_values, aes(x = Year, y = Value)) + 
      geom_line(col = 'lightblue', fill = 'lightblue') +
      labs(
        title = paste("U.S", input$regions," Region's Carbon Output Over Time"),
        x = "Years",
        y = "Carbon Output (Megaton)")
  })
  
  output$carbon_scatter_state <- renderPlotly({
    
    state_df <- filter(main_df, State == input$state, category == "carbon")
    
    # change from wide to long
    state_long <- pivot_longer(state_df, cols = c(2:25), names_to = "Year", values_to = "Value")
    
    # Calculate the sum of values for each year
    sum_values <- state_long %>%
      group_by(Year) %>%
      summarise(Value = sum(Value))
    
    # put data as a dataframe and turns Year into numeric
    
    sum_values <- as.data.frame(sum_values)
    sum_values$Year <- as.numeric(sum_values$Year)
    
    scatter <- ggplot(sum_values, aes(x = Year, y = Value)) + 
      geom_point(aes(size = Value), col = "purple") +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Carbon Output Total",
        x = 'Year',
        y = "Carbon Output (Megatons)"
      ) +
      coord_cartesian(ylim = c(0, max(sum_values$Value)))
    
  })
  
  output$title_img <- renderImage({
    
    list(src = "www/Effects-tryptich.jpeg",
         width = "100%",
         height = "100%")
  }, deleteFile = F)
  
  output$carbon_img <- renderImage({
    
    list(src = "www/Greenhouse-effect.jpg",
         width = "40%",
         height = "100%")
  }, deleteFile = F)
  
  output$EPA_img <- renderImage({
    
    list(src = "www/chicago_img.jpg",
         width = "30%",
         height = "100%")
  }, deleteFile = F)

}

shinyApp(ui = ui, server = server)

