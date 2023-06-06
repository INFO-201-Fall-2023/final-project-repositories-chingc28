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
                     h1("About Us"),
                     p("text goes here"))
                      

####
ui <- navbarPage("US Storms vs. Economy Over the Years",
                 state_level,
                 regional_level,
                 national_level,
                 about_me)

server <- function(input, output) {
  
  output$state_storm_text <- renderText({
  
  selected_state <- input$state
  filtered_df <- filter(df, LineCode == 1, GeoName == selected_state, incident_type != 'Biological', incident_type != "Dam/Levee Break", !is.na(incident_type))
  storm_events <- nrow(filtered_df)
  freq_table <- table(df[df$GeoName == selected_state & df$incident_type != c("Biological", "Dam/Levee Break") & !is.na(df$incident_type), "incident_type"])
  max_storm_type <- names(freq_table)[which.max(freq_table)]
  storm_list <- unique(filtered_df[,"incident_type"])
  max_storm_num <- nrow(df[df$GeoName == selected_state & df$incident_type == max_storm_type & df$LineCode == 1, ])
  
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
      theme(plot.margin = margin(50, 100, 50, 50)) +
      theme(aspect.ratio = ) +
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

}

shinyApp(ui = ui, server = server)

