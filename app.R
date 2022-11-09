
#libraries
library(shiny)
library(dplyr)
library(stringr)
library(highcharter)

tuesdata <- tidytuesdayR::tt_load('2022-03-29')

#data treatment
sports <- tuesdata$sports |> 
  filter(state_cd == "ID", year == 2019) |> 
  select(year, institution_name, city_txt, starts_with("sum"),
         starts_with("rev"), starts_with("exp"), sports) |> 
  filter(!(sum_partic_men ==0 & sum_partic_women ==0  )) |> 
  #reshape the dataset to an adequate plotting format
  tidyr::pivot_longer(cols = ends_with("men")) |> 
  #add variables to make it tidier
  mutate(sex = case_when(stringr::str_detect(name, "women") ~ "Women",
                         TRUE ~ "Men"),
         meaning_value = case_when(stringr::str_detect(name, "partic") ~ "n_participants",
                                   stringr::str_detect(name, "rev")  ~ "revenue",
                                   stringr::str_detect(name, "exp") ~ "expenses")) |>
  tidyr::pivot_wider(id_cols = c(institution_name,city_txt,sports,year,sex),
                     names_from = meaning_value,
                     values_from = value) |> 
  #mutating per capita
  mutate(revenue_per_capita = revenue/n_participants,
         expense_per_capita = expenses/n_participants)


agg_sports <-tuesdata$sports |>
  mutate(year = as.character(year)) |> 
  filter(state_cd == "ID") |>
  select(year, institution_name, city_txt, starts_with("sum"),
         starts_with("rev"), starts_with("exp"), sports) |> 
  filter(!(sum_partic_men ==0 & sum_partic_women ==0  ))

sports2 <- agg_sports|> 
  #mutating per capita
  mutate(expense_per_capita_man = exp_men/sum_partic_men,
         expense_per_capita_woman = exp_women/sum_partic_women) |> 
  group_by(year,sports) |> 
  summarise(median_male_pcap = median(expense_per_capita_man, na.rm =TRUE),
            median_female_pcap = median(expense_per_capita_woman, na.rm = TRUE)) |> 
  ungroup() |> 
  tidyr::pivot_longer(c("median_female_pcap", "median_male_pcap")) |> 
  mutate(name = case_when(stringr::str_detect(name,"female") ~ "Women",
                          TRUE ~ "Men")) |> 
  rename(investment_per_capita = value, gender = name)


sports3 <- agg_sports|> 
  group_by(year,sports) |> 
  summarise(total_female_part = sum(sum_partic_women, na.rm = TRUE),
            total_male_part = sum(sum_partic_men, na.rm = TRUE)) |> 
  ungroup() |> 
  tidyr::pivot_longer(c("total_female_part", "total_male_part")) |> 
  mutate(name = case_when(stringr::str_detect(name,"female") ~ "Women",
                          TRUE ~ "Men")) |> 
  rename(total_enroll = value, gender = name)
  
  
  




#choice vectors
city_options <- sports |> distinct(city_txt) |> pull(city_txt) #city

sports2_options <- sports2 |> distinct(sports) |> pull(sports)


# Define UI for application that draws a histogram
ui <- fluidPage(
    shiny::navbarPage("Idaho's gender equality in college sports observatory",
    #Panel 1
    tabPanel("Investment and revenue comparision",
             h3("Gender equality in college sports for Idaho"),
             h4("In this webapp, you can dive into different colleges' sports leagues in the state of
                Idaho, and check how equal men's and women's sports are. In this first panel,
                we compare total participation, expenses and revenue per capita."),
             h4("Choose a city, an institution and a sport to get information about it"),
      fluidRow(
        # titlePanel("Selecting inputs"),
        #First column for selecting a city
        column(4,
               selectInput(inputId = "city",
                           label = "Choose a city",
                           choices = city_options,
                           selected = "Boise")),
        
        #Second column for selecting an uni
        column(4,
               uiOutput("secondSelection")),
        
        column(4,
               uiOutput("thirdSelection"))
      ),
      
      #Another row with the results
      fluidRow(
        # titlePanel("Displaying results"),
        column(4,
               h4("Total participation comparision"),
               highcharter::highchartOutput("participants_plot")),
        
        column(4,
               h4("Expenses per capita"),
               highcharter::highchartOutput("expenses_plot")),
        
        column(4,
               h4("Revenue per capita"),
               highcharter::highchartOutput("revenue_plot"))
        
      ),
      fluidRow(
        column(12,
               h4("You can download the data filtered by the inputs here"),
               downloadButton("download", "Download .csv")
               )
        
      )
      
    ),
    #Panel 2
    tabPanel("Expenses for all sports through the years",
             fluidRow(
               column(12,
                      h3("Closing the gap: observing the sport in recent years"),
                      h4("In this panel, you can compare how the median expenses per athlete and total athlete enrolment evolved throughout the years. 
                   Choose a sports' league to get time series on it."),
                   selectInput('sport2',"Choose a sport", choices = sports2_options,
                               selected = "Basketball"))
             ),
             fluidRow(column(5,
                             h4("Median expense per athlete through time"),
                             highcharter::highchartOutput("thru_time")),
                      column(5,
                             h4("Total participation through time"),
                             highcharter::highchartOutput("thru_time2"))
                             
             ),
             fluidRow(
               column(12,
                      h4("You can download the data filtered by the inputs here"),
                      downloadButton("download2", "Download .csv"))
             )
               
             ),
    tabPanel("Data dictionary",
             fluidRow(
               column(12,
                      h3("In this panel, we have a clearer explanation of the terms we use"),
                      h3("Total enrolment"),
                      h4("Students who, as of the day of a varsity team's first scheduled contest (A) Are listed by the institution on the varsity team's roster; (B) Receive athletically related student aid; 
                         or (C) Practice with the varsity team and receive coaching from one or more varsity coaches. A student who satisfies one or more of these criteria is a participant, including a student 
                         on a team the institution designates or defines as junior varsity, freshman, or novice, or a student withheld from competition to preserve eligibility (i.e., a redshirt), or for academic, 
                         medical, or other reasons. This includes fifth-year team members who have already received a bachelor's degree."),
                      
                      h3("Expenses"),
                      h4("All expenses attributable to intercollegiate athletic activities. This includes appearance guarantees and options, athletically related student aid, contract services, equipment, 
                         fundraising activities, operating expenses, promotional activities, recruiting expenses, salaries 
                         and benefits, supplies, travel, and any other expenses attributable to intercollegiate athletic 
                         activities."),
                      
                      h3("Revenue"),
                      h4("All revenues attributable to intercollegiate athletic activities. This includes revenues from appearance guarantees and options, contributions from alumni and others, 
                         institutional royalties, signage and other sponsorships, sport camps, state or other government support, student activity fees, ticket and luxury box sales, and any other
                         revenues attributable to intercollegiate athletic activities. "),
                      h4("All the data comes from the US Department of Education,
                  and more specifically, from the EADA, Equity in Athletics Data Analysis. "))
             ))
             
             
             
             # sidebarLayout(
             #   # h4("Here you can compare how the expenses per athlete evolved throughout the years.
             #   #    Choose a sports' league to get a time series on it"),
             #   sidebarPanel(
             #     selectInput('sport2',"Choose a sport", choices = sports2_options,
             #                 selected = "Basketball")
             #   ),
             #   mainPanel(
             #     highcharter::highchartOutput("thru_time")
             #   )
             # ))
  )

  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #UI Outputs ===================================
  uni_options <- reactive({
    sports |> filter(city_txt == input$city) |> pull(institution_name) })  
  
  output$secondSelection <- renderUI({
    selectInput('uni', 'Choose an university',  
                choices = uni_options())
    
  })
  
  sports_options <- reactive({
    # req(input$uni, input$city)
    sports |> filter(city_txt == input$city, institution_name == input$uni) |> pull(sports)
  })
  
  output$thirdSelection <- renderUI({
    selectInput('sport', 'Finally, choose a sports league',  
                choices = sports_options())
    
  })
  
  
  
  
  #Filtering the dataset ===================================
  df <- reactive({
    req(input$sport2)
   sports2 |> filter(sports == input$sport2)
  })
  
  df3 <- reactive({
    req(input$sport2)
    sports3 |> filter(sports == input$sport2)
  })
  
  #for each sport
  df_sport_specific <- reactive({
    req(input$city, input$uni, input$sport)
    sports |> filter(city_txt == input$city) |> 
      filter(institution_name == input$uni) |> 
      filter(sports == input$sport)
  })

  #Plot outputs ===================================== 
  
  #first bar plot
  output$participants_plot <- renderHighchart({
    req(input$city, input$uni, input$sport)
    hchart(df_sport_specific()
           , "column", hcaes(x = sex, y=n_participants, color = c("#ADD8E6", "#2c3e50"))) |> 
      hc_xAxis(title= list(text = "Gender")) |> 
      hc_yAxis(title = list(text = "Number of athletes enrolled"))
  })
  
  #second bar plot
  output$expenses_plot <- renderHighchart({
    req(input$city, input$uni, input$sport)
    hchart(df_sport_specific()
           , "column", hcaes(x = sex, y=expense_per_capita, color = c("#ADD8E6", "#2c3e50"))) |> 
      hc_xAxis(title= list(text = "Gender")) |> 
      hc_yAxis(title = list(text = "Expense per athlete"))
  })
  
  #third bar plot
  output$revenue_plot <- renderHighchart({
    req(input$city, input$uni, input$sport)
    hchart(df_sport_specific()
           , "column", hcaes(x = sex, y=revenue_per_capita, color = c("#ADD8E6", "#2c3e50"))) |> 
      hc_xAxis(title= list(text = "Gender")) |> 
      hc_yAxis(title = list(text = "Revenue per athlete"))
  })
  
  #different pannel plot
  output$thru_time <- renderHighchart({
    req(input$sport2)
    hchart(df(), "line", hcaes(x = year, y=investment_per_capita, group = gender))|> 
      hc_xAxis(title= list(text = "Year")) |> 
      hc_yAxis(title = list(text = "Expense per athlete"))
  })
  
  output$thru_time2 <- renderHighchart({
    req(input$sport2)
    hchart(df3(), "line", hcaes(x = year, y=total_enroll, group = gender))|> 
      hc_xAxis(title= list(text = "Year")) |> 
      hc_yAxis(title = list(text = "Total number of athletes"))
  })
  

  
  #====================================
  #downloading
  output$download <- downloadHandler(
      filename = function() {
        paste0(input$city,"_", input$uni,"_", input$sport, ".csv")
      },
      content = function(file) {
        write.csv(df_sport_specific(), file, row.names = FALSE)
      }
    )
  
  output$download2 <- downloadHandler(
    filename = function() {
      paste0("through_time_",input$sport2, ".csv")
    },
    content = function(file) {
      write.csv(df() |> full_join(df3(), by =c('year','sports','gender')), file, row.names = FALSE)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)
