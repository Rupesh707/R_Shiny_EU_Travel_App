library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)        
library(plotly)
library(dplyr)
library(ggplot2)
library(grid)
library(scales)
library(leaflet)
library(sp)
library(rworldmap)
library(fresh)
library(stringr)
library(forcats)
library(tidytext)

load("hotel.RData")

###  DATAFRAMES ###

# Splitting reviews into range buckets

review_range <- hotel %>%
  select(Review_Range, Total_Number_of_Reviews,
         Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts, Hotel_Name) %>%
  group_by(Review_Range) %>%
  summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
            Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
            Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
            Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
            Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words),
            Num_Reviews = n(),
            Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
  )

# Removing Lat & Lng from hotel for plot

hotel.names <- hotel %>%
  select(Hotel_Name, Hotel_Address,Country, lat, lng, Average_Score, Total_Number_of_Reviews,
         Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
  filter(lat != 0 & lng != 0) %>%
  group_by(Hotel_Name, Hotel_Address,Country, lat, lng,Average_Score, Total_Number_of_Reviews) %>%
  summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
            Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
            Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
            Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
            Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))

# Adding Country to create plot

m1<- hotel %>%
  select(Hotel_Name, Hotel_Address,Country, lat, lng, Average_Score, Total_Number_of_Reviews,
         Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
  filter(lat != 0 & lng != 0) %>%
  group_by(Hotel_Name, Hotel_Address,Country, lat, lng,Average_Score, Total_Number_of_Reviews) %>%
  summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
            Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
            Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
            Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
            Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))

# Subsetting Country to create plot

country.stats <- hotel.names %>%
  select(Country, Average_Score, Total_Number_of_Reviews, Tot_Pos_Words, Tot_Neg_Words,
         Total_Words) %>%
  group_by(Country) %>%
  summarize(Avg_Hotel_Review = mean(Average_Score),
            Positive_Words = sum(Tot_Pos_Words),
            Negative_Words = sum(Tot_Neg_Words),
            Total_Words = sum(Total_Words),
            Pos_Word_Rate = percent(Positive_Words/Total_Words),
            Neg_Word_Rate = percent(Negative_Words/Total_Words),
            Number_Hotels = n(),
            Total_Number_of_Reviews = sum(Total_Number_of_Reviews))

# Only Negative by Review Score Range - Reviews Containing Only Negative Words

zero_positive <- filter(hotel, Review_Total_Positive_Word_Counts == 0 & Review_Total_Negative_Word_Counts > 0)

zero_pos_rng = zero_positive %>%
  select(Review_Range, Total_Number_of_Reviews, Review_Total_Negative_Word_Counts, Hotel_Name) %>%
  # #Remove the 17 records without geo coordinates
  # filter(lat != 0 & lng != 0) %>%
  group_by(Review_Range) %>%
  summarise(Total_Words = sum(Review_Total_Negative_Word_Counts),
            Num_Reviews = n(),
            Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
  )

### Only Positive by Review Score Range - Reviews Containing Only Positive Words

zero_negative <- filter(hotel, Review_Total_Positive_Word_Counts > 0 & Review_Total_Negative_Word_Counts == 0)

zero_neg_rng = zero_negative %>%
  select(Review_Range, Total_Number_of_Reviews, Review_Total_Positive_Word_Counts, Hotel_Name) %>%
  # #Remove the 17 records without geo coordinates
  # filter(lat != 0 & lng != 0) %>%
  group_by(Review_Range) %>%
  summarise(Total_Words = sum(Review_Total_Positive_Word_Counts),
            Num_Reviews = n(),
            Avg_Words_Per_Review = format(Total_Words/Num_Reviews,digits = 4)
  )
includeCSS
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    #can't change lightblue as it is a function (using hex color instead)
    light_blue = "#3f424d"
  ),
  adminlte_sidebar(
    width = "220px",
    dark_bg = "#e0d7d9",  #"#c9bbbb",
    dark_hover_bg = "#5f97a3",
    dark_color = "#5f97a3"
  ),
  adminlte_global(
    content_bg = "#e4e9eb",
    box_bg = "#green", 
    info_box_bg = "#ba8888"
  )
)

ui <- dashboardPage(
  dashboardHeader(title = strong("EU Travel App",icon("globe")),
                  tags$li(class = "dropdown",
                          tags$a(href="http://www.linkedin.com/in/rupesh-kumar-38260418", 
                                 icon("linkedin"), "LinkedIn", target="_blank")),
                  tags$li(class = "dropdown",
                          tags$a(href="https://github.com/Rupesh707",
                                 icon("github"), "Github", target="_blank")),
                  tags$li(class = "dropdown",
                          tags$a(href="https://www.kaggle.com/hunter0007",
                                 icon("kaggle"),"Kaggle",target="_blank")),
                  tags$li(class = "dropdown",
                          tags$a(href="https://twitter.com/Rupezzz707", 
                                 icon("twitter"), "Twitter",target="_blank"),
                  )),
  
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("STORY", tabName = "Story", icon = icon("building", lib = "font-awesome")),
      menuItem("OVERVIEW", tabName = "Table", icon = icon("users-cog", lib = "font-awesome")),
      menuItem("GEOGRAPHY", tabName = "Map", icon = icon("map-marked-alt", lib = "font-awesome")),
      menuItem("SENTIMENTAL ANALYSIS", tabName = "Analysis", icon = icon("cogs", lib = "font-awesome")),
      menuItem("RECOMMENDATIONS", icon= icon("handshake", lib = "font-awesome"),
               sliderInput("Star", "REVIEW SCORES",                        
                           min = 5, max = 10, 
                           value = c(5, 10), step = .1, sep = ""),
               checkboxGroupInput("Reviews", "REVIEWS",
                                  choices = unique(review_range$Review_Range),
                                  selected = c('[1.5,2.5)', '(2.5,3.5)', '(3.5,4.5)',
                                               '(4.5,5.5)','(5.5,6.5)',
                                               '(6.5,7.5)','(7.5,8.5)','(8.5,9.5)','(9.5,10.5)'
                                  ))),
      menuItem("QUICK SEARCH", tabName = "Search", icon = icon("search", lib = "font-awesome")),
      menuItem("LET'S GO", tabName = "go", icon = icon('tachometer-alt', lib = "font-awesome"),
               includeCSS("www/stars.css"),
               sliderInput(inputId = "Stars", label = "Let's Go!", min = 5,  max = 10, value = 7.5, step = .1),
               tags$div(class = "ratings",
                        tags$div(class = "empty-stars",
                                 uiOutput("stars_ui"))))
    )
    
  ),
  
  body <- dashboardBody(use_theme(mytheme),
                        fluidRow(
                          mainPanel(),
                          tabItems(
                            tabItem(tabName ="Story",
                                    #column(width = 8, img(src = "Rupesh.png", height = 300, width = 400,align = "right")),
                                    h2(strong("WELCOME TO HOTEL RECOMMENDATION APPLICATION !")),
                                    column(width = 12, HTML('<iframe src="https://player.vimeo.com/video/538805985?title=0&amp;byline=0&amp;portrait=0&amp;speed=0&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479" width="550" height="300" frameborder="0" allow="autoplay; fullscreen; picture-in-picture" allowfullscreen title="EuTravelApp - Rshiny"></iframe>'), align = "center"),
                                    column(width = 12,
                                           h5(" As travel & tourism industry comes to a complete halt due to the pandemic."),
                                           h5(" 1.) What will be experience of choosing a hotel be in Europe?"), 
                                           h5(" 2.) Are price comparators really justifying the price post pandemic?"),
                                           h5(" 3.) Are there other ways to quantify hotels bases on reviews?"),
                                           h5(" 4.) How can I choose the right hotel for me?"),
                                           h4("BACKGROUND"),
                                           h5(" Hello world my name is Rupesh Kumar & I am a hospitality professional with years of international experience in travel and tourism across the globe, Travelling, Working & Living. I know how difficult choosing a perfect hotel for the right occasion can be. That's why I created this application using sentimental analysis of over 500K reviews from 1477 hotels across Europe to help travelers make the right decision all the time. The features build in the application will guide you choose a perfect getaway weather it is for work, leisure, special occasion or visiting friends and family."),
                                           h6(strong("BON VOYAGE!!!")),
                                    ),
                            ),
                            tabItem(tabName = "Map",
                                    leafletOutput("Map", width = "100%", height = 1000),
                            ),
                            tabItem(tabName = "Search",
                                    dataTableOutput("dt2")
                            ),
                            tabItem(tabName ="Analysis", 
                                    tabBox(height = "480px", width = 6,
                                           tabPanel(strong(icon("comment")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,
                                                                 plotOutput ("pt")))),
                                           tabPanel(strong(icon("smile-beam")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput("pt7")))),
                                           tabPanel(strong(icon("file-word")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput("pt6")))),
                                           tabPanel(strong(icon("sort-amount-up")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput("pt1"))))),
                                    tabBox(height = "480px", width = 6,
                                           
                                           tabPanel(strong(icon("globe-europe")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput ("pt2")))),
                                           
                                           tabPanel(strong(icon("frown")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput ("pt5")))),
                                           tabPanel(strong(icon("file-word")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput("pt4")))),
                                           tabPanel(strong(icon("sort-amount-down")),
                                                    fluidRow(box(title = "",
                                                                 status = "primary",width = 12,plotOutput("pt3"))))),dataTableOutput("dt1")),
                            
                            tabItem(tabName = "Table", 
                                    column(width = 12,
                                           valueBoxOutput("No.Hotels"),
                                           valueBoxOutput("Negative.Comments"),
                                           valueBoxOutput("Average.Rating")),
                                    
                                    column(width = 12,
                                           valueBoxOutput("Total.Reviews"),
                                           valueBoxOutput("Positive.Words"),
                                           valueBoxOutput("Negative.Words")),
                                    
                                    dataTableOutput ("dt"),
                            ))))
)
##########################################################################################################                                               LEAFLET MAP CODE                   ##########################################################################################################
server <- function(input, output, session){
  output$Map <- renderLeaflet ({
    coords2country = function(points)
    {  
      countriesSP <- getMap(resolution='high')
      pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
      indices = over(pointsSP, countriesSP)
      indices$ADMIN
    }
    
    points <- cbind(m1$lng,m1$lat)
    leaflet() %>% 
      addProviderTiles('OpenStreetMap.Mapnik',
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = m1%>%
                   filter(Average_Score>= input$Star[1], Average_Score <= input$Star[2]),
                 popup = paste0("<strong>Hotel: </strong>", 
                                m1$Hotel_Name,                 
                                "<br><strong>Address: </strong>", 
                                m1$Hotel_Address, 
                                "<br><strong>Average Score: </strong>", 
                                m1$Average_Score, 
                                "<br><strong>Number of Reviews: </strong>", 
                                m1$Total_Number_of_Reviews,
                                "<br><strong>Percent Positive Review Words: </strong>",
                                m1$Pos_Word_Rate),
                 clusterOptions = markerClusterOptions()) 
  })
  
  points <- reactive({
    m1%>%
      filter(Average_Score>= input$Star[1], Average_Score <= input$Star[2])
  })
  
  
  ########################################################################################################                                               DATA TABLES                                                 ##########################################################################################################
  output$dt <- renderDataTable ({
    hotel.names = hotel %>%
      select(Hotel_Name, Hotel_Address,Country, Average_Score, Total_Number_of_Reviews,
             Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
      group_by(Hotel_Name, Hotel_Address,Country,Average_Score, Total_Number_of_Reviews) %>%
      summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
                Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
                Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
                Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
                Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))%>%
      filter(Average_Score>= input$Star[1], Average_Score <= input$Star[2])
    #filter(Country %in% input$Count)
  }) 
  
  output$dt1 <- renderDataTable ({
    hotel.names = hotel %>%
      select(Hotel_Name, Hotel_Address,Country, Average_Score, Total_Number_of_Reviews,
             Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
      group_by(Hotel_Name, Hotel_Address,Country,Average_Score, Total_Number_of_Reviews) %>%
      summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
                Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
                Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
                Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
                Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))%>%
      filter(Average_Score>= input$Star[1], Average_Score <= input$Star[2])
    #filter(Country %in% input$Count)
  })
  
  output$dt2 <- renderDataTable ({
    hotel.names = hotel %>%
      select(Hotel_Name, Hotel_Address,Country, Average_Score, Total_Number_of_Reviews,
             Review_Total_Positive_Word_Counts, Review_Total_Negative_Word_Counts) %>%
      group_by(Hotel_Name, Hotel_Address,Country,Average_Score, Total_Number_of_Reviews) %>%
      summarise(Tot_Pos_Words = sum(Review_Total_Positive_Word_Counts),
                Tot_Neg_Words = sum(Review_Total_Negative_Word_Counts),
                Total_Words = sum(Tot_Pos_Words + Tot_Neg_Words),
                Pos_Word_Rate = percent(Tot_Pos_Words/Total_Words),
                Neg_Word_Rate = percent(Tot_Neg_Words/Total_Words))%>%
      filter(Average_Score %in% input$Stars)
  })
  
  output$stars_ui <- renderUI({
    # to calculate our input %
    n_fill <- (input$Stars / 2 )*20
    # element will look like this: <div class="full-stars" style="width:n%"></div>
    style_value <- sprintf("width:%s%%", n_fill)
    tags$div(class = "full-stars", style = style_value)
  })
  
  ########################################################################################################                                                  PLOTS                                                  ##########################################################################################################
  
  # NUMBER OF REVIEWS BY RANGE OF SCORE
  
  output$pt <- renderPlot({
    
    s1 <- ggplot(data(),
                 aes(x=Review_Range,
                     y=Num_Reviews,
                     fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label = Num_Reviews), nudge_y = 0.5)+
      ggtitle(label="Number Of Reviews By Range of Score")+
      labs(xlab('Review Range'),ylab('Number of Reviews'))+
      theme(legend.position = 'off')
    grid.draw(s1)
    
  })
  
  data <- reactive({
    review_range%>%
      filter(Review_Range %in% input$Reviews)
  })
  
  # Only positive words
  
  output$pt1 <- renderPlot({
    p8 <- ggplot(data(), aes(x=Review_Range,
                             y=Avg_Words_Per_Review,
                             fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label=Avg_Words_Per_Review,nudge_y = 0.2))+
      ggtitle(label="Positive Average Words Per Review")+
      labs(xlab('Review Range'),ylab('Avg. Words per Review'))+
      scale_fill_brewer(palette = 'Greens')+
      theme(legend.position = 'off')
    grid.draw(p8)
  })
  
  # NUMBER OF HOTELS REVIEWS & AVG.SCORE BY COUNTRY
  
  output$pt2 <- renderPlot({
    ggplot(data = country.stats,aes(x=reorder(Country,-Number_Hotels), y=Number_Hotels, fill=Country))+
      geom_bar(stat='identity', alpha=0.7) +
      geom_text(label=format(country.stats$Avg_Hotel_Review, digits = 2)) +
      labs(x='Country', y='Number of Hotels Reviewed') +
      ggtitle(label='Number Of Hotels Reviewed & Avg. Score By Country') +
      scale_fill_brewer(palette='Pastel2')
  })
  
  # PERCENT POSITIVE WORDS BY RANGE OF SCORE
  
  output$pt3 <- renderPlot({
    p5 <- ggplot(data1(), aes(x=Review_Range,
                              y=Avg_Words_Per_Review,
                              fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label=Avg_Words_Per_Review,nudge_y = 0.2))+
      ggtitle(label="Negative Average Words Per Review")+
      labs(xlab('Review Range'),ylab('Avg. Words per Review'))+
      scale_fill_brewer(palette = 'Reds')+
      theme(legend.position = 'off')
    
    grid.draw(p5)
  })
  
  # NEGATIVE WORDS RANGE SCORE
  
  output$pt4 <- renderPlot({
    p4 <- ggplot(data1(), aes(x=Review_Range,
                              y=Num_Reviews,
                              fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label=Num_Reviews,nudge_y = 0.5))+
      ggtitle(label="Negative Words Score")+
      labs(xlab('Review Range'),ylab('Number of Reviews'))+
      scale_fill_brewer(palette = 'Set1')+
      theme(legend.position = 'off')
    
    grid.draw(p4)
    
  })
  
  data1 <- reactive({
    zero_pos_rng%>%
      filter(Review_Range %in% input$Reviews)
  })
  
  # TOTAL NUMBER OF NEGATIVE WORDS
  
  output$pt5 <- renderPlot({
    p6 <- ggplot(data1(), aes(x=Review_Range,
                              y=Total_Words,
                              fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label=Total_Words,nudge_y = 0.2))+
      ggtitle(label="Negative Words By Range Of Score")+
      labs(xlab('Review Range'),ylab('Number of Negative Words'))+
      scale_fill_brewer(palette = 'Set1')+
      theme(legend.position = 'off')
    grid.draw(p6)
    
  })
  
  # ONLY POSITIVE WORDS 
  
  output$pt6 <- renderPlot({
    p7 <- ggplot(data2(), aes(x=Review_Range,
                              y=Num_Reviews,
                              fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label=Num_Reviews,nudge_y = 0.5))+
      ggtitle(label="Positive Words Score")+
      labs(xlab('Review Range'),ylab('Number of Reviews'))+
      scale_fill_brewer(palette = 'Dark2')+
      theme(legend.position = 'off')
    grid.draw(p7)
    
  })
  
  # TOTAL NUMBER OF POS WORDS
  output$pt7 <- renderPlot({
    p9 <- ggplot(data2(), aes(x=Review_Range,
                              y=Total_Words,
                              fill=Review_Range))+
      geom_bar(stat='identity', alpha=0.7)+
      geom_text(aes(label=Total_Words,nudge_y = 0.2))+
      ggtitle(label="Positive Words By Range Of Score")+
      labs(xlab('Review Range'),ylab('Number of Positive Words'))+
      scale_fill_brewer(palette = 'Dark2')+
      theme(legend.position = 'off')
    grid.draw(p9)
    
  })
  
  data2 <- reactive({
    zero_neg_rng%>%
      filter(Review_Range %in% input$Reviews)
  })
  
  #######################################################################################################                                          VALUEBOXES CODE                                    #########################################################################################################
  
  output$No.Hotels <- renderValueBox({
    hot1 <- points()
    valueBox(
      value = length(hot1$Hotel_Name),
      subtitle = "Number Of Hotels", 
      icon = icon("hotel"),
      color = "yellow")
  })
  output$Negative.Comments <- renderValueBox({
    hot3 <- points()
    valueBox(
      value = round(mean(hot3$Total_Words)/100,1),
      subtitle = "Total Average Words",
      icon = icon("diagnoses"),
      color = "maroon")
  })
  output$Average.Rating <- renderValueBox({
    hot4 <- points()
    valueBox(
      value = round(mean(unique(hot4$Average_Score)),1),
      subtitle = "Average Rating", 
      icon = icon("star-half-alt"),
      color = "teal")
  })
  output$Total.Reviews <- renderValueBox({
    hot5 <- points()
    valueBox(
      value = sum(unique(hot5$Total_Number_of_Reviews)),
      subtitle = "Total Review Words",
      icon = icon("globe-europe"),
      color = "blue")
  })
  output$Positive.Words <- renderValueBox({
    hot6 <- points()
    valueBox(
      value = round(mean(hot6$Tot_Pos_Words),1),
      subtitle = "Total Positive Words",
      icon = icon("smile-beam"),
      color = "olive")
  })
  output$Negative.Words <- renderValueBox({
    hot7 <- points()
    valueBox(
      value = round(mean(hot7$Tot_Neg_Words),1),
      subtitle = "Total Negative Words",
      icon = icon("frown"),
      color = "red")
  })
}
shinyApp(ui, server)