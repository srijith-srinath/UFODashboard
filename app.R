# This is a Shiny web application. You can use this to find 
# everything you want about UFOs

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(leaflet)
library(colorRamps)
library(RColorBrewer)

#read the data file into a data.frame
ufos <- read.csv("~/Documents/Independant/Project 2/UFOS_Cleaned.csv", sep = "\t")
#dictionary for countries
countries <- c("US", "CA", "AU", "GB","World")
names(countries) <- c("USA", "Canada", "Australia", "UK", "World")


renderMap <- function(points) {
  map <- leaflet(data = points) %>%
    addTiles() %>%
    addCircles(lng = ~longitude, lat = ~latitude, weight = 3, radius=40, 
               color="#70b7ff", stroke = TRUE, fillOpacity = 0.8)
  return(map)
}


# Define UI for application
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "UFOs, everywhere?"),
    
    
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Duration", tabName = "duration_year", icon = icon("arrow-circle-right")),
        menuItem("Shapes", tabName = "shape_year", icon = icon("arrow-circle-right")),
        menuItem("Maps", tabName = "country_map", icon = icon("arrow-circle-right"))
        )
      
    ),
    
    
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "duration_year",
                h4("How long were they visible?"),
                
                box(
                  title = "Year vs Duration", solidHeader = TRUE, status = "primary", width = 8,
                  plotOutput("duration_year")
                ),
                
                box(
                  title = "24Hr Clock", solidHeader = TRUE, status = "primary", width = 4,
                  plotOutput("d_y")
                ),
                
                box(
                  title = "Inputs", solidHeader = TRUE, status = "info", width = 4,
                  p("Select any shape whose duration by year you want to check."),
                  selectInput("ChooseShape","Select a Shape",c('Cylinder', 'Circle', 'Light', 'Sphere', 'Disk', 'Fireball',
                                                               'Oval', 'Other', 'Rectangle', 'Chevron', 'Formation',
                                                               'Triangle', 'Cigar', 'Delta', 'Changing', 'Egg', 'Diamond',
                                                               'Flash', 'Teardrop', 'Cone', 'Cross','Unknown'))
                ),
                
                infoBoxOutput("Maximum"),
                infoBoxOutput("Minimum")
                
                    

        ),
        
        tabItem(tabName = "shape_year",
                h4("The different shapes they took!!"),
                
                box(
                  title = "Shapes vs Year", solidHeader = TRUE, status = "primary", width = 12,
                  plotOutput("year_shape")
                ),
                
                box(
                  title = "Inputs", solidHeader = TRUE, status = "info", width = 4,
                  textInput("textyear", "Enter any year you want to check on.", "1994"),
                  p("Min value = 1910 and Max value = 2014")
                ),
                
                infoBoxOutput("Max"),
                infoBoxOutput("Min")
        ),
        
        tabItem(tabName = "country_map",
                h4("Where were they visible?"),
                
                box(
                  title = "Maps", solidHeader = TRUE, status = "primary", width = 8,
                  leafletOutput("mymap")
                ),
                
                box(
                  title = "Input 1", solidHeader = TRUE, status = "info", width = 4,
                  selectInput("ChooseShape","Select a Shape",c('Cylinder', 'Circle', 'Light', 'Sphere', 'Disk', 'Fireball',
                                                               'Oval', 'Other', 'Rectangle', 'Chevron', 'Formation',
                                                               'Triangle', 'Cigar', 'Delta', 'Changing', 'Egg', 'Diamond',
                                                               'Flash', 'Teardrop', 'Cone', 'Cross','Unknown')),
                
                
               
                  #title = "Input 2", solidHeader = TRUE, status = "info", width = 4,
                  selectInput("ChooseCountry","Select a Country", c("USA", "Canada", "Australia", "UK","World")),
               
                
                
                  #title = "Click Go", solidHeader = TRUE, status = "info", width = 4,
                  actionButton("go", "Go")
                )
        )
      )
      
    )
  )
)

# Define server logic
server <- function(input, output) {
   
#Server functions for tab -> duration_year
  output$duration_year <- renderPlot( {
    theshape <- input$ChooseShape
    message <- paste("For",theshape, "UFO, the total duration for that year", sep = " ")
    ufos_shape <- filter(ufos, shape == theshape)
    xyz <- data.frame(aggregate(duration_seconds ~ year, ufos_shape, sum))
    xyz['duration_seconds'] <- xyz['duration_seconds']/60
    
    maxi <- xyz[which.max(xyz$duration_seconds),]
    mini <- xyz[which.min(xyz$duration_seconds),]
    
    maxdur <- round(as.vector(maxi$duration_seconds),2)
    mindur <- round(as.vector(mini$duration_seconds),2)
    maxyr <- as.vector(maxi$year)
    minyr <- as.vector(mini$year)
    
    output$Maximum <- renderInfoBox({
      infoBox("Maximum : ", paste0("\'",maxdur,"\'"," s"," for the year ","\'",maxyr,"\'"),icon = shiny::icon("arrow-up"),color = "aqua")})
    output$Minimum <- renderInfoBox({
      infoBox("Minimum : ", paste0("\'",mindur,"\'"," s"," for the year ","\'",minyr,"\'"),icon = shiny::icon("arrow-down"),color = "aqua")})
    
    plot(xyz, main=message,
         xlab = "Year Spotted", "Duration (Min)")
    axis(side = 1, at = seq(1910,2014,5))
  } )
  
  output$d_y <- renderPlot({
    theshape <- input$ChooseShape
    ufos_shape <- filter(ufos, shape == theshape)
    ggplot(ufos_shape, aes(x=hour)) + 
      geom_density(color="darkblue") +
      xlim(range(ufos_shape$hour)) + xlab("Hour in 24hrs")
  })
    
#Server functions for tab -> duration_year  
  output$year_shape <- renderPlot( {
    theyear <- input$textyear
    zz <- filter(ufos, year == theyear)
    abc <- data.frame(aggregate(duration_seconds ~ shape, zz, sum))
    abc['duration_seconds'] <- abc['duration_seconds']/60
    
    maxi <- abc[which.max(abc$duration_seconds),]
    mini <- abc[which.min(abc$duration_seconds),]
    
    maxdur <- round(as.vector(maxi$duration_seconds),2)
    mindur <- round(as.vector(mini$duration_seconds),2)
    maxsh <- as.vector(maxi$shape)
    minsh <- as.vector(mini$shape)
    
    output$Max <- renderInfoBox({
      infoBox("Maximum : ", paste0("\'",maxdur,"\'"," s"," for the shape ","\'",maxsh,"\'"),icon = shiny::icon("arrow-up"),color = "aqua")})
    output$Min <- renderInfoBox({
      infoBox("Minimum : ", paste0("\'",mindur,"\'"," s"," for the shape ","\'",minsh,"\'"),icon = shiny::icon("arrow-down"),color = "aqua")})
    
    theyear <- input$textyear
    ufos_shape <- filter(ufos, year == theyear)
    shapes <- pull(ufos_shape,shape)
    ggplot(data.frame(shapes), aes(x=shapes, color = shapes, fill = shapes)) + theme(legend.position="none") +
      geom_bar() + xlab("Shapes") + ylab("Count (Frequency of Occurrences)")
  } ) 
  
#Server functions for tab -> country_map
  points <- eventReactive(input$go,
  {
    theshape <- input$ChooseShape
    thecountry <- input$ChooseCountry
    
    tcountry <- countries[[thecountry]]
    
    if(tcountry == "World"){
      ufos_country <- filter(ufos, shape == theshape)
    }else
    {
      ufos_country <- filter(ufos, shape == theshape & country == tcountry)
    }
  })
  
  output$mymap <- renderLeaflet({
    map <- renderMap(points())
    map
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

