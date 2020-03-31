#===============================================================================#
# RENTERS VULNERABLE FROM COVID-19 CRASH
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "janitor",
          "shiny")
lapply(libs, library, character.only = TRUE)

source("assemble_data.R")
print(getwd())

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Renter dashboard"),
    
    p("Explanatory text: here are renters, x number are renters and x are 30% burdened"),
    
    p("Of those struggling, here's the jobs they work"),
    
    p("Here are especially vulnerable bc loss of income"),
    
    p("There are also renters who weren't burdened but who work vulnerable jobs"),
    
    br(),
     
    # select box for metro names
    selectInput("geo_name",
                label = "Choose a geographic area",
                choices = sort(unique(geo_list$NAME)),
                selected = "California"),
    
    br(),

    # # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
           plotOutput("distPlot")
           
           #
           
        # )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # geo <- filter(geo_list, NAME == input$geo_name)
    geo <- filter(geo_list, NAME == "California")
    
    output$distPlot <- renderPlot({
        
        state_data %>% 
            filter(GEOID == geo$GEOID,
                   age_cat != "Under20") %>% 
            select(age_cat, n_all, n_renters) %>% 
            gather("hhtype", "num", -age_cat) %>% 
            ggplot(aes(x = age_cat,
                       y = num,
                       fill = hhtype)) +
            geom_col(position = "dodge")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
