#===============================================================================#
# USER INTERFACE FOR SHINY APP
#
# Cecile Murray
#===============================================================================#

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Vulnerable renter dashboard"),
  
  p("Explanatory text for what this tool does + Terner attribution"),
  
  # select box for metro names
  selectInput("geo_name",
              label = "Choose a geographic area",
              choices = sort(unique(geo_list$NAME)),
              selected = "California"),
  
  br(),
  
  p("PLACE XXX renter households are home to X% of the region's population of XXXXX."),
  
  p("x% of these households were already rent burdened before the pandemic, meaning they pay more than 30% of their income on rent."),
  
  p("Adults in PLACE's rent-burdened households are more likely to work in industries vulnerable to job or income losses due to efforts to slow the spread of COVID-19."),
  
  plotOutput("alreadyBurdenedWorkerPlot"),
  
  p("But there are also households that weren't rent burdened before the pandemic, but may now be because of income or job losses due to COVID-19. XXX adults in non-burdened households work in vulnerable industries."),
  
  plotOutput("newlyVulnerableWorkerPlot"),
  
  p("Between renter households that were already struggling and newly vulnerable, x% of renter households are vulnerable to job or income losses because of COVID-19."),
  
  plotOutput("totalVulnerableHouseholdsPlot"),
  
  p("Each month, these vulnerable households pay a total of $xx in rent."),
  
  fluidRow(
    column(5, 
           plotOutput("medianRentPlot")
    ),
    column(5,
           plotOutput("totalRentPlot")
    )
  ),
  
  
  p("x% of PLACE's population live in these households, and they skew younger/xxxx than average for the region."),
  
  plotOutput("racethPlot"),
  
  plotOutput("agePlot"),
  
  
  p("Source: IPUMS USA, University of Minnesota, www.ipums.org."),
  
  p("Notes:")
  
)