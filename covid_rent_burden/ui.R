#===============================================================================#
# USER INTERFACE FOR SHINY APP
#
# Cecile Murray
#===============================================================================#

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Renters at Risk: Households Likely to Be Impacted by Efforts to Flatten the Curve"),
  
  p("... This tool allows user to explore number of renter households likely facing near-term economic impacts of sheltering in place and the scale of the costs required to shore up their rents"),
  
  # select box for place
  selectInput("geo_name",
              label = "Choose a geographic area",
              choices = geo_list$NAME,
              selected = "United States"),
  
  br(),
  br(),
  
  textOutput("renterHHCountText"),
  br(),
  textOutput("vulnerableWorkerText"),
  

  br(),
  br(),
  
  plotOutput("alreadyBurdenedWorkerPlot"),
  
  br(),
  br(),
  
  textOutput("shareVulnerableText"),
  
  plotOutput("newlyVulnerableWorkerPlot"),
  
  br(),
  br(),
  
  p("Between renter households that were already struggling and newly vulnerable, x% of renter households are vulnerable to job or income losses because of COVID-19."),
  
  plotOutput("totalVulnerableHouseholdsPlot"),
  
  br(),
  br(),
  
  p("Each month, these vulnerable households pay a total of $xx in rent."),
  
  fluidRow(
    column(5, 
           plotOutput("medianRentPlot")
    ),
    column(5,
           plotOutput("totalRentPlot")
    )
  ),
  
  br(),
  br(),
  
  p("Who lives in these at-risk renter households?"),
  
  plotOutput("racethPlot"),
  br(),
  plotOutput("agePlot"),
  
  
  p("Source: IPUMS USA, University of Minnesota, www.ipums.org."),
  
  p("Notes/methods: sample size > 10, zero-income households, cost burden definition, race/ethnicity definition")
  
)