#===============================================================================#
# RENTERS VULNERABLE FROM COVID-19 CRASH
#
# Cecile Murray
#===============================================================================#

library("tidyverse")
library("purrr")
library("janitor")
library("shiny")
library("haven")

#===============================================================================#
# DATA LOADING
#===============================================================================#

# setwd(paste0(here(), "/covid_rent_burden"))

load("geo_list.Rdata")
load("st_burden_by_sector.Rdata")
load("st_vulnerable_shares.Rdata")
load("st_rent_by_burden.Rdata")
load("st_age_by_burden.Rdata")
load("st_raceth_by_burden.Rdata")

# setwd(here())

vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# AESTHETIC THEMING
#===============================================================================#

# Terner colors
terner_gray <- "#5B6770"
terner_blue <- "#4E748B" 
terner_gold <- "#B7B09D" 
terner_navy <- "#011E41"
terner_red <- "#E74C39"

# define theme
terner_theme <- function(...) {
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(color = "gray75",
                                          size = rel(0.75),
                                          linetype = "dotted"),
          text = element_text(family = "Lato", size = 11),
          axis.text = element_text(size = 11),
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 14),
          plot.subtitle = element_text(size = 12),
          axis.ticks.x = element_blank(),
          legend.background = element_blank()) +
        theme(...)
}

#===============================================================================#
# UI
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

#===============================================================================#
# SERVER
#===============================================================================#

# Define server logic required to draw a histogram
server <- function(input, output) {

    # get names and FIPS for selected geography
    # geo <- filter(geo_list, NAME == input$geo_name)
    geo <- filter(geo_list,
                  NAME == "California")
    
    # filter data for 1st and 2nd charts to that geography
    geo_sector_burden <- filter(st_table,
                                STATEFIP == geo$GEOID,
                                sample_size_sector_burdened > 10)
    
    # filter data for share of renter households chart
    geo_renter_hholds <- filter(st_vulnerable_shares,
                                STATEFIP == geo$GEOID,
                                sample_size > 10)
    
    # filter data for median and total rent chart
    geo_rent <- filter(st_rent_by_burden,
                       STATEFIP == geo$GEOID,
                       sample_size > 10)

    # filter data for age chart
    geo_age <- filter(st_age_by_burden,
                       STATEFIP == geo$GEOID,
                       sample_size > 10)
    
    # filter data for raceth chart
    geo_raceth <- filter(st_raceth_by_burden,
                       STATEFIP == geo$GEOID,
                       sample_size > 10)

    
    # compute fixed scale limit for % burdened worker charts
    # x_burden_share <- geo_sector_burden %>% 
    #     group_by(sector) %>% 
    #     summarize(share_of_burden = sum(share_of_burden)) 
    # max_x_burden_share <- max(x_burden_share$share_of_burden)
    
    output$alreadyBurdenedWorkerPlot <- renderPlot({
        
        geo_sector_burden %>% 
            ungroup() %>% 
            filter(cost_burdened == "Rent burdened",
                   sector %in% vulnerable_sectors) %>% 
            pivot_longer(cols = c("sector_burden_share",
                                  "sector_share")) %>% 
            select(sector, name, value) %>% 
            ggplot(aes(x = reorder(sector, value),
                       y = value,
                       fill = name)) +
            geom_col(position = "dodge") +
            # geom_text(aes(label = round(value * 100, 2)),
            #           nudge_y = 0.001,
            #           family = "Lato") +
            # scale_x_continuous(limits = c(0, max_x_burden_share)) +
            scale_fill_manual(values = c("sector_burden_share" = "#ffbb22",
                                         "sector_share" = terner_blue)) +
            scale_y_continuous(label = scales::percent) +
            coord_flip() +
            labs(title = "TITLE",
                 subtitle = "Workers in vulnerable sectors as a share of burdened households and all workers, 2018",
                 x = "Sector",
                 y =  "% of workers",
                 caption = "SOURCE/NOTE HERE") +
            terner_theme()
        
    })
    
    output$newlyVulnerableWorkerPlot <- renderPlot({

        geo_sector_burden %>%
            filter(sector != "Not vulnerable") %>%
            ggplot(aes(x = reorder(sector, n_sector_burdened),
                       y = n_sector_burdened,
                       fill = cost_burdened)) +
            geom_col() +
            # geom_text(aes(y = n_renters / 2,
            #     label = round(n_renters, -2)),
            #           # nudge_y = 10000,
            #           family = "Lato") +
            # scale_x_continuous(limits = c(0, max_x_burden_share)) +
            scale_y_continuous(label = scales::number) +
            scale_fill_manual(values = c("Rent burdened" = "#ffbb22",
                                "Not burdened" = terner_blue)) +
            coord_flip() +
            labs(title = "TITLE",
                 subtitle = "Number of workers who rent in vulnerable sectors by rent burden, 2018",
                 x = "Sector",
                 y = "Number of workers in renter households",
                 caption = "SOURCE/NOTE HERE") +
            terner_theme()

    })
    
    output$totalVulnerableHouseholdsPlot <- renderPlot({
        
        vul_shares <- geo_renter_hholds %>% 
            filter(is_vulnerable == 1) 
        
        bind_rows(vul_shares,
                  tibble("cost_burdened" = c("Remainder"),
                         "share_vulnerable" = 1-sum(vul_shares$share_vulnerable))) %>% 
            mutate(cost_burdened = factor(cost_burdened,
                                          levels = c("Remainder",
                                                     "Not burdened",
                                                     "Rent burdened"))) %>% 
            ggplot(aes(x = 0.5,
                       y = share_vulnerable,
                       fill = cost_burdened)) +
            geom_col(width = 0.25) +
            scale_x_continuous(limits = c(0, 1),
                               labels = NULL) +
            scale_y_continuous(label = scales::percent,
                               limits = c(0, 1)) +
            scale_fill_manual(values = c("Rent burdened" = "#ffbb22",
                                         "Not burdened" = terner_blue,
                                         "Remainder" = terner_gold)) +
            coord_flip() +
            labs(title = "TITLE",
                 subtitle = "Share of renter households with workers in vulnerable industries by existing rent burden",
                 x = "",
                 y = "Share of renter households",
                 caption = "SOURCE/NOTE HERE") +
            terner_theme(axis.ticks.x = element_blank())
    })
    
    output$medianRentPlot <- renderPlot({
        
        geo_rent %>% 
            filter(cost_burdened != "Zero household income") %>% 
            ggplot(aes(x = cost_burdened,
                       y = median_rent)) +
            geom_col(fill = terner_blue) +
            scale_y_continuous(label = scales::dollar) +
            labs(title = "TITLE",
                 subtitle = "Median rent in households with at least one vulnerable worker, 2018",
                 x = "",
                 y = "Median monthly gross rent ($)") +
            terner_theme()
    })
    
    output$totalRentPlot <- renderPlot({
        
        geo_rent %>% 
            filter(cost_burdened != "Zero household income") %>% 
            ggplot(aes(x = cost_burdened,
                       y = total_rent)) +
            geom_col(fill = terner_blue) +
            scale_y_continuous(label = scales::dollar) +
            labs(title = "TITLE",
                 subtitle = "Total rent in households with at least one vulnerable worker, 2018",
                 x = "",
                 y = "Total gross rent ($)") +
            terner_theme()
    })
    
    output$agePlot <- renderPlot({
        
        geo_age %>% 
            ggplot(aes(x = age_cat,
                       y = value, 
                       fill = name)) +
            geom_col(position = "dodge") +
            scale_y_continuous(label = scales::percent) +
            scale_fill_manual(values = c("vulnerable_share" = terner_blue,
                                         "group_share" = terner_gold)) +
            labs(title = "TITLE",
                 subtitle = "People in vulnerable households vs. all renter households by age",
                 x = "Age group",
                 y = "% of people in renter households",
                 caption = "Note: group_share means all people in renter hholds in age category") +
            terner_theme()
    })
    
    output$racethPlot <- renderPlot({
        
        geo_raceth %>% 
            ggplot(aes(x = raceth,
                       y = value, 
                       fill = name)) +
            geom_col(position = "dodge") +
            scale_y_continuous(label = scales::percent) +
            scale_fill_manual(values = c("vulnerable_share" = terner_blue,
                                         "group_share" = terner_gold)) +
            labs(title = "TITLE",
                 subtitle = "People in vulnerable households vs. all renter households by race/ethnicity",
                 x = "Race/ethnic group",
                 y = "% of people in renter households",
                 caption = "Note: group_share means all people in renter hholds in race/ethnicity category") +
            terner_theme()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
