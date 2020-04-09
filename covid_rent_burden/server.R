#===============================================================================#
# DEFINE SERVER LOGIC 
#
# Cecile Murray
#===============================================================================#

library("tidyverse")
library("purrr")
library("janitor")
library("shiny")
library("haven")




server <- function(input, output) {
  
  # get names and FIPS for selected geography
  get_geo <- function() {
    filter(geo_list, NAME == input$geo_name)
  }
  

  # filter data for 1st and 2nd charts to that geography
  geo_sector_burden <- function() {
    filter(st_table,
           STATEFIP == get_geo()$GEOID,
           sample_size_sector_burdened > 10)
  }
  
  # filter data for share of renter households chart
  geo_renter_hholds <- function() {
    filter(st_vulnerable_shares,
           STATEFIP == get_geo()$GEOID,
           sample_size > 10)
  }
  
  # filter data for median and total rent chart
  geo_rent <- function() {
    filter(st_rent_by_burden,
           STATEFIP == get_geo()$GEOID,
           sample_size > 10)
  }
  
  # filter data for age chart
  geo_age <- function() {
    filter(st_age_by_burden,
           STATEFIP == get_geo()$GEOID,
           sample_size > 10)
  }
  
  # filter data for raceth chart
  geo_raceth <- function() {
    filter(st_raceth_by_burden,
           STATEFIP == get_geo()$GEOID,
           sample_size > 10)
  }
  
  # compute fixed scale limit for % burdened worker charts
  # x_burden_share <- geo_sector_burden %>% 
  #     group_by(sector) %>% 
  #     summarize(share_of_burden = sum(share_of_burden)) 
  # max_x_burden_share <- max(x_burden_share$share_of_burden)
  
  output$alreadyBurdenedWorkerPlot <- renderPlot({
    
    geo_sector_burden() %>% 
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
    
    geo_sector_burden() %>%
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
    
    vul_shares <- geo_renter_hholds() %>% 
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
    
    geo_rent() %>% 
      filter(cost_burdened != "Zero household income") %>% 
      ggplot(aes(x = cost_burdened,
                 y = median_rent)) +
      geom_col(fill = terner_blue) +
      scale_y_continuous(label = scales::dollar) +
      labs(title = "Vulnerable households in PLACE may struggle to make rent",
           subtitle = "Median rent in households with at least one vulnerable worker, 2018",
           x = "",
           y = "Median monthly gross rent ($)") +
      terner_theme()
  })
  
  output$totalRentPlot <- renderPlot({
    
    geo_rent() %>% 
      filter(cost_burdened != "Zero household income") %>% 
      ggplot(aes(x = cost_burdened,
                 y = total_rent)) +
      geom_col(fill = terner_blue) +
      scale_y_continuous(label = scales::dollar) +
      labs(title = "Rent from vulnerable households in PLACE add up to a substantial amount",
           subtitle = "Total monthly rent in households with at least one vulnerable worker, 2018",
           x = "",
           y = "Total gross rent ($)") +
      terner_theme()
  })
  
  output$agePlot <- renderPlot({
    
    geo_age() %>% 
      ggplot(aes(x = age_cat,
                 y = value, 
                 fill = name)) +
      geom_col(position = "dodge") +
      scale_y_continuous(label = scales::percent) +
      scale_fill_manual(values = c("vulnerable_share" = terner_blue,
                                   "group_share" = terner_gold)) +
      labs(title = "Young people are overrepresented among vulnerable renters in PLACE",
           subtitle = "Age distribution of people in vulnerable renter households vs. all people",
           x = "Age group",
           y = "% of people",
           caption = "Note: group_share = people in age category / all people") +
      terner_theme()
  })
  
  output$racethPlot <- renderPlot({
    
    geo_raceth_by_burden %>% 
    # geo_raceth() %>% 
      ggplot(aes(x = raceth,
                 y = value, 
                 fill = name)) +
      geom_col(position = "dodge") +
      scale_y_continuous(label = scales::percent) +
      scale_fill_manual(values = c("vulnerable_share" = terner_blue,
                                   "group_share" = terner_gold)) +
      labs(title = "Vulnerable renters in PLACE are more diverse than the general population",
           subtitle = "Distribution of people by race/ethnic group in vulnerable renter households vs. all people",
           x = "Race/ethnic group",
           y = "% of people",
           caption = "Note: group_share = people in race/ethnic group / all people") +
      terner_theme()
  })
}
