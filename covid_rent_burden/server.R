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
library("glue")
library("scales")


server <- function(input, output) {
  
  # get names and FIPS for selected geography
  get_geo <- function() {
    filter(geo_list, NAME == input$geo_name)
  }
  
  geo <- reactive({get_geo()})

  # filter data for 1st and 2nd charts to that geography
  geo_sector_burden <- function() {
    filter(industry_burden_table,
           GEOID == geo()$GEOID,
           sample_size_sector_burdened > 10)
  }
  
  # filter data for share of renter households chart
  geo_renter_hholds <- function() {
    filter(geo_vulnerable_shares,
           GEOID == geo()$GEOID,
           sample_size_burden_vulnerable > 10)
  }
  
  # filter data for median and total rent chart
  geo_rent <- function() {
    filter(geo_rent_by_burden,
           GEOID == geo()$GEOID,
           sample_size > 10)
  }
  
  # filter data for age chart
  geo_age <- function() {
    filter(geo_age_by_burden,
           GEOID == geo()$GEOID,
           sample_size > 10)
  }
  
  # filter data for raceth chart
  geo_raceth <- function() {
    filter(geo_raceth_by_burden,
           GEOID == geo()$GEOID,
           sample_size > 10)
  }
  
  
  #===============================================================================#
  # TEXT
  #===============================================================================#
  
  output$renterHHCountText <- renderText({
    
    burden_df <- geo_sector_burden()

    place_name <- geo()$NAME
    renter_share <- -1  
    num_renter <- -1  
    
    
    glue("{renter_share} of {place_name}'s households are renters.",
         "Those renter households are home to {num_renter} people.")
    
  })
  
  
  output$vulnerableWorkerText <- renderText({
    
    n_sector <- geo_sector_burden() %>%
      ungroup() %>%
      filter(sector %in% vulnerable_sectors) %>% 
      select(n_sector)
      
    n_sector_burdened <- geo_sector_burden() %>%
      ungroup() %>%
      filter(cost_burdened == "Rent burdened",
             sector %in% vulnerable_sectors) %>% 
      select(n_sector_burdened) 

    n_sector_nonburdened <- geo_sector_burden() %>%
      ungroup() %>%
      filter(cost_burdened == "Not burdened",
             sector %in% vulnerable_sectors) %>% 
      select(n_sector_burdened) 
      
    
    glue("{format(sum(n_sector), big.mark = ',')} adults in renter households work in an industry immediately impacted by responses to COVID 19. ", 
         "{format(sum(n_sector_burdened), big.mark = ',')} of those workers were in cost-burdened households that were already struggling to pay rent before the crisis, ",
         "while another {format(sum(n_sector_nonburdened), big.mark = ',')} are likely to be newly struggling due to income or job losses.")
    
  })
  
  
  output$shareVulnerableText <- renderText({
    
    vul_shares <- geo_renter_hholds() %>% 
      filter(is_vulnerable == 1) %>%
      ungroup() %>% 
      select(share_vulnerable)
      
    glue("Between already rent burdened workers and those newly struggling, ",
         "{round(sum(vul_shares) * 100, 1)}% of renter households in California are vulnerable to job or income losses because of COVID-19.
")
    
  })
  
  output$typicalRent <- renderText({
    
    total_rent <- geo_rent() %>% 
      filter(is_vulnerable) %>% 
      ungroup() %>% 
      select(total_rent)
    
    median_rent <- -1
    
    glue("These households typically pay ${median_rent} a month in rent. ", "
    To fully cover the rental costs of these households would require",
    "${format(round(total_rent, -6), big.mark = ','} a month.")
  })
  
  
  #===============================================================================#
  # PLOTS
  #===============================================================================#
  
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
