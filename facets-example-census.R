
# Packages ----------------------------------------------------------------

library(tidyverse)
library(gghighlight)
library(ggridges)
library(scales)
library(janitor)
library(tidycensus)


# Get data ----------------------------------------------------------------


latino_pop_2000 <- get_decennial(geography = "state", 
                                 variables = "H007001",
                                 year = 2000) %>% 
     mutate(year = 2000)

latino_pop_2010 <- get_decennial(geography = "state", 
                                 variables = "H007001",
                                 year = 2010) %>% 
     mutate(year = 2010)

latino_pop <- bind_rows(latino_pop_2000,
                        latino_pop_2010) %>% 
     clean_names() %>% 
     filter(name != "Puerto Rico" & name != "District of Columbia")


# Plot --------------------------------------------------------------------

latino_pop_plot <- ggplot(latino_pop, aes(x = year, 
                                          y = value / 1000,
                                          group = name,
                                          fill = name)) +
     geom_area() +
     theme(legend.position = "none",
           axis.title = element_blank(),
           axis.text.x = element_blank(),
           panel.grid = element_blank(),
           axis.ticks = element_blank(),
           strip.text = element_text(face = "bold"),
           strip.background = element_rect(fill = "#eeeeee",
                                           color = "#eeeeee"),
           panel.background = element_rect(fill = "#eeeeee",
                                           color = "#eeeeee")) +
     scale_x_continuous(breaks = c(2000, 2010)) 

# Facetted plot --------------------------------------------------------------------

latino_pop_plot + 
     facet_wrap(~name, 
                ncol = 10)
