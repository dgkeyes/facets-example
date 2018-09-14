
# Packages ----------------------------------------------------------------

library(tidyverse)
library(gghighlight)
library(ggridges)
library(scales)
library(janitor)


# Get data ----------------------------------------------------------------

us_favorable <- read_csv("https://raw.githubusercontent.com/rudeboybert/fivethirtyeight/master/data-raw/trump-world-trust/TRUMPWORLD-us.csv") %>% 
     clean_names() %>%
     select(-avg) %>% 
     gather(key = "country", value = "approval_rating", -year) %>% 
     mutate(country = str_to_title(country)) %>% 
     filter(year == 2015 | year == 2017) %>% 
     filter(!is.na(approval_rating)) %>% 
     group_by(country) %>% 
     add_count() %>% 
     filter(n == 2) %>% 
     ungroup() %>% 
     mutate(country = str_replace(country,
                                  "Uk",
                                  "UK")) %>% 
     mutate(country = str_replace(country,
                                  "South_africa",
                                  "South Africa")) %>% 
     mutate(country = str_replace(country,
                                  "South_korea",
                                  "South Korea")) %>% 
     mutate(year = year - 2000) %>% 
     mutate(year = as.character(year))

library(ggthemes)

ggplot(data = us_favorable,
       aes(x = year, 
           y = approval_rating, 
           group = country)) +
     geom_line() +
     geom_point() +
     # facet_wrap(~country, ncol = 8) +
     scale_y_continuous(limits = c(0, 100)) +
     theme_minimal() +
     theme(panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank(),
           # panel.background = element_rect(fill = "#eeeeee"),
           # panel.border = element_rect(color = "#eeeeee",
           #                             fill = NA),
           strip.text = element_text(face = "bold"),
           # panel.grid.major.y = element_blank(),
           axis.title = element_blank(),
           axis.text.x = element_blank())
     