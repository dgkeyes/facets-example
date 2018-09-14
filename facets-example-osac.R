# Packages ----------------------------------------------------------------

library(tidyverse)
library(scales)
library(extrafont)
library(hrbrthemes)


# Get data ----------------------------------------------------------------

regions <- read_csv("regions.csv")


# Colors ------------------------------------------------------------------

tfff_dark_green <- "#265142"
tfff_light_green <- "#B5CC8E"
tfff_orange <- "#e65100"
tfff_yellow <- "#FBC02D"
tfff_blue <- "#283593"
tfff_red <- "#B71C1C"
tfff_brown <- "#51261C"
tfff_dark_gray <- "#545454"
tfff_medium_gray <- "#a8a8a8"
tfff_light_gray <- "#eeeeee"


# Themes ------------------------------------------------------------------

tfff_theme <- theme_ipsum(base_family = "Calibri",
                          base_size = 10) +
     theme(legend.position = "none",
           axis.title = element_blank(),
           panel.grid.minor.x = element_blank(),
           panel.grid.minor.y = element_blank())


tfff_fill_colors <- scale_fill_manual(values = rev(c(tfff_dark_green,
                                                     tfff_light_green,
                                                     tfff_yellow,
                                                     tfff_orange,
                                                     tfff_brown,
                                                     tfff_blue,
                                                     tfff_red))) 


# Plot - not facetted ------------------------------------------------------------

regions_plot <- ggplot(data = regions, aes(x = year_label, 
                y = n, 
                fill = region)) +
     geom_area() +
     tfff_theme +
     tfff_fill_colors +
     labs(title = "Number of applicants by region",
          fill = "")

regions_plot + 
     theme(legend.position = "right") 

ggsave("regions-plot.png",
       height = 5,
       width = 8)


# Plot - facetted ----------------------------------------------------------------

regions_plot +
     facet_grid(cols = vars(region)) +
     theme(axis.title = element_blank(),
           axis.text = element_text(size = 8))

ggsave("regions-plot-facetted.png",
       height = 5,
       width = 10)
