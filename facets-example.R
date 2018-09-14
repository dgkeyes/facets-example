
# Packages ----------------------------------------------------------------

library(tidyverse)
library(gghighlight)
library(ggridges)
library(scales)

load("faketucky.rda")




# GPA by race/ethnicity ----------------------------------------------------

gpa <- faketucky_20160923 %>% 
     select(avg_gpa_hs, 
            race_ethnicity) %>% 
     filter(race_ethnicity != "")

gpa %>% 
     pull(race_ethnicity) %>% 
     unique()

ggplot(data = gpa, aes(x = avg_gpa_hs,
                       fill = race_ethnicity)) +
     geom_histogram() +
     theme_minimal() +
     theme(legend.position = "none") +
     facet_wrap(~race_ethnicity,
                scale = "free",
                ncol = 5)

# GPA by district ----------------------------------------------------

gpa_by_dist <- faketucky_20160923 %>% 
     select(avg_gpa_hs, 
            first_dist_name,
            first_dist_code) %>% 
     filter(first_dist_code < 141)


ggplot(data = gpa_by_dist, aes(x = avg_gpa_hs)) +
     geom_histogram() +
     theme_void() +
     theme(legend.position = "none") +
     facet_wrap(~first_dist_name,
                scale = "free",
                ncol = 5)



# Graduation --------------------------------------------------------------



frpl <- faketucky_20160923 %>% 
     select(avg_gpa_hs, 
            frpl_ever_in_hs)

ggplot(data = frpl, aes(x = avg_gpa_hs,
                       fill = frpl_ever_in_hs)) +
     geom_histogram() +
     # gghighlight() +
     theme_minimal() +
     facet_wrap(~frpl_ever_in_hs,
                ncol = 1)



# College selectivity -----------------------------------------------------

college_selectivity <- faketucky_20160923 %>% 
     group_by(first_dist_name,
            ihe_barrons_rank_2013) %>% 
     count() %>% 
     filter(ihe_barrons_rank_2013 != "") %>% 
     group_by(first_dist_name) %>% 
     mutate(pct = prop.table(n)) %>% 
     mutate(ihe_barrons_rank_2013 = factor(ihe_barrons_rank_2013, 
                                           levels = c("Non Competitive",
                                                      "Less Competitive",
                                                      "Competitive",
                                                      "Very Competitive",
                                                      "Highly Competitive",
                                                      "Most Competitive")))

ggplot(college_selectivity, 
       aes(x = ihe_barrons_rank_2013,
           y = pct,
           fill = ihe_barrons_rank_2013)) +
     geom_col() +
     coord_flip() +
     theme_minimal() +
     theme(axis.title = element_blank(),
           panel.grid = element_blank(),
           legend.position = "none") +
     # facet_wrap(~first_dist_name, ncol = 10) +
     scale_y_continuous(labels = percent_format(accuracy = 1)) +
     scale_fill_brewer(type = "seq")

ggsave("college-selectivity.pdf",
       height = 40,
       width = 40)


# Stacked

ggplot(college_selectivity, 
       aes(x = first_dist_name,
           y = n,
           fill = ihe_barrons_rank_2013)) +
     geom_col(position = "fill") +
     coord_flip() +
     theme_minimal() +
     theme(axis.title = element_blank(),
           panel.grid = element_blank(),
           legend.position = "top") +
     # facet_wrap(~first_dist_name, ncol = 10) +
     scale_y_continuous(labels = percent_format(accuracy = 1)) +
     scale_fill_brewer(type = "seq")

ggsave("college-selectivity.pdf",
       height = 40,
       width = 20)
