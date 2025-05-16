library(tidyverse)
library(haven)
library(sf)
library(tigris)

# load fox dataset and district shapefile
fox_df = read_dta("./University/MSc/Thesis/data/FoxNewsData.dta")
district_shapes = read_sf("./University/MSc/Thesis/data/districtShapes")
state_abbrev = read_csv("./University/MSc/Thesis/data/state_abbrev.csv")


district_shapes %>%
  # shift_geometry so Alaska and Hawaii are nicely placed in the map
  shift_geometry() %>%
  left_join(state_abbrev, by = c("STATENAME"="State")) %>% 
  # States with single congressional district are labelled as 0 in district_shapes
  # Relabel as 1 so consistent with fox_df
  mutate(DISTRICT = if_else(DISTRICT=="0", "1", DISTRICT), 
         statecd = paste0(Abbreviation, ".", DISTRICT)) %>% 
  left_join(fox_df, by="statecd") %>% 
  # Relabel data for interpretability 
  mutate(fox_1998 = as_factor(case_when(
    subf1998 > 0 ~ "Fox in 1998", 
    subf1998 == 0 & subf2000 > 0 ~ "No Fox in 1998 | Fox in 2000", 
    subf1998 == 0 & subf2000 == 0 ~ "No Fox in 1998 or 2000", 
    TRUE ~ "No Data")),
    fox_1998 = factor(fox_1998, levels=c("Fox in 1998", "No Fox in 1998 | Fox in 2000", 
                                         "No Fox in 1998 or 2000", "No Data"))
  ) %>% 
  # Create map of US congressional districts by access to Fox News
  ggplot(aes(fill=fox_1998)) + 
  geom_sf() + 
  coord_sf(crs = 5070, datum=NA) + 
  scale_fill_manual(values = c("Fox in 1998"="seagreen", 
                                 "No Fox in 1998 | Fox in 2000"="yellow", 
                                 "No Fox in 1998 or 2000"="red4", 
                                 "No Data"="darkgrey")) +
  labs(fill="Fox Access 1998") + 
  theme_minimal(base_size=14)+
  theme(plot.margin=unit(c(0,0,0,0), "null"), 
        legend.position="bottom")


ggsave("./University/MSc/Thesis/outputs/fox_map2.png")