library(tidyverse)
library(haven)
library(rstatix)

#-----------Functions------------
# Create function that gets the second largest element of a list
get_second <-  function(x) {
  u <- unique(x)
  sort(u, decreasing = TRUE)[2L]
}

#--------------Read Data-----------

# Load dataset with DW-NOMINATE scores for Cogngress members
nominate = read_csv("./University/MSc/Thesis/data/nominate.csv") %>% 
  #Filter only to congresses of interest and the House
  filter(congress >= 102, congress <= 108, chamber=="House") %>% 
  #Select columns of interest (Congress, unique id, ideology measures, and district ids)
  select(congress, icpsr, nominate_dim1,
         nokken_poole_dim1, nominate_dim2, 
         nokken_poole_dim2, state_abbrev, district_code, bioname) %>% 
  # Create state_district code to match with other datasets
  mutate(district_code = if_else(district_code==0, 1, district_code),
    state_district = paste0(state_abbrev, ".", district_code))

# Load in dataset with campaign finance ideology scores
spending = read_csv("./University/MSc/Thesis/data/spending_est.csv") %>% 
  # filter to house seats in the years of interest and only winning candidates
  # (i.e., candidates that became House members)
  filter(seat=="federal:house", cycle %in% c(1988, 1990, 1992, 1994, 1996, 1998, 2000, 2002), 
         winner=="W") %>% 
  # match each election cycle with its subsequent Congress
  mutate(congress = case_match(cycle, 
                               1988 ~ 101, 
                               1990 ~ 102, 
                               1992 ~ 103, 
                               1994 ~ 104, 
                               1996 ~ 105, 
                               1998 ~ 106, 
                               2000 ~ 107, 
                               2002 ~ 108)) %>%
  rowwise() %>% 
  # create state_district to match with other datasets
  mutate(state1 = paste(unlist(strsplit(district, split=""))[1:2], collapse=""),
         # Extract district from variable of the form CA01
         district1 = as.numeric(paste(unlist(strsplit(district, split=""))[3:4], collapse="")), 
         state_district = paste0(state1, ".", district1))

# Readi in data with ideology estiamtes from Word2Vec model
pca = read_csv("./University/MSc/Thesis/data/pca_102_108.csv") %>%
  # Recode 0 districts as 1
  mutate(district = if_else(district==0, 1, district)) %>%
  #create state district variable to match with other datasets
  mutate(state_district = paste0(state, ".", district))

# Read in data on Fox exposure by district
fox = read_dta("./University/MSc/Thesis/data/FoxNewsData.dta")%>% 
  # Create binary variable for whether district had fox (if number of subscribers >0, Fox exposure=1)
  mutate(
    fox_1998 = case_when(
      subf1998 > 0 ~ 1, 
      subf1998 == 0 ~ 0, 
      TRUE ~ NA_real_),
    fox_2000 = case_when(
      subf2000 > 0 ~ 1, 
      subf2000 == 0 ~ 0, 
      TRUE ~ NA_real_), 
    fox_2003 = case_when(
      subf2003 > 0 ~ 1, 
      subf2003 == 0 ~ 0, 
      TRUE ~ NA_real_
    )
  )

# Read in dataset with additional district characteristics 
descr = read_dta("./University/MSc/Thesis/data/DistrictData.dta")

# Read csv with map from NOMINATE icpsr to Congrestext unique_id
id_map = read_csv("./University/MSc/Thesis/data/id_map.csv")

# Read in data with election results to the house 
house_results = read_csv("./University/MSc/Thesis/data/1976-2020-house.csv") %>% 
  # Filter to election years of interest
  filter(year %in% c(1992, 1994, 1996, 1998, 2000, 2002)) %>% 
  # Calculate share of vote won by each candidate
  mutate(share = candidatevotes/totalvotes) %>% 
  # calculate the winner's share and the second place share for each disrict
  group_by(year, state, district) %>% 
  mutate(winner_share = max(share), second_share=get_second(share)) %>% 
  # Filter only to winners
  filter(share==winner_share) %>% 
  ungroup() %>% 
  # Calculate margin, create state_district variable, and match each election to subsequent congress
  mutate(margin = winner_share - second_share,
         district = if_else(as.numeric(district)==0, 1, as.numeric(district)), 
         state_district = paste0(state_po, ".", district), 
         congress=case_match(
           year, 
           1992 ~ 103, 
           1994 ~ 104, 
           1996 ~ 105, 
           1998 ~ 106, 
           2000 ~ 107, 
           2002 ~ 108
         ))
  

#-----Descriptive Stats-----

# Get number of districts with access to Fox in 1998
fox %>% 
  group_by(fox_1998) %>% 
  summarize(n=n())

# Get number of districts with access to Fox in 2000
fox %>% 
  group_by(fox_2000) %>% 
  summarize(n=n())

# Get number of districts with access to Fox in 2003
fox %>% 
  group_by(fox_2003) %>% 
  summarize(n=n())

# Create chart of pre-treatment representative partisanship by Fox acesss
fox %>% 
  #Join fox data with data about districts
  left_join(descr, by="statecd") %>% 
  # Filter out districts with no data on Fox access
  filter(!is.na(fox_1998)) %>% 
  select(fox_1998, rep104, rep105) %>% 
  pivot_longer(cols=c(rep104, rep105)) %>% 
  mutate(congress=if_else(name=="rep104", "104th", "105th")) %>% 
  group_by(fox_1998, congress) %>% 
  summarise(Republican = mean(value)) %>% 
  mutate(Democrat = 1-Republican,
         fox_1998 = if_else(fox_1998==1, "Fox Available in 1998", "Fox Not Available in 1998")) %>%
  pivot_longer(cols=c(Republican, Democrat)) %>%
  ggplot(aes(x=as.factor(fox_1998), value, fill=name)) + 
  geom_col() + 
  scale_fill_manual(values=c("Democrat"="blue", "Republican"="red")) +
  xlab("Fox Access 1998") + 
  ylab("Percent of Legislators from Each Party") + 
  labs(fill="Party") + 
  theme_bw(base_size =16) + 
  facet_wrap(~congress) + 
  scale_x_discrete(labels = label_wrap(20)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position="bottom")

ggsave("./University/MSc/Thesis/outputs/party_by_fox.png")


#-------Parallel Trends-----
# Create chart of pre-treatment trends in speech ideology 
pca %>% 
  # pc1 = speech ideology 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress<=105, !is.na(fox_1998)) %>% 
  group_by(congress, fox_1998) %>% 
  summarise(pc1=mean(pc1, na.rm=TRUE)) %>% 
  mutate(fox_1998=case_match(fox_1998, 1~"Fox in 1998", 0~"No Fox in 1998")) %>% 
  ggplot(aes(x=congress,y=pc1, group=fox_1998, colour=fox_1998)) +
  geom_line(linewidth=2) + 
  xlab("Congress") + 
  ylab("First Principal Component (Left-Right Ideology)") + 
  labs(colour="Fox Access 1998") + 
  scale_colour_manual(values=c("Fox in 1998"="seagreen", 
                               "No Fox in 1998"="red4")) + 
  theme_bw(base_size =15) +
  theme(legend.position="bottom")
ggsave("./University/MSc/Thesis/outputs/parallel_trends.png")


#----Validate Metric-----

# Join speech ideology data with nominate data and get correlation 
pca %>% 
  right_join(id_map) %>% 
  inner_join(nominate, by=c("congress", "icpsr", "state_district")) %>%
  summarise(cor = cor(pc1, nominate_dim1), 
            rank_cor = cor(pc1, nominate_dim1, method="spearman"))

# Join speech ideology data with nominate data and get correlation with nokken-poole
pca %>% 
  right_join(id_map) %>% 
  inner_join(nominate, by=c("congress", "icpsr", "state_district")) %>%
  summarise(cor = cor(pc1, nokken_poole_dim1, use="pairwise.complete"), 
            rank_cor = cor(pc1, nokken_poole_dim1 , method="spearman", use="pairwise.complete"))

# Join speech ideology data with spending ideology data and get correlation
pca  %>% 
  inner_join(spending, by=c("congress", "state_district")) %>% 
  summarise(cor = cor(pc1, irt.cfscore, use="pairwise.complete.obs"), 
            rank_cor = cor(pc1, irt.cfscore, use="pairwise.complete.obs", method="spearman"))

# Plot speech ideology vs pc2 (north-south) for each congress with colour by party
pca %>% 
  filter(congress>102) %>% 
  mutate(congress = case_match(congress,
                               102 ~ "102nd", 
                               103 ~ "103rd", 
                               104 ~ "104th", 
                               105 ~ "105th", 
                               106 ~ "106th", 
                               107 ~ "107th", 
                               108 ~ "108th"), 
         party = case_match(party, 
                            "D"~"Democrat", 
                            "R"~"Republican", 
                            "I"~"Independent")) %>%
  ggplot(aes(x=pc1, y=pc2, colour=party)) + 
  geom_point() + 
  facet_wrap(~congress) +
  scale_colour_manual(values = c("Democrat"="blue", "Republican"="red", "Independent"="green")) + 
  labs(colour="Party") + 
  xlab("First Principal Component (Left-Right Ideology)") + 
  ylab("Second Principal Component (North-South Geography)") + 
  theme_bw(base_size = 14) + 
  theme(legend.position="bottom")+
  guides(color  = guide_legend(override.aes = list(size = 5)))

ggsave("./University/MSc/Thesis/outputs/pc_plot_102_108.png")

# Plot speech ideology against DW-NOMINATE dimension 1
pca %>% 
  right_join(id_map) %>% 
  inner_join(nominate, by=c("congress", "icpsr", "state_district")) %>% 
  filter(congress>102) %>% 
  mutate(congress = case_match(congress,
                               102 ~ "102nd", 
                               103 ~ "103rd", 
                               104 ~ "104th", 
                               105 ~ "105th", 
                               106 ~ "106th", 
                               107 ~ "107th", 
                               108 ~ "108th"), 
         party = case_match(party, 
                            "D"~"Democrat", 
                            "R"~"Republican", 
                            "I"~"Independent")) %>%
  ggplot(aes(x=pc1, y=nominate_dim1, colour=party)) + 
  geom_point() + 
  facet_wrap(~congress) + 
  scale_colour_manual(values = c("Democrat"="blue", "Republican"="red", "Independent"="green")) + 
  labs(colour="Party") + 
  xlab("First Principal Component (Speech Left-Right Ideology)") + 
  ylab("DW-NOMINATE Dimension 1 (Vote Left-Right Ideology)") + 
  theme_bw(base_size = 14) + 
  theme(legend.position="bottom")+
  guides(color  = guide_legend(override.aes = list(size = 5)))

ggsave("./University/MSc/Thesis/outputs/pc_vs_nominate_plot.png")

# Get list of most moderate Democrats and Republicans on speech ideology measure
pca %>% 
  filter(party %in% c("D", "R")) %>%
  group_by(party, congress) %>% 
  mutate(mod = if_else(
    party == "D", 
    max(pc1), 
    min(pc1)
  )) %>% 
  filter(pc1 == mod)

# Get list of most extreme Democrats and Republicans on speech ideology measure
pca %>% 
  filter(party %in% c("D", "R")) %>%
  group_by(party, congress) %>% 
  mutate(ext = if_else(
    party == "D", 
    min(pc1), 
    max(pc1)
  )) %>% 
  filter(pc1 == ext)
  
  
#-----DiD------
# DiD model to test Hypothesis 1
model1 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  mutate(t = if_else(congress == 105, 0, 1)) %>%
  lm(pc1 ~ fox_1998 + t + fox_1998*t,
     data=.) 

# Triple DiD model to test Hypothesis 2
model2 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  left_join(house_results, by = c("state_district", "congress")) %>% 
  mutate(t = if_else(congress == 105, 0, 1), 
         # Code representative as vulnerable if they won their last election by less than 5%
         v = if_else(margin <= 0.05, 1, 0)) %>%
  lm(pc1 ~ fox_1998 + t + v + 
       fox_1998*t + fox_1998*v + t*v+ 
       fox_1998*t*v,
     data=.) 

#----Robustness----
#Robustness check adding control variables (H1)
rob1 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  left_join(descr, by =c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  mutate(t = if_else(congress == 105, 0, 1)) %>%
  lm(pc1 ~ fox_1998 + t + fox_1998*t + 
       city + coast + popsqmi + union + shrfb + shr65 + shrblk + shrenr + shrurb + shrunem + loginc,
     data=.) 

#Robustness check adding control variables (H2)
rob2 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  left_join(descr, by =c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  left_join(house_results, by = c("state_district", "congress")) %>% 
  mutate(t = if_else(congress == 105, 0, 1), 
         v = if_else(margin <= 0.05, 1, 0)) %>%
  lm(pc1 ~ fox_1998 + t + v + 
       fox_1998*t + fox_1998*v + t*v+ 
       fox_1998*t*v + 
       city + coast + popsqmi + union + shrfb + shr65 + shrblk + shrenr + shrurb + shrunem + loginc,
     data=.) 

#Robustness check using continuous measure of Fox exposure (share of population subscribed=subrf1998)
rob3 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  mutate(t = if_else(congress == 105, 0, 1)) %>%
  lm(pc1 ~ subrf1998 + t + subrf1998*t,
     data=.) 

#Robustness check using Fox exposure in 2000 as independent variable
rob4 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(106, 107)) %>% 
  mutate(t = if_else(congress == 106, 0, 1)) %>%
  lm(pc1 ~ fox_2000 + t + fox_2000*t,
     data=.) 

#Robustness check using Fox exposure in 2003 as independent variable
rob5 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(107, 108)) %>% 
  mutate(t = if_else(congress == 107, 0, 1)) %>%
  lm(pc1 ~ fox_2003 + t + fox_2003*t,
     data=.) 

#Robustness check using 2.5% margin as vulnerability threshold
rob6 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  left_join(house_results, by = c("state_district", "congress")) %>% 
  mutate(t = if_else(congress == 105, 0, 1), 
         v1 = if_else(margin <= 0.025, 1, 0)) %>%
  lm(pc1 ~ fox_1998 + t + v1 + 
       fox_1998*t + fox_1998*v1 + t*v1+ 
       fox_1998*t*v1,
     data=.) 

#Robustness check using 7.5% margin as vulnerability threshold
rob7 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  left_join(house_results, by = c("state_district", "congress")) %>% 
  mutate(t = if_else(congress == 105, 0, 1), 
         v2 = if_else(margin <= 0.075, 1, 0)) %>%
  lm(pc1 ~ fox_1998 + t + v2 + 
       fox_1998*t + fox_1998*v2 + t*v2+ 
       fox_1998*t*v2,
     data=.) 

#Robustness check using 10% margin as vulnerability threshold
rob8 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>% 
  filter(congress %in% c(105, 106)) %>% 
  left_join(house_results, by = c("state_district", "congress")) %>% 
  mutate(t = if_else(congress == 105, 0, 1), 
         v3 = if_else(margin <= 0.1, 1, 0)) %>%
  lm(pc1 ~ fox_1998 + t + v3 + 
       fox_1998*t + fox_1998*v3 + t*v3+ 
       fox_1998*t*v3,
     data=.) 

#Robustness check using only Democrats
rob8 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>%
  filter(party=="D") %>% 
  filter(congress %in% c(105, 106)) %>% 
  mutate(t = if_else(congress == 105, 0, 1)) %>%
  lm(pc1 ~ fox_1998 + t + fox_1998*t,
     data=.) 

#Robustness check using only Republicans
rob9 = pca %>% 
  select(pc1, pc2, party, state_district, congress) %>% 
  inner_join(fox, by = c("state_district"="statecd")) %>%
  filter(party=="R") %>% 
  filter(congress %in% c(105, 106)) %>% 
  mutate(t = if_else(congress == 105, 0, 1)) %>%
  lm(pc1 ~ fox_1998 + t + fox_1998*t,
     data=.) 