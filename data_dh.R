rm(list = ls())

library(tidyverse)
library(readxl)
library(ggrepel)
library(lubridate)
library(brms)
library(tidybayes)

# raw data
wm <- read_excel("Duttaphrynus hololius.xls", 
                 sheet = "weekly_dat")
# for fig 5
dh <- wm %>%
  mutate(date= as.Date(date,format="%m/%d/%Y")) %>%
  arrange(date) %>% 
  mutate(Month= lubridate::month(date))

adult <- dh %>% filter(stage== 'adult') %>% filter(stage_code>0) %>% 
  group_by(Month, stage) %>% 
  count(stage, name = 'count')

egg <- dh %>% filter(stage== 'egg') %>% filter(stage_code>0) %>% 
  group_by(Month, stage) %>% 
  count(stage, name = 'count') 

tadpole <- dh %>% filter(stage== 'tadpole') %>% filter(stage_code>0) %>% 
  group_by(Month, stage) %>% 
  count(stage, name = 'count')

montlycount <- bind_rows(adult, egg, tadpole) %>% 
  rename(Month=Month) %>% 
  rename(Stage=stage) %>% 
  rename(Count=count)


climate <- read_excel("Duttaphrynus hololius.xls", 
                      sheet = "TeHuRa")

names(climate)

climate <- climate %>%
  mutate(month= as.Date(date,format="%m/%d/%Y")) %>% 
  arrange(month) %>% 
  mutate(Month= lubridate::month(month)) %>% 
  mutate(rain= round(rain, 2),
         temp= round(temp, 2),
         humidity= round(humidity, 2)) #%>% 
  # group_by(Month) %>% 
  # summarise(Rain= sum(rain),
  #           Temp= mean(temp),
  #           Humd= mean(humidity)) %>% 
  # mutate(Rain= round(Rain, 2),
  #        Temp= round(Temp, 2),
  #        Humd= round(Humd, 2))


# prepare data 
stages <- wm %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  arrange(date) %>%
  mutate(BP_ID = factor(BP_ID)) %>%
  select(date, BP_ID, visit, stage, stage_code) %>%
  pivot_wider(names_from = 'stage', # adult, egg, tadpole
              values_from = 'stage_code', # stage present = 1
              values_fill = 0)

wm_new <- wm %>% 
  mutate(date= as.Date(date,format="%m/%d/%Y")) %>%
  arrange(date) %>% 
  mutate(week= format(date, "%W")) %>% 
  select(-visit, -stage, - stage_code) %>% # add observation of features as present absent
  mutate(BP_ID = factor(BP_ID),
         agriculture = if_else(agriculture == 0, "absent", "present"),
         agriculture = factor(agriculture, levels = c("absent", "present")),
         grazing = if_else(grazing == 0, "no", "yes"),
         grazing = factor(grazing, levels = c("no", "yes")),
         silt = if_else(silt == 0, "no", "yes"),
         silt = factor(silt, levels = c("no", "yes")),
         fracture = if_else(fracture == 0, "no", "yes"),
         fracture = factor(fracture, levels = c("no", "yes"))
  ) %>% 
  left_join(stages, multiple = 'all') %>% # add observations of stages as yes no
  mutate(
    adult = if_else(adult == 0, "no", "yes"),
    adult = factor(adult, levels = c("no", "yes")),
    egg = if_else(egg == 0, "no", "yes"),
    egg = factor(egg, levels = c("no", "yes")),
    tadpole = if_else(tadpole == 0, "no", "yes"),
    tadpole = factor(tadpole, levels = c("no", "yes")),
    visit= factor(visit) # week of the visit
  ) #%>% View()

levels(wm_new$agriculture)
levels(wm_new$fracture)
levels(wm_new$silt)

levels(wm_new$adult)
levels(wm_new$egg)
levels(wm_new$tadpole)



depth= c(15, 4.5, 5, 9, 8, 12, 5)
min(depth)
max(depth)
sd(depth)
mean(depth)

msl <- wm <- read_excel("Duttaphrynus hololius.xls", 
                        sheet = "Dutta_details")

