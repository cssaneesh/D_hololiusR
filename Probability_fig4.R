# Used a multivariate regression, where multiple dependent variables are modeled 
# simultaneously using the same set of predictors.

source('data_dh.R')

str(wm_new)
names(wm_new)

# exploring data

wm_new %>% 
  mutate(visit= as.factor(visit)) %>% 
  distinct(visit)

wm_new %>% 
  ggplot(aes(x= visit,fill= adult))+
  geom_bar(position = 'stack')

wm_new %>% 
  ggplot(aes(x= visit,fill= egg))+
  geom_bar(position = 'stack')

wm_new %>% 
 ggplot(aes(x= visit,fill= tadpole))+
  geom_bar(position = 'stack')

# Models----
# Agriculture vs. adult, egg and tadpoles----
# agriculture <- brm(mvbind(adult, egg, tadpole) ~ agriculture, data = wm_new,
#                   family = bernoulli(link = "logit"),
#                   warmup = 1000,
#                   iter = 2000,
#                   chains = 4,
#                   cores=2)
# save(agriculture, file= 'agriculture.Rdata')

load('agriculture.Rdata')
pp_check(agriculture, resp = 'adult') + ggtitle('adult ~ agriculture')
pp_check(agriculture, resp = 'egg') + ggtitle('egg ~ agriculture')
pp_check(agriculture, resp = 'tadpole') + ggtitle('tadpole ~ agriculture')


# Model convergence
mcmc_plot(agriculture,
          type= 'trace') + ggtitle('(adult, egg, tadpole) ~ agriculture')

mcmc_plot(agriculture, 
          type = "acf_bar") + ggtitle('(adult, egg, tadpole) ~ agriculture')

mcmc_plot(agriculture,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
          geom_vline(xintercept = 0, col = 'red', lty='dotted', linewidth= 1)

summary(agriculture)
conditional_effects(agriculture)


exp(fixef(agriculture))
exp(fixef(agriculture)[,-2]) # [,-2] to exclude Est.Error

# The probability of observing adult frogs are about 76% higher  
# when agriculture is present compared to when it's absent. 

# The probability of observing eggs are about 100% higher  
# when agriculture is present compared to when it's absent.

# The probability of observing tadpole are about 90% higher  
# when agriculture is present compared to when it's absent.

# fracture vs. adult, egg and tadpoles-----

# fracture <- brm(mvbind(adult, egg, tadpole) ~ fracture, data = wm_new,
#                    family = bernoulli(link = "logit"),
#                    warmup = 1000, 
#                    iter = 2000, 
#                    chains = 4,
#                    cores=2)
# save(fracture, file= 'fracture.Rdata')

load('fracture.Rdata')
pp_check(fracture, resp = 'adult') + ggtitle('adult ~ fracture')
pp_check(fracture, resp = 'egg') + ggtitle('egg ~ fracture')
pp_check(fracture, resp = 'tadpole') + ggtitle('tadpole ~ fracture')

# Model convergence
mcmc_plot(fracture,
          type= 'trace') + ggtitle('(adult, egg, tadpole) ~ fracture')

mcmc_plot(fracture, 
          type = "acf_bar") + ggtitle('(adult, egg, tadpole) ~ fracture')

mcmc_plot(fracture,
          type = "areas",
          prob = 0.95) + # see if predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red', lty='dotted', linewidth= 1)

summary(fracture)
conditional_effects(fracture, effects = 'fracture')

exp(fixef(fracture)[,-2]) # [,-2] to exclude Est.Error


# Our analysis suggests that there may be a relationship between 
# fractures between observing egg string and they turn into tadpoles,
# but we cannot be certain of the exact magnitude of this effect due 
# to the wide range of uncertainty in our estimates.


# So it is important to continue research and monitoring to better understand the potential 
# impacts of the fracture and silt deposition variable on the egg laying and tadpoles. 
# By doing so, we can make more informed decisions and take appropriate conservation actions to protect the populations of DH.


# silt <- brm(mvbind(adult, egg, tadpole) ~ silt, data = wm_new,
#             family = bernoulli(link = "logit"),
#             warmup = 1000,
#             iter = 2000,
#             chains = 4,
#             cores=2)
# save(silt, file= 'silt.Rdata')

load('silt.Rdata')
pp_check(silt, resp = 'adult', ndraws = 20) + ggtitle('adult ~ silt')
pp_check(silt, resp = 'egg') + ggtitle('egg ~ silt')
pp_check(silt, resp = 'tadpole') + ggtitle('tadpole ~ silt')

# Model convergence
mcmc_plot(silt,
          type= 'trace') + ggtitle('(adult, egg & tadpole) ~ silt')

mcmc_plot(silt, 
          type = "acf_bar") + ggtitle('(adult, egg & tadpole) ~ silt')

mcmc_plot(silt,
          type = "areas",
          prob = 0.95) + # see predictors CI contain zero.
  geom_vline(xintercept = 0, col = 'red', lty='dotted', linewidth= 1)

summary(silt)
conditional_effects(silt, effects = 'silt')

exp(fixef(silt)[,-2])


# plot----
# Get the posterior distribution of the fixed effects
load('agriculture.Rdata')

fixef_agri <- exp(fixef(agriculture)[,-2]) 

fixef_agri_df <- data.frame(fixef_agri)

fixef_agri_df$res_var <- rownames(fixef_agri_df) # a new col for response var
names(fixef_agri_df)

fixef_agri_df$res_var

fixef_agri_df <- fixef_agri_df %>%
  mutate(Feature= as.factor('Agriculture')) %>% 
  filter(!res_var %in% c('adult_Intercept',
                         'egg_Intercept',
                         'tadpole_Intercept')) %>% # remove intercepts
  mutate(
    res_var = recode( # rename reponse variable
      res_var,
      adult_agriculturepresent = 'Adult',
      egg_agriculturepresent = 'Egg',
      tadpole_agriculturepresent = 'Tadpole'
    )
  ) %>%
  mutate(Probability = Estimate / (1 + Estimate)) # probability in case wanted

# exploratory plot
ggplot() + geom_point(data = fixef_agri_df, aes(x = res_var, y = Estimate), size = 3) +
  geom_errorbar(
    data = fixef_agri_df,
    aes(x = res_var,
        ymin = Q2.5,
        ymax = Q97.5),
    linewidth = 1.3,
    width = 0.1
  ) +
  labs(y = "Probability", x = "Agriculture")


# Get the posterior distribution of the fixed effects
load('fracture.Rdata')

exp(fixef(fracture))
exp(fixef(fracture)[,-2])


fixef_frac <- exp(fixef(fracture)[,-2])

fixef_frac_df <- data.frame(fixef_frac)

fixef_frac_df$res_var <- rownames(fixef_frac_df) # a new col for response var

fixef_frac_df <- fixef_frac_df %>%
  mutate(Feature= as.factor('Small-scale mining')) %>% 
  filter(!res_var %in% c('adult_Intercept',
                         'egg_Intercept',
                         'tadpole_Intercept')) %>% # remove intercepts
  mutate(
    res_var = recode( # rename response variable
      res_var,
      adult_fractureyes = 'Adult',
      egg_fractureyes = 'Egg',
      tadpole_fractureyes = 'Tadpole'
    )
  )%>%
  mutate(Probability = Estimate / (1 + Estimate)) # probability in case wanted

# view(fixef_frac_df)

# exp plot
ggplot() +
  geom_point(data= fixef_frac_df, aes(x= res_var, y = Estimate), size= 3) +
  geom_errorbar(
    data = fixef_frac_df,
    aes(
      x = res_var,
      ymin = Q2.5,
      ymax = Q97.5
    ),
    size = 1.3,
    width = 0.1
  )+
  labs(y="Probability", x= "Mining")


# Get the posterior distribution of the fixed effects
load('silt.Rdata')

fixef_silt <- exp(fixef(silt)[,-2])

fixef_silt_df <- data.frame(fixef_silt)
fixef_silt_df$res_var <- rownames(fixef_silt_df) # a new col for response var

fixef_silt_df <- fixef_silt_df %>%
  mutate(Feature= as.factor('Silt accumulation')) %>% 
  filter(!res_var %in% c('adult_Intercept',
                         'egg_Intercept',
                         'tadpole_Intercept')) %>% # remove intercepts
  mutate(
    res_var = recode( # rename response variable
      res_var,
      adult_siltyes = 'Adult',
      egg_siltyes = 'Egg',
      tadpole_siltyes = 'Tadpole'
    )
  ) %>%
  mutate(Probability = Estimate / (1 + Estimate)) # probability in case wanted

# view(fixef_silt_df)

# exp plot
ggplot() +
  geom_point(data= fixef_silt_df, aes(x= res_var, y = Estimate), size= 3) +
  geom_errorbar(
    data = fixef_silt_df,
    aes(
      x = res_var,
      ymin = Q2.5,
      ymax = Q97.5
    ),
    size = 1.3,
    width = 0.1
  )+
  labs(y="Probability", x= "silt")


# combined-----
fixef_agri_df
fixef_frac_df
fixef_silt_df

forfig4 <- bind_rows(fixef_agri_df,
                     fixef_frac_df,
                     fixef_silt_df)

forfig4 <-  rownames_to_column(forfig4, var = 'interaction')


fig4 <- ggplot() +
  geom_point(data= forfig4, aes(x= res_var, y = Estimate, col= res_var), 
             fill= 'black',
             size= 3,
             shape=21,
             # col= 'black',
             stroke=0.8) +
  geom_errorbar(
    data = forfig4,
    aes(
      x = res_var,
      ymin = Q2.5,
      ymax = Q97.5,
      col= res_var
    ),
    linewidth = 1,
    width = 0.1,
    # col= 'grey'
  )+
  scale_color_viridis_d()+
  labs(y="Odds ratios", x= " ")+
  facet_wrap(~Feature)+
  theme_bw(base_size = 12) + theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.tag.position = c(0.3, 0.8)
  ) +
  theme(
    panel.grid.major = element_line(colour = "gray86", linewidth =  0.1),
    panel.background = element_rect(fill = "white")
  )
fig4

ggsave('Fig_4.jpg', fig4, 
       width = 10,
       height = 6,
       dpi = 300)


