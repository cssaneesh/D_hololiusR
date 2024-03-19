source('data_dh.R')

names(montlycount)
names(climate1)

climate1 <- climate %>% group_by(Month) %>%
  summarise(
    mean_rain = mean(rain),
    mean_humidity = mean(humidity),
    mean_temp = mean(temp)
  )

months <- c("Jan", "Feb","Mar","Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")

fig5 <- ggplot() +
  geom_histogram(
    data = montlycount,
    aes(x = Month, y = Count,
        fill = Stage
    ),
    stat = 'identity',
    position = 'dodge',
    binwidth = 10,
  ) +
  scale_fill_viridis(discrete = T, option="D")+
  # scale_fill_brewer(palette = 5)
  # scale_fill_grey() +
  geom_point(
    data = climate1,
    aes(x = Month, y = mean_rain),
    lty = 2,
    col = 'blue'
  )+
  geom_line(
    data = climate1,
    aes(x = Month, y = mean_rain),
    # Temperature
    lty = 2,
    col = 'blue'
  )+
  annotate(
    geom = 'text',
    x = 2,
    y = 8,
    label = 'Mean rainfall (mm)',
    color = 'blue',
    size = 3) +
  geom_point(
    data = climate1,
    aes(x = Month, y = mean_humidity),
    lty = 2,
    col = 'orange'
  )+
  geom_line(
    data = climate1,
    aes(x = Month, y = mean_humidity),
    # Temperature
    lty = 2,
    col = 'orange'
  )+
  annotate(
    geom = 'text',
    x = 4,
    y = 60,
    label = 'Mean humidity (%)',
    color = 'orange',
    size = 3)+
    geom_point(
    data = climate1,
    aes(x = Month, y = mean_temp),
    lty = 2,
    col = 'red'
  )+
  geom_line(
    data = climate1,
    aes(x = Month, y = mean_temp),
    # Temperature
    lty = 2,
    col = 'red'
  )+
  annotate(
    geom = 'text',
    x = 3,
    y = 32,
    label = (expression(paste('Mean temperature (', degree~C,')'))),
    color = 'red',
    size = 3)+
  scale_x_discrete(
    limits = months)+
  theme_bw(base_size = 12) + theme(
    # legend.position = 'none',
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    plot.tag.position = c(0.3, 0.8)
  ) +
  theme(
    panel.grid.major = element_line(colour = "gray86", linewidth =  0.1),
    panel.background = element_rect(fill = "white")
  )+
  labs(x= '', y = '')
  
fig5

ggsave('Fig_5.jpg', fig5, 
       width = 10,
       height = 6,
       dpi = 300)
