#Using statcast data from 2018 to visualize Miguel Andujar batted ball data
M1 <- scrape_statcast_savant(start_date = "2018-04-01", end_date = "2018-04-30", playerid = 624424, player_type = 'batter')
M2 <- scrape_statcast_savant(start_date = "2018-05-01", end_date = "2018-05-31", playerid = 624424, player_type = 'batter')
M3 <- scrape_statcast_savant(start_date = "2018-06-01", end_date = "2018-06-30", playerid = 624424, player_type = 'batter')
M4 <- scrape_statcast_savant(start_date = "2018-07-01", end_date = "2018-07-31", playerid = 624424, player_type = 'batter')
M5 <- scrape_statcast_savant(start_date = "2018-08-01", end_date = "2018-08-31", playerid = 624424, player_type = 'batter')
M6 <- scrape_statcast_savant(start_date = "2018-09-01", end_date = "2018-09-30", playerid = 624424, player_type = 'batter')

Conforto_Data <-rbind(M1, M2, M3, M4, M5, M6) 
View(Conforto_Data)
#Use SQL to filter the data to batted balls hit 95mph or harder
Conforto_95_Data <- sqldf("select *
                          from Conforto_Data
                          where launch_speed >= 95")

#View the data to ensure it is what we want
View(Conforto_95_Data)

#Create strike zone based on MLB averages (Source: Analyzing Baseball Data with R)
top_zone <- 3.5
bot_zone <- 1.6
left_zone <- -0.95
right_zone <- 0.95
strike_zone_df <- data.frame(
  x = c(left_zone, left_zone, right_zone, right_zone, left_zone),
  y = c(bot_zone, top_zone, top_zone, bot_zone, bot_zone)
)


#Split plot by pitcher hand
#Plot our data 
Conforto_Plot <- ggplot() + 
  geom_path(data = strike_zone_df, aes(x = x, y = y)) +
  coord_equal() +
  xlab("Horizontal Distance (ft)") +
  ylab("Vertical Distance (ft)") +
  geom_point(data = Conforto_95_Data, aes(x = plate_x, y = plate_z, color = pitch_type, size = launch_speed)) +
  facet_wrap(. ~ p_throws)

#add title and subtitle to our plot
Conforto_Plot + labs(title = "Pitch Location of Conforto Batted Balls 95+ mph", subtitle = "2018 Regular Season", caption = "Catcher's View")

#Need to adjust the columns provided to us that represent the batted ball coordinates:
Conforto_Data <- Conforto_Data %>%
  mutate(hit_x = hc_x - 125.42, 
         hit_y = 198.27 - hc_y)

#Spray chart showing the density of All Andujar's batted balls:
#geom_point reaveals each type batted ball
Conforto_Data %>%
  filter(type == "X") %>%
  ggplot(aes(x = hit_x, y = hit_y)) + 
  stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="darkblue", high="darkorange1", "Density") +
  geom_point(aes(x = hit_x, y = hit_y, color = events), fill = "black", shape = 21) +
  geom_segment(x=0, xend = 100, y=0, yend = 100, color = "white") +
  geom_segment(x=0, xend = -100, y=0, yend = 100, color = "white") +
  geom_curve(x = -45, xend = 45, y = 53, yend = 53, curvature = -.65, linetype = "dotted", color = "white") +
  theme_bw() + 
  labs(title = "Spray Chart of Michael Conforto's Batted Balls in 2018",
       subtitle = "vs LHP and vs RHP",
       caption = "Data courtesy of MLBAM") +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) + 
  #facet reveals two different spray charts... vsRHP and vs LHP  
  facet_grid(. ~ p_throws)

