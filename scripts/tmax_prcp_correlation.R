source("scripts/local_weather.R")

tmax_prcp <- local_weather %>% 
  mutate(year = year(date)) %>% 
  filter(year != 1891 & year != year(today())) %>% 
  group_by(year) %>% 
  summarize(tmax = mean(tmax, na.rm = TRUE), #remove the NA value before doing the calculation
            prcp = sum(prcp, na.rm = TRUE))


tmax_prcp %>% 
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value))+
  geom_line()+
  facet_wrap(~name, ncol = 1, scales = "free_y")+
  geom_smooth(se = FALSE)

scaled_tmax_prcp <- tmax_prcp %>% 
  mutate(tmax_tr = (tmax - min(tmax))/ (max(tmax) - min(tmax)),
         tmax_min = min(tmax),
         tmax_max = max(tmax),
         
         prcp_tr = (prcp - min(prcp))/ (max(prcp) - min(prcp)),
         prcp_min = min(prcp),
         prcp_max = max(prcp),)

tmax_plot <-scaled_tmax_prcp %>% 
  ggplot(aes(x = year, y = tmax_tr))+
  geom_line(color = "blue")
  

tmax_plot + 
  geom_line(aes(y = prcp_tr), color = "red")+
  scale_y_continuous(labels = seq(10,30,5),
                     breaks = (seq(10,30,5)-11.7)/17.5,
                     limits = (c(8,32) -11.7)/17.5,
                     name = "Average annual temperature",
                     sec.axis= sec_axis(trans = ~.,
                                        labels = seq(300,1800,300),
                                        breaks = (seq(300,1800,300)-420)/1298,
                                        name = "Total Precipitation (mm)")
                     )+
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )

tmax_prcp %>% 
  ggplot(aes(x = tmax, y = prcp))+
  geom_point()+
  geom_smooth(method = "lm", color = "#7294D4")+
  scale_y_continuous( breaks = seq(0,1000, 100),
                      labels = seq(0,100, 10),
                      expand = c(0.01,0))+
  scale_x_continuous( breaks = seq(-20,30, 2),
                      labels = seq(-20,30, 2),
                       expand = c(0.01,0))+
  labs(
    x = "Temperature (\u00B0C)",
    y = "Precipitation (mm)",
    title = glue("Correlation between precipitation and tempertaure in Longyearbyen, Svalbard ({min(tmax_prcp$year)} to {max(tmax_prcp$year)})"),
    subtitle = "Correlation = 0.447514 and p-value = 1.56e-05")+
  theme(
    axis.title.x = element_text(color = "#E6A0C4", face = "bold"),
    axis.title.y = element_text(color = "#C6CDF7", face = "bold"),
    plot.title.position = "plot",
    plot.title = element_text(size = 12, face  = "bold"),
    plot.subtitle = element_text(size = 10)
  )

cor.test(tmax_prcp$tmax, tmax_prcp$prcp)

cor.test(tmax_prcp$tmax, tmax_prcp$prcp, method = "spearman", exact = FALSE)
