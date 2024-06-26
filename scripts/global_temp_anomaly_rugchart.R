library(tidyverse)

bands <-rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N", 
          "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))

url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv"

zone_data <- read_csv(url) %>%
  select(year = Year, all_of(bands))%>%
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>%
  mutate(zone = factor(zone, levels = bands),
         zone_position = as.numeric(zone))

current_year <- zone_data %>%
  filter(year == 2022)

zone_data%>%
  ggplot(aes(x=t_diff, xend= t_diff,
             y=zone_position - 0.25, yend = zone_position + 0.25)) +
  geom_segment(color = "black", alpha = 0.25) +
  geom_segment(data = current_year,
               aes(color = t_diff), size = 2, lineend = "round")+
  scale_y_continuous(breaks = 1:8,
                     labels = bands)+
  scale_x_continuous(breaks = seq(-3,4,1),
                     labels = seq(-3,4,1),
                     limits = c(-3,4)) +
  scale_color_gradient2(low = "darkblue", mid = "white", high = "darkred",
                        midpoint = 0, guide = "none")+
  labs(x = "Temperature anomaly (\u00B0 C)",
       y = NULL,
       title = "Variation in annual temperature anomaly by latitude (1880-2022)",
       subtitle = "Bars for 2022 are colored by the size of anomaly")+
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white"),
    plot.title = element_text(color = "black", face = "bold"),
    plot.subtitle = element_text(color = "gray", size = 8),
    plot.title.position = "plot",
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.grid.major.x = element_line(color = "gray", size =0.25),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_line(color = "black"),
    axis.ticks = element_blank()
  )

ggsave("/Users/zabdi/Documents/GitHub/climate/figures/global_temp_anomaly_rugchart.png", height = 4, width = 6, units = "in")
