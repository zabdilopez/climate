#load libraries

library(tidyverse)
library(gganimate)

#clean up and prepare data
t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, month.abb) %>%
  pivot_longer(-year, names_to="month", values_to="t_diff") %>%
  drop_na()

next_jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

t_data <- bind_rows(t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c(month.abb, "next_Jan")),
         month_number = as.numeric(month)) %>%
  arrange(year, month) %>%
  filter(year != 1879) %>%
  mutate(step_number = 1:nrow(.))

annotation <- t_data %>%
  slice_max(year) %>%
  slice_max(month_number)

temp_lines <- tibble(
  x = 1,
  y = c(1, 0, -1),
  labels = c("+1\u00B0 C", "0\u00B0 C", "-1\u00B0 C")
)

month_labels <- tibble(
  x = 1:12,
  labels = toupper(month.abb),
  y = 1.5
)

gridlines <- tibble(
  x= c(1.2, 1.3, 1.6),
  xend = c(12.8, 12.7, 12.4),
  y = c(1, 0, -1),
  yend = y
)  

#create plot
a <- t_data %>% 
  ggplot(aes(x=month_number, y=t_diff, group=year, color=t_diff)) +
  geom_label(aes(x = 1, y=-1.7, label = year),
             fill="black",
             label.size = 0,
             size=6) +
  geom_line() +
  geom_segment(data = gridlines,aes(x=x, y=y, xend = xend, yend = yend),
               color=c("yellow","green", "yellow"), 
               inherit.aes = FALSE) +
  geom_text(data = temp_lines, aes(x=x, y=y, label=labels),
            color=c("yellow","green", "yellow"),size = 2, fontface ="bold",
               inherit.aes=FALSE) +
  geom_text(data = month_labels, aes(x=x, y=y, label = labels),
              inherit.aes = FALSE, color="yellow")
  scale_y_continuous(
                     limits = c(-2.0, 1.5), expand = c(0, -0.3), 
                    ) +
  scale_color_gradient2(low = "darkblue", high = "red", mid = "white", midpoint = 0,
                        guide = "none") +
  coord_polar(start = 0) +
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  theme(
    panel.background = element_rect(fill="black"),
    plot.background = element_rect(fill = "#444444", color="#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_blank()
    )+
  transition_manual(frames = year, cumulative = TRUE)

#create the animation
animate(a, width=4.155, height=4.5, unit="in", res=300,)

#export the animation
#as GIF
anim_save("figures/climate_spiral_nasa.gif")

#as .mp4
animate(a, width=4.155, height=4.5, unit="in", res=300,
         renderer = av_renderer("figures/climate_spiral_nasa.mp4")
        )
