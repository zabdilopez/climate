#load libraries
library(tidyverse)
library(gganimate)

#load data

t_diff <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year= Year, month.abb) %>%
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>%
  drop_na() %>% #eliminates all NA values
  mutate(month = factor(month, levels = month.abb)) #helps put the months in alphabetical order

#filtering out all the data from last december
last_dec <- t_diff %>%
  filter(month == "Dec") %>%
  mutate(year = year + 1,
         month = "last_Dec")

next_jan <- t_diff %>%
  filter(month == "Jan") %>%
  mutate(year = year - 1,
         month = "next_Jan")

#create 

t_data <- bind_rows(last_dec, t_diff, next_jan) %>%
  mutate(month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
         month_number = as.numeric(month) -1,
         this_year = year == 2023)

annotation <- t_data %>%
  slice_max(year)%>%
  slice_max(month_number)
 
#plot
t_data %>%
  ggplot(aes(x=month_number, y=t_diff, group = year, 
             color = year, size = this_year))+
  geom_hline(yintercept = 0, color = "black")+
  geom_line() +
  geom_text(data = annotation, aes(x=month_number, y=t_diff, label = year, color = year), 
            inherit.aes = FALSE,
            hjust = 0, size = 5, nudge_x = 0.15, fontface = "bold")+
  scale_x_continuous(breaks = 1:12,
                     labels = month.abb,
                     sec.axis = dup_axis(name = NULL, labels = NULL))+
  scale_y_continuous(breaks = seq(-2, 2, 0.2))+
  scale_size_manual(breaks = c(FALSE, TRUE),
                    values = c(0.25, 1), guide = "none") +
  scale_color_gradientn(colors = c("blue", "white", "red"),
                        breaks = seq(1880, 2020, 20),
                        guide = guide_colorbar(frame.colour = "black",
                                               frame.linewidth = 0.5)) +
  coord_cartesian(xlim = c(1,12))+
  labs(x= NULL,
       y = "Temperature change since pre-industrial time [\u00B0C]",
       title = "Global temperature change since 1880 by month")+
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white",color = "black", size = 1),
    panel.grid = element_blank(),
    axis.text = element_text(color = "black", size = 13),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(-5, "pt"),
    axis.title = element_text(color = "black", size = 13),
    plot.title = element_text(color = "black", hjust = 0.5, size = 15),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA),
    legend.text = element_text(color = "black"),
    legend.key.height = unit(56, "pt")
  )

ggsave("figures/temperatures_lines_white.png", width = 8, height = 4.5)
  