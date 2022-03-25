library(tidyverse)
library(ggrepel)
mpg %>% glimpse()

#Basic Plot
mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth()

#Altered Plot with lots of exaples of how to change things. 
my_plot <- 
  mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 36, color = "red", linetype = "dotted") +
  labs(
    x = "X axis Title",
    y = "Y axis Title",
    title = "My title",
    subtitle = "With a subtitle\nthat goes along several\nlines because we can.",
    color = "Color Title",
    caption = "And an italicized caption just for kicks"
  ) +
  scale_x_continuous(
    breaks = c(3,6),
    labels = c("3","six")
  ) +
  scale_y_continuous(
    labels = function(x)paste0(x," $$")
  ) +
  #I'll change the theme to something close to what I want, and then modify it. 
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid.major.x = element_line(color = "yellow", linetype = "dashed", size = 1),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(color = "red", fill = "green"),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, face = "italic"),
    plot.title.position = "plot",
  )
#Show the plot. 
my_plot


##
paste0(1:12, c("st", "nd", "rd", rep("th", 9)))
paste("1st", "2nd", "3rd", sep = ", ")
##

#Select key data points by wrangling your data
largest_displ <-
  mpg %>% 
  group_by(class) %>% 
  filter(displ == max(displ)) %>% 
  glimpse()

#Add empahsis to the plot to show the data points. 
my_plot +
  geom_point(data = largest_displ, size = 3)

my_plot +
  geom_point(data = largest_displ, size = 3) +
  geom_text(data = largest_displ,
            mapping = aes(x = displ, y = hwy, label = model),
            hjust = 0)

# cover dots
my_plot +
  geom_label(data = largest_displ,
             mapping = aes(x = displ, y = hwy, label = model),
             hjust = 0)

# nudge_y changes the place for labels.
my_plot +
  ggrepel::geom_label_repel(data = largest_displ,
                            mapping = aes(x = displ, y = hwy, label = model),
                            nudge_y = 20)

my_plot +
  geom_point(data = largest_displ, size = 3) +
  ggrepel::geom_text_repel(data = largest_displ,
                           mapping = aes(x = displ, y = hwy, label = model),
                           nudge_y = 20)
my_plot +
  geom_point(data = largest_displ, size = 3) +
  ggrepel::geom_text_repel(data = largest_displ,
                           mapping = aes(x = displ, y = hwy, label = model),
                           nudge_y = 20,
                           inherit.aes = FALSE, 
                           color = "white")

my_plot +
  geom_point(data = largest_displ, size = 3) +
  scale_x_continuous(expand = expansion(mult = 0, add = c(0, 3))) +
  ggrepel::geom_text_repel(data = largest_displ,
                           mapping = aes(x = displ, y = hwy, label = model),
                           nudge_x = 20)

my_plot +
  geom_point(data = largest_displ, size = 3) +
  scale_x_continuous(
    breaks = c(3,6),
    labels = c("3","six"),
    expand = expansion(mult = 0, add = c(0, 6))
  ) +
  ggrepel::geom_text_repel(data = largest_displ,
                           mapping = aes(x = displ, y = hwy, label = model),
                           nudge_x = 20,
                           nudge_y = 5,
                           inherit.aes = FALSE, 
                           color = "white")