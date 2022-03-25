# Graphics for communication

library(tidyverse)

## Label

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel", subtitle = "Two seaters", caption = "Data from",
       x = "Engine displacement(L)", y = "Hightway fuel", color = "Car type")


# quote(): reads 'plotmath'
df <- tibble(x = runif(10), y = runif(10))

ggplot(df, aes(x, y)) +
  geom_point() +
  labs(x = quote(sum(x[i]^2, i == 1, n)), #sum(): Sigma, []: lower case
       y = quote(alpha + beta + frac(delta, theta))) # frac(): fracture


## Annotations
best_in_class <- mpg %>% 
  group_by(class) %>% 
  filter(row_number(desc(hwy)) == 1) # row_number(): ranking function

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_text(aes(label = model), data = best_in_class)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_label(aes(label = model), data = best_in_class, nudge_y = 2, alpha = 0.1) # nudge_y: move in y-axis direction

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_point(size = 3, shape = 1, data = best_in_class) + # adding new points
  ggrepel::geom_label_repel(aes(label = model), data = best_in_class) # ggrepel adjust label so that don't overlap

class_avg <- mpg %>% 
  group_by(class) %>% 
  summarise(displ = median(displ), hwy = median(hwy))

ggplot(mpg, aes(displ, hwy, color = class)) +
  ggrepel::geom_label_repel(aes(label = class), data = class_avg, size = 3, label.size = NA, segment.color = NA) +
  geom_point() +
  theme(legend.position = "none")

label <- mpg %>% 
  summarise(displ = max(displ), hwy = max(hwy), label = "Increasing engine size is\nrelated to decreasing fuel economy")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right")

# Exactly on the order, use Inf or -Inf
label <- tibble(displ = Inf, hwy = Inf, label = "Increasing engine")

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_text(aes(label = label), data = label, vjust = "top", hjust = "right") +
  geom_hline(yintercept = 20) +
  geom_vline(xintercept = 4) +
  geom_rect(aes(xmin = 1, xmax = 2, ymax = 10, ymin = 9), color = "red", fill = "green", alpha = 0.2) +
  geom_segment(aes(x = 1, y = 10, xend = 2, yend = 20), size = 2, color = "red")

"Increasing engine size is related to decreasing fuel economy." %>% 
  stringr::str_wrap(width = 40) %>% 
  writeLines()

# Scale

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(label = NULL) +
  scale_y_continuous(labels = NULL)

presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) + # all points head to the xend and yend 
  scale_x_date(NULL, breaks = presidential$start, date_labels = "'%y, %B, %d")

# Legend layout
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, override.aes = list(size = 4)))

# Replacing a scale
ggplot(diamonds, aes(carat, price)) +
  geom_bin2d()

ggplot(diamonds, aes(log10(carat), log10(price))) +
  geom_bin2d()

ggplot(diamonds, aes(carat, price)) +
  geom_bin2d() +
  scale_x_log10() +
  scale_y_log10()

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv)) +
  scale_color_brewer(palette = "Set1")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = drv, shape = drv)) +
  scale_color_brewer(palette = "Set1")

presidential %>% 
  mutate(id = 33 + row_number()) %>% 
  ggplot(aes(start, id, color = party)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "red", Democratic  ="blue"))

df <- tibble(x = rnorm(10000), y = rnorm(10000))

ggplot(df, aes(x, y)) +
  geom_jitter() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_jitter() +
  viridis::scale_fill_viridis() +
  coord_fixed()

ggplot(df, aes(x, y)) +
  geom_point() +
  scale_color_gradient(low = "white", high = "red") +
  coord_fixed()

ggplot(diamonds, aes(carat, price)) +
  geom_point(aes(color = cut), alpha = 0.05)

# Zooming
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth() +
  coord_cartesian(xlim = c(5, 7), ylim = c(10, 30))

mpg %>% 
  filter(displ >= 5, displ <= 7, hwy >= 10, hwy <= 30) %>% 
  ggplot(aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth()

suv <- mpg %>% filter(class == "suv")
compact <- mpg %>% filter(class == "compact")

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point()

ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point()

x_scale <- scale_x_continuous(limits = range(mpg$displ)) # use limits and range
y_scale <- scale_y_continuous(limits = range(mpg$hwy))
col_scale <- scale_color_discrete(limits = unique(mpg$drv)) # use limits and unique

ggplot(suv, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale
ggplot(compact, aes(displ, hwy, color = drv)) +
  geom_point() +
  x_scale +
  y_scale +
  col_scale

# Themes
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  theme_bw()

# Saving plots
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("my_plot.pdf")

fig.width = 6
fig.asp = 0.618
out.width = "70%" # 50% for two, 33% for three, 25% for four
fig.align = "center"


### Chart and Analysis

# (1 graph) Recreate this graph (Links to an external site.)
# as closely as you can. Focus on labels (title, subtitle, and caption), 
# the y-axis, and line color. 

cov <- read_csv("https://github.com/ktoutloud/classslides/raw/master/math335/data/M335_excess-mortality-p-scores.csv")

cov <- cov %>% mutate(Spain = Spain * 100)
View(cov)

cov1 <- cov %>%  select(c(date, Bulgaria, `England & Wales`, `South Korea`, Denmark))
cov1 <- cov1 %>% pivot_longer(cols = -date, names_to = "country", values_to = "percentage")
bul_date <- cov1 %>% filter(country == "Bulgaria") %>% filter(date == "2021-01-17")
eng_date <- cov1 %>% filter(country == "England & Wales") %>% filter(date == "2021-01-17")
den_date <- cov1 %>% filter(country == "Denmark") %>% filter(date == "2021-01-24")
sk_date <- cov1 %>% filter(country == "South Korea") %>% filter(date == "2020-12-06")
date <- bind_rows(bul_date, eng_date, den_date, sk_date)

View(cov1)
library(scales)

base <- ggplot(cov1, aes(x = date, y = percentage, color = country)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(NULL, breaks = seq(0, 100, by = 20), labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = c(20 ,40, 60, 80, 100), linetype = "dashed") +
  geom_hline(yintercept = c(0), linetype = "solid") +
  scale_x_date(NULL, breaks = as.Date(c("2020-01-05", "2020-04-30", "2020-11-16", "2021-05-02")), date_labels = "'%B, %d, %Y") +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  # ggrepel::geom_label_repel(data = date, aes(label = country), label.size = NA, alpha = 0.8 ) +
  geom_label(aes(label = country), data = date, nudge_x = -3, nudge_y = -2, alpha = 0, label.size = NA) +
  theme(legend.position = "none")

base

# (3 graphs) Suppose we wanted to highlight (draw attention to) 
# any points that are above 100%. Create three more graphs that show 
# three different ways to highlight these points.

over_100 <- cov1 %>% filter(percentage > 100)

base + geom_point(data = over_100, size = 3, shape = 1, color = "red", stroke = 2)

base + geom_rect(aes(xmin = min(date), xmax = max(date), ymax = Inf, ymin = 100), color = "red", fill = NA)


over_70_eng <- cov1 %>% filter(country == "England & Wales") %>%  filter(percentage > 70)
over_80_bul <- cov1 %>% filter(country == "Bulgaria") %>%  filter(percentage > 80)

base + geom_line(data = over_70_eng, mappint = aes(x = date, y = percentage), color = "red", size = 1.25) +
  geom_line(data = over_80_bul, mappint = aes(x = date, y = percentage), color = "red", size = 1.25) +
  geom_point(data = over_100, mappint = aes(x = date, y = percentage), color = "black", size = 2.25)

# (2 graphs) Now add a line for the United States and make two graphs 
# showing two different methods to highlight the United States data.

library(gghighlight)

cov2 <- cov %>%  select(c(date, Bulgaria, `England & Wales`, `South Korea`, Denmark, `United States`))

cov2 <- cov2 %>% pivot_longer(-date, names_to = "country", values_to = "percentage")

us_date <- cov2 %>% filter(country == "United States") %>% filter(date == "2020-12-13")

date <- bind_rows(date, us_date)

base2 <- ggplot(cov2, aes(x = date, y = percentage, color = country)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(NULL, breaks = seq(0, 100, by = 20), labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = c(20 ,40, 60, 80, 100), linetype = "dashed") +
  geom_hline(yintercept = c(0), linetype = "solid") +
  scale_x_date(NULL, breaks = as.Date(c("2020-01-05", "2020-04-30", "2020-11-16", "2021-05-02")), date_labels = "%B, %d, %Y") +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  # ggrepel::geom_label_repel(data = date, aes(label = country), label.size = NA, alpha = 0.8 ) +
  geom_label(aes(label = country), data = date, nudge_x = -3, nudge_y = -2, alpha = 0, label.size = NA) +
  theme(legend.position = "none")

base2 + gghighlight(country == "United States", label_key = country)

cov2_1 <- cov2 %>% mutate(US_or_not = ifelse(country == "United States", "Y", "N"))

date <- date %>% mutate(US_or_not = ifelse(country == "United States", "Y", "N"))

ggplot(cov2_1, aes(x = date, y = percentage, group = country, color = US_or_not)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_y_continuous(NULL, breaks = seq(0, 100, by = 20), labels = function(x) paste0(x, "%")) +
  geom_hline(yintercept = c(20 ,40, 60, 80, 100), linetype = "dashed") +
  geom_hline(yintercept = c(0), linetype = "solid") +
  scale_x_date(NULL, breaks = as.Date(c("2020-01-05", "2020-04-30", "2020-11-16", "2021-05-02")), date_labels = "%B, %d, %Y") +
  theme(panel.grid = element_blank(), panel.border = element_blank()) +
  #ggrepel::geom_label_repel(data = date, aes(label = country), label.size = NA, alpha = 0.8 ) +
  geom_label(aes(label = country), data = date, nudge_x = -3, nudge_y = -2, alpha = 0, label.size = NA) +
  theme(legend.position = "none")

