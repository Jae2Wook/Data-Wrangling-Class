library(tidyverse)
library(ggplot2)

[website](https://r4ds.had.co.nz/data-visualisation.html)
[additional info](http://zevross.com/blog/2019/04/02/easy-multi-panel-plots-in-r-using-facet_wrap-and-facet_grid-from-ggplot2/)

# Creating a ggplot
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# Aesthetic mappings
ggplot(mpg, aes(x = displ, y = hwy, color = class)) + 
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, size = class)) +
  geom_point()

# Warning message: Using size for a discrete variable is not advised.
# Maybe better using with numbers better

ggplot(mpg, aes(x = displ, y = hwy, alpha = class)) +
  geom_point() #alpha shows transparency

ggplot(mpg, aes(x = displ, y = hwy, shape = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue", fill = "red", shape = 23, size = 2) # have to put in geom() not in ggplot()

# Facets
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~class, nrow = 3) # show each subset

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(drv ~ cyl) # y ~ x

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_grid(. ~ cyl)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() +
  facet_wrap(~class, ncol = 2)

# Geometric objects
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_smooth()

ggplot(mpg, aes(x = displ, y = hwy, group = drv, color = drv)) +
  geom_smooth(show.legend = FALSE)

ggplot(mpg, aes(x = displ, y = hwy, linetype = drv, color = drv)) + # treat them as local
  geom_smooth() +
  geom_point()

# =

ggplot(mpg) + 
  geom_point(aes(x = displ, y = hwy)) +
  geom_smooth(aes(x = displ, y = hwy))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE) # give condition to smooth line

# ultimate graph
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = drv), show.legend = FALSE) +
  geom_smooth(se = FALSE, aes(group = drv, linetype = drv), show.legend = FALSE)

ggplot(diamonds, aes(x = cut)) +
  geom_bar() # transforms data with "count"

ggplot(diamonds, aes(x = cut)) +
  stat_count() # geom and stat are interchangeable


demo <- tribble( # tribble makes a dataframe. this will be useful
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity") # stat = identity shows height of the y variables

ggplot(diamonds, aes(x = cut, y = stat(prop), group = 1)) +
  geom_bar() # stat(prop): shows proportion

ggplot(diamonds) +
  stat_summary(aes(x = cut, y = depth), fun.min = min, fun.max = max, fun = median)

# why we need group = 1: seems it makes out of 1
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color, y = after_stat(prop)))


# You would use after_stat() to e.g. pick a different computed metric than the default (here density instead of count)
ggplot(mpg, aes(displ)) +
  geom_histogram(aes(y = after_stat(density)))

ggplot(mpg, aes(class, hwy)) +
  geom_boxplot(aes(colour = class, fill = after_scale(alpha(colour, 0.4))))

# Position adjustments
ggplot(diamonds, aes(x = cut, color = cut)) +
  geom_bar() # color = boundary
ggplot(diamonds, aes(x = cut)) +
  geom_bar(aes(fill = cut)) # fill = inside

ggplot(diamonds, aes(x = cut, fill =clarity)) +
  geom_bar()

# position = "identity" will place each object exactly where it falls in the context of the graph. 
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# position = "fill" works like stacking, but makes each set of stacked bars the same height.
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "fill")

# position = "dodge" places overlapping objects directly beside one another.
ggplot(diamonds, aes(x = cut, fill = clarity)) +
  geom_bar(position = "dodge")

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(position = "jitter")

# Coordinate systems
ggplot(mpg, aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip()

# coord_quickmap() sets the aspect ratio correctly for maps.
##### install.packages("maps") not working ?
library(maps)
nz <- map_data("nz")
ggplot(nz, aes(x = long, y = lay, group = group)) +
  geom_polygon(fill = "white", color = "black") +
  coord_quickmap()

# coord_polar() uses polar coordinates.
# Polar coordinates reveal an interesting connection between a bar chart and a Coxcomb chart.
bar <- ggplot(diamonds, aes(x = cut, fill = cut)) + geom_bar(show.legend = FALSE, width = 1) + theme(aspect.ratio = 1) + labs(x = NULL, y = NULL)
bar + coord_flip()
bar + coord_polar()

# Layered grammar of graphics
ggplot(data) +
  geom_function(aes(x = , y = ), stat = , position = ) +
  coord_function() +
  facet_function()