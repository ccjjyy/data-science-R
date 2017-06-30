library(tidyverse)

mpg_data <- mpg
mpg
?mpg
summary(mpg)

#--------------------
# A Graphing Template
#--------------------

# ggplot(data=<DATA>) + <GEOM_FUNCTION>(mapping=aes(<MAPPINGS>))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy))

# Aesthetic Mapping
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, color=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, size=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, alpha=class))
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy, shape=class))

# Facets
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy)) + facet_wrap(~class, nrow=2)
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy)) + facet_grid(drv~cyl)
ggplot(data=mpg) + geom_point(mapping=aes(x=displ, y=hwy)) + facet_grid(.~cyl)

# Geometric Objects
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy))
ggplot(data=mpg) + geom_smooth(mapping=aes(x=displ, y=hwy, color=drv))
ggplot(data=mpg) +
  geom_point(mapping=aes(x=displ, y=hwy)) +
  geom_smooth(mapping=aes(x=displ, y=hwy))

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) + geom_point() + geom_smooth() # a better way

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=drv)) +
  geom_smooth(mapping=aes(color=drv))

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=class)) +
  geom_smooth(data=filter(mpg, class=='subcompact'), se=TRUE)

# Statistical Transformations
diamonds_data <- diamonds
diamonds
?diamonds
summary(diamonds)

ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut))
ggplot(data=diamonds) + stat_count(mapping=aes(x=cut))
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, y=..prop.., group=1))

ggplot(data=diamonds) +
  stat_summary(mapping=aes(x=cut, y=depth), fun.ymin=min, fun.ymax=max, fun.y=median)

# Position Adjustments
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=cut))
ggplot(data=diamonds) + geom_bar(mapping=aes(x=cut, fill=clarity))

ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) +
  geom_bar(position='identity') # overlap

ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) +
  geom_bar(position='fill')

ggplot(data=diamonds, mapping=aes(x=cut, fill=clarity)) +
  geom_bar(position='dodge')

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_point(position='jitter')

ggplot(data=mpg, mapping=aes(x=displ, y=hwy)) +
  geom_jitter() # can also use this

# Coordinate Systems
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) + geom_boxplot()
ggplot(data=mpg, mapping=aes(x=class, y=hwy)) + geom_boxplot() + coord_flip()

ggplot(data=diamonds, mapping=aes(x=cut, fill=cut)) +
  geom_bar(show.legend=FALSE, width=1) +
  coord_polar() +
  theme(aspect.ratio=1) + 
  labs(x=NULL, y=NULL)
  

#--------------------------------
# The Layered Grammar of Graphics
#--------------------------------

# ggplot(data=<DATA>) +
#   <GEOM_FUNCTION>(mapping=aes(<MAPPINGS>), stat=<STAT>, position=<POSITION>) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>
