################################################################################
print("Hello World!")

# print("Hello World!")

# # is for commenting. It is a good habit to comment your code so
# your future self can better understand and reuse your code

# A line of # can even divide sections of the code and act as a break line!
# Left of the break line, you can find a little triangle.
# Click and see what does it do.


################################################################################
# introduction to ggplot2
# Download packages and load them
install.packages(c("ggplot2","gapminder"))
library(ggplot2)
library(gapminder)

# head and tail help you to get a feel about the data
head(gapminder)
gapminder

ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, col = continent)) + 
  geom_point(alpha = 0.3) ## "alpha" controls transparency. Takes a value between 0 and 1.

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + ## Applicable to all geoms
  geom_point(aes(size = pop, col = continent), alpha = 0.3) ## Applicable to this geom only

ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(aes(size = "big", col="black"), alpha = 0.3)
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) + 
  geom_point(size = 3, col="black", alpha = 0.3)


# Now, we will build a ggplot layer by layer
# Here is our foundation layer
p = ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))
p

# Add dotplot and smoothed line
p + geom_point(alpha = 0.3)  +
  geom_smooth(method = "loess") 
p + geom_point(aes(size = pop, col = continent), alpha = 0.3)  +
  geom_smooth(method = "loess") 


# Here is a bad plot. Note the foundation layer applies to all future layers
# You need to make sure it only has the universal stuff applicable to all
p_bad = ggplot(data = gapminder, 
               aes(x = gdpPercap, y = lifeExp, size = pop, col = continent)) +
  geom_point(alpha = 0.3)  +
  geom_smooth(method = "loess") 
p_bad

# Here is a density plot, what does it do?
p + geom_density()

ggplot(data = gapminder) + ## i.e. No "global" aesthetic mappings"
  geom_density(aes(x = gdpPercap, fill = continent), alpha=0.3)

# A fancier plot using the same foundation
p2 =
  p +
  geom_point(aes(size = pop, col = continent), alpha = 0.3) +
  scale_color_brewer(name = "Continent", palette = "Set1") + ## Different colour scale
  scale_size(name = "Population", labels = scales::comma) + ## Different point (i.e. legend) scale
  scale_x_log10(labels = scales::dollar) + ## Switch to logarithmic scale on x-axis. Use dollar units.
  labs(x = "Log (GDP per capita)", y = "Life Expectancy") + ## Better axis titles
  theme_minimal() ## Try a minimal (b&w) plot theme
p2
