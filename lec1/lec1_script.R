print("Hello World!")

# print("Hello World!")

# # is for commenting. It is a good habit to comment your code so
# your future self can better understand and reuse your code

# Collect the student information
Student_Info = data.frame(
  Name = c(),
  Preferred_name = c(),
  Origin = c(),
  Major = c(),
  Year = c(),
  Econ_courses = c(),
  Stat_courses = c(),
  Program_lang = c(),
  Program_exper = c()
)


install.packages(c("ggplot2","gapminder"))
library(ggplot2)
library(gapminder)

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


p = ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))
# You can also use <- to assign value to an object
# p <- ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp))
p


p + geom_point(alpha = 0.3)  +
  geom_smooth(method = "loess") 
p + geom_point(aes(size = pop, col = continent), alpha = 0.3)  +
  geom_smooth(method = "loess") 


p_bad = ggplot(data = gapminder, 
               aes(x = gdpPercap, y = lifeExp, size = pop, col = continent)) +
  geom_point(alpha = 0.3)  +
  geom_smooth(method = "loess") 
p_bad


p + geom_density()

ggplot(data = gapminder) + ## i.e. No "global" aesthetic mappings"
  geom_density(aes(x = gdpPercap, fill = continent), alpha=0.3)

p2 =
  p +
  geom_point(aes(size = pop, col = continent), alpha = 0.3) +
  scale_color_brewer(name = "Continent", palette = "Set1") + ## Different colour scale
  scale_size(name = "Population", labels = scales::comma) + ## Different point (i.e. legend) scale
  scale_x_log10(labels = scales::dollar) + ## Switch to logarithmic scale on x-axis. Use dollar units.
  labs(x = "Log (GDP per capita)", y = "Life Expectancy") + ## Better axis titles
  theme_minimal() ## Try a minimal (b&w) plot theme
p2
