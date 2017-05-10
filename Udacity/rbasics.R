# install & load ggplot package
install.packages('ggplot2', dependencies = TRUE) 
library(ggplot2) 

#install & load a color package
install.packages('RColorBrewer', dependencies = TRUE) 
library(RColorBrewer) 

# load the AirPassengers data set
#airpassengers = data("AirPassengers")
#str(airpassengers)
?AirPassengers

# load the diamonds data set
data("diamonds")

# create scatterplot of 
qplot(data = diamonds, x = carat, y = price, color = cut) +
  scale_color_brewer(palette = 'Accent')
