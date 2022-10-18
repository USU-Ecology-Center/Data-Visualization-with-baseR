##############################################################
### How to making pretty, readable plots in base R
### Illustrated with examples
### USU Ecology Center workshop series
### Michael Stemkovski
##############################################################

# Properties of a good data visualization:
# 1) Easily readable (viewable)
#    - use visual language, not written language (minimize actual reading)
#    - avoid small features
#    - don't use codes/acronyms/arbitrary names
#    - should be understandable without reading the paper closely
#
# 2) Conveys the main scientific observation
#    - the most obvious feature should be the most scientifically important
#    - don't show extraneous information (but be honest)
#    - the takeaway can be stated in one sentence
#    - the figure and the statistics should be analogous
#
# 3) Looks good
#    - don't hurt the reader's eyes
#    - instead, draw the reader's eyes
#
# 4) Information dense & intentional
#    - find ways to use many elements of the figure to convey a message
#    - if you find yourself with many similar-looking panels, reconsider
#    - the following can all mean something:
#    - color, shape, size, line width, shading, position, superposition, etc.
#
# 5) Creative/original
#    - but only if the novelty serves a purpose
#    - feel empowered to use your artistic sensibilities
#    - the only real limit on figures is how much time/dedication/creativity you put into it

library(data.table)
library(RColorBrewer)

### loading in some data
data(iris)
iris <- data.table(iris)

data(beavers)
beaver1 <- beaver1[which(beaver1$day==346),] # getting just the first day

madeup <- data.table(time = c(1991:2020),
                     P = c(1:30) + rnorm(30,10,5))
madeup[, biomass := P*30 + rnorm(30, 100,100)]


#######################
### The basic plots ###
#######################

### continuous predictor variables

# scatter plot
plot(Sepal.Length ~ Petal.Length, data=iris)

# multiple scatter plots
pairs(iris[,-5])

# histogram
hist(iris$Sepal.Width)

# line plot (time-series)
plot(temp ~ time, data = beaver1, type="l")

### categorical predictor variables

# box plot
plot(Sepal.Length ~ Species, data=iris)

# bar plot
iris_means <- iris[, lapply(.SD, mean), by=Species] # mean of every trait by species
barplot(iris_means$Petal.Length, names.arg = iris_means$Species)


#########################
### Beautifying plots ###
#########################


### a better scatter plot
plot(Sepal.Length ~ Petal.Length, data=iris)
# coloring by species
plot(Sepal.Length ~ Petal.Length, data=iris,
     col=as.factor(Species))
# picking better colors
display.brewer.all()
color_pal <- brewer.pal(3, "Dark2")
plot(Sepal.Length ~ Petal.Length, data = iris,
     col = color_pal[as.factor(Species)])
# better points
plot(Sepal.Length ~ Petal.Length, data = iris,
     col = color_pal[as.factor(Species)],
     pch = 19)
# axis labels
plot(Sepal.Length ~ Petal.Length, data = iris,
     col = color_pal[as.factor(Species)],
     pch = 19,
     xlab = "Petal length (cm)", ylab="Sepal length (cm)")
# no border
plot(Sepal.Length ~ Petal.Length, data = iris,
     col = color_pal[as.factor(Species)],
     pch = 19,
     xlab = "Petal length (cm)", ylab="Sepal length (cm)",
     bty="n")
# legend
legend("topleft", inset = 0.05,
       legend=c(iris_means$Species), title="Species",
       pch=19, col=color_pal)


### a better bar plot
barplot(iris_means$Petal.Length, names.arg = iris_means$Species)
# all traits for all species
barplot(as.matrix(iris_means[,-"Species"]), beside=T)
# better color
barplot(as.matrix(iris_means[,-"Species"]), beside=T,
        col=color_pal)
# no bar borders
barplot(as.matrix(iris_means[,-"Species"]), beside=T,
        col=color_pal, border=F)
# axes and labels
barplot(as.matrix(iris_means[,-"Species"]), beside=T,
        col=color_pal, border=F,
        xlab="Trait", ylab="Mean size (cm)",
        names.arg = c("Sepal length", "Sepal width", "Petal width", "Petal width"))
# legend
legend("topright", inset = 0.05,
       legend=c(iris_means$Species), title="Species",
       fill=color_pal, border=F)


### a better histogram
hist(iris$Sepal.Length)
# change the break points
hist(iris$Sepal.Length, breaks = seq(4,8, by=0.25))
# separating out the species
hist(iris[Species == "setosa", Sepal.Length], breaks = seq(4,8, by=0.25), col=color_pal[1])
hist(iris[Species == "versicolor", Sepal.Length], breaks = seq(4,8, by=0.25), col=color_pal[2])
hist(iris[Species == "virginica", Sepal.Length], breaks = seq(4,8, by=0.25), col=color_pal[3])
# combine into one plot with transparency and no border boxes
hist(iris[Species == "setosa", Sepal.Length], breaks = seq(4,8, by=0.25),
     col=adjustcolor(color_pal[1],0.5), border=F,
     xlab="Sepal length (cm)", main="")
hist(iris[Species == "versicolor", Sepal.Length], breaks = seq(4,8, by=0.25),
     col=adjustcolor(color_pal[2],0.5), border=F,
     add=T)
hist(iris[Species == "virginica", Sepal.Length], breaks = seq(4,8, by=0.25),
     col=adjustcolor(color_pal[3],0.5), border=F,
     add=T)
# legend
legend("topright", inset = 0.05,
       legend=c(iris_means$Species), title="Species",
       fill=adjustcolor(color_pal,0.5), border=F)


###############################
### Custom plotting windows ###
###############################

### multiple axes

plot(P ~ time, data = madeup, type="l", xlab="Year", ylab="Annual atmospheric P deposition (tons)")
plot(biomass ~ time, data = madeup, type="l", xlab="Year", ylab="Phytoplankton biomass (tons)")

# making room for the right axis label
par(mar=c(5,4.1,1.5,4.1)) 
plot(P ~ time, data = madeup, type="l", xlab="Year", ylab="",
     lwd=3, col="red")
# printing the axis label separately lets you change the color
mtext("Annual atmospheric P deposition (tons)", side=2, line=2.5, col="red") 
# allows plotting over existing
par(new=T) 
# overlay plot
plot(biomass ~ time, data = madeup, type="l",
     lwd=2, col="blue", lty=3,
     axes=F, bty="n", xlab="", ylab="") # get rid of axes and plot border
# custom axis
P_range <- range(madeup$biomass)
axis(side=4, at=pretty(P_range))
# custom axis label
mtext("Phytoplankton biomass (tons)", side=4, line=2.5, col="blue")
# legend
legend("topleft", inset=0.05, legend=c("P deposition", "Phytoplankton"), lty=c(1,3), lwd=c(3,2), col=c("red", "blue"))


### multiple panels

# defining number of rows and columns in plotting window
par(mfrow=c(1,2))
#
plot(Sepal.Length ~ Petal.Length, data = iris,
     col = color_pal[as.factor(Species)],
     pch = 19,
     xlab = "Petal length (cm)", ylab="Sepal length (cm)",
     bty="n")
# legend
legend("topleft", inset = 0.05,
       legend=c(iris_means$Species), title="Species",
       pch=19, col=color_pal)
# plot 2
par("mar") # current plotting margins (bottom, left, top, right)
par(mar=c(7, 4.1, 1.5, 4.1)) # making room on the bottom for trail labels

barplot(as.matrix(iris_means[,-"Species"]), beside=T,
        col=color_pal, border=F,
        xlab="", ylab="Mean size (cm)",
        names.arg = c("Sepal length", "Sepal width", "Petal width", "Petal width"),
        las=2) # rotating axis tick labels


# just resetting the graphical parameters
par(mfrow=c(1,1), mar=c(5, 4.1, 1.5, 4.1))



#################################
### Easily visualizing models ###
#################################

library(visreg) # a MAGIC package

# running a simple linear model of sepal length predicted by sepal width, with the effect differing by species
model <- lm(Sepal.Length ~ Sepal.Width*Species, data = iris)

# you can run visreg() on most types of models
visreg(model) # note the helpful warnings

# we can specify how we want the model to be visualized
visreg(model, "Sepal.Width", by="Species", overlay=T)



#######################
### Manual tweaking ###
#######################

# save a "Scalable Vector Graphics" file
# width and height are in inches - determines the relative sizes of fonts and plotting elements
svg("/home/michael/Documents/Grad School/Teaching/r_workshops/my_figure.svg", 
    width=9, height=5) 

  par(mfrow=c(1,2), mar=c(5, 4.1, 1.5, 4.1))
  plot(Sepal.Length ~ Petal.Length, data = iris,
       col = color_pal[as.factor(Species)],
       pch = 19,
       xlab = "Petal length (cm)", ylab="Sepal length (cm)",
       bty="n")
  legend("topleft", inset = 0.05,
         legend=c(iris_means$Species), title="Species",
         pch=19, col=color_pal)
  par(mar=c(7, 4.1, 1.5, 4.1))
  barplot(as.matrix(iris_means[,-"Species"]), beside=T,
          col=color_pal, border=F,
          xlab="", ylab="Mean size (cm)",
          names.arg = c("Sepal length", "Sepal width", "Petal width", "Petal width"),
          las=2)

dev.off() # turn off the graphical device (usually RStudio has its own graphical device running)
