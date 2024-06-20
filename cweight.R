#Data Layer:   Factors!

movies <- read.csv(file.choose()) #MovieRating.csv
head(movies)
summary(movies)
str(movies)     
# lets change the names to a shorter version
colnames(movies) <- c( "Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
tail(movies)
#    make the year a "factor" for grouping, not treat it like an int
movies$Year <-factor(movies$Year)
str(movies)


#    aesthetics
library(ggplot2)
ggplot( data= movies, aes(x=CriticRating, y= AudienceRating))
# geometry
ggplot( data= movies, aes(x=CriticRating, y= AudienceRating)) + geom_point()
# add color
ggplot( data= movies, aes(x=CriticRating, y= AudienceRating, color=Genre)) + 
  geom_point()
# add size 
ggplot( data= movies, aes(x=CriticRating, y= AudienceRating, 
                          color=Genre, size= BudgetMillions))    + geom_point()


# add different layers
p <-ggplot( data= movies, aes(x=CriticRating, y= AudienceRating, 
                              color=Genre, size= BudgetMillions)) 

# now we can add “Layers” to our blank chart
p+ geom_point()
p+ geom_line()    # ugly
p + geom_point() + geom_line() # possible
p + geom_line() + geom_point() # possible

# There are more geometry, we will see later


# override aesthetics
q <-ggplot( data= movies, aes(x=CriticRating, y= AudienceRating, 
                              color=Genre, size= BudgetMillions)) 
#example 1
q + geom_point(aes(size=CriticRating))
#example 2
q + geom_point(aes(color= BudgetMillions))
#example 3
q + geom_point(aes(x=BudgetMillions))
# ooops , look at the x label, it still says "CriticRating"
#example 4  fixing that
q + geom_point(aes(x=BudgetMillions)) + xlab("Budget in millions")
#example 5
q + geom_line(size=1) + geom_point()
q + geom_line(size=1) + geom_point(size=2)


r <-ggplot( data= movies, aes(x=CriticRating, y= AudienceRating)) 
r + geom_point()

# add color using mapping
r+ geom_point(aes(color=Genre))
# add color using setting 
r + geom_point(color="DarkGreen")

# add size using mapping
r+ geom_point(aes(size=BudgetMillions))
# add size using setting
r+ geom_point(size=3)


# Histograms 
s <-ggplot( data= movies, aes(x=BudgetMillions)) 
s + geom_histogram(binwidth = 10)  # bar represents 10 points
s + geom_histogram(binwidth = 5)  # bar represents 5 points
s + geom_histogram(binwidth = 30)  # bar represents 30 points
s + geom_histogram(binwidth = 10, fill="Red")   #setting
s + geom_histogram(binwidth = 10, aes(fill=Genre)) #mapping
s + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black")

# Density
s + geom_density()
s + geom_density(aes(fill=Genre))
s + geom_density(aes(fill=Genre), position="Stack")



# method 1
t <- ggplot(data=movies, aes(x=AudienceRating))
t + geom_histogram(binwidth=10, fill="White", color="Blue")
# method 2
t <- ggplot(data=movies)
t + geom_histogram(binwidth=10,
                   aes(x=AudienceRating),fill="White", color="Blue")
# which one is better?  method 2 gives more freedom
t + geom_histogram(binwidth=10,
                   aes(x=CriticRating),fill="White", color="Blue")



u <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre))
u + geom_point()
u + geom_point() + geom_smooth()
u + geom_point() + geom_smooth(fill=NA)
# boxplot
v <- ggplot(data=movies, aes(x=Genre, y=AudienceRating, color=Genre))
v  + geom_point()
v + geom_boxplot()
v + geom_boxplot(size=1.2)
v + geom_boxplot(size=1.2) + geom_point()
v + geom_boxplot(size=1.2) + geom_jitter()  # What does jitter do?
v + geom_jitter() + geom_boxplot(size=1.2) 
v + geom_jitter() + geom_boxplot(size=1.2, alpha=0.3)  # Cute
# Now you draw the same chart but for Critic Rating


b <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre))
b + geom_point(size=2)

#facets
b + geom_point(size=2) + facet_grid(Genre~.)
b + geom_point(size=2) + facet_grid(.~Year)
b + geom_point(size=2) + facet_grid(Genre~Year)
b + geom_point(size=2) + geom_smooth() + facet_grid(Genre~.)
b + geom_point(aes(size=BudgetMillions)) + geom_smooth() + facet_grid(Genre~Year)


b <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre))
b + geom_point(size=2)


#Facets

v<- ggplot(data=movies, aes(x=BudgetMillions))
v + geom_histogram(bin=10, aes(fill=Genre), color="Black")
# Hard to see individual groups
# so do Facets: each group in its own mini chart
v + geom_histogram(bin=10, aes(fill=Genre), color="Black") +
  facet_grid(Genre~.)
#You can also free the scales: 
v + geom_histogram(bin=10, aes(fill=Genre), color="Black") +
  facet_grid(Genre~., scales="free")



m <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, size=BudgetMillions,color=Genre))
m + geom_point()
# lets zoom in on the successes
m + geom_point() + xlim(50,100) + ylim(50,100)
# try zooming on the failures

# another better way
m + geom_point() +coord_cartesian(ylim=c(50,100))
m + geom_point() +coord_cartesian(xlim=c(50,100),ylim=c(50,100))


o <- ggplot(data=movies, aes(x=BudgetMillions))
h <- o + geom_histogram(binwidth=10, aes(fill=Genre), color="Black")
h
# 1. Axes Labels
h + xlab("Money Spent") +
  ylab("Number of Movies")
# 2. Label formatting
h + xlab("Money Spent") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Red", size=20))

# 3. Tick mark formatting
h + xlab("Money Spent") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Red", size=20),
        axis.text.x =  element_text(size=20),
        axis.text.y = element_text(size=20))
# what else can we work on

# 4. Legend formatting
h + xlab("Money Spent") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Red", size=20),
        axis.text.x =  element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))


# 5. Title
h + xlab("Money Spent") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Red", size=20),
        axis.text.x =  element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        plot.title = element_text(color="DarkBlue",size=30))

library('ggthemes')
h + xlab("Money Spent") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(color="DarkGreen", size=20),
        axis.title.y = element_text(color="Red", size=20),
        axis.text.x =  element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        plot.title = element_text(color="DarkBlue",size=30))