library(ggplot2)

inc <- read.csv(file.choose()) # Open Income.csv
head(inc) # 5 variables
# Lets look around first
qplot( x = inc$Age , y= inc$Income) # I guess income depends on Age
qplot( x = inc$Education , y= inc$Income) # A little bit on Education
qplot( x = inc$Gender , y= inc$Income) # not so much on gender
results = lm ( inc$Income~inc$Age+inc$Education+inc$Gender )
results
summary(results)


inc <- read.csv(file.choose())
head(inc) # 5 variables
# Lets look around first
qplot( x = inc$Age , y= inc$Income) # I guess income depends on Age
qplot( x = inc$Education , y= inc$Income) # A little bit on Education
qplot( x = inc$Gender , y= inc$Income) # not so much on gender
results2 = lm ( inc$Income~inc$Age+inc$Education )
results2
summary(results2)



# Generate a linearly relation
x <- runif(75,0,10) # 75 random numbers of uniform distribution
x <- sort(x)
y <- 20 + 10*x + rnorm(75,0,10) # y= 20+10x and a bit of variety
# see it!
plot(x,y)
# Now lets do the regression
lr <- lm(y~x)
lr # print out the deducted equation
# draw the linear regression line
points( x, lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )


x <- runif(75,0,10) # 75 random numbers of uniform distribution
x<- sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)
plot(x,y)
lr <- lm(y~x)
lr
points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )

x <- runif(75,0,10) # 75 random numbers of uniform distribution
x<- sort(x)
y <- 200 + x^3 - 10 * x^2 + x + rnorm(75,0,20)
plot(x,y)
lr <- lm(y~x)
lr
poly <- loess(y~x) # Polynomial regression
fit <- predict(poly)
points(x,fit, type="l", col=2)
points(x,lr$coefficients[1] + lr$coefficients[2] * x, type="l", col=4 )


churn_input <- read.csv(file.choose()) # open the Churn.csv file on blackboard
head(churn_input)
sum(churn_input$Churned)
# Lets look around first at those who actually churned
ch <- churn_input[churn_input$Churned=="1",]
qplot( x = ch$Churned_contacts )
qplot( x = ch$Age )
qplot( x = ch$Married )
qplot( x = ch$Cust_years)
# Now let's see those who did not churn
nch <- churn_input[churn_input$Churned=="0",]
qplot( x = nch$Churned_contacts )
qplot( x = nch$Age )
qplot( x = nch$Married )
qplot( x = nch$Cust_years)



Churn_logistic1 <- glm (Churned~Age + Married + Cust_years + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
Churn_logistic1
summary(Churn_logistic1) # Eliminate Married and Cust_years
Churn_logistic2 <- glm (Churned~Age + Churned_contacts,
                        data=churn_input, family=binomial(link="logit"))
Churn_logistic2
summary(Churn_logistic2)


# read a csv file
# method 1 : select manually
stats <- read.csv(file.choose())
stats



nrow(stats) # number of rows
ncol(stats) # number of columns
head(stats) # first 6 rows
tail(stats) # last 6 rows
str(stats) #structure, Factor W/4 means 4 different values 1,2,3,4
# you can factorize a column by using factor()
summary(stats)

#---------using the $ sign in a data frame
stats[3,3] # row 3 col 3
stats[3,3, drop=F] # row 3 col 3, remember this from last week?
stats[3, "Birth.rate"] #rows do not have names in a data frame
stats$Internet.users # extract a column vector
stats[, "Internet.users"] # same thing, but not as elegant
stats$Internet.users[3] # Angola
levels(stats$Income.Group)
summary(stats$Birth.rate)


stats[1:10,]
stats[2:7,]
stats[c(4,16,100),]
stats$Birth.rate * stats$Internet.users # meaningless, just testing math
stats$Birth.rate + stats$Internet.users # meaningless, just testing math
# Add a column
stats$MyCalc <- stats$Birth.rate * stats$Internet.users
stats
stats$xyz <- 1:5 # add another column
stats
# remove a column
stats$xyz <-NULL
stats$MyCalc <- NULL
stats


# -----------------------------filtering data frames
# Slow way of doing it
filter <- stats$Internet.users < 2 # which one less than 2 percent
stats[filter,]
#Better way of filtering
stats[stats$Birth.rate > 40 & stats$Internet.users < 2,] # one step
stats[stats$Income.Group == "High income", ]
stats[stats$Country.Name == "Egypt", ]

?qplot
# One variable
qplot(stats$Birth.rate) #ok but not prefered
qplot(data=stats,x=Internet.users) #better method, you get a histogram
qplot(data=stats,x=Internet.users, bins=10) #better method
# Two variables
qplot(data=stats, x = Income.Group, y= Birth.rate)
qplot(data=stats, x = Income.Group, y= Internet.users)
# Test the different sizes
qplot(data=stats, x = Income.Group, y= Birth.rate, size=I(10)) #default size is 1
# Test the different Colors
qplot(data=stats, x = Income.Group, y= Birth.rate, size=I(6), color=I("blue"))

# Test the different charts qplot can draw
qplot(data=stats, x = Income.Group, y= Birth.rate, geom="boxplot")
qplot(data=stats, x = Income.Group, y= Birth.rate, geom="path")
#qplot(data=stats, x = Income.Group, y= Birth.rate, geom="dotplot")
qplot(data=stats, x = Income.Group, y= Birth.rate, geom="point") # default
qplot(data=stats, x = Internet.users, y= Birth.rate, geom="boxplot")
qplot(data=stats, x = Internet.users, y= Birth.rate, geom="path")
#qplot(data=stats, x = Internet.users, y= Birth.rate, geom="dotplot")
qplot(data=stats, x = Internet.users, y= Birth.rate, geom="point") # default


# Three variables
qplot(data=stats, x=Internet.users, y=Birth.rate)
qplot(data=stats, x=Internet.users, y=Birth.rate, size=I(4))
qplot(data=stats, x=Internet.users, y=Birth.rate, color=I("red"))
qplot(data=stats, x=Internet.users, y=Birth.rate, color= Income.Group)
qplot(data=stats, x=Internet.users, y=Birth.rate, color= Income.Group, size=I(4))
qplot(data=stats, y=Internet.users, x=Birth.rate, color= Income.Group, size=I(2))
















