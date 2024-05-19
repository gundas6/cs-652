library(ggplot2)

inc <- read.csv(file.choose())
head(inc)
# Lets look around first
qplot( x = inc$distribution , y= inc$rownames) # I guess income depends on Age
qplot( x = inc$share , y= inc$rownames) # A little bit on Education
qplot( x = inc$price , y= inc$rownames) # not so much on gender
results = lm ( inc$rownames~inc$distribution+inc$share+inc$price )
results
summary(results)

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

#Polynomial regression
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

#Logistic regression

# Read in the data
inc_input <- read.csv(file.choose())  # open the file containing inc data
head(inc_input)

# Perform some summary statistics or visualizations
sum(inc_input$price)

# Let's explore the data a bit
high_price <- inc_input[inc_input$price > 106, ]
low_price <- inc_input[inc_input$price <= 106, ]

# Visualizing distribution for high and low price groups
library(ggplot2)
qplot(x = high_price$distribution, main = "High Price Group Distribution")
qplot(x = high_price$share, main = "High Price Group Share")

qplot(x = low_price$distribution, main = "Low Price Group Distribution")
qplot(x = low_price$share, main = "Low Price Group Share")

# Logistic regression: Suppose we want to predict if a price is higher than 106 based on distribution and share
inc_input$High_Price <- ifelse(inc_input$price > 106, 1, 0)

# First logistic regression model
Inc_logistic1 <- glm(High_Price ~ distribution + share, data=inc_input, family=binomial(link="logit"))
summary(Inc_logistic1)

# Based on the summary, we may eliminate non-significant predictors if necessary and rerun the model
# For demonstration, let's assume share is not significant and we eliminate it
Inc_logistic2 <- glm(High_Price ~ distribution, data=inc_input, family=binomial(link="logit"))
summary(Inc_logistic2)
