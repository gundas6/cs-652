stats <- read.csv(file.choose())
stats
#question 1
stats[stats$Country.Name == "France", ]
stats
#Show the countries with birthrate >20 and High income
data<- stats[stats$Birth.rate > 20 & stats$Income.Group == "High income", ]
print(data)

# Show the data of the country with the highest internet users
max_internet_user_country <- stats[stats$Internet.users == max(stats$Internet.users, 
                                                               na.rm = TRUE), ]
print(max_internet_user_country)

#Average birth Rate
Avg_birthrate <- mean(stats$Birth.rate, na.rm = TRUE)
print(Avg_birthrate)

#Standard Deviation of birthrate
SD_birthrate <- sd(stats$Birth.rate, na.rm = TRUE)
print(SD_birthrate)


# Countries whose birth rate is two SDs above the mean
countries_above_2SDs <- stats[stats$Birth.rate > (Avg_birthrate + 2*SD_birthrate), ]

# Print the filtered data frame
print(countries_above_2SDs)

#IQR of the birthrate 
Iqr_birthrate <- IQR(stats$Birth.rate, na.rm = TRUE)
print(Iqr_birthrate)





