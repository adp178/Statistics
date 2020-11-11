cat4_data = read.csv('datasets/AtlanticHurricaneCat4.csv')
cat4_data$X = NULL # Remove unnecessary column

addDecadeCategory = function(data) {
    data$Decade = -1
    return(data)
} 
cat4_data_with_decade = addDecadeCategory(cat4_data)

updateDecadecategory = function(data) {
    for (i in 1:nrow(data))
    {
        row = data[i,]
        year = row$Year
        cat = floor((year - 1851)/10) + 1
        data[i,'Decade'] = 1850 + 10*(cat-1)
    }
    return(data)
}
data_prepared = updateDecadecategory(cat4_data)

head(data_prepared)

getmode = function(v) {
   uniqv = unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_measures_of_location = function(df)
{
    m = mean(df)    
    med = median(df)
    mod = getmode(df)
    return (list(m,med,mod))
}

measures_wind_speed_list = get_measures_of_location(data_prepared$WindSpeed)
sprintf("Mean Wind Speed : %f km/hr",measures_wind_speed_list[1])
sprintf("Median Wind Speed : %f km/hr",measures_wind_speed_list[2])
sprintf("Mode Wind Speed : %f km/hr",measures_wind_speed_list[3])

measures_pressure_list = get_measures_of_location(data_prepared$Pressure)
sprintf("Mean Pressure : %f mbar",measures_pressure_list[1])
sprintf("Median Pressure : %f mbar",measures_pressure_list[2])
sprintf("Mode Pressure : %f mbar",measures_pressure_list[3])

get_measures_of_dispersion = function(v)
{
    std = sd(v)
    variance = var(v)
    coeff_of_variation = std / mean(v)
    minimum = min(v)
    maximum = max(v)
    r = maximum - minimum
    q = quantile(v)
    return (list(std,variance,coeff_of_variation,minimum,maximum,r,q))
}

measures_wind_speed_list = get_measures_of_dispersion(data_prepared$WindSpeed)
sprintf("Standard deviation of Wind Speed : %f",measures_wind_speed_list[1])
sprintf("Variance of Wind Speed : %f",measures_wind_speed_list[2])
sprintf("Coefficient of variation of Wind Speed : %f",measures_wind_speed_list[3])
sprintf("Minimum Wind Speed : %f km/hr",measures_wind_speed_list[4])
sprintf("Maximum Wind Speed : %f km/hr",measures_wind_speed_list[5])
sprintf("Range of Wind Speed : %f km/hr",measures_wind_speed_list[6])
sprintf("Quantiles of Wind Speed : ")
print(measures_wind_speed_list[7])

measures_pressure_list = get_measures_of_dispersion(data_prepared$Pressure)
sprintf("Standard deviation of Pressure : %f",measures_pressure_list[1])
sprintf("Variance of Pressure : %f",measures_pressure_list[2])
sprintf("Coefficient of variation of Pressure : %f",measures_pressure_list[3])
sprintf("Minimum Pressure : %f mbar",measures_pressure_list[4])
sprintf("Maximum Pressure : %f mbar",measures_pressure_list[5])
sprintf("Range of Pressure : %f mbar",measures_pressure_list[6])
sprintf("Quantiles of Pressure : ")
print(measures_pressure_list[7])

options(repr.plot.width = 4, repr.plot.height=4)
boxplot(data_prepared$WindSpeed, main="Wind Speed", ylab="Speed km/hr")

options(repr.plot.width = 4, repr.plot.height=4)
boxplot(data_prepared$Pressure, main="Pressure", ylab="Pressure mbar")

library('ggplot2')
library('fitdistrplus')

decadewise_hist = function(df) {
   options(repr.plot.width = 4, repr.plot.height=4)
   ggplot(data_prepared, aes(x = Decade)) + geom_bar()
}
decadewise_hist(df)

windspeed_hist = function(df) {
   ggplot(df, aes(x = WindSpeed)) +
    geom_histogram(color="black",fill="coral",binwidth=5)+geom_density(alpha=0.2, fill="red")   
}
windspeed_hist(data_prepared)

pressure_hist = function(df) {
    ggplot(df, aes(x = Pressure)) +
    geom_histogram(color="black",fill="blue",binwidth=5)
}
pressure_hist(data_prepared)

options(repr.plot.width = 5, repr.plot.height = 5)
plot(data_prepared$WindSpeed, data_prepared$Pressure, main="Scatterplot",xlab="WindSpeed ", ylab="Pressure ", pch=1)

getAverage = function(data)
{
    t = table(data$Decade)
    sm = 0
    cnt = nrow(t)
    for (i in 1:cnt)
    {
        sm = sm + t[i]
    }
    average = sm/cnt
    return (average)
}
average = getAverage(data_prepared)
sprintf("Average number of hurricanes per decade : %f",average)

getVariance = function(data)
{
    t = table(data$Decade)
    return (var(t))
}
variance = getVariance(data_prepared)

fitDistribution = function (average, dist="poisson")
{
    
    randomNums = seq(from=0,to=50,by=1)
    d1 = data.frame(v=randomNums)
    d2 = data.frame(p=dpois(randomNums,average))
    d3 = data.frame(v = d1, p = d2)
    options(repr.plot.width = 4, repr.plot.height = 4)
    ggplot(d3,aes(x=v,y=p)) + geom_point(size=1) + labs(x="Number of hurricanes",y="Probability",title="PMF of Poisson Distribution")
}
fitDistribution(average)

t = table(data_prepared$Decade)
df = data.frame(t)

qqcomp(fitdist(df$Freq,'nbinom'))

qqcomp(fitdist(df$Freq,'pois'))

sprintf("Mean of number of hurricanes per decade : %f",average)
sprintf("Variance of number of hurricanes per decade : %f",variance)

cat5_data = read.csv('datasets/AtlanticHurricaneCat5.csv')
cat5_data$X = NULL # Remove unnecessary column

addDecadeCategory = function(data) {
    data$Decade = -1
    return(data)
} 
cat5_data_with_decade = addDecadeCategory(cat5_data)

updateDecadecategory = function(data) {
    for (i in 1:nrow(data))
    {
        row = data[i,]
        year = row$Year
        cat = floor((year - 1851)/10) + 1
        data[i,'Decade'] = 1850 + 10*(cat-1)
    }
    return(data)
}
data_prepared = updateDecadecategory(cat5_data)

head(data_prepared)

getmode = function(v) {
   uniqv = unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

get_measures_of_location = function(df)
{
    m = mean(df)    
    med = median(df)
    mod = getmode(df)
    return (list(m,med,mod))
}

measures_wind_speed_list = get_measures_of_location(data_prepared$WindSpeed)
sprintf("Mean Wind Speed : %f mph",measures_wind_speed_list[1])
sprintf("Median Wind Speed : %f mph",measures_wind_speed_list[2])
sprintf("Mode Wind Speed : %f mph",measures_wind_speed_list[3])

measures_pressure_list = get_measures_of_location(data_prepared$Pressure)
sprintf("Mean Pressure : %f mbar",measures_pressure_list[1])
sprintf("Median Pressure : %f mbar",measures_pressure_list[2])
sprintf("Mode Pressure : %f mbar",measures_pressure_list[3])

measures_wind_speed_list = get_measures_of_dispersion(data_prepared$WindSpeed)
sprintf("Standard deviation of Wind Speed : %f",measures_wind_speed_list[1])
sprintf("Variance of Wind Speed : %f",measures_wind_speed_list[2])
sprintf("Coefficient of variation of Wind Speed : %f",measures_wind_speed_list[3])
sprintf("Minimum Wind Speed : %f mph",measures_wind_speed_list[4])
sprintf("Maximum Wind Speed : %f mph",measures_wind_speed_list[5])
sprintf("Range of Wind Speed : %f mph",measures_wind_speed_list[6])
sprintf("Quantiles of Wind Speed : ")
print(measures_wind_speed_list[7])

measures_pressure_list = get_measures_of_dispersion(data_prepared$Pressure)
sprintf("Standard deviation of Pressure : %f",measures_pressure_list[1])
sprintf("Variance of Pressure : %f",measures_pressure_list[2])
sprintf("Coefficient of variation of Pressure : %f",measures_pressure_list[3])
sprintf("Minimum Pressure : %f mbar",measures_pressure_list[4])
sprintf("Maximum Pressure : %f mbar",measures_pressure_list[5])
sprintf("Range of Pressure : %f mbar",measures_pressure_list[6])
sprintf("Quantiles of Pressure : ")
print(measures_pressure_list[7])

options(repr.plot.width = 4, repr.plot.height=4)
boxplot(data_prepared$WindSpeed, main="Wind Speed", ylab="Speed km/hr")

options(repr.plot.width = 4, repr.plot.height=4)
boxplot(data_prepared$Pressure, main="Pressure", ylab="Pressure mbar")

decadewise_hist = function(df) {
   options(repr.plot.width = 4, repr.plot.height=4)
   ggplot(data_prepared, aes(x = Decade)) + geom_bar()
}
decadewise_hist(df)

windspeed_hist = function(df) {
   ggplot(df, aes(x = WindSpeed)) +
    geom_histogram(color="black",fill="coral",binwidth=5)+geom_density(alpha=0.2, fill="red")   
}
windspeed_hist(data_prepared)

pressure_hist = function(df) {
    ggplot(df, aes(x = Pressure)) +
    geom_histogram(color="black",fill="blue",binwidth=5)
}
pressure_hist(data_prepared)

options(repr.plot.width = 5, repr.plot.height = 5)
plot(data_prepared$WindSpeed, data_prepared$Pressure, main="Scatterplot",xlab="WindSpeed ", ylab="Pressure ", pch=1)

getAverage = function(data)
{
    t = table(data$Decade)
    sm = 0
    cnt = nrow(t)
    for (i in 1:cnt)
    {
        sm = sm + t[i]
    }
    average = sm/cnt
    return (average)
}
average = getAverage(data_prepared)
sprintf("Average number of hurricanes per decade : %f",average)

getVariance = function(data)
{
    t = table(data$Decade)
    return (var(t))
}
variance = getVariance(data_prepared)

fitDistribution = function (average, dist="poisson")
{
    
    randomNums = seq(from=0,to=50,by=1)
    d1 = data.frame(v=randomNums)
    d2 = data.frame(p=dpois(randomNums,average))
    d3 = data.frame(v = d1, p = d2)
    options(repr.plot.width = 4, repr.plot.height = 4)
    ggplot(d3,aes(x=v,y=p)) + geom_point(size=1) + labs(x="Number of hurricanes",y="Probability",title="PMF of Poisson Distribution")
}
fitDistribution(average)

t = table(data_prepared$Decade)
df = data.frame(t)

qqcomp(fitdist(df$Freq,'nbinom'))

qqcomp(fitdist(df$Freq,'pois'))

sprintf("Mean of number of hurricanes per decade : %f",average)
sprintf("Variance of number of hurricanes per decade : %f",variance)


