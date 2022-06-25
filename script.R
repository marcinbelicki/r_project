library(zeallot)
library(ggplot2)

data = read.csv("vgsales.csv", sep = ",")

list2env(data ,.GlobalEnv)

# 1
ggplot(
  data = data,
  aes(
    y = Global_Sales,
    x = Rank 
  )
)+
  geom_line()

# 2
description_statistics = function (data) {
  
  number_of_records = length(Global_Sales)
  number_of_records_inverted = 1/number_of_records
  
  mean = mean(Global_Sales)
  
  variance = var(Global_Sales) * (number_of_records - 1) * number_of_records_inverted
  
  standard_deviation = variance ^ .5
  inside = (Global_Sales - mean)/standard_deviation
  
  assymetry = sum(inside^3)*number_of_records_inverted
  curtosis  = sum(inside^4)*number_of_records_inverted - 3
  
  c(min, first_quartil, median, third_quartil, max) %<-% fivenum (Global_Sales)
  
  interquartile_range = third_quartil - first_quartil
  
  quartil_deviation = interquartile_range * .5
  
  mean_quartil = (first_quartil + third_quartil) * .5
  
  quartil_assymetry_coefficient = (mean_quartil - median) / quartil_deviation
  
  quartil_variance_coefficient = quartil_deviation / median * 100
  
  list(
    number_of_records = number_of_records,
    mean = mean,
    standard_deviation = standard_deviation,
    
    assymetry = assymetry,
    curtosis = curtosis,
    
    min = min,
    first_quartil = first_quartil,
    median = median,
    third_quartil = third_quartil,
    max = max,
    
    interquartile_range = interquartile_range,
    quartil_deviation = quartil_deviation,
    mean_quartil = mean_quartil,
    
    quartil_assymetry_coefficient = quartil_assymetry_coefficient,
    quartil_variance_coefficient = quartil_variance_coefficient
  )

  
}

Global_Sales_stats = description_statistics(Global_Sales)
#3
data$Publisher
data$Genre
data$Year

#4
ggplot(
  data = data,
  aes(
    y = NA_Sales,
    x = Rank 
  )
)+
  geom_line()

ggplot(
  data = data,
  aes(
    y = EU_Sales,
    x = Rank 
  )
)+
  geom_line()

ggplot(
  data = data,
  aes(
    y = JP_Sales,
    x = Rank 
  )
)+
  geom_line()

#5


