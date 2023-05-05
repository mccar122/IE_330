#Hello

x = 8
x <- 17
library(ggplot2)

a = c(4,2,15,47,11)

library(readxl)
c = read_excel("L3.xlsx", sheet = "Sheet1")

bakerydata = read.csv(file.choose(), head=T)
# info use summary(bakerydata)
# 3rd largest cake sale
##sort(bakerydata$Cakes, decreasing=TRUE)[3]
# 5 smallest value for smoothie
## sort(bakerydata$Smoothie)[5] 
## OR sort(bakerydata$Smoothie, decreasing=FALSE)[5]
# Total sale of cake when no promotion
## sum(bakerydata$Cakes[bakerydata$promotion=='none'])
# how many times did they offer promotion
## sum(bakerydata$promotion=='promotion')
# what is 80th percentile of smoothies
## quantile(bakerydata$Smoothies, 0.80)
# what is chance of selling >= 100 cakes in a day
## length(bakerydata$Cakes[bakerydata$Cakes>=100])/length(bakerydata$Cakes)
# make histogram of sale of cookies
## hist(bakerydata$Cookies)
# draw a boxplot for the sale of cakes, pies, and smoothies
## boxplot(bakerydata$Cakes, bakerydata$Pies, bakerydata$Smoothies, names=c('Cakes', 'Pies', 'Smoothies'), xlab = 'Product', ylab = 'Sales')
