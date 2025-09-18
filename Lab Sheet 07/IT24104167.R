setwd("C:\\Users\\User\\OneDrive\\Desktop\\Lab7")
getwd()
##Question1
##Uniform Distribution
##Let the random variable X represent the number of minutes the train arrives after 8:00a.m.
##it ask to find p(10<x<25) = p(x<=25)-p(x<=10)
punif(25,min=0,max=40,lower.tail = TRUE)-punif(10,min=0,max=40,lower.tail = TRUE)
##Question2
##Exponential Distribution
##here,random variable x has exponential distribution with lambda = 0.5
##it ask to find p(x<=5)
##probability (<=),if"lower.tail" argumrnt equals to "TRUE"
pexp(5,rate=0.33,lower.tail = TRUE)
##Question3
##Normal Distribution
##here, random variable x has normal distribution with mean = 100 and sd=15
##part i
##It ask to find p(x>130).
##we need to set our probability with (<=).
##here , p(x>130)= 1-p(x<=130)
1-pnorm(130,mean = 100,sd=15,lower.tail = TRUE)
##part ii
##It ask to find output of p(X<=x)=0.95
qnorm(0.95,mean = 100,sd=15,lower.tail = TRUE)
