getwd()
setwd('C:\\Users\\it24104167\\Downloads\\IT24104167')
getwd()

##import dataset
Delivery_Times <- read.table("Exercise - Lab 05.txt", header = TRUE, sep = ",")
Delivery_Times
##to get dataset in new window
fix(Delivery_Times) 
##rename variables in dataset
names(Delivery_Times) <-c("X1") 
fix(Delivery_Times)
##call variable by their name
attach(Delivery_Times) 

##histogram
histogram <-hist(X1, main="Delivery Time (minutes)")

##Draw a histogram using 9 classes where the lower limit is 20 and an upper limit of 70.
histogram <-hist(X1, main="Delivery Time (minutes)", breaks = seq(20,70, length=9),right = FALSE)

##Construct the frequency distribution for the above specification.
##assign class limits of frequency distribution into a variable called "breaks"
breaks <- round(histogram$breaks)
breaks

##assign class limits of frequencies of the histogram into a variable called "freq"
freq <- histogram$counts
freq

##assign mid points of each class into a variable called "mids"
mids <- histogram$mids
mids

##creating the variable called "Classes" for frequency distribution
classes <- c()

##creating a "for loop" to assign classes of the frequency distribution into 'classes' variable created above
for (i in 1:length(breaks)-1) {
  classes[i] <- paste0("[",breaks[i],",",breaks[i+1],")")
}

##Obtaining frequency distribution by combing the values of "classes" & "freq" variables
##cbind command used to merge the columns with same length
cbind(Classes = classes, Frequency = freq)

##Portray the distribution in the form of a frequency polygon.
##draw frequency polygon to the same plot
lines(mids,freq)

##draw frequency polygon in a new plot
plot(mids,freq, type = 'l', main = "Frequency Polygon for Shareholders", xlab = "Delivery_Times", ylab = "Frequency", ylim = c(0,max(freq)))

##Portray the distribution in a cumulative frequency polygon (ogive).
##using "cumsum" command to get cumulative frequencies
cum.freq <- cumsum(freq)
cum.freq

##create a mull variable called "new"
new <- c()

##using "for" loop to store cumlative frequencies in order to get the ogive
for(i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}

##draw cumlative frequency polygon in a new plot
plot(breaks, new, type = 'l', main = "Cumlative Frequency Polygon for Shareholders", xlab = "Delivery_Times", ylab = "Cumlative Frequency", ylim = c(0,max(cum.freq)))

##obtain upper limit of each class along with its cumlative frequency in a table
cbind(Upper = breaks, CumFreq = new)
plot(breaks, new, type = 'o', main = "Cumlative Frequency Polygon for Shareholders", xlab = "Delivery_Times", ylab = "Cumlative Frequency", ylim = c(0,max(cum.freq)))

