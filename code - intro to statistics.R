
#exercise 1
data = read.csv("MLB beer prices-csv_noavg.csv")   #excluding the preexisting averages in order to work with the raw data.
data<- na.omit(data)

library(tidyverse)

data_2013<- data[1:30,]
data_2014<-data[31:61,]
data_2015<-data[32:92,]
data_2016<-data[93:123,]
data_2018<-data[124:154,]  #dividing the complete dataset into yearly incraments for ease of use

PPO <- data[,7]   # PPO = price per ounce   
drinksize_total<-data[,6] 

drink_price <- PPO * drinksize_total # calculating drink price

RandomSIQR <- c(1:10000)   # Random Selection Intequadral Ratio - simple vector that will be used in the loop


for (i in 1: 10000){       # places into the vector RandomSIQR the IQR of a random sample of 100 observations
  RandomSIQR[i] <- IQR (sample(drink_price,100, replace = TRUE))
  
}
ggplot(data = data.frame(RandomSIQR), mapping = aes(x= RandomSIQR))+ geom_density()




#exercise 2
data = read.csv("MLB beer prices-csv_noavg.csv")   #excluding the preexisting averages in order to work with the raw data.
data<- na.omit(data)

#calculating the absolute deviation of PPO 

View(data)        # using pre-existing data for previous code.
PPO <- data[,7]   # PPO = price per ounce

sum <- 0

for (i in 1:150)
{
  sum <- sum + abs(PPO[i]-median(PPO))
}
ad <- sum / 150
print(ad)

#calculation of the ratio 

sampad <- rep (0,100)
ad_ratio <- rep (0,100)

for (b in 1:100)
{
  samp <- sample(PPO, 50, replace = FALSE)
  sum2 <- 0
  for (j in 1:50)
  {
    sum2 <- sum2 + abs(samp[j]-median(samp)) 
  }
  sampad[b] <- sum2 /50 
  ad_ratio[b] <- ad /sampad[b]   #creation of a new vector with the ratio
  
}
print(ad_ratio) #the vector mentioned above

#variance calculation

ad_ratio_var <- var(ad_ratio)
print(ad_ratio_var)        #the variance which we are going to refer to 




#exercise 3
data = read.csv("MLB beer prices-csv_noavg.csv")   #excluding the preexisting averages in order to work with the raw data.
data<- na.omit(data)

data_2013<- data[1:30,]
data_2014<-data[31:61,]
data_2015<-data[32:92,]
data_2016<-data[93:123,]
data_2018<-data[124:154,]  #dividing the complete dataset into yearly incraments for ease of use

beerprice_total<-data[,5]
drinksize_total<-data[,6]  #creating variables for future comparison.
cor_vector2 <- c(1:250)     #creating a vector with 250 slots

cor_value1 <-cor(beerprice_total,drinksize_total) # pearson correlation rank = 0.55
plot(beerprice_total,drinksize_total)

for (i in 1:250) {
  drinksize_sample<- sample(drinksize_total, size = 108, replace = FALSE)
  beerprice_sample<- sample(beerprice_total, size = 108, replace = FALSE)
  cor_vector2 [i] <- cor(beerprice_sample,drinksize_sample)
}
view(cor_vector2)
plot(cor_vector2)
max(cor_vector2)          #big difference in result between the full sample size and the random selection - will compare one year to ensure verify results.

# same loop - strictly using 2016 data

drinksize_sample2016 <- data_2016[,6]
beerprice_sample2016 <- data_2016[,5]    # veriable creation

cor_value2016 <- cor(beerprice_sample2016,drinksize_sample2016)  #correlation 0.51 - similar to total (-0.04)
for (i in 1:250)
{
  drinksize_Rsample2016 <- sample (drinksize_sample2016, size = 22, replace = FALSE)
  beerprice_Rsample2016 <-sample(beerprice_sample2016, size = 22, replace = FALSE)
  cor_vector2 [i] <- cor(beerprice_Rsample2016, drinksize_Rsample2016)
}
plot(cor_vector2)
max(cor_vector2)
min(cor_vector2)
sd (cor_vector2)
mean(cor_vector2)  # testing different attributes of the correlation - correctly appears to be completely random and thus we can assure that there is no monotone correllation.

#graphic depiction with ggplots

ggplot(data = data.frame(cor_vector2), mapping =  aes(x = cor_vector2)) + geom_histogram(bins = 30) + ggtitle("cor_value1: ", format(round(cor_value1, 2), nsmall = 2))




#exercise 4
normal_sample <- rnorm(1000)

difference <- 0
count <- 0

while (difference <= 0.01)
{
  sample20 <- sample(normal_sample, 200, replace = FALSE)  #200=20% of 1000
  a <- mean(sample20)
  md <- median(sample20) 
  difference <- abs(a-md)   #we added "abs" because we were asked about the gap, not about the difference
  count <- count + 1   #that is a way to count the amount of times the loop ran 
  
}

print(count)    #each time the loop runs it adds 1 to "count"




#exercise 5 

#there is no code for this section. 
#you can see the solution in the "word" file.




#exercise 6
COS <- 0 # Chances of success in Precentile
i <- 11
while (COS <= 0.90)
{
  i <- i+1
  Combination <- choose(i, 12)
  COS <-1-(choose(Combination,0) * (0.0000537)^0*(1-0.0000537)^(Combination))
}




#exercise 7
#shared code:
#data = read.csv("MLB beer prices-csv_noavg.csv")   #excluding the preexisting averages in order to work with the raw data.
#data<- na.omit(data)
#data_2013<- data[1:30,]
#data_2014<-data[31:61,]
#data_2015<-data[32:92,]
#data_2016<-data[93:123,]
#data_2018<-data[124:154,]  #dividing the complete dataset into yearly incraments for ease of use
#beerprice_total<-data[,5]
#drinksize_total<-data[,6]  #creating variables for future comparison.

view(data)        # using pre-existing data for previous code.
PPO <- data[,7]   # PPO = price per ounce   
library("tidyverse")

meanPPO<- mean(PPO)
medianPPO<-median(PPO)
sdPPO <- sd(PPO)
#creating a function for mode calculation

getmode<- function(v){
  uniqv<- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
# calculating the mode
modePPO <- getmode(PPO)
ggplot(data = data, mapping = aes(x=PPO)) + geom_density()

# attemptimg the same procces with differant variable due to odd result - possibly due to minimal variation
meanBPT <- mean(beerprice_total)   #BPT = beer price total
medianBPT <- median(beerprice_total)
modeBPT <- getmode(beerprice_total)
sdBPT <- sd(beerprice_total)
modeIndex <- c(1:150)
medianIndex <- c(1:150)
meanIndex <- c(1:150)             # creating veriables for future use
ggplot(data = data, mapping = aes(x = beerprice_total)) + geom_density()  # using ggplot to look at density graph the beer prices

# creating deviation from the mode function
getDModeP <- function(v){
  for (i in 1:150) {
    if (beerprice_total[i] == modeBPT)
    {
      modeIndex [i] <- 1
    }
    else
    {
      modeIndex [i] <- NA
    }
    
  }
  modeIndex <- na.omit(modeIndex)
  (length(modeIndex) / length(beerprice_total)) * 100
}
#calculating the deviation from the mode and inserting it into a new variable

DModeP_BPT <- getDModeP (beerprice_total)

#creating deviation from the median function - very similar to to deviation from the mode
getDMedianP <- function(v){
  for (i in 1:150) {
    if (beerprice_total[i] == medianBPT)
    {
      medianIndex [i] <- 1
    }
    else
    {
      medianIndex [i] <- NA
    }
    
  }
  medianIndex <- na.omit(medianIndex)
  (length(medianIndex) / length(beerprice_total)) * 100
}
#calculating the deviation from the median and inserting it into a new variable

DMedianP_BPT <- getDMedianP(beerprice_total)

#creating deviation from the mean function - again, very similar

getDMeanP <- function(v){
  for (i in 1:150) {
    if (beerprice_total[i] == meanBPT)
    {
      meanIndex [i] <- 1
    }
    else
    {
      meanIndex [i] <- NA
    }
    
  }
  meanIndex <- na.omit(meanIndex)
  (length(meanIndex) / length(beerprice_total)) * 100
  if ((length(meanIndex) / length(beerprice_total)) * 100 == 0)
  { ((length(meanIndex) / length(beerprice_total)) * 100)+100}
}
#calculating the deviation from the mean and inserting it into a new variable

DmeanP_BPT <- getDMeanP (beerprice_total)




#exercise 8
dataPOP = read.csv("popularity prices comparison-csv.csv")   #combined search history avrages and price per ounce
PpoByRank <- dataPOP [,2]          # Ppo = Price Per Pound
PopularityByRank <- dataPOP [,4]
cor_value <-cor(PpoByRank, PopularityByRank)
plot(PopularityByRank, PpoByRank)
ggplot(data = dataPOP, mapping = aes(x = PopularityByRank, y = PpoByRank)) + geom_point() + ggtitle("cor_value: ", format(round(cor_value, 2), nsmall = 2))




#exercise 9

#there is no code for this section. 
#you can see the solution in the "word" file.


