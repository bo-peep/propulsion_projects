#Investigating the GATT data from 1973 to 1979 to try and gain insights into whether it is economically 
#beneficial for a given country to join GATT. 

#A particular country was chosen at random to "deep dive" into. In this case, we will be looking at
#Thailand. 

#Read in the data and import the tidyverse library. 
setwd('C:/Users/julie/Propulsion/Juliet_Bowater/challenges/GATT_challenge')
library(tidyverse)
data = read.csv('GATT_TR_KR.csv')
head(data)

#Get quick insight into the dataset
colnames(data)
nrow(data)
ncol(data)

#How many transactions were there per country?
count <- data %>% count(rep_code, sort = TRUE)
count

#Let's visualise the transactions of the biggest traders, by year and sum of trades. 

#Top traders by count of trades
top_traders <- data %>% count(rep_code, sort = TRUE) %>% head(10)
top_traders$rep_code <- factor(top_traders$rep_code, levels = top_traders$rep_code[order(desc(top_traders$n))])

plot(top_traders)
ggplot(data = top_traders, aes(x= rep_code, y = n)) + 
  geom_point() +
  ggtitle("Top traders '73 to '79 (by number of trades)" ) +
  ylab("count of trades") +
  xlab("country code")

#UK, West Germany, USA, Netherlands and then France top the list (in that order)

#Top traders by sum of trades
highest_trade <- data %>% group_by(rep_code) %>% summarise(sum(trade)) %>% arrange(desc(`sum(trade)`)) %>% head(10)
highest_trade$rep_code <- factor(highest_trade$rep_code, levels = highest_trade$rep_code[order(desc(highest_trade$`sum(trade)`))])

plot(highest_trade)
ggplot(data = highest_trade, aes(x= rep_code, y = `sum(trade)`)) + 
  geom_point() +
  ggtitle("Top traders '73 to '79 (by sum of trade values)" ) +
  ylab("trade in $") +
  xlab("country code")

#USA, West Germany, UK, France and the Netherlands top this list (in that order)
#USA didn't trade as many times as the UK, but the trades must have been of higher value. 

#How did the global sum of trades change across the years?
data %>% group_by(year) %>% summarise(sum(trade))
plot(data %>% group_by(year) %>% summarise(sum(trade)), main = 'Global trade trend by year', col = 'blue', pch = 17)

#Looks like there is something wrong with the data here. The total for 1979 is far too high. 
#Let's dig into what the problem is. 

#The dataset has a huge number of columns. Let's trim these down before digging into what is wrong. 
#Select columns of interest
df <- data %>% select(X, year, rep_code, part_code, gdp_rep, gdpp_rep, distance, gatt_dejure_rep, comm_code, trade)
head(df)

#Check for duplicates in X (apparent old index column)
nrow(df)
#1426985
length(unique(df$X))
#1426985
df %>% count(X) %>% arrange(desc(n))
#no duplicates in X

#There are no duplicates in the old index, but dupes may still be present. 

#Let's get rid of X for the moment, as it doesn't carry any useful data.
df <- data %>% select(year, rep_code, part_code, gdp_rep, gdpp_rep, distance, gatt_dejure_rep, comm_code, trade)
head(df)

#Looking at a smaller subset of the 1979 data, we can see that there are dupes present. 
df %>% filter(rep_code == '841' & year == '1979') %>% arrange(desc(part_code), trade) #USA data
df %>% filter(rep_code == '764' & year == '1979') %>% arrange(desc(part_code), trade) #Thailand data

#We will drop all duplicates across the dataframe. 
df <- df %>% distinct()

#The global sum of trades per year now looks more sensible
plot(df %>% group_by(year) %>% summarise(sum(trade)), main = 'Global trade trend by year', col = 'blue', pch = 17)

#Take a look at the number of trades by country

top_traders <- df %>% count(rep_code, sort = TRUE) %>% head(10)
top_traders$rep_code <- factor(top_traders$rep_code, levels = top_traders$rep_code[order(desc(top_traders$n))])

plot(top_traders)
ggplot(data = top_traders, aes(x= rep_code, y = n)) + 
  geom_point() +
  ggtitle("Top traders '73 to '79 (by number of trades)" ) +
  ylab("count of trades") +
  xlab("country code")

#Looking at this, it becomes obvious that we only have a subset of the data, 
#up to a max of 11375-11379 transactions per country. This makes it impossible to find the top 
#country by number of trades. 
#Moving on...

#Look at the GDP(P) for Thailand between 1973 and 1979.The GDP and GDP(P) are reversed in the dataset.
plot(df %>% filter(rep_code == '764') %>% distinct(year, gdp_rep), main = 'Thailand GDP by year', col = 'red', pch = 10)
#Seems to have an upwards trend


#World average GDP(P) over time by GATT and non GATT
#first filter out rows with na gdp value

gdpp_world <- df %>% filter(!is.na(gdp_rep)) %>% group_by(year, gatt_dejure_rep) %>% summarise(mean_gdp = mean(gdp_rep))
ggplot(gdpp_world, aes(x=year, y=mean_gdp, color=gatt_dejure_rep)) +
  geom_point()

#Can we add Thailand to the above plot?
gdpp_thai <- df %>% filter(rep_code == '764') %>% distinct(year, gdp_rep)
#add a dummy third option for GATT membership, so that Thailand can be singled out.
gdpp_thai$gatt_dejure_rep = 3
#make sure that the colunm titles match
names(gdpp_thai)[2]<-"mean_gdp"

#combine the two tables
world <- bind_rows(gdpp_world, gdpp_thai)

#plot
world$gatt_dejure_rep <- as.factor(world$gatt_dejure_rep)
ggplot(world, aes(x=year, y=mean_gdp, color=gatt_dejure_rep)) +
  geom_point()

#Yes!!
#We can see that Thailand is well below the world average for either GATT or non-GATT. 
#The GATT countries have a higher mean GDP(P), but the mean is probably not especially informative 
#here since GDP(P) will not be normally distributed. The monster ecomomies will be outliers. 

#To visualise this, we can plot a histogram of the GDP(P)s in 1979
gdpp_79 <- df %>% filter(year == '1979') %>% select(gdp_rep) %>% na.omit()
hist(gdpp_79$gdp_rep)
#very much not a normal distribution.


#Looks like it is better to compare growth of the ecomomy, 
#rather than overall size. 

#We can make a box-plot showing the growth of GATT vs non GATT countries,
#starting point is 1973 GDP (this should be 1).

#Make GDP dataframe (bear in mind this is the GDPP column due to switch).

GDP <- df %>% 
  select(year, rep_code, gdpp_rep, gatt_dejure_rep) %>% 
  filter(!is.na(gdpp_rep)) %>% 
  distinct(year, rep_code, .keep_all = TRUE) %>%
  arrange(rep_code)

head(GDP, 10)

#Normalize each year to the 1973 value
#first, 'pivot' the data using spread. 
norm <- spread(GDP, year, gdpp_rep, fill = NA, convert = FALSE)
#normalize to 1973 by just dividing by that column.
norm[,3:9] <- norm[,3:9] / norm[,3]
#drop any rows containing na values
norm <- na.omit(norm)
head(norm)

#gather the columns back together so that we can make a boxplot
gdp_norm <- gather(norm,year,gdp_nom,'1973':'1979')

#make a boxplot by year
gdp_norm$year <- as.factor(gdp_norm$year)
gdp_norm$gatt_dejure_rep <- as.factor(gdp_norm$gatt_dejure_rep)

g <- ggplot(gdp_norm, aes(x = year, y = gdp_nom))
g + geom_boxplot()

#split this by GATT vs non-GATT

g2 <- g + geom_boxplot(
  aes(fill = gatt_dejure_rep),
  position = position_dodge(0.9) 
) +
  scale_fill_manual(values = c("#00AFBB", "#E69F00"))
g2

#lose the outliers

g2 <- g + geom_boxplot(
  outlier.shape = NA,
  aes(fill = gatt_dejure_rep),
  position = position_dodge(0.9)
) +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ylim(0.6,4)
g2

#It actually looks like the non-gatt countries are growing faster!

#Compare non-parametrically (wilcox test)
#install.packages("ggpubr")
library(ggpubr)
gdp_norm$year <- as.factor(gdp_norm$year)

#look at a single year first
w_74 <- gdp_norm %>%filter(year == '1974')

compare_means(formula = gdp_nom ~ gatt_dejure_rep,
              data = w_74,
              method = "wilcox.test",
              paired = FALSE)
#significant difference

#now compare the means for all years
compare_means(formula = gdp_nom ~ gatt_dejure_rep,
              data = gdp_norm,
              method = "wilcox.test",
              paired = FALSE,
              group.by = 'year')
#all years significantly different to varying degrees

#add the significance information to the graph
g2 <- g + geom_boxplot(
  outlier.shape = NA,
  aes(fill = gatt_dejure_rep),
  position = position_dodge(0.9)
) +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ylim(0.6,4) +
  stat_compare_means(aes(group = gatt_dejure_rep))
g2
#AWESOME!! (Though needs to be quite big to see it properly.)

#Can easily replace with stars:

g2 <- g + geom_boxplot(
  outlier.shape = NA,
  aes(fill = gatt_dejure_rep),
  position = position_dodge(0.9)
) +
  scale_fill_manual(values = c("#00AFBB", "#E69F00")) +
  ylim(0.6,4) +
  stat_compare_means(aes(group = gatt_dejure_rep), label = 'p.signif')
g2

#So it looks like non-GATT countries grew significantly faster during this period than GATT ones, 
#as a proportion of their starting GDP. 
#It's possible that this was because the larger countries were largely in GATT, and they might grow
#less as a proportion of their starting point. We would need to investigate that separately

#############################
#What might have happened over this time period if Thailand had been in GATT?

#Compare Thailand and a gdp doppleganger that was in GATT
#What was Thailand's starting point?
df %>% filter(rep_code == '764' & year =='1973') %>% head(1)
#GDP was 10,838,593,717

#Find another country that was between 10.5 and 11 billion and was in GATT
df %>% filter(gdpp_rep > 10500000000 
              & gdpp_rep < 11000000000 
              & gatt_dejure_rep == 1) %>%
  distinct(rep_code)

#only one canidate - 604! That's Peru

tnp <- df %>% filter(rep_code %in% c(764, 604)) %>% distinct(year, rep_code, .keep_all = TRUE)
tnp$year = as.factor(tnp$year)
tnp$rep_code = as.factor(tnp$rep_code)


ggplot(tnp, aes(x=year, y=gdpp_rep, color=rep_code)) +
  geom_point()

#Peru actually did worse than Thailand!
#This is interesting to look at, but holds no statistical significance. 

#######################

#Seems like we will have to do more investigations before deciding whether it is economicelly
#beneficial for countries to be in GATT...


