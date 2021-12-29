##-------------------------------------
## ASSIGNMENT 2
## Diego López, Aida Pinacho
## Ariadna Villanueva, Andrea Álvarez
##-------------------------------------

getwd()
setwd("~/MASTER/STATISTICAL DATA ANALYSIS")

# Load libraries
library(tidyverse)
library(reshape2)
library(viridis)
theme_set(theme_bw())

# Load the modified data
data <- read.table("billionaires_final.csv", sep = ",", header = T)

# Convert character columns and year to factors
character_vars <- lapply(data, class) == "character"
data[, character_vars] <- lapply(data[, character_vars], as.factor)
data$year <- as.factor(data$year)
str(data)

# Replace empty cells with NA
data[data==""] <- "NA"
data <- droplevels(data)

# Delete NA
nrow(data)
cat("Total number of NA: ",sum(colSums(is.na(data))))
data <- na.omit(data)
cat("Rows after deleting NA: ", nrow(data))

# Save dataframe
write.csv(data, file="billionaires_noNA.csv", row.names = FALSE)

##----------------------------------
## GRAFICAS PARA HACER: gender age wealth type y inherited
##----------------------------------

## 1. GENDER
data_gender <- data.frame(data$year, data$demographics.gender, as.numeric(data$wealth.worth.in.billions))
colnames(data_gender) <- c("Year", "Gender", "Wealth")
head(data_gender)

# Density plot
data_g2 <- data_gender[data_gender$Year == 2014,]
colnames(data_g2) <- c("Year", "Gender", "Wealth")
head(data_g2)
ggplot(data_g2, aes(x=Wealth, group=Gender, fill=Gender)) +
  geom_density(adjust=1.5, alpha=.8) +
  ggtitle("Wealth distribution according to gender (2014)") +
  xlim(-1, 20)
## In this representation, 30 rows are deleted due to the xlim 

# Boxplot
data_gender2 <- data_gender[data_gender$Gender != "married couple",]
ggplot(data_gender2, aes(x=Year, y=Wealth)) +
  geom_boxplot(aes(fill=factor(Gender)), outlier.shape = NA) +
  ggtitle("Wealth distribution according to gender") +
  labs(colour = "Gender") +
  scale_fill_discrete(name = "Gender") +
  ylim(0,10) +
  ylab("Billions")

## BAR STACKED PLOT
data_g2 <- data_gender[data_gender$Year == 2014,]
data_g <- matrix(0,nrow = 6, ncol = 3)
colnames(data_g) <- c("Year", "Gender", "Count")
data_g[,1] <- c(rep(1996, 2) , rep(2001, 2) , rep(2014, 2))
g <- c("male", "female")
data_g[,2] <- c(rep(g, 3))
data_g

for (i in 1:nrow(data_g)){
  year <- data_g[i,1]
  gender <- data_g[i,2]
  s <- 0
  for (j in 1:nrow(data)){
    if (data$year[j] == year && data$demographics.gender[j] == gender){
      s <- s + 1
    }
  }
  data_g[i,3] <- s
}
data_g <- as.data.frame(data_g)

library(hrbrthemes)
ggplot(data_g, aes(fill=Gender, y=as.numeric(Count), x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Wealth distribution according to gender across years") +
  theme(plot.title = element_text(size=13)) +
  ylab("") 

##----------------------------------
## 2. AGE

# Divide data in age intervals
data_age <- data.frame(data$year, data$demographics.age, data$wealth.worth.in.billions)
colnames(data_age) <- c("Year", "Age", "Wealth")
data_age <- data_age[data_age$Age>15, ]

for (i in 1:nrow(data_age)){
  if (15 < data_age$Age[i] & data_age$Age[i] <= 35){
    data_age$Age[i] <- "15-35"}
  if (35 < data_age$Age[i] & data_age$Age[i] <= 55){
    data_age$Age[i] <- "35-55"}
  if (55 < data_age$Age[i] & data_age$Age[i] <= 75){
    data_age$Age[i] <- "55-75"}
  if (data_age$Age[i] > 75){
    data_age$Age[i] <- "75-100"}
}
head(data_age)

# Boxplot
ggplot(data_age, aes(x=Year, y=Wealth)) +
  geom_boxplot(aes(fill=factor(Age)), outlier.shape = NA) +
  ggtitle("Wealth distribution according to age intervals") +
  labs(colour = "Age") +
  scale_fill_discrete(name = "Age interval") +
  ylim(0,10) +
  ylab("Billions")

# Density plot
# Only check data of 2014
data_age2 <- data_age[data_age$Year == 2014,]
head(data_age2)
ggplot(data_age2, aes(x=Wealth, group=Age, fill=Age)) +
  geom_density(adjust=1.5, alpha=.4) +
  ggtitle("Wealth distribution according to age intervals") +
  xlim(-1, 15)
## In this representation, 80 values are deleted due to the xlim 

## BAR STACKED PLOT
data_a <- matrix(0,nrow = 12, ncol = 3)
colnames(data_a) <- c("Year", "Age", "Count")
data_a[,1] <- c(rep(1996, 2) , rep(2001, 2) , rep(2014, 2))
g <- c("15-35", "35-55","55-75","75-100")
data_a[,2] <- c(rep(g, 3))
data_a

for (i in 1:nrow(data_a)){
  year <- data_a[i,1]
  gender <- data_a[i,2]
  s <- 0
  for (j in 1:nrow(data_age)){
    if (data_age$Year[j] == year && data_age$Age[j] == gender){
      s <- s + 1
    }
  }
  data_a[i,3] <- s
}
data_a <- as.data.frame(data_a)

library(hrbrthemes)
ggplot(data_a, aes(fill=Age, y=as.numeric(Count), x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Distribution of wealth according to age across years") +
  theme(plot.title = element_text(size=13)) +
  ylab("") 

##----------------------------------
## 3. WEALTH TYPE

data_type <- data.frame(data$year, data$wealth.type, data$wealth.worth.in.billions)
colnames(data_type) <- c("Year", "Type", "Wealth")
str(data_type)

# Boxplot
ggplot(data_type, aes(x=Year, y=Wealth)) +
  geom_boxplot(aes(fill=factor(Type)), outlier.shape = NA) +
  ggtitle("Wealth distribution according to wealth type") +
  labs(colour = "Type") +
  scale_fill_discrete(name = "Wealth type") +
  ylim(0,10) +
  ylab("Billions")

# Density plot
data_type2 <- data_type[data_type$Year == 2014,]
head(data_type2)
ggplot(data_type2, aes(x=Wealth, group=Type, fill=Type)) +
  geom_density(adjust=1.5, alpha=.4) +
  ggtitle("Wealth distribution according to wealth type (2014)") +
  xlim(-1, 20)
## In this representation, 30 values are deleted due to the xlim 

# BAR STACKED PLOT
data_t <- matrix(0,nrow = 15, ncol = 3)
colnames(data_t) <- c("Year", "Type", "Count")
data_t[,1] <- c(rep(1996, 5) , rep(2001, 5) , rep(2014, 5))
data_t[,2] <- c(rep(levels(data_type$Type), 3))
data_t

for (i in 1:nrow(data_t)){
  year <- data_t[i,1]
  type <- data_t[i,2]
  s <- 0
  for (j in 1:nrow(data_type)){
    if (data_type$Year[j] == year && data_type$Type[j] == type){
      s <- s + 1
    }
  }
  data_t[i,3] <- s
}
data_t <- as.data.frame(data_t)

ggplot(data_t, aes(fill=Type, y=as.numeric(Count), x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Distribution of wealth according to wealth type across years") +
  theme(plot.title = element_text(size=13)) +
  ylab("") 

##----------------------------------
## 4. INHERITED

data_inh <- data.frame(data$year, data$wealth.how.inherited, data$wealth.worth.in.billions)
colnames(data_inh) <- c("Year", "Inherited", "Wealth")
# Delete the level "not inherited"
data_inh <- data_inh[data_inh$Inherited != "not inherited",]
data_inh <- droplevels(data_inh)
str(data_inh)

# Boxplot
ggplot(data_inh, aes(x=Year, y=Wealth)) +
  geom_boxplot(aes(fill=factor(Inherited)), outlier.shape = NA) +
  ggtitle("Inherited Wealth distribution by generation") +
  labs(colour = "Inherited") +
  scale_fill_discrete(name = "Inherited type") +
  ylim(0,10) +
  ylab("Billions")

# Density plot
data_i2 <- data_inh[data_inh$Year == 2014,]
head(data_i2)
ggplot(data_i2, aes(x=Wealth, group=Inherited, fill=Inherited)) +
  geom_density(adjust=1.5, alpha=.4) +
  ggtitle("Inherited Wealth distribution by generation (2014)") +
  xlim(-1, 20)
## In this representation, 30 values are deleted due to the xlim 

# BAR STACKED PLOT
data_i <- matrix(0,nrow = 15, ncol = 3)
colnames(data_i) <- c("Year", "Inherited", "Count")
data_i[,1] <- c(rep(1996, 5) , rep(2001, 5) , rep(2014, 5))
data_i[,2] <- c(rep(levels(data_inh$Inherited), 3))
data_i

for (i in 1:nrow(data_i)){
  year <- data_i[i,1]
  inh <- data_i[i,2]
  s <- 0
  for (j in 1:nrow(data_inh)){
    if (data_inh$Year[j] == year && data_inh$Inherited[j] == inh){
      s <- s + 1
    }
  }
  data_i[i,3] <- s
}
data_i <- as.data.frame(data_i)

ggplot(data_i, aes(fill=Inherited, y=as.numeric(Count), x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("Inherited Wealth distribution by generation across years") +
  theme(plot.title = element_text(size=13)) +
  ylab("") 

##----------------------------------
## 4. TYPE OF WEALTH VS GENDER

data_2014 <- data[data$year == "1996",]
data_2014 <- droplevels(data_2014)
str(data_2014)

data_tg <- data.frame(data_2014$demographics.gender, data_2014$wealth.type, data_2014$wealth.worth.in.billions)
colnames(data_tg) <- c("Gender", "Type", "Wealth")
str(data_tg)

# BAR STACKED PLOT
data_tg2 <- matrix(0,nrow = 10, ncol = 3)
colnames(data_tg2) <- c("Gender", "Type", "Count")
data_tg2[,1] <- c(rep("male", 5), rep("female", 5))
data_tg2[,2] <- c(rep(levels(data_tg$Type), 2))
data_tg2

for (i in 1:nrow(data_tg2)){
  gender <- data_tg2[i,1]
  type <- data_tg2[i,2]
  s <- 0
  for (j in 1:nrow(data_tg)){
    if (data_tg$Gender[j] == gender && data_tg$Type[j] == type){
      s <- s + 1
    }
  }
  data_tg2[i,3] <- s
}
data_tg2 <- as.data.frame(data_tg2)

ggplot(data_tg2, aes(y=as.numeric(Count), x=Gender)) + 
  geom_bar(aes(fill=Type), position="fill", stat="identity") +
  ggtitle("Wealth type distribution according to gender") +
  theme(plot.title = element_text(size=13)) +
  ylab("") +
  xlab("1996")
