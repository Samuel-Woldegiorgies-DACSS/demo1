## in R, 
## read file from a URL
## file is in **excel**
## create the object '**fragility23**' to hold the contents

# starting fresh

rm(list = ls())

# reading data

# opening a file from a url (file in Excel), name it 'fragility23'
linkGit="https://github.com/DACSS-Fundamentals/overview/raw/refs/heads/main/FSI-2023-DOWNLOAD.xlsx"

#install.packages('rio')
library(rio) # package needed
fragility23=rio::import(file = linkGit) #object that will hold the result

# first operations

## find out column names in fragility23
names(x = fragility23)

## Check the data types in df
# let's find out if numeric columns have been read as should
str(object = fragility23)

## show me the first 10 rows
head(x = fragility23,10)

##show me the last 10 rows
tail(fragility23,10)

# transformativs manipulation

## keep some columns: 
##Country, Total,
## S1: Demographic Pressures,
## P1: State Legitimacy,
## E2: Economic Inequality
## into object 'frag23_sub'
grep(pattern = "Country|S1|P1|E2|Total",x = names(fragility23),fixed = F,value = T) # test

keep=grep("Country|S1|P1|E2|Total",names(fragility23),fixed = F,value = T)
frag23_sub=fragility23[,keep]


## rename 'frag23_sub' columns, use S1,P1 E2 only
names(frag23_sub)[3:5]=c("S1","E2", "P1")

## get me the top ten best countries on the column 'E2' from 'frag23_sub'
tail(frag23_sub[order(x=-frag23_sub$E2),],10) #option1
tail(frag23_sub[order(x=-frag23_sub$E2),'Country'],10) #option2


## give the statistical description of "frag23_sub"
summary(object = frag23_sub)

## the value of the worst quartile of Total
q3_Total=quantile(x = frag23_sub$Total, 
                  probs = 0.75, 
                  na.rm = TRUE)

## show correlations between "S1","E2", "P1"
cor(x=frag23_sub[,-c(1,2)]) #no 'Country', no "Total'

## show correlations between "S1","E2", "P1" and their "significance"
#?install.packages("corrtable")
library(corrtable)
corrtable::correlation_matrix(df = frag23_sub[,-c(1,2)])

## regress P1 and E2 on S1
lm(S1~P1+E2,data=frag23_sub)
model <- lm(S1 ~ P1 + E2, data = frag23_sub)
summary(model)



# some plotting
## give me a plot for the 'P1' variable
hist(x = frag23_sub$P1) #base r



## visual correlation between S1 and E2
plot(x=frag23_sub$S1, y=frag23_sub$E2)

## color points if country is on the worst quartile of Total
frag23_sub$Total>=q3_Total
frag23_sub$worstQt=frag23_sub$Total>=q3_Total

plot(frag23_sub$S1, 
     frag23_sub$E2,pch=20,
     col = as.factor(frag23_sub$worstQt))


# visual of the regression P1 and E2 on S1

#install.packages("sjPlot")
library(sjPlot)
plot_models(model)

