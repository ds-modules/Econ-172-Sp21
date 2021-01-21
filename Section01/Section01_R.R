#Get started with R
print("Hello World!")
2+2

#install.packages("haven") ## only run once to install
library(haven) #this library allows you to load datasets in Stata format
#install.packages("tidyverse") ## only run once to install
library(tidyverse)
#install.packages("summarytools") ## only run once to install
#library(summarytools)
#install.packages("stargazer") ## only run once to install
library(stargazer) ##This package is great for making tables in .html, .tex and many other formats.
#install.packages("broom") ## only run once to install
library(broom)

## Load in data in csv form, from the folder just loaded into your directory.
## Important: Paths are always relative to the RMarkdown file location
##            Alternatively, specify your working directory using 
##            knitr::opts_chunk$set(root.dir = "xxx")
prop_rights <- read.csv("prop_rights.csv")

prop_rights ## print the dataframe to screen
head(prop_rights) ## or print first few observations

## Let's take a look the variables in this dataset.
names(prop_rights) ## just print
prop_rights_variables <- names(prop_rights) ## store list, assigned "prop_rights_variables"
prop_rights_variables ## take a look at this list


#create a new df of only Africa countries
africa <- filter(prop_rights, africa==1) 

#Create a new variable equal to GDP (rather than log)
africa$levelGDP <- exp(africa$logGDP)

#Keep only the variables "Country, Protection, Level GDP, and Log Settler Mortality

africa <- select(africa, country, protection, levelGDP, logsettlermortality )

## Let's look at some summary statistics.
## Here is the min, 25%-ile, median, mean, 75%-ile, and max for the variable gdppc
summary(prop_rights$gdppc) ## just print
prop_rights_sumstats <- summary(prop_rights$gdppc) ## store summary, "prop_rights_variables"
prop_rights_sumstats ## take a look at this summary

## Let's look specifically at mean GDP
mean(prop_rights$gdppc) ## just print
meangdp <- mean(prop_rights$gdppc) ## save as a variable, called "meangdp"
meangdp ## take a look at the value stored in of "meangdp"

## Univariate regression of log gdp per capita on property rights index.
lm(logGDP ~ protection, data=prop_rights) # display regression results
## Multivariate, adding absolute latitude.
lm(logGDP ~ protection + lat_abst, data=prop_rights) # display regression results
## Storing results
reg1 <- lm(logGDP ~ protection, data=prop_rights) # save results, assign to "reg1"
reg2 <- lm(logGDP ~ protection + lat_abst, data=prop_rights) # save results, assignto "reg2"

## Now access regression results for the multivariate regression
summary(reg2) # access complete regression results

stargazer(reg1, reg2,
          out="Table 1",type="latex",header=FALSE, table.placement = "!h",
          title="Property Rights and Development",align=TRUE,
          report = "vc*st", omit.stat=c("LL","ser","f","rsq","adj.rsq"),no.space=TRUE)

plot(prop_rights$protection,prop_rights$logGDP,
     xlab="Protection against expropriation", ylab="Log (DP per capita)")
abline(reg1) # line of best fit using regression results as per "reg1"