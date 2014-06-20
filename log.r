#Seattle Times - data understanding of BC emissions info

#Pull in data
bc.2010 <- read.csv("~/Documents/Data/bc-emissions/bc-data/bc-2010.csv")
bc_2011 <- read.csv("~/Documents/Data/bc-emissions/bc-data/bc_2011.csv")
bc_2012 <- read.csv("~/Documents/Data/bc-emissions/bc-data/bc_2012.csv")

#So now we have three csvs of emissions data. There are three questions we want
#to answer with this data - 1. Which facilities have increased or decreased emissions
#between 2010-2012? 2. Rank total carbon emissions, or show how many have gone up/down.
#3. Divide totals between taxed and untaxed producers (combustion v.s. others, etc.).

#let's tackle that first one. We'll need to sort and total each facilities emissions.
#These are in total Co2 tons, by the way.

sort.10 <- bc.2010[order(bc.2010$Facility),]
#sorted by facility (for 2010)

#Now let's find the totals for each 2010 facility (&remove that weird total row, 
#which we do first).

sort.10 = sort.10[-1,]
View(sort.10)


