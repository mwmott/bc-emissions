#Seattle Times - data understanding of BC emissions info

#NOTE::: several of the spreadsheets created with this are inaccurate, due to
#source data having '-''s instead of '0''s for missing data - this causes
#sums to have subtractions. If re-making this project, be careful that doesn't happen.

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

sort.11 <- bc_2011[order(bc_2011$Facility),]
sort.11 = sort.11[-1,]
View(sort.11)

sort.12 <- bc_2012[order(bc_2012$Facility),]
sort.12 = sort.12[-1,]
View(sort.12)

#Now all years are sorted alphabetically - now we have to compare totals across years by facility.

#To do this we'll need to merge. Before then, let's append colnames to include the year so we're clear.
#We do this with the package stringr.

require(stringr)

# Example: 
# nam <- names(mydf)
# names(mydf) <- ifelse(nam %in% c('X', 'A', 'Z'), 
#                      str_c(nam, '.ovca'),  str_c(nam, '.ctrls'))

nam <- names(sort.10)
names(sort.10) <- ifelse(nam %in% c("Total.Stationary.Combusion",
                                    "Total.Industrial.Process","Total.Flaring",
                                    "Total.Venting","Total.Fugitive",
                                    "Total.On.site.Transportation",
                                    "Total.Waste","Total.Wastewater",
                                    "Total.Combined.Industrial.Process.and.Stationary.Combustion",
                                    "Total.Emissions..tonnes.CO2e."), 
                         str_c(nam, '.2010'), str_c(nam, ''))
View(sort.10)

#Success! Now the other two:

nam <- names(sort.11)
names(sort.11) <- ifelse(nam %in% c("Total.Stationary.Combusion",
                                    "Total.Industrial.Process","Total.Flaring",
                                    "Total.Venting","Total.Fugitive",
                                    "Total.On.site.Transportation",
                                    "Total.Waste","Total.Wastewater",
                                    "Total.Combined.Industrial.Process.and.Stationary.Combustion",
                                    "Total.Emissions..tonnes.CO2e."), 
                         str_c(nam, '.2011'), str_c(nam, ''))
View(sort.11)

nam <- names(sort.12)
names(sort.12) <- ifelse(nam %in% c("Total.Stationary.Combusion",
                                    "Total.Industrial.Process","Total.Flaring",
                                    "Total.Venting","Total.Fugitive",
                                    "Total.On.site.Transportation",
                                    "Total.Waste","Total.Wastewater",
                                    "Total.Combined.Industrial.Process.and.Stationary.Combustion",
                                    "Total.Emissions..tonnes.CO2e."), 
                         str_c(nam, '.2012'), str_c(nam, ''))
View(sort.12)

#Great! Time to merge
all.fac <- merge(sort.10, sort.11, by='Facility', all.x=TRUE, all.y=TRUE)
all.fac <- merge(all.fac, sort.12, by='Facility', all.x=TRUE, all.y=TRUE)
View(all.fac)

#Merged done, now we break it up into what we want. Combustion levels by facility:
all.fac.comb <- data.frame(all.fac[,c(1, 2, 3, 14, 25)])
require(ggplot2)
plot(all.fac.comb)

#Now we run into a little trouble - too many! So let's rank them by the top emissions in 2010
#(then we'll try a plot).

combustion_totals <- all.fac.comb[order(all.fac.comb$Total.Stationary.Combusion.2010, decreasing = TRUE),]
combustion_totals <- data.frame(combustion_totals)
write.table(combustion_totals, "/Users/michael/combustion.totals.csv", sep=",", row.names=TRUE)

#########################
#Now to get sorting - let's look at the taxed v. untaxed question.

#I decided to make three spreadsheets of this - top 25 combustion, top 25 everything else, & BC aggregated separated.

#Top 25 combustion (minus BC)
View(all.fac.comb)
all.fac.comb.bc.aggregated <- all.fac.comb[grep("BC Aggregated Facilities",all.fac.comb$Facility),]
write.table(all.fac.comb.bc.aggregated, "/Users/michael/Desktop/bc.aggregated.csv", sep=",", row.names=TRUE)
#Sorted these after since I forgot, but this outputs all the bc aggregated fac's by 2010 combustion levels.

#Top 25 combustion w/o bc sorted by 2010:
all.fac.comb.no.bc <- all.fac.comb[-grep("BC Aggregated Facilities", all.fac.comb$Facility),]
View(all.fac.comb.no.bc)

all.fac.comb.no.bc.sort <- all.fac.comb.no.bc[order(all.fac.comb.no.bc$Total.Stationary.Combusion.2010),]
write.table(all.fac.comb.no.bc.sort, "/Users/michael/Desktop/top.comb.no.bc.csv", sep=",", row.names=TRUE)

#Top 25 everything else
all.fac.else <- data.frame(all.fac[,c(1, 2, 4, 5, 6, 7, 8, 9, 10, 15:21, 26:32)])
View(all.fac.else)

#Need to make total for all else - let's do that.
#Doing this in libre since it's a pain in R.
write.table(all.fac.else, "/Users/michael/Desktop/all.fac.else.csv", sep=",", row.names=TRUE)

all.fac.else <- read.csv("~/Documents/Data/bc-emissions/all.fac.else.csv")
View(all.fac.else)

all.fac.else.totals <- data.frame(all.fac.else[,c(1, 2, 10, 18, 26)])
all.fac.else.totals <- all.fac.else.totals[order(all.fac.else.totals$Total.untaxed.2010, decreasing=TRUE),]
View(all.fac.else.totals)

#These have BC up there again - let's take those out and subset

all.fac.else.bc.aggregated <- all.fac.else.totals[grep("BC Aggregated Facilities",all.fac.else.totals$Facility),]
write.table(all.fac.else.bc.aggregated, "/Users/michael/Desktop/all.fac.else.bc.aggregated.csv", sep=",", row.names=TRUE)
all.fac.else.no.bc <- all.fac.else.totals[-grep("BC Aggregated Facilities", all.fac.else.totals$Facility),]
write.table(all.fac.else.no.bc, "/Users/michael/Desktop/all.fac.else.no.bc.csv", sep=",", row.names=TRUE)
View(all.fac.else.bc.aggregated)
View(all.fac.else.no.bc)

#Okay cool. Now for the easy stuff (ha!). Let's total everything and compare. Doing this in
#libre again since summing factors is difficult.

all.fac.totals <- read.csv("~/Documents/Data/bc-emissions/all.fac.totals.csv")
View(all.fac.totals)

all.fac.totals.m <- melt(all.fac.totals, id.vars="Type", value.name="Emissions", variable.name="Year")
View(all.fac.totals.m)

#require(scales) to clean axis labels
require(scales)

  ggplot(data=all.fac.totals.m, aes(x=variable, y=value, group = Type, colour = Type)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") +
  scale_y_continuous(labels = comma) + 
  labs(title = "BC Emission Totals (taxed v. untaxed))", x="Year", y="Emission level (tons)", size=10)

#Graphed! Make sure to confirm numbers later.

#Now let's look at Chevron and Spectra. Saving spreadsheets also -

all.fac.chev <- all.fac[grep("Chevron",all.fac$Facility),]
all.fac.spec <- all.fac[grep("Spectra",all.fac$Company.x),]
write.table(all.fac.spec, "/Users/michael/desktop/spectra.csv", sep=",", row.names=TRUE)
write.table(all.fac.chev, "/Users/michael/desktop/chevron.csv", sep=",", row.names=TRUE)

#Hal wants to know the greatest differenece between these - which will 
#just require a bit of subtraction. Ex: df$V3 <- df$V1 - df$V2

all.fac.else.total$diff.11.10 <- all.fac.else.total$Total.untaxed.2011 - all.fac.else.total$Total.untaxed.2010
all.fac.else.total$diff.12.11 <- all.fac.else.total$Total.untaxed.2012 - all.fac.else.total$Total.untaxed.2011
all.fac.else.total <- all.fac.else.total[,c(1,2,3,6,4,7,5)]
all.fac.else.total <- all.fac.else.total[order(all.fac.else.total$diff.11.10, decreasing=TRUE),]
View(all.fac.else.total)
all.fac.else.total.no.bc <- all.fac.else.total[-grep("BC Aggregated Facilities", all.fac.else.total$Facility),]
View(all.fac.else.total.no.bc)

#K, now we have the biggest differences (bc and not) between 2011 and 2010.

write.table(all.fac.else.total, "/Users/michael/desktop/untaxed.difference.11.10.csv", sep=",", row.names=TRUE)
write.table(all.fac.else.total.no.bc, "/Users/michael/desktop/untaxed.difference.no.bc.11.10.csv", sep=",", row.names=TRUE)

#Here's the biggest diffs between 2012 and 2011.
all.fac.else.total <- all.fac.else.total[order(all.fac.else.total$diff.12.11, decreasing=TRUE),]
View(all.fac.else.total)

write.table(all.fac.else.total, "/Users/michael/desktop/untaxed.difference.12.11.csv", sep=",", row.names=TRUE)
write.table(all.fac.else.total.no.bc, "/Users/michael/desktop/untaxed.difference.no.bc.12.11.csv", sep=",", row.names=TRUE)

#Then the same for combustion

all.fac.comb$diff.11.10 <- all.fac.comb$Total.Stationary.Combusion.2011 - all.fac.comb$Total.Stationary.Combusion.2010
all.fac.comb$diff.12.11 <- all.fac.comb$Total.Stationary.Combusion.2012 - all.fac.comb$Total.Stationary.Combusion.2011
all.fac.comb <- all.fac.comb[,c(1,2,3,6,4,7,5)]
View(all.fac.comb)

#For some reason these are still in factors, so I'll need to do the subtraction in libre. Doing now
write.table(all.fac.comb, "/Users/michael/desktop/all.fac.comb.csv", sep=",", row.names=TRUE)

all.fac.comb <- all.fac.comb[order(all.fac.comb$diff.11.10, decreasing=FALSE),]



View(all.fac.comb)
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.11.10.csv", sep=",", row.names=TRUE)

#####
#Outside of this mess, got the .csv fixed in excel, now converting
all.fac.comb$diff.11.10 <- as.numeric(as.character(all.fac.comb$diff.11.10))
all.fac.comb$diff.12.11 <- as.numeric(as.character(all.fac.comb$diff.12.11))
all.fac.comb <- all.fac.comb[order(all.fac.comb$diff.11.10, decreasing=TRUE),]
View(all.fac.comb)

#YES
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.11.10.csv", sep=",", row.names=TRUE)

all.fac.comb <- all.fac.comb[order(all.fac.comb$diff.12.11, decreasing=TRUE),]
View(all.fac.comb)

write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.12.11.csv", sep=",", row.names=TRUE)

#and without BC

all.fac.comb.no.bc <- all.fac.comb[-grep("BC Aggregated Facilities", all.fac.comb$Facility),]
View(all.fac.comb.no.bc)
all.fac.comb.no.bc <- all.fac.comb.no.bc[order(all.fac.comb.no.bc$diff.11.10, decreasing=TRUE),]
View(all.fac.comb.no.bc)
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.no.bc.11.10.csv", sep=",", row.names=TRUE)

all.fac.comb.no.bc <- all.fac.comb.no.bc[order(all.fac.comb.no.bc$diff.12.11, decreasing=TRUE),]
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.no.bc.12.11.csv", sep=",", row.names=TRUE)


#Note- there are zeros in company names here, but only because they were not entered - not a computer error.

#Sort of lastly, diffs 10-12. Did this in excel for speed.
all.fac.comb <- read.csv("~/Documents/Data/bc-emissions/all.fac.comb.csv")
View(all.fac.comb)

all.fac.comb <- all.fac.comb[order(all.fac.comb$diff.12.10, decreasing=TRUE),]
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.12.10.csv", sep=",", row.names=TRUE)
---------

all.fac.comb.no.bc <- all.fac.comb.no.bc[order(all.fac.comb.no.bc$diff.12.10, decreasing=TRUE),]
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.no.bc.11.10.csv", sep=",", row.names=TRUE)

all.fac.comb.no.bc <- all.fac.comb.no.bc[order(all.fac.comb.no.bc$diff.12.11, decreasing=TRUE),]
write.table(all.fac.comb, "/Users/michael/desktop/combustion.difference.no.bc.12.11.csv", sep=",", row.names=TRUE)


