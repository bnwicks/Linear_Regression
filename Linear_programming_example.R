### Linear programming Example ###
library(reshape2)
library(lpSolve)
require('plyr')
library("Lahman")
data("Master")
data("Fielding")

# calculate batting average and other stats
batting <- battingStats()

# add salary to Batting data; need to match by player, year and team
batting <- merge(batting, 
                 Salaries[,c("playerID", "yearID", "teamID", "salary")], 
                 by=c("playerID", "yearID", "teamID"), all.x=TRUE)

# Add name, age and bat hand information:
masterInfo <- Master[, c('playerID', 'birthYear', 'birthMonth',
                         'nameLast', 'nameFirst', 'bats')]
batting <- merge(batting, masterInfo, all.x = TRUE)
batting$age <- with(batting, yearID - birthYear -
                      ifelse(birthMonth < 10, 0, 1))

batting <- arrange(batting, playerID, yearID, stint)

## Generate a ggplot similar to the NYT graph in the story about Ted
## Williams and the last .400 MLB season 
# http://www.nytimes.com/interactive/2011/09/18/sports/baseball/WILLIAMS-GRAPHIC.html

# Restrict the pool of eligible players to the years after 1899 and
# players with a minimum of 450 plate appearances (this covers the
# strike year of 1994 when Tony Gwynn hit .394 before play was suspended
# for the season - in a normal year, the minimum number of plate appearances is 502)
eligibleHitters <- subset(batting, yearID >= 1900 & PA > 450)

# Find the hitters with the highest BA in MLB each year (there are a
# few ties).  Include all players with BA > .400
topHitters <- ddply(eligibleHitters, .(yearID), subset, (BA == max(BA))|BA > .400)

# Create a factor variable to distinguish the .400 hitters
topHitters$ba400 <- with(topHitters, BA >= 0.400)

# Sub-data frame for the .400 hitters plus the outliers after 1950
# (averages above .380) - used to produce labels in the plot below
bignames <- rbind(subset(topHitters, ba400),
                  subset(topHitters, yearID > 1950 & BA > 0.380))
# Cut to the relevant set of variables
bignames <- subset(bignames, select = c('playerID', 'yearID', 'nameLast',
                                        'nameFirst', 'BA'))

# Ditto for the original data frame
topHitters <- subset(topHitters, select = c('playerID', 'yearID', 'BA', 'ba400'))

# Positional offsets to spread out certain labels
#                     NL TC JJ TC GS TC RH GS HH RH RH BT TW TW  RC GB TG 
bignames$xoffset <- c(0, 0, 0, 0, 0, 0, 0, 0, -8, 0, 3, 3, 0, 0, -2, 0, 0)
bignames$yoffset <- c(0, 0, -0.003, 0, 0, 0, 0, 0, -0.004, 0, 0, 0, 0, 0, -0.003, 0, 0)  +  0.002

require('ggplot2')                               
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 2.15.3
ggplot(topHitters, aes(x = yearID, y = BA)) +
  geom_point(aes(colour = ba400), size = 2.5) +
  geom_hline(yintercept = 0.400, size = 1) +
  geom_text(data = bignames, aes(x = yearID + xoffset, y = BA + yoffset,
                                 label = nameLast), size = 3) +
  scale_colour_manual(values = c('FALSE' = 'black', 'TRUE' = 'red')) +
  ylim(0.330, 0.430) +
  xlab('Year') +
  scale_y_continuous('Batting average',
                     breaks = seq(0.34, 0.42, by = 0.02),
                     labels = c('.340', '.360', '.380', '.400', '.420')) +
  geom_smooth() +
  theme(legend.position = 'none')
## Scale for 'y' is already present. Adding another scale for 'y', which will
## replace the existing scale.
## geom_smooth: method="auto" and size of largest group is <1000, so using
## loess. Use 'method = x' to change the smoothing method.



