######### INTRODUCTION TO SPATIAL DATA ANALYSIS AND VISUALIZATION IN R #########
# load the csv files needed
Ethnicity <- read.csv('tables/KS201EW_oa11.csv')
Rooms <- read.csv("tables/KS403EW_oa11.csv")
Qualifications <-read.csv("tables/KS501EW_oa11.csv")
Employment <-read.csv("tables/KS601EW_oa11.csv")

### Observing column names ######

colnames(Ethnicity)
Ethnicity <- Ethnicity[, c(1,21)]
Rooms <- Rooms[, c(1,13)]
Qualifications <- Qualifications[, c(1,20)]
Employment <- Employment[, c(1,20)]

##### Renaming column headers ######
# to change an individual column name
#names(Employment)[2] <- "Unemployed"

# to change many column names

names(Ethnicity) <- c('OA', 'White_British')
names(Rooms) <- c('OA', 'Low_Occupancy')
names(Employment)<- c('OA', 'Unemployed')
names(Qualifications) <- c('OA', 'Qualification')

######### JOINING DATA IN R #############
# using merge() func joins two datasets together and create new datset
# merging Ethinicity and Rooms
merged_data_1 <- merge(Ethnicity, Rooms, by='OA')
# merge new dataset with Employment
merged_data_2 <- merge(merged_data_1, Employment, by='OA')
# merge with Qualification dataset
Census.Data <- merge(merged_data_2, Qualifications, by='OA')
names(Census.Data) # contain all the four variables

######### EXPORTING DATA ############
write.csv(Census.Data, 'practical_data.csv', row.names=F)

############### DATA EXPLORATION IN R #############
Census.Data <- read.csv('practical_data.csv')


### Exploring the data
print(Census.Data)
print(Census.Data[1:20,1:5])
# to get no of cols: ncol(), to get no of row: nrow()

#Desprictive Statistics: 
mean(Census.Data$Unemployed) #median(), range(), summary()

## Univariate Plots : mainly for continous variables
# histogram
hist(Census.Data$Unemployed, breaks = 20, col = 'blue', main = '% in full-time
     employment', xlab='Percentage' )

#Boxplots # ploting for multi-variables
boxplot(Census.Data[,2:5])

#Vioplot : Its combines both boxplot and hist in one graphic
# creates a violin plot for 4 variables, uses 3 shades of blue
vioplot(Census.Data$Unemployed, Census.Data$Qualification, 
        Census.Data$White_British, Census.Data$Low_Occupancy, ylim=c(0,100), col = "dodgerblue", 
        rectCol="dodgerblue3", colMed="dodgerblue4", names=c("Unemployed", "Qualifications", 
                                                             "White British", "Occupancy"))

####### BIVARIATE PLOTS IN R ######
####### Scatter plots: plot() requires two variables.

plot(Census.Data$Unemployed, Census.Data$Qualification, xlab = '% in full time
     employment', ylab='% with a Qualifications',pch=18) #plot symbols: pch = 0-23, other symbols

#Symbols plot
# Create a proportional symbols plot
symbols(Census.Data$Unemployed,Census.Data$Qualification,  circles = Census.Data$White_British, 
        fg="white", bg ="purple", inches = 0.2) 
##########ADDING A REGRESSION LINE #################
# using lm() func to plot  a linear model. abline() func rep the outputted reg line
# bubble plot
symbols(Census.Data$Unemployed, Census.Data$Qualification,  circles = Census.Data$White_British, fg="white", bg ="purple", inches = 0.2,  xlab="% in full time employment", ylab="% With a Qualification") +
  # adds a regression line, sets the colour to red
  abline(lm(Census.Data$Qualification~ Census.Data$Unemployed), col="black", lty = 2, lwd = 2)

# we can edit the line using the line type (lty) and line width (lwd) commands from abline

####  USING GGPLOT 2 ######################
ggplot(Census.Data, aes(x = Unemployed, y = Qualification))+
  geom_point()

##### PLOT 4 DIFF VARIABLES IN 1 2 DIMENSIONAL CHART
ggplot(Census.Data, aes(x = Unemployed, y = Qualification))+
  geom_point(aes(color = White_British, size = Low_Occupancy))

