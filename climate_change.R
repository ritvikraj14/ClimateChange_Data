# Reading data Climate_change.csv
climate = read.csv("~/Documents/Dataset_Explanation/ClimateChange_Data/climate_change.csv")
View(climate)

# Split data into training and test set
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

# Build linear model 
climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)
summary(climatelm)
# MEI, CO2, CFC.11, CFC.12, TSI, and Aerosols are all significant
# Value of R squared is 0.75

# The regression coefficient of N20 and CFC-11 are negative. This is due to the linear correlation of N2O and CFC.11 with other variables in the data set is quite large.

# find correlation between both all variables
cor(train)

# Build linear model with only MEI, N20, TSI, Aeroseols
LinReg = lm(Temp ~ MEI + N2O + TSI + Aerosols, data=train)
summary(LinReg) # Sign of N20 flips and this has not lost its explanatory power

# In this particular problem many of the variables (CO2, CH4, N2O, CFC.11 and CFC.12) are highly correlated, since they are all driven by human industrial development.

# Check what step function does
?step

# R provides a function, step, that will automate the procedure of trying different combinations of variables to find a good compromise of model simplicity and R2
StepModel = step(climatelm)
summary(StepModel)

# Once we have got the best model in dataframe StepModel we can use our test data 
tempPredict = predict(StepModel, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST

# Value of R2 is 0.6286051
