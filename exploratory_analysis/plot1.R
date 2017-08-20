# This file plot1.R contain function plot1 that draw the plot1 plot required in assignmet of week1
# please note that the data file "household_power_consumption.txt" should be put in the same folder
# when running this R file,or change the read data topint to the correct folder

plot1 <- function()
{
 
  #read data
  full_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings = "?" )
  
  #change type of first column to be DATE
  full_data$Date <- as.Date(full_data$Date,"%d/%m/%Y")
  
  #subset data to only the part we will use {Feb 1st and 2nd, 2007}
  data_of_power <- subset(full_data,Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02"))
  
  
  #set device to png
  png(filename = "plot1.png")
  
 #Draw plot1
   hist(data_of_power$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red", breaks = 20)
 
   #close device and create the file
   dev.off()
}

