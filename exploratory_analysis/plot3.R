# This file plot1.R contain function plot3 that draw the plot3 plot required in assignmet of week1
# please note that the data file "household_power_consumption.txt" should be put in the same folder
# when running this R file,or change the read data topint to the correct folder

plot3 <- function()
{
  
  #read data
  full_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = c("character","character","numeric","numeric","numeric","numeric","numeric","numeric","numeric"), na.strings = "?" )
  
  #change type of first column to be DATE
  full_data$Date <- as.Date(full_data$Date,"%d/%m/%Y")
  
  #subset data to only the part we will use {Feb 1st and 2nd, 2007}
  data_of_power <- subset(full_data,Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02"))
  
  
  #set device to png
  png(filename = "plot3.png")
  
  # Draw plot3
  plot(strptime(paste(data_of_power$Date, data_of_power$Time, sep=" "), format = "%Y-%m-%d %H:%M:%S"),data_of_power$Sub_metering_1, type="l", ylim = c(0,40), xlab = " ", ylab = " " )
  par(new = TRUE)
  plot(strptime(paste(data_of_power$Date, data_of_power$Time, sep=" "), format = "%Y-%m-%d %H:%M:%S"),data_of_power$Sub_metering_2, type="l", ylim = c(0,40), col="red", xlab = " ", ylab = " " )
  par(new = TRUE)
  plot(strptime(paste(data_of_power$Date, data_of_power$Time, sep=" "), format = "%Y-%m-%d %H:%M:%S"),data_of_power$Sub_metering_3, type="l", ylim = c(0,40), col="blue", xlab = " ", ylab = "Energy sub metering" )
  legend("topright",names(data_of_power)[7:9],lty = 1,col = c("black", "red","blue"))
  
  #close device and create the file
  dev.off()
}