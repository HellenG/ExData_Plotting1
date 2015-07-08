#------------------------------------------------------------------------------#
#                 DATA SCIENCE COURSE
#    Plotting Assignment 1 for Exploratory Data Analysis
#------------------------------------------------------------------------------#


# Step 1: Downloading, Reading and Subsetting Data

#Working directory set to course project 1 folder i.e. "ExData_Plotting1"
if(file.exists("Ass1.zip") == FALSE){
    link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(link, "Ass1.zip")
    unzip("Ass1.zip")
}
#Indices for the first observation in 1/2/2007 
f <- "household_power_consumption.txt"
starts <- min(grep(pattern = "^1/2/2007", x = readLines(con = f)))

#Reading data for the 2 days
c.names <- colnames(read.table(f, header = TRUE, sep = ";", nrows = 1, fileEncoding = "UTF-8", encoding = "UTF-8"))
no.rows <- 60 * 24 * 2 #Number of observations for the 2 days in minutes
sub.data <- read.table(f, stringsAsFactors = FALSE, sep = ";", col.names = c.names, na.strings = "?", nrow = no.rows, skip = starts - 1, fileEncoding = "UTF-8", encoding = "UTF-8")

#Combining date and time then converting to POSIXct (time object) 
date.time <- as.POSIXct(strptime(paste(sub.data$Date, sub.data$Time), format = "%d/%m/%Y %T", tz = Sys.timezone()))
sub.data <- data.frame(date.time, sub.data[,3:9])  


# Step 2: Creating time series vectors
#  For the Daily Global_Active_Power variation
tsData <- ts(sub.data[,"Global_active_power"], frequency = 1440)
#  For the Daily Voltage Variation
tsData1 <- ts(sub.data[,"Voltage"], frequency = 1440)
#  For the Daily metering variation and comparison across the three Energy
sm1 <- ts(sub.data[,"Sub_metering_1"], frequency = 1440)
sm2 <- ts(sub.data[,"Sub_metering_2"], frequency = 1440)
sm3 <- ts(sub.data[,"Sub_metering_3"], frequency = 1440)
#  For the Daily Global_reactive_power variation
tsData2 <- ts(sub.data[,"Global_reactive_power"], frequency = 1440)

# Step 3: Generate the graph and export to a png file
#Developing the layout for the four plots
par(mfrow = c(2, 2), mar = c(3.8, 4, 2.4, 1.8), cex.lab = 0.7)
#Plot 1 (first row, first column)
plot(x = tsData, type = "l", axes = F, ylab = "Global Active Power (kilowatts)", xlab = "")
axis(1, at = c(1, 2, 3), labels = c("Thu", "Fri", "Sat"))
axis(2)
box()

#Plot 2 (First Row, Second Column)
plot(x = tsData1, type = "l", ylab = "Voltage", xlab = "datetime", axes = F)
axis(1, at = c(1, 2, 3), labels = c("Thu", "Fri", "Sat"))
axis(2)
box()

#Plot 3 (Second Row, First Column)
ts.plot(sm1, sm2, sm3, gpars = list(type = "s", col = c(1, 2, 4), axes = F,
                                    xlab = "", ylab = "Energy Sub metering"))
axis(1, at = c(1, 2, 3), labels = c("Thu", "Fri", "Sat"))
axis(2)
legend("topright", col = c(1, 2, 4), pch = 45, cex = 0.5, box.col = 0, 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
box()

#Plot 4 (Second Row, Second Column)
plot(x = tsData2, type = "l", ylab = "Global Reactive Power", 
     xlab = "datetime", axes = F)
axis(1, at = c(1, 2, 3), labels = c("Thu", "Fri", "Sat"))
axis(2)
box()

dev.copy(png, file = "plot4.png")
dev.off()
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), cex.lab = 1)
#------------------    END     -----------------------------------------------#