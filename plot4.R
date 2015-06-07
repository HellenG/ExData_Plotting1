#------------------------------------------------------------------------------#
#                 DATA SCIENCE COURSE
#    Plotting Assignment 1 for Exploratory Data Analysis
#------------------------------------------------------------------------------#
#  Plot 

# Step 1: Downloading, Reading and Subsetting Data

#Please set your working directory(where data will be saved)
link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(link, "Ass1.zip")
unzip("Ass1.zip")

#Since the data is large, I have used a function to subset the desired 2 days
subData <- function(){
        x <- read.delim("household_power_consumption.txt", sep = ";", quote = "", na.strings = "?", fileEncoding = "UTF-8", encoding = "UTF-8")
        dt <- as.POSIXct(strptime(paste(x$Date, x$Time), format = "%d/%m/%Y %T", tz = Sys.timezone()))
        y <- data.frame(dt, x[,3:9])
        s <- as.POSIXct(paste("2007-02-01", "00:00:00"), tz = Sys.timezone(), origin = "1970-01-01 00:00:00")
        e <- as.POSIXct(paste("2007-02-02", "23:59:59"), tz = Sys.timezone(), origin = "1970-01-01 00:00:00")
        y[y$dt >= s & y$dt <= e,]
}

df <- subData() 

#  This last plot gives a snap shot of the:
#       - Daily Global_Active_Power variation
#       - Daily Voltage Variation
#       - Daily metering variation and comparison across the three Energy 
#         Sub-Metering Consumption and
#       - Daily Global_reactive_power variation

# Step 2: Creating a time series vectors
#  For the Daily Global_Active_Power variation
tsData <- ts(df[,"Global_active_power"], frequency = 1440)
#  For the Daily Voltage Variation
tsData1 <- ts(df[,"Voltage"], frequency = 1440)
#  For the Daily metering variation and comparison across the three Energy
sm1 <- ts(df[,"Sub_metering_1"], frequency = 1440)
sm2 <- ts(df[,"Sub_metering_2"], frequency = 1440)
sm3 <- ts(df[,"Sub_metering_3"], frequency = 1440)
#  For the Daily Global_reactive_power variation
tsData2 <- ts(df[,"Global_reactive_power"], frequency = 1440)

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