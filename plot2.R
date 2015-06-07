#------------------------------------------------------------------------------#
#                 DATA SCIENCE COURSE
#    Plotting Assignment 1 for Exploratory Data Analysis
#------------------------------------------------------------------------------#
#  Plot 2

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

#  Based on the graph, the EDA question is:
#  What is the daily 'Global_active_power' variation, hence...

# Step 2: Creating a time series vector
tsData <- ts(df[,"Global_active_power"], start = c(1, 1), frequency = 1440)

# Step 3: Generate the graph
plot(x = tsData, type = "l", ylab = "Global Active Power (kilowatts)", 
     xlab = "", axes = F)#Don't mind the warnings.
axis(1, at = c(1, 2, 3), labels = c("Thu", "Fri", "Sat"))
axis(2)
box()
dev.copy(png, file = "plot2.png")
dev.off()

#------------------    END     -----------------------------------------------#