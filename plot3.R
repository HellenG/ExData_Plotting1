#------------------------------------------------------------------------------#
#                 DATA SCIENCE COURSE
#    Plotting Assignment 1 for Exploratory Data Analysis
#------------------------------------------------------------------------------#
#  Plot 3

# Step 1: Downloading, Reading and Subsetting Data

#Working directory set as course project folder "ExData_Plotting1"
if(file.exists("Ass1.zip") == FALSE){
    link <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    download.file(link, "Ass1.zip")
    unzip("Ass1.zip")
}

#Since the data is large, I have used a function to subset the desired 2 days
#Indices for the first observation in 1/2/2007 
f <- "household_power_consumption.txt"
starts <- min(grep(pattern = "^1/2/2007", x = readLines(con = f)))

#Reading data for the 2 days
c.names <- colnames(read.table(f, header = TRUE, sep = ";", nrows = 1, fileEncoding = "UTF-8", encoding = "UTF-8"))
no.rows <- 60 * 24 * 2 #Number of observations for the 2 days in minutes
sub.data <- read.table(f, stringsAsFactors = FALSE, sep = ";", col.names = c.names, na.strings = "?", nrow = no.rows, skip = starts - 1, fileEncoding = "UTF-8", encoding = "UTF-8")

#Combining date and time and converting to POSIXct (time object) 
date.time <- as.POSIXct(strptime(paste(sub.data$Date, sub.data$Time), format = "%d/%m/%Y %T", tz = Sys.timezone()))
sub.data <- data.frame(date.time, sub.data[,3:9]) ) 

#  Based on the graph, the EDA issue is:
#  Comparing the three Energy Sub-Metering Consumption

# Step 2: Creating a time series vectors
sm1 <- ts(sub.data[,"Sub_metering_1"], frequency = 1440)
sm2 <- ts(sub.data[,"Sub_metering_2"], frequency = 1440)
sm3 <- ts(sub.data[,"Sub_metering_3"], frequency = 1440)

# Step 3: Generate the graph
ts.plot(sm1, sm2, sm3, gpars = list(type = "s", col = c(1, 2, 4), axes = F,
                                    xlab = "", ylab = "Energy Sub metering"))
axis(1, at = c(1, 2, 3), labels = c("Thu", "Fri", "Sat"))
axis(2)
legend("topright", col = c(1, 2, 4), pch = 45, cex = 0.7,
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
box()
dev.copy(png, file = "plot3.png")
dev.off()

#------------------    END     -----------------------------------------------#