l#------------------------------------------------------------------------------#
#                 DATA SCIENCE COURSE
#    Plotting Assignment 1 for Exploratory Data Analysis
#------------------------------------------------------------------------------#
#  Plot 1

# Step 1: Downloading, Reading and Subsetting Data

#Working directory set as course project folder i.e. "ExData_Plotting1"
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

#Combining date and time and converting to POSIXct (time object) 
date.time <- as.POSIXct(strptime(paste(sub.data$Date, sub.data$Time), format = "%d/%m/%Y %T", tz = Sys.timezone()))
sub.data <- data.frame(date.time, sub.data[,3:9])

# Step 2: Plotting 
 
png(filename = "plot1.png")
hist(x = sub.data$Global_active_power, main = "Global Active Power", col = 2,
     xlab = "Global Active Power(kilowatts)")
dev.off()

#------------------------------------------------------------------------------#