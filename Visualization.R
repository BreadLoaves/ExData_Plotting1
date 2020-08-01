#Downloading and unzipping file
URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

download.file(URL, destfile = "Help.zip", mode = "wb")
unzip("Help.zip", exdir = getwd())

#Data initial read
data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

#Formatting date to type date
data$Date <- as.Date(data$Date, "%d/%m/%Y")

#Filtering data
February <- subset(data, Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

#Removing incomplete observations
February <- February[complete.cases(February),]

#Combining date and time
datetime <- paste(February$Date, February$Time)

#Vector rename
datetime <- setNames(datetime, "datetime")

#Removing datetime column
February <- February[, !(names(February) %in% c("Date", "Time"))]

#Adding datetime
February <- cbind(datetime, February)
February$datetime <- as.POSIXct(datetime)

#Plotting - Histogram
hist(February$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (Kilowatts", col = "purple")

#Plotting - Saving histogram
dev.copy(png, "plot1.png", width = 480, height = 480)
dev.off()

#Plotting - Plot2
plot(February$Global_active_power~February$datetime, type = "l", ylab = "Global Active Power (Kilowatts", xlab = "")

#Plotting - Saving histogram
dev.copy(png, "plot2.png", width = 480, height = 480)
dev.off()

#Plotting - Plot3
with(February, {
  plot(Sub_metering_1~datetime, type = "l",
       ylab = "Global active power (Kilowatts", xlab = "")
  lines(Sub_metering_2~datetime, col = "red")
  lines(Sub_metering_3~datetime, col = "blue")
})

legend("topleft", col = c("Black", "Red", "Blue"), lwd = c(1,1,1),
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#Plotting - Saving histogram
dev.copy(png, "plot3.png", width = 480, height = 480)
dev.off()

#Plotting - Plot4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))

  with(February, {
    plot(Global_active_power~datetime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    plot(Voltage~datetime, type="l",
         ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1~datetime, type="l",
         ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~datetime,col='Red')
    lines(Sub_metering_3~datetime,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
           legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

    plot(Global_reactive_power~datetime, type="l",
         ylab="Global Rective Power (kilowatts)",xlab="")

  })

#Plotting - Saving histogram
dev.copy(png, "plot4.png", width = 480, height = 480)
dev.off()









