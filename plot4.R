plot4 <- function () {
        # download the data if not downloaded before
        
        # check to see if the '/data' directory exists
        if (!file.exists("./data")) { 
                dir.create("./data")
        }        
        # check to see if the './data/Dataset.zip' file exists which means file was previously downloaded
        if (!file.exists("./data/Dataset.zip")) {
                fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                download.file(fileUrl, destfile = "./data/Dataset.zip", method = "libcurl")
        }
        # check to see if the dataset parth exists which means file was previously unzipped
        sourcefile <- "./data/household_power_consumption.txt"
        if (!file.exists(sourcefile)) {
                # Unzip the downloaded files
                unzip(zipfile="./data/Dataset.zip",exdir="./data")
        }
        ## check read.table vs fread for speed
        # system.time(powerdata <- read.table(sourcefile,
        #                                     header = TRUE,
        #                                     sep = ";",
        #                                     dec = ".",
        #                                     na.strings = "?"))
        
        require(data.table)
        # system.time(powerdata <- fread(sourcefile,
        #                                header = TRUE,
        #                                sep = ";",
        #                                dec = ".",
        #                                na.strings = "?"))
        
        ## use fread as performance test indicate it complete the read in half the time
        powerdata <- fread(sourcefile,
                           header = TRUE,
                           sep = ";",
                           stringsAsFactors = FALSE,
                           dec = ".",
                           na.strings = "?")
        #names(powerdata)
        #head(powerdata)
        
        # retrieve the subset of data
        subsetpowerdata <- subset(powerdata, powerdata$Date %in% c("1/2/2007", "2/2/2007"))
        
        # to the necessary time and date conversions
        datetime <- strptime(paste(subsetpowerdata$Date, subsetpowerdata$Time, sep=" "), "%d/%m/%Y %H:%M:%S")
        subsetpowerdata$DateTime <- as.POSIXct(datetime)
        
        # do the necessary typecasts
        subsetpowerdata$Global_active_power <- as.numeric(as.character(subsetpowerdata$Global_active_power))
        subsetpowerdata$Global_reactive_power <- as.numeric(as.character(subsetpowerdata$Global_reactive_power))
        subsetpowerdata$Voltage <- as.numeric(as.character(subsetpowerdata$Voltage))
        subsetpowerdata$Sub_metering_1 <- as.numeric(as.character(subsetpowerdata$Sub_metering_1))
        subsetpowerdata$Sub_metering_2 <- as.numeric(as.character(subsetpowerdata$Sub_metering_2))
        subsetpowerdata$Sub_metering_3 <- as.numeric(as.character(subsetpowerdata$Sub_metering_3))
        
        # calling the basic plot functions to a PNG file
        png("plot4.png", width=480, height=480)
        par(mfrow = c(2, 2)) 
        plot(subsetpowerdata$DateTime, 
             subsetpowerdata$Global_active_power, 
             type="l", 
             xlab="", 
             ylab="Global Active Power", 
             cex=0.2)
        plot(subsetpowerdata$DateTime, 
             subsetpowerdata$Voltage, 
             type="l", 
             xlab="datetime", 
             ylab="Voltage")
        plot(subsetpowerdata$DateTime, 
             subsetpowerdata$Sub_metering_1, 
             type="l", 
             ylab="Energy Submetering", 
             xlab="")
        lines(subsetpowerdata$DateTime, 
              subsetpowerdata$Sub_metering_2, 
              type="l", 
              col="red")
        lines(subsetpowerdata$DateTime, 
              subsetpowerdata$Sub_metering_3, 
              type="l", 
              col="blue")
        legend("topright", 
               c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               lty=, 
               lwd=2.5, 
               col=c("black", "red", "blue"), 
               bty="o")
        plot(subsetpowerdata$DateTime, 
             subsetpowerdata$Global_reactive_power, 
             type="l", 
             xlab="datetime", 
             ylab="Global_reactive_power")
        dev.off()
}