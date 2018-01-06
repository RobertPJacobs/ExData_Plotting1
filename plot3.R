plot3 <- function () {
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
        subsetpowerdata$Date <- as.Date(subsetpowerdata$Date, format="%d/%m/%Y")
        datetime <- paste(as.Date(subsetpowerdata$Date), subsetpowerdata$Time)
        subsetpowerdata$DateTime <- as.POSIXct(datetime)
        
        # do the necessary typecasts
        subsetpowerdata$Sub_metering_1 <- as.numeric(as.character(subsetpowerdata$Sub_metering_1))
        subsetpowerdata$Sub_metering_2 <- as.numeric(as.character(subsetpowerdata$Sub_metering_2))
        subsetpowerdata$Sub_metering_3 <- as.numeric(as.character(subsetpowerdata$Sub_metering_3))
        
        # calling the basic plot functions to a PNG file
        png("plot3.png", width=480, height=480)
        plot(subsetpowerdata$DateTime,
             subsetpowerdata$Sub_metering_1,
             type="n",
             xlab="",
             ylab="Energy sub metering")
        with(subsetpowerdata,lines(DateTime,
                                   as.numeric(as.character(Sub_metering_1)),
                                   col="black"))
        with(subsetpowerdata,lines(DateTime,
                                   as.numeric(as.character(Sub_metering_2)),
                                   col="red"))
        with(subsetpowerdata,lines(DateTime,
                                   as.numeric(as.character(Sub_metering_3)),
                                   col="blue"))
        legend("topright", 
               lty=1, 
               col=c("black", "red", "blue"),
               legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        dev.off()
}