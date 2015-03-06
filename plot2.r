plot2 <- function(data=NULL) {
    if(is.null(data))
        data <- load_data()

    png("plot2.png", width=400, height=400)
    
    plot(data$Time, data$Global_active_power,
         type="l",
         xlab="",
         ylab="Global Active Power (kilowatts)")
    
    dev.off()
}
date_time <- function(date, time) {
    return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
}

load_data <- function() {
    filename <- "household_power_consumption.txt"
    df <- read.table(filename,
                     header=TRUE,
                     sep=";",
                     colClasses=c("character", "character", rep("numeric",7)),
                     na="?")
    # convert date and time variables to Date/Time class
    df$Time <- strptime(paste(df$Date, df$Time), "%d/%m/%Y %H:%M:%S")
    df$Date <- as.Date(df$Date, "%d/%m/%Y")
    # only use data from the dates 2007-02-01 and 2007-02-02
    dates <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
    df <- subset(df, Date %in% dates)
    
    return(df)
    
}
