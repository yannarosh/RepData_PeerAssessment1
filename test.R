
# if file doesn't exist......
unzip("./activity.zip")

# Load and preprocess
A <- read.csv("activity.csv")

A$date <- as.Date(A$date)

# What is mean total number of steps taken per day?

require(ggplot2)

Aagr <- aggregate(A$steps, by = list(A$date), sum)

g <- ggplot(data = Aagr, aes(Aagr$x))
g + geom_histogram(fill = "blue") + xlab("No. of steps") + ylab("Frequency") + 
        ggtitle("Total number of steps per day")