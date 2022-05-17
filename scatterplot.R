data$Time <- as.POSIXct(data$Time, format="%M:%S")
data$Duration <- as.numeric(format(data$Time, "%M")) + as.numeric(format(data$Time, "%S"))/60
ggplot(data, aes(x=Syntax, y=Duration, shape=factor(Sequence), color=Experience)) + geom_point() + theme_minimal()
