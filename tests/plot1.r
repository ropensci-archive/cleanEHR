library(ggplot2)

patient <- function(pi){
    as.data.frame(csvdata@data[[csvdata@patient.id[pi]]], stringAsFactor=F)
}

getItem <- function(pi, item){
    patient(pi)[[paste("id", 
                       checklist$ItemId[which(checklist$dataItem ==item)],
                       sep="")]]
}

plot1 <- function(mean.heart.rate){
    df <- data.frame(x=seq(mean.heart.rate), y=mean.heart.rate)
    gg <- ggplot(df)+geom_histogram(binwidth=1)+aes(x=x)

    gg <- ggplot(df) + aes(x=y, y=..density..)
    gg <- gg + geom_histogram(colour = "darkgreen", fill = "white", binwidth = 0.5)
    gg <- gg + geom_density(colour="red",size=.7)
    gg <- gg + xlab("Mean heart rate")
    gg <- gg + ylab("Density")
    gg <- gg + ggtitle("Mean heart rate of all patients during recorded period")
    ggsave("plot1.png")
}


load("csvdata.Rdata")
checklist <- read.csv("data/data_item.csv")
patient.id <- csvdata@patient.id
patient.num <- csvdata@patient.num
mean.heart.rate <- vector()
for(i in seq(patient.num)){
    mean.heart.rate[i] <- 
        mean(as.numeric(as.character(getItem(i, "Heart rate"))), na.rm = TRUE)
}
plot1(mean.heart.rate)
