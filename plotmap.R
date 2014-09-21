library(ggplot2)
library(maps)
all_states <- map_data("state")
states_codes <- read.csv("states.csv")
m <- merge(x = all_states, y = states_codes, by.x = "region", by.y="state")

top_five <- head(top_ten, 5)
data <- a[a$EVTYPE %in% top_five$EVTYPE,]
plot_data <- merge(x = m, y = data, by.x="code", by.y="STATE")
plot_data <- plot_data[order(plot_data$order),] ##it gets disordered in the merge

p <- ggplot()
p <- p + geom_polygon(data=plot_data, aes(x=long, y=lat, group = group, 
                                          fill = log(INJURIES_AND_FATALITIES)),
                      colour="white") 
p <- p + scale_fill_continuous(low = "white", high = "darkred",na.value = "gray", guide="colorbar")
p <- p + facet_wrap(facet=~EVTYPE, nrow = 2, ncol = 3) + theme(legend.position=c(0.8,0.3))



p<- ggplot()
p <- p + geom_polygon(data=plot_data, aes(x=long, y=lat, group = group, 
                                          fill = log(total_damage)),
                      colour="white") 
p <- p + scale_fill_continuous(low = "white", high = "darkgreen",na.value = "gray", guide="colorbar")
p <- p + facet_wrap(facet=~EVTYPE, nrow = 2, ncol = 3) + theme(legend.position=c(0.8,0.3))