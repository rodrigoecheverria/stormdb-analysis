df <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))
select <- df$PROPDMG > 0 | df$CROPDMG > 0 | df$FATALITIES > 0 | df$INJURIES > 0
df_c <- df[select,]

clean_event_types <- tolower(df_c$EVTYPE) 
clean_event_types <- gsub(pattern = "/", replacement = " ", x = clean_event_types)      ##881
clean_event_types <- gsub(pattern = "-", replacement = " ", x = clean_event_types)      ##866
clean_event_types <- gsub(pattern = "\\.", replacement = " ", x = clean_event_types)    ##866
clean_event_types <- gsub(pattern = "\\\\", replacement = " ", x = clean_event_types)   ##881
clean_event_types <- gsub(pattern = "\\(|\\)", replacement = "", x = clean_event_types) ##864
clean_event_types <- gsub("[:space:]*[a-z]?\\d+[:space:]?.*$", "", clean_event_types)
clean_event_types <- gsub("^ *|(?<= ) | *$", "", clean_event_types, perl=T)             ##848
clean_event_types <- gsub("flooding|floods", "flood", clean_event_types, perl=T)        ##832
clean_event_types <- gsub("strom|storms|stroms", "storm", clean_event_types, perl=T)    ##824
clean_event_types <- gsub("winds|wnd|wins", "wind", clean_event_types, perl=T)          ##803
clean_event_types <- gsub("thunderestorm|thundertorm|thundertsorm|thunerstorm|tstm", "thunderstorm", clean_event_types, perl=T)  ##803
clean_event_types <- gsub("torndao|tornadoes", "tornado", clean_event_types, perl=T)
clean_event_types <- gsub("fires", "fire", clean_event_types, perl=T)
clean_event_types <- gsub("typhoon", "hurricane", clean_event_types, perl=T)

df_c$EVTYPE <- as.factor(clean_event_types)
df_c$begin_date_posix <- strptime(df_c$BGN_DATE, "%m/%d/%Y %H:%M:%S")

df_c$INJURIES_AND_FATALITIES <- df_c$INJURIES + df_c$FATALITIES
a <- aggregate(cbind(INJURIES_AND_FATALITIES, INJURIES, FATALITIES) ~ EVTYPE + STATE,df_c, sum)
a2 <- aggregate(cbind(INJURIES_AND_FATALITIES, INJURIES, FATALITIES) ~ EVTYPE, a, sum)

top_ten <- head(a2[order(a2$INJURIES_AND_FATALITIES, decreasing = T),], 10)
a <- a[a$EVTYPE %in% top_ten$EVTYPE,]

library(reshape2)
top_ten_clean <- top_ten
top_ten_clean$INJURIES_AND_FATALITIES <- NULL
plot_data2 <- melt(top_ten_clean, id.vars = c("EVTYPE"))
p2 <- ggplot()
ggplot(plot_data2, aes(x=reorder(EVTYPE, value),y=value, fill=variable)) + geom_bar(stat='identity')

map_f <- function(exp) {
  
  if(exp %in% as.character(0:9)) 
    return (10^as.numeric(exp)) 
  if(nchar(as.character(exp)) == 0)
    return (1)
  m <- c(1,1,1,1, 1000, 1000000, 1000000000)
  names(m) <- c("?","-","+","h","k","m","b")
  return (as.numeric(m[tolower(exp)]))
}
df_c$PROPDMGEXP.num <- sapply(X = as.character(df_c$PROPDMGEXP), FUN = map_f)
df_c$CROPDMGEXP.num <- sapply(X = as.character(df_c$CROPDMGEXP), FUN = map_f)
df_c$total_damage <- df_c$PROPDMG * df_c$PROPDMGEXP.num + df_c$CROPDMG * df_c$CROPDMGEXP.num

a <- aggregate(total_damage ~ EVTYPE + STATE,df_c, sum)
a2 <- aggregate(total_damage ~ EVTYPE, a, sum)
top_ten <- head(a2[order(a2$total_damage, decreasing = T),], 10)
a <- a[a$EVTYPE %in% top_ten$EVTYPE,]