source("load_data.R")
library(ggplot2)
library(tidyverse)

## === Barplot for destination distibution ===
test = load_data(2018, 2018)

destinations = test[test$DEST != "BOS",]$DEST

n_airports = length(unique(destinations))
preserve_count = round(n_airports*0.8)

grouped = fct_lump(destinations, n=preserve_count)

df = data.frame(
table(grouped)
)

plot = ggplot(df, aes(x=reorder(grouped, -Freq), Freq))+
  geom_bar(stat="identity", width=1) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(axis.ticks.x = element_blank()) +
  xlab("Airport") +
  ylab("Number of flights in 2018") +
  labs(title = "Destination distribution out of Boston")
  
ggsave("Plots/destination_distribution_outgoing_2018.pdf")

## === End barplot ===

outgoing = test[test$DEST != "BOS",]
outgoing$DEP_DELAY = replace_na(outgoing$DEP_DELAY, 0)

avg.dest.delay = aggregate(outgoing$DEP_DELAY, by=list(outgoing$DEST), FUN=mean)

plot = ggplot(avg.dest.delay, aes(x=Group.1, x))+
  geom_bar(stat="identity", width=1) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(axis.ticks.x = element_blank()) +
  xlab("Airport") +
  ylab("Average departure delay in 2018") +
  labs(title = "Departure delay out of Boston in 2018")

ggsave("Plots/departure_delay_outgoing_2018.pdf")


outgoing$ARR_DELAY = replace_na(outgoing$ARR_DELAY, 0)

avg.arr.delay = aggregate(outgoing$ARR_DELAY, by=list(outgoing$DEST), FUN=mean)

plot = ggplot(avg.arr.delay, aes(x=Group.1, x))+
  geom_bar(stat="identity", width=1) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  theme(axis.ticks.x = element_blank()) +
  xlab("Airport") +
  ylab("Average arrival delay in 2018") +
  labs(title = "Arrival delay of flights comming out of Boston in 2018")

ggsave("Plots/arrival_delay_outgoing_2018.pdf")







