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
