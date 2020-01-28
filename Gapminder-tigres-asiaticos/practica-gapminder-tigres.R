#07/03/19

library(tidyverse)
data_mundial <- read.csv("https://bitsandbricks.github.io/data/gapminder.csv")

colnames(datamundial)[3]<-"año"
#Asia<-filter(data_mundial, continente=="Asia")

Tigres<- data_mundial%>% filter(pais == "Korea, Rep."|pais == "Taiwan"|pais == "Hong Kong, China"|pais == "Singapore")


ggplot(Tigres) +
  geom_line(aes(x=año, y=PBI_PC)) +
  geom_vline(aes(xintercept = 1997), color = "red") +
  geom_vline(aes(xintercept = 1970), color = "gold") +
  geom_vline(aes(xintercept = 2000), color = "green") +
  facet_wrap(~pais)

ggplot(Tigres) +
  geom_line(aes(x=año, y=PBI_PC, line=pais, color=pais)) +
  geom_vline(aes(xintercept = 1997), color = "red") +
  geom_vline(aes(xintercept = 1970), color = "gold") +
  geom_vline(aes(xintercept = 2000), color = "green") 

ggplot(Tigres) +
  geom_line(aes(x=año, y=PBI_PC, linetype=pais)) +
  geom_vline(aes(xintercept = 1997), color = "red") +
  geom_vline(aes(xintercept = 1970), color = "gold") +
  geom_vline(aes(xintercept = 2000), color = "green")

