library("tidyverse") 

femicidios<-read.csv("femicidios.csv")


#establecer correlación
cor(femicidios$año, femicidios$casos)

modelo_fem<- lm(casos ~ año, data = femicidios)

modelo_fem

#lo corremos y nos mostrará esto en la consola
#Coefficients:
#(Intercept)          año  
#-12677.08         6.43  

ggplot(data = femicidios) +
  geom_point(aes(x = año, y = casos)) +
  labs(title = "Femicidios por año",
       subtitle = "Argentina",
       y = "Casos", x="Año",
       caption = "con línea de regresión") +
  geom_abline(aes(intercept = -12677.08, slope = 6.43), color = "red")+
  xlim(c(2008, 2022)) +
  ylim(c(205,320))


