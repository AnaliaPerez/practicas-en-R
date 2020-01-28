# Prácticas en R
---
Hice un gráfico en R, con línea de regresión, con un pequeño dataset hecho con las cifras anuales de femicidios que publica La Casa del Encuentro, y que empezó a recopilar el número de casos desde 2008.

Primero hice una tabla en excel y la guardé bajo el formato .csv (femicidios.csv)

Vamos a usar la librería Tidyverse, así que la invocamos con el siguiente comando

> library("tidyverse")
 
para que lea el archivo que hicimos con los datos, ponemos a continuación

> femicidios<-read.csv("femicidios.csv")


Entonces el programa ya tiene en su conocimiento los datos con los que vamos a trabajar, así que podemos establecer la correlación
 

> cor(femicidios$año, femicidios$casos)
> modelo_fem<- lm(casos ~ año, data = femicidios)


escribimos 

>modelo_fem 

y lo corremos, nos mostrará valores (en la consola) que nos permitirán interpretar los datos
>Coefficients:
>(Intercept)          (slope) 
>-12677.08         6.43  

"intercept" es donde la línea de nuestro modelo toca el eje Y, y el valor "slope" (pendiente), al estar en positivo, nos muestra, valga la redundancia, una correlación positiva entre femicidios y año, que se puede leer así: "por cada año que pasa hay 6,43 casos de femicidio más"

Y luego hacemos el gráfico propiamente dicho

> ggplot(data = femicidios) +
  geom_point(aes(x = año, y = casos)) +
  labs(title = "Femicidios por año",
       subtitle = "Argentina",
       y = "Casos", x="Año",
       caption = "con línea de regresión") +
  geom_abline(aes(intercept = -12677.08, slope = 6.43), color = "red")+
  xlim(c(2008, 2022)) +
  ylim(c(205,320))

Ctrl + enter y voila!

![plot](https://github.com/AnaliaPerez/practicas-en-R/blob/master/Rplot01.png)

