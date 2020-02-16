#22-4-19

library("tidyverse")

fertilidad<-read.csv('fertilidad_adolescente.csv')
educación<-read.csv('años_educación_obligatoria.csv')
continentes<- read.csv('gapminder(1).csv')


seleccionf_2010<- filter(fertilidad, año=="2010")

seleccioned_2010<- filter(educación, año=="2010")

crucePRUEBALJ<-left_join(seleccioned_2010, seleccionf_2010, by="pais")


#le agrego los continentes
cont<- select(continentes, pais, continente)
crucecc<- left_join(crucePRUEBALJ, cont, by="pais")

#quedaron filas repetidas, lo soluciono con la siguiente linea
crucecont<-distinct(crucecc)%>%
  drop_na()

#las siguientes líneas son para establecer correlación
#cor(cruce$Duration.of.compulsory.education..years...World.Bank.EdStats...years., cruce$niños.nacidos.por.1000.niñas.de.10.19.años)
# -0.2809537
#modelo_ed_fert<- lm(Duration.of.compulsory.education..years...World.Bank.EdStats...years. ~ niños.nacidos.por.1000.niñas.de.10.19.años, data = cruce)
#modelo_ed_fert
#coefficients: intercept: 10.00316
#       niños nacidos...: -0.02996

#con lo siguiente observo ciertas variables en particular (me interesa ver cuáles quedaron con NA en alguna columna)
chequeo<- select(crucecont, pais, continente)

#realizamos el gráfico
ggplot(crucecont) +
  geom_jitter(aes(x = Duration.of.compulsory.education..years...World.Bank.EdStats...years., y= niños.nacidos.por.1000.niñas.de.10.19.años, color= continente)) +
  labs(title = "Relación entre años de educación obligatoria y tasa de embarazo adolescente",
       subtitle = "información mundial del año 2010",
       y = "tasa embarazo adolescente*", x="duración en años de la educación obligatoria",
       caption = "*Niños nacidos vivos de madres de 10 a 19 años, por cada 1000 nacimientos.Datos extraidos de la web de Our World in Data")
  geom_abline(aes(intercept = 10.00316, slope = -0.02996), color = "green")+
 xlim(c(3, 18)) +
 ylim(c(3,100))

#me dice que removió 9 filas que tenian valores perdidos (missing values)
#ver en la carpeta la visualizacion de color de la pagina, los paises de Latinoamérica en general tienen muchos años de escolaridad obligatoria pero tambien una tasa de embarazo adolescente mas o menos alta
#los paises europeos tienen menos años en general que Sudamérica, y tasas notablemente más bajas de embarazo adolescente
#toda esta heterogeneidad explicaría la escasa correlacion entre ambas variables

#probemos para un año más reciente, algunos aumentaron la duración de la educación obligatoria
#no tengo datos para los mismos años,  comparo los ultimos disponibles: 2014 y 2016 

seleccionf_2016<- filter(fertilidad, año=="2016")
seleccionf_2016$año=NULL
seleccioned_2014<- filter(educación, año=="2014")
seleccioned_2014$año=NULL
cruce2<-merge(seleccioned_2014, seleccionf_2016)

cruce2continentes <- left_join(cruce2, cont, by="pais")

cruce2continentes <-distinct(cruce2continentes)%>%
  drop_na()
#cor(cruce2$Duration.of.compulsory.education..years...World.Bank.EdStats...years., cruce2$niños.nacidos.por.1000.niñas.de.10.14.años)
# - 0.2215106  
#modelo2_14_16<- lm(cruce2$Duration.of.compulsory.education..years...World.Bank.EdStats...years. ~ cruce2$niños.nacidos.por.1000.niñas.de.10.14.años, data = cruce)
#modelo2_14_16
#coefficients: intercept: 10.19323
#       niños nacidos...: -0.02882


ggplot(cruce2continentes) +
  geom_jitter(aes(x = Duration.of.compulsory.education..years...World.Bank.EdStats...years., y= niños.nacidos.por.1000.niñas.de.10.14.años, color= continente)) +
  labs(title = "Relación entre años de educación obligatoria y tasa de embarazo adolescente",
       subtitle = "información mundial correspondiente a los años 2014 y 2016",
       y = "tasa embarazo adolescente", x="duración en años de la educación obligatoria",
       caption = "con línea de regresión. Datos extraidos de la web de Our World in Data") +
  geom_abline(aes(intercept = 10.19323, slope = -0.02882), color = "blue")+
  xlim(c(3, 18)) +
  ylim(c(3,100))


