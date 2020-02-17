#27/05/19

install.packages("gganimate")
library(gganimate)
library(treemapify)
library(tidyverse)
library(reshape2)

fertility <- read.csv("fertility rate BM-R.csv")

GDP<-read.csv("gdp _pc_wb.csv") 

regiones<- read.csv("regiones_data_BM.csv")


#los datos están organizados de una forma que no me sirve para hacer el treemap y no me interesa abarcar los años más lejanos 
#(sobre todo por tener muchos faltantes de datos)
#por eso realizamos la siguientes operaciones de selección y reordenamiento

#los años están en columnas, por ello seleccionamos las de los años que queremos
GDP_años <- GDP%>%
  select(Country.Name, Country.Code, 15:62)

fert_años <- fertility%>%
  select(Country.Name, Country.Code, 15:62)

#y reordenamos, los años pasan a ser filas
melt_gdp<-GDP_años%>%
  melt(id=(c("Country.Name","Country.Code")))%>%
  arrange(Country.Name)%>%
  rename(year = variable,
         gdp_pc = value)%>%
  mutate(year = gsub("X", "", melt_gdp$year))

melt_fert<-fert_años%>%
  melt(id=(c("Country.Name","Country.Code")))%>%
  arrange(Country.Name)%>%
  rename(year = variable,
         fert = value)%>%
  mutate(year = gsub("X", "", melt_fert$year))

#unimos las 2 tablas
fert_gdppc<- left_join(melt_gdp, melt_fert)

#adosamos las regiones
region<- regiones%>%
  select(Country.Code, Region)

final_f_g<- left_join(fert_gdppc,region) 

#el año se mostraba con decimales, para resolver eso es la siguiente línea
finalfg<- final_f_g %>%
  mutate(year= as.integer(year))

#otro retoque: el dataset tenía en la columna pais una serie de regiones agrupadas que no me interesaban para exponer 
#en el gráfico, como no tenían region las quito con las siguientes líneas
sinreggrupos<- finalfg %>%
  filter(Region != "") %>%
  drop_na()

#realizamos el treemap animado, se toma su tiempo. Quizas sea recomendable hacer antes algún gráfico sin animar 
#(como el que pongo mas adelante) para ver si hay algo más que resolver con los datos
animado <- ggplot(sinreggrupos, aes(
  label = Country.Name,
  area = gdp_pc,
  subgroup = Region,
  fill = fert
)) +
  geom_treemap(layout = "fixed") +
  geom_treemap_text(layout = "fixed", place = "centre", grow = TRUE, colour = "white") +
  geom_treemap_subgroup_text(layout = "fixed", place = "bottomleft", colour = "grey", alpha = 0.5, grow = T) +
  geom_treemap_subgroup_border(layout = "fixed") +
  scale_fill_gradient(low = "#B55B81", high= "#48001E")+
  transition_time(year) +
  ease_aes('linear') +
  labs(title= "year: {frame_time}")


#guardamos en formato gif con la siguiente línea

anim_save("animado_treemap.gif", animado)


#probamos un treemap sin animar, para un año en específico (cualquiera, en este caso tomé 2007), para ver algunas cosas: color, errores
#esto de tirar el gráfico sin animar es buena idea para ver si hay problemas con el dataset
fertrate <- fertility%>%
  select(Country.Name, X2007)%>%
  rename(pais= Country.Name)

datoscompletos<-left_join(pbiy2007, fertrate, by="pais")%>%
  drop_na()

ggplot(datoscompletos, aes(area = PBI_PC, fill = X2007, subgroup=continente, label = pais)) +
  geom_treemap() +
  geom_treemap_subgroup_border()+
  scale_fill_gradient(low = "#ED3A2B", high= "#7C0900")+
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre")+
  geom_treemap_subgroup_text(place = "bottomleft", colour = "grey", alpha = 0.5, grow = T)+
  labs(title = "Relación entre PBI per cápita y tasa de fertilidad (promedio de hijos por mujer)", subtitle = "El área representa el PBI per cápita y la intensidad de color la tasa de fertilidad", fill="FRT",
       caption = "Elaborado en base a datos del Banco Mundial")




#en el treemap animado, en virtud de su bajo PBI pc, las regiones de África y Medio Oriente prácticamente desaparecen.
#es por esto que las separo y realizo un gráfico sólo con esas regiones

africa_medioo<- finalfg%>%
  filter(Region== "Middle East & North Africa" | Region== "Sub-Saharan Africa")

africa_medioo%>%
  mutate(year= as.integer(year))

#reviso la tabla
str(africa_medioo)

#probamos con un treemap clásico para cualquier año 

africa_medioo2015<- africa_medioo%>%
  filter(year== 2015)

ggplot(africa_medioo2015, aes(area = gdp_pc, fill = fert, subgroup=Region, label = Country.Name)) +
  geom_treemap() +
  geom_treemap_subgroup_border()+
  scale_fill_gradient(low = "#ED3A2B", high= "#7C0900")+
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre")+
  geom_treemap_subgroup_text(place = "bottomleft", colour = "grey", alpha = 0.5, grow = T)+
  labs(title = "Relación entre PBI per cápita y tasa de fertilidad (promedio de hijos por mujer)", subtitle = "El área representa el PBI per cápita y la intensidad de color la tasa de fertilidad", fill="FRT",
       caption = "Elaborado en base a datos del Banco Mundial")

#Ahora hacemos el animado

animado_afr <- ggplot(africa_medioo, aes(
  label = Country.Name,
  area = gdp_pc,
  subgroup = Region,
  fill = fert
)) +
  geom_treemap(layout = "fixed") +
  geom_treemap_text(layout = "fixed", place = "centre", grow = TRUE, colour = "white") +
  geom_treemap_subgroup_text(layout = "fixed", place = "centre", colour= "gray", grow = T) +
  geom_treemap_subgroup_border(layout = "fixed", colour = "grey", alpha = 0.5) +
  scale_fill_gradient(low = "#ED3A2B", high= "#7C0900")+
  transition_time(year) +
  ease_aes('linear') +
  labs(title = "year: {frame_time}")+
  theme(plot.title = element_text(size = 18, face = "bold"))

animado_afr

anim_save("anim_africa_medio_oriente.gif", animado_afr)
