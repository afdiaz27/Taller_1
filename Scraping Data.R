###Taller 1

require(pacman)
p_load(tidyverse,rvest)

####Scraping las páginas en donde están los data chunks

#Inicializar listas
url_dc<-list()
pag_dc<-list()
dc<-list()

#Obtener tablas de los diez data chunks

for (x in 1:10){
  
  url_dc[x]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",x,".html")
  pag_dc[[x]]<-read_html(toString(url_dc[x]))
  dc[[x]]<-pag_dc[[x]] %>% html_node("table") %>% html_table()
}

#Unir las tablas en un dataframe
database<-bind_rows(dc[1:10])

#Eliminación del indice incompleto y creación de uno nuevo
id<-c(1:length(database[[1]]))
database<-cbind(id, database)
database<-database %>% select(-...1)

#Crear dataframe solamente con individuos mayores a 18 años

database_18<-database %>% filter(age > 18)
