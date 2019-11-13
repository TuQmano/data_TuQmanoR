# POST: Interacciones en Twitter 
# https://tuqmano.github.io/
# CODIGO PARA VISUALIZACION DE GRAFICOS TERNARIOS


# CARGAMOS LIBERRIAS

library(tidyverse) # para manipulacion de datos *incluye ggplot para viz
library(ggtern) # para aplicar funciones de graficos ternarios a ggplot

# Levantamos data 
tuits <- read_csv("../Documents/menta/ternarios/data/tweets.csv") %>% 
  mutate(date = as.Date(date)) # modificamos class de la variable de tiempo

# Ajustamos base de datos 
tuits <- tuits %>% 
  select(date, username, tweet, retweets_count,replies_count,likes_count) %>%  # seleccionamos varibles relevantes
  mutate(ratio = round(replies_count/(retweets_count+likes_count)*100, digits=1), #calculamos ratio
         label = ratio)




# Segmentamos datos y preparamos viz
tuits %>%
  filter(date >= "2019-06-22" & date <= "2019-10-28") %>% # filtarmos fechas de interes 
  mutate(periodo = if_else(date<="2019-08-11","Campaña P.A.S.O","Campaña Generales")) %>% # etiquetamos periodos
  mutate(periodo = factor(periodo,levels = c("Campaña P.A.S.O", "Campaña Generales"))) %>% 
  arrange(desc(periodo)) %>% 
  ggtern(aes(x = replies_count, y= retweets_count, z = likes_count, colour = username, fill= username)) + # creamos ternario
  geom_point(alpha = .7) + # grficamos posicion de cada tuit  para tres variables
  labs(title = 'Reacciones a los tuits según usuario por período',
       colour = 'Usuario', x = "RP", y = "RT", z = "MG") +
  scale_color_manual(values = c(kicillofok = "#00aeec",alferdez = "#00aeec",
                                mariuvidal = "#ffcf02", mauriciomacri = "#ffcf02"))+ # seteamos colores por espacios politicos
  scale_fill_manual(values = c(kicillofok = "#00aeec",alferdez = "#00aeec", 
                               mariuvidal = "#ffcf02", mauriciomacri = "#ffcf02")) +
  facet_grid(periodo ~ username) +  # comentamos esta linea para visualizar sin facets por periorod electoral y usuario
  theme_minimal() +
  theme(axis.text = element_blank(), legend.position = 'none', axis.title = element_text(size = 8)) 


