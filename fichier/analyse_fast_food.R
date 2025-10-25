# charger la library
library("dplyr")
library("ggplot2")

# lecture du fichier fastfood aux US
fast_food = read.csv("FastFoodRestaurants.csv")
head(fast_food)

# transformation en tibble
fast_food_tibble = as_tibble(fast_food)

# Quelles sont les 5 villes avec le plus de fast food

fast_food_tibble %>%
  group_by(city) %>%
  summarise(Nombre_de_restaurants = length(city)) %>%
  arrange(desc(Nombre_de_restaurants)) %>%
  head(n=5)

# Quels sont les fast food les plus présents dans ces 5 villes ?

city_list = fast_food_tibble %>% 
  group_by(city) %>% 
  summarise(Nombre_de_restaurants = length(city)) %>% 
  arrange(desc(Nombre_de_restaurants)) %>% 
  head(n = 5) %>% 
  pull(city)


fast_food_tibble %>%
  filter(city %in% city_list)

# Les 10 fast food les plus présent dans les 5 villes

fast_food_tibble %>%
filter(city %in% city_list) %>%
group_by(name) %>%
summarise(nombre_de_fast_food = length(name)) %>%
arrange(desc(nombre_de_fast_food)) 

# Quels sont les fast food les plus présents aux USA ?

fast_food_tibble %>%
  group_by(name) %>%
  summarise(nombre_de_fast_food = length(name), pourcentage_de_fast_food= (length(name)*100/10000)) %>%
  arrange(desc(nombre_de_fast_food)) 

# Dans quelle ville y a t-il le plus de  McDonald's ?

fast_food_tibble %>%
  filter(name %in% "McDonald's") %>%
  group_by(city) %>%
  summarise(nombre_de_fast_food = length(city)) %>%
  arrange(desc(nombre_de_fast_food)) 

# Où se situe New-York par rapoort aux 5 villes avec le plus de fast food ?

fast_food_tibble %>%
  group_by(city) %>%
  filter(city %in% "New York") %>%
  summarise(nombre_de_fast_food = length(city))
  

# Fast food les plus présents à New York

fast_food_tibble %>%
  filter(city %in% "New York") %>%
  group_by(name) %>%
  summarise(nombre_de_fast_food = length(name), pourcentage_de_fast_food= (length(name)*100/10000)) %>%
  arrange(desc(nombre_de_fast_food)) 

# Visualisation des données de l'ensemble des fast-foods aux USA

city_list = fast_food_tibble %>%
  group_by(city) %>%
  summarise(Nombre_de_restaurants = length(city)) %>%
  arrange(desc(Nombre_de_restaurants)) %>%
  head(n = 10) %>% 
  pull(city)

fast_food_tibble_10_villes = fast_food_tibble %>%
  filter(city %in% city_list) 


list_fast_food = fast_food_tibble_10_villes %>%
  group_by(name) %>%
  summarise(Nombre_de_restaurants = length(name)) %>%
  arrange(desc(Nombre_de_restaurants)) %>%
  head(n = 10) %>% 
  pull(name)


fast_food_tibble_10_villes_10_restaurants = fast_food_tibble_10_villes %>%
  filter(name %in% list_fast_food)


# Création du graphique et export en PDF

pdf("fast_food.pdf")
g <- ggplot(fast_food_tibble_10_villes_10_restaurants, aes(city, fill = name)) + geom_bar() + theme_minimal()
g <- g + xlab("Les 10 capitales du fast-foods") + ylab("Les 10 restaurants les plus implantés")
g <- g + ggtitle("Représentation des fast-foods les plus implantés \n dans les 10 capitales du fast-food")
g <- g + theme(plot.title = element_text(hjust = 0.5), axis.text = element_text(face = "bold", size = 7, angle = 45))
g <- g + ylim(0, 100) + theme(legend.title = element_blank()) + scale_fill_brewer(palette = "Paired")
g
dev.off()


  
  

  
