

#1. Data transformation 

library(ggplot2)
library(knitr)
library(dplyr)

#2.1 carregar base de dades
df <- read.csv2(file = 'data/southAfricanHeartDisease.csv', dec=',')

#2.2 Convertir base de dades a "tible"

df <- as_tibble(df)

df


#2.3 `dplyr` verbs -------------------------------------------------------

#2.3.1 `filter()`  Extreure files que coincideixen amb el requisit que fiquis


# filter(df, condicio1, conidcio2, .......) 

#exemple
filter(df, City %in% c("Barcelona", "Navarra"), Revenue > 3000)


#2.3.2  `arrange()`   Ordena files per defecte de mes petit a mes gran
arrange(df, columnax)
arrange(df, desc(columnax))


#2.3.3  `select()`   Extreure columnes segons el nom de la columna
select(df, Company, City)
select(df, Company:Activity)
select(df, -c(Company, City))
select(df, starts_with("C"))
select(df, ends_with("y"))
select(df, contains("it"))

#2.3.4 `mutate()`   Crear nova columna
mutate(df, 
       Rev_per_month = Revenue / 12, 
       Rev_per_week = Rev_per_month / 4)


#2.3.5 `summarise()`   crear taula amb els valors que t'interessi. Ex: suma dels valors d'una columna, mitjana dels valors d'una columna...
summarise(df, n = n(), 
          Rev_total = sum(Revenue), 
          Rev_mean = mean(Revenue), 
          Tot_act = n_distinct(Activity))



## `%>%` operator   L'operador passa el resultat de l'esquerra com a primer argument de la funcio de la dreta 
df %>% 
  summarise(n = n(), 
            Rev_total = sum(Revenue), 
            Rev_mean = mean(Revenue), 
            Tot_act = n_distinct(Activity))

### `group_by()`  Separa per grups segons l'argument. Ex. group_by(City) + summarise retorna les dades q tu vols de cada un dels grups (categories)
df %>% 
  group_by(City) %>% 
  summarise(n = n(), 
            Rev_total = sum(Revenue), 
            Rev_mean = mean(Revenue), 
            Tot_act = n_distinct(Activity))



# Ex: - How many companies are there in each city with a `revenue> 3000` and how many with a `revenue <3000`?    

df %>% 
  mutate(Volume = ifelse(Revenue > 3000, ">3000", "<3000")) %>% 
  group_by(City, Volume) %>% 
  summarise(n_3000 = n())

# tidyr: Una base de dades est? tidy si: 
######1. Cada variable te la seva propia columna
######2. Cada observacio te la seva propia fila
######3. Cada valor te la seva propia casella

#Exemple base de dades no tidy perq no Cada observacio te la seva propia fila
pollution <- tibble(
  city = c("New York", "New York", "London", "London", "Beijing", "Beijing"),
  size = c("large", "small", "large", "small", "large", "small"),
  amount = c(23, 14, 22, 16, 121, 56)
)
pollution


library(tidyr)



## spread(df,key,value) Transforma la base de dades de format llarg a format ample. Mou els valors de "key" a noms de columnes
####df=base de dades, 
###key= nom de la columna, que conte els valors q vols que acabin sent columnes independents
###value=nom de la columna q acaba sent els valors de la taula


pollution_2 <- spread(pollution, size, amount)


## gather() Transforma de format ample a format llarg. Mou els noms de les columnes a valors 
####gather(df, "new_var_columns", "new_var_values", -c(vars_remain_the_same))
####df=base de dades, 
###"new_var_columns" = nom de la columna que tindr? com a valro els noms de les columnes actuals.
###"new_var_values"=nom de la clumna que tindr? els valors


pollution3 <- gather(pollution_2, "size", "amount", -city)


## separate()  Separa una columna en més columnes
####separate(df, col, into, sep, convert)

#####df: Nom de la base de dades
#####col: Nom de la columna a separar
#####into: Vector dels noms de les columnes un cop separada
#####sep: El valor amb el que separes Ex: sep= "-"
#####convert = T    Per convertir caracter (chr) a integrer


# Exemple:

tormenta <- tibble(
  nombre = c("Mitch", "Gloria"),
  fecha = c("22-10-1998", "13-2-2020")
)
tormenta

tormenta_2 <- tormenta %>% 
  separate(fecha, c("dia", "mes", "any"), sep = "-", convert = T)
tormenta_2


## unite()   Uneix varies columnes en una
tormenta_2 %>% 
  unite("fecha", any, mes, dia, sep="/")



# ----- JOINS ------
library(lubridate)
library(dplyr)
    
    # 1. MUTATING JOINS: Utilitzen info de un conjunt de dades per afegir
    # variables a un altre conjunt. Retorna el df amb les columnes de x i de y

inner_join(x, y, by = c("nom columna en x" = "nom columna en y"))
# inner_join retorna el subset de files que està en x i en y

left_join(x, y, by = c("nom columna en x" = "nom columna en y"))
# left_join retorna totes les files de x, estiguin o no en y

right_join(x, y, by = c("nom columna en x" = "nom columna en y"))
# right_join retorna totes les files de y, estiguin o no en x

full_join(x, y, by = c("nom columna en x" = "nom columna en y"))
# full_join retorna totes les files de x, seguit de les files de y que no es troben a x


    # 2. FILTERING JOINS: Utilitzen info de un conjunt de dades per extreure 
    # casos de un altre conjunt. Retorna el df filtrat només amb les columnes de x

semi_join(x, y, by = c("nom columna en x" = "nom columna en y"))
# semi_join retorna totes les files de x amb coincidencia en y

anti_join(x, y, by = x("nom columna en x" = "nom columna en y"))
# anti_join retorna totes les files de x sense coincidencia en y


# ----- BUCLES ------
  # 1. Reservar suficient espai per la resposta (vector output nomalment)
  # 2. Definir el index sobre el que es fa el bucle (for i in seq_along(x))
  # 3. Codi que fa els càlculs

output <- vector("double", ncol(mtcars))  # Sortida
names(output) <- names(mtcars)
for (i in seq_along(mtcars)) {            # Sequencia
  output[[i]] <- mean(mtcars[[i]])        # Calculs
}
output

# Estructura condicional
if(condicio1) {
  codi si es compleix la condicio1
} else if (condicio2){
  codi si es compleix la condicio2
} else{
  codi si no es compleix cap condicio
}

# Funcions
nombre_funcion <- function(parametros,...) {
  calculos
  
  return(valor)
}
#Posem els punts suspensius com a argument per poder afegir instruccions en cas de necessitar-ho
# P.ex. si veiem que el df té valors NA, podem escriure -->function(df, na.rm=T) quan la cridem
  
  #Exemple
col_mean <- function(df) {
  output <- vector("numeric", length(df))
  for (i in seq_along(df)) {
    output[i] <- mean(df[[i]])
  }
  output
}


# ----- Save files -----

pollution

write_csv(pollution, "pollution.csv")

save(pollution, tabla1, tabla3, file="varios.RData")

load("varios.RData")


