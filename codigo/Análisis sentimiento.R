###################################
#Análisis Sentimientos
####################################



# Remove all lists from the current workspace
rm(list = ls(all = T))

if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

#Instalamos librerías
library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(rvest)
library(httr)
library(xml2)
library(jsonlite)
library(lubridate)
library(scales)
library(syuzhet)

# El siguiente paso es cargar el diccionario que vas a usar y la función 'get_sentiments'

sentimientos <- read_tsv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/diccionarios/get_sentiments.R")

#############################################################################
# en la columna lexicon y puede ser nrc, afinn y bing). La columna valor es siempre NA 
# para nrc y bing, y un valor numérico entre -5 y +5 para afinn. En cambio, afinn 
# tiene siempre un valor NA en la columna sentimiento, mientras que en bing puede ser 
#negativo o positivo y en nrc uno de estos diez valores: positivo, negativo, ira, 
# miedo, tristeza, disgusto, asombro, confianza, alegría, premonición. 
#Algunas palabras pueden tener en nrc varios valores, pero siempre relacionados. 
# Fíjate en el cuadro anterior, abandonar está marcado como miedo, negativo y tristeza.
##############################################################################


# Cargas los textos de la carpeta del escritorio
cervantes <- read_lines("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/corpus/Española_inglesa.txt", locale = default_locale())
lope <- read_lines("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/corpus/Fortunas_de_Diana.txt", locale = default_locale())
# El siguente paso es crear un vector de caracteres con el título de cada entrega. 
# Llámalo titulo.
titulos <- c("La española inglesa",
             "Las fortunas de Diana")
#Lo siguiente es crear una lista, llamada libros
libros <- list(cervantes,
               lope)
# Ahora has de crear una gran tabla, que llamarás serie, con todos los textos, con los 
# títulos, los números de capítulo de cada una de ellos y con los textos divididos 
# en palabras-token
serie <- tibble()
for(i in seq_along(titulos)) {
  limpio <- tibble(capitulo = seq_along(libros[[i]]),
                   texto = libros[[i]]) %>%
    unnest_tokens(palabra, texto) %>%
    mutate(libro = titulos[i]) %>%
    select(libro, everything())
  serie <- rbind(serie, limpio)
}

# El último paso previo es convertir la columna de los títulos –libro– en un factor 
# (<fct>). 

serie$libro <- factor(serie$libro, levels = rev(titulos))

#Comprobar
serie$palabra[1:50]


#Comprobar palabras positivas y negativas
serie %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentimiento)) %>%
  count(sentimiento, sort = TRUE)

#Analizamos cuantas palabras hay de cada emoción
palabras_emocion <- serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 80 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(libro, indice = indice, sentimiento)

#palabras de emoción por página
palabras_emocion
#La siguiente es crear una tabla en la que se resumen y recopilan 
#los valores –n– de cada uno de los sentimientos –sentimiento– 
#para cada página –indice– de cada una de las entregas de la serie –libro–. 
sentimientos_páginas <- serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 80 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(libro, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) 


#Visualización de los resultados
serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 80 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(libro, indice = indice , sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo, libro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = libro)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~ libro, ncol = 2, scales = "free_x")

# otro diccionario: AFINN
serie %>%
  group_by(libro) %>%
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 80 + 1) %>%
  inner_join(get_sentiments("AFINN")) %>%
  group_by(libro, indice) %>%
  summarise(sentimiento = sum(valor)) %>%
  mutate(method = "AFINN") %>%
  ggplot(aes(indice, sentimiento, fill = libro)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  facet_wrap(~ libro, ncol = 2, scales = "free_x")

# Y otro más: BING
recuenta_palabras_bing <- serie %>%
  inner_join(get_sentiments("bing")) %>%
  count(palabra, sentimiento, sort = TRUE)

#Gráfico
recuenta_palabras_bing %>%
  group_by(sentimiento) %>%
  top_n(25) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = "Contribución al sentimiento", x = NULL) +
  coord_flip()

# Aplicando estas fórmulas sabemos en cuantas páginas de 
#250 palabra nos ha dividido el texto
30400 %/% 80 + 1
ceiling(30400/80)
#Comparamos sentimientos con distintos diccionarios: AFINN
afinn <- serie %>%
  group_by(libro) %>% 
  mutate(recuento_palabras = 1:n(),
         indice = recuento_palabras %/% 120 + 1) %>% 
  inner_join(get_sentiments("AFINN")) %>%
  group_by(libro, indice) %>%
  summarise(sentimiento = sum(valor)) %>%
  mutate(method = "AFINN")

# Sentimiento por página según AFINN
afinn

# BING y NRC
bing_and_nrc <- bind_rows(serie %>%
                            group_by(libro) %>% 
                            mutate(recuento_palabras = 1:n(),
                                   indice = recuento_palabras %/% 120 + 1) %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),
                          
                          serie %>%
                            group_by(libro) %>% 
                            mutate(recuento_palabras = 1:n(),
                                   indice = recuento_palabras %/% 120 + 1) %>% 
                            inner_join(get_sentiments("nrc") %>%
                                         filter(sentimiento %in% c("positive", "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(libro, indice, method, sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo) %>%
  select(libro, indice, method, sentimiento)


# Tenemos una estimación del sentimiento neto (positivo - negativo) 
# en cada fragmento del texto novedoso para cada léxico de sentimiento. 
# Unámoslos y trazémoslos.

bind_rows(afinn, 
          bing_and_nrc) %>%
  ungroup() %>%
  mutate(libro = factor(libro, levels = titulos)) %>%
  ggplot(aes(indice, sentimiento, fill = method)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  geom_tile(aes(fill = method), colour = "white") +
  facet_grid(libro ~ method)

#podemos analizar el recuento de palabras que contribuyen a cada sentimiento.
bing_word_counts <- serie %>%
  inner_join(get_sentiments("bing")) %>%
  count(libro, palabra, sentimiento, sort = TRUE) %>%
  ungroup()

bing_word_counts

#visualmente para evaluar las primeras n palabras para cada sentimiento:
bing_word_counts %>%
  group_by(sentimiento) %>%
  top_n(10) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentimiento, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

#Visualizamos las palabras que más contribuyen a definir los sentimientos de cada obra

bing_word_counts %>%
  group_by(sentimiento) %>%
  top_n(15) %>%
  ggplot(aes(reorder(palabra, n), n, fill = sentimiento)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~libro, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

# La librería 'syuzhet'
library(syuzhet)

## El siguiente paso es cargar el diccionario que vas a usar y la función 'get_sentiments'
sentimientos <- read_tsv("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/diccionarios/sentimientos_2.txt",
                         col_types = "cccn",
                         locale = default_locale())
source("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/diccionarios/get_sentiments.R")



# Creamos tabla
texto_analizar <- tibble(texto = lope)
texto_analizar
#Analizar sentimiento por palabras
texto_analizar <- texto_analizar %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 100 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo*-1)



#Podemos exportar la tabla R a Csv
write.csv(texto_analizar,"texto_analizar_lope en csv.csv")


# Comprobaamos la puntuación emocional decada página 
puntuacion <- texto_analizar %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)
puntuacion

#Representamos gráficamente
ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("Las fortunas de Diana")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

#Aplicamos la función syuzhet
texto_trans <- get_dct_transform(puntuacion$sentimiento,
                                 low_pass_size = 10,
                                 #x_reverse_len = nrow(puntuacion),
                                 scale_range = TRUE)
texto_trans

# La función de syuzhet devuelve unos valores numéricos 
# positivos y negativos que corresponden a cada página

#Creamos una nueva tabla
texto_trans <- tibble(pagina = seq_along(texto_trans),
                      ft = texto_trans)

texto_trans
#Podemos exportar la tabla
write.csv(texto_trans,"texto_trans_lope en csv.csv")

#Dibujamos el gráfico
ggplot(texto_trans, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Trama narrativa",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("Las fortunas de Diana"))))


#El texto con el que vamos a trabajar

Cervantes<- read_lines("~/Library/Mobile Documents/com~apple~CloudDocs/Desktop/Lope_Cervantes/corpus/Española_inglesa.txt", locale = default_locale())

# Creamos tabla
texto_analizar <- tibble(texto = cervantes)
texto_analizar
#Analizar sentimiento por palabras
texto_analizar <- texto_analizar %>%
  unnest_tokens(palabra, texto) %>%
  mutate(pagina = (1:n()) %/% 100 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentimiento, pagina = pagina) %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(negativo = negativo*-1)



#Podemos exportar la tabla R a Csv
write.csv(texto_analizar,"texto_analizar_cervantes en csv.csv")


# Comprobaamos la puntuación emocional decada página 
puntuacion <- texto_analizar %>%
  mutate(sentimiento = positivo+negativo) %>%
  select(pagina, sentimiento)
puntuacion

#Representamos gráficamente
ggplot(data = puntuacion, aes(x = pagina, y = sentimiento)) +
  geom_bar(stat = "identity", color = "midnightblue") +
  theme_minimal() +
  ylab("Sentimiento") +
  xlab("Tiempo narrativo") +
  ggtitle(expression(paste("Sentimiento en ",
                           italic("La española inglesa")))) +
  theme(legend.justification=c(0.91,0), legend.position=c(1, 0))

#Aplicamos la función syuzhet
texto_trans <- get_dct_transform(puntuacion$sentimiento,
                                 low_pass_size = 10,
                                 #x_reverse_len = nrow(puntuacion),
                                 scale_range = TRUE)
texto_trans

# La función de syuzhet devuelve unos valores numéricos 
# positivos y negativos que corresponden a cada página

#Creamos una nueva tabla
texto_trans <- tibble(pagina = seq_along(texto_trans),
                      ft = texto_trans)

texto_trans
#Podemos exportar la tabla
write.csv(texto_trans,"texto_trans_cervantes en csv.csv")

#Dibujamos el gráfico
ggplot(texto_trans, aes(x = pagina, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "aquamarine3", fill = "aquamarine3") +
  theme_minimal() +
  labs(x = "Trama narrativa",
       y = "Transformación Valorada del Sentimiento") +
  ggtitle(expression(paste("Forma de la historia de ",
                           italic("La española inglesa"))))
