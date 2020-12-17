#TAREA 2 ICO - BigDATA
#Nombre: Nicolas Tolosa

#PREGUNTA 1: Cargue las bases de datos incoporando en cada una de ellas la variable "tamanio",
#donde indique de que tamaño es la empresa de ese país.(1 pto)


#instalar solo si no está instalado
#install.packages("tidyverse")
library("tidyverse")

setwd("~/GitHub/Tarea2_FOR_LOOP")


#los decimales en el df estan con "." para evitar problemas con el tipo de datos

chile_peq <- read.csv("./datos/pequena_chile.csv", header = T,
                                 sep = ";",
                                 dec = ",")

chile_mic <- read.csv("./datos/micro_chile.csv", header = T,
                      sep = ";",
                      dec = ",")
chile_med <- read.csv("./datos/medianas_chile.csv", header = T,
                      sep = ";",
                      dec = ",")
chile_gra <- read.csv("./datos/grandes_chile.csv", header = T,
                      sep = ";",
                      dec = ",")



peru_peq <- read.csv("./datos/pequena_peru.csv", header = T,
                     sep = ";",
                     dec = ",")
peru_mic <- read.csv("./datos/micro_peru.csv", header = T,
                     sep = ";",
                     dec = ",")
peru_med <- read.csv("./datos/medianas_peru.csv", header = T,
                     sep = ";",
                     dec = ",")
peru_gra <- read.csv("./datos/grandes_peru.csv", header = T,
                     sep = ";",
                     dec = ",")

colombia_peq <- read.csv("./datos/pequena_colombia.csv", header = T,
                         sep = ";",
                         dec = ",")
colombia_mic <- read.csv("./datos/micro_colombia.csv", header = T,
                         sep = ";",
                         dec = ",")
colombia_med <- read.csv("./datos/medianas_colombia.csv", header = T,
                         sep = ";",
                         dec = ",")
colombia_gra <- read.csv("./datos/grandes_colombia.csv", header = T,
                         sep = ";",
                         dec = ",")

#numero de filas para rellenar con replicate
ch_peq_length <- nrow(chile_peq)
ch_mic_length <- nrow(chile_mic)
ch_med_length <- nrow(chile_med)
ch_gra_length <- nrow(chile_gra)
pe_peq_length <- nrow(peru_peq)
pe_mic_length <- nrow(peru_mic)
pe_med_length <- nrow(peru_med)
pe_gra_length <- nrow(peru_gra)
co_peq_length <- nrow(colombia_peq)
co_mic_length <- nrow(colombia_mic)
co_med_length <- nrow(colombia_med)
co_gra_length <- nrow(colombia_gra)


chile_peq$tamanio = replicate(ch_peq_length,"pequena")
chile_mic$tamanio = replicate(ch_mic_length,"micro")
chile_med$tamanio = replicate(ch_med_length,"mediana")
chile_gra$tamanio = replicate(ch_gra_length,"grande")
peru_peq$tamanio = replicate(pe_peq_length,"pequena")
peru_mic$tamanio = replicate(pe_peq_length,"micro")
peru_med$tamanio = replicate(pe_peq_length,"mediana")
peru_gra$tamanio = replicate(pe_peq_length,"grande")
colombia_peq$tamanio = replicate(co_peq_length,"pequena")
colombia_mic$tamanio = replicate(co_peq_length,"micro")
colombia_med$tamanio = replicate(co_peq_length,"mediana")
colombia_gra$tamanio = replicate(co_peq_length,"grande")


#PREGUNTA 2: Reuna todas las bases en una sola y defina de qué tipología (tipo de datos) son cada
#una de las variables que se encuentran en la data.(1 pto)


#corrige el nombre de la columna 5 de todos los df porque algunas decian "procentaje_mujeres"
#por lo que no permite juntar todas las bases en una
names(chile_peq)[5] = "porcentaje_mujeres"
names(chile_mic)[5] = "porcentaje_mujeres"
names(chile_med)[5] = "porcentaje_mujeres"
names(chile_gra)[5] = "porcentaje_mujeres"

names(peru_peq)[5] = "porcentaje_mujeres"
names(peru_mic)[5] = "porcentaje_mujeres"
names(peru_med)[5] = "porcentaje_mujeres"
names(peru_gra)[5] = "porcentaje_mujeres"

names(colombia_peq)[5] = "porcentaje_mujeres"
names(colombia_mic)[5] = "porcentaje_mujeres"
names(colombia_med)[5] = "porcentaje_mujeres"
names(colombia_gra)[5] = "porcentaje_mujeres"

#junta todas las bases en un solo df llamado base_total
base_total <- rbind(chile_peq,
                    chile_mic,
                    chile_med,
                    chile_gra,
                    peru_peq,
                    peru_mic,
                    peru_med,
                    peru_gra,
                    colombia_peq,
                    colombia_mic,
                    colombia_med,
                    colombia_gra)

#verifica que tipo de datos son cada variable en el df
index <- colnames(base_total)
index_length <- length(index)

for (i in 1:index_length) {
  print(paste(index[i],"es tipo:",class(base_total[[index[i]]])))
}

#PREGUNTA 3: Determine a través del uso de condicionales y/o for cuántas obervaciones tiene Peru
#versus Chile.(2 pto)

#en consola muestra la cantidad de observaciones "n" de cada pais
n_paises <- base_total %>% 
             group_by(pais) %>% 
             tally()
rbind(n_paises[1,],n_paises[3,])


#PREGUNTA 4: Determine a través del uso de condicionales y/o for ¿cuál es el país con mayor
#ingresos de explotación para los años que considera la muestra.(2 pto)


  

#PREGUNTA 4: Determine a través del uso de condicionales y/o for ¿cuál es el país con mayor
#ingresos de explotación para los años que considera la muestra.(2 pto)

explotacion <- aggregate(ingresos ~ pais,base_total,sum)
print(paste("el pais con mayor ingresos es:",explotacion[2,1],"con",explotacion[2,2]))


#PREGUNTA 5: Genere una variable(columna) , donde si el país es Chile multiplique la tasa de interes
#por 0,1, cuando sea Peru le sume 0,3 y, y finalmente si es Colombia divida por 10 (2ptos).Use condicionales y/o for



operaciones <- function(pais,tasa){
  pais <- tolower(pais)
  if (pais == "chile") {
    return(tasa*0.1)
  }
  if (pais == "peru"){
    return(tasa+0.3)
  }
  if (pais == "colombia"){
    return(tasa/10)
  }
  return(tasa)
}

lista_t <- list()
p <- 1
for (j in base_total) {
  print(j)
  lista_t[[p]] <- operaciones("chile",10)
  p <- p+1
}
k <- function(row,output){
  pais <- row[match("pais",names(base_total))]
  tasa <- row[match("tasa_interes",names(base_total))]
  print(paste(pais,tasa))
}
apply(base_total,1,k)

lista_tasas
lista_paises <- unlist(base_total$pais)
lista_paises


#PREGUNTA 6: Reemplace en la columna exportaciones con 1 cuando es mayor a 2,1, con un 2
#cuando es menor 2,1y un 3 cuando es igual a 2,1, redondee al primer decimal la
#variable(2 ptos). Use condicionales y/o for.


#redondea los valores de exportaciones a 1 decimal
round(base_total$exportaciones,1)

#condicionales que reemplazan las variables
base_total$exportaciones[base_total$exportaciones < 2.1] <- 2
base_total$exportaciones[base_total$exportaciones > 2.1] <- 1
base_total$exportaciones[base_total$exportaciones == 2.1] <- 3

#BONUS TRACK

#No grafique ninguna variable pero le dejo un corazon


t <- seq(0, 2*pi, by=0.1)
x <- 16*sin(t)^3
y <- 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)

plot(x, y, type="l")
polygon(x, y, col="red")

