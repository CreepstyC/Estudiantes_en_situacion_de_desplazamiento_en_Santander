library(readxl)
library(tidyverse)
library(writexl)
library(skimr)  #resúmenes estadísticos para marcos de datos
library(DT) #presentación de tablas y conjuntos de datos
library(knitr)  #generación dinámica de reportes
library(kableExtra) #manipulación y creación de tablas
library(dplyr) #manipulación y creación de tablas

estudiantesDesplazados <- read_excel("ESTUDIANTES_EN_SITUACI_N_DE_DESPLAZAMIENTO_EN_SANTANDER.xlsx")
datatable(head(estudiantesDesplazados), options = list(scrollX = '300px', paging = FALSE, searching=FALSE))

variables <- c("d_ano", "d_muni", "d_nombmuni", "d_provincia", "d_nomsec" , "d_nomzon", "dane_ant", "d_nombinst", "d_sede", "d_nombsede", "d_nomjor", "d_grado", "d_edad", "d_genero", "d_hombres", "d_mujeres", "d_tipo", "metodo", "sector", "etnia", "discapa")

descripciones <- c("Año del registro", "ID del municipio en el que se encuentra", "Nombre del municipio en el que se encuentra", "Nombre de la provincia en la que está ubicado", "Tipo de sector en el que se encuentra", "Tipo de zona en la que se encuentra", "Código de ubicación geográfica de la institución", "Nombre de la institución en la cual está estudiando", "ID de la sede en la que se encuentra", "Nombre de la sede en la que está", "Jornada a la que asiste el estudiante", "Grado que cursa", "Edad del estudiante", "Género en formato de tipo caracter", "Género en formato binario con 1 para masculino y 0 para femenino", "Género en formato binario con 0 para masculino y 1 para femenino", "Razón o tipo de desplazamiento", "Método por el cuál se brinda la educación", "Sector al que pertenece", "Etnia a la que pertenece el estudiante", "Discapacidad presente en el estudiante")

datosVariables <- data.frame(Variable = variables, Descripcion = descripciones)

kable(datosVariables, "html") %>%
  kable_styling() %>%
  column_spec(1, bold = TRUE)

estudiantesDesplazados_clean <- estudiantesDesplazados %>%
  apply(2, function(x) gsub("[[:punct:]]", "", x)) %>%
  as_tibble(.) %>%
  select(-d_edad, -d_genero, -d_mujeres, -d_ano) %>%
  mutate(d_sede = str_sub(d_sede, -1)) %>%
  rename(ID_Municipio = d_muni, NombreMunicipio = d_nombmuni, Provincia = d_provincia, NombreSector = d_nomsec, Zona = d_nomzon, CódigoGeográfico = dane_ant, NombreInstitucion = d_nombinst, Sede = d_sede, NombreSede = d_nombsede, Jornada = d_nomjor, Grado = d_grado, Género = d_hombres, TipoDeDesplazamiento = d_tipo, MetodoEducación = metodo, Sector = sector, Edad = edad, Etnia = etnia, Discapacidad = discapa)

estudiantesDesplazados_clean <- estudiantesDesplazados_clean %>%
  mutate(ID_Municipio = as.numeric(ID_Municipio), CódigoGeográfico = as.numeric(CódigoGeográfico), Grado = as.numeric(Grado), Género = as.numeric(Género), Sector = as.numeric(Sector), Edad = as.numeric(Edad))

datatable(head(estudiantesDesplazados_clean), options = list(scrollX = '300px', paging = FALSE, searching=FALSE))





head(estudiantesDesplazados) # para ver mas valores: head(aux, 10)
tail(estudiantesDesplazados)
View(estudiantesDesplazados)
names(estudiantesDesplazados)
class(estudiantesDesplazados$d_muni)
length(estudiantesDesplazados$d_ano)
unique(estudiantesDesplazados$d_provincia)

is.na(estudiantesDesplazados)
sum(is.na(estudiantesDesplazados))

summary(estudiantesDesplazados)

library(skimr)
skim(estudiantesDesplazados)

estudiantesDesplazados %>%
  dplyr::group_by(d_provincia) %>%
  skim()
  
glimpse(estudiantesDesplazados)
dim(estudiantesDesplazados)

#frecuencia de aparición
tabledata <- table(estudiantesDesplazados$d_provincia)
barplot(tabledata, horiz = FALSE, las = 2)
pie(tabledata)

hist(estudiantesDesplazados$edad)

plot(estudiantesDesplazados$edad, estudiantesDesplazados$d_grado, xlim = c(0,70), ylim = c(-2,30))

boxplot(tabledata)
which.max(tabledata)
