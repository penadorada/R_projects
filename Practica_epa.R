library(dplyr)
library(ggplot2)
library(data.table)
library(MicroDatosEs)
library(reshape2)

# Descargamos los datos desde el portal del INE http://www.ine.es/prodyser/microdatos.htm
# Utilizamos el paquete MicroDatosEs para leer los de datos de la EPA

epa<-epa2005('datos_1t17.zip')
epa<-data.table(epa)

# Inspeccionamos los datos.
glimpse(epa)

# Deducimos el número de OCUPADOS
# Desglose por CCAA y tipo de ocupación

ocupa<-epa[epa$AOI=='Ocupados subempleados por insuficiencia de horas' | epa$AOI=='Resto de ocupados']

ocupa1<-ocupa%>% 
    group_by(SITU) %>% 
    summarise(tot_ocupados_Sector=n()) 
   
ocupa1<-na.omit(ocupa1)
ocupa1

# Ocupados Totales en España en el Primer Trimestre 2017

sum(ocupa$FACTOREL)/100

# Ocupados en cada CCAA
ocupa2<-ocupa %>% 
  group_by(CCAA) %>% 
  summarise(Total_CCAA=sum(FACTOREL)/100)
ocupa2

# Gráfico distribución ocupados en España
ggplot(ocupa2, aes(y=Total_CCAA, x = CCAA, col='red')) + geom_bar(stat='identity',aes(fill='black')) +theme_bw() +coord_flip() 

# Desglose ocupación por sectores actividad y sexo
## Se comprueba como ciertas profesiones están copadas por una sexo o por otro.
ocupa3<- ocupa %>% group_by(ACT,SEXO) %>% 
  summarise(ocupados_actividad=n()) %>% 
  arrange(desc(ocupados_actividad))

View(dcast(ocupa3, ACT  ~ SEXO))

# Porcentaje ocupados sector público vs Sector Privado
ocupa4<-subset(epa,select = c('CCAA','SITU','FACTOREL'))
ocupa4<-na.omit(ocupa4)

ocupa4$Pub_Priv[!ocupa4$SITU=='Asalariado sector público']<-'Trabajador Sect Privado'
ocupa4$Pub_Priv[ocupa4$SITU=='Asalariado sector público']<-'Trabajador sect Público'

ocupa4 %>% 
  group_by(CCAA,Pub_Priv) %>% 
  summarise(Tot_sector=n())  %>% 
  group_by(CCAA) %>% 
  mutate(Ratio=round((Tot_sector/sum(Tot_sector)*100),2))
           
# Número Total ocupados en sector Privado

ocupa4b<-subset(epa,select = c('CCAA','SITU','FACTOREL'))
ocupa4b$Pub_Priv[!ocupa4b$SITU=='Asalariado sector público']<-'Trabajador Sect Privado'
ocupa4b$Pub_Priv[ocupa4b$SITU=='Asalariado sector público']<-'Trabajador sect Público'
ocupa4b<-na.omit(ocupa4b)

ocupa4b['SITU']<-NULL

ocu5<- ocupa4b %>% 
  group_by(CCAA,Pub_Priv) %>% 
  summarise(tot_pp=sum(FACTOREL)/100) %>% 
  filter(Pub_Priv=='Trabajador Sect Privado') 

sum(ocu5$tot_pp)
   
# Número de  OCUPADOS España  por tipo de contratos

ocupa$Contrato[is.na(ocupa$DUCON1)]<-'Trab cuenta propia'
ocupa$Contrato[ocupa$DUCON1=='Temporal']<-'Temporal'
ocupa$Contrato[ocupa$DUCON1=='Indefinido']<-'Indefinido'

ocupa5<-subset(ocupa,select = c('CCAA','Contrato','FACTOREL'))

## En  España:
ocupa5 %>% 
  group_by(Contrato) %>% 
  summarise(tot_poblac=sum(FACTOREL)/100)

### Desglose por CCAA:
ocupa5 %>% 
  group_by(CCAA,Contrato) %>% 
  summarise(tot_poblac=sum(FACTOREL)/100)

#Calculamos las tasa de paro.
## 1º Determinamos la población Activa.
## Eliminamos los inactivos (columna AOI) y los menores de 16 años.

library(stringr)
act<-epa %>% 
  filter(!str_detect(AOI,'Inactivos')) 

act<-act[!act$EDAD=='de 0 A 4 años',]
act<-act[!act$EDAD=='de 5 A 9 años',]
act<-act[!act$EDAD=='de 10 A 15 años',]

## Definimos los parados
act$parados[str_detect(act$AOI,'Parados')]<-1
act$parados[!str_detect(act$AOI,'Parados')]<-0

act$parados<-as.numeric(act$parados)
p<-round(sum(act$FACTOREL * act$parados) / sum(act$FACTOREL)*100,2)
p

# Cálculo de la Tasa de Paro por provincia.

actp<-act[,c('CCAA','parados','FACTOREL')]

actp %>% 
  group_by(CCAA,parados) %>%
  summarise(Total_Parados=n()) %>% 
  mutate(Tasa_Paro_ccaa=(Total_Parados/sum(Total_Parados))*100) %>% 
  filter(parados==1)

# Distinción por sexo y ciudad de los trabajadores sin trabajo.

actp<-act[,c('CCAA','PRONA','SEXO','parados','FACTOREL')]

acts<-actp %>% 
  group_by(PRONA,SEXO,parados) %>% 
  summarise(Sexo_parados=n())  %>% 
  filter(parados==1) %>% 
  group_by(PRONA) %>% 
  mutate(tasa_paro=Sexo_parados/sum(Sexo_parados)*100)

actss<-dcast(acts,PRONA ~ SEXO)
actss

# Desglose de desempleados por rangos de  edad.
## Nota: resultado engañoso puesto que no están incluidos los inactivos desanimados.

actp<-act[,c('CCAA','PRONA','EDAD','parados','FACTOREL')]

actp %>% 
  group_by(EDAD,parados) %>%
  summarise(Total_Parados=n()) %>% 
  mutate(Tasa_Paro_ccaa=(Total_Parados/sum(Total_Parados))*100) %>% 
  filter(parados==1)

