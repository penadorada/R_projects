
#---------------------------------------------------------------------------------------------------------------------
# ANÁLISIS DATOS DE INSTALACIONES DE PRODUCCIÓN ELECTRICA EN ESPAÑA. MINISTERIO DE INDUSTRIA.
# REGISTRO DE PRODUCTORES DE ENERGÍA ELÉCTRICA. 
# https://sede.minetur.gob.es/es-ES/datosabiertos/catalogo-datos/
#---------------------------------------------------------------------------------------------------------------------
# Importamos,leemos e inspeccionamos  los datos:
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)

ele<-read.csv2('electraexp.csv',sep=';',header=TRUE, dec = ",")

glimpse(ele)
ele[duplicated(ele),]
View(ele)
#------------------------------------------------------------------------------------------------------------------
#     ANÁLISIS TEMPORAL DE LAS INSTALACIONES ELÉCTRICAS EN ESPAÑA
#------------------------------------------------------------------------------------------------------------------

# Eliminamos columna 'Alta.registro'(todo NAs) y corregimos el nombre de las columnas 'F..Alta provicional', 'F..Alta' y
# 'F..Puesta.EN.Servicio'
ele['Alta.Registro']<-NULL
colnames(ele)[15] <- 'F_alta_provisional'
colnames(ele)[13] <- 'F_Alta'
colnames(ele)[12] <- 'Fecha_puesta_servicio'


# Cambiamos a formato fecha las columnas siguientes:
ele['F_Alta']<-as.Date(ele$F_Alta,"%d/%m/%Y",na.rm=T)
ele['Fecha_puesta_servicio'] <- as.Date(ele$Fecha_puesta_servicio,'%d/%m/%Y')
ele$F_alta_provisional <- as.Date(ele$F_alta_provisional,'%d/%m/%Y',na.rm=T)
summary(ele$Fecha_puesta_servicio)
summary(ele$F_Alta)
summary(ele$F_alta_provisional)

# HISTORIAL DE LA POTENCIA BRUTA INSTALADA EN ESPAÑA DESDE 1900
  # Eliminamos los registros con fecha no disponible
  # Creamos una columna nueva solo con el año de puesta en servicio
  eleH <- ele[!is.na(ele$Fecha_puesta_servicio), ]
  eleH['Puesta_servicio'] <-year(eleH$Fecha_puesta_servicio)
  eleH <- eleH%>% 
    group_by(Puesta_servicio,Tecnologia,Potencia.Bruta) %>% 
    summarise(Instalaciones=n())
# Gráfico
# Eliminamos dos instalaciones con años 3012 y 5013
 eleH <- eleH[!(eleH$Puesta_servicio==3013 | eleH$Puesta_servicio==5012), ]
 
  ggplot(eleH, aes(x=Puesta_servicio, y=Potencia.Bruta)) +geom_bar(stat='identity',fill='blue')+theme_bw()+
    ggtitle('Evolución puesta en marcha instalaciones eléctricas en España')
  
# Gráfico desde el año 2000
  eleh1 <- eleH[eleH$Puesta_servicio>=2000,]
  ggplot(eleh1, aes(x=Puesta_servicio, y=Instalaciones)) +geom_bar(stat='identity',fill='tomato')+theme_bw()+
    ggtitle('Evolución número de instalaciones eléctricas  en España')

# ANÁLISIS CRONOLÓGICO DEL TIPO DE GENERACIÓN ELÉCTRICA EN ESPAÑA DESDE EL AÑO 2000  
  eleH1 <-eleH %>% 
    filter(Puesta_servicio>=2000) %>% 
    group_by(Puesta_servicio,Tecnologia) %>% 
    summarise(Total=sum(Potencia.Bruta))
 # Gráfico
   ggplot(eleH1, aes(x=Puesta_servicio, y=Total)) +geom_bar(stat='identity',aes(fill=Tecnologia),color='black')+theme_bw()+
    ggtitle('Evolución puesta en marcha instalaciones eléctricas en España desde 2000')
 # Tabla resumen por año de la potencia activada según tecnología
    eleH2 <-dcast(eleH1, Puesta_servicio ~ Tecnologia)
    View(eleH2)
#-------------------------------------------------------------------------------------------------------------------------

## 1) ANÁLISIS GEOGRÁFICO DE LA POTENCIA INSTALADA
  
#-------------------------------------------------------------------------------------------------------------------------
#   Potencia instalada por provincia:
  eleP <- ele %>% 
    group_by(Autonomia,Provincia) %>% 
    summarise(Total=sum(Potencia.Bruta))

## Top 3 provincias con menos potencia bruta:
  eleP %>% 
    arrange(Total) %>% 
    head(3)

## Top 3 provincias con más potencia bruta:
  eleP %>% 
    arrange(desc(eleP$Total)) %>% 
    head(3)

## Gráfico Potencia Bruta en España por CCAA:
    ele %>% 
     filter(is.na(ele$Potencia.Bruta) | is.na(ele$Potencia.Neta))
 
 # Potencia bruta tiene 10 NAs y Potencia Neta 205. Eliminamos los 10  Nas de potencia bruta y utilizamos 
 # potencia bruta como medida de la potencia instalada:
     ele<-ele[!is.na(ele$Potencia.Bruta),]
  
 # Determinamos potencia total por CCAA:
  eleP<-ele %>% 
    group_by(Autonomia) %>% 
    summarise(Total=sum(Potencia.Bruta)) %>% 
    arrange(desc(Total))
 
 # Gŕafico
   ggplot(eleP, aes(x=Autonomia, y= Total)) + geom_bar(stat='Identity',fill='blue') + coord_flip() +theme_bw() + ggtitle('Potencia Bruta en España por CCAA')

 # Potencia bruta según el tipo de tecnología:
  # Hay 41 registros  en los que la variable 'Tecnología' está en blanco.Lo sustituimos por 'Desconocido':
    ele$Tecnologia<-as.character(ele$Tecnologia)
    ele$Tecnologia[ele$Tecnologia=='']<-'Desconocido'
  
 # Clasificación del tipo de generación por potencia bruta: 
  eleG<-ele %>% 
    group_by(Tecnologia) %>% 
    summarise(Total=sum(Potencia.Bruta)) %>% 
    arrange(Total,Tecnologia)
  View(eleG)
    # La eólica terrestre es claramente la  fuente más importante en España
  
  # Gráfico
    ggplot(eleG,aes(x='',y=Total,fill=Tecnologia)) + geom_bar(width = 0.5,stat='identity',col='black') + coord_polar(theta = 'y',start=0) +
         ggtitle('Desglose tipos de tecnologias de generación')
     
 # Top 3 tecnologias de producción bruta por provincia:
  ele %>% 
    group_by(Provincia,Tecnologia) %>% 
    summarise(Tot_Product=sum(Potencia.Bruta)) %>% 
    arrange(desc(Tot_Product)) %>% 
    slice(1:3)
  
 # Provincias líderes según la tecnología de producción. 
   ele %>% 
    group_by(Tecnologia,Provincia) %>% 
    summarise(Tot_Product=sum(Potencia.Bruta)) %>% 
    slice(which.max(Tot_Product)) %>% 
    arrange(desc(Tot_Product))
   #(Destaca Murcia, Nº 1 en tres tecnologías) 
   
#------------------------------------------------------------------------------------------------------------------------
 
## 2)  ANÁLISIS PROPIEDAD DE LAS INSTALACIONES. PRODUCTORES PÚBLICO/PRIVADOS/PARTICULARES

#------------------------------------------------------------------------------------------------------------------------
  # 1º Creamos una nueva columna (Tipo_Titular) que identifique el tipo de productor.
  # 2º Clasificamos los tipos de productores en:
  #    - empresa (SL,SA,SC,SAU,AIE, etc),
  #    - Instituciones públicas (empresas municipales, hospitales, universidades, entidades, diputaciones, etc) 
  #    - Ayuntamientos
  #    - Particulares (personas físicas, cooperativas,fundaciones)
  
ele$Titular<-as.character(ele$Titular)

  ele$Tipo_Titular[grepl('\\sSL$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sS.A.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sSA$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sS.L.U.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sSLU$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sS.L.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sS.A$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sS.L$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sSAU$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('AJUNTAMENT',ele$Titular,ignore.case = T)]<-'ayuntamiento'
  ele$Tipo_Titular[grepl('AJUANTAMENT',ele$Titular,ignore.case = T)]<-'ayuntamiento'
  ele$Tipo_Titular[grepl('Ayuntamiento',ele$Titular,ignore.case = T)]<-'ayuntamiento'
  ele$Tipo_Titular[grepl('AYTO',ele$Titular,ignore.case = T)]<-'ayuntamiento'
  ele$Tipo_Titular[grepl('AYUNTAM',ele$Titular,ignore.case = T)]<-'ayuntamiento'
  ele$Tipo_Titular[grepl('ACCIONA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('GAMESA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('IBERDROLA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('ENTE VASCO DE LA ENERGIA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
  ele$Tipo_Titular[grepl(',SL$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',SA$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sS.C.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('S.A.U.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('FERSA' ,ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('REPSOL',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('^IES',ele$Titular,ignore.case = T)]<-'Institución_Pública'
  ele$Tipo_Titular[grepl('S.C.P.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sSL.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',S.A$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sS.L.U',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('\\sENTE\\s',ele$Titular,ignore.case = T)]<-'Institución_Pública'
  ele$Tipo_Titular[grepl(',S.L.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',S.A.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sS.A.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sS.A.U$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sA.G.$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',\\sS.\\sA.$',ele$Titular,ignore.case=T)]<-'empresa'
  ele$Tipo_Titular[grepl('ARAGONESA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('HUNOSA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('GEPESA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('ENDESA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('ELECNOR',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('NATURENER',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('VIESGO',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('CETASA',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('AIE$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('S.A..$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl(',S.L$',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('S.L.',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('S.A.',ele$Titular,ignore.case = T)]<-'empresa'
  ele$Tipo_Titular[grepl('A.I.E.',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[ele$Titular=='AENA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='RENFE']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='MUSA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='FOMENTO DE CONSTRUCCIONES Y CONTRATAS']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='DYTA EOLICA DE CASTILLA LA MANCHA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='UNELCO']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ECYR']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENEMANSA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PEMALSA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='EMASESA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIAS RENOVABLES DE LA REGION DE MURCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIAS RENOVABLES DE LA REG. DE MURCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIAS REN. REGION DE MURCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIAS RENOVABLES  DE LA REGION DE  MURCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIA RENOVABLES DE LA REG. DE MURCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='DESARROLLOS EOLICOS MANCHEGOS EL PINAR,']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='DESARROLLO DE ENERGIAS RENOVABLES RIOJA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PARQUES EOLICOS DE LA REGION DE MURCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='P. E. DE MALPICA,SA(PEMALSA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PARQUE EOLICO MADRIDEJOS, S..']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='INACSA (INDUSTRIA DEL ACETATO SA)']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENVIRONMENTAL INTERNATIONAL ENGINEERING,']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='POLIAMIDAS Y ALUMINIOS PENARROYA-PUEBLON']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PAPELERA GUIPUZCOANA DE ZICUNAGA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='EXPLOTACIONES EOLICAS SIERRA DE UTRERA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='COMPANIA EOLICA GRANADINA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENVIRONMENTAL INTERNATIONAL ENGINEERING']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='BABCOCK KOMMUNAL MBH(26%)-TECMED, SA(74%']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='EUROPEAN SUN PARK ARNEDO']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='SEPIOLSA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIAS RENOVABLES BONETE 7']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PARQUE EOLICO BAIX EBRE, SA(PEBESA)']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PARC EOLIC LES COMES, SL (ANT ECOTECNIA)']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='N.E.G. MICON']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='TRATAM. AMBIENTALES SIERRA DE LA TERCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='CORPORACION EOLICA DE BARRUELO']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ALCOHOLERA DE LA PUEBLA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='FOTOVOLTAICAS MAHORA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ENERGIAS RENOVABLES BONETE 8']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='INVESTIGACION Y DESARROLLO DE ENERGIASA RENOABLES IDER, S.L.']<-'empresa'
    ele$Tipo_Titular[grepl('VALORIZA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('GUASCOR',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('TELEFONICA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('^HIDROELECTRICA DE',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('SL.',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('UNIPERSONAL',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('NUFRI',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('I.D.A.E',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('A.I.E',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('AIE\\s',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('VALORIZA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('EUROPAC',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('ECOVASA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('MAPFRE',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('SICOGESA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('S.R.L.',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('\\s.SL.',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('\\sSLU.$',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('\\sSLU\\s',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl(',\\sSA\\s',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('IDAE',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('\\sS.\\sL.$',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl(',\\sS.\\sL.$',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl(',\\s\\sS.A.$',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl(',SA.$',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl(',\\sSA.$',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('IDER',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('UNIVERSIDAD',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('UNIVERSITAT',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('(CENER)',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('HOSPITAL',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('SOGAMA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('CANAL DE ISABEL II',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('FERIA MUESTRARIO internacional valencia',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('JUNTA DE ANDALUCIA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CIMALSA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('FERROCARRILS DE LA GENERALITAT VALENCIANA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('PATRONATO MUNICIPAL DEPORTES  ALCOBENDAS',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('E. M. T. URBANS DE PALMA DE MALLORCA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CONSORCIO PALACIO DE CONGRESOS DE VALENCIA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CALDES XXI (ENTITAT PUBLICA EMPRESARIAL LOCAL)',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('AIGUES TER LLOBREGAT',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('DIPUTACION',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('ENTIDAD PUBLICA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('ENTITAT PUBLICA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('AIGUES DE ',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('SIRUSA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('JUNTA DE ',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CONSORCIO DE AGUAS',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CIUDAD DEPORTIVA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('INSTITUTO',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CONFEDERACION HIDROGRAFICA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('UNIVERSITARIA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('GESTION Y DESARROLLO',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CONSEJERIA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('EMPRESA METROPOLITANA',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('EMPRESA MUNICIPAL',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('RESIDUAL',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('INDUSTRIAS',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('SINAE',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('CERAMICA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('PRODUCTOS CERAMICOS',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('HIDRO ENERGIA XANA ',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('MINICENTRALES IMPERIAL GALLUR',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('LINEGAS',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('INEUROPA DE COGENER',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('INVESTIGACION ENERGETICA CASTILLA LEON',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('CONSORCIO GESTION',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='MEDIO AMBIENTE, AGUA, RESIDUOS Y ENERGIA DE CANTABRIA']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='TRARGISA']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='UNION EOLICA ANDALUZA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PEQUENAS CENTRAL.HIDR.SA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='AUXIME']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='CEPRISA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='SA LLENSA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='BALMIMESA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='INVESTIGACION ENERGETICA CASTILLA LEON']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='CONSTRUCCIONES ESPECIALES Y DRAGADOS']<-'empresa'
    ele$Tipo_Titular[grepl('CONSORCIO DE BOMBEROS',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('ORGANISMO AUTONOMO PARQUES',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('ECOENERGIES BARCELONA SUD',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('^PEQUENAS CENT.HIDROEL.SA',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('ENERVAP COGENERACION',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[grepl('^CONSORCI GESTIO\\s',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('^CONSORCI PER\\s',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('ENERGETIC CASTILLA LEON',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[grepl('concello',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='PATRONATO M.DE DEPORTES DE ALCOBENDAS']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='INVESTIGACION MAS DESARROLLO ENERGETICO.']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='APROVECH.Y RECUPER.ENERG.']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='MDT BITUMEN OU']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='EXPLOTACIONES ENERGETICAS CASTILLA LEON']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='EXPLOTACIONES EN.CASTILLA-LA MANCHA']<-'Institución_Pública'
    ele$Tipo_Titular[grepl('HOSP.UNIVERS.$',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='SATARRA, C.B.']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='PARQUES FOTOVOLTAICOS DE FUENTENOVILLA Y VALDENOCHES']<-'empresa'
    ele$Tipo_Titular[grepl('CERAMIQUES',ele$Titular,ignore.case = T)]<-'empresa'
    ele$Tipo_Titular[ele$Titular=='INVESTIGACION ENERGETICA  CASTILLA LEON']<-'Institución_Pública'
    ele$Tipo_Titular[ele$Titular=='EXPLOTACIONES ENERGETICAS DEL BAGES']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='SUN SYSTEMS VILOBI NAU PONENT 2']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='ACEITES USADOS REC. ENERG.ANDALUCIA']<-'empresa'
    ele$Tipo_Titular[ele$Titular=='GESTION Y MANTENIMIENTO EOLICO DEL NORTE']<-'empresa'
    ele$Tipo_Titular[grepl('^ENTE\\s',ele$Titular,ignore.case = T)]<-'Institución_Pública'
    ele[20751,'Tipo_Titular']<-'empresa'
    ele$Tecnologia<-as.character(ele$Tecnologia)
    ele[59386,'Tecnologia']<-'Tratamiento de residuos'
    ele$Tipo_Titular[is.na(ele$Tipo_Titular)]<-'Particulares_otros'
    
    table(ele$Tipo_Titular)
    
# Porcentaje de la producción eléctrica por Tipo de titular
     ele %>% 
      group_by(Tipo_Titular) %>% 
      summarise(Tot=sum(Potencia.Bruta)) %>% 
      group_by(Tipo_Titular) %>% 
      summarise(Ratio = (Tot/sum(ele$Potencia.Bruta)*100))
     # (A pesar del número de instituciones públicas y personas físicas que figuran como productores, representan
     # menos del 1% del total. Deducimos que es autoconsumo)
#--------------------------------------------------------------------------------------------------------------

#   1) ANÁLISIS DE LA PRODUCCIÓN DEL SECTOR PÚBLICO (Instituciones públicas y Ayuntamientos)
 
#--------------------------------------------------------------------------------------------------------------
  
      elepub <- ele[ele$Tipo_Titular=='Institución_Pública', ]
    
# Tipo de tecnología preferida por el sector público:
     elepub %>% 
       group_by(Tecnologia) %>% 
       summarise(Total=sum(Potencia.Bruta)) %>% 
       arrange(desc(Total))
       # (Hidraúlica es la más importante)
     
# Mayores productores públicos:
    elepub %>% 
       group_by(Titular,Provincia,Tecnologia) %>% 
       summarise(To=sum(Potencia.Bruta)) %>% 
       arrange(desc(To)) 
      # La confederación hidrográfica del Duero y el Canal de Isabel II
    
# Mayores productores públicos en cada provincia:
    elepub %>% 
      group_by(Provincia,Titular) %>% 
      summarise(Tot=sum(Potencia.Bruta)) %>% 
      arrange(desc(Tot)) %>% 
      slice(1:3)
#------------------------------------------------------------------------------------------------------------------
# UNIVERSIDADES, HOSPITALES y AYUNTAMIENTOS 
    
# UNIVERSIDADES:
    uni <-c('UNIVERSIDAD','Universitat')
    eleU<-elepub[grepl(paste(uni,collapse = '|'),elepub$Titular,ignore.case = T),]
  
  # Universidades productoras más importantes
    eleU %>% 
      group_by(Titular,Provincia,Tecnologia) %>% 
      summarise(Total=sum(Potencia.Bruta)) %>% 
      arrange(desc(Total))
     # (La Universidad de Valencia es claramente la mayor productora en dos tecnologías.)
    
  # Agrupamos por provincias la producción de electricidad 'universitaria'
    eleU %>% 
      group_by(Provincia) %>% 
      summarise(Total=sum(Potencia.Bruta)) %>% 
      arrange(desc(Total))
        # (Valencia y La Coruña son claramente excepciones. Eso nos lleva a pensar que es debido a la puesta 
        # en funcionamiento de planes específicos para el ahorro energético)
    
# HOSPITALES
    hosp <- elepub[grepl('HOSPITAL',elepub$Titular,ignore.case = T), ]
    hosp %>% 
      group_by(Titular,Provincia,Tecnologia) %>% 
      summarise(Total=sum(Potencia.Bruta)) %>% 
      arrange(desc(Total))
       #(Tres hospitales valencianos encabezan el ranking de nuevo)
    
    # Agrupamos por provincias la producción de electricidad 'hospitalaria'
      hosp %>% 
      group_by(Provincia) %>% 
      summarise(Total=sum(Potencia.Bruta)) %>% 
      arrange(desc(Total))
     #(De nuevo Valencia resulta líder)
      
# AYUNTAMIENTOS
      ay <- ele[ele$Tipo_Titular=='ayuntamiento', ]
      ay %>% 
        group_by(Titular,Provincia,Tecnologia) %>% 
        summarise(Total=sum(Potencia.Bruta)) %>% 
        arrange(desc(Total))
        #(El ayuntamiento de Nigüelas en Granada es el ayuntamiento con mayor potencia bruta)
      
      ay %>% 
        group_by(Provincia) %>% 
        summarise(Total=sum(Potencia.Bruta)) %>% 
        arrange(desc(Total))
        #(La provincia de Salamanca es la que mayor capacidad productora tienen sus ayuntamientos)
      ay %>% 
        group_by(Provincia) %>% 
        summarise(Num_aytm=n()) %>% 
        arrange(desc(Num_aytm))
        # (Madrid es la Provincia con mayor número de ayuntamientos productores)
#------------------------------------------------------------------------------------------------------------------   
      
## 3) ANÁLISIS DE LA PRODUCCIÓN DEL SECTOR PRIVADO (empresas)

#------------------------------------------------------------------------------------------------------------------
      
      elepib <- ele[ele$Tipo_Titular=='empresa', ]
      elepib<- elepib %>% 
              arrange(desc(Potencia.Bruta))
      
# Ranking empresas productoras en España
      elepib %>% 
        group_by(Titular) %>% 
        summarise(Tot=sum(Potencia.Bruta)) %>% 
        arrange(desc(Tot))
      # Iberdrola es la más importante
      
# Ranking empresas líderes por tecnología
      elepib %>% 
        group_by(Tecnologia,Titular) %>%  
        summarise(Tot=sum(Potencia.Bruta)) %>% 
        slice(which.max(Tot)) %>% 
        arrange(desc(Tot))
      
# 3 mayores  empresas productoras en cada provincia
      elepib %>% 
        group_by(Provincia,Titular) %>%  
        summarise(Tot=sum(Potencia.Bruta)) %>% 
        arrange(Provincia,desc(Tot)) %>% 
        slice(1:3)
      
#---------------------------------------------------------------------------------------------------------------------
     
       # 4) ANÁLISIS DE LA PRODUCCIÓN SECTOR PRIVADO. Personas físicas, asociaciones, cooperativas
      
#---------------------------------------------------------------------------------------------------------------------
      elepar <- ele[ele$Tipo_Titular=='Particulares_otros', ]

# Ranking mayores productores:
      elepar %>% 
        group_by(Autonomia,Titular) %>% 
        summarise(Total=sum(Potencia.Bruta)) %>% 
        arrange(desc(Total)) %>% 
        head(25)
      #(Dos personas físicas y una cooperativa son los más importantes. Andalucía destaca como la comunidad autónoma
      # con más particulares y cooperativas entre los 10 mayores productores)
      
# Desglose por Tecnología
      elepar %>% 
        group_by(Tecnologia,Titular) %>%
        summarise(Total=sum(Potencia.Bruta)) %>% 
        slice(which.max(Total)) %>% 
        arrange(desc(Total))
    
#---------------------------------------------------------------------------------------------------------------------
      # TABLA RESUMEN 
      # Productores Privados, públicos y particulares más importantes por provincia
#---------------------------------------------------------------------------------------------------------------------
# Fusionamos tablas para visualizar lideres de producción por provincia
      
     pub<- elepub %>% 
        group_by(Provincia,Titular) %>% 
        summarise(Tot=sum(Potencia.Bruta)) %>%
        slice(which.max(Tot)) 
      
     pib <- elepib %>% 
        group_by(Provincia,Titular) %>%  
        summarise(Tot=sum(Potencia.Bruta)) %>% 
        slice(which.max(Tot))
      
     par <-elepar %>% 
        group_by(Provincia,Titular) %>%
        summarise(Total=sum(Potencia.Bruta)) %>% 
        slice(which.max(Total)) 
     
     colnames(pib)[2]<-'Empresa'
     colnames(pub)[2]<-'Institución'
     
     a<-full_join(pub,pib,by='Provincia',all=True)
     resumen <-full_join(a,par,by='Provincia',all=True)
     colnames(resumen)[6]<-'Particular'
     View(resumen)
  
# Gráfico
  #Agregamos columna con el Total por provincia:
    resumen['Total_Provincia']<-resumen$Tot.x+resumen$Tot.y+resumen$Total
    ggplot(resumen, aes(x=Total_Provincia, y=Provincia)) + geom_point(stat='identity')+ theme_bw() + geom_vline(xintercept=mean(resumen$Total_Provincia,na.rm=T),color='blue')
     
     