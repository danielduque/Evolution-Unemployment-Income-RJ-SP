rm(list = ls())

library(bit64)
library(data.table)
library(descr)
library(readr)
library(survey)
library(checkmate)
library(lme4)
library(oaxaca)
library(dplyr)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(transformr)
library(zoo)
library(foreach)
library(readxl)
library(reshape2)
library(geobr)
library(scales)
library(maptools)
library(RColorBrewer)
library(stringi)
library(PNADcIBGE)
library(survey)
library(dineq)
library(convey)
library(IC2)
library(RJ)
library(tigris)
library(highcharter)
library(viridis)
library(rworldmap)
library(tweenr)
library(ggthemes)
library(rgeos)
library(countrycode)
library(curl)
library(tidyr)

# Escolher um working directory que tenha os exceis disponibilizados
wd <- "C:/Users/DaniellaBritto/Desktop/PNADs/PNADsCont/PNADC - dta Novo Dicionário (2019)"

setwd(wd)

# Excel com nomes dos txt's das PNADCs
PNADCs <- read_excel("PNADCs.xlsx")

# Limitar ao Trimestre 2
PNADCs <- PNADCs %>% filter(Trimestre==2)

# Excel com deflatores estaduais
Deflator <- read_excel("C:/Users/DaniellaBritto/Desktop/PNADs/PNADsCont/PNADC - dta Novo Dicionário (2019)/deflator_tri_040506_2019_pnadc.xls",sheet="deflator")

# Dataframe do RJ para Desemprego e Renda Domiciliar per Capita por Tipo geográfico
# Tipo: Capital, Resto da Região Metropolitana e Resto do Estado
RJ <- as.data.frame(matrix(nrow=1,ncol=4))

# Dataframe do RJ para Desemprego e Renda Domiciliar per Capita de todo Estado
Desemp_tempo <- as.data.frame(matrix(nrow=8,ncol=3))

# Nomeando as variáveis

Desemp_tempo <- Desemp_tempo %>% rename(AnoTri=V1,
                                      `RTDpc Médio`=V2,
                                      `Desemprego`=V3)
                                      

RJ <- RJ %>% rename(AnoTri=V1,
                              Tipo=V2,
                              `RTDpc Médio`=V3,
                              `Desemprego`=V4)


options(survey.lonely.psu  = "average")

# Começar o loop
for (x in 1:8){
  
  PNADC_T <- read_pnadc(microdata = PNADCs$PNADCs[x], input_txt="Input_PNADC_trimestral.txt", vars= c("UPA", "V1028", "UF", "Trimestre", "Ano", "V1008", "V1014", "V1016", "V2001","V2007", "V2010", "V1022", "V20082", "VD4002",  "V2008", "V20081", "VD4031", "VD3004", "V2009", "VD4019", "V4040", "V40401", "V40402", "V40403", "VD3005","V1023"))
  
  # Juntar PNADC_T com Deflatores
  PNADC_T <- merge(PNADC_T,Deflator,by=c("Ano","Trimestre","UF"))
  
  # Filtrar para o Rio de Janeiro
  PNADC_T <- PNADC_T %>% filter(UF==33)
  
  # Construção das variáveis principais
  PNADC_T <- PNADC_T %>% mutate(UF = as.factor(UF),
                                Ano = as.factor(Ano),
                                Trimestre = as.factor(Trimestre),
                                V2001 = as.numeric(V2001),
                                V2007 = as.factor(V2007),
                                V2010 = as.factor(V2010),
                                V1022 = as.factor(V1022),
                                V20082 = as.numeric(V20082),
                                VD4002 = as.factor(VD4002),
                                V2008 = as.numeric(V2008),
                                V20081 = as.numeric(V20081),
                                VD4031 = as.numeric(VD4031),
                                VD3004 = as.factor(VD3004),
                                V2009 = as.numeric(V2009),
                                VD4019 = as.numeric(VD4019),
                                V4040 = as.factor(V4040),
                                V40401 = as.numeric(V40401),
                                V40402 = as.numeric(V40402),
                                V40403 = as.numeric(V40403),
                                VD3005 = as.numeric(VD3005),
                                AnoTri=paste(Ano,Trimestre,sep="."),
                                RendaTrab=ifelse(VD4002==1 & is.na(VD4019)==F & VD4019<=999999, VD4019*Habitual, ifelse(VD4002==2,0,NA)),
                                QlqrRendaTrab=ifelse(is.na(RendaTrab)==T,0,RendaTrab),
                                id_dom = as.numeric(paste(UPA,V1008,V1014, sep = "")),
                                pessoa= 1,
                                PD=ifelse(VD4002==2,1,ifelse(VD4002==1,0,NA)),
                                V1023=ifelse(V1023==4,3,V1023))
  
  # Construir variável de renda domiciliar e número de indivíduos no domicílio
  PNADC_T <- PNADC_T %>% group_by(id_dom) %>% mutate(n_ind=sum(pessoa),
                                                     RTD=sum(QlqrRendaTrab))
  # Renda Domiciliar Per Capita
  PNADC_T <- PNADC_T %>% mutate(RTDpc=RTD/n_ind)
  
  # Criar base de dados para ler a PNADC como uma base amostral complexa 
  dstrat1 <- svydesign(id=~UPA, strata=~Estrato, weights=~V1028, data=PNADC_T)
  
  # Calcular Renda Domiciliar per Capita e Taxa de Desemprego
  RTDOMpcmed <- svymean(~RTDpc, dstrat1, na.rm = T)
  Desemp <- svymean(~PD, dstrat1, na.rm = T)
  
  # Imputar na base criada anteriormente
  Desemp_tempo$AnoTri[x]=PNADC_T$AnoTri[1]
  Desemp_tempo$`RTDpc Médio`[x]= RTDOMpcmed
  Desemp_tempo$`Desemprego`[x] = Desemp

  
  # Criar base que será adicionada à RJ
  PNADC_T2 <-  as.data.frame(matrix(nrow=3,ncol=4))
  PNADC_T2 <- PNADC_T2 %>% rename(AnoTri=V1,
                                  Tipo=V2,
                                  `RTDpc Médio`=V3,
                                  `Desemprego`=V4)
  
  # Loop para gerar as variáveis por Capital, Resto da RM e Resto da UF
  for(y in 1:3){
    PNADC_T2$AnoTri[y]= PNADC_T$AnoTri[y]
    PNADC_T2$Tipo[y]= y 
    PNADC_T2$`RTDpc Médio`[y] <- svyratio(~RTD, ~n_ind, subset(dstrat1,V1023==y), na.rm = T)
    PNADC_T2$`Desemprego`[y] <- svymean(~PD, subset(dstrat1,V1023==y), na.rm = T)
    
  }
  
  #Jogar resultados anteriores na base RJ
  
  RJ <- rbind(RJ,PNADC_T2)    
  
}

rm(PNADC_T)
rm(dstrat1)

# Excluir a primeira linha (vazia)
RJ <- RJ[2:(3*8+1),]

# Trocar 1, 2 e 3 por "Capital", "Resto da RM" e "Resto da UF"
RJ <- RJ %>% mutate(Tipo=ifelse(Tipo==1,"Capital",ifelse(Tipo==2,"Resto da RM","Resto da UF")))

# Gráfico de linhas
ggplot(RJ,aes(x=AnoTri,group=Tipo))+
  geom_line(aes(y=Desemprego*100,colour=Tipo))+
  labs(title="Desemprego no RJ (em %)", caption="Fonte: PNAD Contínua 2ºTri",x="",y="",colour="")+
  theme(axis.text.x = element_text(angle = 30,size=8))

ggsave(file="Desemprego_RJ_graf.png")

# Exportar para base csv
fwrite(Desemp_tempo, file ="Desemp_Tempo_RJ.csv")
fwrite(RJ, file ="Renda_Desemp_RJ.csv")

# Usar geobr para ler os municípios do RJ
UF <- read_municipality(year=2010,code_muni=33)

# Ler a classificação dos municípios por Tipo
Rio <- read_excel("Tipo_Mun_RJ.xlsx",sheet="Mun_RJ")

# Juntar Rio com RJ por Tipo
RJ2 <- merge(RJ,Rio,by="Tipo")

# Criar variável equivalente à da base UF, com código do município
RJ2 <- RJ2 %>% mutate(code_muni=paste(UF,Cod_Mun,sep=""))

# Juntar RJ2 com UF por código do município
RJfim <- merge(RJ2,UF,by="code_muni")

# Multiplicar por 100
RJfim <- RJfim %>% mutate(Desemprego=Desemprego*100)

# Primeiro mapa-gif (Desemprego)
plot1 <- ggplot() + geom_sf(data=RJfim, aes(fill= as.numeric(`Desemprego`)),
                            color = "grey", size = 0.0001) +
  scale_fill_distiller(palette = "Reds", 
                       breaks = pretty_breaks(n = 6),
                       direction=1)+
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title="Desemprego no RJ", subtitle = "AnoTri: {current_frame}", caption="Fonte: PNAD Contínua 2ºTri",x="",y="",fill="")+
  coord_sf(datum = NA) + 
  transition_manual(AnoTri)

# Segundo mapa-gif (Renda Domiciliar per Capita)
plot2 <- ggplot() + geom_sf(data=RJfim, aes(fill= as.numeric(`RTDpc Médio`)),
                            color = "grey", size = 0.0001) +
  scale_fill_distiller(palette = "Blues", 
                       breaks = pretty_breaks(n = 6),
                       direction=1)+
  guides(fill = guide_legend(reverse = TRUE))+
  labs(title="Renda do Trabalho Domiciliar Per Capita no RJ", subtitle = "AnoTri: {current_frame}", caption="Fonte: PNAD Contínua 2ºTri",x="",y="",fill="")+
  coord_sf(datum = NA) + 
  transition_manual(AnoTri)


anim_save(plot1,file="Desemp_RJ.gif")
anim_save(plot2,file="RTDPC_RJ.gif")