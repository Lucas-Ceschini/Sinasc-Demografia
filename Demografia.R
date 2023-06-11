# Importando library / importazione di library
library(readr)
library(tidyverse)
library(readxl)

# Carregando os dados / Carregamento dei dati
municipios <- read_excel("Lista-de-Municípios-com-IBGE-Brasil.xlsx")
rs<-municipios%>%
  filter(UF=='RS')

SINASC_2010 <- read_delim("SINASC_2010.csv",
                          delim = ";", 
                          escape_double = FALSE, 
                          trim_ws = TRUE)
SINASC_2021 <- read_delim("SINASC_2021.csv",
                          delim = ";", 
                          escape_double = FALSE, 
                          trim_ws = TRUE)
SINASC_2019 <- read_delim("SINASC_2019.csv",
                          delim = ";", 
                          escape_double = FALSE, 
                          trim_ws = TRUE)
projecao_popRS<-read_excel("projecoes_2018_populacao_2010_2060_20200406 (4).xls", 
                         sheet = "RS")

RS_SINASC_2010<-SINASC_2010%>%
  filter(CODMUNRES %in% rs$IBGE)
RS_SINASC_2021<-SINASC_2021%>%
  filter(CODMUNRES %in% rs$IBGE)
RS_SINASC_2019<-SINASC_2019%>%
  filter(CODMUNRES %in% rs$IBGE)

# criando do DF da população de mulheres RS ----
# creazione di DF dalla popolazione di donne in RS
mulheres_popRS<-data.frame(projecao_popRS[-1:-25,])
mulheres_popRS<-data.frame(mulheres_popRS[-23:-265,])

mulheres2010<-data.frame(mulheres_popRS[-1:-6,-3:-52])
mulheres2010<-data.frame(mulheres2010[-8:-22,])

mulheres2019<-data.frame(mulheres_popRS[-1:-5,-12:-52])
mulheres2019<-data.frame(mulheres2019[-10:-22,])

mulheres2021<-data.frame(mulheres_popRS[-1:-5,-14:-52])
mulheres2021<-data.frame(mulheres2021[-10:-22,])

# criando DF da população RS ----
# creazione di DF dala popolazione in RS
geral_popRS<-data.frame(projecao_popRS[-1:-50,])
geral_popRS<-data.frame(geral_popRS[-21:-300,])

# 2010
geral2010<-data.frame(geral_popRS[,-3:-52])
# 2019
geral2019<-data.frame(geral_popRS[,-3:-10])
# 2021
geral2021<-data.frame(geral_popRS[,-3:-12])

################################################################################
# Ano de 2010 ----
#TBN - Taxa Bruta de Natalidade
n=count(RS_SINASC_2010)
p=geral2010[1,2]
tbn_2010=1000*(n$n/p)
print(tbn_2010)

#TFG - Taxa de Fecundidade Geral
n=count(RS_SINASC_2010)
p=sum(mulheres2010$...2)
TFG2010<-(n*1000)/p

#TEF Taxa Específica de Fecundidade
RS_SINASC_2010$Idade<- cut(RS_SINASC_2010$IDADEMAE, 
                           breaks=seq(15,50,5), 
                           labels = c('15-19','20-24',
                                      '25-29','30-34','35-39',
                                      '40-44','45-49'))
dados<-RS_SINASC_2010%>%
  group_by(Idade,SEXO)%>%
  summarise(freq=n())
TEF2010<-data.frame(mulheres2010)#[,-2:-12])
TEF2010<-TEF2010%>%
  rename( 'Idade - 2010' = RIO.GRANDE.DO.SUL,
          'Mulheres' = ...2)
TEF2010$NV<-c(dados$freq[-8])
TEF2010$nFx<-c(round((TEF2010$NV)/TEF2010$Mulheres,digits = 5))

#TFT - Taxa de Fecundidade Total
TFT2010<-5*sum(TEF2010$nFx)

#TBR - Taxa Bruta de Reprodução
TBR2010<-TEF2010
dados<-RS_SINASC_2010%>%
  group_by(Idade,SEXO)%>%
  summarise(freq=n())%>%
  filter(SEXO==2)
TBR2010$NV_MULHERES<-dados$freq[-8]
TBR2010$tbr<-c(round((TBR2010$NV_MULHERES)/TBR2010$Mulheres,digits = 5))
tbr2010=5*sum(TBR2010$tbr)


################################################################################
# Ano de 2019 ----
#TBN - Taxa Bruta de Natalidade
n=count(RS_SINASC_2019)
p=geral2019[1,3]
tbn_2019=1000*(n$n/p)
print(tbn_2019)

#TFG - Taxa de Fecundidade Geral
n=count(RS_SINASC_2019)
p=sum(mulheres2019$...11)
TFG2019<-(n*1000)/p

#TEF Taxa Específica de Fecundidade
RS_SINASC_2019$Idade<- cut(RS_SINASC_2019$IDADEMAE, 
                           breaks=seq(15,50,5), 
                           labels = c('15-19','20-24',
                                      '25-29','30-34','35-39',
                                      '40-44','45-49'))
dados<-RS_SINASC_2019%>%
  group_by(Idade)%>%
  summarise(freq=n())
TEF2019<-data.frame(mulheres2019[c(-1,-9),-2:-10])
TEF2019<-TEF2019%>%
  rename( 'Idade - 2019' = RIO.GRANDE.DO.SUL,
          'Mulheres' = ...11)
TEF2019$NV<-c(dados$freq[-8])
TEF2019$nFx<-c(round((TEF2019$NV)/TEF2019$Mulheres,digits = 5))

#TFT - Taxa de Fecundidade Total
TFT2019<-5*sum(TEF2019$nFx)

#TBR - Taxa Bruta de Reprodução
TBR2019<-TEF2019
dados<-RS_SINASC_2019%>%
  group_by(Idade,SEXO)%>%
  summarise(freq=n())%>%
  filter(SEXO==2)
TBR2019$NV_MULHERES<-dados$freq[-8]
TBR2019$tbr<-c(round((TBR2019$NV_MULHERES)/TBR2019$Mulheres,digits = 5))
tbr2019=5*sum(TBR2019$tbr)

################################################################################
# Ano de 2021 ----
#TBN - Taxa Bruta de Natalidade
n=count(RS_SINASC_2021)
p=geral2021[1,3]
tbn_2021=1000*(n$n/p)
print(tbn_2021)

#TFG - Taxa de Fecundidade Geral
n=count(RS_SINASC_2021)
p=sum(mulheres2021$...13)
TFG2021<-(n*1000)/p

#TEF Taxa Específica de Fecundidade
RS_SINASC_2021$Idade<- cut(RS_SINASC_2021$IDADEMAE, 
                           breaks=seq(15,50,5), 
                           labels = c('15-19','20-24',
                                      '25-29','30-34','35-39',
                                      '40-44','45-49'))
dados<-RS_SINASC_2021%>%
  group_by(Idade)%>%
  summarise(freq=n())
TEF2021<-data.frame(mulheres2021[c(-1,-9),-2:-12])
TEF2021<-TEF2021%>%
  rename( 'Idade - 2021' = RIO.GRANDE.DO.SUL,
          'Mulheres' = ...13)
TEF2021$NV<-c(dados$freq[-8])
TEF2021$nFx<-c(round((TEF2021$NV)/TEF2021$Mulheres,digits = 5))

#TFT - Taxa de Fecundidade Total
TFT2021<-5*sum(TEF2021$nFx)

#TBR - Taxa Bruta de Reprodução
TBR2021<-TEF2021
dados<-RS_SINASC_2021%>%
  group_by(Idade,SEXO)%>%
  summarise(freq=n())%>%
  filter(SEXO==2)
TBR2021$NV_MULHERES<-dados$freq[-8]
TBR2021$tbr<-c(round((TBR2021$NV_MULHERES)/TBR2021$Mulheres,digits = 5))
tbr2021=5*sum(TBR2021$tbr)
