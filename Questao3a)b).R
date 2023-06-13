library("pacman")
p_load(tidyverse, readxl, lubridate, read.dbc, foreign, read.xl, data.table)

############################################################################################################################################################

## Questao 3

#### Manipulacao do banco ###################################################################################################################################


sim10 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DORS2010.dbc')
simp10 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DORS2010.dbc')
sim10$DTOBITO <- sim10$DTOBITO %>% dmy()
sim10$DTNASC <- sim10$DTNASC %>% dmy()
sim10$IDADE <- sim10$IDADE %>% as.character()
sim10 <- sim10 %>% mutate(sim10, IDADE = case_when(
  substr(IDADE, 1, 1) == '4'~ substr(IDADE, 2, 3),
  substr(IDADE, 1, 1) == '5' ~ paste('1', substr(IDADE, 2, 3), sep = '')))
sim10$IDADE <- sim10$IDADE %>% as.numeric()
sim10$SEXO <- sim10$SEXO %>% as.character()
sim10$SEXO[sim10$SEXO == '1'] <- 'M'
sim10$SEXO[sim10$SEXO == '2'] <- 'F'


sim19 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DORS2019.dbc')
simp19 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DORS2019.dbc')
sim19$DTOBITO <- sim19$DTOBITO %>% dmy()
sim19$DTNASC <- sim19$DTNASC %>% dmy()
sim19$IDADE <- sim19$IDADE %>% as.character()
sim19 <- sim19 %>% mutate(sim19, IDADE = case_when(
  substr(IDADE, 1, 1) == '4'~ substr(IDADE, 2, 3),
  substr(IDADE, 1, 1) == '5' ~ paste('1', substr(IDADE, 2, 3), sep = '')))
sim19$IDADE <- sim19$IDADE %>% as.numeric()
sim19$SEXO <- sim19$SEXO %>% as.character()
sim19$SEXO[sim19$SEXO == '1'] <- 'M'
sim19$SEXO[sim19$SEXO == '2'] <- 'F'

sim21 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DORS2021.dbc')
simp21 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DORS2021.dbc')
sim21$DTOBITO <- sim21$DTOBITO %>% dmy()
sim21$DTNASC <- sim21$DTNASC %>% dmy()
sim21$IDADE <- sim21$IDADE %>% as.character()
sim21 <- sim21 %>% mutate(sim21, IDADE = case_when(
  substr(IDADE, 1, 1) == '4'~ substr(IDADE, 2, 3),
  substr(IDADE, 1, 1) == '5' ~ paste('1', substr(IDADE, 2, 3), sep = '')))
sim21$IDADE <- sim21$IDADE %>% as.numeric()
sim21$SEXO <- sim21$SEXO %>% as.character()
sim21$SEXO[sim21$SEXO == '1'] <- 'M'
sim21$SEXO[sim21$SEXO == '2'] <- 'F'


ibge <- read_xls('D:/1Folder/Estatistica/Demografia/Trab1/projecoes.xls', sheet = 'RS')
names(ibge)[names(ibge)== 'RIO GRANDE DO SUL']<- "grupo"
ibge$grupo <- ibge$grupo %>% factor(levels = c('Total'
                                               ,'0-4'                                                                                                                                     
                                               ,'5-9'                                                                                                                                    
                                               ,'10-14'                                                                                                                                   
                                               ,'15-19'                                                                                                                                  
                                               ,'20-24'                                                                                                                                 
                                               ,'25-29'                                                                                                                                
                                               ,'30-34'                                                                                                                               
                                               ,'35-39'                                                                                                                             
                                               ,'40-44'                                                                                                                             
                                               ,'45-49'                                                                                                                            
                                               ,'50-54'                                                                                                                           
                                               ,'55-59'                                                                                                                          
                                               ,'60-64'                                                                                                                         
                                               ,'65-69'                                                                                                                        
                                               ,'70-74'                                                                                                                       
                                               ,'75-79'                                                                                                                      
                                               ,'80-84'                                                                                                                     
                                               ,'85-89'                                                                                                                    
                                               ,'90+'))

pf <- ibge[4 : 24,]
pf10 <- pf[-(1:2), 1:2]
pf19 <- pf[-(1:2), c(1,11)]
pf21 <- pf[-(1:2), c(1,13)]

ph <- ibge[27: 47,]
ph10 <- ph[-(1:2), 1:2]
ph19 <- ph[-(1:2), c(1,11)]
ph21 <- ph[-(1:2), c(1,13)]

pt <- ibge[50: 70,]

###################################################################################################################################################################

### A)

#### Taxa Bruta de Mortalidade 2010

(nrow(sim10) /pt[2, 2])*1000
# 7.144889


#### Taxa Bruta de Mortalidade 2019

(nrow(sim19) /pt[2, 11])*1000
# 7.843555


#### Taxa Bruta de Mortalidade 2021

(nrow(sim21) /pt[2, 13])*1000
# 10.26649


#### Taxa Específica de Mortalidade Feminina 2010, 2019, 2021

#### 2010

sim10f <- sim10 %>%
  drop_na(IDADE)%>%
  filter(SEXO == 'F')%>%
  mutate(grupo = IDADE %>% cut(breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,200)))%>%
  select(IDADE, SEXO, grupo)%>%
  count(grupo, SEXO)

sim10f$grupo <- pf10$grupo

sim10f <- left_join(sim10f, pf10, by ="grupo")

sim10f%>%
  ggplot(., aes(x = grupo, y = n/(sim10f$...2)*1000))+
  geom_col(size = 1, fill = 'red', color = 'black')+
  labs(x = "Idade", y = "nMx")+
  theme_update(axis.text.x=element_text(size=7))+
  ggtitle("Taxa Específica de Mortalidade Feminina 2010")
ggsave("D:/1Folder/Estatistica/Demografia/Trab1/temfem2010.pdf", width = 158, height = 93, units = "mm")

((sim10f$n %>% sum())/pf[2,2])*1000
# 6.523312



#### 2019

sim19f <- sim19 %>%
  drop_na(IDADE)%>%
  filter(SEXO == 'M')%>%
  mutate(grupo = IDADE %>% cut(breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,200)))%>%
  select(IDADE, SEXO, grupo)%>%
  count(grupo, SEXO)


sim19f$grupo <- pf19$grupo

sim19f <- left_join(sim19f, pf19, by ="grupo")

sim19f%>%
  ggplot(., aes(x = grupo, y = n/(sim19f$...11)*1000))+
  geom_col(size = 1, fill = 'red', color = 'black')+
  labs(x = "Idade", y = "nMx")+
  theme_update(axis.text.x=element_text(size=7))+
  ggtitle("Taxa Específica de Mortalidade Feminina 2019")
ggsave("D:/1Folder/Estatistica/Demografia/Trab1/temfem2019.pdf", width = 158, height = 93, units = "mm")

((sim19f$n %>% sum())/pf[2,11])*1000
# 8.382553



#### 2021

sim21f <- sim21 %>%
  drop_na(IDADE)%>%
  filter(SEXO == 'M')%>%
  mutate(grupo = IDADE %>% cut(breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,200)))%>%
  select(IDADE, SEXO, grupo)%>%
  count(grupo, SEXO)


sim21f$grupo <- pf21$grupo

sim21f <- left_join(sim21f, pf21, by ="grupo")

sim21f%>%
  ggplot(., aes(x = grupo, y = n/(sim21f$...13)*1000))+
  geom_col(size = 1, fill = 'red', color = 'black')+
  labs(x = "Idade", y = "nMx")+
  theme_update(axis.text.x=element_text(size=7))+
  ggtitle("Taxa Específica de Mortalidade Feminina 2021")
ggsave("D:/1Folder/Estatistica/Demografia/Trab1/temfem2021.pdf", width = 158, height = 93, units = "mm")

((sim21f$n %>% sum())/pf[2,13])*1000
# 11.15449



#### Taxa Específica de Mortalidade Masculina 2010, 2019, 2021

#### 2010

sim10h <- sim10 %>%
  drop_na(IDADE)%>%
  filter(SEXO == 'M')%>%
  mutate(grupo = IDADE %>% cut(breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,200)))%>%
  select(IDADE, SEXO, grupo)%>%
  count(grupo, SEXO)

sim10h$grupo <- ph10$grupo

sim10h <- left_join(sim10h, ph10, by ="grupo")

sim10h%>%
  ggplot(., aes(x = grupo, y = n/(sim10h$...2)*1000))+
  geom_col(size = 1, fill = 'blue', color = 'black')+
  labs(x = "Idade", y = "nMx")+
  scale_y_continuous(breaks = seq(0, 90, by = 10))+
  theme_update(axis.text.x=element_text(size=7))+
  ggtitle("Taxa Específica de Mortalidade Masculina 2010")
ggsave("D:/1Folder/Estatistica/Demografia/Trab1/temmas2010.pdf", width = 158, height = 93, units = "mm")

((sim10h$n %>% sum())/ph[2,2])*1000
# 7.455803



#### 2019

sim19h <- sim19 %>%
  drop_na(IDADE)%>%
  filter(SEXO == 'M')%>%
  mutate(grupo = IDADE %>% cut(breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,200)))%>%
  select(IDADE, SEXO, grupo)%>%
  count(grupo, SEXO)

sim19h$grupo <- ph19$grupo

sim19h <- left_join(sim19h, ph19, by ="grupo")

sim19h%>%
  ggplot(., aes(x = grupo, y = n/(sim19h$...11)*1000))+
  geom_col(size = 1, fill = 'blue', color = 'black')+
  labs(x = "Idade", y = "nMx")+
  scale_y_continuous(breaks = seq(0, 90, by = 10))+
  theme_update(axis.text.x=element_text(size=7))+
  ggtitle("Taxa Específica de Mortalidade Masculina 2019")
ggsave("D:/1Folder/Estatistica/Demografia/Trab1/temmas2019.pdf", width = 158, height = 93, units = "mm")

((sim19h$n %>% sum())/ph[2,11])*1000
# 7.946579




#### 2021

sim21h <- sim21 %>%
  drop_na(IDADE)%>%
  filter(SEXO == 'M')%>%
  mutate(grupo = IDADE %>% cut(breaks = c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,200)))%>%
  select(IDADE, SEXO, grupo)%>%
  count(grupo, SEXO)

sim21h$grupo <- ph21$grupo

sim21h <- left_join(sim21h, ph21, by ="grupo")

sim21h%>%
  ggplot(., aes(x = grupo, y = n/(sim21h$...13)*1000))+
  geom_col(size = 1, fill = 'blue', color = 'black')+
  labs(x = "Idade", y = "nMx")+
  scale_y_continuous(breaks = seq(0, 90, by = 10))+
  theme_update(axis.text.x=element_text(size=7))+
  ggtitle("Taxa Específica de Mortalidade Masculina 2021")
ggsave("D:/1Folder/Estatistica/Demografia/Trab1/temmas2021.pdf", width = 158, height = 93, units = "mm")

((sim21h$n %>% sum())/ph[2,13])*1000
# 10.57047



################################################################################################################################

### B)

obitos_fetais <- data.frame(Idade = c(2010, 2019, 2021),
                            n = c(1211, 1082, 1062))



#### Taxa de Mortalidade Infantil 2020 (M = 1, F = 2)

nasc2010 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DNRS2010.dbc')
nasc2019 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DNRS2019.dbc')
nasc2020 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DNRS2020.dbc')
nasc2021 <- read.dbc('D:/1Folder/Estatistica/Demografia/Trab1/DNRS2021.dbc')

nasc2020m <- nasc2020%>%
  filter(SEXO == '1')

nasc2020f <- nasc2020%>%
  filter(SEXO == '2')



##### Taxa de Mortalidade Infantil Masculina

nmi19m <- simp19 %>%
  filter(substr(IDADE, 1, 1) %in% c('3','2','1','0'))%>%
  filter(SEXO == '1')%>%
  nrow()

nmi21m <- simp21 %>%
  filter(substr(IDADE, 1, 1) %in% c('3','2','1','0'))%>%
  filter(SEXO == '1')%>%
  nrow()
  

(mean(nmi21m + nmi19m)/nrow(nasc2020m))*1000
# 21.87608



##### Taxa de Mortalidade Infantil Feminina

nmi19f <- simp19 %>%
  filter(substr(IDADE, 1, 1) %in% c('3','2','1','0'))%>%
  filter(SEXO == '2')%>%
  nrow()

nmi21f <- simp21 %>%
  filter(substr(IDADE, 1, 1) %in% c('3','2','1','0'))%>%
  filter(SEXO == '2')%>%
  nrow()

(mean(nmi21f + nmi19f)/nrow(nasc2020f))*1000
# 18.02958



#### Taxa de Mortalidade Infantil Neonatal

##### 2010

nmn10 <-simp10 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','1', '0'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('228','227'))

(nrow(nmn10)/nrow(nasc2010))*1000
# 7.475064



##### 2019

nmn19 <-simp19 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','1', '0'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('228','227'))

(nrow(nmn19)/nrow(nasc2019))*1000
# 7.645101



##### 2021

nmn21 <-simp21 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','1', '0'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('228','227'))

(nrow(nmn21)/nrow(nasc2021))*1000
# 7.135625
 


#### Taxa de Mortalidade Infantil Posneonatal

##### 2010

nmpn10 <-simp10 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','3'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('200', '201', '202', '203', '204', '205', '206', '207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmpn10)/nrow(nasc2010))*1000
# 3.684997


##### 2019

nmpn19 <-simp19 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','3'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('200', '201', '202', '203', '204', '205', '206', '207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmpn19)/nrow(nasc2019))*1000
# 2.956997


##### 2021

nmpn21 <-simp21 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','3'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('200', '201', '202', '203', '204', '205', '206', '207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmpn21)/nrow(nasc2021))*1000
# 2.41872



#### Taxa de Mortalidade Infantil Neonatal Precoce

##### 2010

nmp10 <-simp10 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','1', '0'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmp10)/nrow(nasc2010))*1000
# 5.456197



##### 2019

nmp19 <-simp19 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','1', '0'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmp19)/nrow(nasc2019))*1000
# 5.416209



##### 2021

nmp21 <-simp21 %>%
  filter(substr(IDADE, 1, 1) %in% c('2','1', '0'))%>%
  filter(!substr(IDADE, 1, 3) %in% c('207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmp21)/nrow(nasc2021))*1000
# 5.054401



#### Taxa de Mortalidade Infantil Neonatal Tardia

##### 2010


nmt10 <-simp10 %>%
  filter(substr(IDADE, 1, 1) %in% c('2'))%>%
  filter(substr(IDADE, 1, 3) %in% c('207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmt10)/nrow(nasc2010))*1000
# 2.123939


##### 2019

nmt19 <-simp19 %>%
  filter(substr(IDADE, 1, 1) %in% c('2'))%>%
  filter(substr(IDADE, 1, 3) %in% c('207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmt19)/nrow(nasc2019))*1000
# 2.295759


##### 2021

nmt21 <-simp21 %>%
  filter(substr(IDADE, 1, 1) %in% c('2'))%>%
  filter(substr(IDADE, 1, 3) %in% c('207', '208', '209', '210', '211', '212', '213', '214', '215', '216', '217', '218', '219', '220', '221', '222', '223', '224', '225', '226', '227', '228'))

(nrow(nmt21)/nrow(nasc2021))*1000
# 2.169616





#### Taxa de Mortalidade Infantil Perinatal

##### 2010

nmp10

((nrow(nmp10)+obitos_fetais[1,2])/nrow(nasc2010))*1000
# 14.54485


##### 2019

nmp19

((nrow(nmp19)+obitos_fetais[2,2])/nrow(nasc2019))*1000
# 13.45508



##### 2021

nmp21

((nrow(nmp21)+obitos_fetais[3,2])/nrow(nasc2021))*1000
# 13.58822








########################################################################################################################################################




