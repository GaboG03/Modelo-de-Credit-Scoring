library(tidyverse)
library(readr)
library(openxlsx)
library(funModeling)
library(ggplot2)
library(fastDummies)
library(ROSE)
library(Zelig)
library(caret)

#baseT <- as.data.frame(read.xlsx("C:\\Users\\gabo_\\Documents\\8vo Semestre\\Riesgo 2021-A\\base_finalT.xlsx"))

#baseprod1<- as.data.frame(read.table("C:\\Users\\gabo_\\Documents\\8vo Semestre\\Riesgo 2021-A\\BaseProd_M.csv",header=TRUE, sep=",", na.strings="NA", dec=",", strip.white=TRUE))


#baseprod1 <- rename(baseprod1,CODIGO_ID = ï..CODIGO_ID, ICC_Indice_confianza_consumidor = ICC_Indice.confianza.consumidor, PETROLEO_WTI = PETRÃ.LEO.WTI,  CRUDO_ORIENTE = CRUDO.ORIENTE)

#baseprod1<- baseprod1  %>% select(CODIGO_ID,ICC_Indice_confianza_consumidor,IDEAC,PETROLEO_WTI,CRUDO_ORIENTE)


baseprod<- as.data.frame(read.xlsx("C:\\Users\\GABRIEL\\OneDrive\\Documents\\Octavo Semestre\\Riesgo 2021-A\\base_finalTM.xlsx"))


baseprod2<- baseprod

######## ELIMINAMOS LAS VARIABLES NO RELEVANTES

baseprod2 <- select(baseprod2,-CODIGO_ID,-Fecha,-MARCA_CUENTA_AHORROS)

baseT2<- baseprod2

baseT2<- baseT2 %>% select(-CRUDO_ORIENTE)


### SERIES ECONOMICAS

ICCdif<-diff(baseT2$ICC_Indice_confianza_consumidor)
ICCdif[22965]<- ICCdif[22964]

baseT2$ICC_Indice_confianza_consumidor<- ICCdif


### IDEAC: 
IDEACdif<- diff(baseT2$IDEAC)
IDEACdif[22965] <- IDEACdif[22964]
baseT2$IDEAC<- IDEACdif


CRUDOdif<- diff(diff(baseT2$CRUDO_ORIENTE))
CRUDOdif[22965]<- CRUDOdif[22964]
baseT2$CRUDO_ORIENTE<- CRUDOdif


######################




categoricas <- c('ORIGEN_APROBACION','FORMA_PAGO', 'SALDO_TOTAL_TARJETA', 'CUPO_PROMEDIO_TARJETA', 'SALDO_UTILIZ_PROM_CLIENTE' ,'CANTIDAD_TOTAL_AVANCES','PROMEDIO_MENSUAL_CONSUMOS_LOCALES' , 'MAXIMO_NUM_DIAS_VENCIDO', 'NUMERO_OPERACIONES_TITULAR', 'PROMEDIO_DIAS_SOBREGIRO_CC', 'PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO' ,'MARCA_CUENTA_CORRIENTE', 'RIESGO_CLIENTE_TOTAL_GFP' , 'VALOR_DEPOSITO_A_PLAZO', 'SEGMENTO_RIESGO', 'SUCURSAL', 'GENERO','INSTRUCCION' )

banco_training <- baseT2 %>% dummy_cols(select_columns = categoricas)  %>% select(-categoricas)

set.seed(123456)

entrenamiento <- createDataPartition(banco_training$MarcaMora_Tarjeta, p = 0.8, list=FALSE)

banco_training1 <- banco_training[entrenamiento,]
banco_val1 <- banco_training[-entrenamiento,]


banco_training1$MarcaMora_Tarjeta<- as.factor(banco_training1$MarcaMora_Tarjeta)

datosBalanceados<- ovun.sample(MarcaMora_Tarjeta~., data= banco_training1, method="both", p=0.2,N=18372,seed=1)

datosF<- datosBalanceados$data

banco_training1<- datosF

banco_training1$MarcaMora_Tarjeta<- if_else(banco_training1$MarcaMora_Tarjeta=="0",0,1)


logit_banco <-glm(MarcaMora_Tarjeta~., data = banco_training1, family='binomial')

summary(logit_banco)

step(logit_banco, direction = "backward")



#logit_banco1<- glm(formula = MarcaMora_Tarjeta ~ ANTIGUEDAD_TARJETA_ANIOS + 
#                     NUM_TC_SIST_FIM + ICC_Indice_confianza_consumidor + PETROLEO_WTI + 
#                     ORIGEN_APROBACION_Demanda + FORMA_PAGO_No + SALDO_TOTAL_TARJETA_A + 
#                     CUPO_PROMEDIO_TARJETA_A + CUPO_PROMEDIO_TARJETA_B + 
#                     CUPO_PROMEDIO_TARJETA_C + SALDO_UTILIZ_PROM_CLIENTE_A + 
#                     CANTIDAD_TOTAL_AVANCES_0 + PROMEDIO_MENSUAL_CONSUMOS_LOCALES_A + 
#                     MAXIMO_NUM_DIAS_VENCIDO_A + MAXIMO_NUM_DIAS_VENCIDO_B + NUMERO_OPERACIONES_TITULAR_A + 
#                     NUMERO_OPERACIONES_TITULAR_B + PROMEDIO_DIAS_SOBREGIRO_CC_NO + 
#                     PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_A + PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_B + 
#                     MARCA_CUENTA_CORRIENTE_No + RIESGO_CLIENTE_TOTAL_GFP_A + 
#                     VALOR_DEPOSITO_A_PLAZO_NO + SEGMENTO_RIESGO_A + SUCURSAL_COSTA 
#                     , family = "binomial", data = banco_training1)

logit_banco1<- glm(formula = MarcaMora_Tarjeta ~ ANTIGUEDAD_TARJETA_ANIOS + 
                     IDEAC + PETROLEO_WTI +
                      SALDO_TOTAL_TARJETA_A + 
                     CUPO_PROMEDIO_TARJETA_C + 
                     SALDO_UTILIZ_PROM_CLIENTE_A + 
                     CANTIDAD_TOTAL_AVANCES_0 + PROMEDIO_MENSUAL_CONSUMOS_LOCALES_A + 
                     MAXIMO_NUM_DIAS_VENCIDO_A + MAXIMO_NUM_DIAS_VENCIDO_B + 
                     PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_A + PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_B + 
                     MARCA_CUENTA_CORRIENTE_No + RIESGO_CLIENTE_TOTAL_GFP_A + 
          SEGMENTO_RIESGO_A + SUCURSAL_COSTA + ICC_Indice_confianza_consumidor
                    , family = "binomial", data = banco_training1)

summary(logit_banco1)


devLoG <-sum(residuals(logit_banco1 ,type="deviance")^2)

pvalue_devLog <-1-pchisq(devLoG,logit_banco1$df.null-logit_banco1$df.residual)
pvalue_devLog


a<- anova(logit_banco1,test="Chisq")

# Pearson: 

pearLog <-sum(residuals(logit_banco1 ,type="pearson")^2)
pvalue_pearLog <-1-pchisq(pearLog,18351)
pvalue_pearLog


LR <- logit_banco1$null.deviance - logit_banco1$deviance

N <-sum(weights(logit_banco1)) 

RsqrCN <- 1 -exp(-LR/N)


L0.adj <-exp(-logit_banco1$null.deviance/N)

RsqrNal <- RsqrCN/(1 - L0.adj)


# Holsmer
library(ResourceSelection)
hoslem.test(banco_training1$MarcaMora_Tarjeta,fitted.values(logit_banco1))


library(car)
library(carData)

viflog <- car::vif(logit_banco1)
viflog



#relogitmod1<- zelig(formula = MarcaMora_Tarjeta ~ ANTIGUEDAD_TARJETA_ANIOS + 
#                      NUM_TC_SIST_FIM + ICC_Indice_confianza_consumidor + PETROLEO_WTI + 
#                      ORIGEN_APROBACION_Demanda + FORMA_PAGO_No + SALDO_TOTAL_TARJETA_A + 
#                      CUPO_PROMEDIO_TARJETA_A + CUPO_PROMEDIO_TARJETA_B + 
#                      CUPO_PROMEDIO_TARJETA_C + SALDO_UTILIZ_PROM_CLIENTE_A + 
#                      CANTIDAD_TOTAL_AVANCES_0 + PROMEDIO_MENSUAL_CONSUMOS_LOCALES_A + 
#                      MAXIMO_NUM_DIAS_VENCIDO_A + MAXIMO_NUM_DIAS_VENCIDO_B + NUMERO_OPERACIONES_TITULAR_A + 
#                      NUMERO_OPERACIONES_TITULAR_B + PROMEDIO_DIAS_SOBREGIRO_CC_NO + 
#                      PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_A + PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_B + 
#                      MARCA_CUENTA_CORRIENTE_No + RIESGO_CLIENTE_TOTAL_GFP_A + 
#                      VALOR_DEPOSITO_A_PLAZO_NO + SEGMENTO_RIESGO_A + SUCURSAL_COSTA, data=banco_training1 ,model = "relogit", tau  = 865/22965)



relogitmod1<- zelig(formula = MarcaMora_Tarjeta ~ ANTIGUEDAD_TARJETA_ANIOS + 
                      IDEAC + PETROLEO_WTI +
                      SALDO_TOTAL_TARJETA_A + 
                      CUPO_PROMEDIO_TARJETA_C + 
                      MAXIMO_NUM_DIAS_VENCIDO_A + MAXIMO_NUM_DIAS_VENCIDO_B + 
                      PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_A + PROMEDIO_MENSUAL_SALDO_CUENTA_PASIVO_B + 
                      MARCA_CUENTA_CORRIENTE_No + RIESGO_CLIENTE_TOTAL_GFP_A + 
                      SEGMENTO_RIESGO_A + SUCURSAL_COSTA + ICC_Indice_confianza_consumidor, 
    family = "binomial", data = banco_training1,model = "relogit", tau  = 865/22965)


summary(relogitmod1)

odds <-exp(coefficients(relogitmod1))
odds





library(ResourceSelection)

observados<- relogitmod1$get_fitted()[[1]]

hoslem.test(banco_training1$MarcaMora_Tarjeta,observados[1,] )


library(plotROC)
library(ROCit) 
library(ggExtra)

banco_val2 <-banco_val1 %>% mutate(Predichos=predict(relogitmod1,banco_val1, type="response")[[1]])

g1<-ggplot(banco_val2,aes(d=MarcaMora_Tarjeta ,m=Predichos))+ geom_roc()


logroc <-ggplot(banco_val2,aes(d=MarcaMora_Tarjeta ,m=Predichos)) + theme_bw()+
  geom_roc(n.cuts=0, colour="black") +theme(axis.text=element_text(colour ="blue"),
                                            plot.title = element_text(hjust = 0.5))+scale_x_continuous("\n Especifidad",breaks=seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad \n",
                     breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1,
              colour="green", linetype="dashed") +
  annotate("text", x=0.6, y=0.45,  parse=TRUE ,
           label=paste0("AUC: ",round(calc_auc(g1)$AUC ,3)),
           colour="green")+ggExtra::removeGridX ()+
  ggExtra::removeGridY ()
logroc


####### TODA LA BASE

#### TODA LA BASE 

banco_training2 <-banco_training1 %>% mutate(Predichos=predict(relogitmod1,banco_training1, type="response")[[1]])

g11<-ggplot(banco_training2,aes(d=MarcaMora_Tarjeta ,m=Predichos))+ geom_roc()


logroc1 <-ggplot(banco_training2,aes(d=MarcaMora_Tarjeta ,m=Predichos)) + theme_bw()+
  geom_roc(n.cuts=0, colour="black") +theme(axis.text=element_text(colour ="blue"),
                                            plot.title = element_text(hjust = 0.5))+scale_x_continuous("\n1 - Especifidad",breaks=seq(0, 1, by = .2))+
  scale_y_continuous("Sensibilidad \n",
                     breaks = seq(0, 1, by = .2)) +
  geom_abline(intercept=0, slope=1,
              colour="green", linetype="dashed") +
  annotate("text", x=0.6, y=0.45,  parse=TRUE ,
           label=paste0("AUC: ",round(calc_auc(g11)$AUC ,3)),
           colour="green")+ggExtra::removeGridX ()+
  ggExtra::removeGridY ()
logroc1





dres <-data.frame(pred=predict(relogitmod1 ,
                               banco_training1, type="response")[[1]],
                  var=banco_training1$MarcaMora_Tarjeta)

ROC <-rocit(score=dres$pred ,class=dres$var)

ksplot <-ksplot(ROC)

cutoff <-ksplot$'KS Cutoff'


kstat <-as.numeric(ksplot$'KS stat')


# tabla: 

res <-predict(relogitmod1,banco_val1,type="response")[[1]]
res <-if_else(res > cutoff ,1,0)


mc<-table(res,banco_val1$MarcaMora_Tarjeta)
names(mc)<-c("Si","No")
mc


res1 <-predict(relogitmod1,banco_training1,type="response")[[1]]
res1 <-if_else(res1 > cutoff ,1,0)


mc1<-table(res1,banco_training1$MarcaMora_Tarjeta)
names(mc1)<-c("Si","No")
mc1



### SERIES ECONOMICAS ##########

Series <- as.data.frame(read.xlsx("C:\\Users\\gabo_\\Documents\\8vo Semestre\\Riesgo 2021-A\\BaseProd.xlsx",sheet="ICC"))

Series <- Series %>% select(-X6)

Series <- rename(Series, ICC_Indice_confianza_consumidor = ICC_Indice.confianza.consumidor, PETROLEO_WTI = PETRÓLEO.WTI,  CRUDO_ORIENTE = CRUDO.ORIENTE)

plot(Series$ICC_Indice_confianza_consumidor,type="o")

library(tseries)

adf.test(diff(Series$ICC_Indice_confianza_consumidor),alternative="stationary", k=0)

plot(diff(Series$ICC_Indice_confianza_consumidor),type="l",col="blue",xlab="Tiempo")


adf.test(diff(Series$IDEAC),alternative="stationary", k=0)

plot(diff(Series$IDEAC),type="l",col="blue",xlab="Tiempo")


adf.test(diff(diff(Series$CRUDO_ORIENTE)),alternative="stationary", k=0)

plot(diff(diff(Series$CRUDO_ORIENTE)),type="l",col="blue",xlab="Tiempo")


adf.test(diff(diff(Series$PETROLEO_WTI)),alternative="stationary", k=0)

plot(diff(diff(Series$CRUDO_ORIENTE)),type="l",col="blue",xlab="Tiempo")


############ Calculo de probabilidades: 


###### Grupos Homogéneos: 

indicadora <- as.data.frame(read.xlsx("C:\\Users\\GABRIEL\\OneDrive\\Documents\\Octavo Semestre\\Riesgo 2021-A\\BaseProd_1.xlsx"))

banco_training <-banco_training %>% mutate(PREDICHOS=predict(relogitmod1,banco_training, type="response")[[1]])
 


basefinal <- cbind(banco_training,indicadora$Etiquetas_Fechas)
names(basefinal)[40]="Fechas"


basefinal$Fechas<-indicadora$Etiquetas_Fechas

write.xlsx(basefinal,"basecicloPIT.xlsx")


#### GRUPOS: 

basefinal <- as.data.frame(read.xlsx("C:\\Users\\gabo_\\Documents\\8vo Semestre\\Riesgo 2021-A\\basecicloPIT.xlsx"))


grupos <- kmeans(x =basefinal[, c('PREDICHOS')], centers = 4, nstart = 1)

basefinal <- basefinal %>% mutate(GR = grupos$cluster)
basefinal<- basefinal %>% mutate(GR = as.factor(GR))

C1 <- basefinal %>% filter(GR == 1)
C2 <- basefinal %>% filter(GR == 2)
C3 <- basefinal %>% filter(GR == 3)
C4 <- basefinal %>% filter(GR == 4)
C5 <- basefinal %>% filter(GR == 5)
C6 <- basefinal %>% filter(GR == 6)


options(scipen = 999)


c('minC1' = min(C1$PREDICHOS), 'maxC1' = max(C1$PREDICHOS),
  'minC2' = min(C2$PREDICHOS), 'maxC2' = max(C2$PREDICHOS),
  'minC3' = min(C3$PREDICHOS), 'maxC3' = max(C3$PREDICHOS),
  'minC4' = min(C4$PREDICHOS), 'maxC4' = max(C4$PREDICHOS))
#### CURVA ROC 

#ORDENAMOS: 
G1<-C1 ; G2<- C4; G3<-C3 ; G4<- C2



G1$GR <- as.character(G1$GR)
G2$GR <- as.character(G2$GR)
G3$GR <- as.character(G3$GR)
G4$GR <- as.character(G4$GR)


####### G1

variables <- c(G1$PREDICHOS, G2$PREDICHOS)
categorias <- c(G1$GR, G2$GR)
bartlett.test(variables, categorias)


variables <- c(G1$PREDICHOS, G3$PREDICHOS)
categorias <- c(G1$GR, G3$GR)
bartlett.test(variables, categorias)


variables <- c(G1$PREDICHOS, G4$PREDICHOS)
categorias <- c(G1$GR, G4$GR)
bartlett.test(variables, categorias)


#####GRUPO 2 

variables <- c(G2$PREDICHOS, G3$PREDICHOS)
categorias <- c(G2$GR, G3$GR)
bartlett.test(variables, categorias)

variables <- c(G2$PREDICHOS, G4$PREDICHOS)
categorias <- c(G2$GR, G4$GR)
bartlett.test(variables, categorias)


############################### G3

variables <- c(G3$PREDICHOS, G4$PREDICHOS)
categorias <- c(G3$GR, G4$GR)
bartlett.test(variables, categorias)



install.packages("devtools")
library(devtools)

devtools::install_github("matherion/userfriendlyscience", dependencies=TRUE)

library(userfriendlyscience)

basefinal$GR <- as.factor(basefinal$GR)

write.xlsx(basefinal,"FINALPIT.xlsx")


GH <- oneway(basefinal$GR, y = basefinal$PREDICHOS,posthoc = 'games-howell')
GH


A<- if_else(basefinal$Fechas== 1 | basefinal$Fechas==2 | basefinal$Fechas==3, 1 , if_else( basefinal$Fechas== 4 | basefinal$Fechas==5 | basefinal$Fechas==6 , 2, if_else( basefinal$Fechas== 7 | basefinal$Fechas==8 | basefinal$Fechas==9 , 3, if_else(basefinal$Fechas== 10 | basefinal$Fechas==11 | basefinal$Fechas==12,4,5  ) ) ) )

basefinal<- basefinal %>% mutate(Fecha1 = A)



no <- basefinal %>% filter(MarcaMora_Tarjeta == 0) %>% group_by(GR, Fechas) %>% dplyr::summarise(tbuenos = n())

yes <- basefinal %>% filter(MarcaMora_Tarjeta == 1) %>% group_by(GR, Fechas) %>% dplyr::summarise(tmalos = n())

mes <- full_join(no, yes, by = c('GR', 'Fechas'))
mes$tmalos <- mes$tmalos/(mes$tmalos + mes$tbuenos)

PD <- mes %>% group_by(GR) %>% dplyr::summarise(PD = mean(tmalos, na.rm = TRUE))
PD


ggplot(data=mes) + geom_line(aes(x = Fechas, y = tmalos, color = GR), size=1) +
  labs(x = "Periodo", y = "PD",
       title = "Ciclo Económico") +
  theme_bw()+
  theme(legend.background = element_rect()) +
  scale_color_hue(name = 'GH')



mes <- full_join(no, yes, by = c('GR', 'Fechas'))
mes$total <- mes$tmalos + mes$tbuenos
bdd <- mes %>% group_by(Fechas) %>% mutate(porc = round(100*(total/sum(total,na.rm=T)),2))

ggplot(data=bdd,
       aes(x = factor(Fechas, level= c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")),
           y = porc, fill = GR)) +
  geom_bar(stat="identity") +
  theme_minimal() +
  xlab("Meses") + ylab("Porcentaje") +
  labs(title = "Porcentaje de clientes por grupos en cada mes",
       fill= "Grupos") +
  scale_fill_brewer(palette="Spectral") 



########################

basefinal<- basefinal %>% mutate(EAD=100)


set.seed(123456)

lgd_dist <- data.frame(dist = rbeta(nrow(basefinal), 0.5, 0.5))

ggplot(lgd_dist, aes(x=dist)) + 
  geom_histogram(binwidth = 0.025, fill="#1F84E9", color="#024385") +
  labs(title = "LGD") + theme_minimal() +
  theme(axis.title.x = element_blank())+
  ylab("Conteo")



LGD <- sort(lgd_dist$dist, decreasing = TRUE)
basefinal$LGD <- NA


g1 <- which(basefinal$GR == '2')
g2 <- which(basefinal$GR == '3')
g3 <- which(basefinal$GR == '4') 
g4 <- which(basefinal$GR == '1')



basefinal$LGD[g1] <- LGD[1:length(g1)]
basefinal$LGD[g2] <- LGD[(length(g1)+1):(length(g1)+length(g2))]
basefinal$LGD[g3] <- LGD[(length(g1)+length(g2)+1):(length(g1)+length(g2)+length(g3))]
basefinal$LGD[g4] <- LGD[(length(g1)+length(g2)+length(g3)+1):(length(g1)+length(g2)+length(g3)++length(g4))]


basefinal %>% group_by(GR) %>% dplyr::summarise("LGD por Grupo" = mean(LGD))

## PE DE CADA PERSONA 


banco <- basefinal %>% mutate(PE = PREDICHOS*EAD*LGD)



ggplot(banco, aes(x = PE)) + 
  geom_histogram(binwidth = 10, fill="#1F84E9", color="#024385") +
  labs(title = "Distribución de la Pérdida Esperada") + theme_minimal() +
  theme(axis.title.x = element_blank())+
  ylab("Conteo")


c("PE de la cartera" = mean(banco$PE))


c("VaR" = quantile(banco$PE, probs = 0.99, names = FALSE))


c("CE" = quantile(banco$PE, probs = 0.99, names = FALSE) - mean(banco$PE))


### CAPITAL ECONOMICO PARA CADA PERIODO DE TIEMPO: 

pit <- banco %>% select(Fecha1, PE) %>% group_by(Fecha1) %>% 
  dplyr::summarise("PE Cartera" = mean(PE), 
                   "VaR" = quantile(PE, probs = 0.99, names = FALSE), 
                   "CE" = quantile(PE, probs = 0.99, names = FALSE) - mean(PE))
pit

mean(pit$CE)


ggplot(banco, aes(x = PE)) +
  geom_histogram(binwidth = 10, fill="#1F84E9", color="#024385") +
  labs(title = "Distribución de la Pérdida Esperada") +
  facet_wrap(~Fechas) + theme_bw() + ylab("Conteo")


boxplot(formula =PREDICHOS~ GR, data =  basefinal, col = c("orange3", "yellow3", "green3","blue3"))
