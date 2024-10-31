##########################################################################
#### Asociación entre intención de voto y dimensiones de personalidad ####
##########################################################################

#### Cargar base de datos

library(readxl)
BD <- read_excel("BDPersonalidadCandidatos.xlsx")

##########################
### Pruebas Bivariadas ###
##########################

###############
# Extranjeros #
###############

BD$TargetExt <- as.factor(BD$TargetExt)
BD$PosturaCandidat <- as.factor(BD$PosturaCandidat)
BD$GeneroCandidato <- as.factor(BD$GeneroCandidato)
BD$AtractivoCandidato <- as.factor(BD$AtractivoCandidato)

##########
# T-test #
##########

#If the variance after grouping falls down significantly, it means 
#that the categorical variable can explain most of the variance of the 
#continuous variable and so the two variables likely have a strong association. 
#If the variables have no correlation, then the variance in the groups is 
#expected to be similar to the original variance.

#######
# Ope #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Opredext[TargetExt == "AumentoMantuvo"]))# p = 2.2e-16
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Opredext[TargetExt == "Bajo"])) # p = 1.001e-13

#Test for difference of variance
res.ftest1 <- var.test(Opredext ~ TargetExt, data = BD)
res.ftest1 #0.007779: significant difference between the variances of the two sets of data

#T-test 
res1 <- t.test(Opredext ~ TargetExt, data = BD, var.equal = FALSE)
res1 #5.39e-08: Dependientes o asociadas

#######
# Con #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Cpredext[TargetExt == "AumentoMantuvo"]))# p = 1.722e-11
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Cpredext[TargetExt == "Bajo"])) # p = 1.537e-11

#Test for difference of variance
res.ftest2 <- var.test(Cpredext ~ TargetExt, data = BD)
res.ftest2 #0.5651: no significant difference between the variances of the two sets of data

#T-test 
res2 <- t.test(Cpredext ~ TargetExt, data = BD, var.equal = TRUE)
res2 #2.2e-16: Dependientes o asociadas

#######
# Ext #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Epredext[TargetExt == "AumentoMantuvo"]))# p = 2.2e-16
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Epredext[TargetExt == "Bajo"])) # p = 2.2e-16

#Test for difference of variance
res.ftest3 <- var.test(Epredext ~ TargetExt, data = BD)
res.ftest3 #0.01584: no significant difference between the variances of the two sets of data

#T-test 
res3 <- t.test(Epredext ~ TargetExt, data = BD, var.equal = FALSE)
res3 #0.005624: Dependientes o asociadas

#######
# Agr #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Apredext[TargetExt == "AumentoMantuvo"]))# p = 9.598e-15
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Apredext[TargetExt == "Bajo"])) # p = 1.931e-10

#Test for difference of variance
res.ftest4 <- var.test(Apredext ~ TargetExt, data = BD)
res.ftest4 #0.5831: no significant difference between the variances of the two sets of data

#T-test 
res4 <- t.test(Apredext ~ TargetExt, data = BD, var.equal = TRUE)
res4 #2.655e-06: Dependientes o asociadas

#######
# Neu #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Npredext[TargetExt == "AumentoMantuvo"]))# p = 5.246e-16
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Npredext[TargetExt == "Bajo"])) # p = 4.057e-10

#Test for difference of variance
res.ftest5 <- var.test(Npredext ~ TargetExt, data = BD)
res.ftest5 #0.0001365: no significant difference between the variances of the two sets of data

#T-test 
res5 <- t.test(Npredext ~ TargetExt, data = BD, var.equal = FALSE)
res5 #6.119e-16: Dependientes o asociadas

###################
# Chi-square test #
###################

chisq.test(BD$TargetExt, BD$PosturaCandidat, correct=FALSE) #0.7857 (>0.1) Independientes
chisq.test(BD$TargetExt, BD$GeneroCandidato, correct=FALSE) #0.95 (>0.1) Independientes
chisq.test(BD$TargetExt, BD$AtractivoCandidato, correct=FALSE) #0.4991 (>0.1) Independientes

###############
#   Locales   #
###############

BD$TargetLocal <- as.factor(BD$TargetLocal)
BD$PosturaCandidat <- as.factor(BD$PosturaCandidat)
BD$GeneroCandidato <- as.factor(BD$GeneroCandidato)
BD$AtractivoCandidato <- as.factor(BD$AtractivoCandidato)

##########
# T-test #
##########

#If the variance after grouping falls down significantly, it means 
#that the categorical variable can explain most of the variance of the 
#continuous variable and so the two variables likely have a strong association. 
#If the variables have no correlation, then the variance in the groups is 
#expected to be similar to the original variance.

#######
# Ope #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Opredlocal[TargetLocal == "AumentoMantuvo"]))# p = 8.535e-13
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Opredlocal[TargetLocal == "Bajo"])) # p = 3.723e-11

#Test for difference of variance
res.ftest1 <- var.test(Opredlocal ~ TargetLocal, data = BD)
res.ftest1 #0.1081: no significant difference between the variances of the two sets of data

#T-test 
res1 <- t.test(Opredlocal ~ TargetLocal, data = BD, var.equal = FALSE)
res1 #1.685e-08: Dependientes o asociadas 

#######
# Con #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Cpredlocal[TargetLocal == "AumentoMantuvo"]))# p = 3.659e-16
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Cpredlocal[TargetLocal == "Bajo"])) # p = 6.733e-13

#Test for difference of variance
res.ftest2 <- var.test(Cpredlocal2 ~ TargetLocal, data = BD)
res.ftest2 #3.485e-16: significant difference between the variances of the two sets of data

#T-test 
res2 <- t.test(Cpredlocal2 ~ TargetLocal, data = BD, var.equal = FALSE)
res2 #0.001665 (<0.01)

#######
# Ext #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Epredlocal[TargetLocal == "AumentoMantuvo"]))# p = 8.136e-15
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Epredlocal[TargetLocal == "Bajo"])) # p = 9.854e-10

#Test for difference of variance
res.ftest3 <- var.test(Epredlocal ~ TargetLocal, data = BD)
res.ftest3 #1.948e-065: significant difference between the variances of the two sets of data

#T-test 
res3 <- t.test(Epredlocal ~ TargetLocal, data = BD, var.equal = FALSE)
res3 #2.2e-16: Dependientes o asociadas

#######
# Agr #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Apredlocal2[TargetLocal == "AumentoMantuvo"]))# p = 4.884e-14
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Apredlocal2[TargetLocal == "Bajo"])) # p = 4.039e-10

#Test for difference of variance
res.ftest4 <- var.test(Apredlocal ~ TargetLocal, data = BD)
res.ftest4 #0.03282: significant difference between the variances of the two sets of data

#T-test 
res4 <- t.test(Apredlocal ~ TargetLocal, data = BD, var.equal = FALSE)
res4 #2.2e-16: Independientes

#######
# Neu #
#######

##Asumption 1
#Yes, since the samples from altomantuvo and bajo are not related.

##Asumption 2
# Shapiro-Wilk normality test for AltoMantuve
with(BD, shapiro.test(Npredlocal[TargetLocal == "AumentoMantuvo"]))# p = 9.566e-13
# Shapiro-Wilk normality test for Bajo
with(BD, shapiro.test(Npredlocal[TargetLocal == "Bajo"])) # p = 1.027e-11

#Test for difference of variance
res.ftest5 <- var.test(Npredlocal ~ TargetLocal, data = BD)
res.ftest5 #0.9354: no significant difference between the variances of the two sets of data

#T-test 
res5 <- t.test(Npredlocal ~ TargetLocal, data = BD, var.equal = TRUE)
res5 #2.621e-08: Dependientes o asociadas

###################
# Chi-square test #
###################

chisq.test(BD$TargetLocal, BD$PosturaCandidat, correct=FALSE) #0.2791 (>0.1) Independientes
chisq.test(BD$TargetLocal, BD$GeneroCandidato, correct=FALSE) #0.5852 (>0.1) Independientes
chisq.test(BD$TargetLocal, BD$AtractivoCandidato, correct=FALSE) #0.3953 (>0.1) Independientes

##############################
### Regresiones Logísticas ###
##############################

####################################
### Sin personalidad de votantes ###
####################################

###############
# Extranjeros # #Sin personalidad de votantes
###############

logitExt1 <- glm(TargetExt ~ Opredext + Cpredext + Epredext + Apredext + Npredext, data = BD, family = "binomial")
summary(logitExt1)

logitExt2 <- glm(TargetExt ~ Opredext + Cpredext + Epredext + Apredext + Npredext
                 + PosturaCandidat + GeneroCandidato + AtractivoCandidato, data = BD, family = "binomial")
summary(logitExt2)

###########
# Locales # #Sin personalidad de votantes
###########

logitLocal1 <- glm(TargetLocal ~ Opredlocal + Cpredlocal2 + Epredlocal + Apredlocal + Npredlocal, data = BD, family = "binomial")
summary(logitLocal1)

logitLocal2 <- glm(TargetLocal ~ Opredlocal + Cpredlocal + Epredlocal + Apredlocal + Npredlocal
                 + PosturaCandidat + GeneroCandidato + AtractivoCandidato, data = BD, family = "binomial")
summary(logitLocal2)

####################################
### Con personalidad de votantes ###
####################################

#### Cargar base de datos 2

library(readxl)
BDConPersonalidadVotantesNorm <- read_excel("TesisPersonalidad2/TesisPersonalidad/BDConPersonalidadVotantesNorm.xlsx")
#View(BDConPersonalidadVotantesNorm)

### Cálculo de personalida de votantes según el BFI

varScore <- function(Data, Forward, Reverse=NULL, Range = NULL, Prorate = TRUE, MaxMiss = .20)
{
  #select relevant items
  d = Data[,c(Forward, Reverse)]
  
  #check for out of range
  if (!is.null(Range)){
    if (min(d, na.rm=TRUE) < Range[1] || max(d, na.rm=TRUE) > Range[2]){
      stop('Item score(s) out of range')
    }
  }
  
  #check that length of Range == 2 if Reverse is not null
  if (!is.null(Reverse) && length(Range) !=2) {
    stop('Must specify item range (Range) to reverse score items')
  }
  
  #Reverse score relevant items
  if (!is.null(Reverse)){
    for (v in Reverse) {
      d[,v] = (Range[1] + Range[2]) - d[,v]
    }   
  }
  
  if (Prorate){
    Total = rowMeans(d, na.rm=TRUE)*dim(d)[2]
  }
  else{
    Total = rowSums(d, na.rm=TRUE)
  }
  
  #count missing and set > MaxMiss to NA
  MissCount = rowSums(is.na(d))
  MissCount = MissCount/dim(d)[2]
  Total[MissCount > MaxMiss] = NA
  
  return(Total)
}

BF_EXT = varScore(BD, Forward= c('BF1', 'BF11', 'BF26', 'BF36'), Reverse= c('BF6', 'BF21', 'BF31'), Range = c(1,5) )
BF_AGR = varScore(BD, Forward= c('BF7', 'BF17', 'BF22', 'BF32', 'BF42'), Reverse= c('BF2', 'BF12', 'BF27', 'BF37'), Range = c(1,5) )
BF_CON = varScore(BD, Forward= c('BF3', 'BF13', 'BF28', 'BF33', 'BF38'), Reverse= c('BF8', 'BF18', 'BF23', 'BF43'), Range = c(1,5) )
BF_NEU = varScore(BD, Forward= c('BF4', 'BF14', 'BF19', 'BF29', 'BF39'), Reverse= c('BF9', 'BF24', 'BF34'), Range = c(1,5) )
BF_OPEN = varScore(BD, Forward= c('BF5', 'BF10', 'BF15', 'BF20', 'BF25', 'BF30', 'BF40', 'BF44'), Reverse= c('BF35', 'BF41'), Range = c(1,5) )

BD$BF_EXT = BF_EXT
BD$BF_AGR = BF_AGR
BD$BF_CON = BF_CON
BD$BF_NEU = BF_NEU
BD$BF_OPEN = BF_OPEN

library(openxlsx)
write.xlsx(BD, 'BDConPersonalidadVotantes.xlsx')

### Estandarización de 0 a 1 de las dimensiones de personalidad

library(readxl)
BD2 <- read_excel("BDConPersonalidadVotantes.xlsx")

library(datawizard)
ONorm <- normalize(BD2$BF_OPEN, include_bounds = FALSE)
CNorm <- normalize(BD2$BF_CON, include_bounds = FALSE)
ENorm <- normalize(BD2$BF_EXT, include_bounds = FALSE)
ANorm <- normalize(BD2$BF_AGR, include_bounds = FALSE)
NNorm <- normalize(BD2$BF_NEU, include_bounds = FALSE)

BD2$ONorm = ONorm
BD2$CNorm = CNorm
BD2$ENorm = ENorm
BD2$ANorm = ANorm
BD2$NNorm = NNorm

library(openxlsx)
write.xlsx(BD2, 'BDConPersonalidadVotantesNorm.xlsx')

BDConPersonalidadVotantesNorm$TargetExt <- as.factor(BDConPersonalidadVotantesNorm$TargetExt)
BDConPersonalidadVotantesNorm$TargetLocal <- as.factor(BDConPersonalidadVotantesNorm$TargetLocal)
BDConPersonalidadVotantesNorm$PosturaCandidat <- as.factor(BDConPersonalidadVotantesNorm$PosturaCandidat)
BDConPersonalidadVotantesNorm$GeneroCandidato <- as.factor(BDConPersonalidadVotantesNorm$GeneroCandidato)
BDConPersonalidadVotantesNorm$AtractivoCandidato <- as.factor(BDConPersonalidadVotantesNorm$AtractivoCandidato)

###############
# Extranjeros # #Con personalidad de votantes
###############

logitExtA <- glm(TargetExt ~ ONorm + CNorm + ENorm + ANorm + NNorm, data = BDConPersonalidadVotantesNorm, family = "binomial")
summary(logitExtA)

logitExtB <- glm(TargetExt ~ ONorm + CNorm + ENorm + ANorm + NNorm +
                   Opredext + Cpredext + Epredext + Apredext + Npredext, data = BD, family = "binomial")
summary(logitExtB)

logitExtC <- glm(TargetExt ~ ONorm + CNorm + ENorm + ANorm + NNorm +
                   Opredext + Cpredext + Epredext + Apredext + Npredext
                 + PosturaCandidat + GeneroCandidato + AtractivoCandidato, data = BD2, family = "binomial")
summary(logitExtC)

###########
# Locales # #Con personalidad de votantes
###########

logitLocalA <- glm(TargetLocal ~ ONorm + CNorm + ENorm + ANorm + NNorm, data = BD, family = "binomial")
summary(logitLocalA)

logitLocalB <- glm(TargetLocal ~ ONorm + CNorm + ENorm + ANorm + NNorm +
                   Opredlocal + Cpredlocal2 + Epredlocal + Apredlocal + Npredlocal, data = BD, family = "binomial")
summary(logitLocalB)

logitLocalC <- glm(TargetLocal ~ ONorm + CNorm + ENorm + ANorm + NNorm +
                Opredlocal + Cpredlocal + Epredlocal + Apredlocal + Npredlocal
                 + PosturaCandidat + GeneroCandidato + AtractivoCandidato, data = BD2, family = "binomial")
summary(logitLocalC)


