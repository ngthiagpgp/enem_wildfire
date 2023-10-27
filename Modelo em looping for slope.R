####### R code for the analysis in:#########################################################################
# R code for the analysis in:
#
#  Gardin, T. N.; ;Requia, W. J. Air quality and individual-level academic performance in Brazil:
#   A nationwide study of more than 15 million students.
#   between 2000 and 2020. 
#  https://doi.org/XXXXXXX/XXXXX
#
# 
#######LOAD PACKAGES#########################################################################

library(tidyverse)
library(lme4)
library(parameters)
library(splines)

#######DATA PREP#########################################################################
#LOAD THE DATAFRAME

df<- readRDS("~/ENEM/base de referencia 2.rds")
df <-df %>% select(NU_NOTA_REDACAO,NU_NOTA_OBJETIVA,
                   no2_ppb,o3_ppb,pm25_ugm3,
                   NU_ANO,
                   IDHM,
                   SG_UF_RESIDENCIA,
                   Q15,
                   Q10,
                   TP_DEPENDENCIA_ADM_ESC,
                   TP_SEXO,TP_LOCALIZACAO_ESC,CO_MUNICIPIO_RESIDENCIA) %>% 
  drop_na(NU_NOTA_REDACAO,NU_NOTA_OBJETIVA,no2_ppb,o3_ppb,pm25_ugm3,SG_UF_RESIDENCIA)#selecionar variaveis de interesse 

df[,1:7] <- lapply(df[,1:7], as.numeric)
df[,8:14] <- lapply(df[,8:14], as.factor)

# Rename Variables and observations

df<-df %>% mutate(periodo=if_else(as.numeric(NU_ANO)>2009,"pré-2008","pós-2008"))
names(df)<- c("score_Essay", "Score_General_Subjects",
              "airpol_no2","airpol_o3", "airpol_pm25",
              "year" ,               
              "Munc_HDI",                  
              "UF_Home"  ,    
              "income"    ,               
              "Educ_mother",                   
              "Manegement",
              "gender"     ,          
              "Location"    ,
              "Codigo_municipio",
              "period"    )
# 
df$School_management[df$Manegement == "1"] <- "School management: Public"
df$School_management[df$Manegement=="2"]<- "School management: Public"
df$School_management[df$Manegement=="3"]<- "School management: Public"
df$School_management[df$Manegement=="4"]<- "School management: Private"

df$Manegement<-as.factor(df$School_management)

df$period<-as.factor(df$period)

df$Location1[df$Location == "1"] <- "Urban"
df$Location1[df$Location=="2"]<- "Rural"
df$Location<-as.factor(df$Location1)

df$gender1[df$gender =="M"] <- "Male"
df$gender1[df$gender=="F"]<- "Female"
df$gender<-as.factor(df$gender1)


# looking Variables
df<-df %>% select(-Location1,-School_management,-gender1)
str(df)
df %>% summary()
write_rds(df,"Analysis base.rds")
df<- readRDS("~/ENEM/Analysis base.rds")
df<- df %>% drop_na(Manegement,Location,gender)

#######Loop for models#########################################################################

rm(list=setdiff(ls(), "df"))
#Define variables for loop####
Poluente<- c("airpol_no2","airpol_o3", "airpol_pm25")# Definir Variavel Preditora
for (x in Poluente) {
  print(x)
}
Notas<- c("score_Essay", "Score_General_Subjects")# definir Variavel Resposta
for (y in Notas) {
  print(y)
}
proporçao<- 1 # resample
repetições <- 1:1
for (z in repetições) {
  print(z)
}
Resultado.temp<-data.frame()
resultado<-data.frame()

a<-0
#startLoop ####
for (z in repetições) {
  samp <-
    #slice_sample(df,prop = proporçao,weight_by = UF_Home)
    df
  for (x in Poluente) {
    for (y in Notas) {
      gc()
      b<-length(Poluente)*length(Notas)*length(repetições)
      print(paste("start",x,y,z,"prop:",proporçao,"total:",b, Sys.time()))
      
      
      ## main model#########
      
      model<-"Only Intercept"
      
      mod_A<- lmer(
        samp[[y]]~  samp[[x]]+ #main variables
          #(1+samp[[x]]|UF_Home)+# slope and intercept vary:mixed effect variable
          (0+samp[[x]]|UF_Home)+# slope only  vary:mixed effect variable 
          #(1|UF_Home)+# intercept only vary:mixed effect variable
          bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
          Manegement+Location+gender,# sensitive variables
        data =samp)
      print("model run")
      
      ## Extract information for results ####
      
      n_obs <- as.numeric(nrow(samp))
      n_group <- as.numeric(sapply(ranef(mod_A),nrow))
      coefficients <- fixef(mod_A)#extração de coeficientes de efeito fixo
      beta <- coefficients[2]
      # Extract SE and 95%CI
      Vcov <- vcov(mod_A, useScale = FALSE)
      Std_Errors <- sqrt(diag(Vcov))
      se <- Std_Errors[2]
      upperCI <-  beta + 1.96*se
      lowerCI <-  beta - 1.96*se
      # Extract P-value
      zval <- coefficients / Std_Errors
      pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
      
      pvalue <- pval[2]
      
      # Extract Variance of Random effects
      
      variances <- as.data.frame(VarCorr(mod_A))
      variance_slope <- variances[2,4]
      variance_intercept <- variances[1,4]
      
      #Extração de efeitos aleatórios
      random_efect<-coef(mod_A)[[1]][1:2]
      colnames(random_efect) <- c("Intercept_rand", "Slope_rand")
      #random_efect
      SE<-standard_error(mod_A, effects = "random")[[1]]
      #standard_error(mod_A, effects = "random")
      #colnames(SE) <- c("SE_Intercept_rand", "SE_Slope_rand") # if slope and intercept model
      #colnames(SE) <- c("SE_Slope_rand")# if onlyslope  model
      colnames(SE) <- c("SE_Intercept_rand")# if only intercept model
      
      fator<-rownames(random_efect)
      Resultado.temp <-data.frame()
      Resultado.temp<-cbind(random_efect,SE,fator)
      
      names(Resultado.temp)[2] <-"slope"
      Resultado.temp$efeito_fixo<-beta
      Resultado.temp$poluente<-x
      Resultado.temp$nota<-y
      Resultado.temp$subset<-"overall"
      Resultado.temp$amostragem<- n_obs
      Resultado.temp$N_grupos<- n_group[1]
      Resultado.temp$lowerCI_efx<- lowerCI
      Resultado.temp$upperCI_efx<- upperCI
      Resultado.temp$pvalue_efx<- pvalue
      Resultado.temp$variance_slope_efx<- variance_slope
      Resultado.temp$teste<-paste(x,y,z,sep = ".")
      Resultado.temp$modelo<-paste(x,y,sep = ".")
      Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
      Resultado.temp$model <-model
      
      
      resultado <- rbind.data.frame(resultado, Resultado.temp)
      
      # subset: period####
      #samp<- df %>% drop_na(period)
      subset<-unique(samp$period) 
      gc()
      
      for (k in subset) {
        samp_sub<-samp %>% filter(period==k)
        print(k)
        print(paste("start",k,x,y,z,"prop:",proporçao,"total:",b))
        print(Sys.time())
        ##main model####
        
        mod_A<- lmer(
          samp_sub[[y]]~  samp_sub[[x]]+ #main variables
            #(1+samp_sub[[x]]|UF_Home)+# slope and intercept vary:mixed effect variable
            (0+samp_sub[[x]]|UF_Home)+# slope only  vary:mixed effect variable 
            #(1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
            Manegement+Location+gender,# sensitive variables
          data = samp_sub)
        print("model run")
        
        # Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp_sub))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- fixef(mod_A)#extração de coeficientes de efeito fixo
        beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- sqrt(diag(Vcov))
        se <- Std_Errors[2]
        upperCI <-  beta + 1.96*se
        lowerCI <-  beta - 1.96*se
        # Extract P-value
        zval <- coefficients / Std_Errors
        pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
        
        pvalue <- pval[2]
        
        # Extract Variance of Random effects
        
        variances <- as.data.frame(VarCorr(mod_A))
        variance_slope <- variances[2,4]
        variance_intercept <- variances[1,4]
        
        #Extração de efeitos aleatórios
        random_efect<-coef(mod_A)[[1]][1:2]
        colnames(random_efect) <- c("Intercept_rand", "Slope_rand")
        random_efect
        SE<-standard_error(mod_A, effects = "random")[[1]]
        SE
        #colnames(SE) <- c("SE_Intercept_rand", "SE_Slope_rand") # if slope and intercept model
        #colnames(SE) <- c("SE_Slope_rand")# if onlyslope  model
        colnames(SE) <- c("SE_Intercept_rand")# if only intercept model
        
        fator<-rownames(random_efect)
        Resultado.temp <-data.frame()
        Resultado.temp<-cbind(random_efect,SE,fator)
        
        names(Resultado.temp)[2] <-"slope"
        Resultado.temp$efeito_fixo<-beta
        Resultado.temp$poluente<-x
        Resultado.temp$nota<-y
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$lowerCI_efx<- lowerCI
        Resultado.temp$upperCI_efx<- upperCI
        Resultado.temp$pvalue_efx<- pvalue
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        
        resultado <- rbind.data.frame(resultado, Resultado.temp)
        
        
      }
      
      ## subset: manegement####
      #samp<- df %>% drop_na(Manegement)
      subset<-unique(samp$Manegement)
      gc()
      
      for (k in subset) {
        samp_sub<-samp %>% filter(Manegement==k)
        print(k)
        
        print(paste("start",k,x,y,z,Sys.time()))
        
        ###main model####
        
        
        mod_A<- lmer(
          samp_sub[[y]]~  samp_sub[[x]]+ #main variables
            #(1+samp_sub[[x]]|UF_Home)+# slope and intercept vary:mixed effect variable
            (0+samp_sub[[x]]|UF_Home)+# slope only  vary:mixed effect variable 
            #(1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
            Location+gender,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ## Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp_sub))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- fixef(mod_A)#extração de coeficientes de efeito fixo
        beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- sqrt(diag(Vcov))
        se <- Std_Errors[2]
        upperCI <-  beta + 1.96*se
        lowerCI <-  beta - 1.96*se
        # Extract P-value
        zval <- coefficients / Std_Errors
        pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
        
        pvalue <- pval[2]
        
        # Extract Variance of Random effects
        
        variances <- as.data.frame(VarCorr(mod_A))
        variance_slope <- variances[2,4]
        variance_intercept <- variances[1,4]
        
        #Extração de efeitos aleatórios
        random_efect<-coef(mod_A)[[1]][1:2]
        colnames(random_efect) <- c("Intercept_rand", "Slope_rand")
        random_efect
        SE<-standard_error(mod_A, effects = "random")[[1]]
        #SE
        #colnames(SE) <- c("SE_Intercept_rand", "SE_Slope_rand") # if slope and intercept model
        #colnames(SE) <- c("SE_Slope_rand")# if onlyslope  model
        colnames(SE) <- c("SE_Intercept_rand")# if only intercept model
        
        fator<-rownames(random_efect)
        Resultado.temp <-data.frame()
        Resultado.temp<-cbind(random_efect,SE,fator)
        
        names(Resultado.temp)[2] <-"slope"
        Resultado.temp$efeito_fixo<-beta
        Resultado.temp$poluente<-x
        Resultado.temp$nota<-y
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$lowerCI_efx<- lowerCI
        Resultado.temp$upperCI_efx<- upperCI
        Resultado.temp$pvalue_efx<- pvalue
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        
        resultado <- rbind.data.frame(resultado, Resultado.temp)
        
        
      }
      
      ## subset: Location####
      #samp<- df %>% drop_na(Location)
      
      subset<-unique(samp$Location)
      for (k in subset) {
        samp_sub<-samp %>% filter(Location==k)
        print(k)
        print(paste("start",k,x,y,z,Sys.time()))
        ###main model####
        
        
        mod_A<- lmer(
          samp_sub[[y]]~  samp_sub[[x]]+ #main variables
            #(1+samp_sub[[x]]|UF_Home)+# slope and intercept vary:mixed effect variable
            (0+samp_sub[[x]]|UF_Home)+# slope only  vary:mixed effect variable 
            #(1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
            Manegement+gender,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ## Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp_sub))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- fixef(mod_A)#extração de coeficientes de efeito fixo
        beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- sqrt(diag(Vcov))
        se <- Std_Errors[2]
        upperCI <-  beta + 1.96*se
        lowerCI <-  beta - 1.96*se
        # Extract P-value
        zval <- coefficients / Std_Errors
        pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
        
        pvalue <- pval[2]
        
        # Extract Variance of Random effects
        
        variances <- as.data.frame(VarCorr(mod_A))
        variance_slope <- variances[2,4]
        variance_intercept <- variances[1,4]
        
        #Extração de efeitos aleatórios
        random_efect<-coef(mod_A)[[1]][1:2]
        colnames(random_efect) <- c("Intercept_rand", "Slope_rand")
        random_efect
        SE<-standard_error(mod_A, effects = "random")[[1]]
        #SE
        #colnames(SE) <- c("SE_Intercept_rand", "SE_Slope_rand") # if slope and intercept model
        #colnames(SE) <- c("SE_Slope_rand")# if onlyslope  model
        colnames(SE) <- c("SE_Intercept_rand")# if only intercept model
        
        fator<-rownames(random_efect)
        Resultado.temp <-data.frame()
        Resultado.temp<-cbind(random_efect,SE,fator)
        
        names(Resultado.temp)[2] <-"slope"
        Resultado.temp$efeito_fixo<-beta
        Resultado.temp$poluente<-x
        Resultado.temp$nota<-y
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$lowerCI_efx<- lowerCI
        Resultado.temp$upperCI_efx<- upperCI
        Resultado.temp$pvalue_efx<- pvalue
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        
        resultado <- rbind.data.frame(resultado, Resultado.temp)
        
        
      }
      ## subset: gender####
      #samp<- df %>% drop_na(gender)
      gc()
      
      subset<-unique(samp$gender)
      for (k in subset) {
        samp_sub<-samp %>% filter(Location==k)
        print(k)
        print(paste("start",k,x,y,z,Sys.time()))
        
        ###main model####
        
        
        mod_A<- lmer(
          samp_sub[[y]]~  samp_sub[[x]]+ #main variables
            #(1+samp_sub[[x]]|UF_Home)+# slope and intercept vary:mixed effect variable
            (0+samp_sub[[x]]|UF_Home)+# slope only  vary:mixed effect variable 
            #(1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
            Manegement+Location,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ## Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp_sub))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- fixef(mod_A)#extração de coeficientes de efeito fixo
        beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- sqrt(diag(Vcov))
        se <- Std_Errors[2]
        upperCI <-  beta + 1.96*se
        lowerCI <-  beta - 1.96*se
        # Extract P-value
        zval <- coefficients / Std_Errors
        pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
        
        pvalue <- pval[2]
        
        # Extract Variance of Random effects
        
        variances <- as.data.frame(VarCorr(mod_A))
        variance_slope <- variances[2,4]
        variance_intercept <- variances[1,4]
        
        #Extração de efeitos aleatórios
        random_efect<-coef(mod_A)[[1]][1:2]
        colnames(random_efect) <- c("Intercept_rand", "Slope_rand")
        random_efect
        SE<-standard_error(mod_A, effects = "random")[[1]]
        #SE
        #colnames(SE) <- c("SE_Intercept_rand", "SE_Slope_rand") # if slope and intercept model
        #colnames(SE) <- c("SE_Slope_rand")# if onlyslope  model
        colnames(SE) <- c("SE_Intercept_rand")# if only intercept model
        
        fator<-rownames(random_efect)
        Resultado.temp <-data.frame()
        Resultado.temp<-cbind(random_efect,SE,fator)
        
        names(Resultado.temp)[2] <-"slope"
        Resultado.temp$efeito_fixo<-beta
        Resultado.temp$poluente<-x
        Resultado.temp$nota<-y
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$lowerCI_efx<- lowerCI
        Resultado.temp$upperCI_efx<- upperCI
        Resultado.temp$pvalue_efx<- pvalue
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        resultado <- rbind.data.frame(resultado, Resultado.temp)
        
        
      }
      a<-a+1
      print(paste("end",x,y,z,",repeat:",round(a/b*100,digits = 3),"%"))
      
    }
  }
  
}


#Save Results############

resultado[,4]<-lapply(resultado[,4], as.factor)
resultado[,6:8]<-lapply(resultado[,6:8], as.factor)
resultado[,15:18]<-lapply(resultado[,15:18], as.factor)

write_rds(resultado, "Model_Resultas_Intercept")
summary(resultado)
