rm(list=setdiff(ls(), "df"))

library(tidyverse)
library(lme4)
library(parameters)
library(splines)

#define variables for loop


Notas<- c("score_Essay", "Score_General_Subjects")# definir Variavel Resposta
for (y in Notas) {
  print(y)
}

proporçao<- 0.01 # resample
repetições <- 1:1
for (z in repetições) {
  print(z)
}
Resultado.temp<-data.frame()
resultado<-data.frame()

a<-0

#Start model only  Intercept ##################
for (z in repetições) {
  
  samp <-df #%>%  slice_sample(prop = proporçao)
  for (y in Notas) {
      gc()
      Sys.sleep(10)   
      b<-length(x)*length(Notas)*length(repetições)
      print(paste("start",x,y,z,"prop:",proporçao,"total:",b))
      print(Sys.time())
      
      ## main model####
      
      model<-"only Intercept"
      
      mod_A<- lmer(
        samp[[y]]~  airpol_no2+airpol_o3+ airpol_pm25+ #main variables
          #(1+wildfire|UF_Home)+# slope and intercept vary:mixed effect variable
          #(0+wildfire|UF_Home)+# slope only  vary:mixed effect variable 
          (1|UF_Home)+# intercept only vary:mixed effect variable
          bs(as.numeric(year))+#preciptation+Wind_speed+
          Educ_mother+income+Munc_HDI+#standat control variables
          Manegement+Location+gender,# sensitive variables
        data =samp)
      summary(mod_A)
      model_summary<-summary(mod_A)
      saveRDS(model_summary,paste("model_summary",model,x,y,z,".rds"))
      print("model run")
      
      ### Extract information for results ####
      
      n_obs <- as.numeric(nrow(samp))
      n_group <- as.numeric(sapply(ranef(mod_A),nrow))
      coefficients <- as.data.frame(fixef(mod_A))#extração de coeficientes de efeito fixo
      beta<-head(coefficients,4)
      beta
      #beta <- coefficients[2]
      # Extract SE and 95%CI
      Vcov <- vcov(mod_A, useScale = FALSE)
      Std_Errors <- as.data.frame(sqrt(diag(Vcov)))
      se0 <- head(Std_Errors,4)
      beta$se<-se0$`sqrt(diag(Vcov))`
      beta
      beta$upperCI <-  beta$`fixef(mod_A)` + 1.96*beta$se
      beta$lowerCI <-  beta$`fixef(mod_A)` - 1.96*beta$se
      # Extract P-value
      beta$zval <- beta$`fixef(mod_A)` / beta$se
      beta
      beta$pval <- 2 * pnorm(abs(beta$zval), lower.tail = FALSE)
      beta
      #pvalue <- pval[2]
      
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
      random_efect$SE_Intercept_rand<-SE
      
      random_efect$fator<-rownames(random_efect)
      beta$poluent<-rownames(beta)
      
      fixed_effect<-beta
      fixed_effect %>%  gather("meta",value,-poluent) %>% mutate(nome=paste0(poluent,"sep",meta)) %>% 
        select(value,nome) ->fixed_df
      A<-t(fixed_df[1])
      B<-(t(fixed_df[2]))
      colnames(A)<-B
      A
      Resultado.temp<-cbind(A,random_efect)
      Resultado.temp
      
      #names(Resultado.temp$)[2 <-"slope"
      Resultado.temp
      Resultado.temp$nota<-y
      
      Resultado.temp$subset<-"overall"
      Resultado.temp$amostragem<- n_obs
      Resultado.temp$N_grupos<- n_group[1]
      Resultado.temp$variance_slope_efx<- variance_slope
      Resultado.temp$teste<-paste(x,y,z,sep = ".")
      Resultado.temp$modelo<-paste(x,y,sep = ".")
      Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
      Resultado.temp$model <-model
      
      
      resultado <- rbind.data.frame(resultado, Resultado.temp)
      
      ## subset: period####
      samp_sub<- samp %>% drop_na(period)
      subset<-unique(samp_sub$period) 
      for (k in subset) {
        samp_sub<-samp %>% filter(period==k)
        print(k)
        print(paste("start",k,x,y,z,"prop:",proporçao,"total:",b))
        print(Sys.time())
        Sys.sleep(10)
        ###main model####
        
        unique(samp_sub$UF_Home)
        mod_A<- lmer(
          samp_sub[[y]]~  airpol_no2+airpol_o3+ airpol_pm25+ #main variables#main variables
            #(1+wildfire|UF_Home)+# slope and intercept vary:mixed effect variable
            #(0+wildfire|UF_Home)+# slope only  vary:mixed effect variable 
            (1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
            Manegement+Location+gender,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ### Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- as.data.frame(fixef(mod_A))#extração de coeficientes de efeito fixo
        beta<-head(coefficients,4)
        beta
        #beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- as.data.frame(sqrt(diag(Vcov)))
        se0 <- head(Std_Errors,4)
        beta$se<-se0$`sqrt(diag(Vcov))`
        beta
        beta$upperCI <-  beta$`fixef(mod_A)` + 1.96*beta$se
        beta$lowerCI <-  beta$`fixef(mod_A)` - 1.96*beta$se
        # Extract P-value
        beta$zval <- beta$`fixef(mod_A)` / beta$se
        beta
        beta$pval <- 2 * pnorm(abs(beta$zval), lower.tail = FALSE)
        beta
        #pvalue <- pval[2]
        
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
        random_efect$SE_Intercept_rand<-SE
        random_efect$fator<-rownames(random_efect)
        beta$poluent<-rownames(beta)
        
        fixed_effect<-beta
        fixed_effect %>%  gather("meta",value,-poluent) %>% mutate(nome=paste0(poluent,"sep",meta)) %>% 
          select(value,nome) ->fixed_df
        A<-t(fixed_df[1])
        B<-(t(fixed_df[2]))
        colnames(A)<-B
        A
        Resultado.temp<-cbind(A,random_efect)
        Resultado.temp
        
        #names(Resultado.temp$)[2 <-"slope"
        Resultado.temp
        Resultado.temp$nota<-y
        
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        
        
        resultado <- rbind.data.frame(resultado, Resultado.temp)
        
        
        
      }
      
      # subset: manegement####
      samp_sub<- samp %>% drop_na(Manegement)
      subset<-unique(samp_sub$Manegement)
      table(samp$Manegement)
      for (k in subset) {
        samp_sub<-samp %>% filter(Manegement==k)
        
        print(paste("start",k,x,y,z,Sys.time()))
        
        Sys.sleep(10)
        ##main model####
        
        
        mod_A<- lmer(
          samp_sub[[y]]~  airpol_no2+airpol_o3+ airpol_pm25+ #main variables+ #main variables
            #(1+wildfire|UF_Home)+# slope and intercept vary:mixed effect variable
            #(0+wildfire|UF_Home)+# slope only  vary:mixed effect variable 
            (1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+Educ_mother+income+Munc_HDI+#standat control variables
            Location+gender,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ### Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- as.data.frame(fixef(mod_A))#extração de coeficientes de efeito fixo
        beta<-head(coefficients,4)
        beta
        #beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- as.data.frame(sqrt(diag(Vcov)))
        se0 <- head(Std_Errors,4)
        beta$se<-se0$`sqrt(diag(Vcov))`
        beta
        beta$upperCI <-  beta$`fixef(mod_A)` + 1.96*beta$se
        beta$lowerCI <-  beta$`fixef(mod_A)` - 1.96*beta$se
        # Extract P-value
        beta$zval <- beta$`fixef(mod_A)` / beta$se
        beta
        beta$pval <- 2 * pnorm(abs(beta$zval), lower.tail = FALSE)
        beta
        #pvalue <- pval[2]
        
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
        random_efect$SE_Intercept_rand<-SE
        random_efect$fator<-rownames(random_efect)
        beta$poluent<-rownames(beta)
        
        fixed_effect<-beta
        fixed_effect %>%  gather("meta",value,-poluent) %>% mutate(nome=paste0(poluent,"sep",meta)) %>% 
          select(value,nome) ->fixed_df
        A<-t(fixed_df[1])
        B<-(t(fixed_df[2]))
        colnames(A)<-B
        A
        Resultado.temp<-cbind(A,random_efect)
        Resultado.temp
        
        #names(Resultado.temp$)[2 <-"slope"
        Resultado.temp
        Resultado.temp$nota<-y
        
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        
        
        resultado <- rbind.data.frame(resultado, Resultado.temp)
      }
      
      # subset: Location####
      samp_sub<- samp %>% drop_na(Location)
      
      subset<-unique(samp_sub$Location)
      for (k in subset) {
        samp_sub<-samp %>% filter(Location==k)
        
        print(paste("start",x,k,y,z,Sys.time()))
        
        Sys.sleep(10)
        ##main model####
        
        
        mod_A<- lmer(
          samp_sub[[y]]~ airpol_no2+airpol_o3+ airpol_pm25+ #main variables+ #main variables
            #(1+wildfire|UF_Home)+# slope and intercept vary:mixed effect variable
            #(0+wildfire|UF_Home)+# slope only  vary:mixed effect variable 
            (1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+preciptation+Wind_speed+Educ_mother+income+Munc_HDI+#standat control variables
            Manegement+gender,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ### Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- as.data.frame(fixef(mod_A))#extração de coeficientes de efeito fixo
        beta<-head(coefficients,4)
        beta
        #beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- as.data.frame(sqrt(diag(Vcov)))
        se0 <- head(Std_Errors,4)
        beta$se<-se0$`sqrt(diag(Vcov))`
        beta
        beta$upperCI <-  beta$`fixef(mod_A)` + 1.96*beta$se
        beta$lowerCI <-  beta$`fixef(mod_A)` - 1.96*beta$se
        # Extract P-value
        beta$zval <- beta$`fixef(mod_A)` / beta$se
        beta
        beta$pval <- 2 * pnorm(abs(beta$zval), lower.tail = FALSE)
        beta
        #pvalue <- pval[2]
        
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
        random_efect$SE_Intercept_rand<-SE
        random_efect$fator<-rownames(random_efect)
        beta$poluent<-rownames(beta)
        
        fixed_effect<-beta
        fixed_effect %>%  gather("meta",value,-poluent) %>% mutate(nome=paste0(poluent,"sep",meta)) %>% 
          select(value,nome) ->fixed_df
        A<-t(fixed_df[1])
        B<-(t(fixed_df[2]))
        colnames(A)<-B
        A
        Resultado.temp<-cbind(A,random_efect)
        Resultado.temp
        
        #names(Resultado.temp$)[2 <-"slope"
        Resultado.temp
        Resultado.temp$nota<-y
        
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
        Resultado.temp$variance_slope_efx<- variance_slope
        Resultado.temp$teste<-paste(x,y,z,sep = ".")
        Resultado.temp$modelo<-paste(x,y,sep = ".")
        Resultado.temp$formula<-as.character(summary(mod_A)[[15]])[2]
        Resultado.temp$model <-model
        
        
        resultado <- rbind.data.frame(resultado, Resultado.temp)
        
      }
      
      # subset: gender####
      samp_sub<- samp %>% drop_na(gender)
      
      subset<-unique(samp_sub$gender)
      for (k in subset) {
        samp_sub<-samp %>% filter(gender==k)
        
        print(paste("start",x,k,y,z,Sys.time()))
        Sys.sleep(10)
        
        ##main model####
        
        
        mod_A<- lmer(
          samp_sub[[y]]~  airpol_no2+airpol_o3+ airpol_pm25+ #main variableswildfire+samp_sub[[x]]+ #main variables
            #(1+wildfire|UF_Home)+# slope and intercept vary:mixed effect variable
            #(0+wildfire|UF_Home)+# slope only  vary:mixed effect variable 
            (1|UF_Home)+# intercept only vary:mixed effect variable
            bs(as.numeric(year))+preciptation+Wind_speed+Educ_mother+income+Munc_HDI+#standat control variables
            Manegement+Location,# sensitive variables
          data =samp_sub)
        print("model run")
        
        ### Extract information for results ####
        
        n_obs <- as.numeric(nrow(samp))
        n_group <- as.numeric(sapply(ranef(mod_A),nrow))
        coefficients <- as.data.frame(fixef(mod_A))#extração de coeficientes de efeito fixo
        beta<-head(coefficients,4)
        beta
        #beta <- coefficients[2]
        # Extract SE and 95%CI
        Vcov <- vcov(mod_A, useScale = FALSE)
        Std_Errors <- as.data.frame(sqrt(diag(Vcov)))
        se0 <- head(Std_Errors,4)
        beta$se<-se0$`sqrt(diag(Vcov))`
        beta
        beta$upperCI <-  beta$`fixef(mod_A)` + 1.96*beta$se
        beta$lowerCI <-  beta$`fixef(mod_A)` - 1.96*beta$se
        # Extract P-value
        beta$zval <- beta$`fixef(mod_A)` / beta$se
        beta
        beta$pval <- 2 * pnorm(abs(beta$zval), lower.tail = FALSE)
        beta
        #pvalue <- pval[2]
        
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
        random_efect$SE_Intercept_rand<-SE
        random_efect$fator<-rownames(random_efect)
        beta$poluent<-rownames(beta)
        
        fixed_effect<-beta
        fixed_effect %>%  gather("meta",value,-poluent) %>% mutate(nome=paste0(poluent,"sep",meta)) %>% 
          select(value,nome) ->fixed_df
        A<-t(fixed_df[1])
        B<-(t(fixed_df[2]))
        colnames(A)<-B
        A
        Resultado.temp<-cbind(A,random_efect)
        Resultado.temp
        
        #names(Resultado.temp$)[2 <-"slope"
        Resultado.temp
        Resultado.temp$nota<-y
        
        Resultado.temp$subset<-k
        Resultado.temp$amostragem<- n_obs
        Resultado.temp$N_grupos<- n_group[1]
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
  write_rds(resultado, paste("Model_",model,"Resultas_intercept_tripol_test.0.9.rds"))

