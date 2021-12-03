pacman::p_load("tidyverse","magrittr","readxl","caret","glmnet")
library(kableExtra)
library(dplyr)
library(sqldf)
library(reshape2)
library(dplyr)
library(corrgram)
library(ggplot2)
library(pROC)
library(readxl)
library(pscl)
library(sandwich)
library(lmtest)
library(MASS)
library(magrittr)
theme_set(theme_bw())
library(ggstance)
library(kableExtra)
library(gridExtra)
library(randomForest)

# Exploratory Data Analysis

seer<- read.csv("seer_final.csv",header = T)

#colnames(seer)
seer$Nine_Grade%<>%as.numeric()
seer$High_School%<>%as.numeric()
seer$Atleast_Bachlor%<>%as.numeric()
seer$Person_Below_Poverty%<>%as.numeric()
seer$Unemployed%<>%as.numeric()
seer$Median_Household_Income%<>%as.numeric()
seer$Language_Isolation%<>%as.numeric()
seer$Survival_Months%<>%as.numeric()
seer$Vital_Status%<>%as.factor()

for (i in c(1:length(seer$Therapy_1))){
  st=paste(seer$Therapy_1[i],seer$Therapy_2[i],seer$Therapy_3[i],seer$Therapy_4[i],seer$Therapy_5[i])
   if(!str_detect(st,'Chem')&str_detect(seer$Chemotherapy[i],'Yes') == TRUE){
      seer$Diff_Reco_Chem[i] = 1;
   }
else {
    seer$Diff_Reco_Chem[i] = 0;
  }
}

seer$Nine_Grade= seer$Nine_Grade/100
seer$High_School= seer$High_School/100
seer$Atleast_Bachlor= seer$Atleast_Bachlor/100
seer$Person_Below_Poverty= seer$Person_Below_Poverty/100
seer$Unemployed= seer$Unemployed/100
seer$Median_Household_Income = seer$Median_Household_Income*10
seer$Language_Isolation = seer$Language_Isolation/100

race_community<- sqldf("select Race, avg(Nine_Grade) as NineGrade, avg(High_School) as HighSchool, avg(Atleast_Bachlor) as Bachlor, avg(Person_Below_Poverty) as BelowPoverty, avg(Unemployed) as Unemployed, avg(Median_Household_Income) as Income, avg(Language_Isolation) as LanguageIsolation from seer
                       group by Race")

melt_race<- melt(race_community,id.vars=c("Race"),
                measure.vars = c("NineGrade","HighSchool","Bachlor","BelowPoverty","Unemployed","LanguageIsolation"),
                variable.name = "community")

### Demographic Analysis

seer%>%group_by(Race,Sex)%>%summarise(count=n())%>%group_by(Race)%>%
  summarise(count=count,percent=count/sum(count),Sex=Sex)->g1

library(ggthemes)
#devtools::install_github('cttobin/ggthemr')
library(ggthemr)
library(scales,warn.conflicts=F)
ggthemr('dust',type = "outer")
ggplot(g1,aes(x=Race,y=count,fill=Sex))+
  geom_histogram(stat="identity",position = "stack")+
    coord_flip()

ggthemr_reset()
ggplot(seer,aes(fill=Race,col=Race,x=Age))+ geom_density(alpha=.2)+
theme(panel.background = element_rect(fill = 'black'))+
theme_wsj(base_size = 10)

seer%>%group_by(Insurance,Race)%>%summarise(count=n())%>%group_by(Race)%>%summarise(count=count,percent=count/sum(count),Insurance=Insurance)->g2

ggplot(g2,aes(x=Race,y=count,fill=Insurance))+
  geom_histogram(stat="identity",position = "stack")+
  #geom_text(mapping = aes(label =round(percent,2)), size = 2, color = 'black',
   #         position = position_stack(0.6))+
  #theme(axis.text.x  = element_text(angle=30, vjust=0.5))
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  theme(
        axis.text.x =element_text(vjust=0.5))

ggthemr_reset()
race_percent<- ggplot(data=melt_race)+
  geom_line(mapping=aes(x=Race,y=value,group=community,color=community))+
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  labs(title="Community Information to Races", x="Race",y="Percentage" )+
  theme(panel.background = element_rect(fill = 'black'))+
theme_wsj(base_size = 8)
race_percent

race_income<- ggplot(data=race_community)+
  geom_bar(aes(x=Race,y=Income,fill=Race),stat="identity")+
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  labs(title='Average Income to different Races', x="Race",fill="Race")+
  theme(legend.position = 'none',plot.title = element_text(size=10,hjust = 0.5))
race_income

### Clinical Information Analysis

race_survival<- ggplot(data=seer, aes(x = Race, y = as.numeric(Survival_Months), fill =Race)) +
        geom_boxplot(alpha=0.7) +
        scale_y_continuous(name = "Survival Month")+
        #scale_x_discrete(name = "Race")+
        scale_x_discrete(guide = guide_axis(n.dodge=2)) +
        labs(title="Average Survival Months to Different Races", fill="Race")+
  theme(legend.position = 'none',plot.title = element_text(size=10,hjust = 0.5))

death<- sqldf("select Race, Cause_Death from seer")
for (i in 1:7286){
  if (str_detect(death$Cause_Death[i], "attributable")){
    death$dead[i] = 1
  }else{
    death$dead[i] = 0
  }
}

death2<- sqldf("select Race, avg(dead) as Death_Rate from death group by Race")
race_death<- ggplot(data=death2)+
  geom_bar(aes(x=Race,y=Death_Rate,fill=Race),stat="identity")+
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  labs(title="Death Rate Attributable by This Cancer", x="Race",y="Death_Rate",fill="Race")+
  theme(legend.position = 'none',plot.title = element_text(size=10,hjust = 0.5))

grid.arrange(race_survival,race_death,ncol = 2)

### Insights From EDA

# Modelling

### Feature Extraction and Model Selection

fit1=glm(Diff_Reco_Sur ~ Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer,family=binomial(link="logit"))
#summary(fit1)

fit11=glm(Diff_Reco_Rad ~ Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer,family=binomial(link="logit"))
#summary(fit11)

fit111=glm(Diff_Reco_Chem ~ Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer,family=binomial(link="logit"))
#summary(fit111)

Sur <- data.frame(summary(fit1)$coefficients)[,c(1,4)]
Rad <- data.frame(summary(fit11)$coefficients)[,c(1,4)]
Chem <- data.frame(summary(fit111)$coefficients)[,c(1,4)]
output <- cbind(Sur,Rad,Chem)
colnames(output) <- c('Sur','p_value','Rad','p_value','Chem','p_value')

output %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  footnote(general = "The table above is the summary output of three different Logistic Regression models.",
           general_title = "Table 1: ")

pre1 <- predict(fit1,type='response')
plot.roc(seer$Diff_Reco_Sur, pre1,main="Surgery ROC Curve",col="red", percent=TRUE,print.auc=TRUE,ci=TRUE,of="thresholds",thresholds="best",print.thres="best")
pre11 <- predict(fit11,type='response')
plot.roc(seer$Diff_Reco_Rad, pre1,main="Radiation ROC Curve",col="red", percent=TRUE,print.auc=TRUE,ci=TRUE,of="thresholds",thresholds="best",print.thres="best")
pre111 <- predict(fit111,type='response')
plot.roc(seer$Diff_Reco_Chem, pre1,main="Chem ROC Curve",col="red", percent=TRUE,print.auc=TRUE,ci=TRUE,of="thresholds",thresholds="best",print.thres="best")
rocplot1 <- roc(seer$Diff_Reco_Sur,pre1)
ci.auc(rocplot1)
rocplot11 <- roc(seer$Diff_Reco_Rad,pre11)
ci.auc(rocplot1)
rocplot111 <- roc(seer$Diff_Reco_Chem,pre111)
ci.auc(rocplot1)

### Topic 2: difference between the recommended and patient’s attitude

fit2 = glm(surgery_refused ~ Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer %>% filter(surgery_recommended ==1),family=binomial(link="logit"))
#summary(fit2)

fit22 = glm(radiation_refused ~ Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer %>% filter(radiation_recommended ==1),family=binomial(link="logit"))
#summary(fit22)

Surgery_refused <- data.frame(summary(fit2)$coefficients)[,c(1,4)]
Radiation_refused <- data.frame(summary(fit22)$coefficients)[,c(1,4)]
output2 <- cbind(Surgery_refused,Radiation_refused)
colnames(output2) <- c('Surgery_refused','p_value','Radiation_refused','p_value')

output2 %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria") %>%
  footnote(general = "The table above is the summary output of different Logistic Regression models.",general_title = "Table 2: ")

### Topic 3: Vital Status

col_one <- c('Diff_Reco_Sur','Diff_Reco_Rad','Diff_Reco_Chem')
col_two <- c('significant','significant','significant')
col_three <- c('significant','non-significant','significant')
col_four <- c('significant','significant','significant')
output3 <- cbind(col_one,col_two,col_three,col_four)
colnames(output3) <- c('Treatment','ACME(Indirect Effect)','ADE(Direct Effect)','Total Effect')

output3 %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Cambria")

rf=randomForest(Survival_Months ~ Diff_Reco_Rad+Diff_Reco_Sur+Diff_Reco_Chem+radiation_refused+surgery_refused+Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer)
#summary(rf)
varImpPlot(x=rf,sort=TRUE,n.var=nrow(rf$importance),main="Importance")

#feature.names=names(seer)[-21]
#dtrain <- xgb.DMatrix(data.matrix(seer[,feature.names]), label=seer$Survival_Months)
#xgboost(data = dtrain, 
#        booster = "gbtree", 
#        objective = "reg:squarederror", 
#        max.depth = 5, 
#        eta = 0.5, 
#        nthread = 2, 
#        nround = 2, 
#        min_child_weight = 1, 
#        subsample = 0.5, 
#        colsample_bytree = 1, 
#        num_parallel_tree = 1)

rf2=randomForest(Vital_Status ~ Diff_Reco_Rad+Diff_Reco_Sur+Diff_Reco_Chem+radiation_refused+surgery_refused+Race+Age+Nine_Grade+Atleast_Bachlor+Person_Below_Poverty+Unemployed+Median_Household_Income+Language_Isolation,data = seer)
#summary(rf2)
varImpPlot(x=rf2,sort=TRUE,n.var=nrow(rf$importance),main="Importance")