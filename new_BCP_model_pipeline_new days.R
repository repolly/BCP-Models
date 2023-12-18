library(readxl)
library(stringr)
library(tidyverse)
library(caret)
library(survey)
library(pscl)
# ##################################################################################################
# ##prepare climate data NUTS1 Level
# ##################################################################################################
# years <- 1986:2019
types <- c("heat waves","cold waves", "droughts", "floods", "fire weather risk")
extr <- paste0("Regional exposure_",types)
sheetsExposurePiv <- excel_sheets(paste0("Climate exposure & impact indexes/",extr[1],".xlsx"))#29sheets
sheetsWeather <- excel_sheets(paste0("Climate exposure & impact indexes/Weather extremes_90%_regions.xlsx"))#5sheets

allregions <- c()
for(i in 1:5){
  days <- data.frame(read_excel(paste0("Climate exposure & impact indexes/Weather extremes_90%_regions.xlsx"),sheet = i))
  allregions <- unique(c(allregions,days[1:which(is.na(days$NUTS_ID))[1],1:42]$NUTS_ID))
}
allregions <- allregions[is.na(allregions)==F]

counts <- unique(substr(pr1$NUTS_ID,1,2))



sheetsarea <- excel_sheets(paste0("Climate exposure & impact indexes/",extr[1],".xlsx"))[-29]#29sheets
weights <- data.frame()
for (i in 1:length(sheetsarea))
{
  # i=1
  area <- data.frame(read_xlsx(paste0("Climate exposure & impact indexes/",extr[1],".xlsx"),sheet = sheetsarea[i]))
  area <- area[,c("NUTS_ID", "CROP_ID", "Average.crop.supply.2001.2020.per.NUTS.1.region..1000.tonnes.")]
  weights <- rbind(weights,area) 
}
weights <- weights[is.na(weights$CROP_ID)==F,]
weights <- weights[is.na(weights$NUTS_ID)==F,]
weights$CROP_ID[weights$CROP_ID=="C1410"] <- "C1400"
unique(weights$CROP_ID)

############################################create pivort table
piv <- data.frame()
for(i in 1:4){
  # i=1
  days <- data.frame(read_excel(paste0("Climate exposure & impact indexes/Weather extremes_75%_regions.xlsx"),sheet = i))
  which(days$NUTS_ID=="NUTS_ID")

  # days <- days[1:which(is.na(days$NUTS_ID))[1],1:42]#all data
  days <- days[(which(days$NUTS_ID=="NUTS_ID")+1):(which(days$NUTS_ID=="SUM")-1),1:42]#only 90 percentile
  td <- t(days)
  piv0 <- data.frame()
  for(j in 1:ncol(td))
  {
    # j=1
    years <- as.numeric(substr(names(td[,j])[-1:-2],2,5))
    days <- as.numeric(td[,j][-1:-2])
    extreme <- rep(td[,j][2],length(days))
    nuts_id <- rep(td[,j][1],length(days))
    piv1 <- data.frame("NUTS_ID"=nuts_id,"extreme"=extreme,"year"=years,"days"=days)
    piv0 <- rbind(piv0,piv1)
  }
  piv <- rbind(piv, piv0)
}
piv <- piv[is.na(piv$extreme)==F,]
piv$extreme[piv$extreme=="Flood*"] <- "Flood"
###############################################create feature columns
alldays <- data.frame("NUTS_ID"=allregions)
for(i in 1:4)
{
  # i=1
  extreme <- unique(piv$extreme)[i]
  pivextr <- piv[piv$extreme==extreme,-2]
  names(pivextr)[3] <- extreme
  alldays <- left_join(alldays,pivextr)
}
alldays[is.na(alldays)] <- 0
alldays
alldays$country <- substr(alldays$NUTS_ID,1,2)
names(alldays)[c(3,4,6)] <- c("heatwave","coldwave","flood")
alldays
#aggregate to nuts0 level
# aggdays <- aggregate(heatwave~year+country,data=alldays,sum)
# aggdays <- left_join(aggdays,aggregate(coldwave~year+country,data=alldays,sum))
# aggdays <- left_join(aggdays,aggregate(drought ~year+country,data=alldays,sum))
# aggdays <- left_join(aggdays,aggregate(flood~year+country,data=alldays,sum))
# aggdays <- left_join(aggdays,aggregate(firerisk~year+country,data=alldays,sum))
# aggdays
# write.csv(aggdays,"Aggregated_days_90_perc.csv")
# aggdays <- read.csv("Aggregated_days.csv",sep = ",")[,-1]
# aggdays90 <- read.csv("Aggregated_days_90_perc.csv",sep = ",")[,-1]
# aggdays90 <- aggdays90[aggdays90$year!=0,]
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
##################################################################################################
##prepare exposure data
##################################################################################################
# pivreg <- data.frame(read.table(paste0("Pivot_Regional exposure_Fabio.csv"),sep=",",header=T))
# pivreg <- pivreg[,c(-1,-3,-4,-5,-7:-12,-47:-50,-53)]
# names(pivreg)
# aggexpos <- data.frame()
# for (type in unique(pivreg$exposure_type)[1:5])
# {
#   # type <- "droughts"
#   for (count in unique(pivreg$Countries))
#   {
#     # count="AT"
#     for(cropid in unique(pivreg$CROP_ID)[-2])
#     {
#       # cropid="C1100"
#       tempexp <- pivreg[(pivreg$CROP_ID==cropid)+(pivreg$Countries==count)+(pivreg$exposure_type==type)==3,]
#       tempexp <- tempexp[is.na(tempexp$NUTS_ID)==F,]#replace numbers
#       for(i in 1:nrow(tempexp))
#       {
#         tempexp[i,3:36][tempexp[i,3:36]>0] <- tempexp$Distribution.of.crop.supply.among.NUTS1.regions[i]
#       }
#       exposure <- colSums(tempexp[,-c(1,2,37,38,39,40)])
#       years <- 1986:2019
#       extreme <- type
#       country <- count
#       piv1 <- data.frame("country"=country,"extreme"=extreme,"cropid"=cropid,"year"=years,"exposedarea"=exposure)
#       aggexpos <- rbind(aggexpos,piv1)
#     }
#   }
# }
# aggexpos
# write.csv(aggexpos,"Pivot_Aggregated_Exposed_area_EU.csv")
# ###
# aggexpos <- read.csv("Pivot_Aggregated_Exposed_area_EU.csv",sep = ",")[,-1]
# allexpos <- data.frame("country"=unique(aggexpos$country))
# for(i in 1:5)
# {
#   # i=2
#   extreme <- unique(aggexpos$extreme)[i]
#   pivexpos <- aggexpos[aggexpos$extreme==extreme,-2]
#   names(pivexpos)[4] <- paste0("exp_area_",extreme)
#   allexpos <- left_join(allexpos,pivexpos)
# }
# 
# write.csv(allexpos,"Aggregated_Exposed_area_EU.csv")
# allexpos <- read.csv("Aggregated_Exposed_area.csv",sep = ",")[,-1]
# allexposEU <- read.csv("Aggregated_Exposed_area_EU.csv",sep = ",")[,-1]
# allexpos[,4:8] <- allexpos[,4:8]*100
# allexposEU[,4:8] <- allexposEU[,4:8]*100
# names(allexpos)[c(4,5,6,7,8)] <- c("exp_area_heatwaves","exp_area_coldwaves","exp_area_droughts","exp_area_floods","exp_area_fireweatherrisk")
# names(allexposEU)[c(4,5,6,7,8)] <- c("exp_area_heatwaves","exp_area_coldwaves","exp_area_droughts","exp_area_floods","exp_area_fireweatherrisk")
# names(aggdays)[c(3,4,5,6,7)] <- c("days_heatwave","days_coldwave","days_drought","days_flood","days_firerisk")
# names(aggdays90)[c(3,4,5,6,7)] <- c("days_heatwave","days_coldwave","days_drought","days_flood","days_firerisk")
#############################################################################################################################################
#############################################################################################################################################
#############################################################################################################################################
#overview: 
#         dependent = impact columns (year, country, crop name or code) && (impact columns)
#         independent 1 = climate data NUTS 1 Level (year, country) && (days aggregated for all regions(heat, cold,flood,fire,drought))
#         independent 2 = exposed area for each crop (year, country, crop code) && (area in % for all regions(heat, cold,flood,fire,drought))
##################################################################################################
##prepare climate data
##################################################################################################

impacts <- data.frame(read_excel(paste0("Climate exposure & impact indexes/EU Climate Extreme Impact database_28.11.2022.xlsx")))
impacts[,6:9] <- (impacts[,6:9]!=0)*1
names(impacts)[c(1,3,4)] <- c("year","cropid","country")
imp <- impacts[impacts$country=="AT",]
data <- left_join(left_join(impacts,aggdays90),allexpos)#aggdays),allexpos)
data[is.na(data)] <- 0
d0 <- data.frame()
zero_threshhold <- 0.2
scarecity_threshhold <- 0.5
reg_levels <- c("Subnational","National","EU")
for(reg_level in 1:2){
  # reg_level=1
  d0 <- data.frame()
  for (count in unique(data$country))
  {
    # count="AT"
    for(cropid in unique(data$cropid))
    {
      # cropid="C1100"
      dat <- data[(data$cropid==cropid)+(data$country==count)==2,]
      dat
      #change days: to weighted ones
      ad <- alldays[alldays$country==count,]#get country data
      w <- weights[(weights$CROP_ID==cropid),]#get crop weights
      if(nrow(w)>0 && sum(w[substr(w$NUTS_ID,1,2)==count,3])>0)
      {
        jo <- left_join(ad,w)
        jo$share <- jo$Average.crop.supply.2001.2020.per.NUTS.1.region..1000.tonnes./sum(w[substr(w$NUTS_ID,1,2)==count,3])
        jo <- jo[is.na(jo$share)==F,]
        jo[,3:6] <- jo[,3:6]*jo$share
        jo$area_heatwave <- 0
        jo$area_coldwave <- 0
        jo$area_drought <- 0
        jo$area_flood <- 0
        jo$area_heatwave[jo$heatwave>0] <- jo$share[jo$heatwave>0]
        jo$area_coldwave[jo$coldwave>0] <- jo$share[jo$coldwave>0]
        jo$area_drought[jo$drought>0] <- jo$share[jo$drought>0]
        jo$area_flood[jo$flood>0] <- jo$share[jo$flood>0]
        jo <- jo[,-c(1,8,9,10)]
        jo <- aggregate(.~year+country ,data=jo,sum)
        names(jo)[3:6] <- c("days_heatwave","days_coldwave","days_drought","days_flood")
        names(jo)[7:10] <- c("exp_area_heatwaves","exp_area_coldwaves","exp_area_droughts","exp_area_floods")
        
        dat <- cbind(dat[,1:9],jo[,-1:-2])
        #####lin model and p-value
        # lin <- lm(Sub.national.extremes..0.75.perc..~days_heatwave+days_coldwave+days_drought+days_flood+days_firerisk+
        #             exp_area_heatwaves+exp_area_coldwaves+exp_area_droughts+exp_area_floods+exp_area_fireweatherrisk,data=dat)
        # su <- summary(lin)
        # su$r.squared
        # f <- su$fstatistic
        # pb <- pf(f[1],f[2],f[3],lower.tail=F)
        # attributes(pb) <- NULL
        uncertainty_indicator <- dat$Regional.yield.data.coverage..uncertainty.indicator.[1]
        cropname <- dat$Eurostat_Product[1]
        
        #####logistic regression model with odds indication
        if(reg_level==1){classifier <- glm(Sub.national.extremes..0.75.perc..~days_heatwave+days_coldwave+days_drought+days_flood+
                                             exp_area_heatwaves+exp_area_coldwaves+exp_area_droughts+exp_area_floods,data=dat)}
        if(reg_level==2){classifier <- glm(National.extreme..0.75.perc..~days_heatwave+days_coldwave+days_drought+days_flood+
                            exp_area_heatwaves+exp_area_coldwaves+exp_area_droughts+exp_area_floods,data=dat)}
        
        # We can calculate the multiplicative effect of a feature on the odds ratio via:
        odds <- exp(coef(classifier))
        #The odds tell us how much more ‘likely’ an outcome of 1 is than an outcome of 0.
        #Here, a unit change in # of times pregnant increases the odds of diabetes by a factor 1.13, 
        #while a unit change in diabetes pedigree function increases the odds by a factor of 2.56.
        # odds
        
        # Pseudo R^2
        #Unlike linear regression with ordinary least squares estimation, there is no R2 statistic which explains the 
        #proportion of variance in the dependent variable that is explained by the predictors. However, there are a number 
        #of pseudo R2 metrics that could be of value. Most notable is McFadden’s R2, which is defined as 1−[ln(LM)/ln(L0)] 
        #where ln(LM) is the log likelihood value for the fitted model and ln(L0) is the log likelihood for the null model 
        #with only an intercept as a predictor. The measure ranges from 0 to just under 1, with values closer to zero indicating 
        #that the model has no predictive power.
        # library(pscl)
        # pR2(classifier)
    
        # ?pR2
        # WALD-test
        #A wald test is used to evaluate the statistical significance of each coefficient in the model 
        #and is calculated by taking the ratio of the square of the regression coefficient to the square of 
        #the standard error of the coefficient. The idea is to test the hypothesis that the coefficient of 
        #an independent variable in the model is significantly different from zero. If the test fails to reject 
        #the null hypothesis, this suggests that removing the variable from the model will not substantially harm the fit of that model.
        # library(survey)
        
        if(is.na(classifier$coefficients[-1][1])==F && classifier$coefficients[-1][1]!=0){r1 <- regTermTest(classifier, "days_heatwave")$p}else{r1=1}
        if(is.na(classifier$coefficients[-1][2])==F && classifier$coefficients[-1][2]!=0){r2 <- regTermTest(classifier, "days_coldwave")$p}else{r2=1}
        if(is.na(classifier$coefficients[-1][3])==F && classifier$coefficients[-1][3]!=0){r3 <- regTermTest(classifier, "days_drought")$p}else{r3=1}
        if(is.na(classifier$coefficients[-1][4])==F && classifier$coefficients[-1][4]!=0){r4 <- regTermTest(classifier, "days_flood")$p}else{r4=1}
        if(is.na(classifier$coefficients[-1][5])==F && classifier$coefficients[-1][5]!=0){r6 <- regTermTest(classifier, "exp_area_heatwaves")$p}else{r6=1}
        if(is.na(classifier$coefficients[-1][6])==F && classifier$coefficients[-1][6]!=0){r7 <- regTermTest(classifier, "exp_area_coldwaves")$p}else{r7=1}
        if(is.na(classifier$coefficients[-1][7])==F && classifier$coefficients[-1][7]!=0){r8 <- regTermTest(classifier, "exp_area_droughts")$p}else{r8=1}
        if(is.na(classifier$coefficients[-1][8])==F && classifier$coefficients[-1][8]!=0){r9 <- regTermTest(classifier, "exp_area_floods")$p}else{r9=1}
        
        # Variable Importance
        #To assess the relative importance of individual predictors in the model, we can also look at the absolute 
        #value of the t-statistic for each model parameter. This technique is utilized by the varImp function in the 
        #caret package for general and generalized linear models.
        # library(caret)
        # varImp(classifier)
        
        # print(paste(c(count, cropname, cropid, uncertainty_indicator,odds[-1],pR2(classifier)[5],r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)))#, pb,su$r.squared,su$coefficients)))
        d0 <- rbind(d0,c(count, cropname, cropid, uncertainty_indicator,odds[-1],pR2(classifier)[5],r1,r2,r3,r4,r6,r7,r8,r9))#, pb,su$r.squared,su$coefficients))
      }
    }
  }
  feats <- c("days_heatwave","days_coldwave","days_drought","days_flood","exp_area_heatwaves","exp_area_coldwaves","exp_area_droughts","exp_area_floods")
  
  names(d0) <- c("country","crop_name","crop_id","uncertainty_indicator",paste0("odds_ratio_",feats),"max_likelihood_pseudo_R2",paste0("Wald_Test_p_val_",feats))
  d0
  write.csv(d0,paste0("BCP_Phase_1_NEW_results_",reg_levels[reg_level],"_Level_90_perc.csv"))
}
####EU Level
impacts <- data.frame(read_excel(paste0("Climate exposure & impact indexes/EU Climate Extreme Impact database_28.11.2022.xlsx")))
impacts[,6:9] <- (impacts[,6:9]!=0)*1
names(impacts)[c(1,3,4)] <- c("year","cropid","country")
imp <- impacts[impacts$country=="AT",]
data <- left_join(left_join(impacts,aggdays90),allexposEU)#aggdays),allexpos)
data[is.na(data)] <- 0
d0 <- data.frame()
zero_threshhold <- 0.2
scarecity_threshhold <- 0.5
reg_levels <- c("Subnational","National","EU")
reg_level <- 3
d0 <- data.frame()
for (count in unique(data$country))
{
  # count="AT"
  for(cropid in unique(data$cropid))
  {
    # cropid="C1500"
    dat <- data[(data$cropid==cropid)+(data$country==count)==2,]
    dat
    #####lin model and p-value
    # lin <- lm(Sub.national.extremes..0.75.perc..~days_heatwave+days_coldwave+days_drought+days_flood+days_firerisk+
    #             exp_area_heatwaves+exp_area_coldwaves+exp_area_droughts+exp_area_floods+exp_area_fireweatherrisk,data=dat)
    # su <- summary(lin)
    # su$r.squared
    # f <- su$fstatistic
    # pb <- pf(f[1],f[2],f[3],lower.tail=F)
    # attributes(pb) <- NULL
    uncertainty_indicator <- dat$Regional.yield.data.coverage..uncertainty.indicator.[1]
    cropname <- dat$Eurostat_Product[1]
    
    #####logistic regression model with odds indication
    if(reg_level==3){classifier <- glm(Net.EU.single.crop.extreme..0.75.perc..~days_heatwave+days_coldwave+days_drought+days_flood+days_firerisk+
                                         exp_area_heatwaves+exp_area_coldwaves+exp_area_droughts+exp_area_floods+exp_area_fireweatherrisk,data=dat)}
    dat[dat$exp_area_heatwaves>0,]
    dat[dat$Net.EU.single.crop.extreme..0.75.perc..>0,]
    # We can calculate the multiplicative effect of a feature on the odds ratio via:
    odds <- exp(coef(classifier))
    # library(pscl)
    # library(survey)
    
    if(is.na(classifier$coefficients[-1][1])==F && classifier$coefficients[-1][1]!=0){r1 <- regTermTest(classifier, "days_heatwave")$p}else{r1=1}
    if(is.na(classifier$coefficients[-1][2])==F && classifier$coefficients[-1][2]!=0){r2 <- regTermTest(classifier, "days_coldwave")$p}else{r2=1}
    if(is.na(classifier$coefficients[-1][3])==F && classifier$coefficients[-1][3]!=0){r3 <- regTermTest(classifier, "days_drought")$p}else{r3=1}
    if(is.na(classifier$coefficients[-1][4])==F && classifier$coefficients[-1][4]!=0){r4 <- regTermTest(classifier, "days_flood")$p}else{r4=1}
    if(is.na(classifier$coefficients[-1][5])==F && classifier$coefficients[-1][5]!=0){r5 <- regTermTest(classifier, "days_firerisk")$p}else{r5=1}
    if(is.na(classifier$coefficients[-1][6])==F && classifier$coefficients[-1][6]!=0){r6 <- regTermTest(classifier, "exp_area_heatwaves")$p}else{r6=1}
    if(is.na(classifier$coefficients[-1][7])==F && classifier$coefficients[-1][7]!=0){r7 <- regTermTest(classifier, "exp_area_coldwaves")$p}else{r7=1}
    if(is.na(classifier$coefficients[-1][8])==F && classifier$coefficients[-1][8]!=0){r8 <- regTermTest(classifier, "exp_area_droughts")$p}else{r8=1}
    if(is.na(classifier$coefficients[-1][9])==F && classifier$coefficients[-1][9]!=0){r9 <- regTermTest(classifier, "exp_area_floods")$p}else{r9=1}
    if(is.na(classifier$coefficients[-1][10])==F && classifier$coefficients[-1][10]!=0){r10 <- regTermTest(classifier, "exp_area_fireweatherrisk")$p}else{r10=1}
    
    # Variable Importance
    #To assess the relative importance of individual predictors in the model, we can also look at the absolute 
    #value of the t-statistic for each model parameter. This technique is utilized by the varImp function in the 
    #caret package for general and generalized linear models.
    # library(caret)
    # varImp(classifier)
    ?pR2
    # print(paste(c(count, cropname, cropid, uncertainty_indicator,odds[-1],pR2(classifier)[5],r1,r2,r3,r4,r5,r6,r7,r8,r9,r10)))#, pb,su$r.squared,su$coefficients)))
    d0 <- rbind(d0,c(count, cropname, cropid, uncertainty_indicator,odds[-1],pR2(classifier)[5],r1,r2,r3,r4,r5,r6,r7,r8,r9,r10))#, pb,su$r.squared,su$coefficients))
  }
}
feats <- c("days_heatwave","days_coldwave","days_drought","days_flood","days_firerisk","exp_area_heatwaves","exp_area_coldwaves","exp_area_droughts","exp_area_floods","exp_area_fireweatherrisk")

names(d0) <- c("country","crop_name","crop_id","uncertainty_indicator",paste0("odds_ratio_",feats),"max_likelihood_pseudo_R2",paste0("Wald_Test_p_val_",feats))
# d0
write.csv(d0,paste0("BCP_Phase_1_results_",reg_levels[reg_level],"_Level_90_perc.csv"))




# ss <- ss[ss$Year %in% years,]
# 
# at <- ss[(ss$Eurostat_ProductCode==crops_id[ee])+(ss$Region==count)==2,]
# 
# p1 <- p[((p$Countries==count)+(p$CROP_ID==crops_id[ee])+(p$exposure_type==type))==3,]