library(readxl)
library(stringr)
####################regional exposure correlation
library(correlation)
a1 <- data.frame(read.table("rel_increases_Fabio.csv",header=T,sep=","))
row.names(a1) <- a1[,1]
a1 <- a1[,-1]
#a0 <- data.frame(read.table("nom_increases_Fabio.csv",header=T,sep=","))
#row.names(a0) <- a0[,1]
#a0 <- a0[,-1]
#a0 <- data.frame(t(a0))
years <- 1986:2019
names(a1) <- years
types <- c("heat waves","cold waves", "droughts", "floods", "fire weather risk")
extr <- paste0("Regional exposure_",types)
sheets <- excel_sheets(paste0("Climate exposure & impact indexes/",extr[1],".xlsx"))#29sheets

d0 <- data.frame()
zero_threshhold <- 0.2
scarecity_threshhold <- 0.5
for (type in types[2])
{
  # type <- "floods"
  ##################################################
  piv <- data.frame(read.table(paste0("Pivot_Regional exposure_Fabio.csv"),sep=",",header=T))
  crops_id <- c()#unique(piv$CROP_ID)
  #crops_id <- crops_id[is.na(crops_id)==F]
  crops <- unique(piv$crop)
  
  for (i in 1:length(crops)-1){
    #i=2
    crops_id <- c(crops_id,piv[piv$crop==crops[i],]$CROP_ID[1])
  }
  crops_id <- crops_id[is.na(crops_id)==F]
  counts <- unique(piv$Countries)
  for (count in counts)
  {
    # count <- "AT"
    print(count)
    for (ee in 1:length(crops_id))
    {
      # ee=1
      #crop <- "C1100" 
      print(crops_id[ee])
      i1 <- piv$CROP_ID==crops_id[ee]
      i2 <- piv$Countries==count
      i3 <- piv$exposure_type==type
      at1 <- piv[(i1+i2+i3)==3,] 
      at1 <- at1[is.na(at1$NUTS_ID)==F,]
      p <- piv[,c(-1,-3,-4,-5,-7:-12,-47:-51,-53)]
      ##################################################
      re <- data.frame(read_excel(paste0("Climate exposure & impact indexes/",extr[which(types==type)],".xlsx"),sheet = sheets[ee]))
      re <- re[is.na(re$NUTS_ID)==F,]
      row.names(re) <- re$NUTS_ID
      re <- re[,c(-1:-11,-46:-54)]
      names(re) <- years
      a2 <- data.frame(t(rbind(re,a1)))
      ####################deviation from 5 year rolling avg = supply shock
      ss <- data.frame(read.table(paste0("Climate exposure & impact indexes/EU Climate Extreme Impact database_04.07.2022_fix.csv"),sep=",",header=T))
      ss <- ss[ss$Year %in% years,]
      
      at <- ss[(ss$Eurostat_ProductCode==crops_id[ee])+(ss$Region==count)==2,]
      #unique(at$Year)
      # plot(1986:2019,at$Sub.national.extremes..0.75.perc../1000, type = "l",ylim = c(-1,1))#rel increases timeline for first activity
      # lines(1986:2019,a2$DE_p115, type = "l", col="red")
      if(nrow(at)>0)
      {
        att <- data.frame(t(at))
        
        names(att) <- att[1,]
        att <- att[-1,]
        
        #add missings years to att
        years_missing <- years[(years %in% names(att))==F]
        if(length(years_missing)>0)
        {
          att_missing <- data.frame()
          for(ye in years_missing)
          {
            new_row <- c(att[1,1],att[2,1],att[3,1],0,att[5,1],0,0,0)
            att_missing <- rbind(att_missing,new_row)  
          }
          att_missing <- data.frame(t(att_missing))
          names(att_missing) <- years_missing
          att <- cbind(att,att_missing)
        }
        p1 <- p[((p$Countries==count)+(p$CROP_ID==crops_id[ee])+(p$exposure_type==type))==3,]
        p2 <- p1[(is.na(p1$NUTS_ID)==F),]
        
        row.names(p2) <- p2$NUTS_ID
        p2 <- p2[,-1]
        p2 <- p2[,c(-1,-36,-37,-38)]
        names(p2) <- years
        
        #exposure aggregated or regional boolean in pbool
        expos <- data.frame(t(colSums(p2)))
        names(expos) <- years
        row.names(expos) <- "exposure area"
        pbool <- (p2>0)*1
        ####################bring all features together in one matrix
        
        ##regional boolean
        ab <- data.frame(t(rbind(att,pbool)))
        min_coverage <- min(as.numeric(ab$Regional.yield.data.coverage..uncertainty.indicator.))
        if(min_coverage>=scarecity_threshhold)
        {
          ab$exposure.type. <- type
          ##aggregates exposure
          aa <- data.frame(t(rbind(att,expos)))
          aa$exposure.type <- type
          acts <- data.frame(t(a1[grepl(count,row.names(a1)),]))
          #######################linear regression model
          for (act_ind in 1:ncol(acts))
          {
            #act_ind=20
            aact <- acts[,act_ind]
            activity <- names(acts)[act_ind]
            print(activity)
            if(sum(aact!=0)>0)
            {
              ##aggregates exposure
              # aa$activity <- aact 
              # aa$Sub.national.extremes..0.75.perc.. <- as.numeric(aa$Sub.national.extremes..0.75.perc..)
              # aa$exposure.area <- as.numeric(aa$exposure.area)
              # aa1 <- aa[,c(-1:-3,-10)]
              # for (i in 1:ncol(aa1)){aa1[,i] <- as.numeric(aa1[,i])}
              # #lin model and p-value
              # lin <- lm(activity~.,data=aa1)
              # su <- summary(lin)
              # f <- su$fstatistic
              # pe <- pf(f[1],f[2],f[3],lower.tail=F)
              # attributes(pe) <- NULL
              
              ##regional boolean
              ab$activity <-aact 
              ab$Sub.national.extremes..0.75.perc.. <- as.numeric(ab$Sub.national.extremes..0.75.perc..)
              
              #i1 <- which(names(ab)=="EU.Multi.crop.extreme..0.75.perc..")
              #i2 <- which(names(ab)=="exposure.type.")
              #for (i in ((i1+1):(i2-1))){ab[,i] <- as.integer(ab[,i])}
              ab1 <- ab[,c(-1:-3,-ncol(ab)+1)]
              for (i in 1:ncol(ab1)){ab1[,i] <- as.numeric(ab1[,i])}
              zero_counter <- sum(ab1$activity==0)/length(ab1$activity)
              
              
              if(zero_counter<=zero_threshhold)
              {
                #lin model and p-value
                lin <- lm(activity~.,data=ab1)
                su <- summary(lin)
                su$r.squared
                f <- su$fstatistic
                pb <- pf(f[1],f[2],f[3],lower.tail=F)
                attributes(pb) <- NULL
                
                print(paste(count,crops_id[ee],crops[ee], type, min_coverage, activity, pb,su$r.squared,zero_counter))
                d0 <- rbind(d0,c(count,crops_id[ee],crops[ee], type,min_coverage ,activity, pb,su$r.squared,zero_counter))#,pe))
              }
            }else{print("sum is Zero")}
          }
        }
      }
    }
  }
}
names(d0) <- c("country","crop","crop_id", "type", "Scarecity", "activity", "pb","r.squared","zero_counter")
d0
write.csv(d0,"lin_mod_cold waves.csv")

head(d0[d0$pb<0.01,])

type <- "droughts"
count <- "FR"
ee <- 11

##################################################
piv <- data.frame(read.table(paste0("Pivot_Regional exposure_Fabio.csv"),sep=",",header=T))
crops_id <- c()#unique(piv$CROP_ID)
#crops_id <- crops_id[is.na(crops_id)==F]
crops <- unique(piv$crop)

for (i in 1:length(crops)-1){
  #i=2
  crops_id <- c(crops_id,piv[piv$crop==crops[i],]$CROP_ID[1])
}
crops_id <- crops_id[is.na(crops_id)==F]

counts <- unique(piv$Countries)
i1 <- piv$CROP_ID==crops_id[ee]
i2 <- piv$Countries==count
i3 <- piv$exposure_type==type
at1 <- piv[(i1+i2+i3)==3,] 
at1 <- at1[is.na(at1$NUTS_ID)==F,]
p <- piv[,c(-1,-3,-4,-5,-7:-12,-47:-51,-53)]
##################################################
re <- data.frame(read_excel(paste0("Climate exposure & impact indexes/",extr[which(types==type)],".xlsx"),sheet = sheets[ee]))
re <- re[is.na(re$NUTS_ID)==F,]
row.names(re) <- re$NUTS_ID
re <- re[,c(-1:-11,-46:-54)]
names(re) <- years
a2 <- data.frame(t(rbind(re,a1)))
####################deviation from 5 year rolling avg = supply shock
ss <- data.frame(read.table(paste0("Climate exposure & impact indexes/EU Climate Extreme Impact database_04.07.2022_fix.csv"),sep=",",header=T))
ss <- ss[ss$Year %in% years,]

at <- ss[(ss$Eurostat_ProductCode==crops_id[ee])+(ss$Region==count)==2,]

att <- data.frame(t(at))

names(att) <- att[1,]
att <- att[-1,]

p1 <- p[((p$Countries==count)+(p$CROP_ID==crops_id[ee])+(p$exposure_type==type))==3,]
p2 <- p1[(is.na(p1$NUTS_ID)==F),]

row.names(p2) <- p2$NUTS_ID
p2 <- p2[,-1]
p2 <- p2[,c(-1,-36,-37,-38)]
names(p2) <- years

#exposure aggregated or regional boolean in pbool
expos <- data.frame(t(colSums(p2)))
names(expos) <- years
row.names(expos) <- "exposure area"
pbool <- (p2>0)*1
####################bring all features together in one matrix

##regional boolean
ab <- data.frame(t(rbind(att,pbool)))
ab$exposure.type. <- type

##aggregates exposure
aa <- data.frame(t(rbind(att,expos)))
aa$exposure.type <- type
acts <- data.frame(t(a1[grepl(count,row.names(a1)),]))

act_ind=4
aact <- acts[,act_ind]
activity <- names(acts)[act_ind]
activity

##regional boolean
ab$activity <-aact 
ab$Sub.national.extremes..0.75.perc.. <- as.numeric(ab$Sub.national.extremes..0.75.perc..)

#i1 <- which(names(ab)=="EU.Multi.crop.extreme..0.75.perc..")
#i2 <- which(names(ab)=="exposure.type.")
#for (i in ((i1+1):(i2-1))){ab[,i] <- as.integer(ab[,i])}
ab1 <- ab[,c(-1:-3,-ncol(ab)+1)]
for (i in 1:ncol(ab1)){ab1[,i] <- as.numeric(ab1[,i])}
#lin model and p-value
lin <- lm(activity~.,data=ab1)
su <- summary(lin)
f <- su$fstatistic
pb <- pf(f[1],f[2],f[3],lower.tail=F)
attributes(pb) <- NULL
pb
summary(lin)

head(d0[d0$pb<0.01,])#unique(at$Year)

activity
plot(1986:2019,ab1$FRC, type = "l",ylim = c(-1,1))#rel increases timeline for first activity
lines(1986:2019,a2$FR_p004, type = "l", col="red")

d00 <- read.table("lin_mod_floods.csv", sep=",",header=T)[,-1]
# count <- "BE"
rel <- data.frame()
for (count in counts)
{
d01 <- d00[((d00$country==count) +(d00$crop=="C1100")+(d00$pb<0.01)+(d00$r.squared>0.04)+(d00$Scarecity>=0.5)+(d00$zero_counter<0.2))==6,]
rel <- rbind(rel,d01)
print(c(count,(nrow(d01))))
}
min(rel$zero_counter)
rel

d00 <- read.table("lin_mod_floods_0.5_0.01_0.6_0.2.csv", sep=";",header=T)
d00 <- read.table("lin_mod_droughts_0.5_0.01_0.6_0.2.csv", sep=";",header=T)
# count <- "BE"
rel <- data.frame()
for (crop in crops)
{
  d01 <- d00[((d00$crop==crops)),]#+(d00$pb<0.01)+(d00$r.squared>0.04))==3,]
  rel <- rbind(rel,d01)
  print(c(crop,(nrow(d01))))
}
max(as.numeric(d00$r.squared))
rel







### Import libraries
library(randomForest)
library(ggplot2)

set.seed(4543)
rf.fit <- randomForest(activity~., data=ab1, ntree=100,
                       keep.forest=FALSE, importance=TRUE)
rf.fit

### Visualize variable importance ----------------------------------------------

# Get variable importance from the model fit
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)
?importance
ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
