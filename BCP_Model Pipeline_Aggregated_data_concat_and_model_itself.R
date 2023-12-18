library(readxl)
library(stringr)
##################################################################################################
#Use filtering:
##################################################################################################
#1) create aggregation file:
lf <- list.files("BCP_Fabio/");lf <- lf[grep("bin",lf)]

#1.1) get all processes
procs <- c()
for(ft in lf)
{
  f <- data.frame(read.table(paste0("BCP_Fabio/",ft),sep=",", header=T))[,-1]
  procs <- c(procs,names(f))
}
procs <- unique(procs)
procs
#1.2) save for each process all available commodities in one df
df <- data.frame(rows=1:1000)#rowlength should be 1000
for(proc in procs)
{
  print(paste0(proc," which is ",which(proc==procs)," of ",length(procs)))
  fcol <- c()
  for(ft in lf)
  {
    f <- data.frame(read.table(paste0("BCP_Fabio/",ft),sep=",", header=T))[,-1]
    f1 <- f[,names(f)==proc]
    fcol <- unique(c(fcol,f1[nchar(f1)>1]))
  }
  fcol <- c(fcol,rep("X",1000-length(fcol)))
  df <- cbind(df,fcol)
  names(df)[ncol(df)] <- proc
}
head(df[,1:10])
write.table(df,paste0("BCP_Fabio/BIN_ALL.csv"))

bin <- data.frame(read.table(paste0("BCP_Fabio/BIN_ALL.csv"),sep=",", header=T))[,-1]
j=0
for(i in nrow(bin):1)
{
  if (j==0 && length(unique(bin[i,]))>1)
  {
    bin <- bin[1:(i),]
    j=1
  }
}
write.table(df,paste0("BCP_Fabio/BIN_ALL.csv"))
print("transformation 1 is finished")
            
##################################################################################################
#2)Data transformation:
##################################################################################################
#2.1) transform BIN_ALL from Fabio codes to Eurostat codes
bin <- data.frame(read.table(paste0("BCP_Fabio/BIN_ALL.csv"),sep=",", header=T))[,-1]
head(bin[,1:10])

conc_com <- data.frame(read_excel("Concordance table_BCP_RCP scenarios_EUROSTAT adapted.xlsx",sheet = 6))[,c(2,4)]
names(conc_com)[3] <- "CROP_ID"
head(conc_com)
bin1 <- bin
for (i in 1:ncol(bin))
{
  bin_col_temp <- left_join(bin[,1],conc_com)
  bin1[,1] <- bin_col_temp                          
}
write.table(bin1,paste0("BCP_Fabio/BIN_ALL_EUROSTAT.csv"))

bin <- data.frame(read.table(paste0("BCP_Fabio/BIN_ALL_EUROSTAT.csv"),sep=",", header=T))[,-1]
j=0
for(i in nrow(bin):1)
{
  if (j==0 && length(unique(bin[i,]))>1)
  {
    bin <- bin[1:(i),]
    j=1
  }
}
write.table(df,paste0("BCP_Fabio/BIN_ALL_EUROSTAT.csv"))
print("eurostat finished")
##################################################################################################
#3)Data aggregation:
##################################################################################################
bin <- data.frame(read.table(paste0("BCP_Fabio/BIN_ALL_EUROSTAT.csv"),sep=",", header=T))[,-1]
head(bin[,1:10])
types <- c("heat waves","cold waves", "droughts", "floods", "fire weather risk")
years <- 1986:2019

lf <- list.files("lin_mods/")
ls <- strsplit(lf,"_")
types <- unique(sapply(1:length(ls),function(x){ls[[x]][1]}))
counts <- unique(sapply(1:length(ls),function(x){ls[[x]][2]}))
crops <- unique(sapply(1:length(ls),function(x){ls[[x]][3]}))
acts <- unique(sapply(1:length(ls),function(x){paste0(ls[[x]][4],"_",ls[[x]][5])}))
df <- data.frame("types"=sapply(1:length(ls),function(x){ls[[x]][1]}),"counts"=sapply(1:length(ls),function(x){ls[[x]][2]}),"crops"=sapply(1:length(ls),function(x){ls[[x]][3]}),
                 "acts"=paste0(sapply(1:length(ls),function(x){ls[[x]][4]}),"_",sapply(1:length(ls),function(x){ls[[x]][5]})))
head(df)
type=types[1]
count=counts[1]
crop=crops[1]
act=acts[1]

for (count in counts)
{
  print(paste0(count," which is ",which(count==counts)," of ",length(counts)))
  
  for (crop in crops)
  {
    for(act in acts)
    {
      #get bin column an check if EUROSTAT CODE is contained in it
      if(crop %in% unique(bin[,names(bin)==act]))
      {
        #load for all data the columns, rename them and add them together
        filenames_temp <- lf[((count==df$counts)+(crop==df$crops)+(act==df$acts))==3]
        f <- data.frame()
        for(ft in filenames_temp)
        {
           # ft <- filenames_temp[1]
           type_ind <- which(ft==filenames_temp)
           ft1 <- data.frame(read.table(paste0("lin_mods/",ft),sep=",", header=T))
           head(ft1)
           if(ft == filenames_temp[1])
           {
             ft1 <- ft1[,-c(1,ncol(ft1),ncol(ft1)-2,ncol(ft1)-3,ncol(ft1)-5,ncol(ft1)-6,ncol(ft1)-7,ncol(ft1)-8,ncol(ft1)-9)]
             names(ft1)[c(6:ncol(ft1)-3,ncol(ft1)-1,ncol(ft1))] <- paste0(types[type_ind],"_",names(ft1))[c(6:ncol(ft1)-3,ncol(ft1)-1,ncol(ft1))]
           }else{
             ft1 <- ft1[,-c(1,2,3,4,5,6,ncol(ft1),ncol(ft1)-2,ncol(ft1)-3,ncol(ft1)-5,ncol(ft1)-6,ncol(ft1)-7,ncol(ft1)-8,ncol(ft1)-9,ncol(ft1)-10)]
             names(ft1) <- paste0(types[type_ind],"_",names(ft1))
           }
           head(ft1)
           f <-rbind(f,t(ft1)) 
        }
        f <- data.frame(t(f))
        head(f)
        write.csv(f,paste0("lin_mods_aggregated_files/",substr(filenames_temp[1],nchar(types[1])+2,nchar(filenames_temp[1]))))
      }
    }
  }
}
print("aggregation is finished")

##SUPPLY ANALYSIS:load data and create only models for the national activities
lf <- list.files("lin_mods_aggregated_files/")
ls <- strsplit(lf,"_")
counts <- unique(sapply(1:length(ls),function(x){ls[[x]][1]}))
crops <- unique(sapply(1:length(ls),function(x){ls[[x]][2]}))
acts <- unique(sapply(1:length(ls),function(x){paste0(ls[[x]][3],"_",ls[[x]][4])}))
df <- data.frame("counts"=sapply(1:length(ls),function(x){ls[[x]][1]}),"crops"=sapply(1:length(ls),function(x){ls[[x]][2]}),
                 "acts"=paste0(sapply(1:length(ls),function(x){ls[[x]][3]}),"_",sapply(1:length(ls),function(x){ls[[x]][4]})))
head(df)
type=types[1]
count=counts[1]
crop=crops[1]
act=acts[1]
zero_threshhold <- 1
scarecity_threshhold <- 0
sdf <- data.frame()
for(ft in lf)
{
  # ft=lf[1]
  ft1 <- data.frame(read.table(paste0("lin_mods_aggregated_files/",ft),sep=",", header=T))[,-1]
  if(grep(count,act))
  {
    #build linear model and save summary in data frame sdf
    min_coverage <- min(as.numeric(ft1$Regional.yield.data.coverage..uncertainty.indicator.))
    zero_counter <- sum(ft1$activity==0)/length(ft1$activity)
    
    if(zero_counter<=zero_threshhold && min_coverage>=scarecity_threshhold)
    {
      #lin model and p-value
      lin <- lm(activity~.,data=ft1)
      su <- summary(lin)
      su$r.squared
      #get also other parameters and their significance in the future
      f <- su$fstatistic
      pb <- pf(f[1],f[2],f[3],lower.tail=F)
      attributes(pb) <- NULL
      sdf <- rbind(ft, pb, su$r.squared,zero_counter,min_coverage )
    }
  }
}
write.csv(sdf,paste0("lin_mods_aggregated_files/linear_models.csv"))

# f <- data.frame(read.table(paste0("lin_mods_aggregated_files/",ft),sep=",", header=T))


