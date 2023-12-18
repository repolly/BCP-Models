library(reshape2)
library(readxl)
library(stringr)
library(tidyverse)
library(caret)
library(survey)
library(pscl)

years <- 1997:2017
year=1997
crops <- c("Wheat","Potato","Rapeseed","Grn maize","All Crops")
cropids <- c("C1100","R1000","1400","G3000","X9999")
#syn only important for the EU CE years
# syn <- data.frame(read_excel(paste0("Climate exposure & impact indexes/Climate extreme shocks_selected crops_regions_synthesis report.xlsx"),sheet = 1))
# names(syn)[7:46] <- 1981:2020
# colinds <- which(syn[nrow(syn),] %in% 1:5)
colinds <- c(2001,2009,2008,2006,2013)
allrange <- min(colinds):max(colinds)
incolinds <- years[years%in%colinds==F]#allrange[allrange%in%colinds==F]
conc <- data.frame(read_excel(paste0("Climate exposure & impact indexes/Concordance tables FABIO&FORBIO&EUROSTAT.xlsx"),sheet = 2))
conc1 <- data.frame(read_excel(paste0("Climate exposure & impact indexes/Concordance tables FABIO&FORBIO&EUROSTAT.xlsx"),sheet = 4))
conc <- left_join(conc[-126:-128,c(1,2)],conc1[,c(1,5)])[1:62,]

####################row sums of activity supplying products
for (year in years)
{
  if(year %in% colinds || year %in% incolinds)
  {
    # print(paste0("/BCP_Fabio/use_",year,".csv -> ",crops[j]))
    a <- data.frame(read.table(paste0("BCP_Forbio/use_",year,".csv"),sep=","))
    names(a) <- a[1,]
    row.names(a) <- a[,1]
    a <- a[-1,]
    a <- a[,-1]
    for (i in 1:ncol(a))
    {a[,i] <- as.numeric(a[,i])}
    if(year==min(colinds))
    {
      aa <- a
      print(paste0("set a",year))
      write.csv(a,paste0("avg_CE_use_tables_",year,".csv"))
    }
    if(year==incolinds[1])
    {
      bb <- a
      print(paste0("set b",year))
    }
    if(year %in% colinds[-1])
    {
      aa <- aa+a
      print(paste0("add a",year))
      write.csv(a,paste0("avg_CE_use_tables_",year,".csv"))
    }
    if(year %in% incolinds[-1])
    {
      bb <- bb+a
      print(paste0("add b",year))
    }
  }
}
nams <- row.names(aa)
cropid <- cropids[1]
cc <- conc[conc$Eurostat_ProductCode==cropid,1]
coms <- as.character(sapply(1:length(cc),function(x) nams[grepl(cc[x],nams)]))
aa1 <- aa/length(colinds)
bb1 <- bb/length(incolinds)
cc1 <- aa1-bb1
write.csv(aa1,paste0("FORBIO_avg_CE_use_tables.csv"))
write.csv(bb1,paste0("FORBIO_avg_NOT_CE_use_tables.csv"))
write.csv(cc1,paste0("FORBIO_avg_differences_use_tables.csv"))

#need matrix: (1) country of origin, (2) commodity (from the country of origin), (3) country of destination, (4) activity in the country of destination, (5) value
cc1$Origin <- row.names(cc1)
cc1m <- melt(data.frame(cc1), na.rm= TRUE, id.vars="Origin",
             value.name="Value", variable.name="Destination" )
#need matrix: (1) country of origin, (2) commodity (from the country of origin), (3) country of destination, (4) activity in the country of destination, (5) value
# cc1m$country_of_origin <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Origin,"_")[[x]][1])
# cc1m$commodity <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Origin,"_")[[x]][2])
# cc1m$country_of_destination <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Destination,"_")[[x]][1])
# cc1m$activity <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Destination,"_")[[x]][2])
# f <- cc1m[,c(4,5,6,7,8,3)]
write.csv(cc1m,paste0("FORBIO_avg_differences_use_tables_flat_version.csv"))

# #filter for different crops in the huge matrix
# for(j in 1:5)
# {
#   nams <- row.names(aa)
#   cropid <- cropids[1]
#   cc <- conc[conc$Eurostat_ProductCode==cropid,1]
#   coms <- as.character(sapply(1:length(cc),function(x) nams[grepl(cc[x],nams)]))
#   
#   # aa1 <- aa[nams %in% coms,]
#   # aa1 <- aa1[,colSums(aa1)!=0]
#   # bb1 <- bb[nams %in% coms,]
#   # bb1 <- bb1[,colSums(bb1)!=0]
#   # 
#   if(sum((row.names(aa1)==row.names(bb1))==F)==0)
#   {
#     aa1 <- aa1/length(colinds)
#     bb1 <- bb1/length(incolinds)
#     cc1 <- aa1-bb1
#     write.csv(aa1,paste0("avg_CE_use_tables_",crops[j],".csv"))
#     write.csv(bb1,paste0("avg_NOT_CE_use_tables_",crops[j],".csv"))
#     write.csv(cc1,paste0("avg_differences_use_tables_",crops[j],".csv"))
#   }else{print("have different rownames")}
# }
############################################################### FINAL USE:
for (year in years)
{
  if(year %in% colinds || year %in% incolinds)
  {
    # print(paste0("/BCP_Fabio/Y_",year,".csv -> ",crops[j]))
    a <- data.frame(read.table(paste0("BCP_Forbio/Y_",year,".csv"),sep=","))
    names(a) <- a[1,]
    row.names(a) <- a[,1]
    a <- a[-1,]
    a <- a[,-1]
    for (i in 1:ncol(a))
    {a[,i] <- as.numeric(a[,i])}
    if(year==min(colinds))
    {
      aa <- a
      print("set a")
      write.csv(a,paste0("avg_CE_Y_tables_",year,".csv"))
    }
    if(year==incolinds[1])
    {
      bb <- a
      print("set b")
    }
    if(year %in% colinds[-1])
    {
      aa <- aa+a
      print("add a")
      write.csv(a,paste0("avg_CE_Y_tables_",year,".csv"))
    }
    if(year %in% incolinds[-1])
    {
      bb <- bb+a
      print("add b")
    }
  }
}
aa1 <- aa/length(colinds)
bb1 <- bb/length(incolinds)
cc1 <- aa1-bb1
write.csv(aa1,paste0("Forbio_avg_CE_Y_tables.csv"))
write.csv(bb1,paste0("Forbio_avg_NOT_CE_Y_tables.csv"))
write.csv(cc1,paste0("Forbio_avg_differences_Y_tables.csv"))

cc1$Origin <- row.names(cc1)
cc1m <- melt(data.frame(cc1), na.rm= TRUE, id.vars="Origin",
                   value.name="Value", variable.name="Destination" )
#need matrix: (1) country of origin, (2) commodity (from the country of origin), (3) country of destination, (4) activity in the country of destination, (5) value
# cc1m$country_of_origin <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Origin,"_")[[x]][1])
# cc1m$commodity <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Origin,"_")[[x]][2])
# cc1m$country_of_destination <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Destination,"_")[[x]][1])
# cc1m$activity <- sapply(1:nrow(cc1m),function(x) str_split(cc1m$Destination,"_")[[x]][2])
# f <- cc1m[,c(4,5,6,7,8,3)]
write.csv(cc1m,paste0("Forbio_avg_differences_Y_tables_flat_version.csv"))

# #filter for different crops in the huge matrix
# for(j in 1:5)
# {
#   nams <- row.names(aa)
#   cropid <- cropids[1]
#   cc <- conc[conc$Eurostat_ProductCode==cropid,1]
#   coms <- as.character(sapply(1:length(cc),function(x) nams[grepl(cc[x],nams)]))
#   
#   aa1 <- aa[nams %in% coms,]
#   # aa1 <- aa1[,colSums(aa1)!=0]
#   bb1 <- bb[nams %in% coms,]
#   # bb1 <- bb1[,colSums(bb1)!=0]
#   # 
#   if(sum((row.names(aa1)==row.names(bb1))==F)==0)
#   {
#     aa1 <- aa1/length(colinds)
#     bb1 <- bb1/length(incolinds)
#     cc1 <- aa1-bb1
#     write.csv(aa1,paste0("avg_CE_Y_tables_",crops[j],".csv"))
#     write.csv(bb1,paste0("avg_NOT_CE_Y_tables_",crops[j],".csv"))
#     write.csv(cc1,paste0("avg_differences_Y_tables_",crops[j],".csv"))
#   }else{print("have different rownames")}
# }
 

# flat format for avg
# forbio
# take crop regiona combination -> look for most extreme years (EU years) and look if distr changes -> look in climate extreme impact matrix 5 most extreme

