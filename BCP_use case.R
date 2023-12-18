library(readxl)
library(writexl)
library(stringr)
####################regional exposure correlation
library(correlation)
a1 <- data.frame(read.table("rel_increases_Fabio.csv",header=T,sep=","))
row.names(a1) <- a1[,1]
a1 <- a1[,-1]

years <- 1986:2019
names(a1) <- years
types <- c("heat waves","cold waves", "droughts", "floods", "fire weather risk")
extr <- paste0("Regional exposure_",types)
sheets <- excel_sheets(paste0("Climate exposure & impact indexes/",extr[1],".xlsx"))#29sheets
year <- 1997
getwd()
bin_matrix <- function(year)
{
  year=1987
  n <- paste0("G:/Meine Ablage/Analysis supply chain shocks FABIO_FORBIO/BCP_Fabio/use_",year,".csv")
  print(n)
  a <- data.frame(read.table(n,sep=","))
  names(a) <- a[1,]
  row.names(a) <- a[,1]
  a <- a[-1,]
  a <- a[,-1]
  a <- data.frame((a>0)*1)
  
  # row.names(a)[a$DE_c023==1]
  b <- list()
  for (i in 1:ncol(a))
  {
    if(nchar(names(a)[i])<8)# we only look at country names which only have 2 letters at the beginning, e.g. LU_... and not ROW_...
    {
      namestoadd <- row.names(a)[which(a[,i]>0)]
      if(length(namestoadd)>0)
      {
        namestoadd <- namestoadd[nchar(namestoadd)==7]
        b <- c(b,list(c(names(a)[i],namestoadd)))
      }
    }
  }
  lens <- sapply(1:length(b),function(x) length(b[[x]]))
  maxlen <- max(sapply(1:length(b),function(x) length(b[[x]])))
  blen <- length(b)
  d <- data.frame()
  for (i in 1:blen)
  {
    tofill <- maxlen-length(b[[i]])
    d <- rbind(d,c(b[[i]],rep("X",tofill)))
  }
  d <- data.frame(t(d))
  names(d) <- d[1,]
  d <- d[-1,]
  row.names(d) <- 1:nrow(d)
  write.csv(d,paste("BCP_Fabio/bin_",year,".csv"))
}
for (y in years)
{
  bin_matrix(y)
}
# 
# 
# d00 <- read.table("lin_mod_floods_0.5_0.01_0.6_0.2.csv", sep=";",header=T)
# d00 <- read.table("lin_mod_droughts_0.5_0.01_0.6_0.2.csv", sep=";",header=T)
# # count <- "BE"
# rel <- data.frame()
# for (crop in crops)
# {
#   d01 <- d00[((d00$crop==crops)),]#+(d00$pb<0.01)+(d00$r.squared>0.04))==3,]
#   rel <- rbind(rel,d01)
#   print(c(crop,(nrow(d01))))
# }
# max(as.numeric(d00$r.squared))
# rel
# 
# 
# 
# 
# 
# 
# 
# ### Import libraries
# library(randomForest)
# library(ggplot2)
# 
# set.seed(4543)
# rf.fit <- randomForest(activity~., data=ab1, ntree=100,
#                        keep.forest=FALSE, importance=TRUE)
# rf.fit
# 
# ### Visualize variable importance ----------------------------------------------
# 
# # Get variable importance from the model fit
# ImpData <- as.data.frame(importance(rf.fit))
# ImpData$Var.Names <- row.names(ImpData)
# ?importance
# ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
#   geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
#   geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   theme(
#     legend.position="bottom",
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank()
#   )


library(readxl)
library(stringr)
library(tidyr)
library(dplyr)
years <- 1986:2019
year=1997

#load tabels and create a merging table
a <- data.frame(read_excel("EU Climate Extreme Impact database_analysis_Liesbeth.xlsx",sheet = 2))
a[,c(3:ncol(a))] <- 0
b <- data.frame(read_excel("EU Climate Extreme Impact database_analysis_Liesbeth.xlsx",sheet = 1))
c1 <- data.frame(read_excel("Concordance tables FABIO&BCP_FABIO&EUROSTAT.xlsx",sheet = 2))
c2 <- data.frame(read_excel("Concordance tables FABIO&BCP_FABIO&EUROSTAT.xlsx",sheet = 4))[,c(1,5)]
c3 <- left_join(c1,c2)
c3 <- c3[is.na(c3$Eurostat_ProductCode)==F,c(1,8)]

b <- left_join(b,c3)[,-6]#now we know which commodity has which eu-code 
b <- b[((b$Year>=1986)+(b$Year<2020))==2,]


i=1
j=2

for (i in 1:length(unique(a$...1)))#go through all countries
{
  for (j in 1:length(c3$com_code))#go through all com codes
  {
    cc <- c3$com_code[j]
    country <- unique(a$...1)[i]
    cc_ind <- grep(cc,a$...2)
    count_ind <- grep(country,a$...2)
    row_ind <- intersect(cc_ind,count_ind)#index of the row we want to fill with 1s
    
    #get data from table b an check if we can fill table a with a 1
    i1 <- (b$com_code==cc)*1
    i2 <- (b$Region==country)*1
    b1 <- b[i1+i2==2,]
    b1 <- b1[is.na(b1$Year)==F,]#now we have all years for the com_code and the country
    
    #run through all 3 regions in b table
    for (k in 1:3)
    {
      # k=1
      yearstofill <- which(b1[,4+k]!=0)#check which years we have to fill
      if(length(yearstofill)>0)
      {
        for(y in b1$Year[yearstofill])
        {
          # y=1986
          col_ind <- grep(y,names(a))[k]#get the correct column we need to fill with a 1
          a[row_ind,col_ind] <- 1
          print(c(row_ind, country,cc,col_ind))
          
        }
      }
    }
  }    
}
a
write.csv(a,"final table.csv")
