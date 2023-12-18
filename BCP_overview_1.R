library(readxl)
library(stringr)

# years <- 1986:2019
# year=1997
# 
# ####################row sums of activity supplying products
# aggr_acts <- function(year)
# {
# print(paste0("/BCP_Fabio/sup_",year,".csv"))
# a <- data.frame(read.table(paste0("BCP_Fabio/sup_",year,".csv"),sep=","))
# names(a) <- a[1,]
# row.names(a) <- a[,1]
# a <- a[-1,]
# a <- a[,-1]
# for (i in 1:ncol(a))
# {a[,i] <- as.numeric(a[,i])}
# aa <- rowSums(a)
# return(aa)
# }
# 
# relative_increases <- function()
# {
#   d <- data.frame()
#   ns <- names(aggr_acts(1998))
#   for (y in years)
#   {
#     # y=2016
#     if (y<2019)
#     {
#       curr_year <- aggr_acts(y)
#       next_year <- aggr_acts(y+1)
#       new_col <- (next_year-curr_year)/curr_year
#       new_col[curr_year==0] <- 0
#       new_col[((curr_year==0) + (next_year>0))==2] <- 1
#       d <- rbind(d,new_col)#relative increases
#     }
#   }
# 
#   names(d) <- ns
#   d <- data.frame(t(d))
#   d <- cbind(0,d)
#   names(d) <- years
#   d[is.na(d)] <- 0
#   d
# }
# a1 <-relative_increases()
# a1
# write.csv(a1,paste0("rel_increases_Fabio.csv"))
# 
# 
# ####################weather extremes percentile timelines
# sheets <- 1:5
# #extr <- c("heat", "cold", "drought", "wet", "fire risk")
# 
# getweatherextreme90matrix <- function(sheet)
# {
# w <- data.frame(read_excel("Climate exposure & impact indexes/Weather extremes_90%_regions.xlsx",sheet = sheet))
# start <- which((w[,1]=="NUTS_ID")==T)-1
# end <- which((w[,1]=="SUM")==T)-1
# w <- w[start:end,]
# ind1 <- which(names(w)=="X2020")+2
# ind2 <- ind1+3
# names(w)[ind1:ind2] <- w[1,ind1:ind2]
# w <- w[-1,]
# w <- w[,-c(ind1-1,ind2+1:ncol(w))]
# return(w)
# }
# getweatherextreme90matrix(3)

####################regional exposure correlation
library(correlation)
a1 <- data.frame(read.table("rel_increases_Fabio.csv",header=T,sep=","))
row.names(a1) <- a1[,1]
a1 <- a1[,-1]
years <- 1986:2019
names(a1) <- years
types <- c("heat waves","cold waves", "droughts", "floods", "fire weather risk")
# extr <- paste0("Regional exposure_",types)
# sheets <- excel_sheets(paste0("Climate exposure & impact indexes/",extr[ee],".xlsx"))#29sheets

# dorig <- data.frame()
# for (ee in 1:length(types))
# {
#   for (s in 1:(length(sheets)-1))
#   {
#     re <- data.frame(read_excel(paste0("Climate exposure & impact indexes/",extr[ee],".xlsx"),sheet = sheets[s]))
#     re <- re[is.na(re$NUTS_ID)==F,]
#     #change matrix to regional shares
#     re$Countries <- substring(re$NUTS_ID,1,2)
#     un_counts <- unique(re$Countries)
#     for(u in un_counts){
#       # u <- "DE"
#       du <- re[re$Countries==u,] 
#       du$Distribution.of.crop.supply.among.Country.regions. <- du$Average.crop.supply.2001.2020.per.NUTS.1.region..1000.tonnes./sum(du$Average.crop.supply.2001.2020.per.NUTS.1.region..1000.tonnes.)
#       
#       #replace values in matrix
#       col_ind <- which(grepl("X",names(du)))
#       for(co in col_ind){
#         for(ro in 1:nrow(du)){
#           if(du[ro,co]>0){
#             du[ro,co] <- du$Distribution.of.crop.supply.among.Country.regions.[ro]
#           } 
#         }
#       }
#       du$exposure_type <- types[ee] 
#       du$crop <- sheets[s]
#       dorig <- rbind(dorig,du)
#     }
#   }  
# }    
# write.csv(dorig,paste0("Pivot_Regional exposure_Fabio.csv"))
re <- data.frame(read.table("Pivot_Regional exposure_Fabio.csv",header=T,sep=","))[,-1]

df <- data.frame()
un_counts <- unique(re$Countries)
for (u in un_counts){
  # u="AT"
  du <- re[re$Countries==u,]
  for (ee in types){
    # ee="heat waves"
    de <- du[du$exposure_type==ee,]
    for (cc in unique(de$crop)){
      # cc="C1100 Wheat & spelt"
      dc <- de[de$crop==cc,]
      
      #filter for year columns and create correlation data
      row.names(dc) <- dc$NUTS_ID
      dc <- dc[,c(-1:-11,-46:-54)]
      names(dc) <- 1986:2019
      
      #restrict a1 only for national activities
      ros <- row.names(a1)
      act_counts <- sapply(ros,function(x) {strsplit(x, "[_]")[[1]][1]})
      ac <- a1[act_counts==u,]
      if(nrow(ac)>0){
        ##add all regional exposes to the activities and compute correlations
        a2 <- data.frame(t(rbind(dc,ac)))
        cob1 <- correlation(a2[,1:nrow(dc)],a2[,(nrow(dc)+1):ncol(a2)])
        
        dfc <- data.frame("exposure_type"=ee,"region"=cob1$Parameter1,"activity"=cob1$Parameter2, "crop"=cc,"r"=cob1$r,"p"=cob1$p)
        dfc$p[is.na(dfc$p)] <- 1
        df <- rbind(df,dfc)
      }
      print(paste0(u,"_",ee,"_",cc))
    }
  }
}
write.csv(df,paste0("corr_Fabio.csv"))  
head(df)
dim(df)
min(df$p)
write.csv(df,"corr.csv")


df <- data.frame(read.table("corr_floods.csv",header=T,sep=","))[,-1]
df1 <- df[df$p<0.5,]
df1 <- df1[df1$r<0,] 
nrow(df1)
wheat <- df1[df1$crop==sheets[1],]
heat_w <- wheat[wheat$exposure_type==types[1],]
nrow(heat_w)
flood_w <- wheat[wheat$exposure_type==types[4],]
nrow(flood_w)


####################regional exposure correlation plots
library(corrplot)

re <- data.frame(read_excel(paste0("Climate exposure & impact indexes/",extr[4],".xlsx"),sheet = sheets[1]))
re <- re[is.na(re$NUTS_ID)==F,]
row.names(re) <- re$NUTS_ID
re <- re[,c(-1:-22,-44:-50)]
names(re) <- 1997:2017
row.names(re)#aggregate and only compare regions, change shares from european to regional and do both ways

##all regional exposes
a2 <- data.frame(t(rbind(re,a1)))

#heat waves and wheat:
re <- data.frame(read_excel(paste0("Climate exposure & impact indexes/",extr[1],".xlsx"),sheet = sheets[1]))
re <- re[is.na(re$NUTS_ID)==F,]
row.names(re) <- re$NUTS_ID
re <- re[,c(-1:-22,-44:-50)]
names(re) <- 1997:2017
plot(1997:2017,a2$AT1*100, type = "l")#rel increases timeline for first activity
lines(1997:2017,a2$DK_a_62, type = "l", col="red")#sum regional exposure wheat and heatwaves
plot(1997:2017,a2$PL7*100, type = "l")#rel increases timeline for first activity
lines(1997:2017,a2$AT_a_62 , type = "l", col="red")#sum regional exposure wheat and heatwaves
plot(1997:2017,a2$NL1*1000, type = "l")#rel increases timeline for first activity
lines(1997:2017,a2$FI_a_xy  , type = "l", col="red")#sum regional exposure wheat and heatwaves

#corrplot(cob3, method="color", type="lower")
cob3 <- cor(a2[,c(1,2:50)])
corrplot(cob3, method="circle", type="lower")
cob3 <- cor(a2[,c(1,51:100)])
corrplot(cob3, method="circle", type="lower")
cob3 <- cor(a2[,c(1,101:150)])
corrplot(cob3, method="circle", type="lower")
cob3 <- cor(a2[,c(1,151:199)])
corrplot(cob3[1,], method="circle", type="lower")

