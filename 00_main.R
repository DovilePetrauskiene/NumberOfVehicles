# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><> PROJECT: Number of vehicles
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# -------------------------------------------------------------------------
# ----------- SCRIPT: 00_main.R
# -------------------------------------------------------------------------

# Loading Packages and Scripts --------------------------------------------
library("plyr")
library("reshape2")
library("ggplot2")
library("readxl")

source("00_functions.R")
load("EMI base.RData")

# Input -------------------------------------------------------------------

veh <- read.csv(file = "input/Research data number of vehicles.csv",check.names = F,stringsAsFactors = F, sep=";")
pop <- read.csv(file = "input/Population shares - vehicles.csv", check.names = F, stringsAsFactors = F, sep=";")
poppass <- read_excel("input/Cities pop.xlsx")
reg.pop <- read_excel("input/Region pop.xlsx")
expf <- read.csv("input/exceptions file.csv", check.names = F, stringsAsFactors = F, sep=";")
productnames <- read.csv("input/ProductNames.csv", check.names = F, stringsAsFactors = F, sep=";")
MO.pop <- read.csv("input/SUMs. Morocco.csv", check.names = F, stringsAsFactors = F, sep=",")

# ismetam miestus, kuriu total neturim ir sali MY, nes nera nei vieno skaiciaus, eilutes tuscios
veh <- veh[!(veh$RegionCode %in% c("CA26 approx.", "CA27 approx.", "IS04 approx.", "Rxx1", 
                                   "Rxx2", "TH06 approx.", "TH10 approx.", "TH11")) &
             veh$CountryCode!="MY",]

#su US neaisku, kas yra, kolkas ismetam 2000 US
veh <- veh[!(veh$CountryCode=="US"),]

pop <- pop[pop$`Item code` %in% c(1554),]
# Data Manipulation -------------------------------------------------------

# 1) Eliminating all approx, _1, _2 by using pop approx, _1, _2 -----------


####firstly eliminate approx
app <- veh[grep("approx", veh$RegionCode),]$RegionCode
app1 <- app

# app <- setdiff(app, c("EC01 approx.", "EC02 approx."))

after.approx <- NULL

for (c.app in app){
  
  cc <- gsub(" .*", "", c.app)
  s.pop <- pop[pop$`Region code` %in% c(cc),as.character(c(2000:2016))]/
    pop[pop$`Region code` %in% c(c.app),as.character(c(2000:2016))]
  
  if (length(s.pop[!is.na(s.pop)])==1){
    
    s.pop[as.character(2000:2016)] <- s.pop[!is.na(s.pop)]
    s.pop <- as.numeric(s.pop)
  }else{
    
    s.pop <- MASplineVector(s.pop, k=0.6)
  }
  
  s.veh <- veh[veh$RegionCode %in% c(c.app) & (veh$ProductID %in% c(2668,2667,2666,2000)),]
  
  for (j in 1:nrow(s.veh)){
    
   s.new <- s.veh[j,]
   s.new$RegionCode <- cc
   s.new[,as.character(2000:2016)] <- as.numeric(s.new[,as.character(2000:2016)])*s.pop
   
   after.approx <- rbind(s.new, after.approx)
  }
}

# nenaudosim "PH01_1", "PH01_2 approx.", "PH01_3 approx." su indicatoriais 2001, 2002, 2003, 
# nes per daug komplikuota
veh123 <- veh[(veh$ProductID %in% c(2001, 2002, 2003)) & 
                !(veh$RegionCode %in% c("PH01_1", "PH01_2 approx.", "PH01_3 approx.")), ]
veh <- veh[(!(veh$RegionCode %in% app1)) & (veh$ProductID %in% c(2668,2667,2666,2000)),]
veh <- rbind(after.approx, veh123, veh)

#####then eliminate _1, _2, _3, _4, _5, _6, _7, _8, _9, _10

h1 <- veh[grep("_1", veh$RegionCode),]$RegionCode 
h2 <- veh[grep("_2", veh$RegionCode),]$RegionCode
h3 <- veh[grep("_3", veh$RegionCode),]$RegionCode
h4 <- veh[grep("_4", veh$RegionCode),]$RegionCode
h5 <- veh[grep("_5", veh$RegionCode),]$RegionCode
h6 <- veh[grep("_6", veh$RegionCode),]$RegionCode
h7 <- veh[grep("_7", veh$RegionCode),]$RegionCode
h8 <- veh[grep("_8", veh$RegionCode),]$RegionCode
h9 <- veh[grep("_9", veh$RegionCode),]$RegionCode

all.pieces <- c(h1, h2, h3, h4, h5, h6, h7, h8, h9)

d.pieces <- veh[veh$RegionCode %in% all.pieces,]

all.pieces1 <- unique(gsub("_.*", "", all.pieces))

d.eliminated.pieces <- NULL

for (cc in all.pieces1){
  
  ss <- d.pieces[grep(cc, d.pieces$RegionCode),]
  
  for (i in unique(ss$ProductID)){
    
    suma <- apply(ss[ss$ProductID==i, as.character(2000:2016)],2,sum)
    oth <- unique(ss[ss$ProductID==i, c("Region/CityName", "CountryName", "CountryCode", 
                                 "ProductID", "ProductName", "Unit")])
    RegionCode=c(cc)
    s.new <- cbind(RegionCode,as.data.frame(oth), t(as.data.frame(suma)))
    d.eliminated.pieces <- rbind(s.new, d.eliminated.pieces)
  }
}

veh <- veh[!(veh$RegionCode %in% all.pieces),]
veh <- rbind(veh, d.eliminated.pieces)


# 2) Making cities data of region data by using population ----------------

reg.veh <- veh[substr(veh$RegionCode, 1, 2) %in% c("R0", "R1", "R2", "R3", "R4", 
                                                   "R5", "R6", "R7", "R8", "R9"),]
afterreg <- NULL

for (reg in unique(reg.veh$RegionCode)){
  
  cc <- as.character(city.codes[city.codes$RegionCode==reg,]$CityCode)
  
  for (j in cc){
  
    if (length(j)!=0){
      
      if (!(j %in% veh$RegionCode)){
        
        print(j)
        s.reg.veh <- reg.veh[reg.veh$RegionCode %in% c(reg),]
        
        for (id in s.reg.veh$ProductID){
          
          if (id %in% c(2668, 2667, 2666, 2000)){
          
            s.reg.pop <- reg.pop[reg.pop$RegionCode %in% c(reg), paste0("Y", 2000:2016)]
            s.city.pop <- poppass[poppass$CityCode %in% c(j) & poppass$ProductID==1554, 
                                  paste0("Y", 2000:2016)]
            
            # kadangi "MO" orginalus duomenys tik iki 2005 metu, reikia ilgesnes total.forecast eilutes,
            # del to naudojam pop nuo 2000 iki 2016, o ne pop15_64 nuo 2005 iki 2016.
            if (substr(j, 1, 2)=="MO"){
              
              s.city.pop <- MO.pop[MO.pop$`Metropolitan SUM` %in% j, as.character(2000:2016)]
            }
            
            s.new <- s.reg.veh[s.reg.veh$ProductID==id,]
            s.new[,as.character(2000:2016)] <- (s.city.pop/s.reg.pop)*s.new[,as.character(2000:2016)]
            s.new$RegionCode <- j
            
            afterreg <- rbind(afterreg, s.new)
          }else{
            
            s.new <- s.reg.veh[s.reg.veh$ProductID==id,]
            s.new$RegionCode <- j
            
            afterreg <- rbind(afterreg, s.new)
            
          }
        }
      }
    }
  }
}

veh <- setdiff.data.frame(veh, reg.veh)
veh <- rbind(veh, afterreg)

veh1 <- veh



# 3) Eliminating id codes 2000, 2001, 2002, 2003; -------------------------

# calculating 2666, 2667, 2668 from 2000, 2001, 2002, 2003; all methods we used are discribed
# in exceptions file.csv

#has only indicator 2000, we can't do anything with this for now, so any forecast for the city is going to be made
#only2000

veh1 <- veh1[!(veh1$RegionCode %in% (expf[expf$exp=="only2000",]$CityCode)),]

#has only indicator 2000, but the country has indicators 2666, 2667, 2668; so the structure of the country is going to be adapted to the city
#country678

# Fukcija, kuri suranda tokius atvejus
# country678 <- country678.cases.founder(veh1, city.codes)

veh1 <- country.ind.2000(veh1, expf)

#indicator 2668 missing, it is going to be calculated from indicators 2000, 2667, 2666 (2668=2000-2667-2666)
#miss.ind

veh1 <- missing.ind(veh1, expf)

#countries data is no more necessary

veh1 <- veh1[nchar(veh1$RegionCode)==4,]


# Forecasts ---------------------------------------------------------------


# 4) totals forecast ------------------------------------------------------

# in cases make678 and trend2000 (exceptions file) indicator 2000 is going to be the total 
# of the vehicles

# Funkcija surasti trend2000 atvejams
# trend2000 <- trend2000.cases.finder(veh1)

# should be TRUE if not something wrong
all(veh1[veh1$ProductID==2000,]$RegionCode %in% c(expf[expf$exp %in% c("make678", "trend2000"),]$CityCode))
########################

totals1 <- veh1[veh1$RegionCode %in% c(expf[expf$exp %in% c("make678", "trend2000"),]$CityCode) &
                  veh1$ProductID==2000,]
totals1 <- totals1[c("RegionCode", "CountryCode", as.numeric(2000:2016))]

totals2 <- veh1[veh1$ProductID %in% c(2666,2667,2668) & 
                  !(veh1$RegionCode %in% c(expf[expf$exp %in% c("make678", "trend2000"),]$CityCode)),]
totals2 <- totals2[c("RegionCode", "CountryCode", "ProductID", as.numeric(2000:2016))]

totals2 <- melt(totals2, id=c("RegionCode", "CountryCode", "ProductID"))
totals2 <- ddply(totals2, .(RegionCode, CountryCode, variable), summarize,  total=sum(value))
totals2 <- dcast(totals2, RegionCode+CountryCode~variable, value.var = c("total"))

totals <- rbind(totals1, totals2)

#forecasting (city.totals.vehicle/city.pop15-64) with MASpline 

poppass15_64 <- poppass[poppass$ProductID==1559,]

year <- as.character(2000:2016)

total.forecast <- NULL

for (cc in unique(totals$RegionCode)){
  
  s.pop <- poppass15_64[poppass15_64$CityCode==cc, paste0("Y", 2000:2016)]*1000
  
  # kadangi "MO" orginalus duomenys tik iki 2005 metu, reikia ilgesnes total.forecast eilutes,
  # del to naudojam pop nuo 2000 iki 2016, o ne pop15_64 nuo 2005 iki 2016.
  if (substr(cc, 1, 2)=="MO"){
    
    s.pop <- MO.pop[MO.pop$`Metropolitan SUM` %in% cc, as.character(2000:2016)]*1000
  }
  
  st <- totals[totals$RegionCode==cc,]
  
  if (length(st[, year][!is.na(st[, year])])==1){
    
    tt <- as.numeric(st[,year])/as.numeric(s.pop)
    tt <- rep(tt[!is.na(tt)], length(tt))
    st[, year] <- tt*as.numeric(s.pop)
    
  }else{
    
    if (cc %in% expf[expf$exp=="changek",]$CityCode){
      
      k <- 0.35
      st[, year] <- MASplineVector(as.numeric(st[,year])/as.numeric(s.pop), plot=T, k=k)*as.numeric(s.pop)
      
    }else{
    
      k <- 0.7
      st[, year] <- MASplineVector(as.numeric(st[,year])/as.numeric(s.pop), plot=T, k=k)*as.numeric(s.pop)
    }
  }
  
  total.forecast <- rbind(total.forecast, st)
}


# 5) Forecasting shares 2666, 2667, 2668 from total with MASpline ---------

veh1 <- veh1[veh1$RegionCode!="IN21",]
total.forecast <- total.forecast[total.forecast$RegionCode!="IN21",]

data.out.proc <- NULL
data.out.num <- NULL

for (cc in unique(total.forecast$RegionCode)){

  s <- veh1[veh1$RegionCode==cc,]
  st <- total.forecast[total.forecast$RegionCode==cc,]
  
  if (cc %in% expf[expf$exp=="trend2000",]$CityCode){
    
    s <- s[s$ProductID!=2000,]
  }
  
  if (cc %in% expf[expf$exp=="make678",]$CityCode){
   
    s <- s[s$ProductID!=2000,]
    s[s$ProductID==2001,]$ProductID <- 2666
    s[s$ProductID==2002,]$ProductID <- 2667
    s[s$ProductID==2003,]$ProductID <- 2668
    
    for (j in s$ProductID){
      
      s[s$ProductID==j,year] <- s[s$ProductID==j,year]/100
      s[s$ProductID==j,year] <- MASplineVector(s[s$ProductID==j,year])
    }
    
  }else{
    
    if (cc %in% expf[expf$exp=="connect",]$CityCode){
      
      for (i in c(2667, 2666, 2668)){
        
        s[s$ProductID==i,year] <- s[s$ProductID==i,year]/st[,year]
      }
      
      s1 <- s[s$ProductID %in% c(2666:2668),]
      
      s2 <- s[s$ProductID %in% c(2001:2003),]
      s2[s2$ProductID==2001,]$ProductID <- 2666
      s2[s2$ProductID==2002,]$ProductID <- 2667
      s2[s2$ProductID==2003,]$ProductID <- 2668
      
      
      for (i in c(2667, 2666, 2668)){
        
        s2[s2$ProductID==i,year] <- s2[s2$ProductID==i,year]/100
      }
      
      s1 <- s1[order(s1$ProductID),]
      s2 <- s2[order(s2$ProductID),]
      
      for (y in as.character(c(2000:2016))){
        
        if (all(is.na(s2[,y]))){
          
          next
          
        }else{
          
          s1[,y] <- s2[,y]
        }
      }
      
      s <- s1
      
      for (kk in c(2666, 2667, 2668)){
        
        s[s$ProductID==kk,year] <- MASplineVector(s[s$ProductID==kk,year])
      }
      
    }else{
      
      
      ## randam tokius atvejus, kai orginaliu duomenu 2666, 2667, 2668 laiko eilutes nera vienodo ilgio,
      ## tada isskaiciuojam trukstamus metus is total.forecast. Tarkim 2005 metais turim orginalu 2666 ir 2667 
      ## rodikli, taciau nera 2668. Tada 2668(2005m)=total.forecast(2005m)-2666(2005m)-2667(2005)
      out.sl <- NULL
      for (j in s$ProductID){
        
        sl <- length(s[s$ProductID==j, year][!is.na(s[s$ProductID==j, year])])
        out.sl <- rbind(out.sl,c(j, sl))
      }
      out.sl <- as.data.frame(out.sl)
      
      if (nrow(out.sl)>2){
      
        if ((out.sl$V2[1]!=out.sl$V2[2]) | (out.sl$V2[2]!=out.sl$V2[3])){
          
          idd <- out.sl[out.sl$V2==max(out.sl$V2),]$V1[1]
          
          st <- total.forecast[total.forecast$RegionCode==cc,year]
            
          s1 <- s[s$ProductID %in% idd,]
            
          s2 <- s[s$ProductID %in% setdiff(c(2666, 2667, 2668),idd),]
            
          s2.sum.new <- st-s1[, year]
            
          for (yy in names(s2.sum.new)){
              
            yy <- as.character(yy)
              
              
            if (all(is.na(c(s2.sum.new[,yy], s2[,yy])))){
              next
            }
              
            if (all(!is.na(c(s2.sum.new[,yy], s2[,yy])))){
              next
            }
              
            if ((!is.na(c(s2.sum.new[,yy]))) & any(!is.na(s2[,yy]))){
                
              s2[,yy][is.na(s2[,yy])] <- s2.sum.new[,yy] - s2[,yy][!is.na(s2[,yy])]
            }
              
          }
          
          for (yy in names(s2.sum.new)){
              
            if ((!is.na(c(s2.sum.new[,yy]))) & all(is.na(s2[,yy]))){
              
              year2 <- as.character(2005:2016)   
              s2sproc <- s2
              
              if (length(s2sproc[1, year2][!is.na(s2sproc[1, year2])])==1){
                
                s2sproc[1, year2] <- (s2sproc[1, year2]/s2.sum.new[,year2])[!is.na(s2sproc[1, year2]/s2.sum.new[,year2])]
                s2sproc[2, year2] <- (s2sproc[2, year2]/s2.sum.new[,year2])[!is.na(s2sproc[2, year2]/s2.sum.new[,year2])]
              }else{
              
                s2sproc[1, year2] <- MASplineVector(s2sproc[1, year2]/s2.sum.new[,year2], k=0.2, plot=T)
                s2sproc[2, year2] <- MASplineVector(s2sproc[2, year2]/s2.sum.new[,year2], k=0.2, plot=T)
              }
              
              s2sproc[, year2] <- apply(s2sproc[, year2], 2, function(x){x/sum(x)})
              s2sproc[1, year2] <- s2sproc[1, year2]*s2.sum.new[,year2]
              s2sproc[2, year2] <- s2sproc[2, year2]*s2.sum.new[,year2]
              
              s2 <- s2sproc 
            }
          }
          
          s <- rbind(s1, s2)
             
        }
      }
        
      # forecastinam su MASplineVEctor
      for (j in s$ProductID){
          
        if ((length(s[s$ProductID==j,year][!is.na(s[s$ProductID==j,year])]))==1){
            
          s[s$ProductID==j,year] <- s[s$ProductID==j,year]/st[,year]
          s[s$ProductID==j,year] <- rep(s[s$ProductID==j,year][!is.na(s[s$ProductID==j,year])],
            length(s[s$ProductID==j,year]))
            
        }else{
            
          s.out <- MASplineVector(s[s$ProductID==j,year]/st[,year], k=0.5)
            
          if (any(s.out<0)){
              
            ss <- s[s$ProductID==j,year]/st[,year]
              
            if (s.out[1]<0) ss[1] <- 0
            if (s.out[length(s.out)]<0) ss[length(ss)] <- 0
              
            s[s$ProductID==j,year] <- MASplineVector(ss)
              
          }else{
              
            s[s$ProductID==j,year] <- s.out
          }
        }
      }
    }
  }
  
  ##sunormuojam
  
  sum <- apply(s[,as.character(2000:2016)], 2, sum)
  
  s[,as.character(2000:2016)] <- t(apply(s[,as.character(2000:2016)], 1, function(x){
    x/sum}))
  
  data.out.proc <- rbind(data.out.proc,s)
  
  snum <- s
  snum[,as.character(2000:2016)] <- do.call("rbind", apply(snum[,as.character(2000:2016)], 1, function(x){
    x*st[,as.character(2000:2016)]}))
  
  data.out.num <- rbind(data.out.num, snum)
  
}



# 6) Final corrections and checkings --------------------------------------


data.out.proc <- data.out.proc[, c("RegionCode", "CountryCode", "ProductID", "Unit", as.character(2000:2016))]
data.out.proc <- rename(data.out.proc, c(RegionCode="CityCode"))
data.out.proc$Unit <- "%"
data.out.proc <- merge(data.out.proc, city.codes[, c("CityCode", "CountryName", "CityName", "CityCodeID")], by=c("CityCode"))
data.out.proc <- merge(data.out.proc, productnames, by=c("ProductID"))
data.out.proc <- data.out.proc[, c("CityCodeID", "CityCode", "CountryCode", "CityName", "CountryName",
                                   "ProductID", "ProductName", "Unit", as.character(2000:2016))]


data.out.num <- data.out.num[, c("RegionCode", "CountryCode", "ProductID", "Unit", as.character(2000:2016))]
data.out.num <- rename(data.out.num, c(RegionCode="CityCode"))
data.out.num$Unit <- "'000"
data.out.num <- merge(data.out.num, city.codes[, c("CityCode", "CountryName", "CityName", "CityCodeID")], by=c("CityCode"))
data.out.num <- merge(data.out.num, productnames, by=c("ProductID"))
data.out.num <- data.out.num[, c("CityCodeID", "CityCode", "CountryCode", "CityName", "CountryName",
                                   "ProductID", "ProductName", "Unit", as.character(2000:2016))]

for (index in 1:nrow(data.out.num)){
  
  data.out.num[index, as.character(2000:2016)] <- data.out.num[index, as.character(2000:2016)]/1000
}


# saving data
write.csv(data.out.proc, 
          paste0("output/Number of Vechiles final output proc_",Sys.Date(), ".csv"), row.names=F)
write.csv(data.out.num, 
          paste0("output/Number of Vechiles final output_",Sys.Date(),".csv"), row.names=F)


# 7) Graphs ---------------------------------------------------------------


pdf(paste0("plots/Number of vehicles_", Sys.Date(),".pdf"),width=16.5 * 0.8,height=13 * 0.7)

print(titlepage(title = "Number of Vehicles", sub = "Cities", date = Sys.Date(), author = "JP", size=15))

year <- as.character(2005:2016)
for (cn in unique(data.out.num$CountryName)){
  
  sn <- data.out.num[data.out.num$CountryName==cn,]
  sn <- sn[order(sn$CityCode),]
  sp <- data.out.proc[data.out.proc$CountryName==cn,]
  
  par(mfrow=c(2,2), oma=c(0,0,2,0))
  
  for (pr in unique(sn$ProductName)){
    
    frame1 <- sn[sn$ProductName==pr,c("CityName", "CityCode", year)]
    
    pr.id <- unique(sn[sn$ProductName==pr,]$ProductID) 
    
    rawd <- veh1[veh1$CountryCode==unique(sn$CountryCode) & veh1$ProductID==pr.id,]
    
    index <- 1
    
    par(mar = c(5,4,4,2))
    
    plot(1, type="n", xlab="", ylab="Number '000", xlim=c(2004.5, 2016.5), ylim=c(0, max(frame1[,year], na.rm = T)),
         main=pr, mgp=c(2.5,1,0))
    
    for (i in unique(frame1$CityCode)){
      
      cityf <- frame1[frame1$CityCode==i,]
      cityr <- rawd[rawd$RegionCode==i,]
      cityr[, as.character(2000:2016)] <- cityr[, as.character(2000:2016)]/1000 
      lg <- length(unique(frame1$CityCode))
      
      matplot(t(cityf[,year]), x=(2005:2016), main=pr, type=c("l"), 
              col=emi_pal()(lg)[index], lty=1, xlim=c(2004.5, 2016.5), pch=1, add=T)
      text(x=c(2004.5, 2016.5), y= c(cityf[,"2005"], cityf[,"2016"]),  cityf$CityName, cex=0.7,
           col=emi_pal()(lg)[index])
      grid()
      
      if (nrow(cityr)==1){
      
        matplot(t(cityr[,year]), x=(2005:2016), type=c("p"), 
                col=emi_pal()(lg)[index], xlim=c(2004.5, 2016.5), add=T, pch=16)
        
      }
      
      index <- index+1
    }
    
  }
  
  title(main=cn, outer=TRUE, cex.main=2)
  
  # for (city in unique(sn$CityCode)){
  # 
  #   cityname <- city.codes[city.codes$CityCode==city,]$CityName
  # 
  #   s.city <- sp[sp$CityCode==city,]
  #   s.city <- s.city[order(s.city$ProductID),]
  #   s.city <- s.city[,c("ProductID", year)]
  #   s.city <- melt(s.city, id=c("ProductID"))
  #   s.city <- dcast(s.city, variable~ProductID, value.var = "value")
  #   row.names(s.city) <- s.city$variable
  #   s.city$variable <- NULL
  # 
  #   sraw <- veh1[veh1$RegionCode==city & veh1$ProductID %in% c(2666, 2667, 2668),]
  # 
  #   if (nrow(sraw)==0){
  # 
  #     sraw <- veh1[veh1$RegionCode==city & veh1$ProductID %in% c(2001, 2002, 2003),]
  #     sraw[,as.character(2005:2016)] <- apply(sraw[,as.character(2005:2016)], 2, function(x){x/100})
  #     sraw[sraw$ProductID==2001,]$ProductID <- 2666
  #     sraw[sraw$ProductID==2002,]$ProductID <- 2667
  #     sraw[sraw$ProductID==2003,]$ProductID <- 2668
  #   }
  # 
  #   sraw[,as.character(2005:2016)] <- apply(sraw[,as.character(2005:2016)], 2, function(x){x/sum(x)})
  #   sraw <- sraw[order(sraw$ProductID),]
  #   sraw <- sraw[,c("ProductID", year)]
  #   sraw <- melt(sraw, id=c("ProductID"))
  #   sraw <- dcast(sraw, variable~ProductID, value.var = "value")
  #   row.names(sraw) <- sraw$variable
  #   sraw$variable <- NULL
  # 
  #   par(mfrow=c(1,1), oma=c(0,0,0,0))
  # 
  #   matplotf(s.city, main=paste0(city, " - ", cityname), legend = F,
  #            text = T, col=emi_pal()(3), lwd = 2, stx = 2005, abline=c(0,1))
  #   
  #   if (!all(is.na(sraw$`2666`) & is.na(sraw$`2667`) & is.na(sraw$`2668`))){
  #     
  #     matplotf(sraw, legend = F, col=emi_pal()(3), point=T, stx = 2005, lwd=2, add=T)
  #   }
  #   mtext("2666 - Passenger cars in use", side = 3, col = emi_pal()(3)[1], cex=0.7,
  #         line = 1, adj=0.1, at=2015)
  #   mtext("2667 - Commercial Vehicles in Use", side = 3, col = emi_pal()(3)[2],
  #         cex=0.7, line = 0.5, adj=0.1, at=2015)
  #   mtext("2668 - Motorcycles and Mopeds in Use", side = 3, col = emi_pal()(3)[3],
  #         cex=0.7, line = 0, adj=0.1, at=2015)
  # 
  #   grid()
  # 
  # }
  
}

dev.off()

