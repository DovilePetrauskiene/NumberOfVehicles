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
source("standardGraphs.R")
load("EMI base.RData")

# Input -------------------------------------------------------------------

veh <- read.csv(file = "input/Research data number of vehicles.csv",check.names = F,stringsAsFactors = F, sep=";")
pop <- read.csv(file = "input/Population shares - vehicles.csv", check.names = F, stringsAsFactors = F, sep=";")
poppass <- read_excel("input/Cities pop.xlsx")
reg.pop <- read_excel("input/Region pop.xlsx")
expf <- read.csv("input/exceptions file.csv", check.names = F, stringsAsFactors = F, sep=";")
productnames <- read.csv("input/ProductNames.csv", check.names = F, stringsAsFactors = F, sep=";")
MO.pop <- read.csv("input/SUMs. Morocco.csv", check.names = F, stringsAsFactors = F, sep=",")
poss.of.cars <- read_excel("input/Additional indicators.xlsx")
us.codes <- read.csv("input/USA region codes.csv", check.names = F, stringsAsFactors = F, sep=",")
old.data <- read.csv("input/Vehicles passport 2016-06-10.csv", check.names = F, stringsAsFactors = F, sep=",")

# ismetam miestus, kuriu total neturim ir sali MY, nes nera nei vieno skaiciaus, eilutes tuscios
veh <- veh[!(veh$RegionCode %in% c("CA26 approx.", "CA27 approx.", "IS04 approx.", "Rxx1", 
                                   "Rxx2", "TH06 approx.", "TH10 approx.", "TH11")) &
             veh$CountryCode!="MY",]

#su US neaisku, kas yra, kolkas ismetam 2000 US
veh <- veh[!(veh$CountryCode=="US" & veh$ProductID==2000),]

pop <- pop[pop$`Item code` %in% c(1554),]

#reg.pop sutvarkom US kodus
us.reg <- reg.pop[reg.pop$CountryCode %in% "US",]
us.reg <- rename(us.reg, c(RegionCode="PassportCode"))
us.reg <- merge(us.reg, us.codes[, c("PassportCode", "RegionCode")], by=c("PassportCode"))
us.reg$PassportCode <- NULL

reg.pop <- reg.pop[!(reg.pop$CountryCode %in% "US"),]
reg.pop <- rbind(reg.pop, us.reg)


# Data Manipulation -------------------------------------------------------


# 1) Eliminating all approx, _1, _2 by using pop approx, _1, _2 -----------


####firstly eliminate approx
app1 <- unique(veh[grep("approx", veh$RegionCode),]$RegionCode)
after.approx <- EliminateApprox(veh, pop)

# nenaudosim "PH01_1", "PH01_2 approx.", "PH01_3 approx." su indicatoriais 2001, 2002, 2003, 
# nes per daug komplikuota
veh123 <- veh[(veh$ProductID %in% c(2001, 2002, 2003)) & 
                !(veh$RegionCode %in% c("PH01_1", "PH01_2 approx.", "PH01_3 approx.")), ]
veh <- veh[(!(veh$RegionCode %in% app1)) & (veh$ProductID %in% c(2668,2667,2666,2000)),]
veh <- rbind(after.approx, veh123, veh)

####then eliminate _1, _2, _3, _4, _5, _6, _7, _8, _9, _10
veh <- Eliminate12(veh)


# 2) Making cities data of region data by using population ----------------

veh <- CityVehMadeOfRegionVeh(veh, reg.pop, poppass)

veh1 <- veh
veh <- veh1

# 3) Eliminating id codes 2000, 2001, 2002, 2003; -------------------------

# calculating 2666, 2667, 2668 from 2000, 2001, 2002, 2003; all methods we used are discribed
# in exceptions file.csv

#has only indicator 2000, we can't do anything with this for now, so no forecast for the city is going to be made
#only2000

veh <- veh[!(veh$RegionCode %in% (expf[expf$exp=="only2000",]$CityCode)),]

#has only indicator 2000, but the country has indicators 2666, 2667, 2668; so the structure of the country is going to be adapted to the city
#country678

# Fukcija, kuri suranda tokius atvejus
# country678 <- country678.cases.founder(veh, city.codes)

veh <- country.ind.2000(veh, expf)

#indicator 2668 missing, it is going to be calculated from indicators 2000, 2667, 2666 (2668=2000-2667-2666)
#miss.ind

veh <- missing.ind(veh, expf)

#countries data is no more necessary

veh <- veh[nchar(veh$RegionCode)==4,]


# Forecasts ---------------------------------------------------------------


# 4) totals forecast ------------------------------------------------------


# in cases make678 and trend2000 (exceptions file) indicator 2000 is going to be the total 
# of the vehicles

# Funkcija surasti trend2000 atvejams
# trend2000 <- trend2000.cases.finder(veh)

# should be TRUE if not - something wrong
all(veh[veh$ProductID==2000,]$RegionCode %in% c(expf[expf$exp %in% c("make678", "trend2000"),]$CityCode))
########################

#totals1 - taking indicator 2000 as total
totals1 <- veh[veh$RegionCode %in% c(expf[expf$exp %in% c("make678", "trend2000"),]$CityCode) &
                  veh$ProductID==2000,]
totals1 <- totals1[c("RegionCode", "CountryCode", as.numeric(2000:2016))]

#totals2=sum(2666, 2667, 2668)
totals2 <- veh[veh$ProductID %in% c(2666,2667,2668) & 
                  !(veh$RegionCode %in% c(expf[expf$exp %in% c("make678", "trend2000"),]$CityCode)),]
totals2 <- totals2[c("RegionCode", "CountryCode", "ProductID", as.numeric(2000:2016))]

totals2 <- melt(totals2, id=c("RegionCode", "CountryCode", "ProductID"))
totals2 <- ddply(totals2, .(RegionCode, CountryCode, variable), summarize,  total=sum(value))
totals2 <- dcast(totals2, RegionCode+CountryCode~variable, value.var = c("total"))

#connecting both total1 and total2
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

veh <- veh[veh$RegionCode!="IN21",]
total.forecast <- total.forecast[total.forecast$RegionCode!="IN21",]

data.out.proc <- NULL
data.out.num <- NULL

for (cc in unique(total.forecast$RegionCode)){

  s <- veh[veh$RegionCode==cc,]
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


#### cheking if data matches with possessions of cars

bad.cities <- NULL
for (city in data.out.num[data.out.num$ProductID==2666,]$CityCode){
  
  s.poss <- poss.of.cars[poss.of.cars$ProductID==2505 & poss.of.cars$CityCode==city, 
                         c("CityCode", paste0("Y", 2005:2030))]
  s.hh <- poss.of.cars[poss.of.cars$ProductID==2485 & poss.of.cars$CityCode==city, 
                       c("CityCode", paste0("Y", 2005:2030))]
  s.poss <- s.poss[, paste0("Y", 2005:2030)]*s.hh[, paste0("Y", 2005:2030)]/100
  
  cars <- data.out.num[data.out.num$ProductID==2666 & data.out.num$CityCode==city,]
  
  commercial.veh <- data.out.num[data.out.num$ProductID==2667 & data.out.num$CityCode==city,]
  
  allcars <- cars
  
  if (nrow(commercial.veh)!=0){
  
    allcars[, as.character(2000:2016)] <- cars[, as.character(2000:2016)]+commercial.veh[, as.character(2000:2016)]
  }
    
  if (!(any(allcars[, as.character(2005:2016)]>s.poss[, paste0("Y", 2005:2016)]))){
    
    bad.cities <- c(bad.cities, city)
  }
}

#correcting bad things

year2 <- as.character(2005:2016)

cities.to.correct <- intersect(expf[expf$exp=="UsePossCars",]$CityCode, bad.cities)

for (city in cities.to.correct){
  
  s.poss <- poss.of.cars[poss.of.cars$ProductID==2505 & poss.of.cars$CityCode==city, 
                         c("CityCode", paste0("Y", 2005:2030))]
  s.hh <- poss.of.cars[poss.of.cars$ProductID==2485 & poss.of.cars$CityCode==city, 
                       c("CityCode", paste0("Y", 2005:2030))]
  s.poss <- s.poss[, paste0("Y", 2005:2030)]*s.hh[, paste0("Y", 2005:2030)]/100
  
  data.out.num[data.out.num$CityCode==city & data.out.num$ProductID==2666,year2] <- 
    s.poss[,paste0("Y",year2)]-data.out.num[data.out.num$CityCode==city & data.out.num$ProductID==2667,year2]
}

# saving data
write.csv(data.out.proc, 
          paste0("output/Number of Vechiles final output proc_",Sys.Date(), ".csv"), row.names=F)
write.csv(data.out.num, 
          paste0("output/Number of Vechiles final output_",Sys.Date(),".csv"), row.names=F)


# 7) Graphs ---------------------------------------------------------------

# GraphVehiclesExtended(veh, data.out.num, data.out.proc)
# GraphVehicles(veh, data.out.num)

cities <- unique(c(data.out.num$CityCode, old.data$CityCode))

veh$ProductName <- NULL
veh <- merge(veh, productnames, by=c("ProductID"))

year <- as.character(2000:2016)
year2 <- as.character(2005:2016)
yearY <- paste0("Y", 2005:2016)


pdf(paste0("plots/Number of vehicles vs old_", Sys.Date(),".pdf"),width=16.5 * 0.8,height=13 * 0.7)

print(titlepage(title = "Number of Vehicles vs old", sub = "Cities", date = Sys.Date(), author = "JP", size=15))

for (city in sort(cities)){
  
  s.est <- subset(data.out.num, CityCode==city)
  s.old <- subset(old.data, CityCode==city)
  s.new <- subset(veh, RegionCode==city)
  s.new <- s.new[s.new$ProductID %in% c(2666,2667,2668),]
  
  if (nrow(s.old)!=0){
    
    s.old <- melt(s.old[,c("ProductName", yearY)], id=c("ProductName"))
    s.old$period <- "old"
  }
  
  if (nrow(s.est)!=0){
    
    s.est <- melt(s.est[,c("ProductName", year2)], id=c("ProductName"))
    s.est$period <- "estimate"
  }
  
  if (nrow(s.new)!=0){
    
    s.new[, year] <- apply(s.new[, year], 2, function(x){x/1000})
    s.new <- melt(s.new[,c("ProductName", year2)], id=c("ProductName"))
    s.new$period <- "new"
    
  }
  
  # for (pr in unique(s.new$ProductName)){
  #   
  #   for (yy in year2){
  #     
  #     if (!is.na(s.new[s.new$ProductName==pr & s.new$variable==as.numeric(yy),]$value)){
  #       
  #       s.est[s.est$ProductName==pr & s.est$variable==yy,]$period <- "new"
  #     }
  #   }
  # }
  
  df <- rbind(s.old, s.est, s.new)
  
  df$variable <- gsub("Y", "",df$variable)
  
  drawPlot(df, ypav="Number of Vehicles", citycode=city)
}

dev.off()
