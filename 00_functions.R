# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><> PROJECT: Number of vehicles
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# -------------------------------------------------------------------------
# ----------- SCRIPT: 00_functions.R
# -------------------------------------------------------------------------

# emibase -----------------------------------------------------------------

emibase <- function(){
  cat("Loading EMI base package ...","\n")
  load("K:/GMID Research/SM-route/EMI base/EMI base.Rdata",envir = .GlobalEnv)
  cat("Succesfully loaded","\n")
}

setdiff.data.frame <-
  function(A,B) A[ !duplicated( rbind(B,A) )[ -seq_len(nrow(B))] , ]

#has only indicator 2000, but the country has indicators 2666, 2667, 2668; so the structure of the country is going to be adapted to the city
#country678
country.ind.2000 <- function(veh, expf){
  
  countries <- unique(expf[expf$exp=="country678",]$CountryCode)
  
  out.data <- NULL
  for (cn in countries){
    
    cin <- veh[veh$RegionCode==cn & veh$ProductID %in% c(2666, 2667, 2668), 
                c("ProductID", "ProductName", as.character(2000:2016))]
    cin[,as.character(2000:2016)] <- apply(cin[,as.character(2000:2016)], 2, function(x){
      x/sum(x)
    })
    
    cities <- unique(expf[expf$exp=="country678" & expf$CountryCode==cn, ]$CityCode)
    
    for (cc in cities){
      
      cc2000 <- veh[veh$RegionCode==cc,]
      cc.new <- cin
      cc.new[, as.character(2000:2016)] <- do.call("rbind",apply(cc.new[, as.character(2000:2016)], 1, function(x){
        x*cc2000[, as.character(2000:2016)]})) 
      cc2000 <- cc2000[,c("RegionCode", "Region/CityName", "CountryName", "CountryCode", "Unit")]
      cc.new <- cbind(rbind(cc2000, cc2000, cc2000), cc.new)
      
      out.data <- rbind(out.data, cc.new)
    }
    
  }
  
  veh <- veh[!(veh$CountryCode %in% countries),]
  veh <- rbind(veh, out.data)
  
  return(veh)
}


#indicator 2668 missing, it is going to be calculated from indicators 2000, 2667, 2666 (2668=2000-2667-2666)
#miss.ind

missing.ind <- function(veh, expf){
  
  cities <- expf[expf$exp=="miss.ind",]$CityCode
  
  data.out <- NULL
  for (cc in cities){
    
    year <- as.character(2000:2016)
    
    cc.new <- veh[veh$RegionCode==cc,]
    cc.new <- rbind(cc.new, cc.new[3,])
    cc.new[4,]$ProductID <- 2668
    cc.new[cc.new$ProductID==2668,year] <- cc.new[cc.new$ProductID==2000, year] -
      cc.new[cc.new$ProductID==2666, year] - cc.new[cc.new$ProductID==2667, year]
    
    data.out <- rbind(data.out, cc.new)
    
  }
  
  data.out <- data.out[data.out$ProductID!=2000,]
  
  veh <- veh[!(veh$RegionCode %in% cities),]
  veh <- rbind(veh, data.out)
  
  return(veh)
}

country678.cases.founder <- function(veh, city.codes){
  
  country678 <- NULL
  for (cc in veh$RegionCode){
    
    s <- veh[veh$RegionCode==cc & veh$ProductID %in% c(2000, 2666, 2668, 2667,
                                                          2001, 2002, 2003),]
    
    if (nrow(s)==1){
      
      if (s$ProductID==2000){
        
        cn.code <- city.codes[city.codes$CityCode==cc,]$CountryCode
        
        if (any(c(2666, 2668, 2667) %in% veh[veh$RegionCode==cn.code,]$ProductID) ||
            any(c(2001, 2002, 2003) %in% veh[veh$RegionCode==cn.code,]$ProductID)){
          
          country678 <- c(country678, cc)
        }
      }
    }
  }
  
  return(country678)
}


trend2000.cases.finder <- function(veh){
  
  trend2000 <- NULL
  
  year <- as.character(2000:2016)
  for (cc in unique(veh$RegionCode)){
    
    s <- veh[veh$RegionCode==cc & veh$ProductID %in% c(2000, 2666, 2667, 2668),]
    
    if ((2000 %in% s$ProductID) & nrow(s)==4){
      
      tt <- NULL
      for (j in unique(s$ProductID)){
        
        tt <- rbind(c(j,length(s[s$ProductID==j,year][!is.na(s[s$ProductID==j,year])])),tt)
      }
      tt <- as.data.frame(tt)
      
      if (max(tt$`V2`)==tt[tt$V1==2000,]$V2){
        
        trend2000 <- c(cc, trend2000)
      }
    }
  }
  
  return(trend2000)
}


#Eliminating approx from veh variable
EliminateApprox <- function(veh, pop){
  
  app <- veh[grep("approx", veh$RegionCode),]$RegionCode
  app1 <- app
  
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
  
  return(after.approx)
}


#Eliminate _1, _2, _3, _4, _5, _6, _7, _8, _9; just putting them together, 
#for example - we have indicator 2666 of AT03_01 and AT03_02, so AT03(ind. 2666)= AT03_01 + AT03_02

Eliminate12 <- function(veh){
  
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
  
  return(veh)
}

#################
CityVehMadeOfRegionVeh <- function(veh, reg.pop, poppass){
  
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
  
  return(veh)
}


#graph
GraphVehiclesExtended <- function(veh, data.out.num, data.out.proc){
  
  pdf(paste0("plots/Number of vehicles extended_", Sys.Date(),".pdf"),width=16.5 * 0.8,height=13 * 0.7)
  
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
      
      rawd <- veh[veh$CountryCode==unique(sn$CountryCode) & veh$ProductID==pr.id,]
      
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
    
    for (city in unique(sn$CityCode)){
      
      cityname <- city.codes[city.codes$CityCode==city,]$CityName
      
      s.city <- sp[sp$CityCode==city,]
      s.city <- s.city[order(s.city$ProductID),]
      s.city <- s.city[,c("ProductID", year)]
      s.city <- melt(s.city, id=c("ProductID"))
      s.city <- dcast(s.city, variable~ProductID, value.var = "value")
      row.names(s.city) <- s.city$variable
      s.city$variable <- NULL
      
      sraw <- veh[veh$RegionCode==city & veh$ProductID %in% c(2666, 2667, 2668),]
      
      if (nrow(sraw)==0){
        
        sraw <- veh[veh$RegionCode==city & veh$ProductID %in% c(2001, 2002, 2003),]
        sraw[,as.character(2005:2016)] <- apply(sraw[,as.character(2005:2016)], 2, function(x){x/100})
        sraw[sraw$ProductID==2001,]$ProductID <- 2666
        sraw[sraw$ProductID==2002,]$ProductID <- 2667
        sraw[sraw$ProductID==2003,]$ProductID <- 2668
      }
      
      sraw[,as.character(2005:2016)] <- apply(sraw[,as.character(2005:2016)], 2, function(x){x/sum(x)})
      sraw <- sraw[order(sraw$ProductID),]
      sraw <- sraw[,c("ProductID", year)]
      sraw <- melt(sraw, id=c("ProductID"))
      sraw <- dcast(sraw, variable~ProductID, value.var = "value")
      row.names(sraw) <- sraw$variable
      sraw$variable <- NULL
      
      par(mfrow=c(1,1), oma=c(0,0,0,0))
      
      matplotf(s.city, main=paste0(city, " - ", cityname), legend = F,
               text = T, col=emi_pal()(3), lwd = 2, stx = 2005, abline=c(0,1))
      
      if (!all(is.na(sraw$`2666`) & is.na(sraw$`2667`) & is.na(sraw$`2668`))){
        
        matplotf(sraw, legend = F, col=emi_pal()(3), point=T, stx = 2005, lwd=2, add=T)
      }
      mtext("2666 - Passenger cars in use", side = 3, col = emi_pal()(3)[1], cex=0.7,
            line = 1, adj=0.1, at=2015)
      mtext("2667 - Commercial Vehicles in Use", side = 3, col = emi_pal()(3)[2],
            cex=0.7, line = 0.5, adj=0.1, at=2015)
      mtext("2668 - Motorcycles and Mopeds in Use", side = 3, col = emi_pal()(3)[3],
            cex=0.7, line = 0, adj=0.1, at=2015)
      
      grid()
      
    }
    
  }
  
  dev.off()
}


GraphVehicles <- function(veh, data.out.num){
  
  pdf(paste0("plots/Number of vehicles_", Sys.Date(),".pdf"),width=16.5 * 0.8,height=13 * 0.7)
  
  print(titlepage(title = "Number of Vehicles", sub = "Cities", date = Sys.Date(), author = "JP", size=15))
  
  year <- as.character(2005:2016)
  for (cn in unique(data.out.num$CountryName)){
    
    sn <- data.out.num[data.out.num$CountryName==cn,]
    sn <- sn[order(sn$CityCode),]
    # sp <- data.out.proc[data.out.proc$CountryName==cn,]
    
    par(mfrow=c(2,2), oma=c(0,0,2,0))
    
    for (pr in unique(sn$ProductName)){
      
      frame1 <- sn[sn$ProductName==pr,c("CityName", "CityCode", year)]
      
      pr.id <- unique(sn[sn$ProductName==pr,]$ProductID) 
      
      rawd <- veh[veh$CountryCode==unique(sn$CountryCode) & veh$ProductID==pr.id,]
      
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
    #   sraw <- veh[veh$RegionCode==city & veh$ProductID %in% c(2666, 2667, 2668),]
    # 
    #   if (nrow(sraw)==0){
    # 
    #     sraw <- veh[veh$RegionCode==city & veh$ProductID %in% c(2001, 2002, 2003),]
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
}



