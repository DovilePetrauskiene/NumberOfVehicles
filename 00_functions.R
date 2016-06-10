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
country.ind.2000 <- function(veh1, expf){
  
  countries <- unique(expf[expf$exp=="country678",]$CountryCode)
  
  out.data <- NULL
  for (cn in countries){
    
    cin <- veh1[veh1$RegionCode==cn & veh1$ProductID %in% c(2666, 2667, 2668), 
                c("ProductID", "ProductName", as.character(2000:2016))]
    cin[,as.character(2000:2016)] <- apply(cin[,as.character(2000:2016)], 2, function(x){
      x/sum(x)
    })
    
    cities <- unique(expf[expf$exp=="country678" & expf$CountryCode==cn, ]$CityCode)
    
    for (cc in cities){
      
      cc2000 <- veh1[veh1$RegionCode==cc,]
      cc.new <- cin
      cc.new[, as.character(2000:2016)] <- do.call("rbind",apply(cc.new[, as.character(2000:2016)], 1, function(x){
        x*cc2000[, as.character(2000:2016)]})) 
      cc2000 <- cc2000[,c("RegionCode", "Region/CityName", "CountryName", "CountryCode", "Unit")]
      cc.new <- cbind(rbind(cc2000, cc2000, cc2000), cc.new)
      
      out.data <- rbind(out.data, cc.new)
    }
    
  }
  
  veh1 <- veh1[!(veh1$CountryCode %in% countries),]
  veh1 <- rbind(veh1, out.data)
  
  return(veh1)
}


#indicator 2668 missing, it is going to be calculated from indicators 2000, 2667, 2666 (2668=2000-2667-2666)
#miss.ind

missing.ind <- function(veh1, expf){
  
  cities <- expf[expf$exp=="miss.ind",]$CityCode
  
  data.out <- NULL
  for (cc in cities){
    
    year <- as.character(2000:2016)
    
    cc.new <- veh1[veh1$RegionCode==cc,]
    cc.new <- rbind(cc.new, cc.new[3,])
    cc.new[4,]$ProductID <- 2668
    cc.new[cc.new$ProductID==2668,year] <- cc.new[cc.new$ProductID==2000, year] -
      cc.new[cc.new$ProductID==2666, year] - cc.new[cc.new$ProductID==2667, year]
    
    data.out <- rbind(data.out, cc.new)
    
  }
  
  data.out <- data.out[data.out$ProductID!=2000,]
  
  veh1 <- veh1[!(veh1$RegionCode %in% cities),]
  veh1 <- rbind(veh1, data.out)
  
  return(veh1)
}

country678.cases.founder <- function(veh1, city.codes){
  
  country678 <- NULL
  for (cc in veh1$RegionCode){
    
    s <- veh1[veh1$RegionCode==cc & veh1$ProductID %in% c(2000, 2666, 2668, 2667,
                                                          2001, 2002, 2003),]
    
    if (nrow(s)==1){
      
      if (s$ProductID==2000){
        
        cn.code <- city.codes[city.codes$CityCode==cc,]$CountryCode
        
        if (any(c(2666, 2668, 2667) %in% veh1[veh1$RegionCode==cn.code,]$ProductID) ||
            any(c(2001, 2002, 2003) %in% veh1[veh1$RegionCode==cn.code,]$ProductID)){
          
          country678 <- c(country678, cc)
        }
      }
    }
  }
  
  return(country678)
}


trend2000.cases.finder <- function(veh1){
  
  trend2000 <- NULL
  
  year <- as.character(2000:2016)
  for (cc in unique(veh1$RegionCode)){
    
    s <- veh1[veh1$RegionCode==cc & veh1$ProductID %in% c(2000, 2666, 2667, 2668),]
    
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
