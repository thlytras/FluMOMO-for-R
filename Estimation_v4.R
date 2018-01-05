# Load required packages
library(readstata13)

# Function to get lag, forward and backward
vecshift <- function(x, shift=0) {
  if (shift==0) return(x)
  if (shift>0) return(c(x[-(1:shift)], rep(NA,shift)))
  if (shift<0) return(c(rep(NA, -shift), x[1:(length(x)+shift)]))
}

# Function to calculate ISO week
isoweek <- function(x, type="both_num", sep="-", inv=FALSE, colnames=c("isoyear","isoweek")) {
  alts=c("week","year","both_text","both_num","matrix")
  if(!(type %in% alts)) stop("Unknown isoweek type requested!")
  x.date<-as.Date(x)
  x.weekday<-as.integer(format(x.date,"%w"))
  x.weekday[x.weekday==0]=7
  x.nearest.thu<-x.date-x.weekday+4
  x.isoyear<-as.integer(substring(x.nearest.thu,1,4))
  x.isoweek<-(as.integer(x.nearest.thu-as.Date(paste(x.isoyear,"-1-1",sep="")))%/%7)+1
  switch(type,
    week = x.isoweek,
    year = x.isoyear,
    both_text = if (inv) {
      ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoweek,x.isoyear,sep=sep))
    } else {
      ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,paste(x.isoyear,x.isoweek,sep=sep))
    },
    both_num = ifelse((is.na(x.isoyear) | is.na(x.isoweek)),NA,x.isoyear*100+x.isoweek),
    matrix = if (inv) {
      `colnames<-`(cbind(x.isoweek, x.isoyear), rev(colnames))
    } else {
      `colnames<-`(cbind(x.isoyear, x.isoweek), colnames)
    }
  )
}


ptrend <- 0.05
p26 <- 0.05
p52 <- 0.10

# ** START: deaths data **
# Death data from A-MOMO 
if (A_MOMO == 1) {
  deaths <- read.table(paste0(indir,"/A-MOMO data.txt"), sep=",", dec=".", header=T)[,c("group","YoDi","WoDi","nb","nbc")]
  deaths$deaths <- pmax(deaths$nb, deaths$nbc)
  deaths$agegrp <- match(deaths$group, c("0to4","5to14","15to64","65P","Total")) - 1  # Convert group to integer (0-5)
  deaths <- deaths[!is.na(deaths$agegrp), c("agegrp", "YoDi", "WoDi", "deaths")]
  names(deaths)[2:3] <- c("year","week")
}
if (A_MOMO != 1) {
  deaths <- read.table(paste0(indir,"/deaths.csv"), sep=",", dec=".", header=T)[,c("agegrp","year","week","deaths")]
}
deaths <- subset(deaths, (year*100+week >= start_year*100+start_week) & (year*100+week <= end_year*100+end_week))

deaths <- deaths[order(deaths$agegrp,deaths$year,deaths$week),]
# ** END: deaths data **


# ** START: Temperature data **
ET <- read.dta13(paste0(indir,"/daily.dta"), nonint.factors=TRUE)[,c("date","pop3","nuts3","temp")]
ET[is.na(ET$pop3)]$pop3 <- 1
ET <- ET[order(ET$date),]  # Ordering speeds summarizing a bit
ET <- aggregate(cbind(temp, pop3) ~ nuts3 + date, data=ET, FUN = mean)
ET$pop3.sum <- with(ET, tapply(pop3, date, sum))[as.character(ET$date)]
ET$temp <- ET$temp * ET$pop3 / ET$pop3.sum
ET <- aggregate(ET[,"temp",drop=FALSE], ET[,"date", drop=FALSE], sum)
ET$ISOweek <- isoweek(ET$date)
ET <- subset(ET, (ISOweek >= start_year*100+start_week) & (ISOweek <= end_year*100+end_week))
ET <- aggregate(ET[,"temp"], ET[,"ISOweek", drop=F], function(x) c(mean(x), range(x)))
ET <- as.data.frame.matrix(do.call(cbind, as.list(ET)))
names(ET)[2:4] <- c("temp","tmin","tmax")
ET$wk <- order(ET$ISOweek)
ET$sin52 <- sin((2*pi/(365.25/7))*ET$wk)
ET$cos52 <- cos((2*pi/(365.25/7))*ET$wk)
ET$ptemp <- predict(lm(temp ~ sin52 + cos52, data=ET))
ET$ptmin <- predict(lm(tmin ~ sin52 + cos52, data=ET))
ET$ptmax <- predict(lm(tmax ~ sin52 + cos52, data=ET))
ET[,c("wk","cos52","sin52")] <- NULL
ET$ET <- with(ET, (temp-ptmax)*(temp>ptmax)+(temp-ptmin)*(temp<ptmin))
ET$year <- ET$ISOweek %/% 100
ET$week <- ET$ISOweek %% 100
ET$ISOweek <- NULL
ET <- ET[order(ET$year,ET$week),]
write.table(ET, file=paste0(indir,"/temperature.csv"), row.names = FALSE, quote = FALSE, sep =";", dec=".")
# ** END: Temperature data **



# ** START: Merge data **
if (file.exists(paste0(indir,"/IA.csv"))) {
  results4 <- read.table(paste0(indir,"/IA.csv"), sep=",", dec=".", header=TRUE)
} else if (file.exists(paste0(indir,"/IA.dta"))) {
  results4 <- read.dta13(paste0(indir,"/IA.dta"))
} else stop("Cannot find Influenza Activity data (IA.csv or IA.dta)")
results4 <- merge(deaths, results4,  by=c("agegrp","year","week"), all=FALSE)
results4 <- merge(results4,ET[,c("year","week","ET")], by=c("year","week"), all=FALSE)
rm(deaths, ET)

if (population) {
  if (file.exists(paste0(indir,"/population.csv"))) {
    pop_data <- read.table(paste0(indir,"/population.csv"), sep=",", dec=".", header=TRUE)
  } else if (file.exists(paste0(indir,"/population.dta"))) {
    pop_data <- read.dta13(paste0(indir,"/population.dta"))
  } else stop("Cannot find Population data (population.csv or population.dta)")
  results4 <- merge(results4, pop_data[,c("agegrp","year","week","N")],  by=c("agegrp","year","week"), all=FALSE)
  rm(pop_data)
} else {
  results4$N <- 1
}

results4 <- subset(results4, (year*100+week >= start_year*100+start_week) & (year*100+week <= end_year*100+end_week))
results4 <- results4[with(results4, order(agegrp, year, week)),]

results4$season <- with(results4, year - (week<27)) # Subtract 1 if week < 27
results4$summer <- with(results4, week>=21 & results4$week<=39)
results4$winter <- !results4$summer

# lags IA
for (a in 0:4) {
  for (s in sort(unique(results4[results4$agegrp==a, "season"]))) {
    for (d in 0:IAlags) {
      results4[results4$agegrp==a, paste("d", d, "_IA", s, sep="")] <- vecshift(results4[results4$agegrp==a, "IA"], -d) * as.numeric(results4[results4$agegrp==a, "season"]==s)
    }
  }
}
# warm/cold summer/winter and lags
for (s in c("summer","winter")) {
  results4[,paste0("cold_", s)] <- with(results4, -((ET<0) & get(s)) * ET)
  results4[,paste0("warm_", s)] <- with(results4, ((ET>0) & get(s)) * ET)
  for (t in c("cold","warm")) {
    for (d in 0:ETlags) {
      results4[,paste0("d", d, "_", t, "_", s)] <- NA
      for (a in 0:4) {
        results4[results4$agegrp==a,paste0("d", d, "_", t, "_", s)] <- vecshift(results4[results4$agegrp==a,paste0( t, "_", s)], -d)
      }
    }
  }
  results4[,paste0("cold_", s)] <- NULL
  results4[,paste0("warm_", s)] <- NULL
}
rm(a, s, t, d)
results4$summer <- NULL
results4$winter <- NULL

# Create wk
results4 <- results4[with(results4, order(agegrp, year, week)),]
results4$wk <- unlist(lapply(table(results4$agegrp), seq))

results4$sin52 <- sin((2*pi/(365.25/7)) * results4$wk)
results4$cos52 <- cos((2*pi/(365.25/7)) * results4$wk)
results4$sin26 <- sin((4*pi/(365.25/7)) * results4$wk)
results4$cos26 <- cos((4*pi/(365.25/7)) * results4$wk)

# ** END: Merge data **



results4$Eall <- NA
results4$VEall <- NA
results4$EB <- NA
results4$VEB <- NA
results4$EIA <- NA
results4$VEIA <- NA
results4$EET <- NA
results4$VEET <- NA
results4$RVB <- NA


suppressWarnings({ # I'm getting rank-deficient fits due to variable 'wk'. Why? Suppressing for now...

for (a in 0:4) {
  f <- paste(c("deaths ~ wk + sin52 + cos52 + sin26 + cos26", 
    grep("^d[0-9]", names(results4), value=TRUE)), collapse=" + ")
  m <- glm(f, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  fa <- paste(c("deaths ~ sin52 + cos52 + sin26 + cos26",
    grep("^d[0-9]", names(results4), value=TRUE)), collapse=" + ")
  ma <- glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>ptrend) {
    results4[results4$agegrp==a,]$wk <- 0
    m <- glm(f, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  }
  fa <- paste(c("deaths ~ wk", grep("^d[0-9]", names(results4), value=TRUE)), collapse=" + ")
  ma <- glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>max(p26,p52)) {
    m <- ma   # We don't need to fit it again
  } else {
    fa <- paste(c("deaths ~ wk + cos52 + sin52", grep("^d[0-9]", names(results4), value=TRUE)), collapse=" + ")
    ma <- glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
    if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>p26) {
      m <- ma   # We don't need to fit it again
    } else {
      fa <- paste(c("deaths ~ wk", grep("^d[0-9]", names(results4), value=TRUE)), collapse=" + ")
      ma <- glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
      if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>p52) {
        m <- ma   # We don't need to fit it again
      }
    }
  }
  print(summary(m))
  
  results4[results4$agegrp==a,]$Eall <- exp(predict.glm(m, newdata=results4[results4$agegrp==a,], se.fit=TRUE)$fit)
  results4[results4$agegrp==a,]$VEall <- predict.glm(m, newdata=results4[results4$agegrp==a,], se.fit=TRUE)$se.fit^2

  results4.B <- results4[results4$agegrp==a,]
  for (d in 0:IAlags) {
    for (s in min(results4.B$season):max(results4.B$season)) {
      results4.B[,paste("d", d, "_IA", s, sep="")] <- 0
    }
    for (s in c("summer","winter")) {
      results4.B[,paste("d", d, "_warm_", s, sep="")] <- 0
      results4.B[,paste("d", d, "_cold_", s, sep="")] <- 0
    }
  }
  results4[results4$agegrp==a,]$EB <- exp(predict.glm(m, newdata=results4.B, se.fit=TRUE)$fit)
  results4[results4$agegrp==a,]$VEB <- predict.glm(m, newdata=results4.B, se.fit=TRUE)$se.fit^2

  results4.IA <- results4[results4$agegrp==a,]
  for (s in c("summer","winter")) {
    for (d in 0:ETlags) {
      results4.IA[,paste("d", d, "_warm_", s, sep="")] <- 0
      results4.IA[,paste("d", d, "_cold_", s, sep="")] <- 0
    }
  }
  results4[results4$agegrp==a,]$EIA <- exp(predict.glm(m, newdata=results4.IA, se.fit=TRUE)$fit)
  results4[results4$agegrp==a,]$VEIA <- predict.glm(m, newdata=results4.IA, se.fit=TRUE)$se.fit^2
  
  results4.ET <- results4[results4$agegrp==a,]
  for (d in 0:IAlags) {
    for (s in sort(unique(results4.ET$season))) {
      results4.ET[,paste("d", d, "_IA", s, sep="")] <- 0
    }
  }
  results4[results4$agegrp==a,]$EET <-  exp(predict.glm(m, newdata=results4.ET, se.fit=T)$fit)
  results4[results4$agegrp==a,]$VEET <- predict.glm(m, newdata=results4.ET, se.fit=T)$se.fit^2

  results4[results4$agegrp==a,]$RVB <- with(results4[results4$agegrp==a,], 
    ((2/3)*(EB^(2/3-1))^2)*summary(m)$dispersion*EB + 
    ((2/3)*(EB^(2/3-1))^2)*(exp(VEB)-1)*exp(2*log(EB)+VEB) )
    
}

})   # suppressWarnings() end here


# Delete variables in results4
results4[,c("wk","sin52","cos52","sin26","cos26")] <- NULL

for (d in 0:IAlags) {
  for (s in sort(unique(results4$season))) {
    results4[,paste("d", d, "_IA", s, sep="")] <- NULL
  }
  for (s in c("summer","winter")) {
    results4[,paste("d", d, "_warm_", s, sep="")] <- NULL
    results4[,paste("d", d, "_cold_", s, sep="")] <- NULL
  }
}
results4$season <- NULL
# Delete external objects
rm(a, f, fa, s, d, m, ma, results4.B, results4.ET, results4.IA, ptrend, p26, p52)

results4$EdIA <- results4$Eall - results4$EET
results4[is.na(results4$EdIA),"EdIA"] <- 0
results4$EdET <- results4$Eall - results4$EIA
results4[is.na(results4$EdET),"EdET"] <- 0

results4$RVdIA <- with(results4, ((2/3)*(Eall^(2/3-1))^2)*(exp(VEall)-1)*exp(2*log(Eall)+VEall) + ((2/3)*(EET^(2/3-1))^2)*(exp(VEET)-1)*exp(2*log(EET)+VEET))
results4[with(results4, is.na(RVdIA) | is.infinite(RVdIA)), "RVdIA"] <- 0
results4$RVdET <- with(results4, ((2/3)*(Eall^(2/3-1))^2)*(exp(VEall)-1)*exp(2*log(Eall)+VEall) + ((2/3)*(EIA^(2/3-1))^2)*(exp(VEIA)-1)*exp(2*log(EIA)+VEIA))
results4[with(results4, is.na(RVdET) | is.infinite(RVdET)), "RVdET"] <- 0

results4$excess <- results4$deaths - results4$EB
results4$uexcess <- results4$deaths - (results4$EB + results4$EdIA + results4$EdET)
# ## Exclude negative IA effects ####
if (IArest == 1) {
  results4$uexcess <- with(results4, deaths - (EB + pmax(0,EdIA) + EdET))
}

results4 <- results4[with(results4, order(agegrp, year, week)),]

results4$EB_95L <- with(results4, pmax(0,sign((sign(EB)*abs(EB)^(2/3))-1.96*sqrt(RVB))*abs((sign(EB)*abs(EB)^(2/3))-1.96*sqrt(RVB))^(3/2)))
results4$EB_95U <- with(results4, sign((sign(EB)*abs(EB)^(2/3))+1.96*sqrt(RVB))*abs((sign(EB)*abs(EB)^(2/3))+1.96*sqrt(RVB))^(3/2))

results4$EdIA_95L <- with(results4, pmax(0,sign((sign(EdIA)*abs(EdIA)^(2/3))-1.96*sqrt(RVdIA))*abs((sign(EdIA)*abs(EdIA)^(2/3))-1.96*sqrt(RVdIA))^(3/2)))
results4$EdIA_95U <- with(results4, pmax(EdIA_95L,sign((sign(EdIA)*abs(EdIA)^(2/3))+1.96*sqrt(RVdIA))*abs((sign(EdIA)*abs(EdIA)^(2/3))+1.96*sqrt(RVdIA))^(3/2)))
results4$EdET_95L <- with(results4, pmax(0,sign((sign(EdET)*abs(EdET)^(2/3))-1.96*sqrt(RVdET))*abs((sign(EdET)*abs(EdET)^(2/3))-1.96*sqrt(RVdET))^(3/2)))
results4$EdET_95U <- with(results4, pmax(EdET_95L,sign((sign(EdET)*abs(EdET)^(2/3))+1.96*sqrt(RVdET))*abs((sign(EdET)*abs(EdET)^(2/3))+1.96*sqrt(RVdET))^(3/2)))

results4 <- results4[with(results4, order(agegrp, year, week)),]
results4$wk <- unlist(lapply(table(results4$agegrp), seq))

results4$summer <- with(results4, ifelse(week>=21 & week<=39, year, NA))
results4$winter <- with(results4, ifelse(week<=20 | week>=40, year - (week<=20), NA))


cummulate <- function(x, pos=FALSE) {
  x[is.na(x)] <- 0
  if (pos) x <- pmax(0,x)
  return(cumsum(x))
}

cummulateVE <- function(x, pos=FALSE) {
  x[is.na(x)] <- 0
  y <- (exp(x[,2])-1)*exp(2*log(x[,1])+x[,2])
  if (pos) y <- y * (x[,1]>=0)
  y[is.infinite(y)] <- 0
  return(cumsum(y))
}

VEd <- function(x, pos=FALSE) {
  x[is.na(x)] <- 0
  y <- (2/3)*(abs((x[,2])^(2/3-1))^2)*(x[,4] + x[,3])
  if (pos) y <- y * (x[,1]>=0)
  y[x[,2]==0] <- 0
  return(y)
}

for (s in c("summer","winter","year")) {
  nn <- !is.na(results4[,s])  # logical vector indicating the non-missings
  for (v in c("excess","uexcess")) {
    results4[,paste0("c", v, "_", s)] <- NA
    for (a in 0:4) {
      ag <- (results4$agegrp==a)
      for (sy in sort(unique(results4[,s]))) {
        results4[(results4[,s]==sy) & nn & ag, paste0("c", v, "_", s)] <-
            cummulate(results4[(results4[,s]==sy) & nn & ag, v], FALSE)
      }
    }
  }
  for (v in c("EIA","EET","Eall")) {
    results4[,paste0("c", v, "_", s)] <- NA
    results4[,paste0("Vc", v, "_", s)] <- NA
    for (a in 0:4) {
      ag <- (results4$agegrp==a)
      for (sy in sort(unique(results4[,s]))) {
        results4[(results4[,s]==sy) & nn & ag, paste0("c", v, "_", s)] <- 
            cummulate(results4[(results4[,s]==sy) & nn & ag, v], FALSE)
        results4[(results4[,s]==sy) & nn & ag, paste0("Vc", v, "_", s)] <-
            cummulateVE(results4[(results4[,s]==sy) & nn & ag, c(v,paste0("V",v))], FALSE)
      }
    }
  }
  for (v in c("IA","ET")) {
    results4[,paste0("cEd", v, "_", s)] <- NA
    results4[,paste0("VcEd", v, "_", s)] <- NA
    for (a in 0:4) {
      ag <- (results4$agegrp==a)
      for (sy in sort(unique(results4[,s]))) {
        results4[(results4[,s]==sy) & nn & ag, paste0("cEd", v, "_", s)] <- 
            cummulate(results4[(results4[,s]==sy) & nn & ag, paste0("Ed",v)], FALSE)
        results4[(results4[,s]==sy) & nn & ag, paste0("VcEd", v, "_", s)] <-
            VEd(results4[
                (results4[,s]==sy) & nn & ag, c(paste0("Ed",v), 
                paste0("cE", v, "_", s), paste0("VcE", v, "_", s), paste0("VcEall_", s))
              ], FALSE)
      }
    }
    results4[nn,paste0("cEd",v,"_",s,"_95L")] <-
        sign(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) - 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)])) * (abs(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) - 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)]))^(3/2))
        
    results4[nn,paste0("cEd",v,"_",s,"_95U")] <- 
        sign(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) + 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)])) * (abs(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) + 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)]))^(3/2))
  }
  # ## Exclude negative IA effects ##
  if (IArest == 1 ) {
    for (v in c("EIA")) {
      results4[,paste0("c", v, "_", s)] <- NA
      results4[,paste0("Vc", v, "_", s)] <- NA
      for (a in 0:4) {
        ag <- (results4$agegrp==a)
        for (sy in sort(unique(results4[,s]))) {
          results4[(results4[,s]==sy) & nn & ag, paste0("c", v, "_", s)] <- 
              cummulate(results4[(results4[,s]==sy) & nn & ag, v], TRUE)
          results4[(results4[,s]==sy) & nn & ag, paste0("Vc", v, "_", s)] <- 
              cummulateVE(results4[(results4[,s]==sy) & nn & ag, c(v,paste0("V",v))], TRUE)
        }
      }
    }
    for (v in c("IA")) {
      results4[,paste0("cEd", v, "_", s)] <- NA
      results4[,paste0("VcEd", v, "_", s)] <- NA
      for (a in 0:4) {
        ag <- (results4$agegrp==a)
        for (sy in sort(unique(results4[,s]))) {
          results4[(results4[,s]==sy) & nn & ag, paste0("cEd", v, "_", s)] <-
              cummulate(results4[(results4[,s]==sy) & nn & ag, paste0("Ed",v)], TRUE)
          results4[(results4[,s]==sy) & nn & ag, paste0("VcEd", v, "_", s)] <- 
              VEd(results4[(results4[,s]==sy) & nn & ag, c(paste0("Ed",v), 
                  paste0("cE", v, "_", s), paste0("VcE", v, "_", s), paste0("VcEall_", s))], TRUE)
        }
      }
      results4[nn, paste0("cEd",v,"_",s,"_95L")] <-
          sign(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) - 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)])) * (abs(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) - 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)]))^(3/2))
      results4[nn,paste0("cEd",v,"_",s,"_95U")] <- 
          sign(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) + 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)])) * (abs(sign(results4[nn,paste0("cEd",v,"_",s)]) * (abs(results4[nn,paste0("cEd",v,"_",s)])^(2/3)) + 1.96*sqrt(results4[nn,paste0("VcEd",v,"_",s)]))^(3/2))
    }
  }
}
results4$country <- country
results4$IArestricted <- as.integer(IArest)
results4 <- results4[, c("country", "IArestricted", "agegrp", "year", "week", "deaths", "N", 
        "IA", "VEIA", "ET", "VEET", 
        "EB", "EB_95L", "EB_95U", "RVB", 
        "EdIA", "EdIA_95L", "EdIA_95U", "RVdIA", 
        "EdET", "EdET_95L", "EdET_95U", "RVdET", 
        "cexcess_year", "cuexcess_year", 
        "cEdIA_year", "cEdIA_year_95L", "cEdIA_year_95U", 
        "cEdET_year", "cEdET_year_95L", "cEdET_year_95U", 
        "summer", "cexcess_summer", "cuexcess_summer", 
        "cEdIA_summer", "cEdIA_summer_95L", "cEdIA_summer_95U", 
        "cEdET_summer", "cEdET_summer_95L", "cEdET_summer_95U", 
        "winter", "cexcess_winter", "cuexcess_winter", 
        "cEdIA_winter", "cEdIA_winter_95L", "cEdIA_winter_95U", 
        "cEdET_winter", "cEdET_winter_95L", "cEdET_winter_95U")]

if (IArest) { 
  write.table(results4, file=paste0(outdir,"/",country,"_output_v4_IArestricted.csv"), 
      row.names = FALSE, quote = FALSE, sep =";", dec=".", na="") 
} else { 
  write.table(results4, file=paste0(outdir,"/",country,"_output_v4.csv"), 
      row.names = FALSE, quote = FALSE, sep =";", dec=".", na="")
}

rm(a, s, sy, v, nn, ag, cummulate, cummulateVE, vecshift, VEd, results4)
