### Load packages ############################################################
required_packages <- c("foreign", "doBy", "ISOweek", "readstata13")
for (i in seq(along = required_packages)) require(required_packages[i], , quietly=T, character.only = T)
rm(required_packages,i)
##############################################################################

# Function to get lag, forward and backward
vecshift <- function(x, shift=0) {
  if (shift==0) return(x)
  if (shift>0) return(c(x[-(1:shift)], rep(NA,shift)))
  if (shift<0) return(c(rep(NA, -shift), x[1:(length(x)+shift)]))
}

ptrend=0.05
p26=0.05
p52=0.10

### START: deaths data ###
# Death data from A-MOMO ###
if (A_MOMO == 1) {
  deaths <- read.table(paste0(indir,"/A-MOMO data.txt"), sep=",", dec=".", header=T)[,c("group","YoDi","WoDi","nb","nbc")]
  deaths <- deaths[deaths$group %in% c("0to4","5to14","15to64","65P","Total"),]
  names(deaths)[names(deaths) %in% c("YoDi","WoDi","nbc")] = c("year","week","deaths")
  deaths$deaths = pmax(deaths$nb,deaths$deaths)
  deaths$nb <- NULL
  deaths$agegrp = NA
  deaths[deaths$group=="0to4",]$agegrp = 0
  deaths[deaths$group=="5to14",]$agegrp = 1
  deaths[deaths$group=="15to64",]$agegrp = 2
  deaths[deaths$group=="65P",]$agegrp = 3
  deaths[deaths$group=="Total",]$agegrp = 4
  deaths$group <- NULL
}
if (A_MOMO != 1) {
  deaths <- read.table(paste0(indir,"/deaths.csv"), sep=",", dec=".", header=T)[,c("agegrp","year","week","deaths")]
}
deaths <- deaths[(start_year*100+start_week<=deaths$year*100+deaths$week)&(deaths$year*100+deaths$week<=end_year*100+end_week),]
deaths <- deaths[order(deaths$agegrp,deaths$year,deaths$week),]
### END: deaths data ###

### START: Temperature data ###
ET <- read.dta(paste0(indir,"/daily.dta"),warn.missing.labels = F)[,c("date","pop3","nuts3","temp")]
ET[is.na(ET$pop3)]$pop3=1
ET <- summaryBy(temp + pop3 ~ nuts3 + date, data=ET, FUN = mean)
ET <- merge(ET, summaryBy(pop3.mean ~ date, data=ET, FUN = sum), by="date")
ET$temp = ET$temp.mean*ET$pop3.mean/ET$pop3.mean.sum
ET <- summaryBy(temp ~ date, data=ET, FUN = sum)
ET$ISOweek = ISOweek(ET$date)
ET$year = as.numeric(substr(ET$ISOweek,1,4))
ET$week = as.numeric(substr(ET$ISOweek,7,8))
ET <- ET[(start_year*100+start_week<=ET$year*100+ET$week)&(ET$year*100+ET$week<=end_year*100+end_week),]
ET <- summaryBy(temp.sum ~ ISOweek, data=ET, FUN = c(mean,min,max))
names(ET)[names(ET) %in% c("temp.sum.mean","temp.sum.min","temp.sum.max")] = c("temp","tmin","tmax")
ET$wk = as.numeric(factor(ET$ISOweek))
ET <- within(ET,paste(
  sin52 <- sin((2*pi/(365.25/7))*wk),
  cos52 <- cos((2*pi/(365.25/7))*wk)
))
ET$ptemp = predict(lm(temp ~ sin52 + cos52, data=ET), se.fit=F)
ET$ptmin = predict(lm(tmin ~ sin52 + cos52, data=ET), se.fit=F)
ET$ptmax = predict(lm(tmax ~ sin52 + cos52, data=ET), se.fit=F)
ET[,c("wk","cos52","sin52")] <- NULL
ET$ET=(ET$temp-ET$ptmax)*(ET$temp>ET$ptmax)+(ET$temp-ET$ptmin)*(ET$temp<ET$ptmin)
ET$year = as.numeric(substr(ET$ISOweek,1,4))
ET$week = as.numeric(substr(ET$ISOweek,7,8))
ET[,"ISOweek"] <- NULL
ET <- ET[order(ET$year,ET$week),]

write.table(ET, file=paste0(indir,"/temperature.csv"), row.names = F, quote = F, sep =";", dec=".")

### END: Temperature data ###

### START: Merge data ###
results4 <- merge(deaths,read.table(paste0(indir,"/IA.csv"), sep=",", dec=".", header=T),  by=c("agegrp","year","week"), all=F)
rm(deaths)
results4 <- merge(results4,ET[,c("year","week","ET")], by=c("year","week"), all=F)
rm(ET)
if (population == 1) {
  results4 <- merge(results4,read.table(paste0(indir,"/population.csv"), sep=",", dec=".", header=T)[,c("agegrp","year","week","N")],  by=c("agegrp","year","week"), all=F)
}
if (population != 1) {results4$N = 1}
results4 <- results4[(start_year*100+start_week<=results4$year*100+results4$week)&(results4$year*100+results4$week<=end_year*100+end_week),]

results4 <- results4[with(results4, order(agegrp, year, week)),]

results4$season = results4$year*(results4$week>=27)+(results4$year-1)*(results4$week<27)
results4$summer = (21<=results4$week)*(results4$week<=39)
results4$winter = as.numeric(results4$summer==0)

# lags IA
for (a in 0:4) {
  for (s in min(results4[results4$agegrp==a,]$season):max(results4[results4$agegrp==a,]$season)) {
    for (d in 0:IAlags) {
      results4[results4$agegrp==a,paste("d", d, "_IA", s, sep="")] = vecshift(results4[results4$agegrp==a,]$IA, -d)*as.numeric(results4[results4$agegrp==a,]$season==s)
    }
  }
}
# warm/cold summer/winter and lags
for (s in c("summer","winter")) {
  results4[,paste0("cold_", s)] = -as.numeric((results4[,"ET"]<0)&(results4[,s]==1))*results4[,"ET"]
  results4[,paste0("warm_", s)] = as.numeric((results4[,"ET"]>0)&(results4[,s]==1))*results4[,"ET"]
  for (t in c("cold","warm")) {
    for (d in 0:ETlags) {
      results4[,paste0("d", d, "_", t, "_", s)] <- NA
      for (a in 0:4) {
        results4[results4$agegrp==a,paste0("d", d, "_", t, "_", s)] = vecshift(results4[results4$agegrp==a,paste0( t, "_", s)], -d)
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
results4$wk <- unlist(tapply(results4$agegrp,results4$agegrp,function(x) seq(1,length(x),1)))

results4 <- within(results4, {
  sin52 <- sin((2*pi/(365.25/7))*wk)
  cos52 <- cos((2*pi/(365.25/7))*wk)
  sin26 <- sin((4*pi/(365.25/7))*wk)
  cos26 <- cos((4*pi/(365.25/7))*wk)
})

results4$Eall=NA
results4$VEall=NA
results4$EB=NA
results4$VEB=NA
results4$EIA=NA
results4$VEIA=NA
results4$EET=NA
results4$VEET=NA
results4$RVB=NA

for (a in 0:4) {
  f = paste("deaths ~ wk + sin52 + cos52 + sin26 + cos26 +",paste(grep("^d[0-9]",names(results4),value=T),collapse=" + "))
  m = glm(f, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  fa = paste("deaths ~ sin52 + cos52 + sin26 + cos26 +",paste(grep("^d[0-9]",names(results4),value=T),collapse=" + "))
  ma = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>ptrend) {
    results4[results4$agegrp==a,]$wk=0
    m = glm(f, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  }
  fa = paste("deaths ~ wk +",paste(grep("^d[0-9]",names(results4),value=T),collapse=" + "))
  ma = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>max(p26,p52)) {
    m = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
  }
  else {
    fa = paste("deaths ~ wk + cos52 + sin52 +",paste(grep("^d[0-9]",names(results4),value=T),collapse=" + "))
    ma = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
    if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>p26) {
      m = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
    }
    else {
      fa = paste("deaths ~ wk +",paste(grep("^d[0-9]",names(results4),value=T),collapse=" + "))
      ma = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
      if (anova(m,ma, test="LRT")$`Pr(>Chi)`[2]>p52) {
        m = glm(fa, quasipoisson, offset = log(N), data=results4[results4$agegrp==a,])
      }
    }
  }
  print(summary(m))
  results4[results4$agegrp==a,]$Eall = exp(predict.glm(m, newdata=results4[results4$agegrp==a,], se.fit=T)$fit)
  results4[results4$agegrp==a,]$VEall = predict.glm(m, newdata=results4[results4$agegrp==a,], se.fit=T)$se.fit^2

  results4.B <- results4[results4$agegrp==a,]
  for (d in 0:IAlags) {
    for (s in min(results4.B$season):max(results4.B$season)) {
      results4.B[,paste("d", d, "_IA", s, sep="")] = 0
    }
    for (s in c("summer","winter")) {
      results4.B[,paste("d", d, "_warm_", s, sep="")] = 0
      results4.B[,paste("d", d, "_cold_", s, sep="")] = 0
    }
  }
  results4[results4$agegrp==a,]$EB = exp(predict.glm(m, newdata=results4.B, se.fit=T)$fit)
  results4[results4$agegrp==a,]$VEB = predict.glm(m, newdata=results4.B, se.fit=T)$se.fit^2

  results4.IA <- results4[results4$agegrp==a,]
  for (s in c("summer","winter")) {
    for (d in 0:ETlags) {
      results4.IA[,paste("d", d, "_warm_", s, sep="")] = 0
      results4.IA[,paste("d", d, "_cold_", s, sep="")] = 0
    }
  }
  results4[results4$agegrp==a,]$EIA = exp(predict.glm(m, newdata=results4.IA, se.fit=T)$fit)
  results4[results4$agegrp==a,]$VEIA = predict.glm(m, newdata=results4.IA, se.fit=T)$se.fit^2
  
  results4.ET <- results4[results4$agegrp==a,]
  for (d in 0:IAlags) {
    for (s in min(results4.ET$season):max(results4.ET$season)) {
      results4.ET[,paste("d", d, "_IA", s, sep="")] = 0
    }
  }
  results4[results4$agegrp==a,]$EET = exp(predict.glm(m, newdata=results4.ET, se.fit=T)$fit)
  results4[results4$agegrp==a,]$VEET= predict.glm(m, newdata=results4.ET, se.fit=T)$se.fit^2

  results4[results4$agegrp==a,]$RVB = within(results4[results4$agegrp==a,c("EB","VEB","RVB")], {
                                        RVB = ((2/3)*(EB^(2/3-1))^2)*summary(m)$dispersion*EB + ((2/3)*(EB^(2/3-1))^2)*(exp(VEB)-1)*exp(2*log(EB)+VEB)
                                      })$RVB
}

# Delete variables in results4
for (v in c("wk","sin52","cos52","sin26","cos26")) {
  results4[,v] <- NULL
}
for (d in 0:IAlags) {
  for (s in min(results4$season):max(results4$season)) {
    results4[,paste("d", d, "_IA", s, sep="")] <- NULL
  }
  for (s in c("summer","winter")) {
    results4[,paste("d", d, "_warm_", s, sep="")] <- NULL
    results4[,paste("d", d, "_cold_", s, sep="")] <- NULL
  }
}
results4$season <- NULL
# Delete external objects
rm(a, f, fa, s, v, d, m, ma, results4.B, results4.ET, results4.IA, ptrend, p26, p52)

results4$EdIA =  results4$Eall-results4$EET
results4[is.na(results4$EdIA),"EdIA"] = 0
results4$EdET =  results4$Eall-results4$EIA
results4[is.na(results4$EdET),"EdET"] = 0

results4 <- transform(results4, RVdIA=((2/3)*(Eall^(2/3-1))^2)*(exp(VEall)-1)*exp(2*log(Eall)+VEall) + ((2/3)*(EET^(2/3-1))^2)*(exp(VEET)-1)*exp(2*log(EET)+VEET))
results4[(is.na(results4$RVdIA)|is.infinite(results4$RVdIA)),]$RVdIA = 0
results4 <- transform(results4, RVdET=((2/3)*(Eall^(2/3-1))^2)*(exp(VEall)-1)*exp(2*log(Eall)+VEall) + ((2/3)*(EIA^(2/3-1))^2)*(exp(VEIA)-1)*exp(2*log(EIA)+VEIA))
results4[(is.na(results4$RVdET)|is.infinite(results4$RVdET)),]$RVdET = 0

results4$excess = results4$deaths-results4$EB
results4$uexcess = results4$deaths-(results4$EB + results4$EdIA + results4$EdET)
### Exclude negative IA effects ###
if (IArest == 1) {results4$uexcess = results4$deaths-(results4$EB + pmax(0,results4$EdIA) + results4$EdET)}

results4 <- results4[with(results4, order(agegrp, year, week)),]

results4$EB_95L = with(results4, pmax(0,sign((sign(EB)*abs(EB)^(2/3))-1.96*sqrt(RVB))*abs((sign(EB)*abs(EB)^(2/3))-1.96*sqrt(RVB))^(3/2)))
results4$EB_95U = with(results4, sign((sign(EB)*abs(EB)^(2/3))+1.96*sqrt(RVB))*abs((sign(EB)*abs(EB)^(2/3))+1.96*sqrt(RVB))^(3/2))

results4$EdIA_95L = with(results4, pmax(0,sign((sign(EdIA)*abs(EdIA)^(2/3))-1.96*sqrt(RVdIA))*abs((sign(EdIA)*abs(EdIA)^(2/3))-1.96*sqrt(RVdIA))^(3/2)))
results4$EdIA_95U = with(results4, pmax(EdIA_95L,sign((sign(EdIA)*abs(EdIA)^(2/3))+1.96*sqrt(RVdIA))*abs((sign(EdIA)*abs(EdIA)^(2/3))+1.96*sqrt(RVdIA))^(3/2)))
results4$EdET_95L = with(results4, pmax(0,sign((sign(EdET)*abs(EdET)^(2/3))-1.96*sqrt(RVdET))*abs((sign(EdET)*abs(EdET)^(2/3))-1.96*sqrt(RVdET))^(3/2)))
results4$EdET_95U = with(results4, pmax(EdET_95L,sign((sign(EdET)*abs(EdET)^(2/3))+1.96*sqrt(RVdET))*abs((sign(EdET)*abs(EdET)^(2/3))+1.96*sqrt(RVdET))^(3/2)))

results4 <- results4[with(results4, order(agegrp, year, week)),]
results4$wk <- unlist(tapply(results4$agegrp,results4$agegrp,function(x) seq(1,length(x),1)))

results4$summer = with(results4, ifelse((21<=week)&(week<=39),year,NA))
results4$winter = with(results4, ifelse((results4$week<=20)|(results4$week>=40),year-as.numeric(week<=20),NA))


cummulate <- function(x, pos=F) {
  x[is.na(x)] = 0
  if (pos) { x <- pmax(0,x)}
  for (i in 1:length(x)) {
    if (i==1) { x[i] = x[i] }
    else { x[i] = x[i-1] + x[i] }
  }
  return(x)
}

cummulateVE <- function(x, pos=F) {
  x[is.na(x)] = 0
  if (pos) { y = ifelse(is.infinite((exp(x[,2])-1)*exp(2*log(x[,1])+x[,2])*(x[,1]>=0)), 0, (exp(x[,2])-1)*exp(2*log(x[,1])+x[,2])*(x[,1]>=0)) }
  else { y = ifelse(is.infinite((exp(x[,2])-1)*exp(2*log(x[,1])+x[,2])), 0, (exp(x[,2])-1)*exp(2*log(x[,1])+x[,2])) }
  for (i in 1:length(y)) {
    if (i==1) { y[i] = y[i] }
    else { y[i] = y[i-1] + y[i] }
  }
  return(y)
}

VEd <- function(x, pos=F) {
  x[is.na(x)] = 0
  if (pos) { y = ifelse( x[,2]==0, 0, (2/3)*(abs((x[,2])^(2/3-1))^2)*(x[,4] + x[,3])*(x[,1]>=0)) }
  else { y = ifelse( x[,2]==0, 0, (2/3)*(abs((x[,2])^(2/3-1))^2)*(x[,4] + x[,3])) }
  return(y)
}

for (s in c("summer","winter","year")) {
  for (v in c("excess","uexcess")) {
    results4[,paste0("c", v, "_", s)] = NA
    for (a in 0:4) {
      for (sy in min(unique(results4[,s]), na.rm=T):max(unique(results4[,s]), na.rm=T)) {
        results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("c", v, "_", s)] =
          cummulate(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),v],F)
      }
    }
  }
  for (v in c("EIA","EET","Eall")) {
    results4[,paste0("c", v, "_", s)] = NA
    results4[,paste0("Vc", v, "_", s)] = NA
    for (a in 0:4) {
      for (sy in min(unique(results4[,s]), na.rm=T):max(unique(results4[,s]), na.rm=T)) {
        results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("c", v, "_", s)] =
          cummulate(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),v],F)
        results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("Vc", v, "_", s)] =
          cummulateVE(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),c(v,paste0("V",v))],F)
      }
    }
  }
  for (v in c("IA","ET")) {
    results4[,paste0("cEd", v, "_", s)] = NA
    results4[,paste0("VcEd", v, "_", s)] = NA
    for (a in 0:4) {
      for (sy in min(unique(results4[,s]), na.rm=T):max(unique(results4[,s]), na.rm=T)) {
        results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("cEd", v, "_", s)] =
          cummulate(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("Ed",v)],F)
        results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("VcEd", v, "_", s)] =
          VEd(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),
                       c(paste0("Ed",v),paste0("cE", v, "_", s),paste0("VcE", v, "_", s),paste0("VcEall_", s))],F)
      }
    }
    results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s,"_95L")] =
      sign(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))-1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))*
        (abs(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))-1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))^(3/2))
    results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s,"_95U")] =
      sign(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))+1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))*
        (abs(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))+1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))^(3/2))
  }
  ### Exclude negative IA effects ###
  if (IArest == 1 ) {
    for (v in c("EIA")) {
      results4[,paste0("c", v, "_", s)] = NA
      results4[,paste0("Vc", v, "_", s)] = NA
      for (a in 0:4) {
        for (sy in min(unique(results4[,s]), na.rm=T):max(unique(results4[,s]), na.rm=T)) {
          results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("c", v, "_", s)] =
            cummulate(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),v],T)
          results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("Vc", v, "_", s)] =
            cummulateVE(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),c(v,paste0("V",v))],T)
        }
      }
    }
    for (v in c("IA")) {
      results4[,paste0("cEd", v, "_", s)] = NA
      results4[,paste0("VcEd", v, "_", s)] = NA
      for (a in 0:4) {
        for (sy in min(unique(results4[,s]), na.rm=T):max(unique(results4[,s]), na.rm=T)) {
          results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("cEd", v, "_", s)] =
            cummulate(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("Ed",v)],T)
          results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),paste0("VcEd", v, "_", s)] =
            VEd(results4[(results4[,s]==sy)&(!is.na(results4[,s]))&(results4$agegrp==a),
                         c(paste0("Ed",v),paste0("cE", v, "_", s),paste0("VcE", v, "_", s),paste0("VcEall_", s))],T)
        }
      }
      results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s,"_95L")] =
        sign(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))-1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))*
        (abs(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))-1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))^(3/2))
      results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s,"_95U")] =
        sign(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))+1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))*
        (abs(sign(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])*(abs(results4[(!is.na(results4[,s])),paste0("cEd",v,"_",s)])^(2/3))+1.96*sqrt(results4[(!is.na(results4[,s])),paste0("VcEd",v,"_",s)]))^(3/2))
    }
  }
}
results4$country = country
results4$IArestricted = IArest
results4 <- results4[, c("country","IArestricted","agegrp","year","week","deaths","N","IA","VEIA","ET","VEET","EB","EB_95L","EB_95U","RVB","EdIA","EdIA_95L","EdIA_95U","RVdIA","EdET","EdET_95L","EdET_95U","RVdET",
                         "cexcess_year","cuexcess_year","cEdIA_year","cEdIA_year_95L","cEdIA_year_95U","cEdET_year","cEdET_year_95L","cEdET_year_95U",
                         "summer","cexcess_summer","cuexcess_summer","cEdIA_summer","cEdIA_summer_95L","cEdIA_summer_95U","cEdET_summer","cEdET_summer_95L","cEdET_summer_95U",
                         "winter","cexcess_winter","cuexcess_winter","cEdIA_winter","cEdIA_winter_95L","cEdIA_winter_95U","cEdET_winter","cEdET_winter_95L","cEdET_winter_95U")]

if (IArest == 1) { write.table(results4, file=paste0(outdir,"/",country,"_output_v4_IArestricted.csv"), row.names = F, quote = F, sep =";", dec=".", na="") }
if (IArest != 1) { write.table(results4, file=paste0(outdir,"/",country,"_output_v4.csv"), row.names = F, quote = F, sep =";", dec=".", na="") }

rm(a, s, sy, v, cummulate, cummulateVE, vecshift, VEd, results4)