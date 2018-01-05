### CSV files ###

if (IArest == 1) { results4 <- read.table(file=paste0(outdir,"/",country,"_output_V4_IArestricted.csv"), header = T, sep =";", dec=".") }
if (IArest != 1) { results4 <- read.table(file=paste0(outdir,"/",country,"_output_V4.csv"), header = T, sep =";", dec=".") }

csvout <- function(x, s) {
  for (i in min(unique(x[,s])):max(unique(x[,s]))) {
    if (i == min(unique(unique(x[,s])))) {
      y = cbind(unique(x[,"agegrp"]),min(x[(x[,s]==i),"yw"]),max(x[(x[,s]==i),"yw"]),
                round(tail(x[(x[,s]==i),c(paste0("cEdIA_",s),paste0("cEdIA_",s,"_95L"),paste0("cEdIA_",s,"_95U"),
                                          paste0("cEdET_",s),paste0("cEdET_",s,"_95L"),paste0("cEdET_",s,"_95U"),
                                          paste0("cexcess_",s),paste0("cuexcess_",s),"N")],n=1),0))
    }
    else {
      y = rbind(y ,cbind(unique(x[,"agegrp"]),min(x[(x[,s]==i),"yw"]),max(x[(x[,s]==i),"yw"]),
                         round(tail(x[(x[,s]==i),c(paste0("cEdIA_",s),paste0("cEdIA_",s,"_95L"),paste0("cEdIA_",s,"_95U"),
                                                   paste0("cEdET_",s),paste0("cEdET_",s,"_95L"),paste0("cEdET_",s,"_95U"),
                                                   paste0("cexcess_",s),paste0("cuexcess_",s),"N")],n=1),0)))
    }
  }
  colnames(y) <- c("AgeGroup","Start","End","cIA","cIA_95L","cIA_95U","cET","cET_95L","cET_95U","cexcess","cuexcess","N")
  return(y)
}

results4$yw = paste0(sprintf("%04.0f",results4[,"year"]),"w",sprintf("%02.0f",results4[,"week"]))
for (s in c("summer","winter","year")) {
  out <- csvout(results4[(!is.na(results4[,s]))&(results4$agegrp==0),],s)
  for (a in 1:4) {
    out <- rbind(out,csvout(results4[(!is.na(results4[,s]))&(results4$agegrp==a),],s))
  }
  out$AgeGroup <- factor(out$AgeGroup, levels = c(0,1,2,3,4), labels = c("0-4 years","5-14 years","15-64 years","Aged 65","Total"))
  if (IArest == 1) { write.table(out[,c("AgeGroup","Start","End","cIA","cIA_95L","cIA_95U","cET","cET_95L","cET_95U","cexcess","cuexcess")], file=paste0(outdir,"/",s,"_deaths_V4_IArestricted.csv"), row.names = F, quote = F, sep =";", dec=".", na="") }
  if (IArest != 1) { write.table(out[,c("AgeGroup","Start","End","cIA","cIA_95L","cIA_95U","cET","cET_95L","cET_95U","cexcess","cuexcess")], file=paste0(outdir,"/",s,"_deaths_V4.csv"), row.names = F, quote = F, sep =";", dec=".", na="") }
  if (population == 1) {
    out[,c("cIA","cIA_95L","cIA_95U","cET","cET_95L","cET_95U","cexcess","cuexcess")] <- round(100000*out[,c("cIA","cIA_95L","cIA_95U","cET","cET_95L","cET_95U","cexcess","cuexcess")]/out[,"N"],2)
    colnames(out) <- c("AgeGroup","Start","End","cmrIA","cmrIA_95L","cmrIA_95U","cmrET","cmrET_95L","cmrET_95U","cexcess","cuexcess","N")
    if (IArest == 1) { write.table(out[,c("AgeGroup","Start","End","cmrIA","cmrIA_95L","cmrIA_95U","cmrET","cmrET_95L","cmrET_95U","cexcess","cuexcess")], file=paste0(outdir,"/",s,"_mr_V4_IArestricted.csv"), row.names = F, quote = F, sep =";", dec=".") }
    if (IArest != 1) { write.table(out[,c("AgeGroup","Start","End","cmrIA","cmrIA_95L","cmrIA_95U","cmrET","cmrET_95L","cmrET_95U","cexcess","cuexcess")], file=paste0(outdir,"/",s,"_mr_V4.csv"), row.names = F, quote = F, sep =";", dec=".") }
  }
}
rm(out, s, a, csvout, results4)
