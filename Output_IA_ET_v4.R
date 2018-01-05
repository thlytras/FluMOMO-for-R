#install.packages("ggplot2")
require(ggplot2, quietly=T)

### Ambient temperatures ###
ET <- read.table(file=paste0(indir,"/temperature.csv"), header = T, sep =";", dec=".")
ET <- ET[with(ET, order(year, week)),]
ET <- within(ET, paste(
  Uexecc <- pmax(ptmax,temp),
  Lexecc <- pmin(ptmin,temp),
  wk <- 1:length(temp)
))
ET$yw = paste0(sprintf("%04.0f",ET[,"year"]),"w",sprintf("%02.0f",ET[,"week"]))

ggplot(data = ET, aes(wk,temp)) +
  geom_ribbon(aes(x=wk, ymax=ptmax, ymin=Uexecc), fill="red", alpha=.5, na.rm = T) +
  geom_ribbon(aes(x=wk, ymax=ptmin, ymin=Lexecc), fill="blue", alpha=.5, na.rm = T) +
  geom_point(aes(y = temp), colour = "black", na.rm = T) +
  geom_line(aes(y = ptemp), colour = "black", linetype = 1, na.rm = T) +
  geom_line(aes(y = ptmin), colour = "blue", linetype = 1, na.rm = T) +
  geom_line(aes(y = ptmax), colour = "red", linetype = 1, na.rm = T) +
  xlab("year-week") + ylab("Degree Celsius") + ggtitle("Ambient temperatures") +
  labs(caption = "dot: observed temperature   black line: expected temperature
       red: line - max expected temperature   area - extreme heat
       blue: line - min expected temperature   area - extreme cold") +
  scale_x_continuous(breaks=seq(1, max(ET$wk), 26), labels = unique(ET$yw)[seq(1, max(ET$wk), 26)]) +
  theme(axis.text.x = element_text(size = 10, angle = 90),
        axis.line = element_line(colour = "black", size = 0.6),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey90"),
        plot.title = element_text(hjust = 0.5, vjust=2),
        plot.caption = element_text(hjust = 0.5, vjust=2))
ggsave(paste0(outdir,"/temperature_v4R.png"), width = 40, height = 20, units = "cm")
rm(ET)

### Influenza Activity ###

if (IArest == 1) { results4 <- read.table(file=paste0(outdir,"/",country,"_output_v4_IArestricted.csv"), header = T, sep =";", dec=".") }
if (IArest != 1) { results4 <- read.table(file=paste0(outdir,"/",country,"_output_v4.csv"), header = T, sep =";", dec=".") }

results4 <- results4[with(results4, order(agegrp, year, week)),]
results4$wk <- unlist(tapply(results4$agegrp,results4$agegrp,function(x) seq(1,length(x),1)))
results4$agegrp.labels <- ordered(results4$agegrp, labels = c("0-4 years","5-14 years","15-64 years","Aged 65","Total"))
results4$yw = paste0(sprintf("%04.0f",results4[,"year"]),"w",sprintf("%02.0f",results4[,"week"]))
#results4$wk.labels = ordered(results4$wk, labels = unique(results4$yw))

ggplot(data = results4, aes(wk,IA)) +
  geom_line(aes(y = IA), colour = "red", na.rm = T) +
  labs(x = "year-week", y = "Influenza Activity per week", title = "Influenza Activity by age group") +
  scale_x_continuous(breaks=seq(1, max(results4$wk), 26), labels = unique(results4$yw)[seq(1, max(results4$wk), 26)]) +
  theme(axis.text.x = element_text(size = 10, angle = 90),
        axis.line = element_line(colour = "black", size = 0.6),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey90")) +
  facet_wrap( ~ agegrp.labels, nrow = 5, scales = "free_y")
ggsave(paste0(outdir,"/IA_agegroups_v4R.png"), width = 20, height = 30, units = "cm")
for (a in 0:4) {
  ggplot(data = results4[(results4$agegrp==a),], aes(wk,IA)) +
    geom_line(aes(y = IA), colour = "red", na.rm = T) +
    labs(x = "year-week", y = "Influenza Activity per week", title = paste("Influenza Activity, age group:",unique(results4$agegrp.labels)[a+1])) +
    scale_x_continuous(breaks=seq(1, max(results4$wk), 26), labels = unique(results4$yw)[seq(1, max(results4$wk), 26)]) +
    theme(axis.text.x = element_text(size = 10, angle = 90),
          axis.line = element_line(colour = "black", size = 0.6),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"))
    ggsave(paste0(outdir,"/IA_agegroup_",a,"_v4R.png"), width = 40, height = 20, units = "cm")
}
