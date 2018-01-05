# ## Graphs - calendar time ##
require(ggplot2, quietly=T)

if (IArest) {
  results4 <- read.table(file=paste0(outdir,"/",country,"_output_v4_IArestricted.csv"), header = TRUE, sep =";", dec=".")
  results4$EIA <- results4$EB + pmax(0,results4$EdIA)
  results4$EIAET <- results4$EB + pmax(0,results4$EdIA) + results4$EdET
  note <- "Note: Negative effects of IA are not shown"
} else {
  results4 <- read.table(file=paste0(outdir,"/",country,"_output_v4.csv"), header = TRUE, sep =";", dec=".")
  results4$EIA <- results4$EB + results4$EdIA
  results4$EIAET <- results4$EB + results4$EdIA + results4$EdET
  note <- ""
}
results4 <- results4[with(results4, order(agegrp, year, week)),]
results4$wk <- unlist(lapply(table(results4$agegrp), seq))
results4$agegrp.labels <- ordered(results4$agegrp, 
    labels = c("0-4 years","5-14 years","15-64 years","Aged 65","Total"))
results4$yw <- sprintf("%04dw%02d", results4$year, results4$week)

ggplot(data = results4, aes(wk,EB)) +
  geom_ribbon(aes(x=wk, ymax=deaths, ymin=EB), fill="gray", alpha=.5, na.rm = T) +
  geom_line(aes(y = EIAET), colour = "darkgreen", na.rm = T) +
  geom_line(aes(y = EIA), colour = "red", na.rm = T) +
  geom_line(aes(y = EB), colour = "black", linetype = 1, na.rm = T) +
  geom_line(aes(y = EB_95L), colour = "black", linetype = 2, na.rm = T) +
  geom_line(aes(y = EB_95U), colour = "black", linetype = 2, na.rm = T) +
  labs(x = "year-week", y = "deaths per week", title = "Number of deaths by age group") +
  labs(caption = paste("Black line: baseline, dotted black line: 95% confidence interval",
       "Red: Effect of Influenza Activity (IA)",
       "Green: Effect of excess temperature (on top of IA)", note, sep = "\n")) +
  scale_x_continuous(breaks=seq(1, max(results4$wk), 26), labels = unique(results4$yw)[seq(1, max(results4$wk), 26)]) +
  theme(axis.text.x = element_text(size = 10, angle = 90),
        axis.line = element_line(colour = "black", size = 0.6),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey90"),
        plot.title = element_text(hjust = 0.5, vjust=2),
        plot.caption = element_text(hjust = 0.5, vjust=2)) +
  facet_wrap( ~ agegrp.labels, nrow = 5, scales = "free_y")
ggsave(paste0(outdir,"/deaths_agegroups_v4", ifelse(IArest, "_IArestricted", ""), ".png"), 
    width = 20, height = 30, units = "cm")


for (a in 0:4) {
  ggplot(data = results4[(results4$agegrp==a),], aes(wk,EB)) +
    geom_ribbon(aes(x=wk, ymax=deaths, ymin=EB), fill="gray", alpha=.5, na.rm = T) +
    geom_line(aes(y = EIAET), colour = 'darkgreen', na.rm = T) +
    geom_line(aes(y = EIA), colour = 'red', na.rm = T) +
    geom_line(aes(y = EB), colour = "black", linetype = 1, na.rm = T) +
    geom_line(aes(y = EB_95L), colour = "black", linetype = 2, na.rm = T) +
    geom_line(aes(y = EB_95U), colour = "black", linetype = 2, na.rm = T) +
    labs(x = "year-week", y = "deaths per week", title = paste("Number of deaths, age group:",unique(results4$agegrp.labels)[a+1])) +
    labs(caption = paste("Black line: baseline, dotted black line: 95% confidence interval",
                         "Red: Effect of Influenza Activity (IA)",
                         "Green: Effect of excess temperature (on top of IA)", note, sep = "\n")) +
    scale_x_continuous(breaks=seq(1, max(results4$wk), 26), labels = unique(results4$yw)[seq(1, max(results4$wk), 26)]) +
    theme(axis.text.x = element_text(size = 10, angle = 90),
          axis.line = element_line(colour = "black", size = 0.6),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          plot.title = element_text(hjust = 0.5, vjust=2),
          plot.caption = element_text(hjust = 0.5, vjust=2))
  
  ggsave(paste0(outdir, "/deaths_agegroup_", a, "_v4", ifelse(IArest, "_IArestricted", ""), ".png"), 
      width = 40, height = 20, units = "cm")
}

if (population) {
  results4[,c("deaths","EB","EB_95L","EB_95U","EIAET","EIA")] <- 
      100000 * results4[,c("deaths","EB","EB_95L","EB_95U","EIAET","EIA")] / results4[,"N"]
  ggplot(data = results4, aes(wk,EB)) +
    geom_ribbon(aes(x=wk, ymax=deaths, ymin=EB), fill="gray", alpha=.5, na.rm = T) +
    geom_line(aes(y = EIAET), colour = "darkgreen", na.rm = T) +
    geom_line(aes(y = EIA), colour = "red", na.rm = T) +
    geom_line(aes(y = EB), colour = "black", linetype = 1, na.rm = T) +
    geom_line(aes(y = EB_95L), colour = "black", linetype = 2, na.rm = T) +
    geom_line(aes(y = EB_95U), colour = "black", linetype = 2, na.rm = T) +
    labs(x = "year-week", y = "deaths / 100.000 per week", title = "Mortality by age group") +
    labs(caption = paste("Black line: baseline, dotted black line: 95% confidence interval",
                         "Red: Effect of Influenza Activity (IA)",
                         "Green: Effect of excess temperature (on top of IA)", note, sep = "\n")) +
    scale_x_continuous(breaks=seq(1, max(results4$wk), 26), labels = unique(results4$yw)[seq(1, max(results4$wk), 26)]) +
    theme(axis.text.x = element_text(size = 10, angle = 90),
          axis.line = element_line(colour = "black", size = 0.6),
          panel.background = element_rect(fill = NA),
          panel.grid.major = element_line(colour = "grey90"),
          plot.title = element_text(hjust = 0.5, vjust=2),
          plot.caption = element_text(hjust = 0.5, vjust=2)) +
    facet_wrap( ~ agegrp.labels, nrow = 5, scales = "free_y")
  ggsave(paste0(outdir,"/mr_agegroups_v4", ifelse(IArest, "_IArestricted", ""), ".png"), 
    width = 20, height = 30, units = "cm")
  
  for (a in 0:4) {
    ggplot(data = results4[(results4$agegrp==a),], aes(wk,EB)) +
      geom_ribbon(aes(x=wk, ymax=deaths, ymin=EB), fill="gray", alpha=.5, na.rm = T) +
      geom_line(aes(y = EIAET), colour = 'darkgreen', na.rm = T) +
      geom_line(aes(y = EIA), colour = 'red', na.rm = T) +
      geom_line(aes(y = EB), colour = "black", linetype = 1, na.rm = T) +
      geom_line(aes(y = EB_95L), colour = "black", linetype = 2, na.rm = T) +
      geom_line(aes(y = EB_95U), colour = "black", linetype = 2, na.rm = T) +
      labs(x = "year-week", y = "deaths / 100.000 per week", title = paste("Mortality, age group:",unique(results4$agegrp.labels)[a+1])) +
      labs(caption = paste("Black line: baseline, dotted black line: 95% confidence interval",
                           "Red: Effect of Influenza Activity (IA)",
                           "Green: Effect of excess temperature (on top of IA)", note, sep = "\n")) +
      scale_x_continuous(breaks=seq(1, max(results4$wk), 26), labels = unique(results4$yw)[seq(1, max(results4$wk), 26)]) +
      theme(axis.text.x = element_text(size = 10, angle = 90),
            axis.line = element_line(colour = "black", size = 0.6),
            panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            plot.title = element_text(hjust = 0.5, vjust=2),
            plot.caption = element_text(hjust = 0.5, vjust=2))
    ggsave(paste0(outdir,"/mr_agegroup", a, "_v4", ifelse(IArest, "_IArestricted", ""), ".png"), 
        width = 20, height = 30, units = "cm")
  }
}
rm(results4, a, note)
