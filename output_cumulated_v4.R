### Graphs - cumulated ###
#install.packages("ggplot2")
require(ggplot2, quietly=T)

if (IArest == 1) {
  results4 <- read.table(file=paste0(outdir,"/",country,"_output_V4_IArestricted.csv"), header = T, sep =";", dec=".")
  note = "Note: Negative effects of IA are not shown"
}
if (IArest != 1) {
  results4 <- read.table(file=paste0(outdir,"/",country,"_output_V4.csv"), header = T, sep =";", dec=".")
  note = ""
}
results4$agegrp.labels <- factor(results4$agegrp, labels = c("0-4 years","5-14 years","15-64 years","Aged 65","Total"))

for (s in c("summer","winter","year")) {
  for (t in c("IA")) {
    out <- results4[!is.na(results4[,s]), c("agegrp","agegrp.labels","year","week",paste0("cEd",t,"_",s),"N")]
    colnames(out) <- c("agegrp","agegrp.labels","year","week","cd","N")
    out$cm = 100000*out$cd/out$N
    out[out$week==53,"week"] = 52.5
    if (s == "winter") {
      out$wk = with(out, 1 + (week-40)*(week>=40) + (week+12)*(week<=20))
      out$season= factor(paste0(as.character(with(out, year*(week>=40) + (year-1)*(week<=20))),"/",substr(as.character(with(out, year*(week>=40) + (year-1)*(week<=20))+1),3,4)))
    }
    if (s == "summer") {
      out$wk = out$week-20
      out$season = factor(out$year)
    }
    if (s == "year") {
      out$wk = out$week
      out$season = factor(out$year)
    }
    out <- out[with(out, order(agegrp, wk)),]
    ggplot(data = out, aes(x = wk, y = cd, colour = season)) + geom_line()  +
      scale_x_continuous(breaks=seq(1, max(out$wk), 4), labels = unique(floor(out$week))[seq(1, max(out$wk), 4)]) +
      labs(x = "week number", y = "number of deaths", title = paste("Cumulated",s,"number of deaths attributable to",t,"by age group")) +
      labs(caption = note) +
      theme(axis.text.x = element_text(size = 10, angle = 0),
            axis.line = element_line(colour = "black", size = 0.6),
            panel.background = element_rect(fill = NA),
            panel.grid.major = element_line(colour = "grey90"),
            legend.position = "bottom",
            legend.key = element_rect(fill = NA),
            legend.title=element_blank(),
            strip.background = element_rect(fill = NA),
            plot.title = element_text(hjust = 0.5, vjust=2),
            plot.caption = element_text(hjust = 0)) +
    facet_wrap( ~ agegrp.labels, nrow = 5, scales = "free_y")
    if (IArest == 1) { ggsave(paste0(outdir,"/cumulated_",t,"_deaths_agegroups_",s,"_V4_IArestricted.png"), width = 20, height = 30, units = "cm") }
    if (IArest != 1) { ggsave(paste0(outdir,"/cumulated_",t,"_deaths_agegroups_",s,"_V4.png"), width = 20, height = 30, units = "cm") }
    if (population == 1) {
      ggplot(data = out, aes(x = wk, y = cm, colour = season)) + geom_line()  +
        scale_x_continuous(breaks=seq(1, max(out$wk), 4), labels = unique(floor(out$week))[seq(1, max(out$wk), 4)]) +
        labs(x = "week number", y = "deaths / 100,000", title = paste("Cumulated",s,"mortality attributable to",t,"by age group")) +
        labs(caption = note) +
        theme(axis.text.x = element_text(size = 10, angle = 0),
              axis.line = element_line(colour = "black", size = 0.6),
              panel.background = element_rect(fill = NA),
              panel.grid.major = element_line(colour = "grey90"),
              legend.position = "bottom",
              legend.key = element_rect(fill = NA),
              legend.title=element_blank(),
              strip.background = element_rect(fill = NA),
              plot.title = element_text(hjust = 0.5, vjust=2),
              plot.caption = element_text(hjust = 0)) +
        facet_wrap( ~ agegrp.labels, nrow = 5, scales = "free_y")
      if (IArest == 1) { ggsave(paste0(outdir,"/cumulated_",t,"_mr_agegroups_",s,"_V4_IArestricted.png"), width = 20, height = 30, units = "cm") }
      if (IArest != 1) { ggsave(paste0(outdir,"/cumulated_",t,"_mr_agegroups_",s,"_V4.png"), width = 20, height = 30, units = "cm") }
    }
    for (a in 0:4) {
      ggplot(data = out[(out$agegrp==a),], aes(x = wk, y = cd, colour = season)) + geom_line()  +
        scale_x_continuous(breaks=seq(1, max(out$wk), 4), labels = unique(floor(out$week))[seq(1, max(out$wk), 4)]) +
        labs(x = "week number", y = "number of deaths", title = paste0("Cumulated ",s," number of deaths attributable to ",t,", age group: ",unique(results4$agegrp.labels)[a+1])) +
        labs(caption = note) +
        theme(axis.text.x = element_text(size = 10, angle = 0),
              axis.line = element_line(colour = "black", size = 0.6),
              panel.background = element_rect(fill = NA),
              panel.grid.major = element_line(colour = "grey90"),
              legend.position = "bottom",
              legend.key = element_rect(fill = NA),
              legend.title=element_blank(),
              strip.background = element_rect(fill = NA),
              plot.title = element_text(hjust = 0.5, vjust=2),
              plot.caption = element_text(hjust = 0))
      if (IArest == 1) { ggsave(paste0(outdir,"/cumulated_",t,"_deaths_agegroup_",a,"_",s,"_V4_IArestricted.png"), width = 30, height = 20, units = "cm") }
      if (IArest != 1) { ggsave(paste0(outdir,"/cumulated_",t,"_deaths_agegroup_",a,"_",s,"_V4.png"), width = 30, height = 20, units = "cm") }
      if (population == 1) {
        ggplot(data = out[(out$agegrp==a),], aes(x = wk, y = cm, colour = season)) + geom_line()  +
          scale_x_continuous(breaks=seq(1, max(out$wk), 4), labels = unique(floor(out$week))[seq(1, max(out$wk), 4)]) +
          labs(x = "week number", y = "deaths / 100,000", title = paste0("Cumulated ",s," mortality attributable to ",t,", age group: ",unique(results4$agegrp.labels)[a+1])) +
          labs(caption = note) +
          theme(axis.text.x = element_text(size = 10, angle = 0),
                axis.line = element_line(colour = "black", size = 0.6),
                panel.background = element_rect(fill = NA),
                panel.grid.major = element_line(colour = "grey90"),
                legend.position = "bottom",
                legend.key = element_rect(fill = NA),
                legend.title=element_blank(),
                strip.background = element_rect(fill = NA),
                plot.title = element_text(hjust = 0.5, vjust=2),
                plot.caption = element_text(hjust = 0))
        if (IArest == 1) { ggsave(paste0(outdir,"/cumulated_",t,"_mr_agegroup_",a,"_",s,"_V4_IArestricted.png"), width = 30, height = 20, units = "cm") }
        if (IArest != 1) { ggsave(paste0(outdir,"/cumulated_",t,"_mr_agegroup_",a,"_",s,"_V4.png"), width = 30, height = 20, units = "cm") }
      }
    }
  }
}
rm(results4, a, s, t, out, note)
