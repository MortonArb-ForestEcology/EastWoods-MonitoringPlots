################################################
# Now doing some plotting of the data ----
# Things we want to look at:
# - Average biomass by tissue by plot by year
# - Average biomass of leaves, fruit by species through time each year
# - Total number of fruits, ripe fruits by species per year
#
# NOTE: Need to account for absent data at all stages
# NOTE: because we care about the plot, we'll want to aggregate across traps first to get the mean per plot
# NOTE: For species, we'll need to do the above, but also account for absent data <-- this will require some thought
################################################
library(ggplot2)

# Set up file paths etc. --> this should also indicate where you can find these files!
path.google <- "~/Google Drive/My Drive"
path.litter <- file.path(path.google, "East Woods/Rollinson_Monitoring/Data/Leaf_litter_data")
path.figs <- file.path(path.litter, "figures") # where we shoudl save some figures
path.save <- file.path(path.litter, "LeafLitterData_Clean_forArchiving") # Where we shoudl save the data


dir(path.save)

datLeafLitter$year <- lubridate::year(datLeafLitter$date_collection)
datLeafLitter$yday <- lubridate::yday(datLeafLitter$date_collection)
datLeafLitter$week <- lubridate::week(datLeafLitter$date_collection)
summary(datLeafLitter)

aggTrapTotal <- aggregate(mass_g ~ year + plot + trap_ID, data=datLeafLitter, FUN=sum)
aggTrapTotal$plot <- factor(aggTissTrap$plot, levels=plotOrder)
summary(aggTrapTotal)


png(file.path(path.figs, "TotalMass_byPlot_byYear_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggTrapTotal) +
  # facet_wrap(~plot) +
  geom_boxplot(aes(x=as.factor(year), y=mass_g, fill=plot)) +
  scale_fill_manual(values=ewPlotColors) +
  theme_bw()
dev.off()


aggTrapDate <- aggregate(mass_g ~ date_collection + year + yday + plot + trap_ID, data=datLeafLitter, FUN=sum)
aggTrapDate$plot <- factor(aggTrapDate$plot, levels=plotOrder)
summary(aggTrapDate)

png(file.path(path.figs, "TotalMass_byTrap_byDate_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggTrapDate) +
  facet_grid(year~plot) +
  geom_point(aes(x=yday, y=mass_g, color=plot, group=trap_ID), stat="identity") +
  stat_summary(geom="line", aes(x=yday, y=mass_g), fun="mean") +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()
dev.off()

aggTrapWeek <- aggregate(mass_g ~ date_collection + year + week + plot + trap_ID, data=datLeafLitter, FUN=sum)
aggTrapWeek$plot <- factor(aggTrapWeek$plot, levels=plotOrder)
summary(aggTrapWeek)

png(file.path(path.figs, "TotalMass_byTrap_byWeek_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggTrapWeek) +
  facet_grid(year~plot) +
  geom_point(aes(x=week, y=mass_g, color=plot, group=trap_ID), stat="identity") +
  stat_summary(geom="line", aes(x=week, y=mass_g), fun="mean") +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()
dev.off()



# - Average biomass by tissue by plot by year
aggTissTrap <- aggregate(mass_g ~ tissue + year + plot + trap_ID, data=datLeafLitter, FUN=sum)
aggTissTrap$plot <- factor(aggTissTrap$plot, levels=plotOrder)
summary(aggTissTrap)

png(file.path(path.figs, "TissueMass_byPlot_byYear_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggTissTrap[aggTissTrap$tissue!="EMPTY BAG",]) +
  facet_wrap(~tissue, scales="free_y") +
  geom_boxplot(aes(x=as.factor(year), y=mass_g, fill=plot)) +
  labs(x="Year", y="mass (g)") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()
dev.off()


aggTissTrapWk <-aggregate(mass_g ~ tissue + year + week + plot + trap_ID, data=datLeafLitter, FUN=sum)
aggTissTrap$plot <- factor(aggTissTrap$plot, levels=plotOrder)
summary(aggTissTrapWk)

png(file.path(path.figs, "LeafMass_byTrap_byWeek_latest.png"), height=6, width=8, units="in", res=220)
ggplot(data=aggTissTrapWk[aggTissTrapWk$tissue=="leaf",]) +
  facet_grid(year~plot) +
  # facet_wrap(~tissue, scales="free_y") +
  geom_point(aes(x=week, y=mass_g, color=plot)) +
  stat_summary(geom="line", aes(x=week, y=mass_g), fun="mean") +
  labs(x="week", y="mass (g)") +
  scale_fill_manual(values=ewPlotColors) +
  scale_color_manual(values=ewPlotColors) +
  theme_bw()
dev.off()

################################################
