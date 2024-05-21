library(ggplot2)

# high_certainty.csv obtained by adding:
# high_certainty <- joined_df[which(joined_df[["certainty"]] == "meets criteria"),]
# write.csv(high_certainty, "high_certainty.csv", row.names=FALSE)
high_certainty <- read.csv("data/high_certainty.csv", header=TRUE)

hc.mean = mean(high_certainty$reanalysis_estimate)
hc.sd  = sd(high_certainty$reanalysis_estimate)

# For axis ticks
hc.mean.approx <- round(hc.mean, 1)

# Figure 1
data <- high_certainty
ggplot(data, aes(reanalysis_estimate)) +
  geom_histogram(aes(y = after_stat(density)), fill='lightgrey', binwidth=0.05) +
  stat_function(fun = dnorm, args = list(mean=mean(data$reanalysis_estimate), sd=sd(data$reanalysis_estimate)), col='black', linewidth=1.5,
                xlim=c(-0.3, 0.5)) +
  scale_x_continuous(breaks = seq(hc.mean.approx - 0.4, hc.mean.approx+0.4, by = 0.1), limits = c(-0.22, 0.42), expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0)), limits = c(0, 6)) +
  labs(x = "Effect size (r)", y = "Density") +
  theme_classic() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),   panel.border = element_rect(colour = "black", fill=NA, size=1))

length(data$reanalysis_estimate)
