# Task 1.a
mice_before = rnorm(200, 20, 2)
mice_after = rnorm(200, 21, 2.5)
IQR(mice_before)
IQR(mice_after)
mean(mice_before)
mean(mice_after)

# Task 1.b
rats_before = rweibull(200, shape = 10, scale = 20)
plot(rats_before)
rats_after = rweibull(200, shape = 9, scale = 21)
plot(rats_after)
IQR(rats_before)
IQR(rats_after)
mean(rats_before)
mean(rats_after)

# Task 1.c
library(ggplot2) # import library
library(reshape)

mice <- data.frame(mice_before, mice_after) # create df
rats <- data.frame(rats_before, rats_after)

mice_plot1 <- qplot(mice_before, data = mice, geom = "density") # density plot
mice_plot2 <- qplot(mice_after, data = mice, geom = "density")
rat_plot1 <- qplot(rats_before, data = rats, geom = "density")
rat_plot2 <- qplot(rats_after, data = rats, geom = "density")
gridExtra::grid.arrange(mice_plot1, mice_plot2, rat_plot1, rat_plot2, 
                        nrow = 2, ncol = 2) # arrange graphs in grid
melted_mice <- melt(mice)
melted_rats <- melt(rats)
mice_d_melt <- ggplot(melted_mice, aes(x=value,fill=variable)) + 
  geom_density(alpha=0.25) + xlab("weight (g)")
rats_d_melt <- ggplot(melted_rats, aes(x=value,fill=variable)) + 
  geom_density(alpha=0.25) + xlab("weight (g)")
mice_b_melt <- ggplot(melted_mice,aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + ylab("weight (g)")
rats_b_melt <- ggplot(melted_rats,aes(x=variable, y=value, fill=variable)) + 
  geom_boxplot() + ylab("weight (g)")
gridExtra::grid.arrange(mice_d_melt, rats_d_melt, mice_b_melt, rats_b_melt, 
                        nrow = 2, ncol = 2) # arrange graphs in grid

# Task 1.d# Task 1.mice_afterd
mice_bplot1 <- qplot(factor(0), mice_before, data = mice, geom = "boxplot",
                     xlab = "mice before", ylab = "weight (g)")
mice_bplot2 <- qplot(factor(0), mice_after, data = mice, geom = "boxplot",
                     xlab = "mice after", ylab = "weight (g)")
rat_bplot1 <- qplot(factor(0), rats_before, data = rats, geom = "boxplot",
                    xlab = "rats before", ylab = "weight (g)")
rat_bplot2 <- qplot(factor(0), rats_after, data = rats, geom = "boxplot",
                    xlab = "rats after", ylab = "weight (g)")
gridExtra::grid.arrange(mice_bplot1, mice_bplot2, rat_bplot1, rat_bplot2, 
                        nrow = 2, ncol = 2) # arrange graphs in grid
df_test <- melt(mice_bplot1, mice_bplot2, rat_bplot1, rat_bplot2)
qplot(df_test)
#summary(mice_bplot1)

# Task 2.a & b
library(ggpubr) # import library
qq_mice_before <- ggqqplot(mice$mice_before) # QQ plot
qq_mice_after <- ggqqplot(mice$mice_after)
qq_rats_before <- ggqqplot(rats$rats_before) # QQ plot
qq_rats_after <- ggqqplot(rats$rats_after)
gridExtra::grid.arrange(qq_mice_before, qq_mice_after, 
                        nrow = 1, ncol = 2) # arrange graphs in grid
gridExtra::grid.arrange(qq_rats_before, qq_rats_after, 
                        nrow = 1, ncol = 2) # arrange graphs in grid

qq_mice <- ggqqplot(melted_mice$value)+ ylab("mice weight (g)") # QQ plot
qq_rats <- ggqqplot(melted_rats$value)+ ylab("rats weight (g)")
gridExtra::grid.arrange(qq_mice, qq_rats, 
                        nrow = 1, ncol = 2) # arrange graphs in grid

shapiro.test(mice$mice_before) # Shapiro-Wilk test
shapiro.test(mice$mice_after)
shapiro.test(rats$rats_before) # Shapiro-Wilk test
shapiro.test(rats$rats_after)

shapiro.test(melted_mice$value) # Shapiro-Wilk test
shapiro.test(melted_rats$value)

# Task 2.c
# Mice before:
#   QQ - majority of data follows normal distribution
#   Shapiro-Wilks - p-value < 0.05 so reject null hypothesis, significant difference exists
#   
# Mice after:
#   QQ - majority of data follows normal distribution
#   Shapiro-Wilks - p-value > 0.05 so accept null hypothesis, no significant difference exists

# Rats before:
#   QQ - less data follows normal distribution
#   Shapiro-Wilks - p-value < 0.05 so reject null hypothesis, significant difference exists

# Rats after:
#   QQ - even less data follows normal distribution
#   Shapiro-Wilks - p-value < 0.05 so reject null hypothesis, significant difference exists

# Task 3.a
t.test(mice$mice_before, mice$mice_after, paired = TRUE)

# Task 3.b
library("dplyr") # import library

wilcox.test(rats$rats_before, rats$rats_after, alternative = "two.sided",
            paired = TRUE, conf.int = TRUE, conf.level = 0.95) # Wilcoxon test

# Task 4.a
library("fitdistrplus") # import library
before_w <- fitdist(rats$rats_before, "weibull") # Weibull distribution
before_l <- fitdist(rats$rats_before, "lnorm") # Lognormal distribution
before_g <- fitdist(rats$rats_before, "gamma") # Gamma distribution

par(mfrow=c(2,2)) # visualise
plot.legend <- c("Weibull", "Lognormal", "Gamma")
denscomp(list(before_w, before_l, before_g), legendtext = plot.legend)
cdfcomp (list(before_w, before_l, before_g), legendtext = plot.legend)
qqcomp  (list(before_w, before_l, before_g), legendtext = plot.legend)
ppcomp  (list(before_w, before_l, before_g), legendtext = plot.legend)

after_w <- fitdist(melted_rats, "weibull")
after_l <- fitdist(rats$rats_after, "lnorm")
after_g <- fitdist(rats$rats_after, "gamma")

par(mfrow=c(2,2)) # visualise
plot.legend <- c("Weibull", "Lognormal", "Gamma")
denscomp(list(after_w, after_l, after_g), legendtext = plot.legend)
cdfcomp (list(after_w, after_l, after_g), legendtext = plot.legend)
qqcomp  (list(after_w, after_l, after_g), legendtext = plot.legend)
ppcomp  (list(after_w, after_l, after_g), legendtext = plot.legend)

melted_w <- fitdist(melted_rats$value, "weibull")
melted_l <- fitdist(melted_rats$value, "lnorm")
melted_g <- fitdist(melted_rats$value, "gamma")

par(mfrow=c(2,2)) # visualise
plot.legend <- c("Weibull", "Lognormal", "Gamma")
denscomp(list(melted_w, melted_l, melted_g), legendtext = plot.legend, xlab = "Rats weight(g)")
cdfcomp (list(melted_w, melted_l, melted_g), legendtext = plot.legend, xlab = "Rats weight(g)")
qqcomp  (list(melted_w, melted_l, melted_g), legendtext = plot.legend)
ppcomp  (list(melted_w, melted_l, melted_g), legendtext = plot.legend)

BIC(lm(after_w$data~1))
BIC(lm(after_l$data~1))
BIC(lm(after_g$data~1))