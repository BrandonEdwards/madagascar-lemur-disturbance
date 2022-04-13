####### Script Information ########################
# Brandon P.M. Edwards & Alexandria Cosby
# Madagascar Lemur Disturbance
# 01-preliminary-modelling.R
# Created March 2022
# Last Updated April 2022

####### Import Libraries and External Files #######

library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

####### Read Data #################################

disturbance <- read.csv("data/disturbance_data.csv")

####### Wrangle Data ##############################

disturbance <- disturbance[-which(disturbance$Community == "0"), ]

# Add in wet/dry season
disturbance$Season <- NA
disturbance$Season <- ifelse((disturbance$Month %in% c(12,1,2,3)),
                             "Wet",
                             "Dry")

png(file = "output/dist_histogram.png",
    width = 6, height = 4, res = 300, units = "in")
hist(disturbance$Disturbance)
dev.off()

####### Overall Year/Season Model #########################

stan_data_ys <- list(N = nrow(disturbance),
                  y = disturbance$Disturbance,
                  n_seasons = 2,
                  season = as.integer(factor(disturbance$Season, levels = c("Wet", "Dry"))),
                  n_years = length(unique(disturbance$Year)),
                  year = as.integer(factor(as.character(disturbance$Year), levels = c("2015",
                                                                                      "2016",
                                                                                      "2017",
                                                                                      "2018",
                                                                                      "2019",
                                                                                      "2020",
                                                                                      "2021"))))

model_ys <- stan_model(file = "models/uni_zip_ys.stan")

stan_job_ys <- sampling(model_ys,
                     data = stan_data_ys,
                     verbose = TRUE,
                     chains = 3,
                     iter = 2000,
                     warmup = 1000,
                     cores = 3,
                     pars = c("theta", "lambda"))

summary_ys <- summary(stan_job_ys)$summary
summary_ys <- as.data.frame(summary_ys[1:nrow(summary_ys)-1, c(1:4,8)])
names(summary_ys) <- c("mean", "se_mean", "sd", "lower", "upper")
summary_ys$Season <- rep(c(rep("Wet", length(unique(disturbance$Year))),
                       rep("Dry", length(unique(disturbance$Year)))), 2)
summary_ys$Year <- rep(seq(2015,2021), 4)
summary_ys$Parameter <- c(rep("Theta", 14),
                          rep("Lambda", 14))

p_no_disturbance_ys <- ggplot(data = summary_ys[which(summary_ys$Parameter == "Theta"), ]) +
  geom_point(aes(x = Year, y = 1-mean, group = Season, color = Season)) +
 # geom_errorbar(aes(x = Year, ymin = lower, ymax = upper, color = Season)) +
  geom_line(aes(x = Year, y = 1-mean, group = Season, color = Season)) +
  ylab("Probability of Disturbance") +
  NULL

rate_disturbance_ys <- ggplot(data = summary_ys[which(summary_ys$Parameter == "Lambda"), ]) +
  geom_point(aes(x = Year, y = mean, group = Season, color = Season)) +
  geom_line(aes(x = Year, y = mean, group = Season, color = Season)) +
  NULL

####### Disturbance by Community/Year #########################

stan_data_comm <- list(N = nrow(disturbance),
                     y = disturbance$Disturbance,
                     n_years = length(unique(disturbance$Year)),
                     year = as.integer(factor(as.character(disturbance$Year), levels = c("2015",
                                                                                         "2016",
                                                                                         "2017",
                                                                                         "2018",
                                                                                         "2019",
                                                                                         "2020",
                                                                                         "2021"))),
                     n_comms = length(unique(disturbance$Community)),
                     comm = as.integer(factor(disturbance$Community),
                                       levels = c("Ambarindahy", "Andranohobaka", "Maevatanimbary")))

model_comm <- stan_model(file = "models/uni_zip_comm.stan")

stan_job_comm <- sampling(model_comm,
                        data = stan_data_comm,
                        verbose = TRUE,
                        chains = 3,
                        iter = 2000,
                        warmup = 1000,
                        cores = 3,
                        pars = c("theta", "lambda"))

summary_comm <- summary(stan_job_comm)$summary
summary_comm <- as.data.frame(summary_comm[1:(nrow(summary_comm)-1), c(1:4,8)])
names(summary_comm) <- c("mean", "se_mean", "sd", "lower", "upper")
summary_comm$Community <- rep(c(rep("Ambarindahy", length(unique(disturbance$Year))),
                           rep("Andranohobaka", length(unique(disturbance$Year))),
                           rep("Maevatanimbary", length(unique(disturbance$Year)))), 2)
summary_comm$Year <- rep(seq(2015,2021), 6)
summary_comm$Parameter <- c(rep("Theta", 21),
                          rep("Lambda", 21))
summary_comm <- summary_comm[1:nrow(summary_comm) - 1, ]

p_no_disturbance_comm <- ggplot(data = summary_comm[which(summary_comm$Parameter == "Theta"), ]) +
  geom_point(aes(x = Year, y = 1-mean, group = Community, color = Community)) +
  #geom_errorbar(aes(x = Year, ymin = lower, ymax = upper, color = Community)) +
  geom_line(aes(x = Year, y = 1-mean, group = Community, color = Community)) +
  ylab("Probability of Disturbance") +
  NULL

rate_disturbance_comm <- ggplot(data = summary_comm[which(summary_comm$Parameter == "Lambda"), ]) +
  geom_point(aes(x = Year, y = mean, group = Community, color = Community)) +
  geom_line(aes(x = Year, y = mean, group = Community, color = Community)) +
  NULL

####### Output plots ##############################

png(filename = "output/p_no_disturbance_ys.png",
    width = 6, height = 6, res = 300, units = "in")
print(p_no_disturbance_ys)
dev.off()

png(filename = "output/rate_disturbance_ys.png",
    width = 6, height = 6, res = 300, units = "in")
print(rate_disturbance_ys)
dev.off()

png(filename = "output/p_no_disturbance_comm.png",
    width = 6, height = 6, res = 300, units = "in")
print(p_no_disturbance_comm)
dev.off()

png(filename = "output/rate_disturbance_comm.png",
    width = 6, height = 6, res = 300, units = "in")
print(rate_disturbance_comm)
dev.off()

