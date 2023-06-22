

# Library


library(BEKKs)
library(MTS)
library(dplyr)
library(rmgarch)
library(rugarch)
library(readxl)
library(ggplot2)
library(imputeTS)
library(readxl)
library(vars)
library(urca)
library(tseries)
library(tsDyn)
library(mgarchBEKK)
library(forecast)
library(scales)
library(cowplot)
library(strucchange)



# Importing Data

{r}
fish <- read_excel("C:/Users/pzd0035/OneDrive - Auburn University/Auburn/Research/Data/Fish_BEKK_GARCH/Data/fish_price_bekk.xlsx")
fish <- fish[-1, ]



## Lag selection and VAR

{r}
hilsa <- ts(fish$lr_hilsa_national, frequency = 12, start = c(2006,2,1))
tilapia <- ts(fish$lr_tilapia_national, frequency = 12, start = c(2006,2,1))
pangasius <- ts(fish$lr_pangasius_national, frequency = 12, start = c(2006,2,1))
rohu <- ts(fish$lr_rohu_national, frequency = 12, start = c(2006,2,1))
silver <- ts(fish$lr_silver_national, frequency = 12, start = c(2006,2,1))

price <- matrix(c(hilsa, tilapia, pangasius, rohu, silver), ncol = 5)
price <- ts(price, frequency = 12, start=c(2006,2,1))
colnames(price) <- c("hilsa", "tilapia", "pangasius", "rohu", "silver")
head(price)


library(vars)

# VARselect(price)

var <- VAR(price, type = "const", ic="SC")

summary(var)

temp <- residuals(var)
temp <- ts(temp, frequency = 12, start=c(2006,3,1))
plot.ts(temp)
head(temp)



# VECM

{r}
# vec <- ca.jo(price, ecdet = "none", type = "trace",
#              K = 2, spec = "transitory")
# summary(vec)
# 
# library(tsDyn)
# est_tsdyn <- VECM(price, lag = 2, r = 4, include = "none", estim = "ML")
# summary(est_tsdyn)
# 
# temp <- residuals(est_tsdyn)
# head(temp)
# plot.ts(temp)



# Asymmetric BEKK

{r}
# National
# price <- fish[,c(22,26,30,34,38)]
# price <- as.matrix(price)

obj_spec <- bekk_spec(model = list(type = "bekk", asymmetric = TRUE), init_values= NULL)
bekk_price <- bekk_fit(obj_spec, temp, QML_t_ratios = T, max_iter = 150, crit = 1e-09)
summary(bekk_price)

# plot(bekk_price)

print(2*pt(-1.564, 198, lower.tail=F))


# Residual test
portmanteau.test(bekk_price, lags = 6)
portmanteau.test(bekk_price, lags = 12)
res <- data.frame(residuals(bekk_price))
colnames(res) <- c("hilsa", "tilapia", "pangasius", "rohu", "silver")
archTest(residuals(bekk_price), lag = 6)
archTest(residuals(bekk_price), lag = 12)

## Li-Mak Test
library(WeightedPortTest)
sigma <- data.frame(bekk_price[["sigma_t"]])
sigma <- sigma[, c(1,6,10,13,15)]
sigma <- (sigma)^2

Weighted.LM.test(res$hilsa, sigma$Conditional.SD.of.hilsa, lag = 6, type = "correlation", fitdf = 1, weighted = F)
Weighted.LM.test(res$hilsa, sigma$Conditional.SD.of.hilsa, lag = 12, type = "correlation", fitdf = 1, weighted = F)

Weighted.LM.test(res$tilapia, sigma$Conditional.SD.of.tilapia, lag = 6, type = "correlation", fitdf = 1, weighted = F)
Weighted.LM.test(res$tilapia, sigma$Conditional.SD.of.tilapia, lag = 12, type = "correlation", fitdf = 1, weighted = F)

Weighted.LM.test(res$pangasius, sigma$Conditional.SD.of.pangasius, lag = 6, type = "correlation", fitdf = 1, weighted = F)
Weighted.LM.test(res$pangasius, sigma$Conditional.SD.of.pangasius, lag = 12, type = "correlation", fitdf = 1, weighted = F)

Weighted.LM.test(res$rohu, sigma$Conditional.SD.of.rohu, lag = 6, type = "correlation", fitdf = 1, weighted = F)
Weighted.LM.test(res$rohu, sigma$Conditional.SD.of.rohu, lag = 12, type = "correlation", fitdf = 1, weighted = F)

Weighted.LM.test(res$silver, sigma$Conditional.SD.of.silver, lag = 6, type = "correlation", fitdf = 1, weighted = F)
Weighted.LM.test(res$silver, sigma$Conditional.SD.of.silver, lag = 12, type = "correlation", fitdf = 1, weighted = F)


archTest(res$hilsa, lag = 6)
archTest(res$hilsa, lag = 12)

archTest(res$tilapia, lag = 6)
archTest(res$tilapia, lag = 12)

archTest(res$pangasius, lag = 6)
archTest(res$pangasius, lag = 12)

archTest(res$rohu, lag = 6)
archTest(res$rohu, lag = 12)

archTest(res$silver, lag = 6)
archTest(res$silver, lag = 12)


# ACF and PACF plots

ggAcf(res, lag.max = 12)
ggsave("acf_plot.jpg", width = 30, height = 30, units = c("cm"), dpi = 300)

ggPacf(res, lag.max = 12)
ggsave("pacf_plot.jpg", width = 30, height = 30, units = c("cm"), dpi = 300)



## BEKK Graphs

{r}
sigma <- as.data.frame(bekk_price[["sigma_t"]])
head(sigma)

hilsa_tilapia <- sigma$`Conditional correlation of hilsa  and  tilapia`
hilsa_rohu <- sigma$`Conditional correlation of hilsa  and  rohu`
hilsa_pangasius <- sigma$`Conditional correlation of hilsa  and  pangasius`
hilsa_silver <- sigma$`Conditional correlation of hilsa  and  silver`
tilapia_silver <- sigma$`Conditional correlation of tilapia  and  silver`
pangasius_silver <- sigma$`Conditional correlation of pangasius  and  silver`

fish1 <- fish[-1, ]
temp1 <- data.frame(fish1$date,hilsa_tilapia,hilsa_rohu,hilsa_pangasius,hilsa_silver,tilapia_silver,pangasius_silver)




ht <- ggplot()+
  geom_line(data = temp1, mapping = aes(x=fish1.date, y=hilsa_tilapia), color="black") + labs(x= "Year", y="Correlation", title = "Hilsa & Tilapia")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = mean(hilsa_tilapia), col = "red3", linetype = 2) +
  scale_y_continuous(breaks = sort(c(min(hilsa_tilapia),mean(hilsa_tilapia), max(hilsa_tilapia)))) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01))


hr <- ggplot()+
  geom_line(data = temp1, mapping = aes(x=fish1.date, y=hilsa_rohu), color="black") + labs(x= "Year", y="Correlation", title = "Hilsa & Rohu")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = mean(hilsa_rohu), col = "red3", linetype = 2) +
  scale_y_continuous(breaks = sort(c(min(hilsa_rohu),mean(hilsa_rohu), max(hilsa_rohu)))) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01))



hp <- ggplot()+
  geom_line(data = temp1, mapping = aes(x=fish1.date, y=hilsa_pangasius), color="black") + labs(x= "Year", y="Correlation", title = "Hilsa & Pangasius")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = mean(hilsa_pangasius), col = "red3", linetype = 2) +
  scale_y_continuous(breaks = sort(c(min(hilsa_pangasius),mean(hilsa_pangasius), max(hilsa_pangasius)))) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01))



hs <- ggplot()+
  geom_line(data = temp1, mapping = aes(x=fish1.date, y=hilsa_silver), color="black") + labs(x= "Year", y="Correlation", title = "Hilsa & Silver Carp")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = mean(hilsa_silver), col = "red3", linetype = 2) +
  scale_y_continuous(breaks = sort(c(min(hilsa_silver),mean(hilsa_silver), max(hilsa_silver)))) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01))



ts <- ggplot()+
  geom_line(data = temp1, mapping = aes(x=fish1.date, y=tilapia_silver), color="black") + labs(x= "Year", y="Correlation", title = "Tilapia & Silver Carp")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = mean(tilapia_silver), col = "red3", linetype = 2) +
  scale_y_continuous(breaks = sort(c(min(tilapia_silver),mean(tilapia_silver), max(tilapia_silver)))) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01))


ps <- ggplot()+
  geom_line(data = temp1, mapping = aes(x=fish1.date, y=pangasius_silver), color="black") + labs(x= "Year", y="Correlation", title = "Pangasius & Silver Carp")+ 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept = mean(pangasius_silver), col = "red3", linetype = 2) +
  scale_y_continuous(breaks = sort(c(min(pangasius_silver),mean(pangasius_silver), max(pangasius_silver)))) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01))


plot_corr <- plot_grid(ht,hr,hp,hs,ts,ps, ncol = 2, nrow = 3)
ggsave("correlation_plot.jpg", plot = plot_corr, width = 30, height = 30, units = c("cm"), dpi = 300)




## Structural Break

{r}

## Hilsa
hilsa_level <- ts(fish$hilsa_national, frequency = 12, start = c(2006,1,1))
plot.ts(hilsa_level)

sctest(hilsa_level ~ 1, type = "Chow")
hilsa_sb <- breakpoints(hilsa_level~1, h=31, breaks = 2)
summary(hilsa_sb)

confint(hilsa_sb)

date <- as.Date(fish$date)
plot(efp(hilsa_level~date, type = "OLS-CUSUM"))

## Tilapia
tilapia_level <- ts(fish$tilapia_national, frequency = 12, start = c(2006,1,1))
plot.ts(tilapia_level)
sctest(tilapia_level ~ 1, type = "Chow")
tilapia_sb <- breakpoints(tilapia_level~1, h=31, breaks = 2)
summary(tilapia_sb)

confint(tilapia_sb)

plot(efp(tilapia_level~date, type = "OLS-CUSUM"))

## Pangasius
pangasius_level <- ts(fish$pangasius_national, frequency = 12, start = c(2006,1,1))
plot.ts(pangasius_level)
sctest(pangasius_level ~ 1, type = "Chow")
pangasius_sb <- breakpoints(pangasius_level~1, h=31, breaks = 2)
summary(pangasius_sb)

confint(pangasius_sb)

plot(efp(pangasius_level~date, type = "OLS-CUSUM"))

## Rohu
rohu_level <- ts(fish$rohu_national, frequency = 12, start = c(2006,1,1))
plot.ts(rohu_level)
sctest(rohu_level ~ 1, type = "Chow")
rohu_sb <- breakpoints(rohu_level~1, h=31, breaks = 2)
summary(rohu_sb)

confint(rohu_sb)

plot(efp(rohu_level~date, type = "OLS-CUSUM"))

## Silver Carp
silver_level <- ts(fish$silver_national, frequency = 12, start = c(2006,1,1))
plot.ts(silver_level)
sctest(silver_level ~ 1, type = "Chow")
silver_sb <- breakpoints(silver_level~1, h=31, breaks = 2)
summary(silver_sb)

confint(silver_sb)

plot(efp(silver_level~date, type = "OLS-CUSUM"))





## Descriptive

{r}
colMeans(fish[,c(22,26,30,34,38)])

sapply(fish[c('lr_hilsa_national', 'lr_tilapia_national', 'lr_pangasius_national', 'lr_silver_national', 'lr_rohu_national')], sd)

sapply(fish[c('lr_hilsa_national', 'lr_tilapia_national', 'lr_pangasius_national', 'lr_silver_national', 'lr_rohu_national')], min)
sapply(fish[c('lr_hilsa_national', 'lr_tilapia_national', 'lr_pangasius_national', 'lr_silver_national', 'lr_rohu_national')], max)

sapply(fish[c('lr_hilsa_national', 'lr_tilapia_national', 'lr_pangasius_national', 'lr_silver_national', 'lr_rohu_national')], skewness)

library(Rfast)

colskewness(price, pvalue = FALSE)
colkurtosis(price, pvalue = FALSE)

jarque.bera.test(fish$lr_hilsa_national)
jarque.bera.test(fish$lr_tilapia_national)
jarque.bera.test(fish$lr_pangasius_national)
jarque.bera.test(fish$lr_rohu_national)
jarque.bera.test(fish$lr_silver_national)

Box.test(fish$lr_hilsa_national, lag = 12, type = "Ljung-Box")
Box.test(fish$lr_tilapia_national, lag = 12, type = "Ljung-Box")
Box.test(fish$lr_pangasius_national, lag = 12, type = "Ljung-Box")
Box.test(fish$lr_rohu_national, lag = 12, type = "Ljung-Box")
Box.test(fish$lr_silver_national, lag = 12, type = "Ljung-Box")

adf.test(fish$lr_hilsa_national)
adf.test(fish$lr_tilapia_national)
adf.test(fish$lr_pangasius_national)
adf.test(fish$lr_rohu_national)
adf.test(fish$lr_silver_national)

ggAcf(price, lag.max = 12)
ggsave("acf_plot_level.jpg", width = 30, height = 30, units = c("cm"), dpi = 300)

ggPacf(price, lag.max = 12)
ggsave("pacf_plot_level.jpg", width = 30, height = 30, units = c("cm"), dpi = 300)



## Plotting 

{r}
fish$date <- as.Date(fish$date, format="%Y-%m-%d")

plot_hilsa <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=hilsa_national), color="black")+ 
  geom_vline(xintercept = as.numeric(as.Date(c("2011-05-01", "2015-04-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x="Year", y="Hilsa price (taka/kg)", title = "Hilsa Nominal Price") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_hilsa

plot_hilsa_lr <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=lr_hilsa_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2011-05-01", "2015-04-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x="Year", y="Log return", title = "Hilsa Log Return") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_hilsa_lr


plot_tilapia <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=tilapia_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2009-02-01", "2012-03-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x="Year", y="Tilapia price (taka/kg)", title = "Tilapia Nominal Price") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_tilapia

plot_tilapia_lr <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=lr_tilapia_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2009-02-01", "2012-03-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x="Year", y="Log return", title = "Tilapia Log Return") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_tilapia_lr



plot_pangasius <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=pangasius_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2012-02-01", "2019-02-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x="Year", y="Pangasius price (taka/kg)", title = "Pangasius Nominal Price") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_pangasius

plot_pangasius_lr <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=lr_pangasius_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2012-02-01", "2019-02-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x="Year", y="Log return", title = "Pangasius Log Return") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_pangasius_lr




plot_rohu <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=rohu_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2009-05-01", "2012-05-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x="Year", y="Rohu price (taka/kg)", title = "Rohu Nominal Price") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_rohu

plot_rohu_lr <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=lr_rohu_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2009-05-01", "2012-05-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x="Year", y="Log return", title = "Rohu Log Return") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_rohu_lr





plot_silver <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=silver_national), color="black")+ 
  geom_vline(xintercept = as.numeric(as.Date(c("2009-03-01", "2012-04-01"))), linetype=2, size = 1, color="blue4")+ 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x="Year", y="Silver price (taka/kg)", title = "Silver Nominal Price") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_silver

plot_silver_lr <- ggplot()+
  geom_line(data = fish, mapping = aes(x=date, y=lr_silver_national), color="black")+
  geom_vline(xintercept = as.numeric(as.Date(c("2009-03-01", "2012-04-01"))), linetype=2, size = 1, color="blue4")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
  labs(x="Year", y="Log return", title = "Silver Log Return") + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

plot_silver_lr


library(cowplot)

plot_des <- plot_grid(plot_hilsa, plot_hilsa_lr, plot_tilapia, plot_tilapia_lr, plot_pangasius, plot_pangasius_lr, plot_rohu, plot_rohu_lr, plot_silver, plot_silver_lr, nrow=5, ncol=2)

ggsave("descriptive_plot.jpg", plot = plot_des, width = 30, height = 30, units = c("cm"), dpi = 300)





















