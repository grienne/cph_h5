library(ggplot2)
require(lme4)
require(dplyr)


dat <- read.csv(file = "~/Dropbox/CPH738_002/data/EX854_V3_DH.csv", header = T,
                na.strings=c("","NA"))

## handling date observations 
## Date of first StUD Dx
dat$StUD_XDt <- as.Date(dat$StUD_XDt)
## Date of first remission
dat$remis_dt <- as.Date(dat$remis_dt)
## Time to remission (in days)
dat$Days.to.remission <- with(dat,
                              difftime(remis_dt, StUD_XDt, units = "days")
                              )
## make it a numeric variable
dat$Days.to.remission <- as.numeric(dat$Days.to.remission)
summary(dat$Days.to.remission)

## State indicator 
dat$State <- with(dat,
                  ifelse(EGEOLOC == 41, 'alabama',
                  ifelse(EGEOLOC == 52, 'arizona',
                  ifelse(EGEOLOC == 46, 'arkansas',
                  ifelse(EGEOLOC == 62, 'california',
                  ifelse(EGEOLOC == 53, 'colorado',
                  ifelse(EGEOLOC == 4,  'connecticut',
                  ifelse(EGEOLOC == 32, 'delaware',
                  ifelse(EGEOLOC == 31, 'district of columbia',
                  ifelse(EGEOLOC == 33, 'florida',
                  ifelse(EGEOLOC == 34, 'georgia',
                  ifelse(EGEOLOC == 54, 'idaho',
                  ifelse(EGEOLOC == 16, 'illinois',
                  ifelse(EGEOLOC == 17, 'indiana',
                  ifelse(EGEOLOC == 22, 'iowa',
                  ifelse(EGEOLOC == 23, 'kansas',
                  ifelse(EGEOLOC == 42, 'kentucky',
                  ifelse(EGEOLOC == 47, 'louisiana',
                  ifelse(EGEOLOC ==  5, 'maine',
                  ifelse(EGEOLOC == 35, 'maryland',
                  ifelse(EGEOLOC ==  6, 'massachusetts',
                  ifelse(EGEOLOC == 18, 'michigan',
                  ifelse(EGEOLOC == 24, 'minnesota',
                  ifelse(EGEOLOC == 43, 'mississippi',
                  ifelse(EGEOLOC == 25, 'missouri',
                  ifelse(EGEOLOC == 55, 'montana',
                  ifelse(EGEOLOC == 26, 'nebraska',
                  ifelse(EGEOLOC == 56, 'nevada',
                  ifelse(EGEOLOC ==  7, 'new hampshire',
                  ifelse(EGEOLOC == 11, 'new jersey',
                  ifelse(EGEOLOC == 57, 'new mexico',
                  ifelse(EGEOLOC == 12, 'new york',
                  ifelse(EGEOLOC == 36, 'north carolina',
                  ifelse(EGEOLOC == 27, 'north dakota',
                  ifelse(EGEOLOC == 19, 'ohio',
                  ifelse(EGEOLOC == 48, 'oklahoma',
                  ifelse(EGEOLOC == 64, 'oregon',
                  ifelse(EGEOLOC == 13, 'pennsylvania',
                  ifelse(EGEOLOC ==  8, 'rhode island',
                  ifelse(EGEOLOC == 37, 'south carolina',
                  ifelse(EGEOLOC == 28, 'south dakota',
                  ifelse(EGEOLOC == 44, 'tennessee',
                  ifelse(EGEOLOC == 49, 'texas',
                  ifelse(EGEOLOC == 58, 'utah',
                  ifelse(EGEOLOC ==  9, 'vermont',
                  ifelse(EGEOLOC == 38, 'virginia',
                  ifelse(EGEOLOC == 65, 'washington',
                  ifelse(EGEOLOC == 39, 'west virginia',
                  ifelse(EGEOLOC == 20, 'wisconsin',
                  ifelse(EGEOLOC == 59, 'wyoming',
                         NA))))))))))))))))))))))))))))))))))))))))))))))))))

## log-transform days to remission
dat$log.days <- log(dat$Days.to.remission)

## ----------------------------------------
## plot lm() estimates (i.e., no pooling)
## ----------------------------------------
## fit a linear model
fit.lm <- lm(log.days ~ State - 1, data = dat)

## put the results into a data frame
lm.dat <- summary(fit.lm)$coefficients %>% data.frame
names(lm.dat) <- c("mean", "SE", "t.value", "P.value")
lm.dat$State <- sub("State", "", names(coef(fit.lm)))

## get sample sizes for each state
counts <- aggregate(log.days ~ State, data = dat, FUN = length)
lm.dat$counts <- counts$log.days

## plot estimates by sample size
x11()
p <- ggplot(aes(x = counts, y = mean), data = lm.dat)
p + geom_point(size = 2) +
    geom_linerange(aes(ymin = mean - qnorm(0.975) * SE,
                       ymax = mean + qnorm(0.975) * SE),
                   alpha = 0.6) +
    geom_hline(yintercept = mean(dat$log.days, na.rm = T),
               linetype = 'dashed') +
    scale_x_continuous("Sample size of State j") +
    scale_y_continuous("Sample mean log days to remission with 95% CIs") +
    theme_bw()

dev.copy2pdf(file = "lm_means.pdf")


## --------------------------------
## Mixed-effects model
## --------------------------------
## mixed-effects model
fit.mlm <- lmer(log.days ~ 1 + (1|State),
                data = dat)
## put everything into a data frame
rr <- ranef(fit.mlm, condVar = TRUE)
mlm.dat <- rr %>% data.frame
mlm.dat$mean <- mlm.dat$condval + fixef(fit.mlm)
mlm.dat$counts <- counts$log.days
    
## plot estimates by sample size
x11()
p <- ggplot(aes(x = counts, y = mean), data = mlm.dat)
p + geom_point(size = 2) +
    geom_linerange(aes(ymin = mean - qnorm(0.975) * condsd,
                       ymax = mean + qnorm(0.975) * condsd),
                   alpha = 0.6) +
    geom_hline(yintercept = mean(dat$log.days, na.rm = T),
               linetype = 'dashed') +
    scale_x_continuous("Sample size of State j") +
    scale_y_continuous("Sample mean log days to remission with 95% CIs",
                       limits = c(-4, 8.5)) +
    theme_bw()

dev.copy2pdf(file = "mlm_means2.pdf")
