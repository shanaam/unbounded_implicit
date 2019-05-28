
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

GetRowsWithString <- function (string, df) {
  return(df[grep(string, df$task), ])
}

noCurMedians <- read.csv("data/BLCorrectedNoCursorMedians.csv")
noCurMedians <- noCurMedians[2:34,]
noCurMedians$baselineMedians <- NULL

# reshape
# reshape needs funky column names in the form: characters.number, for columns that need to be merged::
colnames(noCurMedians) <- c('participant', 'exc15.1', 'exc30.2', 'exc45.3', 'exc60.4', 'inc15.5', 'inc30.6', 'inc45.7', 'inc60.8')

# reshape the plots so that ALL dependent variables are in one column (deviation for Tap data)
noCurMedians <- reshape(noCurMedians, direction="long", varying=c('exc15.1', 'exc30.2', 'exc45.3', 'exc60.4', 'inc15.5', 'inc30.6', 'inc45.7', 'inc60.8'), timevar='task', v.names='meanDeviation', times=c('exc15', 'exc30', 'exc45.3', 'exc60', 'inc15', 'inc30', 'inc45', 'inc60'))
noCurMedians$rot <- NaN
noCurMedians$strat<- NaN

noCurMedians$strat[1:132] <- "exc"
noCurMedians$strat[133:264] <- "inc"

sequence <- rep(15, 33)
sequence <- append(sequence, rep(30, 33))
sequence <- append(sequence, rep(45, 33))
sequence <- append(sequence, rep(60, 33))
sequence <- append(sequence, sequence)

noCurMedians$rot <- sequence

stepwiseNoCurSummary <- summarySE(data = noCurMedians, measurevar = "meanDeviation", na.rm = TRUE, groupvars = c("rot", "strat"))


# 60 degree
noCur60rawEI <- read.csv("data/60degRotation/60_rigid_implicit_EI_trial.csv")
noCur60rawIE <- read.csv("data/60degRotation/60_rigid_implicit_IE_trial.csv")


noCur60raw <- data.frame(append(noCur60rawEI, noCur60rawIE[, 5:16]))
noCur60rawInc <- GetRowsWithString("no-cursor_60_inc", noCur60raw)
noCur60rawInc <- noCur60rawInc[, 5:29]
noCur60IncMedians <- apply(noCur60rawInc, 2, median, na.rm = TRUE)

noCur60rawExc <- GetRowsWithString("no-cursor_60_exc", noCur60raw)
noCur60rawExc <- noCur60rawExc[, 5:29]
noCur60ExcMedians <- apply(noCur60rawExc, 2, median, na.rm = TRUE)

noCur60 <- data.frame(deviation = append(noCur60IncMeans, noCur60ExcMedians),
                      strat = append(rep('inc', 25), rep('exc', 25)))

# summary
abruptNoCurSummary <- summarySE(data = noCur60, measurevar = "deviation", na.rm = TRUE, groupvars = "strat")

# combined nocusor data

abruptNoCurSummary$exp <- "abrupt"
stepwiseNoCurSummary$exp <- "stepwise"
stepwise60 <- stepwiseNoCurSummary[7:8, ]
stepwise60$rot <- NULL
colnames(stepwise60) <- c('strat', 'N', 'deviation', 'sd', 'se', 'ci', 'exp')


bothExps <- rbind(stepwise60, abruptNoCurSummary)


# plots
library(ggplot2)
library(ggbeeswarm)

p <- ggplot(stepwiseNoCurSummary, aes(rot, meanDeviation, colour = strat)) +
  geom_beeswarm(data = noCur60, aes(x = 85 + 5, y = deviation), size = 3, alpha = 0.6) +
  geom_beeswarm(data = noCurMedians, aes(x = rot + 5, y = meanDeviation), size = 3, alpha = 0.6) +
  geom_point(size = 14, alpha = 1) + 
  geom_linerange(aes(ymin = meanDeviation - ci, ymax = meanDeviation + ci), 
                  lwd = 20, alpha = 0.4) +
  geom_point(size = 14, data = abruptNoCurSummary, aes(x = 85, y = deviation), alpha = 1) + 
  geom_linerange(data = abruptNoCurSummary, aes(x = 85, y = deviation, ymin =deviation - ci, ymax = deviation + ci), 
                  lwd = 20, alpha = 0.4) +
  scale_x_continuous(limits = c(10, 95), breaks = c(15, 30, 45, 60, 85), labels = c(15, 30, 45, 60, 60), name = "rotation size") +
  scale_y_continuous(limits = c(0, 45), breaks = c(0, 15, 30, 45), name = "cursor deviation") +
  theme_minimal() +
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB")) + 
  scale_color_manual(values=c("#FFA47E", "#00BFC4"), name="Strategy Use",  
                     breaks=c("exc", "inc"),
                     labels=c("without strategy", "with strategy")) 
  
p

ggsave(p, height = 14, width = 24, device = "svg", filename = "data/noCur_plot.svg")

# comparing groups
r <- ggplot(bothExps, aes(x = strat, y = deviation, colour = exp)) +
  theme_minimal() +
  geom_beeswarm(data = noCur60, aes(x = strat, y = deviation, colour = "#005DE4"), size = 3, alpha = 0.6) +
  geom_beeswarm(data = noCurMedians[noCurMedians$rot == 60, ], aes(x = strat, y = meanDeviation, colour = "#D40000"), size = 3, alpha = 0.6) +
  geom_point(size = 14) + 
  geom_linerange(aes(ymin =deviation - ci, ymax = deviation + ci), lwd = 20, alpha = 0.4) +
  scale_x_discrete(name = "strategy use", labels = c('without strategy', 'with strategy')) +
  scale_y_continuous(limits = c(0, 45), breaks = c(0, 15, 30, 45, 60), name = "cursor deviation") +
  scale_color_manual(values=c("#005DE4", "#D40000", "#005DE4", "#D40000"), labels=c("abrupt experiment", "stepwise experiment", "abrupt experiment", "stepwise experiment"), name = 'Experiment') + 
  theme(text = element_text(size=40), axis.text = element_text(size=40), legend.text = element_text(size=48), panel.grid.major.y = element_line(colour = "#ABABAB"))

r
ggsave(r, height = 14, width = 20, device = "svg", filename = "data/both_noCur_plot.svg")


# q <- ggplot(abruptNoCurSummary, aes(x = 60, y = deviation, colour = strat)) +
#   geom_point(size = 8) + 
#   geom_pointrange(aes(ymin =deviation - ci, ymax = deviation + ci), lwd = 8, alpha = 0.4, fatten = 0) +
#   scale_x_continuous(breaks = 60, name = "rotation size") +
#   scale_y_continuous(limits = c(0, 60), breaks = c(15, 30, 45, 60), name = "cursor deviation") +
#   scale_colour_hue(name="Strategy Use",  
#                    breaks=c("exc", "inc"),
#                    labels=c("exclude", "include")) +
#   theme_minimal() +
#   theme(text = element_text(size=36), axis.text = element_text(size=36), legend.text = element_text(size=36))
# q
# ggsave(q, height = 14, width = 16, device = "svg", filename = "data/abrupt_noCur_plot.svg")

