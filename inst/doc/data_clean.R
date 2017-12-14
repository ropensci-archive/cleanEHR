## ------------------------------------------------------------------------
library(cleanEHR)
data("sample_ccd")

## ---- fig.width=10, fig.height=6, out.width='600px', results='hide', message=FALSE, warning=FALSE----
# contains all the 1D fields i.e. non-longitudinal
tb1 <- ccd_demographic_table(ccd)

# filter out all dead patient. (All patients are dead in the dataset.)
tb1 <- tb1[DIS=="D"]

# subset variables we want (ARSD = Advanced respiratory support days,
# apache_prob = APACHE II probability)
tb <- tb1[, c("SEX", "ARSD", "apache_prob"), with=FALSE]
tb <- tb[!is.na(apache_prob)]

# plot
library(ggplot2)
ggplot(tb, aes(x=apache_prob, y=ARSD, color=SEX)) + geom_point()


## ------------------------------------------------------------------------
# To prepare a YAML configuration file like this. You write the following text
# in a YAML file. 
conf <- "
NIHR_HIC_ICU_0108:
  shortName: hrate
NIHR_HIC_ICU_0112:
  shortName: bp_sys_a
  dataItem: Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure
NIHR_HIC_ICU_0093:
   shortName: sex
"
library(yaml)
conf <- yaml.load(conf)

## ------------------------------------------------------------------------
# conf is the full path of the YAML configuration.
tb <- create_cctable(ccd, conf, freq=1)
print(tb$torigin) # the table

## ------------------------------------------------------------------------
tb$tclean[, mean(NIHR_HIC_ICU_0108, na.rm=TRUE), by=c("site", "episode_id")]

## ------------------------------------------------------------------------
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  range:
    labels:
      red: (0, 300)
      amber: (11, 150]
      green: (50, 100]
    apply: drop_entry
NIHR_HIC_ICU_0112:
  shortName: bp_sys_a
  dataItem: Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure
NIHR_HIC_ICU_0093:
   shortName: sex
   category:
      M: male
      F: female
      m: male
      f: female
"
conf <- yaml.load(conf)

## ------------------------------------------------------------------------
tb <- create_cctable(ccd, conf, freq=1)
tb$filter_range("amber") # chose only the entry with amber
tb$apply_filters() # apply the filter to the clean table

## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
cptb <- rbind(cbind(tb$torigin, data="origin"), 
              cbind(tb$tclean, data="clean"))

ggplot(cptb, aes(x=time, y=NIHR_HIC_ICU_0108, color=data)) + 
  geom_point(size=1.5) + facet_wrap(~episode_id, scales="free_x")

## ------------------------------------------------------------------------
#tb$reset() # reset the all the filters first.
tb$filter_range("green")
tb$apply_filters()

## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
cptb <- rbind(cbind(tb$torigin, data="origin"), 
              cbind(tb$tclean, data="clean"))

ggplot(cptb, aes(x=time, y=NIHR_HIC_ICU_0108, color=data)) + 
  geom_point(size=1.5) + facet_wrap(~episode_id, scales="free_x")

## ------------------------------------------------------------------------
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
NIHR_HIC_ICU_0112:
  shortName: bp_sys_a
  dataItem: Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure
NIHR_HIC_ICU_0093:
   shortName: sex
   category:
    levels:
      M: male
      F: female
      m: male
      f: female
    apply: drop_entry
"
conf <- yaml.load(conf)

# Try to modify the original data
tb$torigin$NIHR_HIC_ICU_0093[1] <- "ERROR"

tb$reload_conf(conf)  # change configuration file
tb$filter_categories() 
tb$apply_filters() 

## ------------------------------------------------------------------------
unique(tb$torigin$NIHR_HIC_ICU_0093)
unique(tb$tclean$NIHR_HIC_ICU_0093)

## ------------------------------------------------------------------------
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  missingness:
    labels:
      daily: 24
    accept_2d:
      daily: 70
    apply: drop_episode
NIHR_HIC_ICU_0112:
  shortName: bp_sys_a
  dataItem: Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure
NIHR_HIC_ICU_0093:
   shortName: sex
"
conf <- yaml.load(conf)

tb$reload_conf(conf)  # change configuration file
tb$filter_missingness() 
tb$apply_filters()

# episodes in the original data table 
unique(paste(tb$torigin$site, tb$torigin$episode_id))
# episodes in the cleaned data table
unique(paste(tb$tclean$site, tb$tclean$episode_id))

## ------------------------------------------------------------------------
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  range:
    labels:
      red: (0, 300)
      amber: (11, 150]
      green: (50, 100]
    apply: drop_entry
  missingness:
    labels:
      daily: 24
    accept_2d:
      daily: 70
    apply: drop_episode
  nodata:
    apply: drop_episode
NIHR_HIC_ICU_0112:
  shortName: bp_sys_a
  dataItem: Systolic Arterial blood pressure - Art BPSystolic Arterial blood pressure
NIHR_HIC_ICU_0093:
   shortName: sex
   category:
    levels:
      M: male
      F: female
      m: male
      f: female
    apply: drop_entry
"
conf <- yaml.load(conf)

# Method 1
tb <- create_cctable(ccd, conf, freq=1)
tb$filter_range("amber")
tb$filter_missingness()
tb$filter_nodata()
tb$filter_categories()
tb$apply_filters() 

tb$reset() # reset

# Method 2
#tb$clean()

## ------------------------------------------------------------------------
# Initialise the simulated ccRecord
hr <- c(rep(80, 10), rep(NA, 10), rep(90, 10), NA, NA, rep(90, 10), rep(NA, 10), 180, NA, NA, 
        rep(90, 10), 180, NA, 0, NA, NA, rep(60, 10))
# hr <- hr + runif(length(hr)) * 15 # adding noise if needed. 
data <- data.frame(time=as.numeric(seq(hr)), item2d=hr)
rec <- ccRecord()+new.episode(list(NIHR_HIC_ICU_0108=data))

# Prepare the plotting function
library(data.table)
plot_imputation <- function() {
cptb <- data.table(episode_id=as.integer(tb$torigin$episode_id), 
                   time=tb$torigin$time, origin=tb$torigin$NIHR_HIC_ICU_0108, 
                   clean=tb$tclean$NIHR_HIC_ICU_0108)

ggplot(cptb, aes(x=time)) + 
  geom_point(size=5, shape=16, aes(y=origin), colour="red") + 
  geom_point(size=2, aes(y=clean)) + 
  geom_line(aes(y=clean)) + 
  scale_x_continuous(minor_breaks = seq(length(hr)))+ 
  theme(panel.grid.minor = element_line(colour="grey", size=0.5), 
        panel.grid.major = element_line(colour="grey", size=0.5))
}




## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
# mock the configuration YAML
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  missingness:
    impute:
      lead: 2 # 2 previous values 
      lag: 2  # 2 later values
      fun: median # missing value filled by the median of 2 previous and 2 later values. 
  nodata:
    apply: drop_episode
"

conf <- yaml.load(conf)
tb <- create_cctable(rec, conf, freq=1)
tb$imputation()
plot_imputation()

## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  missingness:
    impute:
      lead: 10
      lag: 10
      fun: median
  nodata:
    apply: drop_episode
"

rec <- ccRecord()+new.episode(list(NIHR_HIC_ICU_0108=data))

conf <- yaml.load(conf)
tb <- create_cctable(rec, conf, freq=1)
tb$imputation()
plot_imputation()

## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  missingness:
    impute:
      lead: 10
      lag: 10
      fun: mean
  nodata:
    apply: drop_episode
"

rec <- ccRecord()+new.episode(list(NIHR_HIC_ICU_0108=data))

conf <- yaml.load(conf)
tb <- create_cctable(rec, conf, freq=1)
tb$imputation()
plot_imputation()


## ---- fig.width=12, fig.height=12, out.width='700px', results='hide', message=FALSE, warning=FALSE----
conf <- "NIHR_HIC_ICU_0108:
  shortName: h_rate
  dataItem: Heart rate
  missingness:
    impute:
      lead: 40
      lag: 40
      fun: myfun
  nodata:
    apply: drop_episode
"
# Define my own interpolation function. 
# We use piecewise polynomial interpolation spline here for 
# the demonstration purpose. 
myfun <- function(x) {
    return(splinefun(x)(ceiling(length(x)/2)))
}


conf <- yaml.load(conf)
tb <- create_cctable(rec, conf, freq=1)
tb$imputation()
plot_imputation()

