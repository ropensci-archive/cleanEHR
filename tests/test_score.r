library(dataplay)
library(devtools)
reload(".")



if(!exists("ccd"))
    load("xmlData.Rdata")

range <- c(0,100,200,300,400)
score.ref <-seq(4,1)
fun <- function(time, val){
    return(min(as.numeric(val), na.rm=TRUE))
}

computeItemScore(ccd, 1, getItemInfo("PaO2/FiO2 ratio")['NHIC_code'], range,
                 score.ref, fun)






#------------------------------
# compute sofa 
#------------------------------
sofaMin <- function(time, val){
    return(min(as.numeric(val), na.rm=TRUE))
}


sofaMax <- function(time, val){
    return(max(as.numeric(val), na.rm=TRUE))
}



items.sofa <- c("PaO2/FiO2 ratio", "Platelets", "Bilirubin", 
                "GCS - total", "Creatinine")
check.items <- c('Height', 
                 'Weight',
                 'Hospital housing location (in)',
                 'Location (in)',
                 'Admission type',
                 'Treatment function code',
                 'classification of surgery',
                 'Level 2 (HDU) days',
                 'Level 3 (ICU) days',
                 'Discharge status (Reason for discharge from your unit)',
                 'Discharge location (location out)',
                 'Timeliness of discharge from your unit',
                 'Level of care at discharge from your unit',
                 'Dead or alive on discharge')
check.items.code <- getItemsInfo(check.items, "NHIC_code")
items.sofa.code <- getItemsInfo(items.sofa, "NHIC_code")

# range has to be non-decrease
range <- list("PaO2/FiO2 ratio" = c(0, 100, 200, 300, 400),#mmHg
              "Platelets"       = c(0, 20, 50, 100, 150), #x10^3/mm^3
              #TODO: dummy upper limit should be improved.
              "Bilirubin"       = c(20, 33, 102, 204, 10000000),# umol/l
              "GCS - total"     = c(0, 6, 10, 13, 14), # N/A
              "Creatinine"      = c(0, 110, 171, 300, 440)) #umol/l

score.ref <- list("PaO2/FiO2 ratio" = seq(4, 1), 
                  "Platelets"       = seq(4, 1), 
                  "Bilirubin"       = seq(1, 4), 
                  "GCS - total"     = seq(4, 1),
                  "Creatinine"      = seq(1, 4))

fun.list <- list("PaO2/FiO2 ratio" =  sofaMin,
                 "Platelets"       =  sofaMin,
                 "Bilirubin"       =  sofaMax,
                 "GCS - total"     =  sofaMin,
                 "Creatinine"      =  sofaMax)

#sofa.score <- computeScore(ccd, items.sofa.code, range, score.ref, fun.list, check.items.code)

findAliveDead <- function(x) {
    alive.dead.code <- as.character(getItemsInfo('Dead or alive on discharge', 'NHIC_code'))
    return(as.character(x$items$val[x$items$item==alive.dead.code]))
}

score.mort <- data.frame(score=unlist(lapply(sofa.score, function(x) x$score)),
                         mortality=as.character(unlist(lapply(sofa.score, findAliveDead))))

death.rate<-computeMortalityRate(score.mort)

plot_mortality_sofa <- function(death.rate) {
    gg <- ggplot(death.rate, aes(x=factor(score),
                                 y=death.rate))
    gg <- gg + geom_bar(stat="identity")
    gg <- gg + ggtitle("Mortality rate vs. SOFA score(cardiovascular part missing)")
    gg <- gg + xlab("SOFA score")
    gg <- gg + ylab("Mortality Rate")
    return(gg)
}

gg <- plot_mortality_sofa(death.rate)
ggsave("tests/plots/sofa_vs_mortality.png")
