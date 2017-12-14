## ---- message=FALSE, warning=FALSE---------------------------------------
library(cleanEHR)
data("sample_ccd")

# Extract all non-longitudinal data (demographic, time, survival status, diagnosis)
dt <- ccd_demographic_table(ccd, dtype=TRUE)

## ------------------------------------------------------------------------
print(dt[1:3, ])

## ------------------------------------------------------------------------
head(ccd_unique_spell(ccd, duration=1)[, c("episode_id", "spell")])

## ------------------------------------------------------------------------
icnarc2diagnosis("1.1")
icnarc2diagnosis("1.1.4")
icnarc2diagnosis("1.1.4.27.1")

## ---- fig.width=10, fig.height=11, out.width='700px', results='hide', message=FALSE, warning=FALSE----
plot_episode(ccd@episodes[[7]], c("h_rate",  "bilirubin", "fluid_balance_d"))

