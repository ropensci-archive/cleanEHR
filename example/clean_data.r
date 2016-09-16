# 
library(ccdata)
reload()
if (!exists("ccd_delta_num"))
    load('../data/delta_num.Rdata')
ccd <- ccd_delta_num

dt.sofa <- create.cctable(ccd, freq=1, conf='tests/data/ANALYSIS_REF.yaml')
dt.sofa$filter.ranges()
dt.sofa$filter.category()
dt.sofa$filter.missingness()
dt.sofa$filter.nodata()
dt.sofa$apply.filters()

# imputation - substituting missing values
dt.sofa$imputation()
