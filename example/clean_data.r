# 
library(ccdata)
reload()
if (!exists("ccd_delta_num"))
    load('../data/delta_num.Rdata')
ccd <- ccd_delta_num

dt.sofa <- ccTable(conf=yaml.load_file('tests/data/test_sofa.yml'), record=ccd)
# create table with all selected items in yaml conf with cadance of 1 hour.
dt.sofa$create.table(freq=1)
dt.sofa$filter.ranges()
dt.sofa$filter.category()
dt.sofa$filter.missingness()
dt.sofa$filter.nodata()
dt.sofa$apply.filters()

# imputation - substituting missing values
dt.sofa$imputation()
