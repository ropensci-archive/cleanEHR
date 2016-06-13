# 
library(ccdata)
reload()
if (!exists("ccd_delta_num"))
    load('../data/delta_num.Rdata')
ccd <- ccd_delta_num

dt.sofa <- ccDataTable2(conf=yaml.load_file('tests/data/test_yml.yml'), record=ccd)
# create table with all selected items in yaml conf with cadance of 1 hour.
dt.sofa$create.table(freq=1)

# derive the traffic light missingness table.
dt.sofa$get.missingness()

# filter data by threshold. (throw out episodes)
dt.sofa$filter.missingness()

# reload - if you want to adjust parameters related with missing rate
dt.sofa$reload.conf('tests/data/test_yml.yml')
dt.sofa$filter.missingness()

# imputation - substituting missing values
dt.sofa$imputation()
