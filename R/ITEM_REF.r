#' convert a yaml file to ITEMREF Rdata which will be loaded when the library
#' is called.
#' @export importItemRef
importItemRef <- function(yml) {
    require(yaml)
    ITEM_REF <- yaml.load_file(yml)
    save(ITEM_REF, file="ITEM_REF.RData")
}
