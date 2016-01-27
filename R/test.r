#' convert degree of latitue [deg] to area [Ha]
#' @param lat latitue
#' @param res resolution
#' @return area in Ha 
deg2area<-function(lat,res=0.5){
    deg2rad <- function(deg){
        return (deg*pi*0.00555555555555)
    }
    area<-(111e3*res)*(111e3*res)*cos(deg2rad(lat))/10000#ha
    return(area)
}


#' convert degree of latitue [deg] to area [m^2]
#' @param lat latitue
#' @param res resolution
#' @return area in Ha 
deg2area_m2<-function(lat,res=0.5){
    deg2rad <- function(deg){
        return (deg*pi*0.00555555555555)
    }
    area<-(111e3*res)*(111e3*res)*cos(deg2rad(lat))
    return(area)
}
