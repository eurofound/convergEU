#'  Values to patterns
#'
#'
#'  Gradients values and Delta2 are mapped to one pattern (string and number).
#'  See Eurofound 2018 report.
#'  
#'  
#'  In the mapping table within this function
#'  a pattern number 1-59 is assigned for each combination of EU 
#'  average vs MS average and their differences called delta_endpoints.
#'  Code NA is left to indicate not relevant features.
#'  In version 0.6.4The patterns have been updated from the original 12 to 59 
#'  to cover all possibile combinations.
#'  Code 60 stands for "to be visually inspected". See vignette for further info.
#'
#' @param vaMS  member state values  sorted in ascending order by time.
#' @param vaEU  EU values sorted in ascending order by time.
#' @param vaT  sorted pair of times.
#' @param remap  is FALSE for the original numerical labelling of patterns
#'              otherwise TRUE to map to old numerical correspondence.  
#' @return  a number referring to  pattern whose label depends on the
#'          indicator type as originally produced in the technical report.
#'
#'
#'
map_2_patt_59 <- function(vaMS,vaEU,vaT,remap=FALSE){
 res <- NA   
 delta_E <- vaEU[2]-vaEU[1]
 res <- map_patt_59(vaMS,vaEU,delta_E,vaT);
 # if(delta_E >  0) res <- map_up_patt_39(vaMS,vaEU,delta_E,vaT);
 # if(delta_E <  0) res <- map_down_patt_39(vaMS,vaEU,delta_E,vaT);
 # if(delta_E == 0) res <- map_const_patt_39(vaMS,vaEU,delta_E,vaT);
 if(remap){ # remapping
     # 40 is the "inspect the plot" special label
     current_map <- c(c(1:60))
     res_remap <- current_map[res]
     return(res_remap)
 }else{
     return(res)
   }
 stop("Fake gradient: fix it please!")
 };
#'
map_patt_59 <- function(vaMS,vaEU,delta_E,vaT){
  #if(delta_E <= 0)stop("Wrong function called!")
  delta_M <- vaMS[2]-vaMS[1]
  delta_endpoints <- abs(vaEU[1]- vaMS[1]) - abs(vaEU[2]- vaMS[2])
  # if delta_endpoints > 0, convergence
  # if delta_endpoints < 0, divergence
  # if delta_endpoints = 0, difference stays the same
  if(delta_E > 0 && delta_M > 0 && vaMS[2] >= vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(1)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(2)  
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] <= vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(3)  
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(4)  
  }else if(delta_E > 0 && delta_M == 0 && vaMS[2] >= vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(5)  
  }else if(delta_E > 0 && delta_M == 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(6)  
  }else if(delta_E > 0 && delta_M < 0 && vaMS[2] >= vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(7)  
  }else if(delta_E > 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(8)  
  }else if(delta_E == 0 && delta_M > 0 && vaMS[2] <= vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(9)  
  }else if(delta_E == 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(10)  
  }else if(delta_E == 0 && delta_M < 0 && vaMS[2] >= vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(11)  
  }else if(delta_E == 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(12)  
  }else if(delta_E < 0 && delta_M > 0 && vaMS[2] <= vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(13)  
  }else if(delta_E < 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(14)  
  }else if(delta_E < 0 && delta_M == 0 && vaMS[2] <= vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(15)
  }else if(delta_E < 0 && delta_M == 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(16)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] >= vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(17)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(18)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] <= vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints > 0){
    return(19)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints > 0){
    return(20)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] < vaEU[2] && vaMS[1] <= vaEU[1] && delta_endpoints < 0){ 
    return(21)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints < 0){ 
    return(22)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] >= vaEU[1] && delta_endpoints < 0){
    return(23)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints < 0){ 
    return(24)
  }else if(delta_E > 0 && delta_M == 0 && vaMS[2] < vaEU[2] && vaMS[1] <= vaEU[1] && delta_endpoints < 0){
    return(25)
  }else if(delta_E > 0 && delta_M == 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints < 0){ 
    return(26)
  }else if(delta_E > 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] <= vaEU[1] && delta_endpoints < 0){
    return(27)
  }else if(delta_E > 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints < 0){
    return(28)
  }else if(delta_E == 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] >= vaEU[1] && delta_endpoints < 0){
    return(29)
  }else if(delta_E == 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints < 0){ 
    return(30)
  }else if(delta_E == 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] <= vaEU[1] && delta_endpoints < 0){
    return(31)
  }else if(delta_E == 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints < 0){
    return(32)
  }else if(delta_E < 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] >= vaEU[1] && delta_endpoints < 0){
    return(33)
  }else if(delta_E < 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints < 0){ 
    return(34)
  }else if(delta_E < 0 && delta_M == 0 && vaMS[2] > vaEU[2] && vaMS[1] >= vaEU[1] && delta_endpoints < 0){
    return(35)
  }else if(delta_E < 0 && delta_M == 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints < 0){ 
    return(36)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] <= vaEU[1] && delta_endpoints < 0){
    return(37)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1] && delta_endpoints < 0){
    return(38)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] > vaEU[2] && vaMS[1] >= vaEU[1] && delta_endpoints < 0){
    return(39)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints < 0){
    return(40)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] < vaEU[2] && vaMS[1] < vaEU[1] && delta_endpoints == 0){
    return(41)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(42)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] == vaEU[2] && vaMS[1] == vaEU[1]  && delta_endpoints == 0){
    return(43)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(44)
  }else if(delta_E > 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(45)
  }else if(delta_E > 0 && delta_M == 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(46)
  }else if(delta_E > 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(47)
  }else if(delta_E == 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(48)
  }else if(delta_E == 0 && delta_M == 0 && vaMS[2] < vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(49)
  }else if(delta_E == 0 && delta_M == 0 && vaMS[2] > vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(50)
  }else if(delta_E == 0 && delta_M == 0 && vaMS[2] == vaEU[2] && vaMS[1] == vaEU[1]  && delta_endpoints == 0){
    return(51)
  }else if(delta_E == 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(52)
  }else if(delta_E < 0 && delta_M > 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(53)
  }else if(delta_E < 0 && delta_M == 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(54)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(55)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] > vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(56)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] == vaEU[2] && vaMS[1] == vaEU[1]  && delta_endpoints == 0){
    return(57)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] < vaEU[2] && vaMS[1] > vaEU[1]  && delta_endpoints == 0){
    return(58)
  }else if(delta_E < 0 && delta_M < 0 && vaMS[2] > vaEU[2] && vaMS[1] < vaEU[1]  && delta_endpoints == 0){
    return(59)
  }else{# require inspection
     return(60)
  }
  return(NA) 
};
#` 
#' map_down_patt_39 <- function(vaMS,vaEU,delta_E,vaT){
#'   if(delta_E >= 0)stop("Wrong function called!")
#'   delta_M <- vaMS[2]-vaMS[1]
#'   
#'   if(delta_M > 0 && vaMS[1] >= vaEU[1]){
#'     return(1)
#'   }else if(delta_M == 0 && vaMS[1] >= vaEU[1]){
#'     return(2)  
#'   }else if(delta_M < 0 && delta_M > delta_E && vaMS[1] >= vaEU[1]){
#'     return(3)  
#'   }else if(delta_M == delta_E && vaMS[1] > vaEU[1] && vaMS[2] > vaEU[2]){
#'     return(4)  
#'   }else if(delta_M < delta_E  && vaMS[2] >= vaEU[2]){
#'     return(5)  
#'   }else if(delta_M > 0 &&  vaMS[2] <= vaEU[2]){
#'     return(6)  
#'   }else if(delta_M == 0 &&  vaMS[2] <= vaEU[2]){
#'     return(7)  
#'   }else if(delta_M <0 && delta_M > delta_E &&  vaMS[2] <= vaEU[2]){
#'     return(8)  
#'   }else if(delta_M == delta_E &&  vaMS[2] < vaEU[2]){
#'     return(9)  
#'   }else if(delta_M < delta_E && vaMS[1] <= vaEU[1] ){
#'     return(10)  
#'   }else if(delta_M < delta_E && vaMS[1] > vaEU[1] && vaMS[2] <= vaEU[2]){
#'     return(29)  
#'   }else if(delta_M < 0 && delta_M > delta_E  && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
#'     return(30)  
#'   }else if(delta_M == 0 && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
#'     return(31)  
#'   }else if(delta_M > 0 && vaMS[1] <= vaEU[1] && vaMS[2] >= vaEU[2]){
#'     return(32)  
#'   }else if( vaMS[1] == vaEU[1] && vaMS[2] == vaEU[2]){
#'     return(39)
#'   }else{ # visual inspection required
#'     return(40)  
#'   }
#'   return(NA) 
#' };
#' #'
#' map_const_patt_39 <- function(vaMS,vaEU,delta_E,vaT){
#'   if(delta_E != 0)stop("Wrong function called!")
#'   delta_M <- vaMS[2]-vaMS[1]
#'   if(vaMS[1] <= vaEU[1] && vaMS[2] > vaEU[2]){
#'     return(21)
#'   }else if(delta_M > 0 && vaMS[1] > vaEU[1]){
#'     return(22)  
#'   }else if(delta_M == 0  && vaMS[1] > vaEU[1]){
#'     return(23)  
#'   }else if(delta_M < 0 && vaMS[2] > vaEU[2]){
#'     return(24)  
#'   }else if(delta_M < 0 && vaMS[1] > vaEU[1] && vaMS[2] <= vaEU[2]){
#'     return(25)  
#'   }else if(delta_M < 0  && vaMS[1] <= vaEU[1] ){
#'     return(26)  
#'   }else if(delta_M == 0 && vaMS[1] < vaEU[1] ){
#'     return(27)  
#'   }else if(delta_M > 0 && vaMS[2] <= vaEU[2]){
#'     return(28)  
#'   }else if(vaMS[1] == vaEU[1] && vaMS[2] == vaEU[2]){
#'     return(38)  
#'   }else{ # visual inspection required
#'     return(40)  
#'   }
#  return(NA) 
# };
#'


 












