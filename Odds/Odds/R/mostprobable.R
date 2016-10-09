' Most probable row
#'
#' This function calculates the moste probable playable row given a table of odds for thirteen games
#' @param table of odds, default = NONE
#' @keywords cats
#' @export
#' @examples
#' mostprobable()

mostprobable<-function(table=NONE){ #function caluclates most probable playable outcome
  if(table==NONE){
    print("You must pass a table of odds to the function")
  }
  else{
  probs<-numeric(13)
  for(i in 1:13){
    m<-max(table[i,])
    if(m>0.5){
      probs[i]<-m
    }
    else{
      probs[i]<-1-min(table[i,])
    }
  }
  return(probs)
  }

}
