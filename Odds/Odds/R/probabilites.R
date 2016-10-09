library(gtools) #depencies for calculating combinations

' probabilites
#'
#' This function calculates the probability of obtain an exact amount of correct predictions
#' @param A vector of 13 probabilites (line) and the nuber of incorrect predcitions, default = 0
#' @keywords cats
#' @export
#' @examples
#' cat_function()

probabilities<-function(line,incorrect=0){
  #find prbabilites of exact incorrect number given a play (line)
  if(incorrect==0){
    return(prod(line))
  }
  else{
    s<-seq(1:13)
    combtable<-combinations(13,incorrect,s)
    n<-nrow(combtable)
    vector<-numeric(n)
    for(i in 1:n){
      g<-combtable[i,]
      f<-line
      f[g]<-1-f[g]
      vector[i]<-prod(f)
    }
    return(sum(vector))
  }


}
