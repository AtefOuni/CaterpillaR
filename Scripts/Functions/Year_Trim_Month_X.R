
Year_To_Month_X <- function (Target, Source) {
  
  Result <- vector()
  
  for(i in time(Target)) {
   Prop <- prop.table(window(Source, start = c(i,1), end = c(i,12)))
   Result <- c(Result, Target[time(Target)==i] * Prop)
  }
  
  Result <- ts(Result, start = start(Source), end = end(Source), frequency = 12)
  
  return(Result)
}



Trim_To_Month_X <- function (Target) {

  Result <- vector()
  
  for(i in Target) {
    Result <- c(Result, rep(i/3, times = 3)) 
  }
  
  Result <- ts(Result, start = c(2006,1), frequency = 12)

return(Result)
}
