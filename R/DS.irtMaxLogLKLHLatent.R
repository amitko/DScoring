DS.irtMaxLogLKLHLatent <- function(
                            parameters, # item parameters 
                            response,   # a scalar or a vector of dichotomous response
                            max_est,    # maximum estimate given to all-correct response
                            min_est,    # minimum estimate given to all-incorrect response
                            o = DS.options()){
  
  if (sum(response) == length(response)){
    estimate <- max_est
  }
  else if (sum(response) == 0){
    estimate <- min_est
  }
  else{
    estimate <- optimize(DS.irtLogLKLH,
                         interval = c(min_est, max_est),
                         maximum = TRUE,
                         parameters = parameters,
                         response = response)$max
  }
  
  return(estimate)
}