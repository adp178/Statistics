library(tidyverse)

sprt <- function(null_hyp = 0.45,alt_hyp= 0.55,a0=0.01,a1=0.01, bernoulli_p = 0.3){
  
  k = 0 
  log_lk = 0
  step_converge = 0
  a = log(a1/(1-a0))
  b = log((1-a1)/a0)
  acc_hyp = -1
  # a and b for threshold scheme for accepting h1 and h0
  while(TRUE)
  {
    data = rbinom(1, 1, bernoulli_p) #getting 1 at a time bernoulli random variable (special case of binomial where n=1)
    ll = (data*log(alt_hyp) + (1-data)*log(1-alt_hyp)) - (data*log(null_hyp) + (1-data)*log(1-null_hyp))  #log_likelihood ratio
    k= k +ll #cumulative sum
    step_converge = step_converge + 1 #step increment for convergence
    
    #accept h1 if k>=b
    if(k >= b){
      acc_hyp = 1
      break}
    #accept h0 if k<=a
    if(k<=a){
      acc_hyp = 0 
    break}
  }
  
  return(list(step_converge = step_converge, acc_hyp = acc_hyp))  
  }

#simulating tests
simulate_bernoulli <- function(bernoulli_p, number_of_simulation){      
  
  sum_steps = 0
  H0_count = 0
  H1_count = 0
  
  for(i in c(1:number_of_simulation)){            
    result = sprt(bernoulli_p=bernoulli_p)    
    
    ## if H0 is accepted
    if(result$acc_hyp == 0){                  
      H0_count = H0_count + 1
    }
    ## if H1 is true
    if(result$acc_hyp == 1){                   
      H1_count = H1_count + 1
    }
    sum_steps = sum_steps + result$step_converge
  }
  avg_steps = sum_steps/number_of_simulation
  return(list(avg_steps=avg_steps, H0_count=H0_count, H1_count=H1_count))
}
simulate_bernoulli(0.5, 1000)
