
#Assignment- Multivariate clustering EM algorithm 
#netids- Prathmesh pmk107, kaushal patel kpp98, Akash Patel adp178

library(ggplot2)
library(geometry)
library(rlist)
library(mvtnorm)
EM = function(params, X, clusters = 3, tol=.00001, maxits=1000, showits=T){
  # Arguments are params: starting parameters (means, covariances, cluster probability), 
  # X: data, clusters: number of clusters desired, tol: tolerance, maxits: maximum iterations
  
 
  # Starting points
  N = nrow(X)
  mu = params$mu
  var = params$var
  probs = params$probs
  
  # initializations
  ri = matrix(0, ncol=clusters, nrow=N)         # probability of cluster membership for each observation i
  ll = 0                                        # log likelihood
  it = 0                                        # iteration count
  converged = FALSE                             # convergence
  
  if (showits)                                  # Show iterations if showits == true
    cat(paste("Iterations of EM:", "\n"))
  
  while (!converged & (it < maxits)) { 
    probsOld = probs
    llOld = ll
    riOld = ri
    
    # Expectation
    # Compute expecations
    for (k in 1:clusters){
      ri[,k] = probs[k] * dmvnorm(X, mu[k,], sigma = var[[k]], log=F)
    }
    ri = ri/rowSums(ri)
    
    # Maximization
    rk = colSums(ri)                            # rk is weighted average cluster membership size
    probs = rk/N
    for (k in 1:clusters){
      varmat = matrix(0, ncol=ncol(X), nrow=ncol(X))        # initialize to sum matrices
      for (i in 1:N){
        varmat = varmat + ri[i,k] * X[i,]%*%t(X[i,])
      }
      mu[k,] = (t(X) %*% ri[,k]) / rk[k]
      var[[k]] =  varmat/rk[k] - mu[k,]%*%t(mu[k,])
      ll[k] = -.5 * sum( ri[,k] * dmvnorm(X, mu[k,], sigma = var[[k]], log=T) )
    }
    ll = sum(ll)
    
    ### compare old to current for convergence
    parmlistold =  c(llOld, probsOld)           # c(muOld, unlist(varOld), probsOld)
    parmlistcurrent = c(ll, probs)              # c(mu, unlist(var), probs)
    it = it + 1
    if (showits & it == 1 | it%%5 == 0)         
      cat(paste(format(it), "...", "\n", sep = ""))
    converged = min(abs(parmlistold - parmlistcurrent)) <= tol
  }
  
  clust = which(round(ri)==1, arr.ind=T)        # create cluster membership
  clust = clust[order(clust[,1]), 2]            # order accoring to row rather than cluster
  out = list(probs=probs, mu=mu, var=var, resp=ri, cluster=clust, ll=ll)
} 

x <- read.csv(file = 'C:/Users/Akash PC/Downloads/kmeans.csv')
# Create starting values
number_of_clusters = 3
mustart = matrix(sample(10:-10,number_of_clusters*5, replace= TRUE),nrow=number_of_clusters,ncol=5)
print(mustart)

#initialising parameters
covstart= list()
for (i in (1:number_of_clusters)){ 
covstart[[i]] <-cov(x)
}
covstart

for (i in (1:number_of_clusters)){
  probs <-cov(x)
}
probs = c()
for(i in (1:number_of_clusters))
{
  probs = c(probs,runif(1,0,1))
}
probs


starts = list(mu=mustart, var=covstart, probs=probs)  # params is a list of mu var and probs 

# Run and examine
test = EM(params=starts, X=as.matrix(x), clusters =number_of_clusters, tol=0.000000001, maxits=1000, showits=T)

v1 = test[["mu"]][1,] #mean vector 1
v2=test[["mu"]][2,] #mean vector 2
v3= test[["mu"]][3,] #mean vector 3
axis_1 = v2 - v3 #difference of mean vector of cluster 2 and cluster 3
axis_2 = v3 - v1 #difference of mean vector of cluster 3 and cluster 1
# gram-schmidt Orthogonalization
data_axis1=c() #vector to store coefficients of data points on axis 1
data_axis2 = c()#vector to store coefficients of data points on axis 2
cluster_number= c()

for (i in (1:446))
{
  data_point = x[i,]

  axis1_coeff=dot(unlist(data_point),unlist(axis_1))/(dot(unlist(axis_1),unlist(axis_1))^0.5) #projection data point on axis 1
  axis2_coeff=dot(unlist(data_point),unlist(axis_2))/(dot(unlist(axis_2),unlist(axis_2))^0.5)#projection data point on axis 2
  # whole process of computing 2 coefficients
  data_axis1 = c(data_axis1,axis1_coeff)
  data_axis2 = c(data_axis2,axis2_coeff)
}
df = data.frame(axis1=data_axis1,axis2=data_axis2,cluster_number=test[["cluster"]]) #creating data frames of co-ordinates
print(df)                                                                           #and clusters

ggplot(data = df, mapping = aes(x = axis1, y = axis2)) + #plotting points
  geom_point(alpha = 10,aes(color = cluster_number),size=2,)+
  geom_text(label=df$cluster_number, nudge_x = 0.25, nudge_y = 0.25,   #assigning labels
            check_overlap = T)
pairs(df) #plotting pairs 
pairs(x)

test[["mu"]]




