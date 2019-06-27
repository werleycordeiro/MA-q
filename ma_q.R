ma_q = function(para,data,q){

mu = para[1]
theta = para[2:(q+1)]
sigma2 = para[(2+q)]
e = matrix(0,n,1)
n = dim(data)[1]

loglik = -.5 * n * log(2 * pi)

for(i in 1:dim(data)[1]){
	e[i+q] = data[i]-mu-(theta %*% e[(1+i-1):(q+i-1)])
	e2 = (e[i+q])^2 
	loglik = loglik - .5 * log(sigma2)-(e2/(2*sigma2))
	}
return(-loglik)
}
