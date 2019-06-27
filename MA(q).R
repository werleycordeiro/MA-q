# Package

install.packages("quantmod")
library(quantmod)

# Data

getSymbols('PBR',return.class = 'xts',index.class = 'Date',from = "2019-01-01",to = Sys.Date(),periodicity = "daily",src='yahoo')
data = PBR$PBR.Close # Petróleo Brasileiro S.A. - Petrobras (PBR) - NYSE (USD)

data = diff(data)[-1]

q = 2

para = matrix(0.2,1,(2+q))

para[1] = mean(data)
para[(2+q)] = var(data) 

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

ma_q(para=para,data=data,q=q)

low = c(rep(-Inf,q+1),0)

otim = optim(par=para,fn=ma_q,data=data,q=q,method="L-BFGS-B",lower = low,control=list("trace"=1))

pars = expand.grid(orderq = q:1)
names = paste0("ma",pars$orderq)
colnames(otim$par) = c("intercept",names,"sigma^2")
otim$par

# compare with arma {stats}

arima(data,order=c(0,0,q))$coef # MA(q)
