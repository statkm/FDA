c_time <- c( 5, 20,  200, 370, 380 ,385, 390)
d_time <- length(c_time)
z <- 410
y <- -0.1
alpha <- 2
n <- alpha + 1 
dat <- matrix(0, nrow=d_time, ncol = d_time)

for(l in 1:d_time){
  for(k in 1:l){
    dat[k, l]<-sum( choose(alpha, 0:alpha)*(c_time[l]-c_time[k])^(alpha:0)*(z-c_time[l])^(alpha+0:alpha+1)/(alpha+0:alpha+1) )
    dat[l, k]<- dat[k, l]
  }
}


dat2 <- matrix(0, nrow=n, ncol = d_time)

for(l in 1:d_time){
  for(j in 1:n){
    dat2[j, l]<-sum( choose(j-1, 0:(j-1)) * (c_time[l])^((j-1):0)*(z-c_time[l])^(alpha+0:(j-1)+1)/(alpha+0:(j-1)+1) )
  }
}

dat3 <- t(dat2)

dat4 <- matrix(0, nrow=n, ncol = n)

for(k in 1:n){
  for(j in 1:k){
    dat4[j, k]<-(z^(j+k-2+1) - y^(j+k-2+1)) /(j+k-2+1)
    dat4[k, j]<- dat4[j, k]
  }
}

df_ip <- rbind(cbind(dat4, dat2), cbind(dat3, dat))

df_ip / sqrt( matrix(rep(diag(df_ip), n+d_time), byrow = TRUE, ncol=n+d_time) * matrix(rep(diag(df_ip), n+d_time), byrow = FALSE, ncol=n+d_time) )



write.csv(df_ip, "C:/Users/NRO8079/OneDrive - Takeda/_doc/work/2019FDA/df.csv", row.names = FALSE)
