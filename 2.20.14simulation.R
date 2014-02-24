std.norm<-function(n){
  x<-rnorm(n)
  y<-rnorm(n)
  matrix<-cbind(x,y)
  return(matrix)
}

normal.with.var.option<-function(n, sd1, sd2){
  x<-rnorm(n, sd=sd1)
  y<-rnorm(n, sd=sd2)
  matrix<-cbind(x,y)
  return(matrix)
}

unif.voters<-function(n){
  x<-runif(n)
  y<-runif(n)
  matrix<-cbind(x,y)
  return(matrix)
}

mvnorm.voters<-function(n, mu1, mu2, sig){
  mat<-mvrnorm(n, mu=c(mu1, mu2), Sigma=sig)
  return(mat)
}

mvnorm.mix<-function(n, mu1, mu2, sig, distnum=1){
  dist1<-mvrnorm(n, mu=c(mu1[1], mu2[1]), Sigma=sig)
  dist2<-mvrnorm(n, mu=c(mu1[2], mu2[2]), Sigma=sig)
  dist3<-mvrnorm(n, mu=c(mu1[3], mu2[3]), Sigma=sig)
  if(distnum==1){
    return(dist1)
  }
  if(distnum==2){
    mat<-rbind(dist1, dist2)
    samp<-matrix(sample(mat, 2*n), ncol=2)
    return(samp)
  }
  if(distnum==3){
    mat<-rbind(dist1, dist2, dist3)
    samp<-matrix(sample(mat, 2*n), ncol=2)
    return(samp)
  }
}

trial<-matrix(c(1,2,3,4),nrow=2)

vote1<-mvnorm.mix(100,c(2,6,10),c(3,10, 25),sig=trial, distnum=2)
parties<-matrix(c(10,15,8,25), nrow=2)
parties
distances<-pdist(vote1, parties)
distance.mat<-matrix(distances@dist, ncol=2, byrow=TRUE)
party<-NULL
ifelse(distance.mat[,1]>distance.mat[,2], party[1]<-"red", party[1]<-"blue")

affiliation<-function(voters, parties){
  distances<-pdist(voters, parties)
  distance.mat<-matrix(distances@dist, ncol=2, byrow=TRUE)
  party<-NULL
  affl<-ifelse(distance.mat[,1]>distance.mat[,2], party[1]<-"red", party[1]<-"blue")
  vote<-cbind(voters,affl)
  colnames(vote)<-c("x","y","affiliation")
  return(vote)
}

affiliation(vote1, parties)
plot(vote1[,1],vote1[,2])

