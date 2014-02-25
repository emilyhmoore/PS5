##Problem Set 5
##Emily Moore & Jae Hee Jung


####Simulation set up####

##Questions 1&2

#Load the relevant packages.
library(MASS)
library(pdist)

#voters() creates a matrix of voters' preferences.
#'method': preference distribution type
#'n': number of voters
#'sd': vector of standard deviations
#'mu': vector of means
#'Sigma': variance-covariance matrix
#'distnum': number of multivariate normal distributions to be used
voters <- function(method,n,sd=NULL,mu=NULL,Sigma=NULL,distnum=NULL){

#std.norm() creates standard normal distributed voter preferences.
std.norm<-function(n){
	x<-rnorm(n)
	y<-rnorm(n)
	mat<-cbind(x,y)
	return(mat)
}

#normal.with.var.option() creates normally distributed voter preferences with variance option.
normal.with.var.option<-function(n,sd){
	x<-rnorm(n,sd=sd[1])
	y<-rnorm(n,sd=sd[2])
	mat<-cbind(x,y)
	return(mat)
}

#unif.voters() creates uniformly distributed voter preferences.
unif.voters<-function(n){
	x<-runif(n)
	y<-runif(n)
	mat<-cbind(x,y)
	return(mat)
}

#mvnorm.voters() creates multivariate normal distributed voter preferences.
mvnorm.voters<-function(n,mu,Sigma){
	mat<-mvrnorm(n,mu,Sigma)
	return(mat)
}

#mvnorm.mix() creates voter preferences from up to three multivariate normal distributions.
mvnorm.mix<-function(n,mu,Sigma,distnum){
	dist1<-mvrnorm(n,mu=mu[1,2],Sigma)
	dist2<-mvrnorm(n,mu=mu[3,4],Sigma)
	dist3<-mvrnorm(n,mu=mu[5,6],Sigma)
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

#If the 'method' argument is "std.norm", run std.norm().
if(method=="std.norm"){
	return(std.norm(n))
}

#If the 'method' argument is "normal.with.var.option", run normal.with.var.option().
if(method=="normal.with.var.option"){
	return(normal.with.var.option(n,sd))
}

#If the 'method' argument is "unif.voters", run unif.voters().
if(method=="unif.voters"){
	return(unif.voters(n))
}

#If the 'method' argument is "mvnorm.voters", run mvnorm.voters().
if(method=="mvnorm.voters"){
	return(mvnorm.voters(n,mu,Sigma))
}

#If the 'method' argument is "mvnorm.mix", run mvnorm.mix().
if(method=="mvnorm.mix"){
	return(mvnorm.mix(n,mu,Sigma,distnum))
}
}


##Question 3

#affiliation() gives a vector of party affiliations of each voter based on ideological proximity.
#'voter.pos': a matrix of voter preferences one gets by assigning the output of voters() to the object 'voter'
#'party.pos': a matrix of positions of two parties 
affiliation<-function(voter.pos,party.pos){

#pdist() gives a vector containing the distance between each voter and each party.  
distances<-pdist(voter.pos, party.pos)

#Turn the vector of distances into a matrix.
distance.mat<-matrix(distances@dist, ncol=2, byrow=TRUE)

#Create a NULL object.
affl<-NULL

#If the value in the first column in distance.mat is smaller than the value in the second column, assign "Rep", if not assign "Dem".
affl<-ifelse(distance.mat[,1]<distance.mat[,2],"Rep","Dem")

#Column bind the matrix of voter preferences and the vector of affiliations.
voters.mat<-cbind(voter.pos,affl)

#Assign appropriate column names.
colnames(voters.mat)<-c("x","y","affiliation")

#Return voters.mat.
return(voters.mat)
}

