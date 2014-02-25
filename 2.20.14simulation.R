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
	dist1<-mvrnorm(n,mu=mu[1:2],Sigma)
	dist2<-mvrnorm(n,mu=mu[3:4],Sigma)
	dist3<-mvrnorm(n,mu=mu[5:6],Sigma)
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
#'voter.pos': a matrix of voter preferences one gets from voters()
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


##Question 4

#visualization() returns a plot of the positions of the parties and the voters with indicators for affiliation.
#'party.pos': a matrix of positions of two parties
#'voters.mat': a matrix of voters' preferences and their affilations given by affiliation()
visualization <-function(party.pos,voters.mat){

#First, find the minimum and maximum ideological positions in order to use them as axes limits when plotting. Note that we have to find the minimum and maximum from the first two columns of 'voters.mat', because the third column includes letters (i.e. "Dem" and "Rep). We also have to convert 'voters.mat' into a numeric, because the values in the original matrix are characters.
mini <- min(as.numeric(voters.mat[,1:2]),party.pos)
maxi <- max(as.numeric(voters.mat[,1:2]),party.pos)

#Create an empty plot.	
plot(NULL,xlab="Dimension 1",ylab="Dimension 2",xlim=c(mini,maxi),ylim=c(mini,maxi),type="n")

#Plot the positions of the two parties. We'll assign the first row in the matrix as the Republican Party and the second row as the Democratic Party.
points(party.pos[1,1],party.pos[1,2],pch="R",col="red")
points(party.pos[2,1],party.pos[2,2],pch="D",col="blue")

#Plot the positions of the voters, and specify their affiliations with the colors red and blue.
points(voters.mat[,1],voters.mat[,2],col=ifelse(voters.mat[,3]=="Rep","red","blue"),pch=19)

#Give a title to the plot.
title("Positions of parties and voters")
}

#Now test if the function works correctly.
#Start by drawing voter preferences.
voters1 <- voters("mvnorm.mix",n=100,mu=c(-5,-4,7,8,1,2),Sigma=matrix(c(3,2,6,5),2,2),distnum=2)

#Decide upon random party positions.
parties1 <- matrix(c(-4,-5,8,9),2,2)

#Calculate the affiliations of the voters.
voters1.mat <- affiliation(voter.pos=voters1,party.pos=parties1)

#Check the visualization function. It works.
visualization(party.pos=parties1,voters.mat=voters1.mat)

#Try one more time using a different voter preference distribution.
voters2 <- voters("normal.with.var.option",n=200,sd=c(6,2))

#Decide upon random party positions.
parties2 <- matrix(rnorm(4,0,2),2,2)

#Calculate the affiliations of the voters.
voters2.mat <- affiliation(voter.pos=voters2,party.pos=parties2)

#Check the visualization function. It works!
visualization(party.pos=parties2,voters.mat=voters2.mat)

#A word of caution: Although the functions we created work correctly, the outputs can look odd depending on how odd the distribution of voter preferences and party positions are. So be careful to have reasonable and realistic voter preferences and party positions.


