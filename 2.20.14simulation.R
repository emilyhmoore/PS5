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
#mini <- min(as.numeric(voters.mat[,1:2]),party.pos)
#maxi <- max(as.numeric(voters.mat[,1:2]),party.pos)

#Create a plot of the voter positions	
plot(x=voters.mat[,1],y=voters.mat[,2],xlab="Dimension 1",ylab="Dimension 2",
     col=ifelse(voters.mat[,3]=="Rep","red","blue"),pch=19
     )

#Plot the positions of the two parties. We'll assign the first row in the matrix as the Republican Party and the second row as the Democratic Party.
points(party.pos[1,1],party.pos[1,2],pch="R",col="red4")
points(party.pos[2,1],party.pos[2,2],pch="D",col="blue4")


#Give a title to the plot.
title("Positions of parties and voters")
}

#Now test if the function works correctly.
#Start by drawing voter preferences.
voters1 <- voters("mvnorm.mix",n=100,mu=c(-5,-4,7,8,1,2),Sigma=matrix(c(3,2,6,5),2,2),distnum=2)

#Decide upon random party positions.
party.pos<- matrix(c(-4,-5,8,9),2,2)

#Calculate the affiliations of the voters.
voters.mat <- affiliation(voter.pos=voters1,party.pos=parties1)

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

#A word of caution: Although the functions we created work correctly, 
#the outputs can look odd depending on how odd the distribution of 
#voter preferences and party positions are. So be careful to have 
#reasonable and realistic voter preferences and party positions.


##Function for getting things moving 2

#Load the corpcor package, which allows us to automatically 
#convert a matrix to a positive definite matrix that can be 
#used as the variance-covariance matrix in our function arguments.
library(corpcor)
 
##function for producing relocating parties.
##setting mean and sd as options passed down to give users more control.
#Specify the number of iterations as well.
party.relocation<-function(mean=0, sd=5,iter=5, method="std.norm"){
	
#Select a random number of voters
	n <- sample(100:1000,1)

#According to the voter distribution type chosen, run the voters function appropriately to get voter preferences.
	if(method=="std.norm"){
		voters <- voters(method,n)
	}
	if(method=="normal.with.var.option"){
		sd <- c(sample(1:5,1),sample(1:5,1))
		voters <- voters(method,n,sd)
	}
	if(method=="unif.voters"){
		voters <- voters(method,n)
	}
	if(method=="mvnorm.voters"){
		mu <- c(sample(-5:5,1),sample(-5:5,1))
		Sigma <- make.positive.definite(matrix(sample(1:7,4),2,2))
		voters <- voters(method,n,mu=mu,Sigma=Sigma)
	}
	if(method=="mvnorm.mix"){
		mu <- replicate(6,sample(-5:5,1))
		Sigma <- make.positive.definite(matrix(sample(1:7,4),2,2))
		distnum <- sample(1:3,1)
    voters <- voters(method,n,mu=mu,Sigma=Sigma,distnum=distnum)
	}

 ##randomly calculates initial party position.
  	party.pos<- matrix(rnorm(4,mean=mean,sd=sd),2,2)
  	
#The for loop iterates the simulation the designated amount of times.
	for(i in 1:iter){
  	
  ##calculates affiliation using function above
  affl<-affiliation(voter.pos=voters,party.pos=party.pos)
  
  ##setting graphing parameters to view two at a time to see before and after
   par(mfrow=c(1,2))
   
  ##visualize initial scenario at time t
   visualization(party.pos=party.pos,voters.mat=affl)

   ##create an empty matrix to fill with new party position
  party.pos<-matrix(rep(0,4),2,2)
  
  ##make dataframe for ease of calling later
  affl1<-as.data.frame(affl, stringsAsFactors=FALSE)
  
  ##figure out which observations are dems
  which.dems<-which(affl1$affiliation=="Dem")
  
  ##make a matrix of dems and one of reps
  dems<-affl1[which.dems,]
  reps<-affl1[-which.dems,]
  
  ##Fill in new party position, which is mean x and mean y
  party.pos[2,]<-c(mean(as.numeric(dems$x)), 
                 mean(as.numeric(dems$y)))
  party.pos[1,]<-c(mean(as.numeric(reps$x)), 
                 mean(as.numeric(reps$y)))
                 
  ##Look at new situation at time t+1 with parties located in center of points
visualization(party.pos=party.pos,voters.mat=affl)
}
}

##Try the function.
party.relocation(iter=15)


