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
parties1<- matrix(c(-4,-5,8,9),2,2)

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

#A word of caution: Although the functions we created work correctly, 
#the outputs can look odd depending on how odd the distribution of 
#voter preferences and party positions are. So be careful to have 
#reasonable and realistic voter preferences and party positions.

#Load the corpcor package, which allows us to automatically 
#convert a matrix to a positive definite matrix that can be 
#used as the variance-covariance matrix in our function arguments.
library(corpcor)
 
##function for producing relocating parties.
##setting mean and sd as options passed down to give users more control.
#Specify the number of iterations as well.

#n is the number of voters
#mean is the mean of the party position draw
#sd is the sd of the party position draw. If too high, results are weird.
#iter is the number of iterations
#method is the distribution type of voters
#seed specifies the seed you want for the random parts. 

party.relocation<-function(n=500, mean=0, sd=2,iter=5, method="std.norm", seed=sample(1:10000, 1)){
  
  #Set the random seed based on the seed specified
  set.seed(seed)

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

#Store initial party position in party.vector, which will be expanded throughout the simulation.
    party.vector <- as.vector(party.pos)
  	
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

#Append the changed party position to party.vector, which is the previous party position.
  party.vector <- c(party.vector,party.pos)
  
#Plot time t+1.  
visualization(party.pos=party.pos,voters.mat=affl)
  
}

#Convert party.vector into a matrix.
party.mat<-matrix(party.vector, ncol=4, byrow=TRUE)

#Rearrange the columns appropriately.
party.mat<-cbind(party.mat[,1], party.mat[,3], party.mat[,2], party.mat[,4])
colnames(party.mat)<-c("rep.x", "rep.y", "dem.x", "dem.y")

return(party.mat)
}

#Try the function.
party.relocation(iter=10)


####Explore your model####

##Question 3

#Install packages for parallel
library(foreach)
library(doMC)
library(multicore)
library(plyr)

#Set the parallel environment.
registerDoMC(cores=2)

#parameter1: mean of party position in the party.relocation() function
#parameter2: sd of party position in the party.relocation() function
parameter1 <- seq(-1,1,length.out=3)
parameter2 <- seq(1,3,length.out=3)

#Create an expanded grid of the combinations of the two parameters.
parameters.df <- expand.grid(mean=parameter1,sd=parameter2)

#Run party.relocation() throughout the parameter space.
result <- NULL
for(i in 1:nrow(parameters.df)){
result <- rbind(result,print(party.relocation(mean=parameters.df[i,1],sd=parameters.df[i,2])))
}

#Check the positions of parties throughout the simulation.
result

##Question 4

#Our comparative static of interest is party positions of the Republican Party and the Democratic Party in the first dimension.
#The input of interest is the mean of party positions we set for the previous problem. They are -1, 0, and 1.
party.mean <- matrix(c(rep(-1,18),rep(0,18),rep(1,18)),ncol=3)

#Extract the Republican Party's position in the first dimension.
Rep.x <- result[,1]

#Extract the positions of the Republican Party according to the different means of party positions.
first.rep.mean <- Rep.x[seq(1,54,3)]
second.rep.mean <- Rep.x[seq(2,54,3)]
third.rep.mean <- Rep.x[seq(3,54,3)]

#Repeat the process for the Democratic Party.
Dem.x <- result[,3]
first.dem.mean <- Dem.x[seq(1,54,3)]
second.dem.mean <- Dem.x[seq(2,54,3)]
third.dem.mean <- Dem.x[seq(3,54,3)]

#Plot with the x-axis as the mean party position, and the y-axis as the simulated party positions.
plot(NULL,xlab="mean party position",ylab="party positions throughout the simulation",xlim=c(-2,2),ylim=c(-3,3))
points(party.mean[,1],first.rep.mean,col="red")
points(party.mean[,2],second.rep.mean,col="red")
points(party.mean[,3],third.rep.mean,col="red")
points(party.mean[,1],first.dem.mean,col="blue")
points(party.mean[,2],second.dem.mean,col="blue")
points(party.mean[,3],third.dem.mean,col="blue")
title("How party positions change depending on \n how the mean party position is initially set")

#We can see from the plot that the spread of party positions narrows throughout the course of the simulation.


####Expand your model####

##Question 1

#We will alter the function so that it can take three parties as an option.
affiliation.party.number.option<-function(voter.pos,party.pos,party.number=3){

#pdist() gives a vector containing the distance between each voter and each party.  
distances<-pdist(voter.pos, party.pos)

#Turn the vector of distances into a matrix.
distance.mat<-matrix(distances@dist, ncol=party.number, byrow=TRUE)

#Create a NULL object.
affl<-NULL

#Assign "Rep", "Dem", or "Green" according to where the minimum distance is.
affl <- NULL
for(i in 1:nrow(distance.mat)){
affl <- c(affl,if(min(distance.mat[i,])==distance.mat[i,1]){"Rep"
		}else{
if(min(distance.mat[i,])==distance.mat[i,2]){
	"Dem"
}else{"Green"}
		}
)}

#Column bind the matrix of voter preferences and the vector of affiliations.
voters.mat<-cbind(voter.pos,affl)

#Assign appropriate column names.
colnames(voters.mat)<-c("x","y","affiliation")

#Return voters.mat.
return(voters.mat)
}

#Now alter the visualization function to take in party number option.
visualization.party.number.option <-function(party.pos,voters.mat){

#Create a plot of the voter positions	
plot(x=voters.mat[,1],y=voters.mat[,2],xlab="Dimension 1",ylab="Dimension 2",
     col=ifelse(voters.mat[,3]=="Rep","red",ifelse(voters.mat[,3]=="Dem","blue","green")))

#Plot the positions of the three parties. We'll assign the first row in the matrix as the Republican Party, the second row as the Democratic Party, and the third row as the Green Party.
points(party.pos[1,1],party.pos[1,2],pch="R",col="red4")
points(party.pos[2,1],party.pos[2,2],pch="D",col="blue4")
points(party.pos[3,1],party.pos[3,2],pch="G",col="green4")

#Give a title to the plot.
title("Positions of parties and voters")
}

#This function simulates position changes for three parties.
party.relocation.party.number.option<-function(n=500, mean=0, sd=2,iter=5, method="std.norm", seed=sample(1:10000, 1),party.number=3){
  
  #Set the random seed based on the seed specified
  set.seed(seed)

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
if(party.number==3){
 ##randomly calculates initial party position.
  	party.pos<- matrix(rnorm(party.number*2,mean=mean,sd=sd),party.number,2)

#Store initial party position in party.vector, which will be expanded throughout the simulation.
    party.vector <- as.vector(party.pos)
  	
#The for loop iterates the simulation the designated amount of times.
	for(i in 1:iter){
  	
  ##calculates affiliation using function above
  affl<-affiliation.party.number.option(voter.pos=voters,party.pos=party.pos)
  
  ##setting graphing parameters to view two at a time to see before and after
   par(mfrow=c(1,2))
   
  ##visualize initial scenario at time t
visualization.party.number.option(party.pos=party.pos,voters.mat=affl)

   ##create an empty matrix to fill with new party position
  party.pos<-matrix(rep(0,6),3,2)
  
  ##make dataframe for ease of calling later
  affl1<-as.data.frame(affl, stringsAsFactors=FALSE)
  
  ##figure out which observations are dems, reps, and greens
  which.dems<-which(affl1$affiliation=="Dem")
  which.reps<-which(affl1$affiliation=="Rep")
  which.greens<-which(affl1$affiliation=="Green")
  
  ##make a matrix of dems, reps, and greens.
  dems<-affl1[which.dems,]
  reps<-affl1[which.reps,]
  greens<-affl1[which.greens,]
  
  ##Fill in new party position, which is mean x and mean y
  party.pos[3,]<-c(mean(as.numeric(greens$x)), 
                 mean(as.numeric(greens$y)))
  party.pos[2,]<-c(mean(as.numeric(dems$x)), 
                 mean(as.numeric(dems$y)))
  party.pos[1,]<-c(mean(as.numeric(reps$x)), 
                 mean(as.numeric(reps$y)))

#Append the changed party position to party.vector, which is the previous party position.
  party.vector <- c(party.vector,party.pos)
  
#Plot time t+1.  
visualization.party.number.option(party.pos=party.pos,voters.mat=affl)
  
}

#Convert party.vector into a matrix.
party.mat<-matrix(party.vector, ncol=6, byrow=TRUE)

#Rearrange the columns appropriately.
party.mat<-cbind(party.mat[,1], party.mat[,3], 
                 party.mat[,2], party.mat[,4],
                 party.mat[,5],party.mat[,6])
colnames(party.mat)<-c("rep.x", "rep.y", "dem.x", "dem.y","green.x","green.y")

return(party.mat)
} else {
  ##randomly calculates initial party position.
  party.pos<- matrix(rnorm(4,mean=mean,sd=sd),2,2)
  
  #Store initial party position in party.vector, which will be expanded throughout the simulation.
  party.vector <- as.vector(party.pos)
  
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
    
    #Append the changed party position to party.vector, which is the previous party position.
    party.vector <- c(party.vector,party.pos)
    
    #Plot time t+1.  
    visualization(party.pos=party.pos,voters.mat=affl)
    
  }
  
  #Convert party.vector into a matrix.
  party.mat<-matrix(party.vector, ncol=4, byrow=TRUE)
  
  #Rearrange the columns appropriately.
  party.mat<-cbind(party.mat[,1], party.mat[,3], party.mat[,2], party.mat[,4])
  colnames(party.mat)<-c("rep.x", "rep.y", "dem.x", "dem.y")
  
  return(party.mat)
}
}


#Try run the function.
party.relocation.party.number.option(n=500, mean=0, sd=2,iter=10, method="std.norm", seed=sample(1:10000, 1),party.number=3)
party.relocation.party.number.option(party.number=2)
