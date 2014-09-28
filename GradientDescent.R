h<-function(X, theta){
	sum=0
	for(i in 1:length(X)){
		sum=sum+X[1, i]*theta[i]
	}
	#print("Finished h\n")
	return(sum)
}

sd<-function(calc_val, orig_val){
	
	return((calc_val-orig_val)*(calc_val-orig_val))
}

J<-function(X, theta, y){
	
	diff=0
	
	for(i in 1:length(y)){
		diff=diff+sd(h(X[i,], theta), y[i])
	}

	return(0.5*diff/length(y))
}

calctheta<-function(X, theta, y, j){
	
	diff=0
	
	for(i in 1:length(y)){
		diff=diff+(h(X[i,], theta)-y[i])*X[i, j]
	}
	#print(diff/length(y))
	return(diff/length(y))
}

minimizeJ<-function(X, theta, y, alpha=1, c=0.01, num_of_iteration=100){

	i=1
	temptheta=(1:length(theta))
	print(temptheta[length(theta)])
	Jval=(1:num_of_iteration)
	prev=as.integer(.Machine$integer.max)

	while(i<=num_of_iteration)
	{
		for(j in 1:length(theta))
		{
			temptheta[j]=theta[j]-alpha*(calctheta(X, theta, y, j))
			#print("Inside "+j+"\n")
		}

		for(j in 1:length(theta))
		{
			theta[j]=temptheta[j]
		}
		val=J(X, theta, y)
		Jval[i]=val
		prev=Jval[i]
		i=i+1
		#print(i)

	}

	return(list(theta, Jval))
}
	
	
 
	

	
	