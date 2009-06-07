`p12LIDA` <-function(object,time1,time2) {
	if (missing(object)) stop("Argument 'object' is missing with no default")
	if (inherits(object, "p3state")) mydata<-object$datafr
	if (inherits(object, "data.frame")) mydata<-object
	if (!inherits(object, "data.frame") & !inherits(object, "p3state")) stop("'object' must be of class 'p3state'")
	if (missing(time1)) time1<-0
	if (missing(time2)) stop("Argument 'time2' is missing with no default")
	if (time1<0 | time2<0 | time1>time2) stop("'time1' and 'time2' must be positive, and time1 < time2")
	
	p1<-which(mydata[,1]<=time2 & mydata[,1]>time1 & mydata[,4]>time2 & mydata[,5]==1)
	p2<-max(which(mydata[,1]<=time1))
	aux2<-mydata[p2,7]

if (aux2==0) restp12<-0
else restp12<-sum(mydata[p1,6])/(aux2)
return(restp12)
}


