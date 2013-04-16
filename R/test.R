test <-
function(x,y,method="lm",family="gaussian",nboot=50,speedup=TRUE,unique=FALSE,num.h0=1){

# Statistics T	
#########################################
pred=c()
Tvalue=function(xy,qT=qh0,optionT=method,speed=speedup){
	x=xy[,2:ncol(xy)]
	y=xy[,1]
	var_res=NULL
	nvar=ncol(x)
	x=as.data.frame(x)
		aux=selection(x,y,q=qT,method=optionT,family=family,seconds=FALSE,criterion="deviance")
		pred<<-aux$Prediction
		sel_num=aux$Variable_number
		res=y-pred
		if(speed==TRUE){
		xno=x[,-sel_num]
		var_imp=selection(xno,y,q=1,method=optionT,family=family,seconds=FALSE,criterion="deviance")$Variable_number
		xres=x[,c(var_imp)]}else{xres=x[,-sel_num]}
		data_res=cbind(res,xres)
		data_res=as.data.frame(data_res)

		if(optionT=="lm"){
			pred1=lm(res~.,data=data_res) }
		if(optionT=="glm"){
			pred1=glm(res~.,data=data_res,family=family) }
		if(optionT=="gam"){
			xnam <- paste("s(x[,", 1:(length(data_res)-1),"])",sep="")
			fmla <- as.formula(paste("res ~ ", paste(xnam, collapse= "+")))
			pred1=gam(fmla,family=family) }
			
		pred1=predict(pred1,type="response")
		T=sum(abs(pred1))						
	#print(T)
	}
##############################################

nvar=ncol(x);n=length(y)
xydata=cbind(y,x);pvalue=c()
Decision=c();Hypothesis=c();T=c()
ii=1
if(unique==FALSE){bucle=c(1:(nvar-1))}else{bucle=num.h0}
for (qh0 in bucle){
	print(paste("Processing IC bootstrap for H_0 (",qh0,")..."),sep="")		
	T[ii]=Tvalue(xy=xydata,qT=qh0)
	muhatg=pred 
	errg=y-muhatg
	err1=errg*(1-sqrt(5))/2;err2=errg*(1+sqrt(5))/2

	#Bootstrap
	yboot=array(data=NA,dim=c(n,nvar+1,nboot))
	for (iboot in 1:nboot) {
	yaux=rbinom(n, 1, prob=(5+sqrt(5))/10)
	yb=muhatg+(err1*yaux+err2*(1-yaux))
	aux=cbind(yb,xydata[,-1])
	aux=as.matrix(aux)
	dim(aux)=NULL
	yboot[,,iboot]=aux}

	Tboot=apply(yboot,3,Tvalue) 
	pvalue[ii]=sum(Tboot>=T[ii])/nboot

	if(pvalue[ii]>=0.05){Decision[ii]="Accepted"}else{Decision[ii]="Rejected"}
	Hypothesis[ii]=paste("H_0 (",qh0,")",sep="")
	T[ii]=round(T[ii],2)
	ii=ii+1
	if(Decision[ii-1]=="Accepted"){break}	}

m=cbind(Hypothesis=Hypothesis,Statistic=T,pvalue=pvalue,Decision=Decision)
cat("\n*************************************\n")
return(as.data.frame(m))

}




