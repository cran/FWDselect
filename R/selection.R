selection <-
function(x,y,q,criterion="deviance",method="lm",family="gaussian",seconds=FALSE,nmodels=1){
	
	if(missing(x)){stop("Argument \"x\" is missing, with no default")}
 	if(missing(y)){stop("Argument \"y\" is missing, with no default")}
 	if(missing(q)){stop("Argument \"q\" is missing, with no default")}

	nvar<-ncol(x)	
	inside<-integer(q)
	n=length(y)
	#if(q==nvar) {
     #   stop("The size of subset \"q\" is the same that the number of covariates")
   # }
	
	if(method=="lm"){model<-lm(y~NULL)} 
	if(method=="glm"&family=="binomial"){model<-glm(y~NULL,family="binomial")}
	if(method=="glm"&family=="poisson"){model<-glm(y~NULL,family="poisson")}
	if(method=="gam"){model<-gam(y~NULL)}
	out<-1:nvar 
	xyes=NULL
	for (k in 1:q){	
		aic=NULL
		for (j in out){
			if(method=="gam"){models<-update(model,.~.+ s(x[,j]))}else{
				models<-update(model,.~.+ x[,j])}
			aic<-c(aic,AIC(models))	}
		ii=which.min(aic) 
		inside[k]=out[ii] 
		out=out[-ii]
		if(method=="gam"){xnam=paste("s(x[,",inside[[k]],"])",sep="")}else{
			xnam=paste("x[,",inside[[k]],"]",sep="")}
		xyes[k]=xnam 
		model<-update(models,as.formula(paste(". ~ ", paste(xyes,collapse="+"))))
		bestaic=AIC(model)	}	
	stop<-integer(q)
	end=1
	if (q == 1 |q==nvar ) {end=0} 
	cont=0
	while (end != 0){ 
		for (f in 1:q){
			if(method=="gam"){xnam=paste("s(x[,",inside,"])",sep="")
			xnam[f]="s(x[,j])"; aic=NULL}else{
				xnam=paste("x[,",inside,"]",sep="")
			xnam[f]="x[,j]"; aic=NULL}
			for (j in out){
				model1<-update(model,as.formula(paste(". ~ ", paste(xnam,collapse="+"))))
				aic<-c(aic,AIC(model1))	}
				ii=which.min(aic) 
				if (aic[ii]>bestaic) {
					stop[f]=0 } else { 
					ii=which.min(aic) 
					oldinside=inside
					inside[f]=out[ii] 
					out[ii]=oldinside[f]
					if(method=="gam"){xin=paste("s(x[,",inside[f],"])",sep="")}else{
						xin=paste("x[,",inside[f],"]",sep="")}
					xnam[f]=xin
					model<-update(model,as.formula(paste(". ~ ", paste(xnam,collapse="+"))))
					bestaic=AIC(model)		
					stop[f]=1	}		}
				
		cont=cont+1 
		end=sum(stop) }
		
	#r2cv
	test=seq(1,n,2)
	Wtrainning=rep(1,n);Wtrainning[test]=0
	
	if(method=="lm"){
		formula=model$call$formula
		Mtrainning=lm(formula,weights=Wtrainning)
		pred=predict(lm(formula),type="response") }
		
	if(method=="glm"&family=="binomial"){
		formula=model$call$formula
		Mtrainning=glm(formula,family="binomial",weights=Wtrainning)
		pred=predict(glm(formula,family="binomial"),type="response")}
	
	if(method=="glm"&family=="poisson"){
		formula=model$call$formula
		Mtrainning=glm(formula,family="poisson",weights=Wtrainning)
		pred= predict(glm(formula,family="poisson"),type="response")}
		
	if(method=="gam"){
		formula=model$call$formula
		#formula=model$Best_model$formula
		Mtrainning=gam(formula,weights=Wtrainning)
		pred= predict(gam(formula),type="response")}
		
	muhat=predict(Mtrainning,type="response")
	
	var_res=sum( (y[test]-muhat[test])^2  ) /length(test)
	r2cv=1-( var_res/ ( var(y[test])*(length(test)-1)/length(test) )  )

	muhat_test=muhat[test]
	y_test=y[test]
	
	if(family=="gaussian") dev_cv=sum( (y_test-muhat_test)^2  )
	
	if(family=="binomial") {
		ii=muhat_test<0.0001
		muhat_test[ii]=0.0001
		ii=muhat_test>0.9999
		muhat_test[ii]=0.9999
		entrop=rep(0,length(test))
		ii=(1-y_test)*y_test>0
		entrop[ii]=2*y_test[ii]*log(y_test[ii])+(1-y_test[ii])*log(1-y_test[ii]) 
		entadd=2*y_test*log(muhat)+(1-y_test)*log(1-muhat_test)
		dev_cv=sum(entrop-entadd)
	}
										
	if(family=="poisson") {
		tempf=muhat_test
		ii=tempf<0.0001
		tempf[ii]=0.0001
		dev_cv=2*(-y_test * log(tempf) - (y_test-muhat_test )  )
		ii=y_test>0
		dev_cv[ii]=dev_cv[ii]+(2*y_test[ii]*log(y_test[ii]))
		dev_cv=sum(dev_cv)
	}
			
		
		
	names1=names(x[inside])
	
	if (criterion=="deviance"){res<-list(Best_model=model,
		Variable_names=names1,
		Variable_numbers=inside,
		Information_Criterion= dev_cv,
		ic=criterion,
		seconds=seconds,
		nmodels=nmodels,
		Prediction=pred)}
		#call=match.call())		}
		
	if (criterion=="R2") {res<-list(Best_model=model,
		Variable_names=names1,
		Variable_numbers=inside,
		Information_Criterion= r2cv,
		ic=criterion,
		seconds=seconds,
		nmodels=nmodels,
		Prediction=pred)}
		#call=match.call())		}
		
	if (criterion=="variance") {res<-list(Best_model=model,
		Variable_names=names1,
		Variable_numbers=inside,
		Information_Criterion= var_res,
		ic=criterion,
		seconds=seconds,
		nmodels=nmodels,
		Prediction=pred)}
		#call=match.call())		}
		
		

if (seconds==TRUE){
	bestaic1=bestaic
	bestaicn=0
	cont=-1
	fin=1
	for (h in 1:nmodels){
		cont=-1; fin=1
		while (fin != 0){
			fin=0	
			for (z in 1:q){	
				if(method=="gam"){xnam=paste("s(x[,",inside,"])",sep="")
				xnam[z]="s(x[,j])"; aic2=NULL}else{
					xnam=paste("x[,",inside,"]",sep="")
				xnam[z]="x[,j]"; aic2=NULL}
				vuelta=0
				for (j in out){
				vuelta=vuelta+1
				model1<-update(model,as.formula(paste(". ~ ", paste(xnam,collapse="+"))))
				aic2[vuelta]<-AIC(model1)}	
				if( (z==1)&(cont==-1) ){
					bestaic=100000000000
					oldinside=inside
					inside[z]=out[1]
					out[1]=oldinside[1]	}
				
				for (j in 1:length(out)){	
					if((z==1)&(cont==-1)&(j==1)){j=2}
					if(h==1){
						if(	(aic2[j]<bestaic)&(aic2[j]>bestaic1)){
							bestaic=aic2[j]
							oldinside=inside
							inside[z]=out[j]
							out[j]=oldinside[z]
							fin=1}		
						}else{
						if(	(aic2[j]<bestaic)&(aic2[j]>bestaicn) ){							
							bestaic=aic2[j]
							oldinside=inside
							inside[z]=out[j]
							out[j]=oldinside[z]
							fin=1	}
						}
				} 			
			} 
			cont=cont+1		
		} 
		if(method=="gam"){xin=paste("s(x[,",inside,"])",sep="")}else{
			xin=paste("x[,",inside,"]",sep="")}; xnam=xin
		model<-update(model,as.formula(paste(". ~ ", paste(xnam,collapse="+"))))
		names2=names(x[inside])
		bestaicn=AIC(model)
		
		# r2cv
		
	test=seq(1,n,2)
	Wtrainning=rep(1,n);Wtrainning[test]=0
	
	if(method=="lm"){
		formula=model$call$formula
		Mtrainning=lm(formula,weights=Wtrainning) }
		
	if(method=="glm"&family=="binomial"){
		formula=model$formula
		Mtrainning=glm(formula,family="binomial",weights=Wtrainning) }
	
	if(method=="glm"&family=="poisson"){
		formula=model$formula
		Mtrainning=glm(formula,family="poisson",weights=Wtrainning)}
		
	if(method=="gam"){
		formula=model$formula
		Mtrainning=gam(formula,weights=Wtrainning) }
		
	muhat=predict(Mtrainning,type="response")
	
	var_res=sum( (y[test]-muhat[test])^2  ) /length(test)
	r2cv=1-( var_res/ ( var(y[test])*(length(test)-1)/length(test) )  )

	muhat_test=muhat[test]
	y_test=y[test]
	
	if(family=="gaussian") dev_cv=sum( (y_test-muhat_test)^2  )
	
	if(family=="binomial") {
		ii=muhat_test<0.0001
		muhat_test[ii]=0.0001
		ii=muhat_test>0.9999
		muhat_test[ii]=0.9999
		entrop=rep(0,length(test))
		ii=(1-y_test)*y_test>0
		entrop[ii]=2*y_test[ii]*log(y_test[ii])+(1-y_test[ii])*log(1-y_test[ii]) 
		entadd=2*y_test*log(muhat)+(1-y_test)*log(1-muhat_test)
		dev_cv=sum(entrop-entadd)
	}
										
	if(family=="poisson") {
		tempf=muhat_test
		ii=tempf<0.0001
		tempf[ii]=0.0001
		dev_cv=2*(-y_test * log(tempf) - (y_test-muhat_test )  )
		ii=y_test>0
		dev_cv[ii]=dev_cv[ii]+(2*y_test[ii]*log(y_test[ii]))
		dev_cv=sum(dev_cv)
	}

	
	
	if (criterion=="deviance"){res2<-list(Alternative_model=model,
		Variable_names=names2,
		Variable_numbers=inside,
		Information_Criterion= dev_cv,
		ic=criterion)}
		
	if (criterion=="R2") {res2<-list(Alternative_model=model,
		Variable_names=names2,
		Variable_numbers=inside,
		Information_Criterion= r2cv,
		ic=criterion)}
		
	if (criterion=="variance") {res2<-list(Alternative_model=model,
		Variable_names=names2,
		Variable_numbers=inside,
		Information_Criterion= var_res,
		ic=criterion)}
	
		
	res=c(res,res2)		
	}
} 
class(res) <- "selection"
return(res)

}

