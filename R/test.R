test <-
function(x,y,method="lm",family="gaussian",nboot=50,max.r=5, fix.r=TRUE){

# Funcion que calcula el estadistico T	
#########################################
Tvalue=function(xy,ph0T=ph0,ph1T=ph1,optionT=method,fix.ph1T=fix.r,rT=max.r){
	x=xy[,2:ncol(xy)]
	y=xy[,1]
	var_res=NULL
	nvar=ncol(x)
	x=as.data.frame(x)
	if(fix.ph1T==FALSE){
	for (p in (ph0T+1):rT){
		var_res[p]=selection(x,y,q=p,method=optionT,family=family,seconds=FALSE,criterion="variance")$Information_Criterion }
	ph1<<-which.min(var_res) 
	varh1=var_res[ph1]
	#print(c(varh1,"varh1"))
	 }else{   
		varh1=selection(x,y,q=ph1,method=optionT,family=family,criterion="variance")$Information_Criterion
		}

	varh0=selection(x=x,y=y,q=ph0T,method=optionT,family=family,seconds=FALSE,criterion="variance")$Information_Criterion
	T=varh0/varh1 
	#print(T)
	}
##############################################

ph1=NULL
nvar=ncol(x)
n=length(y)
	
print("Processing IC bootstrap for H_0 ( 1 )...")

xydata=cbind(y,x)
T=c()
Q05=c()
Decision=c()
Hypothesis=c()
Tboot=numeric(nboot)
stopp=FALSE

for (ph0 in 1:(nvar-1)){	
if(ph0==1){T[ph0]=Tvalue(xy=xydata,ph0T=ph0,fix.ph1T=FALSE)}else{T[ph0]=Tvalue(xy=xydata,ph0T=ph0)}
if(ph0!=1) print(paste("Processing IC bootstrap for H_0 (",ph0,")..."),sep="")
	if(T[ph0]>1){
#bootstrap
	iboot=nboot
	yboot=array(data=NA,dim=c(n,nvar+1,iboot)) 
	for (icol in 1:iboot) { 
		aux=as.matrix(xydata[sample(n,n),])
		dim(aux)=NULL
		yboot[,,icol]=aux }
	Tboot=apply(yboot,3,Tvalue) 
	Q05[ph0]=quantile(Tboot,0.05)
	if(Q05[ph0]<=1){Decision[ph0]="Accepted"}else{Decision[ph0]="Rejected"}
	Hypothesis[ph0]=paste("H_0 (",ph0,")",sep="")
	Q05[ph0]=round(Q05[ph0],2)
	T[ph0]=round(T[ph0],2)
	
	if(Q05[ph0]<=1){break} }else{
		Decision[ph0]= "-" 
		Q05[ph0]="-"
		T[ph0]="-"
		Hypothesis[ph0]=paste("H_0 (",ph0,")",sep="")
		stopp=TRUE
		break}
	
} 

cat("\n*************************************\n")
m=cbind(Hypothesis=Hypothesis,Statistic=T,Q05=Q05,Decision=Decision)
if(stopp==TRUE){cat(paste("\n Note: q =",ph0,"is the subset size with smallest variance\n",sep=" "))
	cat("\n*************************************\n")}

return(as.data.frame(m))

}
