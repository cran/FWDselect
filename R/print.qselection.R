print.qselection <-
function(x=object,...){
	object=x
	aux=cbind(object[[1]],object[[2]],as.character(object[[3]]))
	colnames(aux)=names(object)
	aux2=as.data.frame(aux)
	
	print(aux2)	
		
		}
