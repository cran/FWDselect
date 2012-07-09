print.selection <-
function(x=model,...){ #  print.frfast2(model) es igual escribir model
		model<-x
	
		
		#cat("\nTotal Number of Nucleotides ")
		#cat(format(sum(model$nat,model$ngc)))
		#cat("\n")
		cat("\n")
		cat("****************************************************")
		
		cat("\nBest subset of size q =",length(model$Variable_numbers),": ")
		cat(format(model$Variable_names))
		cat("\n")
		
		cat("\nInformation Criterion Value -", model$ic,": ")
		cat(format(model$Information_Criterion))
		cat("\n")
		
		cat("****************************************************")
		cat("\n")
	if(model$seconds==T){
		
		cont=0
		for (i in 1:model$nmodels){
			
		if(i==1){cont=cont+10}else{cont=cont+5}	
		cat("\nAternative (",i,") subset of size q =",length(model$Variable_numbers),": ")
		cat(format(model[cont]))
		
		cat("\n")
		cat("\nInformation Criterion Value -", model$ic,": ")
		cat(format(model[cont+2]))
		cat("\n")
		cat("\n")
	
		}
		
	}
				
		#UseMethod("print")
		
		}
