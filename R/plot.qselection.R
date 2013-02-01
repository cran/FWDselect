plot.qselection=function (x = object, y = NULL, ...) 
{
    object = x
    x = object$q
    y = object[[2]]
    ylab_aux = names(object)[2]
    if (ylab_aux == "deviance") 
        ylab = "Deviance"
    if (ylab_aux == "variance") 
        ylab = "Residual variance"
    if (ylab_aux == "R2") 
        ylab = expression(R^2)
    plot(x, y, type = "o", bg = "black", pch = 21, xlab = "Subset size q", 
        ylab = ylab, xaxt = "n",...)
    axis(1, x,...)
}

