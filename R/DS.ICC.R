DS.ICC <- function(itemParameters, o = DS.options(), tit = NULL, leg = NULL)
{
 pcr = DS.PCR(itemParameters,Dscore = matrix(o$dScale, ncol=1), o)
 matplot(o$dScale,pcr,type = "l", lty=1:nrow(itemParameters), lwd=2, col=1, xlab = "D-scale", ylab = "PCR")
 title(tit)
 legend("bottomright",legend = leg, lty = 1:nrow(itemParameters))
}