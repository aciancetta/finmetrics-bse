
myplot <- function( dates , y , col='darkblue' , t='l' , lwd=2 , ylim=NULL , main=NULL ){
  if( is.null(main) ){ par( mar=c(2,2,0.1,0.1) ) }
  plot( dates , y , t=t , col=col , lwd=lwd , axes=F , xlab='' , ylab='' , xaxs="i" , ylim=ylim , main=main )
  xticks <- axis.Date(1, x=dates, at=seq(dates[1], dates[length(dates)], "year") , lwd=0, lwd.tick=1, tck=0.02)
  yticks <- axis(2 , lwd=0, lwd.tick=1, tck=0.02)
  axis.Date(3, x=dates, at=seq(dates[1], dates[length(dates)], "year"), lwd=0, lwd.tick=1, tck=0.02, lab=F)
  axis(4, lwd=0, lwd.tick=1, tck=0.02, lab=F)
  abline( h=yticks , lty=3 )
  abline( v=xticks , lty=3 )
  box()
}
