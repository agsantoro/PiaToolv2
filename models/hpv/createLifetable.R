createLifetable = function(country) {
  age = 0:100
  qx = as.numeric(mortall[mortall$`Country ¦ Age [12]`== country,-1])
  px = 1 - qx
  lx = c()
  for (i in 1:101) {
    if (i==1) {lx=1} else {
      lx[i]=lx[i-1]*px[i-1]
    }
  }
  
  Lx = c()
  for (i in 1:101) {
    if (i %in% 1:100) {Lx[i]=(lx[i]+lx[i+1])/2} else {
      Lx[i]=lx[i]/2
    }
  }
  
  Tx = c()
  for (i in 1:101) {
    Tx[i] = sum(Lx[i:length(Lx)])
  }
  
  Ex = Tx/lx

  return(data.frame(
    age,
    qx,
    px,
    lx,
    Lx,
    Tx,
    Ex
  ))
  
}
