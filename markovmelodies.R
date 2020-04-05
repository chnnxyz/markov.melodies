randomWalk<-function(pUp,pDn,pSt){
  ##Generates a random walk matrix for 
  mat<-matrix(0,nrow=22,ncol=22)
  for(i in 1:22){
    mat[i,i]<-pSt
  }
  for(i in 1:21){
    mat[i,i+1]<-pUp
  }
  for(i in 2:22){
    mat[i,i-1]<-pDn
  }
  mat[1,2]<-pUp+pDn
  mat[22,21]<-pUp+pDn
  for(i in 1:22){
    mat[i,]<-mat[i,]/sum(mat[i,])
  }
  return(mat)
}

randomMatrix<-function(){
  mat<-matrix(runif(484,0,10),nrow=22,ncol=22)
  for(i in 1:22){
    mat[i,]<-mat[i,]/sum(mat[i,])
  }
  return(mat)
}

markovNormalize<-function(m){
  mat<-m
  for(i in 1:nrow(m)){
    mat[i,]<-mat[i,]/sum(mat[i,])
  }
}

markovMelodyRandomInit<-function(root="c",mode="ionian",sigTop=4,sigBot=4,stopAfterBars=32,noteLengthDist=rep(1,11)/11,transMatrix){
  ref<-NULL
  startref<-sample(1:22,size=1)
  switch(root,"c"={ref<-60},"c#"={ref<-61},"db"={ref<-61},"d"={ref<-62},
         "d#"={ref<-63},"eb"={ref<-63},"e"={ref<-64},"f"={ref<-65},
         "f#"={ref<-66},"gb"={ref<-66},"g"={ref<-67},"g#"={ref<-68},
         "ab"={ref<-68},"a"={ref<-69},"a#"={ref<-70},"bb"={ref<-70},
         "b"={ref<-71})
  refv<-NULL
  switch(mode,"ionian"={
    refv<-c(ref,ref+2,ref+4,ref+5,ref+7,ref+9,ref+11,ref+12,ref+14,ref+16,ref+17,ref+19,ref+21,ref+23,ref+24,ref+26,ref+28,ref+29,ref+31,ref+33,ref+35,ref+36)
  }, "major"={
    refv<-c(ref,ref+2,ref+4,ref+5,ref+7,ref+9,ref+11,ref+12,ref+14,ref+16,ref+17,ref+19,ref+21,ref+23,ref+24,ref+26,ref+28,ref+29,ref+31,ref+33,ref+35,ref+36)
  }, "dorian"={
    refv<-c(ref,ref+2,ref+3,ref+5,ref+7,ref+9,ref+10,ref+12,ref+14,ref+15,ref+17,ref+19,ref+21,ref+22,ref+24,ref+26,ref+27,ref+29,ref+31,ref+33,ref+34,ref+36)
  },"phrygian"={
    refv<-c(ref,ref+1,ref+3,ref+5,ref+7,ref+8,ref+10,ref+12,ref+13,ref+15,ref+17,ref+19,ref+20,ref+22,ref+24,ref+25,ref+27,ref+29,ref+31,ref+32,ref+34,ref+36)
  },"lydian"={
    refv<-c(ref,ref+2,ref+4,ref+6,ref+7,ref+9,ref+11,ref+12,ref+14,ref+16,ref+18,ref+19,ref+21,ref+23,ref+24,ref+26,ref+28,ref+30,ref+31,ref+33,ref+35,ref+36)
  },"mixolydian"={
    refv<-c(ref,ref+2,ref+4,ref+5,ref+7,ref+9,ref+10,ref+12,ref+14,ref+16,ref+17,ref+19,ref+21,ref+22,ref+24,ref+26,ref+28,ref+29,ref+31,ref+33,ref+34,ref+36)
  },"aeolian"={
    refv<-c(ref,ref+2,ref+3,ref+5,ref+7,ref+8,ref+10,ref+12,ref+14,ref+15,ref+17,ref+19,ref+20,ref+22,ref+24,ref+26,ref+27,ref+29,ref+31,ref+32,ref+34,ref+36)
  },"minor"={
    refv<-c(ref,ref+2,ref+3,ref+5,ref+7,ref+8,ref+10,ref+12,ref+14,ref+15,ref+17,ref+19,ref+20,ref+22,ref+24,ref+26,ref+27,ref+29,ref+31,ref+32,ref+34,ref+36)
  },"locrian"={
    refv<-c(ref,ref+1,ref+3,ref+5,ref+6,ref+8,ref+10,ref+12,ref+13,ref+15,ref+17,ref+18,ref+20,ref+22,ref+24,ref+25,ref+27,ref+29,ref+30,ref+32,ref+34,ref+36)
  })
  
  startref<-sample(1:22,size=1) ## Generates vector position for start note
  startnote<-refv[startref] ##Provides number for start note
  durvec<-c(1,3/4,1/2,3/8,1/4,3/16,1/8,3/32,1/16,3/64,1/32)*sigBot
  state<-NULL ##Defining state (matrix row) vector
  note<-NULL ##Defining Note vector (isomorphous to state)
  dur<-NULL ##Defining note duration vector
  state[1]<-startref
  note[1]<-startnote
  dur[1]<-sample(durvec,prob=noteLengthDist,size=1)
  i<-2
  while(sum(dur)<=stopAfterBars*sigTop){
    dur[i]<-sample(durvec,prob=noteLengthDist,size=1)
    state[i]<-sample(1:22,prob=transMatrix[state[i-1],],size=1)
    note[i]<-refv[state[i]]
    i<-i+1
  }
  df<-data.frame(state=state,note=note,duration=dur)
  result<-NULL
  result$root<-root
  result$mode<-mode
  result$sigTop<-sigTop
  result$sigBot<-sigBot
  result$bars<-stopAfterBars
  result$lengthDistribution<-noteLengthDist
  result$transitionMatrix<-transMatrix
  result$initialNote<-startnote
  result$melody<-df
  
  return(result)
}

melodyPlot<-function(x){
  df<-x$melody
  par(mfrow=c(2,2))
  hist(df$note,main="Note Distribution")
  plot(df$note,type="l",main="Note chain")
  plot(x=cumsum(df$duration),y=df$note,type="l",main="Time-weighted Note Chain")
  acf(df$note)
}

melodyToCSV<-function(x,filename="melody.csv"){
  df<-x$melody
  write.csv(df,filename)
}
