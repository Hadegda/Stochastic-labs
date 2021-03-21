setwd("C:/Users/TOSHIBA/Documents/Stochastic_labs/lab2_Fisher") #path to folder with data file

#get data
nFrames = 800
frameSize = 1024
data = scan("wave_ampl.txt", what = double(10), nmax = frameSize*nFrames, sep = ",", dec = ".")

drawInterval = function(interval, data, color) {
  lines(interval[1]:interval[2], data[interval[1]:interval[2]], col = color, lwd = 2)
}

#Find intervals where transition between signal/background and transition should exists
findIntervals = function(y, frameSize) {
  h = hist(y, plot=TRUE)
  m1 = h$breaks[length(h$breaks) / 2]
  m2 = h$breaks[length(h$breaks) / 2 + 1]
  transitions = na.omit(y[y>m1 & y<m2])
  if(m1 > m2) transitions = na.omit(y[y>m1 & y<m2])
  i1 = which(y == transitions[1])
  i2 = which(y == transitions[length(transitions)])
  
  return (list(c(i1, 1), c(i1, i2), c(i2, i1), c(i2, frameSize)))
}

#draw signal divided on background/transition/data + count Fisher statistics
researchFrame = function(data, iFrame, frameSize) {
  offset = frameSize * (iFrame - 1)
  frameData = data[(offset+1):(offset+frameSize)]
  
  y = filter(frameData, rep(1, 5), method = 'convolution', sides = 1)
  
  intervals = do.call(findIntervals, list(y, frameSize))
  
  #find indexes of boundaries between background, transition and data
  iBoundaries = c(1, 0, 0, 0, 0, frameSize)
  for(k in 1:4) {
    interval = intervals[[k]]
    for(j in interval[1]:interval[2]) {
      if ((y[j + 1] - y[j])*(y[j] - y[j-1]) < 0) {
        iBoundaries[k + 1] = j #find slope changing
        break
      }
    }
  }
  
  plot(y, type='l')
  do.call(drawInterval, list(c(iBoundaries[1], iBoundaries[2]), y, 'red'))  #left background
  do.call(drawInterval, list(c(iBoundaries[3], iBoundaries[4]), y, 'green')) #data
  do.call(drawInterval, list(c(iBoundaries[5], iBoundaries[6]), y, 'red'))  #right background
  
  FBackground = var.test(y[iBoundaries[1]:iBoundaries[2]], y[iBoundaries[5]:iBoundaries[6]])
  FData = var.test(y[iBoundaries[3]:iBoundaries[4]], append(y[iBoundaries[1]:iBoundaries[2]], y[iBoundaries[5]:iBoundaries[6]]))
  
  return (list(FData))
}

FforFrame = do.call(researchFrame, list(data, 512, frameSize))
FforFrame
