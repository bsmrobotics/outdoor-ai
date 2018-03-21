make_points = function(){
  ps = data.frame(matrix(nrow=0, ncol=2))
  for(i in c(0.33, 0.67, 1)){
    p = c(runif(1) * 2 * i - i)
    p = c(round(p,3), round(sqrt(i^2 - p^2),3))
    if(runif(1) > 0.5){
      p[2] = -p[2]
    }
    ps = rbind(ps, p)
  }
  names(ps) = c('x','y')
  return(ps)
}

plot_points = function(ps){
  ps = rbind(c(0,0), ps)
  plot(ps, xlim = c(-1,1), ylim = c(-1,1))
  loc = round(runif(1) * 2 - 1,3)
  loc = c(loc,round(runif(1) * 2 - 1,3))
  plot_robot(loc)
  for(p in 2:nrow(ps)){
    lines(segments(ps[p-1,1],ps[p-1,2],ps[p,1],ps[p,2]))
  }
  return(loc)
}

one_trial  = function(){
  ps = make_points()
  loc = plot_points(ps)
  dir = readline(prompt = "Direction?")
  line = c(loc[1],loc[2])
  for(i in 1:nrow(ps)){
    line = c(line,ps[i,1],ps[i,2])
  }
  line = c(line, dir)

  output = ""
  for(item in line){
    output = paste(output,item,sep=",")
  }
  return(output)
}

plot_robot = function(loc){
  segments(loc[1], loc[2], loc[1], loc[2] - 0.25, col = 'red')
  segments(loc[1], loc[2], loc[1], loc[2] + 0.25, col = 'red')
  segments(loc[1], loc[2], loc[1] - 0.25, loc[2], col = 'red')
  segments(loc[1], loc[2], loc[1] + 0.25, loc[2], col = 'red')
  segments(loc[1], loc[2], loc[1] - 0.1, loc[2] - 0.1, col = 'red')
  segments(loc[1], loc[2], loc[1] + 0.1, loc[2] + 0.1, col = 'red')
  segments(loc[1], loc[2], loc[1] + 0.1, loc[2] - 0.1, col = 'red')
  segments(loc[1], loc[2], loc[1] - 0.1, loc[2] + 0.1, col = 'red')
  points(loc[1], loc[2], col = 'red')
}

trials = function(n){
  driver = readline(prompt = "Last Name (All lower case please)")
  file = 'outdoor_training_data.csv'
  for(i in 1:n){
    line = one_trial()
    line = paste0(driver,line)
    write(line, file=file, append=TRUE)
  }
  print("Thanks!")
}
