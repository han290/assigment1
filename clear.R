
setwd('C:/Users/xu/Desktop/curriculum/STA242/Assignment1/data')

dirs = list.files(path ="C:/Users/xu/Desktop/curriculum/STA242/Assignment1/data", recursive = TRUE)

addheader = function(x){
  
  l = readLines(x, encoding = 'UTF-8')
  
  l = gsub(intToUtf8(0xA0),' ', l)
  
  s = which(grepl('^ {1,}[0-9]', l) == TRUE)[1]
  
  ll = readLines('men10Mile_2001', encoding = 'UTF-8')
  
  p = grep('={3}', ll)
  
  l[s-1] = ll[p]
  
  l[s-2] = ll[p-1]
  
  return(l)
  
} 




cleartitle = function(l){
  
  s = grep('={3}', l)# locate the line which has three sticking equal sign.
  
  l = l[-(1:(s-2))] #delete title part
  
  l[1] = toupper(l[1]) #convert header to upper case
  
  # the next two lines are to split sticking '=' between 'Hometown' and 'Net Tim' 
  
  #in 'men10Mile_2006' and 'Women10Mile_2006'
  
  e = regexpr('NET', l[1])
  
  if (e > 0){
    
    regmatches(l[2], e-1) = ' ==' 
    
    l[2] = gsub('   ','  ', l[2])
    
  }
  
  # the next seven lines are to make a '=' 
  
  # below each alfabet of the hearder, because in 'men10Mile_2003'and 'men10Mile_2003'
  
  # the spliting line is like move a space to left.
  
  m = gregexpr('[A-Z]', l[1]) #l[1] is header line
  
  n = regmatches(l[2], m) #l[2] is equal signs line; get the match under each alfabets
  
  t = unlist(n) ==' ' # see if the each match is a blank space
  
  y = which(t ==TRUE)# locate the blank spaces 
  
  y = y[1]# get the location of the first appearance
  
  regmatches(l[2],m)[[1]][y] = '= '#[l[2] is spliting line repalce the blank space 
  
  #with equal sign plus a white space
  
  # because I add a new white space in the spliting line
  
  #this will make the rest part move a space to right, which will make each
  
  #alfabet of header has a '=' under it.
  
  return(l)
}


clear = function(x){
  
  x = gsub('/', ' ', x)#replace '/' with blank space
  
  x = gsub('#', ' #', x)
  
  x = gsub('\\*', ' \\*', x)
  
  S = grepl('Under USATF', x)#locate the line which has 'UNDER USATF'
  
  if( any(S) ){
    
    m = which( S  == TRUE)
    
    x = x[-m]
    
    return(x)
  }
  
  else return(x)
  
}


wid = function(l){ 
  
  i = grep('={3}', l)
  
  m = regexpr('/', l[i+1])# The next two lines is to split the  '=' below the '/' of 'DIV/TOT' variable
  
  regmatches(l[i], m) = ' ' # replace '=' below '/' with blank space in order to split by space 
  
  w = grepl('[0-9]#|[0-9]\\*', l)
  
  if(any(w)) {
    
    s = regexpr('[#*]', l[w][1])
    
    regmatches(l[i],s) = '  '
    
    l[i] = gsub('   ', '  ', l[i])
    
  }
  
  s = l[i] # get the line with equal signs.
  
  s = strsplit(s, ' ')# split by space
  
  s = unlist(s)
  
  n = sapply(s, nchar)
  
  n = n+1
  
  return(n)
}


header = function(l){
  
  w = grepl('[0-9] #|[0-9] \\*',l) # check if the line has these signs. I want to creat a sign column. 
  
  s = regexpr('[#*]', l[w][1])# get the location of signs in the line they first appear. This time
  # I observe that all these signs have the same location on each line in one file.
  
  regmatches(l[1],s) = ' SIGN '# add ' sign' to header. The space before sign is for spliting.
  
  l = l[1]
  
  l = gsub('DIV {1,}TOT', 'DIV TOT', l) #  replace the matches with 'DIV TOT'
  
  l = gsub('GUN TIM', 'TIME', l)# one way to get rid of blank space and combine the name
  
  l = gsub('GUN', 'TIME', l)
  
  l = gsub('NET TIM', 'NET',l) 
  
  l = gsub('5 MILE', 'MI_5', l)
  
  l = gsub('5 MI', 'MI_5', l)
  
  l = gsub('10 KM', 'KM_10', l)
  
  p = gregexpr('PACE',l) 
  
  if (length(p[[1]]) > 1){
    
    regmatches(l,p) = list(c('PACE_5', 'PACE_10', 'PACE')) 
    
  }
  
  l = gsub(' {2,}', ' ', l)#replace two or more consecutive blank spaces with 1 blank space
  
  name = unlist(strsplit(l, ' '))
  
  return(name)
}



readData = function(x){
  
  l = readLines(x, encoding = 'UTF-8')
  
  l = gsub(intToUtf8(0xA0),' ', l)
  
  if (x == 'women10Mile_2001'){
    
    l = addheader(x) 
  }
  
  l = cleartitle(l)
  
  w = wid(l)
  
  l = clear(l)
  
  h = header(l)
  
  con = textConnection(l)
  
  f = read.fwf(con, widths = w, comment.char = '')
  
  close(con)
  
  f = f[-(1:2),]
  
  names(f) = h
  
  locate_year = regexpr('[0-9]{4}', x)
  
  y = regmatches(x, locate_year)
  
  y = as.numeric(y)
  
  f$YEAR = rep(y, nrow(f))
  
  if(grepl('^m', x)){
    
    f$GENDER = rep('M', nrow(f))
    
    f$GENDER = as.factor(f$GENDER)
    
  }
  
  else {
    
    f$GENDER = rep('F', nrow(f))
    
    f$GENDER = as.factor(f$GENDER)
  }
  
  
  if (any (grepl('PLACE', h))) {
    
    f$PLACE = as.numeric(f$PLACE)
    
  }
  
  if(any (grepl ('DIV', h))) {
    
    f$DIV = as.numeric(f$DIV)
    
  }
  
  if(any(grepl('TOT', h))){
    
    
    f$TOT = as.numeric(f$TOT)
  }
  
  
  if(any(grepl('NAME',h))) {
    
    f$NAME = as.character(f$NAME)
    
  }
  
  if(any(grepl('AG', h))) {
    
    f$AG = as.numeric(f$AG)
  }
  
  
  if(any(grepl('HOMETOWN', h))) {
    
    f$HOMETOWN = as.character(f$HOMETOWN)
  }
  
  if(any(grepl('NUM', h))) {
    
    f$NUM = as.numeric(f$NUM)
  }
  
  if(any(grepl('S$', h))) {
    
    f$S = as.character(f$S)
    
  }
  
  if(any(grepl('SIGN',h ))){
    
    f$SIGN = as.character(f$SIGN)
    
  }
  
  
  n = dim(f)[1]
  
  if(any(grepl('NET', h))){
    
    net = as.character(f$NET)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist( strsplit(net[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)
    })
    
    f$NET = tt
    
  }
  
  
  if ( any( grepl( 'TIME', h ))){
    
    tim = as.character(f$TIME)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist(strsplit(tim[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$TIME = tt
  }
  
  if(any(grepl('PACE', h))){
    
    pace = as.character(f$PACE)
    
    tt = sapply(c(1:n), function(x){
      
      time = unlist(strsplit(pace[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$PACE = tt
  }
  
  if (any(grepl('SPLIT', h))){
    
    spl = as.character(f$SPLIT)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist(strsplit(spl[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$SPLIT = tt
  }
  
  if (any(grepl('MI_5',h))){
    
    mi_5 = as.character(f$MI_5)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist(strsplit(mi_5[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$MI_5 = tt
  }
  
  if (any(grepl('KM_10', h))){
    
    km = as.character(f$KM_10)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist(strsplit(km[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$KM_10 = tt
  }
  
  if(any(grepl('PACE_5',h))){
    
    pa = as.character(f$PACE_5)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist(strsplit(pa[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$PACE_5 = tt
  }
  
  if(any(grepl('PACE_10',h))){
    
    pa_ten = as.character(f$PACE_10)
    
    tt =  sapply(c(1:n), function(x){
      
      time = unlist(strsplit(pa_ten[x], ':'))
      
      conversion = 60 ^ seq.int(length(time) - 1, 0)
      
      time = sum(conversion * as.integer(time))
      
      return(time)})
    
    f$PACE_10 = tt
  }
  
  
  return(f)
}



cherry = sapply(dirs, readData)





reunion = function(f){
  
  h = names(f)
  
  if (!any(grepl('PLACE', h))) {
    
    f$PLACE = NA
    
    f$PLACE = as.numeric(f$PLACE)
    
  }
  
  if (!any(grepl('DIV', h))) {
    
    f$DIV = NA
    
    f$DIV = as.numeric(f$DIV)
    
  }
  
  if (!any(grepl('TOT', h))) {
    
    f$TOT = NA
    
    f$TOT = as.numeric(f$TOT)
    
  }
  
  if (!any(grepl('NAME', h))) {
    
    f$NAME = NA
    
    f$NAME = as.character(f$NAME)
    
  }
  
  if (!any(grepl('NUM', h))) {
    
    f$NUM = NA
    
    f$NUM = as.numeric(f$NUM)
    
  }
  
  
  if (!any(grepl('AG', h))) {
    
    f$AG = NA
    
    f$AG = as.numeric(f$AG)
    
  }
  
  if (!any(grepl('HOMETOWN', h))) {
    
    f$HOMETOWN = NA
    
    f$HOMETOWN = as.character(f$HOMETOWN)
    
  }
  
  if (!any(grepl('TIME', h))) {
    
    f$TIME = NA
    
    f$TIME = as.numeric(f$TIME)
    
  }
  
  if (!any(grepl('NET', h))) {
    
    f$NET = NA
    
    f$NET = as.numeric(f$NET)
    
  }
  
  if (!any(grepl('PACE', h))) {
    
    f$PACE = NA
    
    f$PACE = as.numeric(f$PACE)
    
  }
  
  
  if (!any(grepl('MI_5', h))) {
    
    f$MI_5 = NA
    
    f$MI_5 = as.numeric(f$MI_5)
    
  }
  
  if (!any(grepl('KM_10', h))) {
    
    f$KM_10 = NA
    
    f$KM_10 = as.numeric(f$KM_10)
    
  }
  
  
  if (!any(grepl('PACE_5', h))) {
    
    f$PACE_5 = NA
    
    f$PACE_5 = as.numeric(f$PACE_5)
    
  }
  
  if (!any(grepl('PACE_10', h))) {
    
    f$PACE_10 = NA
    
    f$PACE_10 = as.numeric(f$PACE_10)
    
  }
  
  if (!any(grepl('SPLIT', h))) {
    
    f$SPLIT = NA
    
    f$SPLIT = as.numeric(f$SPLIT)
    
  }
  
  if (!any(grepl('SIGN', h))) {
    
    f$SIGN = NA
    
    f$SIGN = as.character(f$SIGN)
    
  }
  
  if (!any(grepl('$S', h))) {
    
    f$S = NA
    
    f$S = as.character(f$S)
    
  }
  
  
  str = c('PLACE', 'DIV', 'TOT', 'NUM', 'NAME','GENDER', 'AG','YEAR', 'HOMETOWN', 'TIME',
          
          'NET', 'PACE', 'SIGN', 'MI_5', 'KM_10', 'PACE_5', 'PACE_10', 'SPLIT', 'S')
  
  
  f[, names(f)] = f[, str]
  
  names(f) = str
  
  return(f)
}


cherryblossom = lapply(cherry, reunion)


dat = do.call(rbind, cherryblossom)

hometown = dat$HOMETOWN

dat$HOMETOWN = gsub(' {2,}', '', hometown)

name = dat$NAME

dat$NAME = gsub(' {2,}', '', name)


save(dat, file = 'C:/Users/xu/Desktop/cherryblossom.rda')





