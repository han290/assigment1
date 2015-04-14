

library(lattice)

library(ggplot2)


#1 Number of runner


histogram(dat$YEAR) 

numberM = subset(dat,GENDER == 'M', select = YEAR)

numberF = subset(dat, GENDER == 'F', select = YEAR)

par(mar = c(2.1,4.1,2.1,2.1))

par(mfcol = c(2,1))

hist(numberM$YEAR, main = 'Male attendence')

hist(numberF$YEAR, main = 'Female attendence')



#2 Official Time across year


par(mfcol = c(1,1))

boxplot(dat$TIME~dat$YEAR, main = 'Official Yime', xlab = 'Year', ylab = 'Time in seconds',
        outline = FALSE)


#3 distribution of age


densityplot(~ AG,data = dat, groups = dat$YEAR, 
            plot.point = FALSE, auto.key = list(space = 'right'))




#4 SIGN


pound = subset(dat, SIGN == '#', c(AG,TIME, NUM))

asterisk = subset(dat, SIGN =='*', c(AG, TIME, NUM))


histogram(asterisk$AG)

histogram(pound$AG)

qplot(SIGN, NUM, data = dat, geom = 'violin', fill = SIGN)





#5 TOT/DIV

tot = subset(dat, YEAR == 2009, select = c(AG, TOT, DIV, HOMETOWN))

tot$TOT = as.factor(tot$TOT)

tot$HOMETOWN = as.factor(tot$HOMETOWN)


qplot(TOT, AG, data = tot, geom = 'boxplot', fill = TOT)


#6 HOMETOWN the most number runner

t = table(dat$HOMETOWN)


d = sort(t,decreasing = TRUE)[1:10]


names(d)


#7 HOMETOWN run fast

runtime = sort(dat$TIME)[1:100]

l = match(runtime, dat$TIME)

ag = dat$AG[l]

par(mfcol = c(1,2))

hist(ag, main = 'top 100')

hist(dat$AG, main = 'whole population')

table(dat$HOMETOWN[l])





















