#Discrete observations
x=c(100,190,210,160,150,160,190,200,170,152)
x
n=length(x)
am=mean(x)
am
lx=log(x)
lx
gm=10^(mean(lx))
gm
hm=1/am
hm
hm= 1/(mean(1/x))
hm
tx =table(x)
tx
m=which(tx==max(tx))
m
stx=sort(unique(x))
stx
mo=stx[m]
mo
me=median(x)
me
q1=quantile(x,0.25)
q1
d3=quantile(x,0.3)
d3
cat("mean=",am,"\n")
cat("geometric mean=",gm,"\n")
cat("harmonic mean=",hm,"\n")
cat("mode=",mo,"\n")
cat("median=",me,"\n")
cat("quartile1=",q1,"\n")
cat("decile3=",d3,"\n")

#ungrouped frequency distribution

x=1:5
x
f=c(7,11,9,8,3)
f
y=rep(x,f)
y
n=sum(f)
n
am=mean(y)
am
ly=log(y)
ly
gm=10^(mean(ly))
gm
hm=n/sum(f/x)
hm
m=which(f==max(f))         
mo=x[m]
mo
me=median(y)
me
q3=quantile(y,0.75)
q3
d7=quantile(y,0.7)
d7
p69=quantile(y,0.69)
p69
cat("Mean=",am,"\n")
cat("Geometric mean=",gm,"\n")
cat("Harmonic mean=",hm,"\n")
cat("Mode=",mo,"\n")
cat("Median=",me,"\n")
cat("Quertile 3=",q3,"\n")
cat("Decile 7=",d7,"\n")
cat("Percentile 69=",p69,"\n")
  

#grouped data
lb=seq(410,470,10)
lb
ub=seq(420,480,10)
ub
h=10
h
f=c(14,20,42,54,45,18,7)
f
x=(ub+lb)/2
x
n= sum(f)
n
am=sum(f*x)/n
am
lx=log(x)
lx
gm=10^(sum(f*lx)/n)
gm
hm=n/sum(f/x)
hm
lcf =cumsum(f)
lcf
mc=min(which(lcf>=n/2))
mc
med=lb[mc]+(n/2-lcf[mc-1])*h/f[mc]
med
qc=min(which(lcf>=3*n/4))
qc
q3=lb[qc]+(3*n/4-lcf[qc-1])*h/f[qc]
q3
dc=min(which(lcf>=6*n/10))
d6=lb[dc]+(6*n/10-lcf[dc-1])*h/f[dc]
d6
moc=which(f==max(f))
moc
mo=lb[moc]+((f[moc]-f[moc-1])/(2*f[moc]-f[moc-1]-f[moc+1]))*h
mo
cat("Mean=",am,"\n")
cat("Geometric mean=",gm,"\n")
cat("Harmonic mean=",hm,"\n")
cat("Median=",med,"\n")
cat("Quartile 3=",q3,"\n")
cat("Decile 6=",d6,"\n")
cat("Mode=",mo,"\n")
 



# exercise
#question 1
x=c(5,2,7,9,12,15,6,7,8,14,21,9,12,15,7,7,9,12,5,2,7,5,10,12,16,14,10,9,7,8)
x
n=length(x)
n
lx=log(x)
lx
am=mean(x)
am
gm=10^(mean(lx))
gm
hm=1/mean(1/x)
hm
hm1=n/sum(1/x)
hm1
med=median(x)
tx=table(x)
tx
m=which(tx==max(tx))
m
stx=sort(unique(x))
stx
mo=stx[m]
mo
d8=quantile(x,0.8)
d8


#question 2
x="Age at the first child birth"
x
x=19:29
x
f=c(37,85,116,225,200,190,320,290,300,190,100)
f
n=sum(f)
n
am=sum(f*x)/n
am
lx=log(x)
lx
gm=10^(sum(f*lx)/n)
gm
hm=n/sum(f/x)
hm
med=median(x)
med
m=which(f==(max(f)))
m
mo=x[m]
cat("Mean=",am,"\n")
cat("Geometric mean=",gm,"\n")
cat("Harmonic mean=",hm,"\n")
cat("Median=",med,"\n")
cat("Mode=",mo,"\n")


#question 3
lb=seq(20,50,5)
lb
ub=seq(24,54,5)
ub
x=(ub+lb)/2
x
f=c(50,70,100,180,120,60,25)
f
lx=log(x)
lx
h=4
h
n=sum(f)
n
am=sum(f*x)/n
am
gm=10^(sum(f*lx)/n)
gm
hm=n/sum(f/x)
hm
lcf=cumsum(f)
lcf
mc=min(which(lcf>=n/2))
mc
med=lb[mc]+(n/2-lcf[mc-1])*h/f[mc]
med
moc=which(f==(max(f)))
moc
mo=lb[moc]+((f[moc]-f[moc-1])/(2*f[moc]-f[moc-1]-f[moc+1]))*h
mo
qc=min(which(lcf>=3*n/4))
qc
q3=lb[qc]+(3*n/4-lcf[qc-1])*h/f[qc]
q3
dc=min(which(lcf>=4*n/10))
dc
d4=lb[dc]+(4*n/10-lcf[dc-1])*h/f[dc]
d4



x=c(100,190,210,160,150,160,190,200,170,152)
x
q1=quantile(x,0.25)
q1
q2=quantile(x,0.5)
q2
q3=quantile(x,0.75)
q3
qd=(q3-q1)/(q3+q1)
qd
n=length(x)
n
vx=var(x)
vx
v=((n-1)/n)*vx
v
sd=sqrt(v)
sd
cv=sd/mean(x)*100
cv




#measure of dispersion
z=c(2,5,9,7,11,6,5,2,7,9,3,2,8,12,4,6,3,9,8,7)
z
range= max(z)-min(z)
range
#coefficient of range
cr=(max(z)-min(z))/(max(z)+min(z))
cr
#quartiles
q1=quantile(z,0.25)
q1
q2=quantile(z,0.5)
q2
q3=quantile(z,0.75)
q3
#quartile deviation
qd=(q3-q1)
qd
#coefficient of quartile deviation
cqd=(q3-q1)/(q3+q1)
cqd
am=mean(z)
am
abs=abs(z-am)
abs
#mean deviation about mean
md=sum(abs)/length(z)
md
cv=sqrt(((n-1)/n)*var(z))/mean(z)
cv
n=length(z)
n
cm1=sum(z-am)/n
cm1
cm2=sum((z-am)^2)/n
cm2
cm3=sum((z-am)^3)/n
cm3
tz=table(z)
tz
m=which(tz==max(tz))
m
stx=sort(unique(z))
stx
mo=stx[m]
mo
cat("mean=",am,"\n")
cat("mode=",mo,"\n")
sd=sqrt(((n-1)/n)*var(z))
sd

#karl pearson coefficient of skewness
skp=(am-mo)/sd
skp
#bowley's coefficient of skewness
skb=(q3+q1-2*q2)/(q3-q1)
skb







# plots

names(stackloss)
library(ggplot2)
ggplot(stackloss,aes(x=Air.Flow))+
  geom_histogram(binwidth = 5,color="black",fill="green")+
  ggtitle("Histogram of Air Flow")+
  xlab("Air Flow")+
  ylab("Count")+
  theme(plot.title=element_text(hjust=0.5))



ggplot(stackloss,aes(x="",y=Air.Flow))+ 
  geom_boxplot()+
  ggtitle("Boxplot")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5))

library(ggplot2)
ggplot(stackloss,aes(x="",y=Water.Temp,fill=Air.Flow))+
  geom_bar(width=1,stat = "identity")+
  coord_polar("y",start=0)+
  ggtitle("Pie chart")+
  theme(plot.title = element_text(hjust=0.5))


library(ggplot2)
ggplot(stackloss,aes(x=Air.Flow,y=Water.Temp))+
  geom_col(fill="blue")+
  ggtitle("bar diagram")+
  theme(plot.title=element_text(hjust = 0.5))


df=data.frame(country=c("India","Germany","China","Pakistan","Sweden"),birthrate=c(33,16,40,35,15))
df
z=ggplot(df,aes(x=country,y=birthrate))+
  geom_col(fill="blue")+
  ggtitle("Birthrate for Countries")+
  theme(plot.title = element_text(hjust = 0.5))
z  

ggplot(stackloss,aes(x=Air.Flow,y=Water.Temp,group=1))+
  geom_line()+
  ggtitle("polygon")+
  theme(plot.title = element_text(hjust = 0.5))

df<-data.frame(Year=c("2000-2001","2000-2001","2000-2001","2001-2002","2001-2002","2001-2002","2002-2003","2002-2003","2002-2003"),
               Marks=c(20,10,5,25,9,10,30,20,20),Stream=c("Arts","Science","Low","Arts","Science","Low","Arts","Science","Low"))
df
ggplot(df,aes(x=Year,y=Marks))+
  geom_col(aes(fill=Stream),position="dodge")+
  ggtitle("Barplot")+
  theme(plot.title = element_text(hjust=0.5))


df<-data.frame(Year=c("2000-2001","2000-2001","2000-2001","2001-2002","2001-2002","2001-2002","2002-2003","2002-2003","2002-2003"),
               Marks=c(20,10,5,25,9,10,30,20,20),Stream=c("Arts","Science","Low","Arts","Science","Low","Arts","Science","Low"))
df
ggplot(df,aes(x=Year,y=Marks))+
  geom_col(aes(fill=Stream))+
  ggtitle("Barplot")+
  theme(plot.title = element_text(hjust=0.5))

library(ggplot2)
ggplot(stackloss,aes(x=Air.Flow))+
  geom_histogram(binwidth = 5,color="black",fill="blue")+
  ggtitle("Histogram")+
  theme(plot.title = element_text(hjust=0.5))
