x=c(100,190,210,160,150,160,190,200,170,152)
x
n=length(x)
n
am=mean(x)
am
1X=log10(x)
1X
X=log10(x)
X
gm=10^mean(X)
gm
hm=n/sum(1/X)
hm
tx=table(x)
tx
m=which(ty==max(x))
m
stx=sort(unique(x))
stx
mode=stx[m]
mode
median(x)
q



# Question no .02 

x=1:5
x
length(x)
f=c(7,11,9,8,3)
f
n=sum(f)
n
y=rep(x,f)
y
am=mean(y)
am
Y=log10(y)
Y
gm=10^mean(Y)
gm
hm=n/sum(x,f)
hm
m=which(f==max(f))
m
mode=x[m]
mode


# Grouped Data 

lb=seq(410,470,10)
lb
ub=seq(420,480,10)
ub
f=c(14,2042,54,45,18,7)
f
x=(lb+ub)/2
x
n=sum(f)
n
am=sum(f*x)
am
gm=10^(sum(f*log10(x))/n)
gm
hm=n/sum(f/x)
hm
lcf=cumsum(f)
lcf
mc=min(which(lcf>=n/2))
mc
median=lb[mc]

mod