#question 1
s1=c(37, 49, 7, 38)
s2=c(16, 37, 21, 42, 27, 40, 39, 51)
sp=s1+s2
sp
sn=(s1+s2)/2
sn
sd=s1/s2
sd
sm=s1*s2
sm

#question 2
a=seq(1,37,3)
a
b=seq(1,13)
b
length(a)
length(b)
c=a*b
print (c)
d=a/b
print (d)
e=a+b
print (e)
f=a-b
print(f)

#question 3
d<-c(1,2,3,4)
A=matrix(d,nrow=2,ncol=2,byrow=T)
A
d1<-c(2,6,8,12)
B=matrix(d1,nrow=2,ncol=2,byrow=T)
B
print(A);print(B)##a) Enter matrics
A+B
B-A
A%*%B
det(A)
det(B)
solve(A)
solve(B)
t(A)
A%*%t(B)
t(A%*%B)
t(A)%*%t(B)#b)A+B, B-A, AB, IAI, IBI, inverse of A and B, A', AB', (AB)', A'B'.
# question no 4
d<-c(1,3,5,8,5,5,2,3,1)
A=matrix(d,nrow=3,ncol=3,byrow=T)
A
d1<-c(2,3,2,0,6,5,1,3,2)
B=matrix(d1,nrow=3,ncol=3,byrow=T)
B

print(A);print(B)##a) Enter matrics
A+B
B-A
A%*%B
det(A)
det(B)
solve(A)
solve(B)
t(A)
A%*%t(B)
solve(t(A%*%B))
t(A)%*%t(B)#b)A+B, B-A, AB, IAI, IBI, inverse of A and B, A', AB', (AB)', A'B'.

#Question 4
df <- data.frame(country = c("India", "Germany", "China", "Pakistan", "Sweden"), birthrate = c(33, 16, 40, 35, 15))
df
ggplot(df, aes(country,birthrate )) + geom_col(fill = "blue") 
+ ggtitle("Birth rate for countries") + xlab("Country") 
+ ylab("Birth rate")+ theme(plot.title = element_text(hjust=0.5))

#Question 5
df <- data.frame(country = c("Delhi", "Kolkata", "Mumbai", "Chennai", "Delhi", "Kolkata", "Mumbai", "Chennai"), temperature = c(40.5, 42.8, 37.8, 39.4, 34.7, 33.5, 32.2, 33.1), Level = c("Max","Max","Max","Max","Min","Min","Min","Min"))
df
ggplot(df, aes(country, temperature)) + geom_col(aes(fill = Level),position = "dodge") + ggtitle("Temperature in Cities") + xlab("Country") + ylab("Birth rate") +
  theme(plot.title = element_text(hjust=0.5))

#Question 6

df <- data.frame(Items = c("Food","Clothing","Rent","Fuel and Lighting","Education","Miscellanoues"), Expenditure= c(240,66,125,57,42,190))
a<-ggplot(df, aes(x="", y=Expenditure, fill=Items))+geom_bar(width = 1,stat="identity")
a+coord_polar("y", start=0)+ ggtitle("Pie Chart")+theme(plot.title = element_text(hjust=0.5))





