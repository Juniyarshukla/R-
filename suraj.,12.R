y=c(40,67,75,48,44,53,56,43,66,57,65,52,83,83,80,76,85,88,89,87)
y
mean(y)
median(y)
ty=table(y)
ty
m=which(ty==max(ty))
m
stx=sort(unique(y))
stx
mode=stx[m]
mode
var=(sum(y-mean(y)))^2/length(y)
var
var(y)
range=max(y)-min(y)
range
q1=quantile(y,.025)
q1
q2=quantile(y,.50)
q2
q3=quantile(y,.75)
q3
q4=quantile(y,1.00)
q4
cm1=(sum(y-mean(y)))/length(y)
cm1
cm2=(sum(y-mean(y)))^2/length(y)
cm2
cm3=(sum(y-mean(y)))^3/length(y)
cm3
cm4=(sum(y-mean(y)))^4/length(y)
cm4
b1=(cm3)^2/(cm2)^3  # Skewness
b1
b2=(cm4)/(cm2)^2   # Kurtosis
b2




# Questivarton no . 02

y
cv=sqrt(var(y))/mean(y)*100
cv
#boxplot
df=data.frame(y=c(40,67,75,48,44,53,56,43,66,57,65,52,83,83,80,76,85,88,89,87))
df
z=ggplot(df,aes(x="",y=y))+geom_boxplot()+ggtitle("Boxplot")+theme_minimal()+theme(plot.title = element_text(hjust = 0.5))
z

# Question No .03

df=data.frame(items=c("Food","clothing","Rent","Fule and Lighting","Education","Miscellanous"),Expenditure=c(240,66,125,57,42,190))
df
z=ggplot(df,aes(x="",y=Expenditure,fill=items))+geom_bar(width = 1,stat = "identity")+coord_polar("y",start = 0)+ggtitle("pie Chart")+theme(plot.title = element_text(hjust = 0.5))
z

 # Question No.04

df=data.frame(country=c("India","Germany","China","Pakistan","Sweeden"),Birthrate=c(33,16,40,35,15))
df
z=ggplot(df,aes(country,Birthrate))+geom_col(fill="blue")+ggtitle("Birthrate for countries ")+xlab("country")+ylab("Birthrate")+theme(plot.title = element_text(hjust = 0.5))
z 

   # Question no .05 

df=data.frame(year=c("2000-2001","2000-2001","2000-2001","2001-2002","2001-2002","2001-2002","2002-2003","2002-2003","2002-2003"),Marks=c(20,10,5,25,9,10,30,20,20),Stream=c("Arts","Science","Low","Arts","Science","Low","Arts","Science","Low"))
df
c<-ggbarplot(data=df,x="year",y="Marks",fill = "Stream",add=c(mean_sd),position = position_dodge(0.8),width=0.7)+scale_fill_brewer(palette="Reds")+ggtitle("Marks And Stream")+theme(plot.title = element_text(hjust = 0.5))
c      





# Unit  - 03


X=c(5,7,8,2,2,9,4,11,12,9,6)
plot(x)
plot(x,type = "h",main = "First Graph",sub = "first subtitle",xlab = "X- axis",ylab="y-axis")
?plot
iris
view(iris)
View(iris)


plot(iris$Sepal.Length,main = "graph of Sepal Length of iris data")

mtcars
view(mtcars)
View(mtcars)
?mtcars
names(mtcars)




# Bivariate Scatter Plot 

x<-c(5,7,8,7,2,2,9,4,11,12,9,6)
y<-c(99,86,87,88,111,103,87,94,78,77,85,86)
plot(x,y,main="observation of cars",xlab = "car age",ylab = "car speed")
abline(lm(y~x),col='red')
cor(x,y,method = "pearson")
cor(x,y,method = "pearson")
cor(x,y,method = "spearman")
    

# Regression 
# one Variabale

model=lm(y~x)
summary(model)


# Two Variable 
model1=lm(Petal.Length~Sepal.Width+Sepal.Length,data = iris)
model1
summary(model1)



# load the MASS package
install.packages("MAAS")

print(str(survey))



# Create a data frame from from the main daat set 

stu_data = data.frame(survey$Smoke,survey$Exer) 

# Create a contingency table with the needed variables.		 
stu_data = table(survey$Smoke,survey$Exer) 
print(print(stu_data) )


# applying chisq.test() function 
print(chisq.test(stu_data)) 

#### qqplot ####
# Set seed for reproducibility
set.seed(121)


# Create random normally distributed values
x <- rnorm(1200)

# QQplot of normally distributed values
qqnorm(x)

# Add qqline to plot
qqline(x, col = "darkgreen")
# Length
qqnorm(iris$Sepal.Length)


# Add qqline to plot
qqline(iris$Sepal.Length, col = "darkgreen")

?qqnorm
y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 1)
qqplot(y, rt(300, df = 5))


# Assiment Question N0 .10 

temperature=c(23,32,25,35,32,35,33,27,27,29,35,33,29,25,31,34,27,38,29,38)
temperature
sales=c(47,75,30,88,60,81,85,60,41,42,20,90,30,65,65,30,55,65,60,60)
sales
plot(temperature,sales,main = "correlation between temperature and sales",xlab = ("temperature"),ylab = ("sales"))
model=lm(sales~temperature)
model
abline(model,col="darkgreen")
cor(temperature,sales,method = "pearson")
cor(temperature,sales,method ="spearman")

summary(model)
# Question  09


temperature=c(23,32,25,35,32,35,33,27,27,29,35,33,29,25,31,34,27,38,29,38)
temperature
sales=c(47,75,30,88,60,81,85,60,41,42,20,90,30,65,65,30,55,65,60,60)
sales
model=lm(sales~temperature)
model
summary(model)
plot(temperature,sales)
abline(model,col="red")