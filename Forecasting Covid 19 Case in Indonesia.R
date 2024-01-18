#Menentukan Folder Kerja
dataku<-read.csv("data_03_26.csv",header=TRUE,sep=",",dec=",")
dataku
attach(dataku)

#Model Kuasa (1)
y<- as.matrix(log(dataku[,2]))
y
x<- as.matrix(log(dataku[,1]))
x

#Model Kurva S (2)
y<- as.matrix(log(dataku[,2]))
y
x<- as.matrix(1/dataku[,1])
x

#Model Eksponensial (3)
y<- as.matrix(log(dataku[,2]))
y
x<- as.matrix(dataku[,1])
x

#Model Logaritma (4)
y<- as.matrix(dataku[,2])
y
x<- as.matrix(log(dataku[,1]))
x

#Model Invers (5)
y<- as.matrix(dataku[,2])
y
x<- as.matrix(1/dataku[,1])
x

#Model Pertumbuhan (6)
y<- as.matrix(log(dataku[,2]))
y
x<- as.matrix(dataku[,1])
x

#Model Gabungan (7)
y<- as.matrix(log(dataku[,2]))
y
x<- as.matrix(dataku[,1])
x

#Model Linier (8)
y<- as.matrix(dataku[,2])
y
x<- as.matrix(dataku[,1])
x

#Model Logistik (9)
y<- as.matrix(dataku[,2])
y
x<- as.matrix(dataku[,1])
x

#Model Kuadrat (10)
y<- as.matrix(dataku[,2])
y
x<- as.matrix(dataku[,1])
x
t<- x
t
t2<- t^2
t2
u<- matrix(c(1),n,1)
u
x0<- cbind(u,t,t2)
x0

#Model Kubik (11)
y<- as.matrix(dataku[,2])
y
x<- as.matrix(dataku[,1])
x
t<- x
t
t2<- t^2
t2
t3<- t^3
t3
u<- matrix(c(1),n,1)
u
x0<- cbind(u,t,t2,t3)
x0


n<- nrow(y)
n
u<- matrix(c(1),n,1)
u
x0<- cbind(u,x)
b<- solve(t(x)%*%x)%*%t(x)%*%y
b
H<- x%*%solve(t(x)%*%x)%*%t(x)
H
I<- diag(c(1),20,20)
I
SSE<- t(y)%*%(I-H)%*%y
SSE
p<- ncol(x)
p
s2<- SSE/(n-p)
s2
MSE<- s2
MSE
S2_b<- s2[1,1]*solve(t(x)%*%x)
S2_b
s_b<- sqrt(diag(S2_b))
s_b
J<- matrix(c(1),n,n)
J
SSR<- t(y)%*%(H-(J/n))%*%y
SSR
MSR<- SSR/(p-1)
MSR
SST<- SSR+SSE
SST
R2<- SSR/SST
R2
R2a<- 1-MSE/(SST/(n-1))
R2a


F<- MSR/s2
F
F_a<- qf(0,95,p-1,n-p)
F_a
p_vF<- 1-pf(F,p-1,n-p)
p_vF
t<- b/s_b
t
p_vb<- 2*(matrix(c(1),p,1)-pt(abs(t),n-p))
p_vb
est<- cbind(b,s_b,t,p_vb)
est
