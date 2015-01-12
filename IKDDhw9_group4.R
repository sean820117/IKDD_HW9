data<-read.table(url("http://www.datagarage.io/api/5488687d9cbc60e12d300ba5"))
o<-seq(2, 4000, by=2)
d_c<-as.character(data[o, ])
p_m<-sapply(strsplit(d_c[], ""), function(d_c) which(d_c == ":"))
data<-data.frame(X=as.double(substr(d_c,p_m[2,]+1, 38)), Y=as.double(substr(d_c,p_m[1,]+1, 17)))

x<-data[,1]
y<-data[,2]
fitting<-lm(y~poly(x,25,raw=TRUE))
x_axis<-seq(-10,10, length=50)
predicting_data<-data.frame(x)
plot(data)
lines(x_axis, predict(fitting, data.frame(x=x_axis)), col="red")
y_pred = predict(fitting,newdata=data.frame(x=predicting_data))

data_output<-data.frame(x=predicting_data,y=y_pred)
data_output

write.csv(data_output, file = "IKDDhw9-y.csv", fileEncoding = "UTF-8")
