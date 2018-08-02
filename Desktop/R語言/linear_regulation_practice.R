#http://rpubs.com/skydome20/R-Note5-First_Practice
#1.查看資料集
require(datasets)
str(iris) #看iris的structure
head(iris,n=6)
summary(iris)

#2.進行資料分析前先觀察資料
# 例如花萼長度和花萼寬度可能有相關，畫圖來看看
#   (1)用base plot
plot(x=iris$Sepal.Length,y=iris$Sepal.Width,pch=2)
#   (2)用Lattice
require(lattice)
xyplot(iris$Sepal.Width~iris$Sepal.Length)
#      或是 xyplot(Sepal.Length~Sepal.Width,data=iris)
#   (3)用ggplot2
require(ggplot2)
ggplot(data=iris)+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width))+
  theme_bw()
# 看不出，換另一組資料
#   (1)base plot
plot(x=iris$Petal.Length,y=iris$Petal.Width)
#   (2)lattice
xyplot(iris$Petal.Width~iris$Petal.Length)
#   (3)ggplot
ggplot()+
  geom_point(aes(x=iris$Petal.Length,y=iris$Petal.Width))+
  theme_bw()
#  =>看到兩者成線性關係，且分成右上、左下兩群，推測可能跟物種有關
#    為了確認這點，在圖上標上顏色或是做盒鬚圖
iris$Species
d1<-iris[iris$Species=="setosa",]  #注意","
d2<-iris[iris$Species=="versicolor",]
d3<-iris[iris$Species=="virginica",]
# 畫圖後再標點->兩段式/三段式的畫布or一體成形的(直接改原本code)
#   (1)用base plot畫布
plot(x=iris$Petal.Length,y=iris$Petal.Width)
points(x=d1$Petal.Length,y=d1$Petal.Width,col="green")
points(x=d2$Petal.Length,y=d2$Petal.Width,col="red")
points(x=d3$Petal.Length,y=d3$Petal.Width,col="blue")
#      label好習慣
legend("topleft",pch=17,legend=c("setosa","versicolor","virginica"),col=c("green","red","blue"))
#  (2)用lattice
xyplot(iris$Petal.Width~iris$Petal.Length,#y~x
       pch=18,group=iris$Species#分群標點
       #label
       ,auto.key=list(space="top",colums=3,title="Species labels"))
#  (3)用ggplot2
ggplot()+
  geom_point(aes(x=iris$Petal.Length,
                 y=iris$Petal.Width,
                 color=iris$Species))#分群標點
  +theme_bw() #讓背景變白
# 另一方法:盒鬚圖看各物種分布
#  (1)base，但好像不能按照factor畫
#species<-as.factor(iris$Species)
boxplot(iris$Petal.Length~iris$Species,xlab="Species",ylab="Petl.Length")
boxplot(iris$Petal.Width~iris$Species,xlab="Species",ylab="Petal.Width")
#  (2)lattice
bwplot(iris$Petal.Width~iris$Petal.Length,group=iris$species)
#  (3)ggplot2 (應該是最適合的)
#     用ggplot畫布
ggplot()+
  geom_boxplot(aes(x=iris$Petal.Length,
                   y=iris$Petal.Width,
                   color=iris$Species))+
  theme_bw()
#    用ggplot的qplot
qplot(x=iris$Petal.Length,
      y=iris$Petal.Width,
      geom="boxplot",
      color=iris$Species
      )
#3.資料的預處理(資料探勘中最花時間的部分)
#  首先，找有無遺漏值:is.na()
data<-data.frame(x=c(1,2,3,NA,5),y=c(4,5,3,NA,NA))
data
#  看有無遺漏值(判斷資料型態用is.XX()，回傳true/false)
is.na(data)
table(is.na(data))#可以知道有幾個遺漏值
#  處理NA，方法有兩種(1.移除，2.填補(例如用平均值填))
#    (1)移除NA
data[complete.cases(data),]
na.omit(data)#但這都是另外建立一個物件，不會改到自己
#    (2)填補NA(這裡填入平均值)
data[is.na(data[,"y"]),"y"]<-mean(data[,"y"],na.rm=T)
data
#  練習完，現在回到iris檢查看看有沒遺漏值(NA)
table(is.na(iris))
#  痾..沒有遺漏值，那就接下去吧
#4.迴歸分析
#  這裡例子以自變數Xi:Sepal.Width、Petal.Length、Petal.Width
#  應變數Y:Sepal.Length
model<-lm(formula=iris$Sepal.Length~iris$Sepal.Width+iris$Petal.Length+iris$Petal.Width)
summary(model)
#  =>Sepal.Length=1.85600+0.65084*Sepal.Width+0.70913*Petal.Length-0.55648*Petal.Width
#    根據p-value=>三個自變數(x)對Y都表示顯著
#    R-squared:0.8586，Adj R-squard:0.8557=>表示模型預測能力不錯
#    Residual standard error:0.3145
#  而在建立出線性迴歸時，必須確認其殘差(residual)是否符合下列三個假設:
#  (1).常態性(normality) (2).獨立性(independence) (3).變異數同質性(homogeneity of variance)
#  所以首先要先從迴歸模型中找到殘差的值
#  可以用names()看回歸模型中具有的資訊
names(model)
#  接著取出residual(殘差)來進行上面三個假設的檢定
model$residual
#    (1)常態性假測:shapiro.test()可以用檢驗殘差的常態性
shapiro.test(model$residual)
#       =>由於虛無假設H0:殘差服從常態分配，因為p-value>0.05=>接受H0
#    (2)獨立性假設:要檢驗殘差的獨立性，需要使用套件car中的durbinWatsonTest()
install.packages("car")
require(car)
#        因為durbinWatsonTest這個函式會自動抓取殘差，所以參數是放model
durbinWatsonTest(model)
#        =>由於虛無假設H0:殘差間相互獨立，因為p值>0.05=>接受H0
#    (3)變異數同質性假設:要檢驗殘差的變異數同質性，需要使用car中的ncvTest()
#       因為這個函式會自動抓殘差，所以參數放model
ncvTest(model)
#       =>由於虛無假測H0:殘差變異數具有同質性，因為p值<0.05=>拒絕H0
#       =>上述的線性模型無法使用(但接下來是為了教技術，所以不管啦~)
# 預測
#   這裡建立模型的目的是為了預測
#   因此給入新的觀測值:Sepal.Width、Petal.Length、Petal.Width
#   到已經建立好的迴歸模型，預測出Sepal.Length的值
#   使用的是predict()
new.iris<-data.frame(Sepal.Width=3.456,Petal.Length=1.535,Petal.Width=0.341)
new.iris
predict(model,new.iris)
#5.變異數分析(anova)
# 經過視覺化的步驟，發現三個品種iris的Petal.Width或Petal.Length
# 的平均數是否有所差異，若要用統計上的檢定，要進一步地確認
# 就可以使用變異數分析(anova)
# 假定檢定的對應H0和H1分別如下:
#   H0: μ(Setosa) = μ(Versicolor) = μ(Virginica)
#   H1:至少有一種平均數和其他品種不相等
# 要用one-way-anova，R的函式式anova()，並且事先要跑線性迴歸模型
a.lm<-lm(iris$Petal.Width~iris$Species)
anova(a.lm)
b.lm<-lm(iris$Petal.Length~iris$Species)
anova(b.lm)
# 兩者的p值都遠小於0.05=>表示不同品種間確實有顯著差異