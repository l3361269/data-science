#http://rpubs.com/skydome20/R-Note5-First_Practice
#1.�d�ݸ�ƶ�
require(datasets)
str(iris) #��iris��structure
head(iris,n=6)
summary(iris)

#2.�i���Ƥ��R�e���[����
# �Ҧp�Ḱ���שM�Ḱ�e�ץi�঳�����A�e�ϨӬݬ�
#   (1)��base plot
plot(x=iris$Sepal.Length,y=iris$Sepal.Width,pch=2)
#   (2)��Lattice
require(lattice)
xyplot(iris$Sepal.Width~iris$Sepal.Length)
#      �άO xyplot(Sepal.Length~Sepal.Width,data=iris)
#   (3)��ggplot2
require(ggplot2)
ggplot(data=iris)+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width))+
  theme_bw()
# �ݤ��X�A���t�@�ո��
#   (1)base plot
plot(x=iris$Petal.Length,y=iris$Petal.Width)
#   (2)lattice
xyplot(iris$Petal.Width~iris$Petal.Length)
#   (3)ggplot
ggplot()+
  geom_point(aes(x=iris$Petal.Length,y=iris$Petal.Width))+
  theme_bw()
#  =>�ݨ��̦��u�����Y�A�B�����k�W�B���U��s�A�����i��򪫺ئ���
#    ���F�T�{�o�I�A�b�ϤW�ФW�C��άO����Ž��
iris$Species
d1<-iris[iris$Species=="setosa",]  #�`�N","
d2<-iris[iris$Species=="versicolor",]
d3<-iris[iris$Species=="virginica",]
# �e�ϫ�A���I->��q��/�T�q�����e��or�@�馨�Ϊ�(������쥻code)
#   (1)��base plot�e��
plot(x=iris$Petal.Length,y=iris$Petal.Width)
points(x=d1$Petal.Length,y=d1$Petal.Width,col="green")
points(x=d2$Petal.Length,y=d2$Petal.Width,col="red")
points(x=d3$Petal.Length,y=d3$Petal.Width,col="blue")
#      label�n�ߺD
legend("topleft",pch=17,legend=c("setosa","versicolor","virginica"),col=c("green","red","blue"))
#  (2)��lattice
xyplot(iris$Petal.Width~iris$Petal.Length,#y~x
       pch=18,group=iris$Species#���s���I
       #label
       ,auto.key=list(space="top",colums=3,title="Species labels"))
#  (3)��ggplot2
ggplot()+
  geom_point(aes(x=iris$Petal.Length,
                 y=iris$Petal.Width,
                 color=iris$Species))#���s���I
  +theme_bw() #���I���ܥ�
# �t�@��k:��Ž�ϬݦU���ؤ���
#  (1)base�A���n���������factor�e
#species<-as.factor(iris$Species)
boxplot(iris$Petal.Length~iris$Species,xlab="Species",ylab="Petl.Length")
boxplot(iris$Petal.Width~iris$Species,xlab="Species",ylab="Petal.Width")
#  (2)lattice
bwplot(iris$Petal.Width~iris$Petal.Length,group=iris$species)
#  (3)ggplot2 (���ӬO�̾A�X��)
#     ��ggplot�e��
ggplot()+
  geom_boxplot(aes(x=iris$Petal.Length,
                   y=iris$Petal.Width,
                   color=iris$Species))+
  theme_bw()
#    ��ggplot��qplot
qplot(x=iris$Petal.Length,
      y=iris$Petal.Width,
      geom="boxplot",
      color=iris$Species
      )
#3.��ƪ��w�B�z(��Ʊ��ɤ��̪�ɶ�������)
#  �����A�䦳�L��|��:is.na()
data<-data.frame(x=c(1,2,3,NA,5),y=c(4,5,3,NA,NA))
data
#  �ݦ��L��|��(�P�_��ƫ��A��is.XX()�A�^��true/false)
is.na(data)
table(is.na(data))#�i�H���D���X�ӿ�|��
#  �B�zNA�A��k�����(1.�����A2.���(�Ҧp�Υ����ȶ�))
#    (1)����NA
data[complete.cases(data),]
na.omit(data)#���o���O�t�~�إߤ@�Ӫ���A���|���ۤv
#    (2)���NA(�o�̶�J������)
data[is.na(data[,"y"]),"y"]<-mean(data[,"y"],na.rm=T)
data
#  �m�ߧ��A�{�b�^��iris�ˬd�ݬݦ��S��|��(NA)
table(is.na(iris))
#  ��..�S����|�ȡA���N���U�h�a
#4.�j�k���R
#  �o�̨Ҥl�H���ܼ�Xi:Sepal.Width�BPetal.Length�BPetal.Width
#  ���ܼ�Y:Sepal.Length
model<-lm(formula=iris$Sepal.Length~iris$Sepal.Width+iris$Petal.Length+iris$Petal.Width)
summary(model)
#  =>Sepal.Length=1.85600+0.65084*Sepal.Width+0.70913*Petal.Length-0.55648*Petal.Width
#    �ھ�p-value=>�T�Ӧ��ܼ�(x)��Y���������
#    R-squared:0.8586�AAdj R-squard:0.8557=>���ܼҫ��w����O����
#    Residual standard error:0.3145
#  �Ӧb�إߥX�u�ʰj�k�ɡA�����T�{��ݮt(residual)�O�_�ŦX�U�C�T�Ӱ��]:
#  (1).�`�A��(normality) (2).�W�ߩ�(independence) (3).�ܲ��ƦP���(homogeneity of variance)
#  �ҥH�����n���q�j�k�ҫ������ݮt����
#  �i�H��names()�ݦ^�k�ҫ����㦳����T
names(model)
#  ���ۨ��Xresidual(�ݮt)�Ӷi��W���T�Ӱ��]���˩w
model$residual
#    (1)�`�A�ʰ���:shapiro.test()�i�H������ݮt���`�A��
shapiro.test(model$residual)
#       =>�ѩ��L���]H0:�ݮt�A�q�`�A���t�A�]��p-value>0.05=>����H0
#    (2)�W�ߩʰ��]:�n����ݮt���W�ߩʡA�ݭn�ϥήM��car����durbinWatsonTest()
install.packages("car")
require(car)
#        �]��durbinWatsonTest�o�Ө禡�|�۰ʧ���ݮt�A�ҥH�ѼƬO��model
durbinWatsonTest(model)
#        =>�ѩ��L���]H0:�ݮt���ۤ��W�ߡA�]��p��>0.05=>����H0
#    (3)�ܲ��ƦP��ʰ��]:�n����ݮt���ܲ��ƦP��ʡA�ݭn�ϥ�car����ncvTest()
#       �]���o�Ө禡�|�۰ʧ�ݮt�A�ҥH�ѼƩ�model
ncvTest(model)
#       =>�ѩ��L����H0:�ݮt�ܲ��ƨ㦳�P��ʡA�]��p��<0.05=>�ڵ�H0
#       =>�W�z���u�ʼҫ��L�k�ϥ�(�����U�ӬO���F�Ч޳N�A�ҥH���ް�~)
# �w��
#   �o�̫إ߼ҫ����ت��O���F�w��
#   �]�����J�s���[����:Sepal.Width�BPetal.Length�BPetal.Width
#   ��w�g�إߦn���j�k�ҫ��A�w���XSepal.Length����
#   �ϥΪ��Opredict()
new.iris<-data.frame(Sepal.Width=3.456,Petal.Length=1.535,Petal.Width=0.341)
new.iris
predict(model,new.iris)
#5.�ܲ��Ƥ��R(anova)
# �g�L��ı�ƪ��B�J�A�o�{�T�ӫ~��iris��Petal.Width��Petal.Length
# �������ƬO�_���Үt���A�Y�n�βέp�W���˩w�A�n�i�@�B�a�T�{
# �N�i�H�ϥ��ܲ��Ƥ��R(anova)
# ���w�˩w������H0�MH1���O�p�U:
#   H0: �g(Setosa) = �g(Versicolor) = �g(Virginica)
#   H1:�ܤ֦��@�إ����ƩM��L�~�ؤ��۵�
# �n��one-way-anova�AR���禡��anova()�A�åB�ƥ��n�]�u�ʰj�k�ҫ�
a.lm<-lm(iris$Petal.Width~iris$Species)
anova(a.lm)
b.lm<-lm(iris$Petal.Length~iris$Species)
anova(b.lm)
# ��̪�p�ȳ����p��0.05=>���ܤ��P�~�ض��T�꦳��ۮt��