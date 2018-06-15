#”»•Ê•ªÍ

#ƒ‰ƒCƒuƒ‰ƒŠ[MASS‚Ì“Ç‚İ‚İ
library(MASS) 

#ƒf[ƒ^‚Ì“Ç‚İ
X <- read.csv("fukusou.csv",row.names=1, header = T)@

#”»•Ê•ªÍ
res <- lda(Ši•t~‹æ•ª1+‹æ•ª2+‘Ø”[Šz+‘Ø”[“ú”+‘Ø”[‰ñ”+”„ã+æˆøŒ+‘Ø”[Š„‡,X) 

#”»•Ê“¾“_ƒqƒXƒgƒOƒ‰ƒ€‚ğ‘‚­
#Proportion of trace:‚©‚çALD1 ‚É‘å‚«‚È”»•Ê—Í
res

#æˆøæ‚²‚Æ‚Ì”»•ÊŒ‹‰Ê
res2 <- predict(res) 
res2

#À•ª—Ş‚Æ”»•ÊŒ‹‰Ê‚ÌƒNƒƒX•\
cls <- table(X$Ši•t,res2$class) 

#‘ÎŠpü‚É‚Ç‚ê‚¾‚¯æ‚¹‚ç‚ê‚Ä‚¢‚é‚©H
cls[row(cls)==col(cls)]
#‘S‘Ì
sum(cls) 
#”»•Ê³‰ğ—¦‚ğZo
sum(cls[row(cls)==col(cls)])/sum(cls) 
#Œë”»•Ê—¦‚ğZo
sum(cls[row(cls)!=col(cls)])/sum(cls) 

#Šé‹Æ‚²‚Æ‚ÌÀŠi•t‚Æ”»•ÊŒ‹‰Êˆê——
data.frame(row.names =row.names(X),ÀŠi•t=X$Ši•t,”»•ÊŒ‹‰Ê=res2$class) 


#‰ñ‹A•ªÍ‚ÉŠî‚Ã‚«—LŒø‚Èw•Wi•Ï”j‚Ì‘I‘ğ
lm(Ši•t~‹æ•ª1+‹æ•ª2+‘Ø”[Šz+‘Ø”[“ú”+‘Ø”[‰ñ”+”„ã+æˆøŒ+‘Ø”[Š„‡, data=X)

#•Ï”‘Œ¸–@‚É‚æ‚é•Ï”‘I‘ğ
h <- step(lm(Ši•t~‹æ•ª1+‹æ•ª2+‘Ø”[Šz+‘Ø”[“ú”+‘Ø”[‰ñ”+”„ã+æˆøŒ+‘Ø”[Š„‡, data=X))
summary(h)

#‘I‘ğ‚³‚ê‚½w•W‚¾‚¯‚ğ—p‚¢‚½”»•Ê•ªÍ
res3 <- lda(Ši•t~‘Ø”[“ú”+‹æ•ª1+‘Ø”[Š„‡+‹æ•ª2+æˆøŒ+‘Ø”[Šz,X)

#æˆøæ‚²‚Æ‚Ì”»•ÊŒ‹‰Ê
res4 <- predict(res3)  
#ÀŠi•t‚Æ”»•ÊŒ‹‰Ê‚ÌƒNƒƒX•\
cls2 <- table(X$Ši•t,res4$class)
#”»•Ê³‰ğ—¦‚ğZo
sum(cls2[row(cls2)==col(cls2)])/sum(cls2) 
#Œë”»•Ê—¦‚ğZo
sum(cls2[row(cls2)!=col(cls2)])/sum(cls2) 


##predict use
train <- X[490:498,]
testResult<-predict(res3,train)
data.frame(testResult$x[,1],train[,9])


##æˆøŠz‚ğ‰¼‚Éİ’è‚·‚é
XX <- data.frame(X,sample(c(1:5000), 498, replace=TRUE))

Result <- predict(res3,XX)
pre <- data.frame(XX, Result$class, Result$x[,1],rownames(pre))
names(pre)[10] <- "”NæˆøŠz" 
names(pre)[11] <- "—\‘ªŠi•t" 
names(pre)[12] <- "—\‘ª"
names(pre)[13] <- "name"

library(ggplot2)
library(ggrepel)
library(ggthemes)
library(RColorBrewer)
library(plyr)


##range
cols <- c("4" = "dodgerblue4", "3" = "dodgerblue3", "2" = "lightsteelblue1", "1" = "lightslategrey")
##plot
ggplot(pre, aes(—\‘ª, ”NæˆøŠz,color=—\‘ªŠi•t)) +
  geom_vline(xintercept=0,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_hline(yintercept=1000,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_point(alpha=0.7) + 
  labs(title="") + 
  xlab("—\‘ª") +
  ylab("”NæˆøŠz(ç‰~)") + 
  scale_colour_manual(values = cols)+
  theme_classic(base_family="HiraKakuProN-W3") 

##plot2
ggplot(pre, aes(—\‘ª, ”NæˆøŠz,color=—\‘ªŠi•t)) +
  geom_vline(xintercept=0,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_hline(yintercept=1000,colour = "black",alpha=0.3,linetype="dashed") + 
  geom_point(alpha=0.7) + 
  labs(title="") + 
  xlab("—\‘ª") +
  ylab("”NæˆøŠz(ç‰~)") + 
  scale_colour_manual(values = cols)+
  geom_text_repel(data=subset(pre, ”NæˆøŠz >= 3000 & —\‘ª >= 2),
                  aes(x=—\‘ª,y=”NæˆøŠz, label = name),
                  col = "black", size = 2.5,segment.color = NA ,
                  family = "HiraKakuPro-W3")+
  annotate("rect",xmin=2, xmax=max(pre$—\‘ª), ymin=3000, ymax=max(pre$”NæˆøŠz),
           alpha=0.2, fill="red") +
  theme_classic(base_family="HiraKakuProN-W3") 


