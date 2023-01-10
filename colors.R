
# 色板(grid) ----

library(grid)

a <- colors()

grid.rect(x = rep(seq(0.04,0.96,length=22),times=30),
          y = rep(seq(0.95,0.05,length=30),each=22),
          width = 0.035,
          height = 0.015,
          gp=gpar(fill=a,lwd=0.5))

grid.text(a,
          x=rep(seq(0.04,0.96,length=22),times=30),
          y = rep(seq(0.965,0.065,length=30),each=22),
          gp=gpar(fontsize=15))


# 色板(ggplot2) ----

library(tidyverse)

clr <- data.frame(no=1:657,color=colors()) %>% 
  rbind(data.frame(no=658:660,color=rep('white',3)))%>% 
  mutate(x=rep(1:33*30,20)) %>% 
  mutate(y=rep(1:20*5,each=33)) %>% 
  slice_head(n=657)

ggplot(clr, aes(xmin=x-12,xmax=x+12, ymin=y-1,ymax=y+1)) +
  geom_rect(aes(fill=I(color)),colour = "black")+
  lims(y=c(110,0))+
  theme_void()+
  geom_text(aes(x,y-2,label=color),size=4)

ggsave('colors.pdf',limitsize = FALSE,
       width = 50,height = 30,device = 'pdf')


# 色板升级版(去掉重复颜色) ----

library(tidyverse)

clr <- data.frame(no=1:502,color=colors(TRUE)) %>% 
  rbind(data.frame(no=503:510,color=rep('white',8))) %>% 
  mutate(l=c(rep(1:10,each=50),rep(10,10))) %>% 
  mutate(x=rep(1:5*5,102)) %>% 
  mutate(y=c(rep(rep(1:10*4,each=5),10),rep(11*4,10))) %>% 
  slice_head(n=502)


l <- split(clr,clr$l)

f <- function(df){
  ggplot(df, aes(xmin=x-2,xmax=x+2, ymin=y-1,ymax=y+1)) +
    geom_rect(aes(fill=I(color)),colour = "black")+
    lims(y=c(50,0))+
    theme_void()+
    geom_text(aes(x,y-2,label=color),size=4)
}

for(i in 1:length(l)){
  f(l[[i]])
  ggsave(paste0(i,'.pdf'),
         width = 8.27,height = 11.69,device = 'pdf')
}



