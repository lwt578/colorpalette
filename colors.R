library(tidyverse)
library(plotly)

# 色板 ----

clr <- data.frame(no=1:657,color=colors()) %>% 
  rbind(data.frame(no=658:660,color=rep('white',3)))

clr <- clr %>% 
  mutate(x=rep(1:33*30,20)) %>% 
  mutate(y=rep(1:20*5,each=33)) %>% 
  slice_head(n=657)


ggplot(clr, aes(xmin=x-12,xmax=x+12, ymin=y-1,ymax=y+1)) +
  geom_rect(aes(fill=I(color)),colour = "black")+
  lims(y=c(110,0))+
  theme_void()+
  geom_text(aes(x,y-2,label=color),size=4)

# 色板升级版 ----

clr <- data.frame(no=1:657,color=colors()) %>% 
  rbind(data.frame(no=658:660,color=rep('white',3)))

clr <- clr %>% 
  mutate(l=rep(1:10,each=66)) %>% 
  mutate(x=rep(1:6*5,110)) %>% 
  mutate(y=rep(rep(1:11*3,each=6),10)) %>% 
  slice_head(n=657)

l <- split(clr,clr$l)

f <- function(df){
  ggplot(df, aes(xmin=x-2,xmax=x+2, ymin=y-1,ymax=y+1)) +
    geom_rect(aes(fill=I(color)),colour = "black")+
    lims(y=c(35,0))+
    theme_void()+
    geom_text(aes(x,y-1.5,label=color),size=3)
  
  
}

photo <- map(l,f)
photo


  
