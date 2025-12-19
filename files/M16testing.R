#new Fig 5 with M16
m16.funC <- function(isim, ft='Con') {
  ft_vals=rep(ft, times=length(isim))
  predict(m16.sf.ftim2, newdata=data.frame(isi.m=isim, 
          FT=ft_vals)) %>%
    unname()
}

#testing FT influence on asy models
# m10.sf.agg.asy<- nls(data=s.fires.mod, formula= #substitute for sf.crisi.mod
#                        ROS ~ (asy.slope1 * ISI + asy.yInt1) * (1-exp(-b*ISI))^c, 
#                      start=list(b=0.1, c=3))

s.fires.mod2 <- s.graph

s.fires.mod2$FT_P <- as.numeric(s.fires.mod$FT == "PPDF")
s.fires.mod2$FT_D <- as.numeric(s.fires.mod$FT == "Decid")


mXX.sf.agg.asy<- nls(data=s.fires.mod2, formula= #substitute for sf.crisi.mod
    ROS ~ (asy.slope1 * ISI + asy.yInt2) * (1-exp(-b*ISI))^c +
      aP*FT_P + aD*FT_D,
                     start=list(b=0.1, c=3, aP=0, aD=0))
##aP and aD both significant?! 
mXX.fun <- function(isi, ftd=0, ftp=0) {
  predict(mXX.sf.agg.asy, 
          newdata=list(ISI=isi, FT_D=ftd, FT_P=ftp))
}
#ok - works

mYY.sf.agg.asy<- nls(data=s.fires.mod, formula= #substitute for sf.crisi.mod
                       ROS ~ (asy.slope1 * isi.m + asy.yInt1) * (1-exp(-b*isi.m))^c +
                       aP*FT_P + aD*FT_D,
                     start=list(b=0.1, c=3, aP=0, aD=0))
#aP, aD NS

#########test on graph below


m16.funP <- function(isim, ft='PPDF') {
  ft_vals=rep(ft, times=length(isim))
  predict(m16.sf.ftim2, newdata=data.frame(isi.m=isim, 
                                           FT=ft_vals)) %>%
    unname()
}

m16.funD <- function(isim, ft='Decid') {
  ft_vals=rep(ft, times=length(isim))
  predict(m16.sf.ftim2, newdata=data.frame(isi.m=isim, 
                                           FT=ft_vals)) %>%
    unname()
}


####
figX <- ggplot(s.fires.mod2 %>% filter(!is.na(SFC)), 
               aes(x=ISI, y=ROS)) +  
  scale_colour_manual(values=c(
    'red2',
    'blue4', 
    'green3')) +
  geom_point(aes(colour=FT, size=SFC), alpha=0.5) + #main fires, adj. PPDF
  geom_point(data=s.fires3 %>% filter(!is.na(SFC)) %>% 
               filter(FT=='PPDF'), 
             aes(size=SFC), shape=21, color='darkgray', 
             fill='darkgray', alpha=0.5) + #PPDF original
  geom_point(data=wildfires, colour='black', fill='black', size=1, 
             shape=21) +
  stat_function(fun=sf.m15b, args=list(ftd=0, ftp=0),
                colour='red') + #con
  stat_function(fun=sf.m15b, args=list(ftd=1, ftp=0),
                colour='green') +
  stat_function(fun=sf.m15b, args=list(ftd=0, ftp=1), 
                colour='blue') +
  stat_function(fun=sf.asyCon.fun, colour='black') +
  # stat_function(fun=m16.funD, colour='green') +
  # stat_function(fun=m16.funP, colour='blue') +
#  stat_function(fun=m16.fun(ft='PPDF'), color='blue') +
#  stat_function(fun=m16.fun(ft='Decid'), color='green') +
 # stat_function(fun=sf.crisim.fun, color='purple') +
#  stat_function(fun=sf.crisimCon.fun, colour='pink') +
  theme_classic()
#  stat_function(fun=sf.con.fun, colour='gray') +
#geom_text(x=28, y=sf.isim2sfc075.fun(28)+3, 
#          label='IsaSFC') +



figX +
  geom_point(data=s.fires.mod %>% slice(c(28, 52, 54, 60, 88, 
                                          89, 91)) %>% 
               select(fire, ISI, isi.m, ROS, FT, SFC), shape=2,
             colour='black', size=5) + #triangles - influential to agg mods
  geom_point(data=s.con2 %>% slice(c(10, 28, 51)) %>% 
               select(fire, ISI, isi.m, ROS, FT), shape=5,
             colour='black', size=5) + #diamongs - influ to con mods
  geom_text(x=23, y=m16.fun(23, 'Con')+1, label=lab1, 
            colour='purple', size=4) +
  geom_text(x=24, y=m16.fun(24, 'Con')-1, label=lab2,
            colour='purple') +
  geom_text(x=19, y=m16.fun(19, 'Con')+1, label=lab3,
            colour='pink') +
  labs(x=expression(ISI[sa]), y='ROS (m/min)', size=expression(SFC~(kg/m^{2})), ## ~ is space; ^{2} is superscript 2 for squared term
       colour='Fuel type') +
  theme(legend.position='none')
