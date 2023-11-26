vi_accum = read.csv("C:\\LIDAR\\Suzano\\artigos\\1502_secao_cubagem\\campo\\cubagem_detalhamento_razao_V_vi_acumulado_allsections.csv",sep=",")
vi_accum = data.frame(vi_accum)
head(vi_accum)

#Heighest section top height as Total height 
model2 <- lm(vi_V~I(top_h_sec_relative) + I(top_h_sec_relative^2),data=vi_accum)
f2 <- function(x) model2$coefficient[[1]] + model2$coefficient[[2]]*x + model2$coefficient[[3]]*x^2

library(ggplot2)

viTot_h =  ggplot(vi_accum,aes(x=top_h_sec_relative,y=vi_V,group=1)) + 
            geom_point(alpha=I(1/50), size=1, colour="grey35", fill="grey35") + #
            xlab("Relative height (%)") + ylab("Accumulated stem volume (%)") +
            scale_y_continuous(limits=c(0,100),breaks=c(seq(0,100,10))) +
            scale_x_continuous(limits=c(0,100),breaks=c(seq(0,100,10))) +
            stat_function(fun=f2, colour="black", size=1) +
            theme_bw() +
            theme(axis.title.x = element_text(size = 12, vjust = -0.25),
                  axis.title.y = element_text(size = 12, vjust = 0.75),
                  axis.text.y=element_text(size = 11),
                  axis.text.x=element_text(size = 11, hjust = 1, vjust = 0.9),
                  panel.border = element_rect(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())

ggsave(viTot_h,
       filename="vi_accumulated_relative_height_v2.png",
       path="C:\\LIDAR\\Suzano\\artigos\\1502_secao_cubagem\\figuras\\vi_accumulated\\auxiliary_data",
       width=130, height=80,
       units="mm",
       dpi=300)

x=50
y= model2$coefficient[[1]] + model2$coefficient[[2]]*x + model2$coefficient[[3]]*x^2
rm(x,y)

hist_50 = subset(vi_accum, vi_accum$top_h_sec_relative == 50)
hist(hist_50$h_sec, breaks = seq(0,20,2))
rm(hist_50)

#Not used
#Total height
model <- lm(vi_V~I(total_ht_relative) + I(total_ht_relative^2),data=vi_accum)
f <- function(x) model$coefficient[[1]] + model$coefficient[[2]]*x + model$coefficient[[3]]*x^2

library(ggplot2)

viTot_h = ggplot(vi_accum,aes(x=total_ht_relative,y=vi_V,group=1)) + 
  geom_point(alpha=I(1/50), size=1, colour="grey35", fill="grey35") + #
  xlab("Relative height (%)") + ylab("Accumulated stem volume (%)") +
  scale_y_continuous(limits=c(0,100),breaks=c(seq(0,100,10))) +
  scale_x_continuous(limits=c(0,100),breaks=c(seq(0,100,10))) +
  stat_function(fun=f, colour="black", size=1) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 12, vjust = -0.25),
        axis.title.y = element_text(size = 12, vjust = 0.75),
        axis.text.y=element_text(size = 11),
        axis.text.x=element_text(size = 11, hjust = 1, vjust = 0.9),
        panel.border = element_rect(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(viTot_h,
       filename="vi_accumulated_relative_height.png",
       path="C:\\LIDAR\\Suzano\\artigos\\1502_secao_cubagem\\figuras\\vi_accumulated\\auxiliary_data",
       width=130, height=80,
       units="mm",
       dpi=300)

