# Loading packages, options and initializations ------------------------
if (!require("pacman")) 
  install.packages("pacman", repos='http://cran.us.r-project.org')

p_load("haven", "tidyverse", "twang")

# Read -----------------------------------------------------------------
clean_data <- readRDS(paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/Data/",
                             "nacchrs_harmonized.RDS"))

g1dat <- clean_data %>% 
  zap_formats() %>% zap_labels()


g1.harmonize <- c("agetv", "female", "raceth", "educyr",
               "marital", "htn", "diab", "depsymp", "srmem", 
               "vision_diff","hearing_diff")

harmonize.noraceth <- c("agetv", "female", "educyr",
                  "marital", "htn", "diab", "depsymp", "srmem", 
                  "vision_diff","hearing_diff")

  # look at dataset
  str(g1dat)
  
  g1dat$female <- as.numeric(g1dat$female)
  
  str(g1dat)

# race/ethnicity stratified plot --------------------------------------- 

g1dat$raceth<-as.numeric(g1dat$raceth)

#Run covariate balance with HRS weights
for(i in 1:max(g1dat$raceth)){
  subset <- g1dat %>% filter(raceth == i)
  
  temp <-twang::bal.stat(data=subset, 
               vars=harmonize.noraceth,
               w.all=subset$rwtresp, # HRS sampling weights
               treat.var='hrs_h',  # indicator for study 
               sampw=subset$rwtresp, # HRS sampling weights
               estimand="ATT", 
               multinom = F, 
               na.action='exclude')
  
  temp<-temp$results
  temp$var<-rownames(temp)
  temp$raceth <-  i
  
  if(!exists("all_covbal")){
    all_covbal <- temp
  } else{
    all_covbal <- 
      rbind(all_covbal, temp)
  }

}
 
# var labels
order = c("agetv", "female", "educyr","marital", "htn","diab","depsymp","srmem",
          "vision_diff", "hearing_diff")
labels = c("Age (years)", "Female", "Education (years)",
           "Married/living as married", "Hypertension/high blood pressure*",
           "Diabetes/high blood sugar*",
           "Elevated depressive symptoms","Poor subjective cognition",
           "Vision difficulty*", "Hearing difficulty*")

all_covbal$raceth_plot <- ifelse(all_covbal$raceth==1,'Latinx',
                        ifelse(all_covbal$raceth==2,'non-Latinx White',
                           ifelse(all_covbal$raceth==3,'non-Latinx Black',NA)))
# plot
g1covbal_plot<-all_covbal %>%
  arrange(raceth_plot) %>%
  mutate(raceth_plot = factor(raceth_plot, levels=c("Latinx", "non-Latinx White", 
                                                    "non-Latinx Black"))) %>%
  # mulitply by -1 to get NACC-HRS instead of vice versa
  ggplot(aes(x=var, y=std.eff.sz*-1, group=raceth_plot)) +  
    scale_color_manual(values=c("#D55E00","#009E73", "#56B4E9"))+
    geom_point(aes(shape=raceth_plot, color=raceth_plot), size=5, alpha=0.7) +
    geom_hline(yintercept=0) +
    geom_hline(yintercept=-0.25, linetype="dashed", size=1) + 
    geom_hline(yintercept=0.25, linetype="dashed", size=1) +
    ylab("Standardized mean difference (NACC-HRS)/SD[HRS]") +
    labs(title="NACC & 2010 U.S. Population Ages 60+
Covariate Balance",
         caption="*self-reported")+
    scale_x_discrete(limits=rev(order), labels=rev(labels))+
    scale_y_continuous(limits=c(-0.5, 1.25), breaks= seq(-0.5, 1.25, 0.25)) +
    theme_bw() +
    labs(colour = "Race/Ethnicity", shape='Race/Ethnicity') + 
    theme(plot.title = element_text(hjust = 0, size=12))+
    theme(axis.title.y=element_blank()) + 
    theme(legend.position = "right") +
    theme(aspect.ratio = 5/3) +
    #ylim(-0.5,1.0)+
    coord_flip()

# look at plot
g1covbal_plot

ggsave(g1covbal_plot, file=paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/Output/",
                                  "coval_plot_raceth20220927.jpg"),
       dpi=300, height = 8, width =7)

# covariate balance overall, weighted -----------------------------------
g1dat$raceth <- as.factor(g1dat$raceth)

g1temp_w<-twang::bal.stat(data=g1dat, 
                            vars=g1.harmonize,
                            w.all=g1dat$rwtresp, # HRS sampling weights
                            treat.var='hrs_h',  # indicator for study 
                            sampw=g1dat$rwtresp, # HRS sampling weights
                            estimand="ATT", 
                            multinom = F, 
                            na.action='exclude')

g1covbal_w<-g1temp_w$results
g1covbal_w$var<-rownames(g1covbal_w)

# var labels
order = c("agetv", "female","raceth:1","raceth:2","raceth:3",
          "educyr","marital", "htn","diab","depsymp","srmem",
          "vision_diff", "hearing_diff")
labels = c("Age (years)", "Female", "Latinx", "non-Latinx White", 
           "non-Latinx Black", "Education (years)",
           "Married/living as married", "Hypertension/high blood pressure*",
           "Diabetes/high blood sugar*",
           "Elevated depressive symptoms","Poor subjective cognition",
           "Vision difficulty*", "Hearing difficulty*")

# plot
# mulitply by -1 to get NACC-HRS instead of vice versa 
g1covbal_plot_w<-ggplot(g1covbal_w, aes(x=var, y=std.eff.sz*-1)) + 
  geom_point(size=4, color='#0072B2') +
  geom_hline(yintercept=0) +
  geom_hline(yintercept=-0.25, linetype="dashed", size=1) + 
  geom_hline(yintercept=0.25, linetype="dashed", size=1) +
  ylab("Standardized mean difference (NACC-HRS)/SD[HRS]") +
  labs(title="NACC & 2010 U.S. Population Ages 60+
Covariate Balance", caption="*self-reported")+
  scale_x_discrete(limits=rev(order), labels=rev(labels))+
  scale_y_continuous(limits=c(-0.5, 1.25),
                     breaks= c(-0.5,-0.25,0,0.25, 0.50, 0.75, 1.0, 1.25)) +
  theme_bw() +
  theme(axis.title.y=element_blank()) + 
  theme(plot.title = element_text(hjust = 0))+
  #theme(legend.position = "right") + 
  theme(aspect.ratio = 5/3) + 
  #ylim(-0.5,0.8)+
  coord_flip()

g1covbal_plot_w 

ggsave(g1covbal_plot_w, file=paste0("C:/Users/tmobley/Box/G1N_NACC_HRS/",
                                    "Output/coval_plot_w20220927.jpg"),
       dpi=300 , height = 8, width =7)
