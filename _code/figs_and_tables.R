library(tidyverse)
library(haven)
library(readr)
library(table1)
library(sjlabelled)
library(ggpubr)
library(ggridges)
library(ggsignif)
library(patchwork)
library(expss)
library(FactoMineR)
library(factoextra)
library(RColorBrewer)
library(stringr)
library(stringi)
library(tibble)
library(ggpmisc)
library(readxl)
library(DT)
library(wesanderson)
library(likert)
library(ggrepel)
library(kableExtra)
#library(glmmTMB)
#library(sjPlot)
#library(sjmisc)
#library(jtools)
#library(ggstance)
#library(huxtable)
#library(MASS)
#library(pscl) 

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g),correct = TRUE, simulate.p.value = TRUE, B = 10000)$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


# Data
# data wrap in new_search.rmd

full_set <- read_csv("data/set.csv")

set <- full_set %>% 
  filter(wave == 'wave_1_2'| wave == 'wave_2_5' | wave ==  'wave_2_6') %>% 
  filter(age_int >= 21 & age_int <= 80) %>% 
  filter(type_doj != "Другое")


# Results

## Figure 1: Distribution of women and men by type of organization

set_add <- set %>% select(sex, org_type) %>% 
  mutate(org_type = case_when(org_type == "Ведущий вуз" ~ "Вуз",
                              TRUE ~ org_type)) %>% 
  group_by(sex, org_type) %>% 
  count() %>% ungroup() %>%
  group_by(sex) %>%
  mutate(perc = 100 * n / sum(n))

set %>% select(sex, org_type) %>% 
  group_by(sex, org_type) %>%
  count() %>% ungroup() %>%
  group_by(sex) %>% 
  mutate(perc = 100 * n / sum(n)) %>%
  filter(org_type == "Ведущий вуз") %>%
  rbind(set_add) %>% 
  mutate(org_type = case_when(org_type == "Ведущий вуз" ~ "Top universities\n(in University sector)",
                              org_type == "Вуз" ~ "University sector",
                              org_type == "Научная" ~ "Scientific sector",
                              org_type == "Другое" ~ "Other",
                              TRUE ~ org_type)) %>% 
  mutate(org_type = factor(org_type, 
                           levels = c("University sector", 
                                      "Top universities\n(in University sector)",
                                      "Scientific sector",
                                      "Other"))) %>% 
  ggplot(aes(x = as.factor(org_type), y = perc, group = sex, fill = sex)) + 
  geom_col(position = position_dodge(width = 0.55), width = 0.5, 
           size = 0.4) +
  geom_text(aes(label = paste0(round(perc, 1), "%")),
            position = position_dodge2(width = 0.55), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#ffbf49", "#a8a8a8")) +
  ylim(0, 100) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.1, 
                                          color = "grey60", linetype = "solid"),
        legend.position = "top",
        legend.title = element_blank()) 

ggsave(filename = "_code/fig/fig1.png",
       family = "Helvetica", units = "mm", dpi = 300,  width = 150,
       height = 70)


## Table 1. Distribution of women and men by position


positions <- read_delim("data/positions.csv", delim = ";",
                        escape_double = FALSE, trim_ws = TRUE)

Tab_1 <- set %>% group_by(short_doj_new) %>%
  mutate(short_check = case_when(n() > 40 ~ short_doj_new,
                                 TRUE ~ "другое")) %>%
  left_join(positions, by = c("short_check" = "ru"))

Tab_1 <- table1( ~ en | sex, data = Tab_1)

Tab_1 <- as.data.frame(Tab_1)[-1, ]
Tab_1 <- Tab_1[-1,]
rownames(Tab_1) <- NULL

kable(Tab_1,
      align = c("l", "c", "c", "c"),
      col.names = c("",
                    "Female<br/> (N=1494)",
                    "Male<br/> (N=901)",
                    "Overall<br/> (N=2395)"),
      escape = FALSE) %>%
  kable_styling(full_width = F, position = "center", font_size = 14)



## Figure 2: Professional activities of women and men

what_do <- read_delim("data/what_do.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(-var_ru)

what_do %>% pivot_longer(-var) %>% 
  ggplot(aes(reorder(var,value), value, group = name, fill = name)) +
  geom_col(position = "dodge", 
           color = "white", 
           size = 0.4) + 
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "),
                                                 width = 28)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(values = c("#ffbf49", "#a8a8a8")) +
  theme_bw() + 
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "grey60", linetype = "solid"),
        legend.position = "top",
        legend.title = element_blank()) 

ggsave(filename = "_code/fig/fig2.png",
       family = "Helvetica", units = "mm", dpi = 300,  width = 150,
       height = 100)



## Figure 3: Mentoring

set %>% filter(wave == "wave_1_2") %>% 
  select(1, sex, v_292, v_293, v_295, v_296) %>% 
  rename(BA = v_292, MA = v_293, PhD = v_295, DS = v_296) %>% 
  pivot_longer(-c(1, sex)) %>% group_by(sex, name) %>% 
  summarise(n = n(),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE)) %>%
  mutate(se = sd / sqrt(n))  %>%
  mutate(ic = se * qt((1-0.05)/2 + .5, n-1)) %>% 
  mutate(name = factor(name, levels = c("BA", "MA", "PhD", "DS"))) %>% 
  ggplot() + 
  geom_bar(aes(x = sex, y = mean, fill = sex), 
           stat = "identity") +
  geom_errorbar(aes(x = sex, ymin = mean - ic, ymax = mean + ic), width = 0.1, 
                colour = "black", alpha = 0.9, size = 0.3) +
  facet_wrap(~ name, scales = "free", ncol = 4) +
  scale_fill_manual(values = c("#ffbf49", "#a8a8a8")) +
  labs(y = "mean", x = "") +
  theme_bw() + 
  theme(axis.title = element_blank(),
        strip.background = element_rect(fill = "NA"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.1, color = "grey60", linetype = "solid"),
        legend.position = "none",
        legend.title = element_blank()) 

ggsave(filename = "_code/fig/fig3.png",
       family = "Helvetica", units = "mm", dpi = 300,  width = 150,
       height = 40)



## Table 2: Children 

Tab_1 <-  table1( ~ v_299_2 + v_299 | sex,
                  data = set,
                  overall = FALSE)

Tab_1 <- as.data.frame(Tab_1)[-1,]
Tab_1[1, 1] <- "Count of children"
Tab_1[7, 1] <- " "
rownames(Tab_1) <- NULL

kable(Tab_1,
      align = c("l", "c", "c"),
      col.names = c("Field", "Female<br/> (N=1494)", "Male<br/> (N=901)"),
      escape = FALSE) %>% 
  kable_styling(full_width = F,
                position = "center",
                font_size = 14)



## Figure 4. The ratio of the age of respondents to the age of the first child 
# (A) and age of Ph.D. thesis defense (B)

p1 <- set %>% select(sex, age_int, first_ch_age) %>% 
  filter(age_int >= 22 & age_int <= 65) %>% 
  ggplot(aes(age_int, first_ch_age, group = sex, color = sex)) +
  geom_smooth(method = "lm", alpha = 0.2)  +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01,
           cor.coef.name = c("corr_per"), size = 3,
           label.y.npc = 0.54, label.x = 35, show.legend = FALSE) +
  scale_color_manual(values = c("#ffae00", "gray56")) +
  labs(title = "(А)", x = "Age of respondent", 
       y = "Age at which the first child appeared", 
       color = " ", linetype = " ") +
  theme_test() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

p2 <- set %>% select(sex, age_int, kan_age) %>% 
  filter(age_int >= 22 & age_int <= 65) %>% 
  ggplot(aes(age_int, kan_age, group = sex, color = sex)) +
  geom_smooth(method = "lm", alpha = 0.2)  +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01,
           cor.coef.name = c("corr_per"), size = 3,
           label.y.npc = 0.54, label.x = 35, show.legend = FALSE) +
  scale_color_manual(values = c("#ffae00", "gray56")) +
  labs(title = "(B)", 
       x = "Age of respondent", y = "Age of PhD defense", 
       color = " ", linetype = " ") +
  theme_test() + 
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = 'top')

ggsave(filename = "_code/fig/fig4.png",
       family = "Helvetica", units = "mm", dpi = 600,  width = 170,
       height = 100)



## Figure 5: How the Ph.D. defense and the birth of the first child are separated in time

p <- set %>% filter(v_299_2 == 0 | v_299_2 == 1 | v_299_2 == 2) %>% 
  mutate(kan_afrer_child = kan_age - first_ch_age) %>% 
  mutate(kan_afrer_child = as.numeric(as.character(kan_afrer_child))) %>% 
  filter(!is.na(kan_afrer_child)) %>% 
  select(sex, kan_afrer_child) %>% 
  filter(kan_afrer_child > -14 & kan_afrer_child < 30) %>% 
  ggplot(aes(kan_afrer_child, group = sex, fill = sex)) +
  geom_density(alpha = 0.6, 
               position = 'identity', 
               size = 0.2) +
  geom_vline(xintercept = 0, color = "red", size = 0.4, linetype = "solid") +
  scale_fill_manual(values = c("#ffbf49", "#a8a8a8"), guide = FALSE) +
  labs(fill = "", x = "", y = "Density") +
  theme_test() + 
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size=9),
        axis.title.y = element_text(size=10)) 

data.tb <- tibble(x = 30, y = 60, 
                  plot = list(p + theme_test(8) + 
                                theme(legend.position = 'none',
                                      plot.background = element_rect(fill = "NA"),
                                      axis.text.y = element_blank(),
                                      axis.ticks.y = element_blank(),
                                      axis.text.x = element_text(size = 9),
                                      axis.title.y = element_text(size = 10))))

set %>% filter(v_299_2 == 0 | v_299_2 == 1 | v_299_2 == 2) %>% 
  mutate(kan_afrer_child = kan_age - first_ch_age) %>% 
  mutate(kan_afrer_child = as.numeric(as.character(kan_afrer_child))) %>% 
  filter(!is.na(kan_afrer_child)) %>% 
  select(sex, kan_afrer_child) %>% 
  filter(kan_afrer_child > -14 & kan_afrer_child < 30) %>% 
  
  ggplot(aes(kan_afrer_child, group = sex, fill = sex)) +
  geom_plot(data = data.tb, aes(x, y, label = plot)) +
  geom_bar(position = position_dodge(preserve = "single"), 
           color = "white", size = 0.3) +
  geom_vline(xintercept = 0, color = "red",
             linetype = "solid", size = 0.5) +
  scale_fill_manual(values = c("#ffbf49", "#a8a8a8")) +
  scale_x_continuous(breaks = c(-15, -10, -5, 0, 5, 10, 15, 20, 25, 30)) +
  labs(fill = "", 
       x = "PhD in N years after the appearance of the first child", 
       y = "Number of respondents") +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "NA"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) 

ggsave(filename = "_code/fig/fig5.png",
       family = "Helvetica", units = "mm", dpi = 300,  width = 150,
       height = 100)




## Figure 6: Gender, parenthood, and self-definition as "more of a teacher" or "more of a scientist

values = rev(wes_palette("Zissou1", 5, type = "continuous"))
values[3] <- "grey88"

# fig A
items <- set %>% 
  select(sex, v_288) %>% filter(v_288 < 6) %>% 
  mutate(v_288 = as.character(v_288)) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = sex,values_from = v_288) %>% 
  column_to_rownames(var = 'id') %>% 
  mutate(Female = factor(Female, 
                         levels = c("1", "2", "3", "4", "5"),
                         labels = c("1 - rather\nа researcher", "2", "3", "4", 
                                    "5 - rather\na teacher"))) %>% 
  mutate(Male = factor(Male, 
                       levels = c("1", "2", "3", "4", "5"),
                       labels = c("1 - rather\nа researcher", "2", "3", "4", 
                                  "5 - rather\na teacher"))) 
lik <- likert(items)
liks <- likert(summary = lik$results)

update_geom_defaults("hline", list(colour="black", size=0.5))
# update_geom_defaults("bar", list(colour="black", size=0.5))

x1 <- plot(liks, text.size = 4) + 
  geom_hline(yintercept = 0, color = "grey60")+
  labs(y = "% respondents") +
  scale_y_continuous(labels = likert:::abs_formatter, lim = c(-105, 105),
                     breaks = seq(-100, 100, 25))  +
  # scale_fill_brewer(palette = "PRGn") + 
  scale_fill_manual(values = values) +
  labs(title = "(A)") +
  theme_test()+
  theme(legend.title = element_blank(),
        legend.position = "top", 
        text = element_text(size = 11))
# fig B
items <- set %>% select(sex, v_288, v_299_2) %>% filter(v_288 < 6) %>% 
  mutate(v_288 = as.character(v_288)) %>% 
  filter(!is.na(v_299_2) & !is.na(v_288)) %>% 
  filter(v_299_2 != "3 и >") %>% 
  mutate(v_299_2 = case_when(v_299_2 == "0" ~ "No children",
                             v_299_2 == "1" ~ "One child",
                             TRUE ~ "Two children")) %>% 
  mutate(id = row_number()) %>% 
  pivot_wider(names_from = sex, values_from = v_288) %>% 
  column_to_rownames(var = 'id') %>% 
  mutate(Female = factor(Female, 
                         levels = c("1", "2", "3", "4", "5"),
                         labels = c("1 - rather\nа researcher", "2", "3", "4", 
                                    "5 - rather\na teacher"))) %>% 
  mutate(Male = factor(Male, 
                       levels = c("1", "2", "3", "4", "5"),
                       labels = c("1 - rather\nа researcher", "2", "3", "4", 
                                  "5 - rather\na teacher"))) %>% 
  mutate(v_299_2 = factor(v_299_2, 
                          levels = c("Two children", "One child", "No children"))) 

lik <- likert(items[,c(2:3)], grouping = items$v_299_2)

theme_update(legend.text = element_text(size = rel(0.7)),
             axis.text.y = element_text(hjust = 0))

update_geom_defaults("hline", list(colour="grey90", size = 0.5))
# update_geom_defaults("bar", list(colour="black", size = 0.5))

x2 <- plot(lik, text.size = 4) + 
  geom_hline(yintercept = 0, color = "grey60") +
  labs(y = "% respondents") +
  scale_y_continuous(labels = likert:::abs_formatter, lim = c(-105, 105),
                     breaks = seq(-100, 100, 25))  +
  # scale_fill_brewer(palette = "PRGn") + 
  scale_fill_manual(values = values) +
  labs(title = "(B)") +
  theme_test()+
  theme(legend.title = element_blank(),
        legend.position = "none", 
        text = element_text(size = 11),
        strip.background = element_rect(fill = "NA"))

# fig A + B
x1 / x2 + plot_layout(heights = unit(c(1.7, 2), c('cm', 'null')))

ggsave(filename = "_code/fig/fig6.png", 
       family = "Helvetica", units = "mm", dpi = 300,  width = 190,
       height = 130)




# Conclusions

## Figure 7: 

gender <- read_delim("data/gender.csv", delim = ";", 
                     escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)

gend_pers <- read_delim("data/gend_pers.csv", 
                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

set2 <- full_set %>% select(sex, v_358:v_361) %>% 
  filter(!if_all(v_358:v_361, is.na)) %>% 
  pivot_longer(-sex, "name", "value") %>% 
  filter(!is.na(value)) %>% 
  left_join(gender,by = c("name" = "X1")) %>% 
  select(-name) %>% 
  rename(name = X2)
set2 <- set2 %>% left_join(gend_pers,by = c("value" = "X1"))

set2 %>% filter(!is.na(X2)) %>% 
  mutate(X2 = factor(X2, 
                     levels = c("0%", 
                                "1-10%", "11-20%", "21-30%", 
                                "31-40%", "41-50%", "51-60%", "61-70%",
                                "71-80%", "81-90%", "91-99%", 
                                "На 100%"),
                     labels = c("0%", 
                                "1-10%", "11-20%", "21-30%", 
                                "31-40%", "41-50%", "51-60%", "61-70%", 
                                "71-80%", "81-90%", "90-99%", 
                                "100%"))) %>% 
  ggplot(aes(sex, fill = forcats::fct_rev(as.factor(X2))))+
  geom_bar(stat = "count", position = "fill", color = NA) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rev(wes_palette("Zissou1", 12, 
                                             type = "continuous")))+
  coord_flip() + 
  
  guides(fill = guide_legend(nrow = 1, 
                             byrow = TRUE, reverse = T, 
                             label.position = "top")) +
  facet_grid(name ~ ., labeller = label_wrap_gen(width = 20)) +  
  theme_bw() + 
  theme(axis.title = element_blank(),
        strip.background = element_rect(fill = "NA"),
        strip.text.y = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 7, angle = 45, vjust = 0.5),
        legend.title = element_blank()) 

ggsave(filename = "_code/fig/fig7.png", 
       family = "Helvetica", units = "mm", dpi = 300,  width = 190,
       height = 110)

