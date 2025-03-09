##file for preparation of the datasets


##exercise 1c)

library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(forcats)
library(readr)
library(ggplot2)
library(stargazer)
library(sandwich)

datafig1 <- read_csv("main_work.csv")
mainWork <- read_csv("main_work.csv")

datafig1.1 <- datafig1 %>%
  mutate(
    share_a1_initial = share_pointssaving1,
    share_a2_initial = share_pointssaving2,
    share_a3_initial = if_else(treatment != 0, share_pointsdebt1, share_pointssaving3),
    share_a4_initial = if_else(treatment != 0, share_pointsdebt2, share_pointssaving4),
    auxiliar11 = NA_real_ 
  )

datafig1.2 <- datafig1.1 %>%
  mutate(
    treatmentLabel = case_when(
      treatment == 1 ~ "Low Debt",
      treatment == 0 ~ "No Debt",
      TRUE ~ "."
    )
  )


plotdata1 <- datafig1.2 %>%
  filter(treatment <= 1, day == 1) %>%
  select(treatmentLabel, share_a1_initial, share_a2_initial, share_a3_initial, share_a4_initial, auxiliar11) %>%
  pivot_longer(cols = starts_with("share_a"), names_to = "share", values_to = "value")


plotdata1$treatmentLabel <- fct_relevel(plotdata1$treatmentLabel, "No Debt", "Low Debt")


NODEBTa1<-plotdata1 %>% filter(plotdata1$treatmentLabel=="Low Debt")
NoDebta1.1<-NODEBTa1%>% filter(NODEBTa1$share=="share_a4_initial")
mean(NoDebta1.1$value)


plotfigure1 <- plotdata1 %>%
  mutate(share = factor(share, levels = c("share_a1_initial", "share_a2_initial", "share_a3_initial", "share_a4_initial"),
                        labels = c("Savings 1 (20%)", "Savings 2 (10%)", "Savings 3 / Debt 1 (15%)", "Savings 4 / Debt 2 (5%)"))) %>%
  group_by(treatmentLabel, share) %>%
  summarise(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

#figure 3 (1e&2b) 

mainWork_tot <- mainWork%>%
  group_by(idturk) %>%
  mutate(across(starts_with("points_ia_a"), ~ sum(.x, na.rm = TRUE), .names = "tot_{.col}"),
         across(starts_with("points_aa_a"), ~ sum(.x, na.rm = TRUE), .names = "tot_{.col}")) %>%
  ungroup()

mainWork_tot <- mainWork_tot %>%
  mutate(across(starts_with("tot_points_ia_a"), 
                ~ .x + get(gsub("tot_points_ia", "tot_points_aa", cur_column())), 
                .names = "{gsub('tot_points_ia_', 'tot_points_', .col)}"))


#fig 3 teil 2 verbessert 

##verbesserung code 3.2
library(ggplot2)
library(dplyr)
library(scales)  



data_plotfig3.1 <- mainWork_tot%>%
  filter(treatment<=1)%>%
  group_by(treatment, tot_points_a4) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(percent = count / sum(count) * 100) %>%  
  mutate(percent = ifelse(percent == 0, NA, percent)) %>%  
  mutate(treatment_label = factor(treatment, levels = c(0, 1), labels = c("No Debt", "Low Debt")))  


highlight_pointsfig3.1 <- data_plotfig3.1 %>%
  filter((tot_points_a4 == 0) | (treatment == 1 & tot_points_a4 == 1500)) 


highlight_labelsfig3.1 <- highlight_pointsfig3.1 %>%
  mutate(label = paste0(tot_points_a4, " points\n", round(percent, 2), "%"))  


p3.2 <- ggplot(data_plotfig3.1, aes(x = tot_points_a4, y = percent, 
                                    color = treatment_label, group = treatment_label,  
                                    text = paste("Treatment:", treatment_label, "<br>",
                                                 "Points:", tot_points_a4, "<br>",
                                                 "Percentage:", round(percent, 2), "%"))) +
  geom_line(size = 1) + 
  geom_point(data = highlight_pointsfig3.1, aes(x = tot_points_a4, y = percent, color = treatment_label),  
             size = 2.5, stroke = 0.8) +  # Punkte jetzt in der Treatment-Farbe
  geom_label(data = highlight_labelsfig3.1, aes(x = tot_points_a4, y = percent * 1.1, label = label),  
             fill = "white", color = "black", size = 3.5, fontface = "bold", hjust = -0.1) +  
  scale_y_log10(labels = scales::percent_format(scale = 1)) +  
  scale_x_continuous(limits = c(0, 2800), breaks = seq(0, 2500, 500)) +
  scale_color_manual(values = c("No Debt" = "blue", "Low Debt" = "maroon")) +  
  labs(
    x = "Points Allocated Savings 4 (5%) / Debt 2 (5%)",
    y = "Percentage of Subjects (log scale)",
    color = "Treatment"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 1500, linetype = "dashed", color = "black", linewidth = 0.8) +  
  theme(legend.position = "bottom")



library(ggplot2)
library(dplyr)
library(scales)  
library(plotly) 


data_plot4.1 <- mainWork_tot %>%
  filter(treatment %in% c(1, 2)) %>%  
  group_by(treatment, tot_points_a3) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(percent = count / sum(count) * 100) %>%  
  mutate(percent = ifelse(percent == 0, NA, percent)) %>%  
  mutate(treatment_label = factor(treatment, levels = c(1, 2), labels = c("Low Debt", "High Debt")))  


highlight_points4.1 <- data_plot4.1 %>%
  filter((tot_points_a3 == 0) | (treatment == 1 & tot_points_a3 == 900) | 
           (treatment == 2 & tot_points_a3 == 900) | (treatment == 2 & tot_points_a3 == 2900))  



highlight_labels4.1 <- highlight_points4.1 %>%
  mutate(label = paste0(tot_points_a3, " points\n", round(percent, 2), "%"))  


p4.1 <- ggplot(data_plot4.1, aes(x = tot_points_a3, y = percent, 
                                 color = treatment_label, group = treatment_label,  
                                 text = paste("Treatment:", treatment_label, "<br>",
                                              "Points:", tot_points_a3, "<br>",
                                              "Percentage:", round(percent, 2), "%"))) +
  geom_line(size = 1) +  
  geom_point(data = highlight_points4.1, aes(x = tot_points_a3, y = percent, color = treatment_label),  
             size = 2.5, stroke = 0.8) +  
  geom_label(data = highlight_labels4.1, aes(x = tot_points_a3, y = percent * 1.1, label = label),  
             fill = "white", color = "black", size = 3.5, fontface = "bold", hjust = -0.1, label.size = 0.25) + 
  scale_y_log10(labels = scales::percent_format(scale = 1)) + 
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500)) +
  scale_color_manual(name = "Treatment",  
                     values = c("Low Debt" = "maroon", "High Debt" = "darkgreen")) + 
  labs(
    x = "Points Allocated Debt 1 (15%)",
    y = "Percentage of Subjects (log scale)"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 900, linetype = "dashed", color = "black", linewidth = 0.8) +  
  theme(legend.position = "bottom")



##figure 4 teil 2
#verbesserung plot 4.2

library(ggplot2)
library(dplyr)
library(scales)  
library(plotly)  


data_plot4.2 <- mainWork_tot %>%
  filter(treatment %in% c(1, 2)) %>%  
  group_by(treatment, tot_points_a4) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(treatment) %>%
  mutate(percent = count / sum(count) * 100) %>% 
  mutate(percent = ifelse(percent == 0, NA, percent)) %>% 
  mutate(treatment_label = factor(treatment, levels = c(1, 2), labels = c("Low Debt", "High Debt"))) 


highlight_points4.2 <- data_plot4.2 %>%
  filter((tot_points_a4 == 0) | (treatment == 1 & tot_points_a4 == 1500) | 
           (treatment == 2 & tot_points_a4 == 1500))  



highlight_labels4.2 <- highlight_points4.2 %>%
  mutate(label = paste0(tot_points_a4, " points\n", round(percent, 2), "%")) 


p4.2 <- ggplot(data_plot4.2, aes(x = tot_points_a4, y = percent, 
                                 color = treatment_label, group = treatment_label,  
                                 text = paste("Treatment:", treatment_label, "<br>",
                                              "Points:", tot_points_a4, "<br>",
                                              "Percentage:", round(percent, 2), "%"))) +
  geom_line(size = 1) +  
  geom_point(data = highlight_points4.2, aes(x = tot_points_a4, y = percent, color = treatment_label),  
             size = 2.5, stroke = 0.8) +
  geom_label(data = highlight_labels4.2, aes(x = tot_points_a4, y = percent * 1.1, label = label),  
             fill = "white", color = "black", size = 3.5, fontface = "bold", hjust = -0.1, label.size = 0.25) + 
  scale_y_log10(labels = scales::percent_format(scale = 1)) + 
  scale_x_continuous(limits = c(0, 2800), breaks = seq(0, 2500, 500)) +
  scale_color_manual(name = "Treatment",  
                     values = c("Low Debt" = "maroon", "High Debt" = "darkgreen")) +
  labs(
    x = "Points Allocated Debt 2 (5%)",
    y = "Percentage of Subjects (log scale)"
  ) +
  theme_minimal() +
  geom_vline(xintercept = 1500, linetype = "dashed", color = "black", linewidth = 0.8) + 
  theme(legend.position = "bottom")







#fig6  (ex. 3a)) fix:
library(dplyr)
library(ggplot2)
library(tidyr)
library(haven)


datafig6 <- read_csv("redistribution_work.csv") %>%
  filter(day > 0)  


datafig6 <- datafig6 %>%
  mutate(treatment_label = factor(treatment, levels = c(0, 1),
                                  labels = c("Redistribution No-Debt", "Redistribution Debt")))


datafig6 <- datafig6 %>%
  mutate(
    returns = round(
      coalesce(balance_a1, 0) * 0.2 + 
        coalesce(balance_a2, 0) * 0.1 + 
        coalesce(balance_a3, 0) * 0.15 +
        coalesce(balance_a4, 0) * 0.05 +
        if_else(treatment == 1, 
                coalesce(balance_a5, 0) * 0.15 + coalesce(balance_a6, 0) * 0.05, 
                -coalesce(balance_a5, 0) * 0.15 - 2200 * 0.05), 
      1)
  )


datafig6 <- datafig6 %>%
  group_by(idturk) %>%
  arrange(day) %>%
  mutate(cum_returns = cumsum(replace_na(returns, 0))) %>%  
  ungroup()


df_cumul6 <- datafig6 %>%
  filter(day == 4, !is.na(cum_returns)) %>%
  group_by(treatment_label) %>%
  arrange(cum_returns) %>%
  mutate(prob = (row_number() - 1) / (n() - 1)) %>%
  select(treatment_label, cum_total_returns = cum_returns, prob)  



ggplot(df_cumul6, aes(x = cum_total_returns, y = prob, color = treatment_label)) +
  geom_line(size = 0.9) +
  scale_color_manual(
    values = c("Redistribution No-Debt" = "#1f497d",  
               "Redistribution Debt" = "#a52a2a") 
  ) +
  scale_x_continuous(breaks = seq(1300, max(df_cumul6$cum_total_returns, na.rm = TRUE), 500)) +  
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +  
  labs(
    x = "Total returns",
    y = "Pr (total returns < x)",
    color = "Treatment"  
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12)
  ) +
  geom_vline(xintercept = 2263, linetype = "dashed", color = "black")  

##borrowing work (ex 3b)): 
library(dplyr)
library(ggplot2)
library(readr)

#borrow treatment alle vs. alle die ertrag maximierten:
#wieviele liehen den gesamt verfügbaren betrag?

data_cleaned <- read_csv("borrowing_work.csv")


reg1 <- lm(borrow_max_both ~ factor(treatment), data = data_cleaned %>% filter(day == 4))
summary(reg1)


reg2 <- lm(borrow_max_both ~ factor(treatment), data = data_cleaned %>% filter(day == 4 & ind_optimal_ia_all == 1))
summary(reg2)


summary_all <- data_cleaned %>%
  filter(day == 4) %>%
  mutate(counter = 1) %>%
  group_by(treatment) %>%
  summarise(
    mean_borrow_max_both = mean(borrow_max_both, na.rm = TRUE),
    sd_borrow_max_both = sd(borrow_max_both, na.rm = TRUE),
    n = sum(counter)
  ) %>%
  mutate(optimal = 0)

summary_optimal <- data_cleaned %>%
  filter(day == 4 & ind_optimal_ia_all == 1) %>%
  mutate(counter = 1) %>%
  group_by(treatment) %>%
  summarise(
    mean_borrow_max_both = mean(borrow_max_both, na.rm = TRUE),
    sd_borrow_max_both = sd(borrow_max_both, na.rm = TRUE),
    n = sum(counter)
  ) %>%
  mutate(optimal = 1)


summary_combined <- bind_rows(summary_all, summary_optimal)


summary_combined <- summary_combined %>%
  mutate(
    hiwrite = case_when(
      treatment == 1 & optimal == 0 ~ (0.625 - 0.2835366) + (0.4331979 - 0.2835366),
      treatment == 1 & optimal == 1 ~ (0.9615385 - 0.5043956) + (0.6921199 - 0.5043956),
      TRUE ~ NA_real_
    ),
    lowrite = case_when(
      treatment == 1 & optimal == 0 ~ (0.625 - 0.2835366) - (0.2835366 - 0.1338753),
      treatment == 1 & optimal == 1 ~ (0.9615385 - 0.5043956) - (0.5043956 - 0.3166713),
      TRUE ~ NA_real_
    ),
    spacing = case_when(
      treatment == 0 & optimal == 0 ~ 0.2,
      treatment == 1 & optimal == 0 ~ 0.525,
      treatment == 0 & optimal == 1 ~ 1.8,
      treatment == 1 & optimal == 1 ~ 2.125,
      TRUE ~ NA_real_
    )
  )


# Erstellung des Barplots
plotb<-ggplot(summary_combined, aes(x = spacing, y = mean_borrow_max_both, fill = factor(treatment))) +
  geom_bar(stat = "identity", width = 0.3, position = position_dodge(width = 0.3)) +
  scale_fill_manual(values = c("0" = "blue", "1" = "maroon"), labels = c("Borrow Savings", "Borrow Debt")) +
  geom_errorbar(aes(ymin = lowrite, ymax = hiwrite), width = 0.2, color = "black", alpha = 0.5) +
  labs(
    y = "Percent of Subjects",
    x = "",
    fill = NULL
  ) +
  scale_x_continuous(
    breaks = c(0.3625, 1.9625),
    labels = c("All Subjects", "Only Max Returns"),
    limits = c(-0.1, 2.5)
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  annotate("text", x = 0.2, y = 0.67, label = "63") +
  annotate("text", x = 0.525, y = 0.38, label = "34") +
  annotate("text", x = 1.8, y = 1.0, label = "96") +
  annotate("text", x = 2.125, y = 0.50, label = "46")





##Tabelle 1 (exercise 1a))

# Pakete laden
library(kableExtra)
library(dplyr)

# Daten vorbereiten
table_1 <- tibble::tribble(
  ~"",             ~"Savings 1", ~"Savings 2", ~"Savings 3", ~"Savings 4", ~"Debt 1", ~"Debt 2",
  "Panel A. No-Debt",   "",        "",        "",        "",        "",    "",
  "Interest rate (%)",   "20",      "10",      "15",      "5",       "15",   "5",
  "Balance",            "1100",    "700",     "900",    "1500",     "0",    "0",
  "Panel B. Low-Debt",   "",        "",        "",        "",        "",    "",
  "Interest rate (%)",   "20",      "10",      "15",      "5",       "15",   "5",
  "Balance",            "1100",    "700",     "-900",   "-1500",    "1800", "3000"
)

# Tabelle mit kableExtra formatieren
table_1<-table_1 %>%
  kbl(
    caption = "TABLE 1 — ACCOUNTS IN MAIN TREATMENTS",
    format = "html",
    align = "c"
  ) %>%
  kable_styling(
    full_width = F, 
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  row_spec(0, bold = TRUE, align = "center") %>%  
  row_spec(c(1, 4), bold = TRUE, align = "center", background = "#f2f2f2") %>% 
  column_spec(1, bold = TRUE, width = "20em") %>%  
  add_header_above(c(" " = 1, " " = 6), line = T, bold = TRUE) %>%
  footnote(
    general = "Table 1: Accounts in Main Treatments (Reprinted from Martínez-Marquina, A., & Shi, M., 2024, p. 1147) "
  )%>%# Linie unter der Überschrift
  row_spec(3, hline_after = TRUE) %>% 
  row_spec(6, hline_after = TRUE)    
table_1



#Tabelle 2 als R code

table_2 <- tibble::tribble(
  ~"",             ~"Savings 1", ~"Savings 2", ~"Debt 1", ~"Debt 2", ~"Savings 3", ~"Savings 4",
  "Interest rate (%)",   "20",      "10",      "15",      "5",       "15",         "5",
  "Balance",            "1100",    "700",    "-2900",   "-3500",    "3800",      "5000"
)


table_2 <- table_2 %>%
  kbl(
    caption = "TABLE 2 — ACCOUNTS IN HIGH-DEBT TREATMENT",
    format = "html",
    align = "c"
  ) %>%
  kable_styling(
    full_width = F, 
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  row_spec(0, bold = TRUE, align = "center") %>%  
  column_spec(1, bold = TRUE, width = "15em") %>%  
  add_header_above(c(" " = 1, "Accounts in High-Debt Treatment" = 6), line = T, bold = TRUE)%>%
  footnote(
  general = "Table 2: Accounts in High-Debt Treatment (Reprinted from Martínez-Marquina, A., & Shi, M., 2024, p. 1148) "
) 


#Tabelle 3 als R code


table_3 <- tibble::tribble(
  ~"",                    ~"Savings 1", ~"Savings 2", ~"Debt 1", ~"Debt 2", ~"Savings 3", ~"Savings 4",
  "Panel A. Redistribution Debt", "", "", "", "", "", "",
  "Interest rate (%)",    "20",      "10",      "15",      "5",       "15",         "5",
  "Balance",             "2,000 (0)", "400 (0)", "-600", "-4,300", "300", "6,400",
  "",                    "", "", "", "", "", "",  
  "Panel B. Redistribution No-Debt", "", "", "", "", "", "",
  "Interest rate (%)",    "20",      "10",      "15",      "5",       "15",         "5",
  "Balance",             "2,000 (0)", "400 (0)", "600", "4,300", "-900", "-2,200"
)


table_3 <- table_3 %>%
  kbl(
    caption = "TABLE 3 — ACCOUNTS IN REDISTRIBUTION TREATMENTS",
    format = "html",
    align = "c"
  ) %>%
  kable_styling(
    full_width = F, 
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  row_spec(0, bold = TRUE, align = "center") %>%  
  row_spec(c(1, 5), bold = TRUE, align = "center", background = "#f2f2f2") %>% 
  column_spec(1, bold = TRUE, width = "20em") %>%  
  add_header_above(c(" " = 1, " " = 6), line = T, bold = TRUE) %>%
  footnote(
    general = c("Minimum balance requirement in parentheses.",
      "Table 3: Accounts in Redistribution Treatments (Reprinted from Martínez-Marquina, A., & Shi, M., 2024, p. 1149)")
  ) %>%
  row_spec(3, hline_after = TRUE) %>%  
  row_spec(7, hline_after = TRUE)     

# Tabelle 4 als R code:


table_4 <- tibble::tribble(
  ~"",              ~"Savings 1", ~"Savings 2", ~"Savings 3", ~"Savings 4", ~"Debt 1",   ~"Debt 2",
  "Panel A. Borrow-Debt", "", "", "", "", "", "",
  "Interest rate (%)",   "20", "10", "15", "5", "15", "5",
  "Balance",            "1,100", "700", "900", "1,500", "0 (-900)", "0 (-1,500)",
  "Panel B. Borrow-Savings", "", "", "", "", "", "",
  "Interest rate (%)",   "20", "10", "15", "5", "15", "5",
  "Balance",            "1,100", "700", "0", "0", "900 (0)", "1,500 (0)"
)

table_4 <- table_4 %>%
  kbl(
    caption = "TABLE 4 — ACCOUNTS IN BORROWING TREATMENTS",
    format = "html",
    align = "c"
  ) %>%
  kable_styling(
    full_width = F, 
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  row_spec(0, bold = TRUE, align = "center") %>%  
  row_spec(c(1, 4), bold = TRUE, align = "center", background = "#f2f2f2") %>% 
  column_spec(1, bold = TRUE, width = "20em") %>%  
  add_header_above(c(" " = 1, " " = 6), line = T, bold = TRUE) %>% 
  footnote(
    general = c("Minimum balance (i.e., borrowing caps) requirement in parentheses.",
                "Table 4: Accounts in Borrowing Treatments (Reprinted from Martínez-Marquina, A., & Shi, M., 2024, p. 1150)")
  ) %>%
  row_spec(3, hline_after = TRUE) %>% 
  row_spec(6, hline_after = TRUE)    




##statistical preparations:


##fig1: überprüfung wert grafik 1 per reg


data_reg1 <- mainWork


data_reg1$share_a1_initial <- data_reg1$share_pointssaving1


model_reg1 <- lm(share_a1_initial ~ treatment, data = data_reg1[data_reg1$day == 1 & data_reg1$treatment <= 1, ])


stargazer(model_reg1,type="text",digits= 7)


#fig1 aanalog share zu account 3 wo weitere größere abweichungen sind

# Daten laden
data_reg2 <- mainWork


data_reg2$share_a3_initial <- data_reg2$share_pointssaving3


data_reg2$share_a3_initial[data_reg2$treatment != 0 & !is.na(data_reg2$share_pointsdebt1)] <- 
  data_reg2$share_pointsdebt1[data_reg2$treatment != 0 & !is.na(data_reg2$share_pointsdebt1)]


model_reg2 <- lm(share_a3_initial ~ treatment, data = subset(data_reg2, day == 1 & treatment <= 1))


stargazer(model_reg2, type="text",digits= 7)

##aussage: low debt subjects allocate 18 percentage points more to debt account with highest interest rate

#fig 1: aussage returns von initial allocation are 9.9 percent lower for low debt sowie prozentuale returns nach tag 1

library(haven)
library(stargazer)
library(lfe)

data_reg4 <- mainWork

data_reg4$avg_rate <- 0.20 * (data_reg4$pointssaving1 / data_reg4$initialendowment) + 
  0.10 * (data_reg4$pointssaving2 / data_reg4$initialendowment) + 
  0.15 * (data_reg4$pointssaving3 / data_reg4$initialendowment) + 
  0.05 * (data_reg4$pointssaving4 / data_reg4$initialendowment) + 
  0.15 * (data_reg4$pointsdebt1 / data_reg4$initialendowment) + 
  0.05 * (data_reg4$pointsdebt2 / data_reg4$initialendowment)


model_reg4 <- lm(avg_rate ~ treatment, data = subset(data_reg4, day == 1 & treatment <= 1))


stargazer(model_reg4, type="text", digits=7)


summary_avg_rate_by_treatment <- tapply(data_reg4$avg_rate, data_reg4$treatment, function(x) summary(x[data_reg4$day == 1 & data_reg4$treatment <= 1]))


data_reg4$ln_avg_rate <- log(data_reg4$avg_rate)


model_reg4.1 <- lm(ln_avg_rate ~ treatment, data = subset(data_reg4, day == 1 & treatment <= 1))


stargazer(model_reg4.1,type="text", digits=7)


#fig 2: betrachtung über alle 4 tage hinweg stats 38 prozent no debt und 13 low debt

library(haven)
library(stargazer)
library(lfe)
library(estimatr)
library(modelsummary)


data_main1403 <- mainWork

data_filtered <- subset(data_main1403, day == 4 & treatment <= 1)


summary_by_treatment <- tapply(data_filtered$ind_optimal_ia_all, data_filtered$treatment, summary)


print(summary_by_treatment)


model_reg5.1 <- lm(ind_optimal_ia_all ~ factor(treatment), data = data_filtered)
 
summary(model_reg5.1, robust = TRUE)

data_filtered$treatment <- as.factor(data_filtered$treatment)


model_reg5.2 <- lm(ind_optimal_ia_all ~ treatment + demo_age_median + demo_sex + demo_white + demo_collegeplus + demo_studentloan + demo_holddebt + factor(demo_covid) + error_count_total, 
                     data = data_filtered)



##fig 3: 


data1601 <- mainWork


vars <- c("points_ia_a3", "points_aa_a3")

for (var in vars) {
  new_var <- paste0("tot_", var)
  data1601 <- data1601 %>%
    group_by(idturk) %>%
    mutate(!!new_var := sum(!!sym(var), na.rm = TRUE))
}


data1601 <- data1601 %>%
  mutate(tot_points_a3 = tot_points_ia_a3 + tot_points_aa_a3)


data1601 <- data1601 %>%
  mutate(no_points_a3 = (tot_points_a3 == 0))


data1601 <- data1601 %>% filter(day == 4)

data1601<-data1601%>%filter(treatment<=1)


data_summary <- data1601 %>%
  group_by(treatment) %>%
  summarise(
    no_points_a3_mean = mean(no_points_a3, na.rm = TRUE),
    no_points_a3_sum = sum(no_points_a3, na.rm = TRUE),
    count = n()
  )

print(data_summary)


model1601 <- lm(no_points_a3 ~ treatment, data = data1601)
stargazer(model1601, type="text", digits=7)



#fig3.1: wieviele investieren 0 in a4


data1602 <- mainWork


data1602 <- data1602 %>%
  group_by(idturk) %>%
  mutate(
    tot_points_ia_a4 = sum(points_ia_a4, na.rm = TRUE),
    tot_points_aa_a4 = sum(points_aa_a4, na.rm = TRUE),
    tot_points_a4 = tot_points_ia_a4 + tot_points_aa_a4
  ) %>%
  ungroup()


data1602 <- data1602 %>%
  mutate(no_points_a4 = as.integer(tot_points_a4 == 0))


data_filtered1602 <- data1602 %>% filter(day == 4)

summary_by_treatment1602 <- data_filtered1602 %>%
  filter(treatment <= 1) %>%
  group_by(treatment) %>%
  summarise(
    total_no_points_a4 = sum(no_points_a4, na.rm = TRUE),
    percent_no_points_a4 = mean(no_points_a4, na.rm = TRUE) * 100
  )
print(summary_by_treatment)

model1602 <- lm(no_points_a4 ~ factor(treatment), data = data_filtered1602 %>% filter(treatment <=1))
stargazer(model1602, type="text", digits=7)

##fig 4: maximmize across all days


data_cleaned1702 <- mainWork


data_day4.1702 <- data_cleaned1702 %>% filter(day == 4)
summary_day4.1702 <- data_day4.1702 %>%
  group_by(treatment) %>%
  summarise(sum_ind_optimal_ia_all = sum(ind_optimal_ia_all, na.rm = TRUE))


model1.1702 <- lm(ind_optimal_ia_all ~ factor(treatment), data = data_cleaned1702)
stargazer(model1.1702,type="text", digits=7)


model2.1702 <- lm(
  ind_optimal_ia_all ~ factor(treatment) + demo_age_median + demo_sex + demo_white + demo_collegeplus +
    demo_studentloan + demo_holddebt + factor(demo_covid) + error_count_total,
  data = data_cleaned1702 %>% filter(day == 4 & treatment <= 2)
)
stargazer(model2.1702, type="text", digits=7)




##fig 4 sind die 8.9 prozent mehr in debt repayment signifikant?

data_cleaned1704 <- mainWork

data_cleaned1704 <- data_cleaned1704 %>%
  group_by(idturk) %>%
  mutate(
    tot_points_ia_a1 = sum(points_ia_a1, na.rm = TRUE),
    tot_points_ia_a2 = sum(points_ia_a2, na.rm = TRUE),
    tot_points_ia_a3 = sum(points_ia_a3, na.rm = TRUE),
    tot_points_ia_a4 = sum(points_ia_a4, na.rm = TRUE),
    tot_points_aa_a1 = sum(points_aa_a1, na.rm = TRUE),
    tot_points_aa_a2 = sum(points_aa_a2, na.rm = TRUE),
    tot_points_aa_a3 = sum(points_aa_a3, na.rm = TRUE),
    tot_points_aa_a4 = sum(points_aa_a4, na.rm = TRUE)
  ) %>%
  ungroup()

data_cleaned1704 <- data_cleaned1704 %>%
  mutate(
    tot_points_a1 = tot_points_ia_a1 + tot_points_aa_a1,
    tot_points_a2 = tot_points_ia_a2 + tot_points_aa_a2,
    tot_points_a3 = tot_points_ia_a3 + tot_points_aa_a3,
    tot_points_a4 = tot_points_ia_a4 + tot_points_aa_a4
  )

data_cleaned1704 <- mainWork_tot %>%
  mutate(
    tot_points_all = tot_points_a1 + tot_points_a2 + tot_points_a3 + tot_points_a4,
    tot_points_debt = tot_points_a3 + tot_points_a4,
    share_tot_points_debt = tot_points_debt / tot_points_all
  )

data_filtered1704 <- data_cleaned1704 %>%
  filter(treatment > 0 & day == 4 & ind_optimal_ia_all == 0)

model1.1704 <- lm(share_tot_points_debt ~ factor(treatment), data = data_filtered1704)
stargazer(model1.1704, type="text",digits=7)

model2.1704 <- lm(
  share_tot_points_debt ~ factor(treatment) +
    demo_age_median + demo_sex + demo_white + demo_collegeplus +
    demo_studentloan + demo_holddebt + factor(demo_covid) + error_count_total,
  data = data_filtered1704
)
stargazer(model2.1704, type="text", digits=7)



