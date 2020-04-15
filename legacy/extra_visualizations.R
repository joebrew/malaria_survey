

g1 <- ggplot(data = df %>%
               mutate(responded = ifelse(responded, 'Respondents', 'Non-respondents')), 
             aes(x = n_citations_total)) +
  geom_density(aes(group = responded,
                   fill = responded),
               alpha = 0.6) +
  scale_x_log10() +
  theme_paper() +
  scale_fill_manual(name = '',
                    values = c('darkorange', 'lightblue')) +
  labs(x = 'Citations (log scale)',
       y = 'Density',
       title = 'Total lifetime citations')


Again, the distribution is shown below (b). 


g2 <- ggplot(data = df %>%
               mutate(responded = ifelse(responded, 'Respondents', 'Non-respondents')), 
             aes(x = n_citations_average)) +
  geom_density(aes(group = responded,
                   fill = responded),
               alpha = 0.6,
               n = 10) +
  scale_x_log10() +
  theme_paper() +
  scale_fill_manual(name = '',
                    values = c('darkorange', 'lightblue')) +
  labs(x = 'Citations (log scale)',
       y = 'Density',
       title = 'Average citations per article')
# Rmisc::multiplot(g1, g2, cols = 2)
g1
g2


{r, fig.height = 4.5, fig.width = 4.5}
# Get averages per area
results_list <- list()
dist_list <- list()
year_list <- list()
areas_df <- areas_df %>% arrange(desc(count))
small_areas <- areas_df$area[1:12]
for (i in 1:length(small_areas)){
  this_area <- small_areas[i]
  sub_data <- df[df[,paste0('area_', gsub(' ', '_', this_area))],]
  this_df <- data_frame(area = this_area,
                        avg = mean(sub_data$years, na.rm = TRUE),
                        med = median(sub_data$years, na.rm = TRUE))
  dist_df <- data_frame(area = this_area,
                        val = sub_data$years)
  numberfy <- function(x){
    ifelse(x == '0%', 0,
           ifelse(x == '1-20%', 10,
                  ifelse(x == '21-40%', 30,
                         ifelse(x == '41-60%', 50,
                                ifelse(x == '61-80%', 70,
                                       ifelse(x == '81-99%', 90,
                                              ifelse(x == '100%', 100, NA)))))))
    
  }
  year_df <- data.frame(area = this_area,
                        year = seq(10, 50, 10),
                        val = c(mean(numberfy(sub_data$years_10), na.rm = TRUE),
                                mean(numberfy(sub_data$years_20), na.rm = TRUE),
                                mean(numberfy(sub_data$years_30), na.rm = TRUE),
                                mean(numberfy(sub_data$years_40), na.rm = TRUE),
                                mean(numberfy(sub_data$years_50), na.rm = TRUE)))
  for (q in seq(0, 100, 10)){
    this_df[,paste0('q_', q)] <- 
      quantile(sub_data$years, q / 100, na.rm = TRUE)
  }
  results_list[[i]] <- this_df
  dist_list[[i]] <- dist_df
  year_list[[i]] <- year_df
}
results <- bind_rows(results_list)
dist_results <- bind_rows(dist_list)
year_results <- bind_rows(year_list)
dist_results$the_area <- dist_results$area

ggplot(data = dist_results,
       aes(#group = area,
         #fill = area,
         x = val)) +
  geom_density(fill = 'darkorange',
               alpha = 0.6) +
  theme_paper(base_size = 10) +
  labs(x = 'Years',
       y = 'Density',
       title = 'Perceived years to eradication',
       subtitle = '12 most popular areas of expertise only') +
  facet_wrap(~the_area) 



Heterogeneity by area of expertise is also apparent in perceived likelihood of eradication over the pre-defined five time-points, as seen below.

{r, fig.height = 4, fig.width = 4.5}
cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(year_results$area)))
ggplot(data = year_results,
       aes(x = year,
           y = val)) +
  geom_line(aes(group = area,
                color = area,
                lty = area)) +
  theme_paper(base_size = 10) +
  scale_color_manual(name = '',
                     values = cols,
                     labels = unique(year_results$area)) +
  scale_linetype_manual(name="", 
                        values = rep(1:6, 100)[1:length(unique(year_results$area))],
                        labels = unique(year_results$area)) +
  xlab('Years until eradication') +
  ylab('Likelihood') +
  ggtitle('Likelihood trajectories',
          'By area of expertise')




ggplot(data = df %>%
         filter(responded) %>%
         # mutate(n_areas = factor(n_areas)) %>%
         filter(!is.na(ethnicity),
                !is.na(gender)),
       aes(x = sentiment,
           group = ethnicity)) +
  geom_density(aes(fill = ethnicity),
               alpha = 0.6,
               n = 10) +
  theme_paper() +
  facet_wrap(~gender)

