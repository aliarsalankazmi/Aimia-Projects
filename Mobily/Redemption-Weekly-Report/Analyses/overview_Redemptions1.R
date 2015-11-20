df2.1 <- df1 %>%
  	select(redemption_we, msisdn, points_denomination) %>%
		group_by(redemption_we, msisdn) %>%
		summarise(total_redemptions_perCustomer = n(),
			  points_redeemed_perCustomer = sum(points_denomination)) %>%
		group_by(redemption_we) %>%
		summarise(unique_customers = n(),
			  total_redemptions = sum(total_redemptions_perCustomer),
			  avg_redemptions = mean(total_redemptions_perCustomer),
			  total_points_redeemed = sum(points_redeemed_perCustomer),
			  avg_points_redeemed = mean(points_redeemed_perCustomer),
			  trim_avg_points_redeemed = mean(points_redeemed_perCustomer, trim = .05),
			  max_points_redeemed = max(points_redeemed_perCustomer),
			  min_points_redeemed = min(points_redeemed_perCustomer),
			  sd_points_redeemed = sd(points_redeemed_perCustomer),
			  var_points_redeemed = var(points_redeemed_perCustomer)) %>%
    arrange(redemption_we)


last_we_date <- format(as.Date(as.character(df2.1$redemption_we[nrow(df2.1)])), '%d-%b-%Y')

df2.1.1 <- df2.1 %>% 
  ungroup() %>%
  arrange(redemption_we) %>%
  mutate(redempChange = round(100 * (total_redemptions - lag(total_redemptions))/lag(total_redemptions)),
         ptsChange = round(100 * (total_points_redeemed - lag(total_points_redeemed))/lag(total_points_redeemed)),
         avgPtsChange = round(100 * (avg_points_redeemed - lag(avg_points_redeemed))/lag(avg_points_redeemed)),
         redempStatus = ifelse(redempChange >= 0, 'increased', 'decreased'),
         ptsStatus = ifelse(ptsChange >= 0, 'increased', 'decreased'),
         avgPtsStatus = ifelse(avgPtsChange >= 0, 'increased', 'decreased')) %>%
  filter(redemption_we == max(redemption_we))


redemptions <- tidyNum(df2.1.1$total_redemptions)
pointsRedeemed <- tidyNum(df2.1.1$total_points_redeemed)
avgPtsRedeemed <- tidyNum(round(df2.1.1$avg_points_redeemed))



g1 <- df2.1 %>% ggplot(aes(x = redemption_we, y = total_redemptions)) + 
  ggtitle('Redemptions Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  singleBarGeom + myTheme

print(g1)

g2 <- df2.1 %>% ggplot(aes(x = redemption_we, y = total_points_redeemed)) + 
  ggtitle('Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
   singleBarGeom + myTheme

print(g2)

g3 <- df2.1 %>% ggplot(aes(x = redemption_we, y = avg_points_redeemed)) + 
  ggtitle('Avg. Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  singleBarGeom + myTheme

print(g3)

g4 <- df2.1 %>% ggplot(aes(x = redemption_we, y = trim_avg_points_redeemed)) + 
  ggtitle('Trimmed Avg. of Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  singleBarGeom + myTheme

print(g4)
