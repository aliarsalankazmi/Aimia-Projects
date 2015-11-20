df4 <- df1 %>%
  	select(redemption_we, pay_type_code, msisdn, points_denomination) %>%
		group_by(redemption_we, pay_type_code, msisdn) %>%
		summarise(total_redemptions_perCustomer = n(),
			  points_redeemed_perCustomer = sum(points_denomination)) %>%
		group_by(redemption_we, pay_type_code) %>%
		summarise(total_redemptions = sum(total_redemptions_perCustomer),
			  unique_customers = n_distinct(msisdn),
			  total_points_redeemed = sum(points_redeemed_perCustomer),
			  avg_points_redeemed = mean(points_redeemed_perCustomer),
			  trim_avg_points_redeemed = mean(points_redeemed_perCustomer, trim = .05),
			  max_points_redeemed = max(points_redeemed_perCustomer),
			  min_points_redeemed = min(points_redeemed_perCustomer),
			  sd_points_redeemed = sd(points_redeemed_perCustomer))


g1 <- df4 %>% ggplot(aes(x = redemption_we, y = total_redemptions)) + singleBarGeom + myTheme + 
  ggtitle('Redemptions Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(pay_type_code ~ ., scales = 'free')

g2 <- df4 %>% ggplot(aes(x = redemption_we, y = total_points_redeemed)) + singleBarGeom + myTheme + ggtitle('Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(pay_type_code ~ ., scales = 'free')

g3 <- df4 %>% ggplot(aes(x = redemption_we, y = avg_points_redeemed)) + singleBarGeom + myTheme + 
  ggtitle('Avg. Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(pay_type_code ~ ., scales = 'free')

g4 <- df4 %>% ggplot(aes(x = redemption_we, y = trim_avg_points_redeemed)) + singleBarGeom + myTheme + 
  ggtitle('Trimmed Avg. of Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(pay_type_code ~ ., scales = 'free')

print(g1)
print(g2)
print(g3)
print(g4)


df4.1 <- df4 %>%
  ungroup() %>%
  arrange(pay_type_code, redemption_we) %>%
  group_by(pay_type_code) %>%
  mutate(redempChange = round(100 *((total_redemptions - lag(total_redemptions))/lag(total_redemptions))),
         ptsChange = round(100 *((total_points_redeemed - lag(total_points_redeemed))/lag(total_points_redeemed))),
         avgPtsChange = round(100 *((avg_points_redeemed - lag(avg_points_redeemed))/lag(avg_points_redeemed))),
         redempStatus = ifelse(redempChange >= 0, 'increased', 'decreased'),
         ptsStatus = ifelse(ptsChange >= 0, 'increased', 'decreased'),
         avgPtsStatus = ifelse(avgPtsChange >= 0, 'increased', 'decreased'),
         redempNumTidy = tidyNum(total_redemptions),
         ptsNumTidy = tidyNum(total_points_redeemed),
         avgNumTidy = tidyNum(avg_points_redeemed)) %>%
  ungroup() %>%
  filter(redemption_we == max(redemption_we))


