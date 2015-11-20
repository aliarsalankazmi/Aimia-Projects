df3 <- df1 %>%
  	select(redemption_we, rdm_type, msisdn, points_denomination) %>%
		group_by(redemption_we, msisdn, rdm_type) %>%
		summarise(total_redemptions_perCustomer = n(),
			  points_redeemed_perCustomer = sum(points_denomination)) %>%
		group_by(redemption_we, rdm_type) %>%
		summarise(total_redemptions = sum(total_redemptions_perCustomer),
			  unique_customers = n_distinct(msisdn),
			  total_points_redeemed = sum(points_redeemed_perCustomer),
			  avg_points_redeemed = mean(points_redeemed_perCustomer),
			  trim_avg_points_redeemed = mean(points_redeemed_perCustomer, trim = .05),
			  max_points_redeemed = max(points_redeemed_perCustomer),
			  min_points_redeemed = min(points_redeemed_perCustomer),
			  sd_points_redeemed = sd(points_redeemed_perCustomer))


g1 <- df3 %>% ggplot(aes(x = redemption_we, y = total_redemptions)) + singleBarGeom + myTheme +
  ggtitle('Redemptions Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(rdm_type ~ ., scales = 'free')

g2 <- df3 %>% ggplot(aes(x = redemption_we, y = total_points_redeemed)) + singleBarGeom + myTheme + ggtitle('Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(rdm_type ~ ., scales = 'free')

g3 <- df3 %>% ggplot(aes(x = redemption_we, y = avg_points_redeemed)) + singleBarGeom + myTheme +
  ggtitle('Avg. Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(rdm_type ~ ., scales = 'free')

g4 <- df3 %>% ggplot(aes(x = redemption_we, y = trim_avg_points_redeemed)) + singleBarGeom + myTheme +
  ggtitle('Trimmed Avg. of Points Redeemed Across Weeks') + ylab('') + xlab('Week Ending Date') + 
  facet_grid(rdm_type ~ ., scales = 'free')

print(g1)
print(g2)
print(g3)
print(g4)


df3.1 <- df3 %>%
  ungroup() %>%
  arrange(rdm_type, redemption_we) %>%
  group_by(rdm_type) %>%
  mutate(redempChange = round(100 *((total_redemptions - lag(total_redemptions))/lag(total_redemptions))),
         ptsChange = round(100 *((total_points_redeemed - lag(total_points_redeemed))/lag(total_points_redeemed))),
         avgPtsChange = round(100 *((avg_points_redeemed - lag(avg_points_redeemed))/lag(avg_points_redeemed))),
         redempStatus = ifelse(redempChange >= 0, 'increased', 'decreased'),
         ptsStatus = ifelse(ptsChange >= 0, 'increased', 'decreased'),
         avgPtsStatus = ifelse(avgPtsChange >= 0, 'increased', 'decreased'),
         redemNumTidy = tidyNum(total_redemptions),
         ptsNumTidy = tidyNum(total_points_redeemed),
         avgNumTidy = tidyNum(avg_points_redeemed)) %>%
  ungroup() %>%
  filter(redemption_we == max(redemption_we))

df3.1.Mobily <- df3.1 %>%
  filter(rdm_type == 'Mobily')

df3.1.Partner <- df3.1 %>%
  filter(rdm_type == 'Partner')
