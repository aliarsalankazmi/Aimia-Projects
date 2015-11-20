df2.2 <- df1 %>%
    select(redemption_we, points_denomination) %>%
		group_by(redemption_we, points_denomination) %>%
		summarise(total_redemptions = n()) %>%
    arrange(redemption_we)

df2.3 <- df1 %>%
		select(redemption_we, pay_type_code, points_denomination) %>%
		group_by(redemption_we, pay_type_code, points_denomination) %>%
		summarise(total_redemptions = n()) %>%
    arrange(redemption_we)

g1 <- df2.3 %>% filter(pay_type_code == 'Prepaid') %>%
		ggplot(aes(x = factor(points_denomination), y = total_redemptions)) + ggtitle('Prepaid Redemptions against Points Denomination') + 
		xlab('Points Denomination') + ylab('Count of Redemptions') + singleBarGeom + myTheme + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		facet_grid(redemption_we ~ ., scales = 'free')

g2 <- df2.3 %>% filter(pay_type_code == 'Postpaid') %>%
		ggplot(aes(x = factor(points_denomination), y = total_redemptions)) + ggtitle('Postpaid Redemptions against Points Denomination') + 
		xlab('Points Denomination') + ylab('Count of Redemptions') + singleBarGeom + myTheme + 
		theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
		facet_grid(redemption_we ~ ., scales = 'free')

g3 <- df2.2 %>% ggplot(aes(x = factor(points_denomination), y = total_redemptions)) + ggtitle('Redemptions against Points Denomination') + 
  	xlab('Points Denomination') + ylab('Count of Redemptions') +  
		singleBarGeom + myTheme + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
		facet_grid(redemption_we ~ ., scales = 'free')


print(g1)
print(g2)
print(g3)


df2.2.1 <- df2.2 %>%
  ungroup() %>%
  arrange(points_denomination, redemption_we) %>%
  group_by(points_denomination) %>%
  mutate(redempDiff = round(100 * ((total_redemptions - lag(total_redemptions))/lag(total_redemptions))),
         redempDiffStatus = ifelse(redempDiff >= 0, 'increased', 'decreased'),
         redempNumTidy = tidyNum(total_redemptions)) %>%
  ungroup() %>%
  filter(redemption_we == max(redemption_we), points_denomination <= 2000)

df2.3.1 <- df2.3 %>% 
  ungroup() %>% 
  group_by(pay_type_code, points_denomination) %>% 
  select(pay_type_code, points_denomination, redemption_we, 
         total_redemptions) %>% 
  arrange(pay_type_code, points_denomination, desc(redemption_we)) %>% 
  mutate(difference = total_redemptions - lead(total_redemptions), 
         percentDiff = round((difference/lead(total_redemptions))*100),
         status = ifelse(percentDiff >= 0, 'increased', 'decreased'),
         redempNumTidy = tidyNum(total_redemptions)) %>%
  ungroup() %>% 
  filter(redemption_we == max(redemption_we), 
         between(points_denomination, 750, 5000))

df2.3.Prepaid <- df2.3.1 %>%
  filter(pay_type_code == 'Prepaid')


df2.3.Postpaid <- df2.3.1 %>%
  filter(pay_type_code == 'Postpaid')
