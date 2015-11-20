df8 <- df1 %>%
          select(redemption_date, msisdn, points_denomination) %>%
          mutate(dayOfWeek = wday(redemption_date, label = TRUE, abbr = TRUE),
                 monthName = month(redemption_date, label = TRUE, abbr = TRUE)) %>%
          group_by(monthName, dayOfWeek) %>%
          summarise(noOfRedemptions = n(),
                    pointsRedeemed = sum(points_denomination))

df8$dayOfWeek <- factor(df8$dayOfWeek, levels = rev(levels(df8$dayOfWeek)))

g1 <- df8 %>%
      ggplot(aes(x = monthName, y = dayOfWeek)) +
      geom_tile(aes(fill = noOfRedemptions), colour = 'white') +
      scale_fill_gradient(name = 'Total Redemptions', low = 'white', high = 'steelblue') +
      geom_point(aes(size = pointsRedeemed), colour = 'black') +
      myTheme + theme(panel.grid = element_blank(), axis.ticks = element_blank(), 
                    panel.border = element_blank()) + 
      ggtitle('Calendar View of Redemptions') + ylab('Day') + xlab('Month') +
      scale_size_continuous(name = 'Points Redeemed')


print(g1)
