# Choose colors to represent the points by group
color <- c("red","green","blue")


# Illustration of the first-order effects of working memory on IQ
ggplot(mod, aes(x = wm, y = iq)) + geom_smooth(method = "lm", color = "black") + 
  geom_point(aes(color = condition))

# Illustration of the moderation effect of working memory on IQ
ggplot(mod, aes(x = wm, y = iq)) + 
  geom_smooth(aes(group = condition), method = "lm", se = T, color = "black", fullrange = T) +
  geom_point(aes(color = condition))
