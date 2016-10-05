# Create 6 subgroups
ab_groups <- tapply(ab$errors, list(ab$driving, ab$conversation), sum)

# Make the required barplot
barplot(ab_groups, beside = TRUE, 
        col = c("orange", "blue"), 
        main = "Driving Errors", 
        xlab = "Conversation Demands", 
        ylab = "Errors")

# Add the legend
legend("topright", c("Difficult","Easy"), 
    title = "Driving",
    fill = c("orange", "blue"))
    