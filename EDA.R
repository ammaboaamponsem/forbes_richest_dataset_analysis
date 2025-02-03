# Calculate the average net worth by industry
average_net_worth <- billionaire_rank.df %>%
  group_by(industry) %>%
  summarize(mean_net_worth = mean(net_worth, na.rm = TRUE))
average_net_worth

# Create the bar chart showing the average net worth over industry
ggplot(average_net_worth, aes(x = industry, y = mean_net_worth)) + 
  geom_bar(stat = "identity", fill = "cyan") +
  labs(
    title = "Average Net Worth of Billionaires by Industry",
    x = "Industry",
    y = "Average Net Worth"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
# Create the Age histogram of Billionaires
ggplot(billionaire_rank.df, aes(x = age)) +
  geom_histogram(binwidth = 10, fill = "purple", color = "black") +
  labs(
    title = "Histogram of Ages of Billionaires",
    x = "Age",
    y = "Number of Billionaires"
  ) +
  theme_bw()

# Identify the top 20 countries with the most billionaires
top_countries <- head(billionaire_rank.df, 20)
top_countries

# Create the Top 20 Billionaires' net worth vs country bar chart
ggplot(top_countries, aes(x = country, y = net_worth)) + 
  geom_bar(stat = "identity", fill = "green") +
  labs(
    title = "Net Worth of Top 20 Billionaires vs Countries",
    x = "Country",
    y = "Net Worth"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group the data by country and summarize the number of billionaires
summary_billionaires.df <- billionaire_rank.df %>%
  group_by(country) %>%
  summarize(num_billionaires = n())
summary_billionaires.df

# Remove countries with fewer than 20 number of billionaires
summary_billionaires.df <- summary_billionaires.df %>%
  filter(num_billionaires >= 20)
summary_billionaires.df

# Order the total number of billionaires in descending order
summary_billionaires.df <- summary_billionaires.df %>%
  arrange(desc(num_billionaires))
summary_billionaires.df

# Make the country into a factor with levels defined by the current ordering
summary_billionaires.df$country <- factor(summary_billionaires.df$country, levels = summary_billionaires.df$country)
summary_billionaires.df$country

# Add a column with cumulative count of billionaires
summary_billionaires.df <- summary_billionaires.df %>%
  mutate(cumulative_count = cumsum(num_billionaires))
summary_billionaires.df

# Add a column with relative frequency of billionaires
summary_billionaires.df <- summary_billionaires.df %>%
  mutate(relative_frequency = num_billionaires / sum(num_billionaires))
summary_billionaires.df

# Add a column with cumulative relative frequency of billionaires
summary_billionaires.df <- summary_billionaires.df %>%
  mutate(cumulative_relative_frequency = cumsum(relative_frequency))
summary_billionaires.df

# Install and load the ggeasy package
library(ggeasy)

# Calculate the cumulative counts and cumulative relative frequency
summary_billionaires.df <- summary_billionaires.df %>%
  arrange(desc(num_billionaires)) %>%
  mutate(
    cumulative_count = cumsum(num_billionaires),
    cumulative_relative_frequency = cumulative_count / sum(num_billionaires)
  )
summary_billionaires.df

library(ggthemes)

# Create the Pareto Chart with cumulative counts and ogive with dots
pareto_chart <- ggplot(summary_billionaires.df, aes(x = reorder(country, -num_billionaires), y = num_billionaires)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_line(aes(x = country, y = cumulative_count, group = 1), color = "black") +
  geom_point(aes(x = country, y = cumulative_count), color = "blue") +  # Add dots
  labs(
    x = "Country",
    y = "Number of Billionaires",
    title = "Pareto and Ogive of Top 12 Countries vs Billionaires"
  ) +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the Pareto Chart
print(pareto_chart)

#Export cleaned data set to CSV file
