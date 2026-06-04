library(ggplot2)
library(ggmemo)

nba <- read.csv("~/Desktop/nba_top30_2025_26.csv")

# --- Chart 1: Top 10 scorers with callouts on #1 and the rookie --------

top10 <- nba[1:10, ]
top10$PLAYER <- factor(top10$PLAYER, levels = rev(top10$PLAYER))

ggplot(top10, aes(x = PLAYER, y = PTS)) +
  geom_col(fill = "#1D428A", width = 0.7) +
  coord_flip() +
  annotate_callout(
    top10,
    where = RANK == 1,
    label = "Scoring champ",
    position = "top-right",
    nudge = c(0, 1.5)
  ) +
  annotate_callout(
    top10,
    where = PLAYER == "Nikola Jokić",
    label = "Triple-double machine\n(12.9 REB, 10.7 AST)",
    position = "top-right",
    nudge = c(0, 2),
    size = 3.5
  ) +
  labs(
    title = "NBA Top 10 Scorers — 2025-26 Season",
    subtitle = "Points per game",
    x = NULL, y = "PPG"
  ) +
  theme_minimal(base_size = 14)


# --- Chart 2: Points vs Assists scatter with notable callouts ----------

ggplot(nba, aes(x = AST, y = PTS)) +
  geom_point(aes(size = REB), alpha = 0.7, color = "#1D428A") +
  annotate_callout(
    nba,
    where = PLAYER == "Nikola Jokić",
    label = "Jokić",
    position = "top-left",
    nudge = c(0.5, 1.5)
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Luka Dončić",
    label = "Luka",
    position = "top-right",
    nudge = c(0.5, 1.5)
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Cooper Flagg",
    label = "Flagg (rookie)",
    position = "bottom-left",
    nudge = c(0.5, 1.5)
  ) +
  scale_size_continuous(range = c(2, 8), name = "Rebounds") +
  labs(
    title = "Scoring vs Playmaking — Top 30 NBA Players",
    subtitle = "Bubble size = rebounds per game",
    x = "Assists per game",
    y = "Points per game"
  ) +
  theme_minimal(base_size = 14)


# --- Chart 3: FG% comparison — show change between extremes -----------

fg_top <- nba[order(-nba$FG_PCT), ][1:10, ]
fg_top$PLAYER <- factor(fg_top$PLAYER, levels = fg_top$PLAYER)

ggplot(fg_top, aes(x = PLAYER, y = FG_PCT * 100)) +
  geom_col(fill = "#C8102E", width = 0.6) +
  annotate_change(
    fg_top,
    from = PLAYER == fg_top$PLAYER[1],
    to = PLAYER == fg_top$PLAYER[10],
    value = FG_PCT,
    format = "points"
  ) +
  labs(
    title = "Top 10 Most Efficient Scorers (FG%)",
    subtitle = "Field goal percentage, 2025-26 regular season",
    x = NULL, y = "FG%"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
