library(ggplot2)
library(ggmemo)

nba <- read.csv("~/Desktop/nba_top30_2025_26.csv")

# --- Chart 1: Top 10 scorers with callouts -----------------------------
# With coord_flip(), ggplot swaps x and y — so nudge_x moves vertically
# and nudge_y moves horizontally. We use position = "bottom-right" which
# after the flip means "to the right of the bar end, shifted up."

top10 <- nba[1:10, ]
top10$PLAYER <- factor(top10$PLAYER, levels = rev(top10$PLAYER))

p1 <- ggplot(top10, aes(x = PLAYER, y = PTS)) +
  geom_col(fill = "#1D428A", width = 0.7) +
  coord_flip(clip = "off") +
  annotate_callout(
    top10,
    where = RANK == 1,
    label = "Scoring champ",
    position = "bottom-right",
    nudge = c(0.4, 2)
  ) +
  annotate_callout(
    top10,
    where = PLAYER == "Nikola Jokić",
    label = "Triple-double machine\n(12.9 REB, 10.7 AST)",
    position = "bottom-right",
    nudge = c(0.4, 3),
    size = 3.5
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "NBA Top 10 Scorers — 2025-26 Season",
    subtitle = "Points per game",
    x = NULL, y = "PPG"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(10, 40, 10, 10))

ggsave("~/Desktop/nba_chart1_scorers.png", p1, width = 11, height = 6, dpi = 150)
cat("Chart 1 saved.\n")


# --- Chart 2: Points vs Assists scatter --------------------------------
# Nudge Flagg label further away so it doesn't overlap nearby points.

p2 <- ggplot(nba, aes(x = AST, y = PTS)) +
  geom_point(aes(size = REB), alpha = 0.7, color = "#1D428A") +
  annotate_callout(
    nba,
    where = PLAYER == "Nikola Jokić",
    label = "Jokić",
    position = "top-left",
    nudge = c(0.8, 1.5)
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Luka Dončić",
    label = "Luka",
    position = "top-left",
    nudge = c(0.8, 1.5)
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Cooper Flagg",
    label = "Flagg (rookie)",
    position = "bottom-left",
    nudge = c(1.0, 2.0)
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Victor Wembanyama",
    label = "Wemby\n(3.1 BLK)",
    position = "top-right",
    nudge = c(0.6, 1.5),
    size = 3.5
  ) +
  scale_size_continuous(range = c(2, 8), name = "Rebounds") +
  labs(
    title = "Scoring vs Playmaking — Top 30 NBA Players",
    subtitle = "Bubble size = rebounds per game",
    x = "Assists per game",
    y = "Points per game"
  ) +
  theme_minimal(base_size = 14)

ggsave("~/Desktop/nba_chart2_scatter.png", p2, width = 10, height = 7, dpi = 150)
cat("Chart 2 saved.\n")


# --- Chart 3: FG% comparison -------------------------------------------
# Create a column already in percentage units so annotate_change() and the
# y-axis scale agree.

fg_top <- nba[order(-nba$FG_PCT), ][1:10, ]
fg_top$FG_PCT_100 <- fg_top$FG_PCT * 100
fg_top$PLAYER <- factor(fg_top$PLAYER, levels = fg_top$PLAYER)

p3 <- ggplot(fg_top, aes(x = PLAYER, y = FG_PCT_100)) +
  geom_col(fill = "#C8102E", width = 0.6) +
  annotate_change(
    fg_top,
    from = PLAYER == fg_top$PLAYER[1],
    to = PLAYER == fg_top$PLAYER[10],
    value = FG_PCT_100,
    format = "points"
  ) +
  labs(
    title = "Top 10 Most Efficient Scorers (FG%)",
    subtitle = "Field goal percentage, 2025-26 regular season",
    x = NULL, y = "FG%"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("~/Desktop/nba_chart3_fg_pct.png", p3, width = 10, height = 6, dpi = 150)
cat("Chart 3 saved.\n")

cat("All done!\n")
