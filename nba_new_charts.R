library(ggplot2)
library(ggmemo)

nba <- read.csv("nba_top30_2025_26.csv")

# --- Chart 4: Efficiency Rating dot plot (Cleveland chart) ----------------
# Shows annotate_callout on a dot plot — not just bar charts

nba_eff <- nba[order(-nba$EFF), ]
nba_eff$PLAYER <- factor(nba_eff$PLAYER, levels = rev(nba_eff$PLAYER))

ggplot(nba_eff[1:15, ], aes(x = EFF, y = PLAYER)) +
  geom_segment(aes(x = 0, xend = EFF, yend = PLAYER),
               color = "grey70", linewidth = 0.4) +
  geom_point(size = 4, color = "#552583") +
  annotate_callout(
    nba_eff[1:15, ],
    where = PLAYER == "Shai Gilgeous-Alexander",
    label = "2025-26 MVP",
    position = "bottom-right",
    nudge = c(3, 0.8),
    size = 3.5
  ) +
  annotate_callout(
    nba_eff[1:15, ],
    where = PLAYER == "Luka Dončić",
    label = "Scoring champ,\nlower efficiency",
    position = "top-right",
    nudge = c(3, 0.8),
    size = 3.5,
    colour = "#C8102E"
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "NBA Efficiency Rating — Top 15 Players",
    subtitle = "Points + rebounds + assists + steals + blocks - missed shots - turnovers",
    x = "Efficiency Rating", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(),
        plot.margin = margin(10, 60, 10, 10))


# --- Chart 5: 3-Point Volume vs Accuracy scatter -------------------------
# Callouts highlighting the volume king vs the accuracy king

ggplot(nba, aes(x = FG3A, y = FG3_PCT * 100)) +
  geom_point(aes(size = PTS), alpha = 0.6, color = "#006BB6") +
  annotate_callout(
    nba,
    where = FG3A == max(nba$FG3A),
    label = "Most 3PA/game",
    position = "bottom-left",
    nudge = c(0.5, 1.5)
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Jamal Murray",
    label = "Murray: best combo\n(7.5 3PA, 43.5%)",
    position = "top-left",
    nudge = c(0.8, 1.5),
    size = 3.5
  ) +
  annotate_callout(
    nba,
    where = PLAYER == "Cooper Flagg",
    label = "Flagg: limited range\n(3.5 3PA, 29.5%)",
    position = "bottom-right",
    nudge = c(0.5, 2),
    size = 3.5,
    colour = "#C8102E"
  ) +
  scale_size_continuous(range = c(2, 7), name = "PPG") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(
    title = "Three-Point Shooting: Volume vs Accuracy",
    subtitle = "Bubble size = points per game",
    x = "3-Point Attempts per Game",
    y = "3-Point %"
  ) +
  theme_minimal(base_size = 14)


# --- Chart 6: Assists leaders — annotate_change with format = "absolute" --
# Demonstrates the "absolute" format (existing charts only use "points")

ast_top <- nba[order(-nba$AST), ][1:10, ]
ast_top$PLAYER <- factor(ast_top$PLAYER, levels = ast_top$PLAYER)

ast_simple <- ast_top[, c("PLAYER", "AST")]

ggplot(ast_top, aes(x = PLAYER, y = AST)) +
  geom_col(fill = "#007A33", width = 0.6) +
  annotate_change(
    ast_simple,
    from = PLAYER == levels(ast_simple$PLAYER)[1],
    to = PLAYER == levels(ast_simple$PLAYER)[10],
    value = AST,
    format = "absolute"
  ) +
  annotate_callout(
    ast_top,
    where = PLAYER == "Cade Cunningham",
    label = "Most improved\nplaymaker",
    position = "bottom-right",
    nudge = c(0.5, 0.5),
    size = 3.5
  ) +
  labs(
    title = "Top 10 Playmakers — Assists per Game",
    subtitle = "2025-26 regular season",
    x = NULL, y = "APG"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --- Chart 7: Scoring vs Rebounds — annotate_change with "percent" --------
# Compares two specific players using annotate_change(format = "percent")
# plus callouts — shows both annotation types working together

duo <- nba[nba$PLAYER %in% c("Victor Wembanyama", "Cooper Flagg"), ]
duo$PLAYER <- factor(duo$PLAYER, levels = c("Cooper Flagg", "Victor Wembanyama"))

duo_simple <- duo[, c("PLAYER", "REB")]

ggplot(duo, aes(x = PLAYER, y = REB)) +
  geom_col(fill = c("#00538C", "#C4CED4"), width = 0.5) +
  annotate_change(
    duo_simple,
    from = PLAYER == "Cooper Flagg",
    to = PLAYER == "Victor Wembanyama",
    value = REB,
    format = "percent"
  ) +
  annotate_callout(
    duo,
    where = PLAYER == "Victor Wembanyama",
    label = "Elite rebounder at 7'4\"",
    position = "top-left",
    nudge = c(0.15, 0.6)
  ) +
  annotate_callout(
    duo,
    where = PLAYER == "Cooper Flagg",
    label = "Rookie: 6.7 RPG",
    position = "top-left",
    nudge = c(0.15, 0.6),
    size = 3.5
  ) +
  labs(
    title = "Rebounding: Wemby vs Flagg",
    subtitle = "Per-game rebounds, 2025-26 season",
    x = NULL, y = "Rebounds per Game"
  ) +
  theme_minimal(base_size = 14)
