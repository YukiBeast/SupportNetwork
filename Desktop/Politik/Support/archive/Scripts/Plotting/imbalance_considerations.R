# imb_per_state considerations
source("Scripts/state_considerations.R", echo = TRUE)

library(ggplot2)
ggplot(imb[state %in% interesting],
        aes(group = country_iso)) +
  geom_line(aes(x = year, y = imb_per_state, col = "among triangles containing state i")) +
  geom_line(aes(x = year, y = imb_per_total, col = "among all imbalanced triangles")) +
  scale_color_manual(values = c("among triangles containing state i" = "#bd1f76", "among all imbalanced triangles" = "rgb(27, 98, 116)")) +
  facet_wrap(~country_iso) +
  labs(x = "Year", y = "Imbalanced Triangles (%)") +
  theme_minimal(base_size = 24)

# m1 <- lm(imb_per_state ~ year + state, data = imb)
# high_coefficent <- names(head(sort(m1$coefficients, decreasing = T), 15))
# high_coefficent <- gsub("state", "", high_coefficent)


ggplot(imb[!is.na(democ)], aes(x = year, y = imb_per_state, group = state)) +
  geom_point() +
  geom_smooth(se = T, method = "lm") +
  facet_wrap(~state) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal(base_size = 24)

ggplot(imb[state == "United States of America"], aes(x = year, y = imb_per_state)) +
  geom_line()



library(ggplot2)
ggplot(imb, aes(x = year, y = imb_per_state, group = state)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(imb, aes(x = year, y = imb_per_state, group = state)) +
  geom_line(alpha = 0.3) +
  geom_smooth(se = F, method = "glm", method.args = list(family = "quasibinomial"),
              alpha = 0.3) +
  facet_wrap(~continent) +
  scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal(base_size = 24)


library(data.table)


ggplot(imb, aes(x = year, y = imb_per_state, group = state)) +
  geom_line(alpha = 1) +
#  geom_smooth(se = FALSE, method = "glm", method.args = list(family = "quasibinomial"),
 #             col = "#0099ff6c") +
  facet_wrap(~state) +
 # scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal(base_size = 1)

ggplot(imb[actor_type == "NO"], aes(x = year, y = imb_per_state, group = state)) +
  geom_line(alpha = 1) +
  geom_smooth(se = FALSE, method = "glm", method.args = list(family = "quasibinomial"),
              col = "#0099ff6c") +
  facet_wrap(~region) +
#  scale_y_continuous(limits = c(0, 0.6)) +
  theme_minimal(base_size = 24)

unique(imb[actor_type == "NO" & is.na(region)]$state)
unique(imb$region)


