by_country <- gapminder |> 
  ungroup() |> 
  mutate(year1950 = year - 1950) |> 
  nest(.by = c(continent, country))
by_country

gapminder |>
  mutate(year1950 = year - 1950) |>
  nest(.by = c(continent, country)) |>
  mutate(model = map(data, \(x) lm(lifeExp ~ year1950, data = x)), 
         glance = map(model, glance),
         tidy = map(model, tidy),
         augment = map(model, augment),
         rsq = map_dbl(glance, "r.squared")) |>
  unnest(tidy) |>
  select(continent, country, rsq, term, estimate) |> 
  pivot_wider(names_from = term, values_from = estimate) |> 
  ggplot(aes(x = `(Intercept)`, y = year1950)) +
  geom_point(aes(colour = continent, size = rsq)) +
  geom_smooth(se = FALSE, method = "loess", formula = "y ~ x") + 
  scale_size_area() +
  labs(x = "Life expectancy (1950)", y = "Yearly improvement") +
  theme_minimal(18)