# Загружаемые библиотеки
library(tidyverse)
library(data.table)
library(lubridate)
library(ggthemes)
library(maptools)
library(raster)
library(rgdal)

# Загрузка данных
table1 <- fread("F:/dataset/rus_football/general_table_rus_football.csv")
table2 <- fread("F:/dataset/rus_football/rus_football_team.csv")

# Объединение таблиц

main <- table1 %>%
  left_join(table2, by = c("V1" = "id")) 

main <- main[,-c(4, 10)]

# Перевод названий столбцов в верхний регистр
names(main) <- toupper(c('ID', 'Surname', 'Name', 'data_birth',
                         'Games', 'Goals', 'data_start', 'data_end', "place", "region"))

# Таблица с количеством игроков, процентом игр и голов по регионам
plot_table <- main %>%
  mutate(GOALS = ifelse(GOALS < 0, 0, GOALS),
         GOALS_SUM = sum(GOALS),
         GAMES_SUM = sum(GAMES)) %>%
  group_by(PLACE) %>%
  transmute(GAMES = round(sum(GAMES)/GAMES_SUM*100, digits = 2),
            GOALS = round(sum(GOALS)/GOALS_SUM*100, digits = 2),
            COUNT = n()) %>%
  ungroup() %>%
  na.omit() %>%
  unique()


# График количества игроков по регионам
gg <- ggplot(plot_table, 
             aes(x = reorder(PLACE, COUNT), y = COUNT, fill = COUNT, label = COUNT)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c() +
  geom_text(nudge_y = 1.5) +
  labs(title = "Количество игроков в сборной России по футболу по регионам страны",
       caption = "Источник: Wikipedia") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# Сохранение результатов
ggsave("count_rus.jpeg", gg, width = 7, height = 10, units = "in")

# График процента игр по регионам
gg1 <- ggplot(plot_table, 
             aes(x = reorder(PLACE, GAMES), y = GAMES, fill = GAMES, label = GAMES)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c() +
  geom_text(nudge_y = 1.5) +
  labs(title = "Процент игр по регионам",
       caption = "Источник: Wikipedia") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave("games_rus.jpeg", gg1, width = 7, height = 10, units = "in")

# График процента голов по регионам
gg2 <- ggplot(filter(plot_table, GOALS >0), 
              aes(x = reorder(PLACE, GOALS), y = GOALS, fill = GOALS, label = GOALS)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis_c() +
  geom_text(nudge_y = 1.5) +
  labs(title = "Процент голов по регионам",
       caption = "Источник: Wikipedia") +
  theme_tufte() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  coord_flip()

ggsave("goals_rus.jpeg", gg2, width = 7, height = 10, units = "in")

# Построение значений на карте России
# Загрузка карты России с гарницами регионов (level =1)
RUSSIA <- getData("GADM", country="RU", level=1)

# Преобразование данных в формат, удобный для построения карты
rus_df <- fortify(RUSSIA)

# Получение id и названий регионов на карте
ty <- RUSSIA@data
name <- data.frame(id = row.names(ty),
                   NAME = ty$NAME_1, stringsAsFactors = FALSE)

# Добавление названия региона на карте к таблице plot_table
map_table <- plot_table %>%
  left_join(dplyr::select(main, PLACE, REGION), by = "PLACE") %>%
  unique()

# Удаление Москвы
map_table <- filter(map_table, REGION != "Moscow City")

# Соединение map_table и name
map_table <- map_table %>%
  left_join(name, by = c("REGION" = "NAME"))

# Соединение map_table и rus_df
map_table <- rus_df %>%
  left_join(map_table)

# Удаление регионов с игроками в сборной, которые не забили ни одного гола
map_table2 <- map_table %>%
  mutate(GOALS = ifelse(GOALS == 0, NA, GOALS))

# Построение графика количества игроков в сборной
map1 <- ggplot(map_table) +
  geom_polygon(aes(x = long, y = lat, fill = COUNT, group = group), colour = "white") +
  xlim(15,190) +
  ylim(40,83) +
  # Функция coord_map() проецирует часть земли, которая сферическая на 2D плоскость
  # Аргумент "azequalarea" - Параметр mapproject. Проекция с равными площадями, центром
  # которой является Северный полюс.
  coord_map("azequalarea") +
  scale_fill_viridis_c() +
  labs(title = "Карта регионов России по количеству игроков в сборной по футболу",
       subtitle = "Без учёта Москвы",
       caption = "Источник: Wikipedia") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size =25),
        plot.subtitle = element_text(size =20, face = "italic"),
        plot.caption = element_text(size =20),
        legend.key.size = unit(1.2, "cm"),
        legend.position = "bottom")

ggsave("map_count.jpeg", map1, width = 14.3, height = 10, unit = "in")

# Построение графика количества игроков в сборной
map2 <- ggplot(map_table) +
  geom_polygon(aes(x = long, y = lat, fill = GAMES, group = GAMES), colour = "white") +
  xlim(15,190) +
  ylim(40,83) +
  coord_map("azequalarea") +
  scale_fill_viridis_c() +
  labs(title = "Карта регионов России по проценту игр в сборной по футболу",
       subtitle = "Без учёта Москвы",
       caption = "Источник: Wikipedia") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size =25),
        plot.subtitle = element_text(size =20, face = "italic"),
        plot.caption = element_text(size =20),
        legend.key.size = unit(1.2, "cm"),
        legend.position = "bottom")

ggsave("map_count.jpeg", map2, width = 14.3, height = 10, unit = "in")

# Построение графика процента голов игроков в сборной
map3 <- ggplot(map_table2) +
  geom_polygon(aes(x = long, y = lat, fill = GOALS, group = group), colour = "white") +
  xlim(15,190) +
  ylim(40,83) +
  coord_map("azequalarea") +
  scale_fill_viridis_c() +
  labs(title = "Карта регионов России по проценту голов в сборной по футболу",
       subtitle = "Без учёта Москвы",
       caption = "Источник: Wikipedia") +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size =25),
        plot.subtitle = element_text(size =20, face = "italic"),
        plot.caption = element_text(size =20),
        legend.key.size = unit(1.2, "cm"),
        legend.position = "bottom")

ggsave("map_goals.jpeg", map3, width = 14.3, height = 10, unit = "in")
