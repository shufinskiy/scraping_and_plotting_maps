# Загружаемые библиотеки
library(rvest)
library(selectr)
library(xml2)
library(jsonlite)
library(tidyverse)
library(data.table)
library(lubridate)

wiki_russian_player <- read_html("https://ru.wikipedia.org/wiki/%D0%A1%D0%BF%D0%B8%D1%81%D0%BE%D0%BA_%D0%B8%D0%B3%D1%80%D0%BE%D0%BA%D0%BE%D0%B2_%D1%81%D0%B1%D0%BE%D1%80%D0%BD%D0%BE%D0%B9_%D0%A0%D0%BE%D1%81%D1%81%D0%B8%D0%B8_%D0%BF%D0%BE_%D1%84%D1%83%D1%82%D0%B1%D0%BE%D0%BB%D1%83")

# Прочтение нужного url-адреса с помощью read_html
wiki <- read_html("https://ru.wikipedia.org/wiki/Список_игроков_сборной_России_по_футболу")

# html_nodes - функция извлечения частей HTML-документа
# с помощью css или XPath селекторов
wiki_html <- html_nodes(wiki, ".wikitable.sortable")

# html_table - функция извлечения таблицы из HTML-документа
wiki_table <- html_table(wiki_html)

# Выбор первой из 2 таблиц
wiki_table <- wiki_table[[1]]

# Удаление первого столбца
wiki_table <- wiki_table[, -1]

# Конвертация data.frame в data.table
wiki_table <- setDT(wiki_table)

# Разделение имени, фамилии и отчества на три отдельных столбца
wiki_table1 <- wiki_table[,c("Фамилия", "Имя") := tstrsplit(`Полное имя`, ",") ]
wiki_table2 <- wiki_table1[, c("Имя", "Отчество") := tstrsplit(str_trim(`Имя`, side = 'left'), " ")]

# исправление опечатки в дате рождения Олега Шатова
wiki_table3 <- wiki_table2[`Фамилия` == "Шатов", 
                           `Дата рождения, возраст` := str_c(str_sub(str_extract(`Дата рождения, возраст`, "^[:digit:]{7}"), start = 1, end = 4), 0, str_sub(str_extract(`Дата рождения, возраст`, "^[:digit:]{7}"), start = 5, end = 7))]

# Преобразование столбцов с датами в формат Date
# Преобразование столбцов матчей и голов в числовые.
wiki_table4 <- wiki_table3 %>%
  mutate_at(vars(`Дата рождения, возраст`,
                 `Дата первого матча`, 
                 `Дата последнего матча`), ~ymd(str_extract(., "^[:digit:]{8}"))) %>%
  mutate_at(vars(`Матчей`, `Голов`),
            ~as.numeric(str_extract(., "(^[:digit:]{1,})|(^-[:digit:]{1,})")))

# Перегруппировка столбцов
wiki_table4 <- wiki_table4[,c(7, 8, 9, 2, 4, 5, 3, 6)]

# Сохранение результатов
write.csv(wiki_table4, "general_table_rus_football.csv")

# Получение всех ссылок со страницы "Список_игроков_сборной_России_по_футболу"
link_html <- html_nodes(wiki, ".wikitable.sortable a")

# Получение адресов всех ссылок с помощью функции html_attr
link <- html_attr(link_html, 'href')

# Выбираем только ссылки на профили игроков в первой таблице
link1 <- link[seq(1, 717, 3)]

# Функция получения всех данных из таблички игрока в его профиле на Википедии
data <- lapply(paste0("https://ru.wikipedia.org", link1), function(n){
  data_html <- read_html(n)
  data_nodes <- html_nodes(data_html, ".infobox.vcard td.plainlist")
  data_text <- html_text(data_nodes)
  return(data_text)
})

# Объединение каждого элемента списка в таблицу данных
# Количество столбцов равно нибольшему по длине элементу списка
data <- plyr::ldply(data, rbind)

# Сохранение результатов
write.table(data, "rus_football_team.csv")
