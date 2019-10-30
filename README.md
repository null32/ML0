# СМПР
## Метрические алгоритмы
1. ### [Метод kNN и kwNN](/lab1)
1. ### [Метод Парзеновского окна](/lab2)
1. ### [Метод Потенциальных функций](/lab3)
1. ### [Алгоритм STOLP](/lab4)
## Сравнение точности метрических алгоритмов
Название | Значение параметров | Точность
:-------:|:-------------------:|:-------:
Метод kNN | k=6 | LOO=0.0333 ~ 97%
Метод kwNN | k=6, q=1 | LOO=0.0333 ~ 97%
Метод Парзеновского окна | (ядро Епанечникова) h=0.35 | LOO=0.04 ~ 96%
Метод Парзеновского окна | (ядро Квартическое) h=0.35 | LOO=0.04 ~ 96%
Метод Парзеновского окна | (ядро Треугольное) h=0.35 | LOO=0.04 ~ 96%
Метод Парзеновского окна | (ядро Гауссовское) h=0.1 | LOO=0.04 ~ 96%
Метод Парзеновского окна | (ядро Прямоугольное) h=0.35 | LOO=0.04 ~ 96%
Метод Потенциальных функций | h=(1х50, 0.5х100) | Переменная

## Байесовские алгоритмы
1. ### [Линии уровня](/lab5) (**[shiny](https://skycolor.shinyapps.io/ML0BayesLevelLines/)**)