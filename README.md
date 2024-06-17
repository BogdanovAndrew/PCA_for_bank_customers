# PCA_for_bank_customers

**Реализация метода снижения размерности для улучшения кластеризации на Python и R**

Проект выполнен в рамках обучения в РТУ МИРЭА по дисциплине "Языки программирования для статистической обработки данных" на языке R. Предварительно для более глубокого понимания сути метода был разработан и впоследующем доработан концепт работы на Python на платформе kaggle:

https://www.kaggle.com/code/andrewbogdanov04/pca-coursework-python

***Тема:*** «Реализация метода снижения размерности табличных данных на основе метода главных компонент (PCA) на примере данных транзакций клиентов банка»

***Исходные данные:*** данные о транзакциях клиентов банка

***Перечень вопросов, подлежащих разработке, и обязательного графического материала:***

Какой математический принцип лежит в основе метода главных компонент (PCA)?
Какие шаги необходимы для реализации PCA для снижения размерности исходных данных?
Как определить оптимальное количество компонент при применении метода PCA к данным?
Какие преимущества и ограничения свойственны методу PCA при анализе исходных данных?
Какие задачи могут решаться с помощью снижения размерности данных на основе PCA?

В качестве исследуемых данных используются «Демографические данные клиентов и данные о транзакциях из индийского банка».

***Набор данных включает в себя следующие столбцы:***

1. TransactionID — единый идентификатор транзакции.
2. CustomerID — уникальный идентификатора клиента.
3. CustomerDOB — дата рождения клиента в формате день, месяц, год.
4. CustGender — пол клиента.
5. CustLocation — местоположение клиента (город).
6. CustAccountBalance — сумма денежных средств на аккаунте клиента.
7. TransactionDate — дата осуществления транзакции в формате день, месяц, год.
8. TransactionTime — время осуществления транзакции.
9. TransactionAmount — сумма осуществлённой транзакции в индийских рупиях.
