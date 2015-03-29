<h2>Distributed Network Messaging Protocol</h2>

Коммерческое использование только с разрешения автора.<br>
При использовании необходимо явно указывать ссылку на источник.<br>
<br>
<p>Sergey Bodrov, 2015-01-20</p>

<br>
<br>
<h1>DNMP - описание основных действий</h1>

<h2>Подключение, авторизация</h2>

<ul><li>Хозяин -> Гость<br>
Контактные данные хозяина, тестовая фраза (строка случайных символов)</li></ul>

<ul><li>Гость -> Хозяин<br>
Контактные данные гостя и тестовая фраза, шифрованная ключом<br>
</li></ul><blockquote>Если ключа нет, то ключом становится тестовая фраза</blockquote>

<ul><li>Хозяин -> Гость<br>
результат опознания<br>
</li></ul><blockquote>Если гость известен, расшифровываем ключом ответ гостя, сравниваем с отправленной ранее тестовой фразой<br>
Если ключа нет, то ключом становится тестовая фраза<br>
Если гость неизвестен, то он добавляется в список контактов, требующих подтверждения</blockquote>


<h2>После успешной авторизации</h2>

<h3>Обновление статуса контакта</h3>

<ul><li>Гость<br>
Меняет статус - online или busy</li></ul>

<ul><li>Хозяин -> Гость<br>
Запрос детальных сведений о контакте INFO GINF</li></ul>

<ul><li>Гость -> Хозяин<br>
Детальные сведения о контакте INFO CINF<br>
Полученый Статус должен быть не offline</li></ul>

<ul><li>Хозяин<br>
Рассылает линкам (кроме Гостя) уведомления о новом статусе Гостя INFO UPDI</li></ul>

<code> Нужна оптимизация, чтобы не спамить уведомлениями. Накапливать уведомления и рассылать пакетами. Рассылать только подписчикам Гостя. </code>

<h3>Обновление таблиц маршрутизации</h3>

<ul><li>Гость -> Хозяин<br>
Список своих узлов-даунлинков (downlink nodes), включая вложенные. Хозяин добавляет полученные узлы в свою таблицу маршрутизации.</li></ul>

<h3>Передача отложенных сообщений</h3>

<code> Как бы это лучше сделать? По запросу клиента с возможностью выбора (как в почтовых сервисах) или автоматом с возможностью отмены (как в чатах) </code>

<ul><li>Хозяин -> Гость<br>
Отложенные сообщения (если есть)</li></ul>

<ul><li>Гость -> Хозяин<br>
Гость может отменить прием отложенных сообщений INFO CLIN</li></ul>

<h3>Обновление нодлиста</h3>

<code> Не всегда есть необходимость каждый раз обновлять нодлист. Только если он меньше NN узлов или не обновлялся NN дней. </code>

<ul><li>Гость -> Хозяин<br>
Запрос нодлиста INFO NLRQ</li></ul>

<ul><li>Хозяин -> Гость<br>
Нодлист INFO NLST</li></ul>

<code> Нодлист может быть очень большим. Может поинтам выдавать только часть нодлиста? </code>

<h3>Обновление сервисов</h3>

<ul><li>Гость -> Хозяин<br>
Запрос списка сервисов SRVD GET_TYPES</li></ul>

<ul><li>Гость -> Хозяин<br>
Запрос обновлений по подписанным сервисам</li></ul>


<h2>После разрыва соединения</h2>

Если нет активных подключений (линков), то всем контактам статус uncnown, себе статус offline.<br>
<br>
Если отключившимся Гостем был наш поинт, то его контакту статус offline, всем линкам рассылка уведомления о статусе контакта.<br>
<br>
Если отключившимся Гостем был узел, то:<br>
<blockquote>- отправляем аплинку сообщение об изменении маршрутизации со списком узлов, которые были у данного аплинка (из таблицы маршрутов)<br>
- удаляем из таблицы маршрутизации ветку маршрутов данного узла