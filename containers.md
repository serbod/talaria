<h1>Distributed Network Messaging Protocol</h1>

Коммерческое использование только с разрешения автора.<br>
При использовании необходимо явно указывать ссылку на источник.<br>
<br>
<p>Sergey Bodrov, 2010-04-08</p>

<h1>Контейнеры и метаданные</h1>

<p>Контейнер (Container) - это некоторый информационный объект (сообщение, картинка, двоичный файл, итд..).  Контейнер содержит метаданные (описание объекта в контейнере) и данные.</p>

<p>Контейнер определяется идентификатором вида &lt;CONT GUID="XXX" CID="YYY" /&gt;, который служит "ссылкой" на источник контейнера и сам контейнер. Идентификатор контейнера может быть вставлен в текстовое сообщение.</p>

<p>Контейнеры могут быть включены в сообщение, или могут быть запрошены по идентификатору.</p>

<h2>Формат данных контейнера</h2>

Данные контейнера могут быть следующих видов:<br>
<li> Целое число: -N, 0, N<br>
<li> Число с десятичной точкой: -N.n, 0, N.n<br>
<li> Строка байтов: "Abc"<br>
<li> Список: (значение, значение2, ...)  где значение может быть любого вида<br>
<li> Словарь: {"имя": значение, "имя2": значение2} где "имя" это строка, а значение может быть любого вида<br>
<br>
<p>Если основой контейнера является Словарь, то он содержит именованные метаданные (имя объекта, дата создания, контрольная сумма, итд..) и сам объект. Любой другой тип основы контейнера означает, что метаданных в нем нет. Список не рекомендуется использовать в качестве основы контейнера.</p>

Пример контейнера:<br>
<pre><code><br>
{"name": "image.jpg"<br>
,"size": 32<br>
,"create_date": "2010-04-08"<br>
,"data": "00101010110100100101001001010101"<br>
}<br>
</code></pre>

<h2>Сериализация контейнера</h2>

<p>Контейнер может быть сериализован в один из стандартных форматов: XML, JSON, BENCODE, CSV, INI. Если формат сериализации не поддерживает числа, то они сохраняются в виде строк. Если не поддерживается Список, то используется Словарь с именами "0", "1", "2", .. </p>