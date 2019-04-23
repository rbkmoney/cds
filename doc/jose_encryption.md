# Операции с `Keyring`

## Установка

### Устанавливаем Step Cli

https://github.com/smallstep/cli#installation-guide

### Создаем JWK ключ

RSA 4096 ключ для шифрования:

    step crypto jwk create rsa-enc.pub.json rsa-enc.json --kty RSA --size 4096 --use enc
    
EC ключ для криптоподписи:
    
    step crypto jwk create ec.pub.json ec.json --kty EC --crv P-256 --use sig
    
### Добавляем JWK в конфигурацию

Добавляем публичный ключ из `rsa-enc.pub.json` и `ec.pub.json` в `map` конфигурации `shareholders`:

```
{shareholders, #{
            <<"ndiezel">> => #{
                owner => <<"ndiezel0@gmail.com">>,
                public_keys => #{
                    enc =>  <<"{
                                  \"use\": \"enc\",
                                  \"kty\": \"RSA\",
                                  \"kid\": \"KUb1fNMc5j9Ei_IV3DguhJh5UOH30uvO7qXq13uevnk\",
                                  \"alg\": \"RSA-OAEP-256\",
                                  \"n\": \"2bxkamUQjD4CN8rcq5BfNLJmRmosb-zY7ajPBJqtiLUTcqym23OkUIA1brBg34clmU2ZQmtd3LWi5kVJk_wr4WsMG_78jHK3wQA-HRhY4WZDZrULTsi4XWpNSwL4dCml4fs536RKy_TyrnpiXg0ug4JVVaEeo7VIZ593mVhCxC8Ev6FK8tZ2HGGOerUXLpgQdhcp9UwaI_l7jgoWNp1f7SuBqv1mfiw4ziC1yvwyXHTKy-37LjLmVB9EVyjqpkwZgzapaOvHc1ABqJpdOrUh-PyOgq-SduqSkMrvqZEdUeR_KbFVxqbxqWJMrqkl2HOJxOla9cHRowg5ObUBjeMoaTJfqie3t6uRUsFEFMzhIyvo6QMYHooxIdOdwpZ4tpzML6jv9o5DPtN375bKzy-UsjeshYbvad1mbrcxc8tYeiQkDZEIM0KeOdHm5C6neEyY6oF4s1vSYBNCnhE5O-R9dmp8Sk5KEseEkOH5u4G2RsIXBA9z1OTDoy6qF21EvRCGzsGfExfkmPAtzbnS-EHHxbMUiio0ZJoZshYo8dwJY6vSN7UsXBgW1v7GvIF9VsfzRmgkl_3rdemYy28DJKC0U2yufePcA3nUJEhtR3UO_tIlHxZvlDSX5eTx4vs5VkFfujNSiPsgH0PEeXABGBFbal7QxU1u0XHXIFwhW5cM8Fs\",
                                  \"e\": \"AQAB\"
                                }">>,
                    sig =>  <<"{
                                  \"use\": \"sig\",
                                  \"kty\": \"EC\",
                                  \"kid\": \"03ohMufKvFyAtbaXJO83S4nPkaiiLPF8dSUPtetU_CA\",
                                  \"crv\": \"P-256\",
                                  \"alg\": \"ES256\",
                                  \"x\": \"N3E6i6WabPQg7MVqsjXd81z6dmSVibxLbgYJ1UIvHR4\",
                                  \"y\": \"jDzBx7KDMzesKbpwl5H1J0TtwvOJxPaEC5wH6zZs_r8\"
                                }">>
                }
            }
        }}
```

## Инициализация

Запуск `cds` из состояния отсутствующего `Keyring`.

Разделен на 2 этапа: Начало и Валидация.

На этапе 'Начало' указывается количество фрагментов ключа, при комбинации которых
 мы получим `MasterKey`,
 который шифрует `Keyring`. Результат - зашифрованные публичными
 ключами из конфигурации фрагменты `MasterKey`, которые отдаются соответствующим
 владельцам для прохождения следующего этапа 'Валидация'.
 
На этапе 'Валидация' каждый владелец ключа передает свой расшифрованный фрагмент
 ключа в виде JWS, подписанного личным криптоключом. Процесс инициализации
 можно считать завершенным, когда последнему владельцу приходит `struct Success {}`.

В случае передачи владельцем фрагмента ключа с неверной подписью, он
 отвергается, а процесс валидации продолжается. В случае передачи владельцем
 некорректного или устаревшего фрагмента, он принимается, но после получения последнего фрагмента
 вернется ошибка и процесс инициализации будет сброшен с необходимостью начать с этапа
 'Начало'.

Этап 'Валидация' необходим для подтверждения того, что все фрагменты `MasterKey` были
 доставлены своим адресатам и успешно расшифрованны.

### Начало

Начинаем процесс инициализации:

```bash
$ woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring StartInit '<insert threshold here>'
```

`threshold` - количество фрагментов мастер-ключа, которое нужно для его востановление

Получаем зашифрованные части мастер-ключа вида:

```json
[
  {
    "id": "ndiezel",
    "owner": "ndiezel0@gmail.com",
    "encrypted_share": "<EncryptedMasterKeyShare>"
  }
]
```

Отдаем `encrypted_share` соответствующим владельцам

### Валидация

Каждый владелец фрагментов ключа расшифровывает свою часть, подписывает и отдает на валидацию:

```bash
$ echo "<insert EncryptedMasterKeyShare here>" | \
  step crypto jwe decrypt --key rsa-enc.json | \
  step crypto jws sign - --key ec.json | \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ValidateInit '"<insert id, ex. ndiezel>"' '"'"$(cat -)"'"'
```

`EncodedMasterKeyShare` - полученный зашифрованный фрагмент мастер-ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`

## Разблокировка

Расшифровывает зашифрованный `Keyring`, что позволяет производить другие операции.

Разделен на 2 этапа: Начало и Валидация.

На этапе 'Начало' вызывается метод начала операции.

На этапе 'Валидация' владельцы фрагментов мастер-ключа отправляют фрагменты в виде JWS,
 подписанного криптоключом. Собранные фрагменты востанавливают мастер-ключ и 
 расшифровывают `Keyring`. В случае успеха последнему отправившему фрагмент вернется
 `Success` и расшифрованный `Keyring` сохраняется в памяти. Если не удается
 востановить `MasterKey` из фрагментов или востановленным ключом не получается 
 расшифровать `Keyring`, то процесс разблокировки прерывается и необходимо начинать с
 этапа 'Начало'.
 
### Начало

Начинаем процесс разблокировки:

```bash
$ woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring StartUnlock
```

### Валидация

`Threshold` владельцев фрагментов ключа расшифровывают свою часть, подписывают и отдают
 на валидацию:
 
```bash
$ echo "<insert EncryptedMasterKeyShare here>" | \
  step crypto jwe decrypt --key rsa-enc.json | \
  step crypto jws sign - --key ec.json | \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ValidateUnlock '"<insert id, ex. ndiezel>"' '"'"$(cat -)"'"'
```

`EncodedMasterKeyShare` - полученный зашифрованный фрагмент мастер-ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`

## Ротация ключа шифрования карточных данных

Создание нового ключа для шифрования карточных данных и добавление его в `Keyring`.

Разделен на 2 этапа: Начало и Валидация.

На этапе 'Начало' вызывается метод начала операции.

На этапе Валидация владельцы фрагментов мастер-ключа отправляют фрагменты в виде JWS,
 подписанного криптоключом. Собранные фрагменты востанавливают мастер-ключ и 
 расшифровывают `Keyring`. В случае успеха последнему, отправившему фрагмент, вернется
 `Success` и новый `Keyring` будет использоваться в дальнейшем. Если не удается
 востановить `MasterKey` из фрагментов или востановленным ключом не получается 
 расшифровать `Keyring`, то процесс Ротации прерывается и необходимо начинать с этапа
 'Начало'.
 
### Начало

Начинаем процесс ротации:

```bash
$ woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring StartRotate
```

### Валидация

`Threshold` владельцев фрагментов ключа расшифровывают свою часть, подписывают и отдают
 на валидацию:
 
```bash
$ echo "<insert EncryptedMasterKeyShare here>" | \
  step crypto jwe decrypt --key rsa-enc.json | \
  step crypto jws sign - --key ec.json | \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ValidateRotate '"<insert id, ex. ndiezel>"' '"'"$(cat -)"'"'
```

`EncodedMasterKeyShare` - полученный зашифрованный фрагмент мастер-ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`

## Замена ключа шифрования `keyring`

Перегенерация `MasterKey`, используемого для шифрования `keyring`, разделение на
 количество `shareholders`, указанных в конфиге фрагментов, и отдача держателям
 указаным в `shareholders`.
 
Разделен на 4 этапа: Начало, Подтверждение, Постподтверждение и Валидация

На этапе 'Начало' указывается количество фрагментов ключа, при комбинации которых
 получается `MasterKey`, который шифрует `Keyring`.
 
На этапе 'Подтверждение' владельцы фрагментов мастер-ключа отправляют фрагменты в 
 виде JWS, подписанного криптоключом. Собранные фрагменты востанавливают мастер-ключ и 
 расшифровывают `Keyring`. В случае успеха последнему, отправившему фрагмент, вернется
 `Success` и осуществится переход на этап 'Постподтверждение'. Если не удается
 востановить `MasterKey` из фрагментов или востановленным ключом не получается 
 расшифровать `Keyring`, то процесс замены ключа прерывается и необходимо начинать с
 этапа 'Начало'.
 
На этапе 'Постподтверждение' получаются зашифрованные публичными ключами из
 конфигурации фрагменты `MasterKey`, которые отдаются соответствующим владельцам
 для прохождения этапа 'Валидация'.
 
На этапе 'Валидация' каждый владелец ключа передает свой расшифрованный фрагмент
 ключа в виде JWS, подписанного личным криптоключом. Этап 'Замена ключа'
 можно считать завершенным, когда последнему владельцу приходит `struct Success {}`.

### Начало

Начинаем процесс замены ключа:

```bash
$ woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring StartRekey '<insert threshold here>'
```

`threshold` - количество фрагментов мастер-ключа, которое нужно для его востановление

### Подтверждение

`Threshold` владельцев фрагментов ключа расшифровывают свою часть, подписывают и отдают
 на подтверждение:
 
```bash
$ echo "<insert EncryptedMasterKeyShare here>" | \
  step crypto jwe decrypt --key rsa-enc.json | \
  step crypto jws sign - --key ec.json | \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ConfirmRekey '"<insert id, ex. ndiezel>"' '"'"$(cat -)"'"'
```

`EncodedMasterKeyShare` - полученный зашифрованный фрагмент мастер-ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`

### Постподтверждение

Получаем зашифрованные фрагменты мастер-ключа.

```bash
$ woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring StartRekeyValidation
```

Пример получаемых фрагментов:

```json
[
  {
    "id": "ndiezel",
    "owner": "ndiezel0@gmail.com",
    "encrypted_share": "<EncryptedMasterKeyShare>"
  }
]
```

### Валидация

`Threshold` владельцев фрагментов ключа расшифровывают свою часть, подписывают и отдают
 на валидацию:
 
```bash
$ echo "<insert EncryptedMasterKeyShare here>" | \
  step crypto jwe decrypt --key rsa-enc.json | \
  step crypto jws sign - --key ec.json | \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ValidateRekey '"<insert id, ex. ndiezel>"' '"'"$(cat -)"'"'
```

`EncodedMasterKeyShare` - полученный зашифрованный фрагмент мастер-ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`