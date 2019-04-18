# Установка

## Устанавливаем Step Cli

https://github.com/smallstep/cli#installation-guide

## Создаем JWK ключ

RSA 4096 ключ для шифрования:

    step crypto jwk create rsa-enc.pub.json rsa-enc.json --kty RSA --size 4096 --use enc
    
EC ключ для криптоподписи:
    
    step crypto jwk create ec.pub.json ec.json --kty EC --crv P-256 --use sig
    
## Добавляем JWK в конфигурацию

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

# Инициализация

Запуск `cds` из состояния отсутствующего `Keyring`.

Разделен на 2 этапа: Начало и Валидация.

На этапе Начало мы указываем количество фрагментов ключа при комбинации которых
 мы получим `MasterKey`
 который шифрует `Keyring`, и получаем в ответ зашифрованные публичными
 ключами из конфигурации фрагменты `MasterKey`, которые отдаются соответствующим
 владельцам для прохождения этапа Валидация.
 
На этапе Валидация каждый владелец ключа передает свой расшифрованный фрагмент
 ключа в виде JWS, подписанном личным криптоключом. Процесс инициализации
 можно считать завершенным когда последнему владельцу приходит `struct Success {}`.

В случае передачи владельцем фрагмента ключа фрагмент с неверной подписью, фрагмент
 отвергается, а процесс Валидации продолжается. В случае передачи владельцем фрагмента
 ключа неверный фрагмент, фрагмент принимается, но после получения последнего фрагмента
 вернется ошибка и процесс инициализации будет сброшен с необходимостью начать с этапа
 Начало.

Этап Валидация необходим для подтверждения, что все фрагменты `MasterKey` были
 доставлены своим адресатам и были успешно расшифрованны.

## Начало

Начинаем процесс инициализации:

```bash
$ woorl -s ~/Development/damsel/proto/cds.thrift \
   'http://127.0.0.1:32778/v1/keyring' \
   Keyring StartInit '<insert threshold here>'
```

`threshold` - количество частей ключей нужное для востановления мастер ключа

Мы получаем зашифрованные части мастер ключа вида:

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

## Валидация

Каждый владелец ключей расшифровывает свою часть, подписывает и отдает на валидацию:

```bash
$ step crypto jws sign <(echo "<insert EncryptedMasterKeyShare here>" | step crypto jwe decrypt --key rsa-enc.json) --key ec.json | \
 (read DecryptedShare; \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ValidateInit '"<insert id, ex. ndiezel>"' '"'"$(echo $DecryptedShare)"'"')
```

`EncodedMasterKeyShare` - полученная зашифрованная часть мастер ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`
