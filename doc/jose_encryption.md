# Установка

## Устанавливаем Step Cli

https://github.com/smallstep/cli#installation-guide

## Создаем JWK ключ

RSA 4096 ключ для шифрования:

    step crypto jwk create rsa-enc.pub.json rsa-enc.json --kty RSA --size 4096 --use enc
    
Ed25519 ключ для криптоподписи:
    
    step crypto jwk create ed.pub.json ed.json --kty OKP --crv Ed25519 --use sig
    
## Добавляем JWK в конфигурацию

Добавляем публичный ключ из `rsa-enc.pub.json` в `map` конфигурации `shareholders`:

```
{shareholders, #{
            <<"ndiezel">> => #{
                owner => <<"ndiezel0@gmail.com">>,
                public_key => <<"{
  \"use\": \"enc\",
  \"kty\": \"RSA\",
  \"kid\": \"KUb1fNMc5j9Ei_IV3DguhJh5UOH30uvO7qXq13uevnk\",
  \"alg\": \"RSA-OAEP-256\",
  \"n\": \"2bxkamUQjD4CN8rcq5BfNLJmRmosb-zY7ajPBJqtiLUTcqym23OkUIA1brBg34clmU2ZQmtd3LWi5kVJk_wr4WsMG_78jHK3wQA-HRhY4WZDZrULTsi4XWpNSwL4dCml4fs536RKy_TyrnpiXg0ug4JVVaEeo7VIZ593mVhCxC8Ev6FK8tZ2HGGOerUXLpgQdhcp9UwaI_l7jgoWNp1f7SuBqv1mfiw4ziC1yvwyXHTKy-37LjLmVB9EVyjqpkwZgzapaOvHc1ABqJpdOrUh-PyOgq-SduqSkMrvqZEdUeR_KbFVxqbxqWJMrqkl2HOJxOla9cHRowg5ObUBjeMoaTJfqie3t6uRUsFEFMzhIyvo6QMYHooxIdOdwpZ4tpzML6jv9o5DPtN375bKzy-UsjeshYbvad1mbrcxc8tYeiQkDZEIM0KeOdHm5C6neEyY6oF4s1vSYBNCnhE5O-R9dmp8Sk5KEseEkOH5u4G2RsIXBA9z1OTDoy6qF21EvRCGzsGfExfkmPAtzbnS-EHHxbMUiio0ZJoZshYo8dwJY6vSN7UsXBgW1v7GvIF9VsfzRmgkl_3rdemYy28DJKC0U2yufePcA3nUJEhtR3UO_tIlHxZvlDSX5eTx4vs5VkFfujNSiPsgH0PEeXABGBFbal7QxU1u0XHXIFwhW5cM8Fs\",
  \"e\": \"AQAB\"
}">>
            }
        }}
```

# Инициализация



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
    "encrypted_share": "<EncodedMasterKeyShare>"
  }
]
```

Отдаем `encrypted_share` соответствующим владельцам

## Валидация

Каждый владелец ключей расшифровывает свою часть и отдает на валидацию:

```bash
$ echo "<insert EncodedMasterKeyShare here>" | \
 step crypto jwe decrypt --key rsa-enc.json | \
 base64 | \
 (read DecryptedShare; \
  woorl -s damsel/proto/cds.thrift \
   'http://cds:8022/v1/keyring' \
   Keyring ValidateInit '{"content_type":"base64","content": "'"$(echo $DecryptedShare)"'"}')
```

`EncodedMasterKeyShare` - полученная зашифрованная часть мастер ключа

`http://cds:8022/v1/keyring` - пример пути до `cds`