Warning:

  1. Have only initial support OpenId 2.0
  1. Not check signature of fields (openid.signed,openid.sig) from response

For using needed mochiweb\_util (from http://code.google.com/p/mochiweb/) and mochiweb\_xpath(from http://groups.google.com/group/mochiweb). Before using please start inets(inets:start()).

Module contains 3 functions:

getRedirect(OpenIdUrl,YourServer,ReturnYourServer) - return url for redirect client to server authentication.

  1. OpenIdUrl - client openId,
  1. YourServer - your domain,
  1. ReturnYourServer - path for return client from server after authentication(this path will be used to return).

getResultRedirect(ResultdedUrl) - parse returned result from server (mainly testing function). Return only parsed result, but not check result authentication.

  1. ResultdedUrl - path where server return client(redirect).

getResultRedirectParsed(ResultdedUrl) - parse returned from server result.

  1. ResultdedUrl -  path where server return client(redirect).

Function can return 2 variants tuple:
  1. {true, _} - successful authentication.
  1. {false,_ } - badly authentication.


---


Модуль предоставляет 3 функции:

Функция getRedirect(OpenIdUrl,YourServer,ReturnYourServer) возвращает url на который нужно перенаправить пользователя для аунтификации по OpenID.

  1. OpenIdUrl - идентификатор пользователя,
  1. YourServer - ваш домен, для которого производится проверка,
  1. ReturnYourServer - путь не который отправить пользователя после проверки

getResultRedirect(ResultdedUrl) - разбор параметров возврата.
Это в большей мере служебная форма, нужна для того чтобы диагностировать проблемы с модулем.

  1. ResultdedUrl - путь на который сервер аунтификации отправил пользователя(совпадает до знака вопроса с путем указаным при формировании перенаправления).

getResultRedirectParsed(ResultdedUrl) - разбор параметров возврата.

  1. ResultdedUrl - путь на который сервер аунтификации отправил пользователя(совпадает до знака вопроса с путем указаным при формировании перенаправления).

Возвращает результат проверки с указанием служебной информации.

  1. {true, _} - удачная аунтификация.
  1. {false,_ } - неудачная аунтификация.