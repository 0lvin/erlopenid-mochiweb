%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%   Copyright (C) 2008 by Denis Pauk ( pauk.denis@gmail.com )            %%
%%                                                                        %%
%%   This program is free software; you can redistribute it and/or modify %%
%%   it under the terms of the GNU General Public License as published by %%
%%   the Free Software Foundation; either version 2 of the License, or    %%
%%   (at your option) any later version.                                  %%
%%                                                                        %%
%%   This program is distributed in the hope that it will be useful,      %%
%%   but WITHOUT ANY WARRANTY; without even the implied warranty of       %%
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        %%
%%   GNU General Public License for more details.                         %%
%%                                                                        %%
%%   You should have received a copy of the GNU General Public License    %%
%%   along with this program; if not, write to the                        %%
%%   Free Software Foundation, Inc.,                                      %%
%%   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(openid).
-export([getRedirect/3]).
-export([getResultRedirect/1]).
-export([getResultRedirectParsed/1]).

%%Получение урл на который нужно отправить пользователя для проверки его опенид
%%Параметры проверяемый опенид, доверие которому проверяеться, на какой урл вернуться
%%Возвращает урл
getRedirect(OpenIdUrl,YourServer,ReturnYourServer) ->
	case http:request(OpenIdUrl) of
		{ok,{_,_,Body}} ->
			getRedirectLink(Body,YourServer,OpenIdUrl,ReturnYourServer);
		{error,Reason} -> 
			{error,Reason}
	end.

%%внутренняя функция формирования из ответа сервера аунтификации запроса на проверку
getRedirectLink(Body,YourServer,OpenIdUrl,ReturnYourServer) ->
	Tree = mochiweb_html:parse(Body),
	[ServerName]=mochiweb_xpath:execute("//link[@rel='openid.server']/@href",Tree),
	URL=binary_to_list(ServerName) ++ "?" ++ mochiweb_util:urlencode([
					{"openid.mode","checkid_setup"},
					{"openid.identity",OpenIdUrl},
					{"openid.return_to",ReturnYourServer},
					{"openid.trust_root",YourServer},
					{"openid.claimed_id",OpenIdUrl}]),
	list_to_binary(URL).

%%Разбивает результирующий запрос на непосредственно имя сервера и параметры ответа {Server,[{p,a},{p,b}]}
getResultRedirect(ResultdedUrl) ->
	{ReturnYourServer,Params,_} = mochiweb_util:urlsplit_path(ResultdedUrl),
	ParsedQuery=mochiweb_util:parse_qs(Params),
	{ReturnYourServer,ParsedQuery}.

%%Веруть значение тага Select
genValueSelect(Head,Select) ->
	{Tag , Value} = Head,
	if 
		Tag == Select ->
			Value;
		true ->
			""
	end.

%%Веруть значение тага Select
genValueByTag([Head],Select) ->
	genValueSelect(Head,Select);

%%Веруть значение тага Select
genValueByTag([Head | Rest],Select) ->
	genValueSelect(Head,Select) ++ genValueByTag(Rest,Select).

%%Получение результата прверки {true,{Имя сервера, идентификатор пользователя, кто проверил}};
getResultRedirectParsed(ResultdedUrl) ->
	{ReturnYourServer,ParsedQuery} = getResultRedirect(ResultdedUrl),
	Mode = genValueByTag(ParsedQuery, "openid.mode"),
	if
	    Mode == "id_res" ->
			 Identity = genValueByTag(ParsedQuery,"openid.identity"),
			 EndPoint = genValueByTag(ParsedQuery,"openid.op_endpoint"),
			 {true, {ReturnYourServer, Identity, EndPoint}};
	    true ->
			 {false,{ReturnYourServer}}
	end.
