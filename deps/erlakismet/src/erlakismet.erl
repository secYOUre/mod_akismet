%% Erlang Akismet Library
%%
%% Copyright (c) 2008 Oleg Sharov, oleg@sharov.name
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%% * Redistributions of source code must retain the above copyright
%%   notice, this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright
%%   notice, this list of conditions and the following disclaimer in the
%%   documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND AN EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(erlakismet).

-author("Oleg Sharov <oleg@sharov.name>").
-version("0.1").

-export([verify_key/2, check/6, spam/6, ham/6, selftest/0]).

verify_key(Key, Blog) ->
	Args = [{"blog", Blog}, {"key", Key}],
	("valid" == http_post(akismetUrl([], "verify-key"), Args)).

check(Key, Blog, UserIP, UserAgent, Content, Extra) ->
	action("comment-check", Key, Blog, UserIP, UserAgent, Content, Extra).

spam(Key, Blog, UserIP, UserAgent, Content, Extra) ->
	action("submit-spam", Key, Blog, UserIP, UserAgent, Content, Extra).

ham(Key, Blog, UserIP, UserAgent, Content, Extra) ->
	action("submit-ham", Key, Blog, UserIP, UserAgent, Content, Extra).

action(Action, Key, Blog, UserIP, UserAgent, Content, Extra) ->
	Args = akismet_params(Blog, UserIP, UserAgent, Content, Extra),
	Url = actionUrl(Key, Action),
	http_post(Url, Args).

actionUrl(Key, Action) -> akismetUrl(Key ++ ".", Action).
akismetUrl(SubDomain, Action) ->
	"http://"++ SubDomain ++"rest.akismet.com/1.1/"++ Action.

http_post(Url, Args) ->
	Body = params(Args, []),
	HTTPResult = http:request(post, {Url, headers(),
	"application/x-www-form-urlencoded; charset=\"utf-8\"", Body} , [], []),
	case HTTPResult of
		{ok, {_, _, Res}} -> Res;
		_ -> {error}
	end.

akismet_params(Blog, UserIP, UserAgent, Content, UserExtra) ->
	DefaultExtra = [{"referrer", []}, {"permalink", []},
		{"comment_type", []}, {"comment_author", []},
		{"comment_author_email", []}, {"comment_author_url", []}],
	Extra = lists:map(fun ({K, V}) ->
		case lists:keysearch(K, 1, UserExtra) of
			{value, Pair} -> Pair;
			_ -> {K, V}
		end
	end, DefaultExtra),
	[{"blog", Blog}, {"user_ip", UserIP}, {"user_agent", UserAgent},
		{"comment_content", Content} | Extra].

params([], Acc) -> lists:flatten(Acc);
params([{K, V} | Tail], []) ->
	params(Tail, [K, $=, edoc_lib:escape_uri(V)]);
params([{K, V} | Tail], Acc) ->
	params(Tail, [K, $=, edoc_lib:escape_uri(V), $& | Acc]).
	
headers() -> [{"User-Agent", "ErlangAkismetClient/0.1"}].

selftest() ->
	Key = "valid_key_here",
	verify_key(Key, "http://brbrbrbr.com/"),

	Extra = [{"referrer", "http://lola.home/"}, {"comment_type", ""},
		{"comment_author", "batan"}, {"comment_author_email", ""},
		{"comment_author_url", ""}],
	"false" = check(Key, "http://brbrbrbr.com/www.html", "85.58.33.17",
	"Mozilla/5.0", "ohoho", Extra),

	Extra2 = [{"referrer", "http://lola.home/"}, {"permalink", ""},
		{"comment_type", ""}, {"comment_author", "viag-test-123"},
		{"comment_author_email", ""}, {"comment_author_url", ""}],
	"true" = check(Key, "http://brbrbrbr.com/www.html", "85.58.33.17",
	"Mozilla/5.0", "ohoho", Extra2),
	ok.
