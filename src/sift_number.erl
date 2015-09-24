%% Copyright (c) 2015 Thomas Burdick <thomas.burdick@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2015 Thomas Burdick <thomas.burdick@gmail.com>
%% @doc Numerical Value Checking and Conversion

-module(sift_number).

-export([rule/1]).
-export([check/2]).

-type opts() :: proplists:proplist().

-spec rule(opts()) -> sift_rule:rule().
rule(Opts) ->
    sift_rule:rule({sift_number, check}, Opts).

-spec check(opts(), any()) -> {ok, number()} | {error, sift_error:error()}.
check({'>', Val0}, Val1) when is_number(Val0) andalso is_number(Val1) andalso Val1 > Val0 ->
    {ok, Val1};
check({'>=', Val0}, Val1) when is_number(Val0) andalso is_number(Val1) andalso Val1 >= Val0 ->
    {ok, Val1};
check({'=', Val0}, Val1) when is_number(Val0) andalso is_number(Val1) andalso Val1 == Val0 ->
    {ok, Val1};
check({'<', Val0}, Val1) when is_number(Val0) andalso is_number(Val1) andalso Val1 < Val0 ->
    {ok, Val1};
check({'=<', Val0}, Val1) when is_number(Val0) andalso is_number(Val1) andalso Val1 =< Val0 ->
    {ok, Val1};
check(Rule, Other) -> 
    State = sift_rule:state(Rule),
    {error, sift_error:error(sift_number, State, Other, <<"Must be a number value and rule">>)}.
