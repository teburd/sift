%% Copyright (c) 2012, Treetop Software LLC
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
%% @copyright 2012 Treetop Software LLC
%% @doc Boolean Value Checking and Conversion

-module(sift_boolean).

-export([rule/1]).
-export([check/2]).

-type opts() :: proplists:proplist().

-spec rule(opts()) -> sift_rule:rule().
rule(Opts) ->
    sift_rule:rule({sift_boolean, check}, Opts).

-spec check(opts(), any()) -> {ok, boolean()} | {error, sift_error:error()}.
check(_Rule, 1) -> {ok, true};
check(_Rule, 0) -> {ok, false};
check(_Rule, <<"True">>) -> {ok, true};
check(_Rule, <<"False">>) -> {ok, false};
check(_Rule, "True") -> {ok, true};
check(_Rule, "False") -> {ok, false};
check(Rule, Other) ->
    State = sift_rule:state(Rule),
    {error, sift_error:error(sift_boolean, State, Other, <<"Must be a boolean value">>)}.
