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

-module(sift).

-export([boolean/1]).
-export([number/1]).
-export([string/1]).
-export([proplist/1]).
-export([check/2]).

-spec boolean(sift_boolean:opts()) -> sift_rule:rule().
boolean(Opts) ->
    sift_boolean:rule(Opts).

-spec number(sift_number:opts()) -> sift_rule:rule().
number(Opts) ->
    sift_number:rule(Opts).

-spec string(sift_string:opts()) -> sift_rule:rule().
string(Opts) ->
    sift_string:rule(Opts).

-spec proplist(sift_proplist:opts()) -> sift_rule:rule().
proplist(Opts) ->
    sift_proplist:rule(Opts).

-spec check(sift_rule:rule(), any()) -> {ok, any()}
    | {error, sift_error:error()}.
check(Rule, Value) ->
    sift_rule:check(Rule, Value).
