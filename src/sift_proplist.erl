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
%% @doc Proplist Conversion and Validation.

-module(sift_proplist).

-export([rule/1]).
-export([check/2]).


-type key_type() :: binary | string | atom | any.
-type opt() :: {key_type, key_type()}.
-type opts() :: [opt()].

-record(rule, {
    key_type = any :: key_type()
    }).

-type rule() :: #rule{}.

-spec rule(opts()) -> rule().
rule(Opts) ->
    sift_record:from_proplist(rule, record_info(fields, rule), Opts).

-spec check(rule(), proplists:proplist()) -> 
    {ok, proplists:proplist()} | {error, sift_error:error()}.
check(_Rule, Value) ->
    {ok, Value}.
