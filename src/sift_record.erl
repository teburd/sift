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


%% @author Tom Burdick <tburdick@treetopllc.com>
%% @copyright 2012 Treetop Software LLC 
%% @doc Record and Proplist Conversion

-module(sift_record).

-export([to_proplist/2]).
-export([from_proplist/3]).

%% @doc Convert a record to a proplist with binary key names
-spec to_proplist(tuple(), list(atom())) -> proplists:proplist().
to_proplist(Record, Fields) 
        when is_tuple(Record), is_list(Fields) ->
    BinFields = lists:map(fun(Field) -> atom_to_binary(Field, utf8) end, Fields),
    lists:zip(BinFields, tl(tuple_to_list(Record))).

%% @doc Convert a flat proplist with binary key names to a record
-spec from_proplist(atom(), list(atom()), list({binary(), any()}))
    -> tuple().
from_proplist(RecordName, Fields, Properties)
        when is_atom(RecordName), is_list(Fields), is_list(Properties) ->
    Record = lists:map(fun(Field) ->
        FieldBin = atom_to_binary(Field, utf8),
        proplists:get_value(FieldBin, Properties)
    end, Fields),
    list_to_tuple([RecordName | Record]).

