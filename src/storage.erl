%%%-------------------------------------------------------------------
%%% @author Amir
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. okt 2016 13:52
%%%-------------------------------------------------------------------
-module(storage).
-author("Amir").

%% API
-compile(export_all).

create()->
  [].

add(Key,Value,Store)->
  lists:append(Store, [{Key, Value}]).

lookup(Key,Store)->
  case lists:keyfind(Key, 1, Store) of
    {Key,Value} -> {Key,Value};
    false -> notfound
  end.

split(From,To,Store)->
  {Updated, Rest} = lists:partition(fun({Key,Value})-> key:between(Key, From, To) end, Store),
  {Updated,Rest}.
merge(Entries,Store)->
    NewStore = Entries ++ Store,
    FinalStore = lists:sort(NewStore),
    FinalStore.
