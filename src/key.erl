%%%-------------------------------------------------------------------
%%% @author Amir
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. okt 2016 13:15
%%%-------------------------------------------------------------------
-module(key).
-author("Amir").

%% API
-compile(export_all).
-define(MaxNode, 1000000000).

generate()->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
    random:uniform(1000000000) - 1.

between(_, From, To) when From == To -> true;
between(Key, _, To) when Key == To -> true;
between(Key, From, To) when To > From ->
  if Key > From andalso Key < To -> true;
    true -> false end;

between(Key, From, To) when From > To ->
  if Key > From andalso Key < ?MaxNode -> true;
    Key < To andalso Key > 0 -> true;
    true -> false
  end.
