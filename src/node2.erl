%%%-------------------------------------------------------------------
%%% @author Amir
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. okt 2016 13:11
%%%-------------------------------------------------------------------
-module(node2).
-author("Amir").

%% API
-compile(export_all).
-define(Stabilize,1000).
-define(Timeout,10000).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  % io:format("Kommer jag in hit?, ~w, ~w", [Id, Peer]),
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  %io:format("We are before successor has been assigned ~n"),
  {ok, Successor} = connect(Id, Peer),
  %io:format("We are after succesor ~w, has been assigned ~n", [Successor]),
  schedule_stabilize(),
  %io:format("The successor that will be sent to node is ~w ~n", [Successor]),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};
connect(Id, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after ?Timeout ->
    io:format("Time out: no response~n",[])
  end.

schedule_stabilize() ->
  timer:send_interval(?Stabilize, self(), stabilize).

node(Id, Predecessor, Successor, Store) ->
  % io:format("We are inside the node function with id ~w, Pre ~w, Succ ~w, ~n ", [Id, Predecessor, Successor]),
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);
    {notify, New} ->
      {Pred, CutStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, CutStore);
    {request, Peer} ->
      %  io:format("We are inside request clausule with peer ~w ~n", [Peer]),
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);
    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);
    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);
    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);
    stabilize ->
      %  io:format("Successor in the stabilize clausule in node is ~w ~n", [Successor]),
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);
    debug ->
      io:format("Id is ~w, Predecessor is ~w, Successor is ~w Store is ~w ~n", [Id, Predecessor, Successor, Store]),
      node(Id, Predecessor, Successor, Store);
    {add, Key, Value, Qref, Client} ->
        Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Added);
    {lookup, Key, Qref, Client} ->
        lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
        node(Id, Predecessor, Successor, Store);
    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged)
  end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id)  of
    true  ->
    Client ! {Qref, ok},
    NewStore = storage:add(Key,Value, Store),
    NewStore;
    false ->
    Spid ! {add, Key, Value, Qref, Client},
    Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
  case key:between(Key, Pkey, Id) of
      true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
      false ->
      {_, Spid} = Successor,
      Spid ! {lookup, Key, Qref, Client}
      end.

stabilize({_, Spid}) ->
  %io:format("Spid in stabilize is ~w ~n", [Spid] ),
  Spid ! {request, self()}.

stabilize(Pred, Id, Successor) ->
  %io:format("We are inside the stabilize function with id ~w, Pre ~w, Succ ~w, ~n", [Id, Pred, Successor]),
  {Skey, Spid} = Successor,
  case Pred of
    nil ->
      Spid ! {notify, {Id, self()}},
      Successor;
    {Id, _} ->
      Successor;
    {Skey, _} ->
      Spid !  {notify, {Id, self()}},
      Successor;
    {Xkey, Xpid} ->
      case key:between(Xkey, Id, Skey) of
        true ->
          Xpid ! {notify, {Id, self()}},
          stabilize({Id,self()}, Id, Pred);
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.

request(Peer, Predecessor) ->
  % io:format("We are inside request function with Peer ~w, Predecessor ~w ~n", [Peer, Predecessor]),
  case Predecessor of
    nil ->
      %  io:format("We are inside the nil at for the first time ~n"),
      Peer ! {status, nil};
    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid},Keep} ;
    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id,Store,Nkey,Npid),
          {{Nkey,Npid}, Keep};
        false ->
          {Predecessor,Store}
      end
  end.

handover(Id, Store, Nkey, Npid) ->
      {Rest, Keep} = storage:split(Id, Nkey, Store),
      Npid ! {handover, Rest},
      Keep.

create_probe(Id, {SKey, SPid})->
  Time = erlang:system_time(micro_seconds),
  Nodes = [SKey],
  SPid ! {probe, Id, Nodes, Time}.

forward_probe(Ref, Time, Nodes, Id, {_, Pid})->

  NewList = Nodes ++ [Id],
  Pid ! {probe, Ref, NewList, Time}.

remove_probe(T, Nodes)->
  Time = erlang:system_time(micro_seconds) - T,
  io:format("Ended with the time ~w", [Time]).

