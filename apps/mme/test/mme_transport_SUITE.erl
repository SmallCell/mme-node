%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(mme_transport_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-include_lib("kernel/include/inet_sctp.hrl").
-include("mme.hrl").

%% Receive a message.
-define(RECV(Pat, Ret), receive Pat -> Ret end).
-define(RECV(Pat), ?RECV(Pat, now())).

%% Sockets are opened on the loopback address.
-define(ADDR, {127,0,0,1}).

%% The default port for a listener.
-define(DEFAULT_PORT, 36412).

-define(SCTP_OPTS, [binary, {active, true}, {sctp_initmsg, ?SCTP_INIT}]).

%% Request a specific number of streams just because we can.
-define(SCTP_INIT, #sctp_initmsg{num_ostreams = 5,
                                 max_instreams = 5}).

%% Messages from gen_sctp.
-define(SCTP(Sock, Data), {sctp, Sock, _, _, Data}).


suite() ->
    [{timetrap,{seconds,20}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

groups() ->
    [{all, [], tc()}].

tc() ->
    [sctp_accept,
     sctp_connect,     
     reconnect].

all() -> 
    [start,
     %% {group, all},
     {group, all, [parallel]},
     stop].

start(_Config) ->
    {ok, _} = nano_trace:start([mme], "/tmp/mme.trace"),
    ok = mme:start(),
    nano_trace:add_app(mme),
    nano_trace:add_app(?MODULE),
    nano_trace:filter(allF),
    ct:print(">> ~p\n", [nano_trace:print_applications()]),

    ok.
    

stop(_Config) ->
    ok = mme:stop(),
    nano_trace:stop().    


%%--------------------------------------------------------------------
sctp_connect(_Config) ->
    %% Connect, send a message and receive it back.
    {ok, Sock} = gen_connect(?DEFAULT_PORT),

    Bin = make_msg(),
    ok = gen_send(Sock, Bin),
    Bin = gen_recv(Sock).

sctp_accept(_Config) ->
    %% Open a listening socket and publish the port number.
    {ok, LSock} = gen_listen(),
    {ok, PortNr} = inet:port(LSock),
    
    %% Accept a connection, receive a message and send it back.
    {ok, Sock} = gen_accept(LSock),
    Bin = gen_recv(Sock),
    ok = gen_send(Sock, Bin).

reconnect(_Config) ->
    ok.

%%--------------------------------------------------------------------
gen_listen() ->
    {ok, Sock} = gen_sctp:open([{ip, ?ADDR}, {port, ?DEFAULT_PORT} | ?SCTP_OPTS]),
    {gen_sctp:listen(Sock, true), Sock}.

gen_connect(PortNr) ->
    {ok, Sock} = Ok = gen_sctp:open([{ip, ?ADDR}, {port, 0} | ?SCTP_OPTS]),
    ok = gen_sctp:connect_init(Sock, ?ADDR, PortNr, []),
    Ok = gen_accept(Sock).

gen_accept(Sock) ->
    Assoc = ?RECV(?SCTP(Sock, {_, #sctp_assoc_change{state = comm_up,
                                                     outbound_streams = O,
                                                     inbound_streams = I,
                                                     assoc_id = A}}),
                  {O, I, A}),
    putr(assoc, Assoc),
    {ok, Sock}.

gen_send(Sock, Bin) ->
    {OS, _IS, Id} = getr(assoc),
    {_, _, Us} = now(),
    gen_sctp:send(Sock, Id, Us rem OS, Bin).

gen_recv(Sock) ->
    {_OS, _IS, Id} = getr(assoc),
    ?RECV(?SCTP(Sock, {[#sctp_sndrcvinfo{assoc_id = Id}], Bin}), Bin).

make_msg() ->
    N = 1024,
    Bin = crypto:rand_bytes(4*N),
    Len = 4*(N+1),
    <<1:8, Len:24, Bin/binary>>.


putr(Key, Val) ->
    put({?MODULE, Key}, Val).

getr(Key) ->
    get({?MODULE, Key}).
