%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2015, vlad
%%% @doc
%%%
%%% @end
%%% Created :  2 Jan 2015 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(sctp_connector).

-behaviour(gen_server).

%% API
-export([start_link/1, connect/3, disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

-include_lib("mme/include/mme_logger.hrl").

-define(SERVER, ?MODULE).

-define(RECONNECT_TIMEOUT, 200).

%% Request a specific number of streams just because we can.
-define(SCTP_INIT, #sctp_initmsg{num_ostreams = 2,
                                 max_instreams = 2}).
-define(SCTP_OPTS, [binary, {recbuf,65536}, {reuseaddr, true}, {sctp_initmsg, ?SCTP_INIT}]).

-record(state, {
          connector :: gen_sctp:sctp_socket(),  % Connector socket
          ip,
          port,
          worker    :: pid(),                   % Current handling process
          opts      :: list(),
%          worker_status = free :: atom(),
          module    :: atom()                   % FSM handling module
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()}.
start_link(Module) when is_atom(Module) ->
    gen_server:start_link(?MODULE, [Module], []).

%%--------------------------------------------------------------------
%% @doc
%% Attmpt peer connection
%% @end
%%--------------------------------------------------------------------
-spec connect(pid(),inet:ip_address(),inet:port_number()) -> {ok, pid()}.
connect(Pid, IP, Port) when is_integer(Port) ->
    gen_server:call(Pid, {connect, IP, Port}).

%%--------------------------------------------------------------------
%% @doc
%% Disconnect
%% @end
%%--------------------------------------------------------------------
-spec disconnect(pid()) -> ok.
disconnect(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, disconnect).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module]) ->
    process_flag(trap_exit, true),              % Make suppervisor polite
    %%Create connecting socket
    case gen_sctp:open() of
        {ok, Connect_socket} ->
            {ok, #state{connector = Connect_socket,
                        module   = Module}};
        {error, Reason} ->
            {stop, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({connect, IP, Port}, _From, State) ->
    case schedule(connect, State#state{ip = IP, port = Port}) of
        {ok, S} -> {noreply, S};
        {error, Reason} -> {stop, Reason, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(disconnect, #state{worker=Pid} = State) when is_pid(Pid) ->
    ?WARNING("disconnect:  ~p",[State]),
    exit(Pid, normal),
    {stop, normal, State};
handle_cast(disconnect, State) ->    
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({sctp, _Sock, _RA, _RP,
             {[], #sctp_assoc_change{state = comm_up,
                                     assoc_id = AssocId}}},
            #state{connector=ConnectorSock, module=Module} = State) ->
        {ok, Pid} = sctp_client_app:start_server_peer(),
        link(Pid),
        gen_sctp:controlling_process(ConnectorSock, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, ConnectorSock, AssocId),
        {noreply, State#state{connector=undefined,worker = Pid}};
handle_info({sctp, _Sock, _RA, _RP,
             {[], #sctp_assoc_change{state = cant_assoc}}},
            State=#state{connector=ConnectorSock,ip=IP}) ->
    ?WARNING("~p Can't Assoc: ~p.\n", [self(),IP]),
    case schedule(cant_assoc, State) of
        {ok, S} ->
            inet:setopts(ConnectorSock, [{active, once}, binary]),
            {noreply, S};
        {error, Reason} -> {stop, Reason, State}
    end;
%% Connection timeout
handle_info({timeout, TimerRef, From}, State=#state{connector=ConnectorSock}) ->
    ?WARNING("~p Timeout: ~p.\n", [TimerRef,From]),    
    case schedule(timeout, State) of
        {ok, S} ->
            inet:setopts(ConnectorSock, [{active, once}, binary]),
            {noreply, S};
        {error, Reason} -> {stop, Reason, State}
    end;
%% peer worker exited and need to be restarted
handle_info({'EXIT',Pid,_Reason}, State=#state{worker=Pid}) when is_pid(Pid)->
    ?WARNING("Peer worker exited: ~p in state: ~p.\n", [Pid, State]),
    case schedule(reconnect, State#state{worker = undefined}) of
        {ok, S} -> {noreply, S};
        {error, Reason} -> {stop, {worker,Reason}, State}
    end;
handle_info(_Info, State) ->
    ?ERROR(">> Unhandled: ~p in state: ~p.\n", [_Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
schedule(connect, State=#state{connector=Connect_socket, 
                               ip = IP, port = Port,
                               opts=_Opts}) ->   
    SctpOpts = [{active, once} | ?SCTP_OPTS],
    ok = gen_sctp:connect_init(Connect_socket, IP, Port, SctpOpts),
    {ok, State};
schedule(timeout, State=#state{opts=Opts}) ->
    NewOpts = Opts,
    schedule(connect, State#state{opts=NewOpts});
schedule(reconnect, State=#state{worker=undefined}) -> 
    case gen_sctp:open() of
        {ok, Connect_socket} ->
            schedule(connect, State#state{connector = Connect_socket});
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%%--------------------------------------------------------------------
terminate(_Reason, State) ->   
    gen_sctp:close(State#state.connector),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
