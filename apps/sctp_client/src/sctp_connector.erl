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
-define(SCTP_INIT, #sctp_initmsg{num_ostreams = 5,
                                 max_instreams = 5}).
-define(SCTP_OPTS, [binary, {recbuf,65536}, {reuseaddr, true}, {sctp_initmsg, ?SCTP_INIT}]).

-record(state, {
          connector :: gen_sctp:sctp_socket(),  % Connector socket
          assoc_id  :: any(),                   % Initiator process
          handler  :: pid(),                    % Current handling process
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
    gen_server:cast(Pid, diconnect).

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
handle_call({connect, IP, Port}, _From, #state{connector=Connect_socket}=State) ->
    Opts = [{active, once} | ?SCTP_OPTS],
    ok = gen_sctp:connect_init(Connect_socket, IP, Port, Opts),
    {reply, ok, State};
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
handle_cast(disconnect, #state{handler=Pid} = State) when is_pid(Pid) ->
    ?WARNING("disconnect:  ~p",[State]),
%    exit(Pid, ok),
    {stop, normal, State#state{handler=undefined}};
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
    try
        %% {ok, CliSocket} = gen_sctp:peeloff(ConnectorSock, AssocId),
        {ok, Pid} = sctp_client_app:start_server_peer(),
        link(Pid),
        gen_sctp:controlling_process(ConnectorSock, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, ConnectorSock, AssocId),

        {noreply, State#state{assoc_id=AssocId, handler = Pid}}
    catch exit:Why ->
        ?ERROR("Error in async connect: ~p.\n", [Why]),
        {stop, Why, State}
    end;
handle_info({sctp, _Sock, _RA, _RP,
             {[], #sctp_assoc_change{state = cant_assoc,
                                     assoc_id = AssocId}}},
            State) ->
    ?WARNING("~p Can't Assoc: ~p.\n", [self(),AssocId]),
    {stop, ok, State};                          %FIXME: add retry

%% peer handler crached and need to be restarted
handle_info({'EXIT',Pid,shutdown}, #state{handler=Pid} = State) when is_pid(Pid)->
    ?WARNING("Peer handler exited: ~p in state: ~p.\n", [Pid, State]),
    %% gen_sctp:controlling_process(State#state.connector, self()),
    %% %% FIXME: reconnect
    {noreply, State, ?RECONNECT_TIMEOUT};
%% peer terminated on disconnect
handle_info({'EXIT',Pid,shutdown}, State) ->
    ?WARNING("Peer handler terminated: ~p in state: ~p.\n", [Pid, State]),
    {noreply, State};
%% handle_info({'EXIT',Pid,shutdown}, State) ->
%%     ?WARNING("Peer handler terminated: ~p in state: ~p.\n", [Pid, State]),
%%     {noreply, State};

handle_info(_Info, State) ->
    ?ERROR(">> Unhandled: ~p in state: ~p.\n", [_Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
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
