%%%-------------------------------------------------------------------
%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2014 by vlad <lib.aca55a@gmail.com>
%%%-------------------------------------------------------------------
-module(mme_sctp).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% The default port for a listener.
-define(DEFAULT_PORT, 36412).

%% How long a listener with no associations lives before offing
%% itself.
-define(LISTENER_TIMEOUT, 30000).

%% How long to wait for a transport process to attach after
%% association establishment.
-define(ACCEPT_TIMEOUT, 5000).

-record(state, {}).

-type listen_option() :: {accept, inet:ip_address()}.

-type uint() :: non_neg_integer().

%% Accepting/connecting transport process state.
-record(transport,
        {parent  :: pid(),
         socket   :: gen_sctp:sctp_socket(),
         assoc_id :: gen_sctp:assoc_id(),  %% association identifier
         peer     :: {[inet:ip_address()], uint()}, %% {RAs, RP}
         streams  :: {uint(), uint()},     %% {InStream, OutStream} counts
         os = 0   :: uint()}).             %% next output stream

%% Listener process state.
-record(listener,
        {ref       :: reference(),
         socket    :: gen_sctp:sctp_socket(),
         count = 0 :: uint(),
         tmap = ets:new(?MODULE, []) :: ets:tid(),
         %% {MRef, Pid|AssocId}, {AssocId, Pid} Field tmap is used to
         %% map an incoming message or event to the relevent transport
         %% process.
         pending = {0, ets:new(?MODULE, [ordered_set])},
         %% Field pending implements a queue of transport processes to
         %% which an association has been assigned.
         tref      :: reference()}).
                         
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{}}.

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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

