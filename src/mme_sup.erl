
-module(mme_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, %% supervisor start
        tree/0         %% supervision tree
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-define(CHILDREN, [mme_sctp_sup]).

-define(TABLES, [{mme_config,   [bag, {keypos, 2}]}]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ets_new(?TABLES),
    Flags = {one_for_one, 1, 5},
    ChildSpecs = [ ?CHILD(Ch, supervisor) || Ch <- ?CHILDREN],
    {ok, {Flags, ChildSpecs}}.


ets_new(List) ->
    lists:foreach(fun ({Table, Opts}) ->                         
                          ets:new(Table, [named_table, public | Opts])
                  end,
                  List).

%% tree/0

tree() ->
    [{?MODULE, whereis(?MODULE), tree(?MODULE)}].

tree(Sup) ->
    lists:map(fun t/1, supervisor:which_children(Sup)).

t({Name, Pid, supervisor, _}) ->
    t(Name, Pid, tree(Pid));
t({Name, Pid, worker, _}) ->
    t(Name, Pid).

t(undefined, Pid, Children) ->
    {Pid, Children};
t(Name, Pid, Children) ->
    {Name, Pid, Children}.

t(undefined, Pid) ->
    Pid;
t(Name, Pid) ->
    {Name, Pid}.


