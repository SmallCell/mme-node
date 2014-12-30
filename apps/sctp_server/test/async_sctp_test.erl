-module(async_sctp_test).

-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit_fsm/include/eunit_seq_trace.hrl").
  
%% Receive a message.
-define(RECV(Pat, Ret), receive Pat -> Ret end).
-define(RECV(Pat), ?RECV(Pat, now())).
 
-define(DEF_PORT, 4444).
-define(DEF_HOST, {127,0,0,1}).

-compile(export_all).
 
acceptor() ->
    acceptor(?DEF_HOST, ?DEF_PORT).

    
acceptor(IP, Port) ->
    ?testTraceItit(44, ['receive', print, timestamp, send]),
    ?testTracePrint(44,"acceptor init"),

    {ok,S} = gen_sctp:open(Port, [{recbuf,65536}, {ip,IP}, {reuseaddr, true}, {active, false}]), 
    ok     = gen_sctp:listen(S, true),

    acceptor_loop(S).

acceptor_loop(S) ->    
    case gen_sctp:recv(S) of
        {error, Error} ->
            ?debugFmt("ACPT SCTP RECV ERROR: ~p~n", [Error]);
        %% {sctp, S, Host,  Port,
        %%  {[],#sctp_assoc_change{state = comm_up,error = 0, 
        %%                         outbound_streams = OS,
        %%                         inbound_streams = IS,
        %%                         assoc_id = A}}} ->
        %% {ok,{{127,0,0,1}, 33302,[],
        %%      {sctp_assoc_change,comm_up,0,10,5,168}}}
        {ok,{H, FromP, [], #sctp_assoc_change{state = comm_up,
                                              error = 0, 
                                              outbound_streams = OS,
                                              inbound_streams = IS,
                                              assoc_id = A}}} ->
            {ok, CliSocket} = gen_sctp:peeloff(S, A),
            ?debugFmt("ACPT Assoc up: ~p~n", [A]), 
            ?debugFmt(">> S: ~p, C: ~p",[S,CliSocket]),
            Pid = spawn_link(?MODULE, svc_loop, [CliSocket]),
            gen_sctp:controlling_process(CliSocket, Pid),
            inet:setopts(CliSocket, [{active, once}, binary]),            
            ok;
        Data ->
            ?debugFmt("ACPT Received: ~p~n", [Data])
    end,
    acceptor_loop(S).

svc_loop(S) -> 
    %% case gen_sctp:recv(S) of 
    receive
        {error, Error} ->
             ?debugFmt("SVC RECV ERROR: ~p~n", [Error]);
        Data -> 
              inet:setopts(S, [{active, once}]),
            ?debugFmt("SVC Received: ~p~n", [Data])
    end,
    svc_loop(S).

client() ->
    client(?DEF_HOST, ?DEF_PORT).

client(PORT) ->
    client(?DEF_HOST, PORT).
 
client(Host, Port) when is_integer(Port) ->
    {ok,S} = gen_sctp:open(), 
    {ok,Assoc} = gen_sctp:connect 
          (S, Host, Port, [{sctp_initmsg,#sctp_initmsg{num_ostreams=5}}]),
      ?debugFmt("Connection Successful, Assoc=~p~n", [Assoc]),
      
      ?debugVal(gen_sctp:send(S, Assoc, 0, <<"Test 0">>)),
      %% timer:sleep(10000),
      %% ?debugVal(gen_sctp:send(S, Assoc, 5, <<"Test 5">>)),
      %% ?debugVal(gen_sctp:abort(S, Assoc)),
     
    timer:sleep(10000),
      gen_sctp:close(S). 


logic() ->
    ?assertEqual(ok, ok),
    Pid = spawn(?MODULE, acceptor, [?DEF_HOST, ?DEF_PORT]),
    timer:sleep(1000),
    client(?DEF_HOST, ?DEF_PORT),
    timer:sleep(1000),
    exit(Pid, ok ).
 
async_sctp_setup_test_() -> 
   {timeout, 5000, 
     {setup,
      fun setup/0,
      fun teardown/1,
         [{"Start/stop SCTP server common logic", fun logic/0}]}}.
 
setup() ->
    Pid = spawn(eunit_seq_trace,tracer,[]),
    seq_trace:set_system_tracer(Pid), % set Pid as the system tracer
    Pid.
 
teardown(TracerPid) ->
    exit(TracerPid, ok).
