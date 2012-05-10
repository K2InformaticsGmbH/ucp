-module(simple_logger).

-behaviour(gen_server).
-export([]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([start_link/2,
         open_file/1,
         open_file/2,
         is_open/1,
         write_log/2,
         write_log/3,
         close/1]).

-record(st, {io_device}).

-spec start_link(atom(), string()) -> {ok, pid()} | {error, term()}.
start_link(FileName, FilePath) ->
    ServRef = server_ref(FileName),
    gen_server:start_link({local, ServRef}, ?MODULE, [FileName, FilePath], []).

-spec init(list()) -> {ok, #st{}}.
init([FileName, FilePath]) ->
    File = filename:join(FilePath, FileName),
    file:make_dir(FilePath),
    {ok, IoDev} = file:open(File, [append]),
    {ok, #st{io_device = IoDev}}.

-spec open_file(atom()) -> {ok, pid()} | {error, term()}.
open_file(FileName) ->
    simple_logger_sup:start_child(FileName, "./").

-spec open_file(atom(), string()) -> {ok, pid()} | {error, term()}.
open_file(FileName, FilePath) ->
    simple_logger_sup:start_child(FileName, FilePath).

-spec is_open(atom()) -> true | false.
is_open(FileName) ->
    ServRef = server_ref(FileName),
    case erlang:whereis(ServRef) of
        undefined -> false;
        _         -> true
    end.

-spec write_log(atom(), list()) -> ok | {error, log_not_open}.
write_log(FileName, Data) ->
    FieldsN = length(Data),
    Format = lists:flatten(lists:duplicate(FieldsN - 1, "~p,")) ++ "~p",
    write_log(FileName, Format, Data).

-spec write_log(atom(), string(), list()) -> ok | {error, log_not_open}.
write_log(FileName, Format, Data) ->
    ServRef = server_ref(FileName),    
    case catch gen_server:call(ServRef, {write, Format, Data}) of
        {'EXIT',{noproc, _}} ->
            {error, log_not_open};
        ok ->
            ok
    end.

-spec close(atom()) -> ok | {error, log_not_open}.
close(FileName) ->
    ServRef = server_ref(FileName),
    case catch gen_server:call(ServRef, close) of
        {'EXIT',{noproc, _}} ->
            {error, log_not_open};
        ok ->
            ok
    end.

%% --------------------------------------------------------------------
%%% gen_server calbacks
%% --------------------------------------------------------------------

-spec handle_call(term(), term(), #st{}) -> term().
handle_call({write, Format, Data}, _From, State) ->
    Entry = io_lib:format(Format ++ "~n" , Data),
    ok = file:write(State#st.io_device, Entry),
    {reply, ok, State};
handle_call(close, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unexpected_request}, State}.

-spec handle_cast(term(), #st{}) -> term().
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #st{}) -> term().
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #st{}) -> ok.
terminate(_Reason, State) ->
    file:close(State#st.io_device),
    ok.

-spec code_change(term(), #st{}, term()) -> term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

server_ref(FileName) ->
    list_to_atom(lists:concat([FileName, "_simple_logger"])).