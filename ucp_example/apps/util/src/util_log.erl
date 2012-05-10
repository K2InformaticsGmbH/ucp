-module(util_log).

-export([open_log/1,
         open_log/2,
         log_event/1,
         stamp/0,
         quote/1,
         log_error/2,
         log_info/2,
         log/2,
         log/3,
         escape_quote/1
     ]).

-include("util.hrl").

%% @spec open_log(Name::atom()) -> ok
%% @doc Open the given log file
-spec open_log(atom()) -> ok.
open_log(Name) ->
    case simple_logger:is_open(Name) of
        true -> ok;
        false ->
            open_log(Name, "./"),
            ok
    end.

%% @spec open_log(Name::atom(), Path::string()) -> ok
%% @doc Open the given log file
-spec open_log(atom(), string()) -> ok.
open_log(Name, Path) ->
    case simple_logger:is_open(Name) of
        true -> ok;
        false ->
            simple_logger:open_file(Name, Path),
            ok
    end.

%% @spec log_info(string(), [term()]) -> ok | {error, term()}
%% @doc Log data in the EVENT_LOG file
%% @end
-spec log_info(string(), [term()]) -> ok | {error, term()}.
log_info(Info, Args) ->
    ExtendedInfo = lists:append([stamp(), " - INFO - ", Info]),
    simple_logger:write_log(?INFO_LOG, ExtendedInfo, Args).

%% @spec log_error(string(), [term()]) -> ok | {error, term()}
%% @doc Log data in the ERROR_LOG file
%% @end
-spec log_error(string(), [term()]) -> ok | {error, term()}.
log_error(Error, Args) ->
    ExtendedError = lists:append([stamp(), " - Error - ", Error]),
    simple_logger:write_log(?ERROR_LOG, ExtendedError, Args).

%% @spec log_event([term()]) -> ok | {error, term()}
%% @doc Log data in the EVENT_LOG file
%% @end
-spec log_event([term()]) ->  ok | {error, term()}.
log_event(Data) ->
    simple_logger:write_log(?EVENT_LOG, [stamp() | Data]).

%% @spec log(Name::atom(), [term()]) -> ok | {error, term()}
%% @doc Log data in the Name file
%% @end
-spec log(atom(), [term()]) ->  ok | {error, term()}.
log(Name, Data) ->
    simple_logger:write_log(Name, [stamp() | Data]).

%% @spec log(Name::atom(), Format::string(), [term()]) -> ok | {error, term()}
%% @doc Log data in the Name file
%% @end
-spec log(atom(), string(), [term()]) ->  ok | {error, term()}.
log(Name, Format, Data) ->
    ExtendedFormat = lists:append([stamp(), " - ", Format]),
    simple_logger:write_log(Name, ExtendedFormat, Data).

%% @spec quote(any()) -> any()
%% @doc Returns the same string enclosed between double quote
%% @end
-spec quote(any()) -> any().
quote(String) when is_list(String) ->
    lists:concat(["\"", String, "\""]);
quote(String) ->
    String.

-spec escape_quote(char()) -> string().
escape_quote($\") ->
    "\"\"";
escape_quote(Any) -> [Any].

%% @spec stamp() -> iolist()
%% @doc Returns the localtime as a printable string
%%      Eg. 2010-01-22 18:36:30.038499
%% @end
-spec stamp() -> iolist().
stamp() ->
    {_, _, USec} = Now = erlang:now(),
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time(Now),
    lists:flatten([integer_to_list(Year), "-",
                   string:right(integer_to_list(Month), 2, $0), "-",
                   string:right(integer_to_list(Day), 2, $0), " ",
                   string:right(integer_to_list(Hour), 2, $0), ":",
                   string:right(integer_to_list(Min), 2, $0), ":",
                   string:right(integer_to_list(Sec), 2, $0), ".",
                   string:right(integer_to_list(USec), 6, $0)]).
