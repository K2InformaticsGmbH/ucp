-module(gen_ucp_server_util).

-export([log_unknown_operation/3,
         log_unknown_result/3,
         log_invalid_session/3,
         log_invalid_trn/3]).


-spec log_unknown_operation(term(), pid(), term()) -> ok.
log_unknown_operation(SrvRef, Session, Ucp) ->
    ok = util_log:log_event([gen_ucp_server_unknown_operation, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Ucp)]),
    ok = util_log:log_error("General UCP server unknown operation - srv_ref: ~p, session: ~p, ucp:~n~p~n", [SrvRef, Session, Ucp]).

-spec log_unknown_result(term(), pid(), term()) -> ok.
log_unknown_result(SrvRef, Session, Ucp) ->
    ok = util_log:log_event([gen_ucp_server_unknown_result, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Ucp)]),
    ok = util_log:log_error("General UCP server unknown result - srv_ref: ~p, session: ~p, ucp:~n~p~n", [SrvRef, Session, Ucp]).

-spec log_invalid_session(term(), pid(), term()) -> ok.
log_invalid_session(SrvRef, Session, Info) ->
    ok = util_log:log_event([gen_ucp_server_invalid_session, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Info)]),
    ok = util_log:log_error("General UCP server invalid session - srv_ref: ~p, session: ~p, info:~n~p~n", [SrvRef, Session, Info]).

-spec log_invalid_trn(term(), pid(), term()) -> ok.
log_invalid_trn(SrvRef, Session, Info) ->
    ok = util_log:log_event([gen_ucp_server_invalid_trn, util:term_to_string(SrvRef), util:term_to_string(Session), util:term_to_string(Info)]),
    ok = util_log:log_error("General UCP server invalid trn - srv_ref: ~p, session: ~p, info:~n~p~n", [SrvRef, Session, Info]).
