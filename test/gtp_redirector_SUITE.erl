%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(gtp_redirector_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include("../include/ergw.hrl").
-include("ergw_test_lib.hrl").
-include("ergw_ggsn_test_lib.hrl").

-define(TIMEOUT, 2000).
-define(HUT, ggsn_gn).              %% Handler Under Test

%%%===================================================================
%%% API
%%%===================================================================

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config0) ->
    AppCfg = inject_redirector(ggsn_SUITE:get_test_config()),
    Config = [{handler_under_test, ?HUT},
              {app_cfg, AppCfg}
              | Config0],
    lib_init_per_suite(Config).

inject_redirector(Config) ->
    ModifyIRX = 
        fun({irx, _}) -> 
                IRX = [{type, 'gtp-c'},
                       {ip,  ?TEST_GSN_R}, % 127.0.0.1 -> 127.0.1.1
                       {reuseaddr, true}],
                {irx, IRX};
           (Other) -> Other
        end,
    RRX = {rrx, [{type, 'gtp-c'},
                 {ip,  ?TEST_GSN},
                 {reuseaddr, true},
                 {redirector, [
                               {redirector_ka_timeout, 500},
                               {redirector_nodes, [{inet4, ?TEST_GSN_R, ?GTP1c_PORT, v1}]}
                              ]} ]},
    ModifySockets = 
        fun({sockets, Sockets}) -> {sockets, lists:map(ModifyIRX, [RRX | Sockets])};
           (Other) -> Other
        end,
    lists:map(fun({ergw, Ergw}) -> 
                      {ergw, lists:map(ModifySockets, Ergw)};
                 (Other) -> Other
             end, Config).

end_per_suite(Config) ->
    ok = lib_end_per_suite(Config),
    ok.

all() ->
    [
     invalid_gtp_pdu,
     invalid_gtp_msg,
     simple_pdp_context_request,
     keep_alive
    ].

%%%===================================================================
%%% Tests
%%%===================================================================

init_per_testcase(_, Config) ->
    ct:pal("Sockets: ~p", [gtp_socket_reg:all()]),
    meck_reset(Config),
    Config.

end_per_testcase(_, Config) ->
    Config.

%%--------------------------------------------------------------------
invalid_gtp_pdu() ->
    [{doc, "Test that an invalid PDU is silently ignored "
           "and that the GTP Redirector socket is not crashing"}].
invalid_gtp_pdu(Config) ->
    ggsn_SUITE:invalid_gtp_pdu(Config).

%%--------------------------------------------------------------------
invalid_gtp_msg() ->
    [{doc, "Test that an invalid message is silently ignored"
      " and that the GTP socket is not crashing"}].
invalid_gtp_msg(Config) ->
    ggsn_SUITE:invalid_gtp_msg(Config).

%%--------------------------------------------------------------------
simple_pdp_context_request() ->
    [{doc, "Check simple Create PDP Context and Delete PDP Context sequence "
           "through GTP Redirector"}].
simple_pdp_context_request(Config) ->
    ggsn_SUITE:simple_pdp_context_request(Config).

%%--------------------------------------------------------------------
keep_alive() ->
    [{doc, "All backendd GTP-C should answer on echo_request which sent by timeout"}].
keep_alive(_Config) ->
    Id = [path, irx, {127,0,0,1}, tx, v1, echo_response],
    Cnt0 = get_value(exometer:get_value(Id)),
    timer:sleep(1000),
    Cnt = get_value(exometer:get_value(Id)),
    ?match(true, Cnt > Cnt0),
    ok.

get_value({ok, DPs}) -> proplists:get_value(value, DPs, -1);
get_value(_) -> -1.

%%%===================================================================
%%% Internal functions
%%%===================================================================
