%% Copyright 2015, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-module(ggsn_gn_proxy).

-behaviour(gtp_api).

-compile({parse_transform, cut}).

-export([init/2, request_spec/1, handle_request/5, handle_response/5, handle_cast/2]).

-include_lib("gtplib/include/gtp_packet.hrl").
-include("include/ergw.hrl").
-include("gtp_proxy_ds.hrl").

-compile([nowarn_unused_record]).

-define(T3, 10 * 1000).
-define(N3, 5).

%%====================================================================
%% API
%%====================================================================

-record(create_pdp_context_request, {
	  imsi,
	  routeing_area_identity,
	  recovery,
	  selection_mode,
	  tunnel_endpoint_identifier_data_i,
	  tunnel_endpoint_identifier_control_plane,
	  nsapi,
	  linked_nsapi,
	  charging_characteristics,
	  trace_reference,
	  trace_type,
	  end_user_address,
	  apn,
	  pco,
	  sgsn_address_for_signalling,
	  sgsn_address_for_user_traffic,
	  alternative_sgsn_address_for_signalling,
	  alternative_sgsn_address_for_user_traffic,
	  msisdn,
	  quality_of_service_profile,
	  traffic_flow_template,
	  trigger_id,
	  omc_identity,
	  common_flags,
	  apn_restriction,
	  rat_type,
	  user_location_information,
	  ms_time_zone,
	  imei,
	  camel_charging_information_container,
	  additional_trace_info,
	  correlation_id,
	  evolved_allocation_retention_priority_i,
	  extended_common_flags,
	  user_csg_information,
	  ambr,
	  signalling_priority_indication,
	  cn_operator_selection_entity,
	  private_extension,
	  additional_ies
	 }).

-record(create_pdp_context_response, {
	  cause,
	  reordering_required,
	  recovery,
	  tunnel_endpoint_identifier_data_i,
	  tunnel_endpoint_identifier_control_plane,
	  nsapi,
	  charging_id,
	  end_user_address,
	  protocol_configuration_options,
	  ggsn_address_for_control_plane,
	  ggsn_address_for_user_traffic,
	  alternative_sgsn_address_for_control_plane,
	  alternative_sgsn_address_for_user_traffic,
	  quality_of_service_profile,
	  charging_gateway_address,
	  alternative_charging_gateway_address,
	  common_flags,
	  apn_restriction,
	  ms_info_change_reporting_action,
	  bearer_control_mode,
	  evolved_allocation_retention_priority_i,
	  csg_information_reporting_action,
	  apn_ambr_with_nsapi,
	  ggsn_back_off_time,
	  private_extension,
	  additional_ies
	 }).

-record(update_pdp_context_request, {
	  imsi,
	  routeing_area_identity,
	  recovery,
	  tunnel_endpoint_identifier_data_i,
	  tunnel_endpoint_identifier_control_plane,
	  nsapi,
	  trace_reference,
	  trace_type,
	  pco,
	  sgsn_address_for_signalling,
	  sgsn_address_for_user_traffic,
	  alternative_sgsn_address_for_signalling,
	  alternative_sgsn_address_for_user_traffic,
	  quality_of_service_profile,
	  traffic_flow_template,
	  trigger_id,
	  omc_identity,
	  common_flags,
	  rat_type,
	  user_location_information,
	  ms_time_zone,
	  additional_trace_info,
	  direct_tunnel_flags,
	  evolved_allocation_retention_priority_i,
	  extended_common_flags,
	  user_csg_information,
	  ambr,
	  signalling_priority_indication,
	  cn_operator_selection_entity,
	  private_extension,
	  additional_ies
	 }).

-record(update_pdp_context_response, {
	  cause,
	  tunnel_endpoint_identifier_data_i,
	  tunnel_endpoint_identifier_control_plane,
	  charging_id,
	  protocol_configuration_options,
	  ggsn_address_for_control_plane,
	  ggsn_address_for_user_traffic,
	  alternative_ggsn_address_for_control_plane,
	  alternative_ggsn_address_for_user_traffic,
	  quality_of_service_profile,
	  charging_gateway_address,
	  alternative_charging_gateway_address,
	  common_flags,
	  apn_restriction,
	  bearer_control_mode,
	  ms_info_change_reporting_action,
	  evolved_allocation_retention_priority_i,
	  csg_information_reporting_action,
	  ambr,
	  private_extension,
	  additional_ies
	 }).


-record(delete_pdp_context_request, {
	  cause,
	  teardown_ind,
	  nsapi,
	  protocol_configuration_options,
	  user_location_information,
	  ms_time_zone,
	  extended_common_flags,
	  uli_timestamp,
	  private_extension,
	  additional_ies
	 }).

-record(delete_pdp_context_response, {
	  cause,
	  protocol_configuration_options,
	  user_location_information,
	  ms_time_zone,
	  uli_timestamp,
	  private_extension,
	  additional_ies
	 }).

request_spec(create_pdp_context_request) ->
    [{{international_mobile_subscriber_identity, 0},	conditional},
     {{routeing_area_identity, 0},			optional},
     {{recovery, 0},					optional},
     {{selection_mode, 0},				conditional},
     {{tunnel_endpoint_identifier_data_i, 0},		mandatory},
     {{tunnel_endpoint_identifier_control_plane, 0},	conditional},
     {{nsapi, 0},					mandatory},
     {{nsapi, 1},					conditional},
     {{charging_characteristics, 0},			conditional},
     {{trace_reference, 0},				optional},
     {{trace_type, 0},					optional},
     {{end_user_address, 0},				conditional},
     {{access_point_name, 0},				conditional},
     {{protocol_configuration_options, 0},		optional},
     {{gsn_address, 0},					mandatory},
     {{gsn_address, 1},					mandatory},
     {{gsn_address, 2},					conditional},
     {{gsn_address, 3},					conditional},
     {{ms_international_pstn_isdn_number, 0},		conditional},
     {{quality_of_service_profile, 0},			mandatory},
     {{traffic_flow_template, 0},			conditional},
     {{trigger_id, 0},					optional},
     {{omc_identity, 0},				optional},
     {{common_flags, 0},				optional},
     {{apn_restriction, 0},				optional},
     {{rat_type, 0},					optional},
     {{user_location_information, 0},			optional},
     {{ms_time_zone, 0},				optional},
     {{imei, 0},					conditional},
     {{camel_charging_information_container, 0},	optional},
     {{additional_trace_info, 0},			optional},
     {{correlation_id, 0},				optional},
     {{evolved_allocation_retention_priority_i, 0},	optional},
     {{extended_common_flags, 0},			optional},
     {{user_csg_information, 0},			optional},
     {{ambr, 0},					optional},
     {{signalling_priority_indication, 0},		optional},
     {{cn_operator_selection_entity, 0},		optional},
     {{private_extension, 0},				optional}];

request_spec(create_pdp_context_response) ->
    [{{cause, 0},					mandatory},
     {{reordering_required, 0},				conditional},
     {{recovery, 0},					optional},
     {{tunnel_endpoint_identifier_data_i, 0},		conditional},
     {{tunnel_endpoint_identifier_control_plane, 0},	conditional},
     {{nsapi, 0},					optional},
     {{charging_id, 0},					conditional},
     {{end_user_address, 0},				conditional},
     {{protocol_configuration_options, 0},		optional},
     {{gsn_address, 0},					conditional},
     {{gsn_address, 1},					conditional},
     {{gsn_address, 2},					conditional},
     {{gsn_address, 3},					conditional},
     {{quality_of_service_profile, 0},			conditional},
     {{charging_gateway_address, 0},			optional},
     {{charging_gateway_address, 1},			optional},
     {{common_flags, 0},				optional},
     {{apn_restriction, 0},				optional},
     {{ms_info_change_reporting_action, 0},		optional},
     {{bearer_control_mode, 0},				optional},
     {{evolved_allocation_retention_priority_i, 0},	optional},
     {{csg_information_reporting_action, 0},		optional},
     {{apn_ambr_with_nsapi, 0},				optional},
     {{ggsn_back_off_time, 0},				optional},
     {{private_extension, 0},				optional}];

request_spec(update_pdp_context_request) ->
    [{{international_mobile_subscriber_identity, 0},	optional},
     {{routeing_area_identity, 0},			optional},
     {{recovery, 0},					optional},
     {{tunnel_endpoint_identifier_data_i, 0},		mandatory},
     {{tunnel_endpoint_identifier_control_plane, 0},	conditional},
     {{nsapi, 0},					mandatory},
     {{trace_reference, 0},				optional},
     {{trace_type, 0},					optional},
     {{protocol_configuration_options, 0},		optional},
     {{gsn_address, 0},					mandatory},
     {{gsn_address, 1},					mandatory},
     {{gsn_address, 2},					conditional},
     {{gsn_address, 3},					conditional},
     {{quality_of_service_profile, 0},			mandatory},
     {{traffic_flow_template, 0},			conditional},
     {{trigger_id, 0},					optional},
     {{omc_identity, 0},				optional},
     {{common_flags, 0},				optional},
     {{rat_type, 0},					optional},
     {{user_location_information, 0},			optional},
     {{ms_time_zone, 0},				optional},
     {{additional_trace_info, 0},			optional},
     {{direct_tunnel_flags, 0},				optional},
     {{evolved_allocation_retention_priority_i, 0},	optional},
     {{extended_common_flags, 0},			optional},
     {{user_csg_information, 0},			optional},
     {{ambr, 0},					optional},
     {{signalling_priority_indication, 0},		optional},
     {{cn_operator_selection_entity, 0},		optional},
     {{private_extension, 0},				optional}];

request_spec(update_pdp_context_response) ->
    [{{cause, 0},					mandatory},
     {{tunnel_endpoint_identifier_data_i, 0},		conditional},
     {{tunnel_endpoint_identifier_control_plane, 0},	conditional},
     {{charging_id, 0},					conditional},
     {{protocol_configuration_options, 0},		optional},
     {{gsn_address, 0},					conditional},
     {{gsn_address, 1},					conditional},
     {{gsn_address, 2},					conditional},
     {{gsn_address, 3},					conditional},
     {{quality_of_service_profile, 0},			conditional},
     {{charging_gateway_address, 0},			optional},
     {{charging_gateway_address, 1},			optional},
     {{common_flags, 0},				optional},
     {{apn_restriction, 0},				optional},
     {{bearer_control_mode, 0},				optional},
     {{ms_info_change_reporting_action, 0},		optional},
     {{evolved_allocation_retention_priority_i, 0},	optional},
     {{csg_information_reporting_action, 0},		optional},
     {{ambr, 0},					optional},
     {{private_extension, 0},				optional}];

request_spec(delete_pdp_context_request) ->
    [{{cause, 0},					optional},
     {{teardown_ind, 0},				conditional},
     {{nsapi, 0},					mandatory},
     {{protocol_configuration_options, 0},		optional},
     {{user_location_information, 0},			optional},
     {{ms_time_zone, 0},				optional},
     {{extended_common_flags, 0},			optional},
     {{uli_timestamp, 0},				optional},
     {{private_extension, 0},				optional}];

request_spec(delete_pdp_context_response) ->
    [{{cause, 0},					mandatory},
     {{protocol_configuration_options, 0},		optional},
     {{user_location_information, 0},			optional},
     {{ms_time_zone, 0},				optional},
     {{uli_timestamp, 0},				optional},
     {{private_extension, 0},				optional}];

request_spec(_) ->
    [].

-record(request_info, {from, seq_no, new_peer}).
-record(context_state, {nsapi}).

-define(CAUSE_OK(Cause), (Cause =:= request_accepted orelse
			  Cause =:= new_pdp_type_due_to_network_preference orelse
			  Cause =:= new_pdp_type_due_to_single_address_bearer_only)).

init(Opts, State) ->
    ProxyPorts = proplists:get_value(proxy_sockets, Opts),
    ProxyDPs = proplists:get_value(proxy_data_paths, Opts),
    GGSN = proplists:get_value(ggns, Opts),
    {ok, State#{proxy_ports => ProxyPorts, proxy_dps => ProxyDPs, ggsn => GGSN}}.

handle_cast({path_restart, Path},
	    #{context := #context{path = Path} = Context,
	      proxy_context := ProxyContext
	     } = State) ->

    RequestIEs0 = [#cause{value = request_accepted},
		   #teardown_ind{value = 1},
		   #nsapi{nsapi = Context#context.state#context_state.nsapi}],
    RequestIEs = gtp_v1_c:build_recovery(ProxyContext, false, RequestIEs0),
    send_request(ProxyContext, ?T3, ?N3, RequestIEs),

    dp_delete_pdp_context(Context, ProxyContext),

    {stop, normal, State};

handle_cast({path_restart, Path},
	    #{context := Context,
	      proxy_context := #context{path = Path} = ProxyContext
	     } = State) ->

    RequestIEs0 = [#cause{value = request_accepted},
		   #teardown_ind{value = 1},
		   #nsapi{nsapi = Context#context.state#context_state.nsapi}],
    RequestIEs = gtp_v1_c:build_recovery(Context, false, RequestIEs0),
    send_request(Context, ?T3, ?N3, RequestIEs),

    dp_delete_pdp_context(Context, ProxyContext),

    {stop, normal, State};

handle_cast({path_restart, _Path}, State) ->
    {noreply, State}.

handle_request(_From, _Msg, _Req, true, State) ->
%% resent request
    {noreply, State};

handle_request(From,
	       #gtp{seq_no = SeqNo, ie = IEs} = Request,
	       #create_pdp_context_request{imsi = IMSIie, recovery = Recovery,
					   apn = APNie}, _Resent,
	       #{tei := LocalTEI, gtp_port := GtpPort, gtp_dp_port := GtpDP,
		 proxy_ports := ProxyPorts, proxy_dps := ProxyDPs, ggsn := GGSN} = State0) ->

    APN = optional_apn_value(APNie, undefined),

    Context0 = init_context(APN, GtpPort, LocalTEI, GtpDP, LocalTEI),
    Context1 = update_context_from_gtp_req(Request, Context0),
    Context = gtp_path:bind(Recovery, Context1),
    State1 = State0#{context => Context},

    #gtp_port{ip = LocalCntlIP} = GtpPort,
    Session0 = #{'GGSN-Address' => gtp_c_lib:ip2bin(LocalCntlIP)},
    Session1 = init_session(IEs, Session0),
    lager:debug("Invoking CONTROL: ~p", [Session1]),
    %% ergw_control:authenticate(Session1),

    ProxyGtpPort = gtp_socket_reg:lookup(hd(ProxyPorts)),
    ProxyGtpDP = gtp_socket_reg:lookup(hd(ProxyDPs)),
    {ok, ProxyLocalTEI} = gtp_c_lib:alloc_tei(ProxyGtpPort),

    lager:debug("ProxyGtpPort: ~p", [lager:pr(ProxyGtpPort, ?MODULE)]),
    ProxyContext0 = init_context(APN, ProxyGtpPort, ProxyLocalTEI, ProxyGtpDP, ProxyLocalTEI),
    ProxyContext1 = ProxyContext0#context{
		      remote_control_ip = GGSN,
		      remote_data_ip    = GGSN,
		      state             = Context#context.state
		     },
    ProxyContext = gtp_path:bind(undefined, ProxyContext1),
    State = State1#{proxy_context => ProxyContext},

    IMSI = optional_imsi_value(IMSIie, undefined),
    {ok, ProxyInfo} = gtp_proxy_ds:map(APN, IMSI),
    ProxyReq0 = build_context_request(ProxyContext, ProxyInfo, Request),
    ProxyReq = build_recovery(ProxyContext, false, ProxyReq0),
    forward_request(ProxyContext, ProxyReq, From, SeqNo, Recovery /= undefined),

    {noreply, State};

handle_request(From,
	       #gtp{seq_no = SeqNo} = Request,
	       #update_pdp_context_request{imsi = IMSIie,
					   recovery = Recovery}, _Resent,
	       #{context := OldContext, proxy_context := ProxyContext0} = State0) ->

    Context0 = update_context_from_gtp_req(Request, OldContext),
    Context = gtp_path:bind(Recovery, Context0),
    State = apply_context_change(Context, OldContext, State0),

    ProxyContext = gtp_path:bind(undefined, ProxyContext0),

    #context{apn = APN} = ProxyContext,
    IMSI = optional_imsi_value(IMSIie, undefined),
    {ok, ProxyInfo} = gtp_proxy_ds:map(APN, IMSI),
    ProxyReq0 = build_context_request(ProxyContext, ProxyInfo, Request),
    ProxyReq = build_recovery(ProxyContext, false, ProxyReq0),
    forward_request(ProxyContext, ProxyReq, From, SeqNo, Recovery /= undefined),

    {noreply, State#{context := Context, proxy_context := ProxyContext}};

handle_request(From,
	       #gtp{type = delete_pdp_context_request, seq_no = SeqNo} = Request, _ReqRec, _Resent,
	       #{proxy_context := ProxyContext} = State) ->
    ProxyReq = build_context_request(ProxyContext, undefined, Request),
    forward_request(ProxyContext, ProxyReq, From, SeqNo, false),

    {noreply, State};

handle_request({GtpPort, _IP, _Port}, Msg, _ReqRec, _Resent, State) ->
    lager:warning("Unknown Proxy Message on ~p: ~p", [GtpPort, lager:pr(Msg, ?MODULE)]),
    {noreply, State}.

handle_response(#request_info{from = From, seq_no = SeqNo, new_peer = NewPeer}, Response,
		#create_pdp_context_response{cause = #cause{value = Cause},
					     recovery = Recovery}, _Request,
		#{context := Context,
		  proxy_context := ProxyContext0} = State) ->
    lager:warning("OK Proxy Response ~p", [lager:pr(Response, ?MODULE)]),

    ProxyContext1 = update_context_from_gtp_req(Response, ProxyContext0),
    ProxyContext = gtp_path:bind(Recovery, ProxyContext1),

    GtpResp0 = build_context_request(Context, undefined, Response),
    GtpResp = build_recovery(Context, NewPeer, GtpResp0),
    gtp_context:send_response(From, GtpResp#gtp{seq_no = SeqNo}),

    if ?CAUSE_OK(Cause) ->
	    dp_create_pdp_context(Context, ProxyContext),
	    lager:info("Create PDP Context ~p", [Context]),

	    {noreply, State#{proxy_context => ProxyContext}};

       true ->
	    {stop, State}
    end;

handle_response(#request_info{from = From, seq_no = SeqNo, new_peer = NewPeer},
		#gtp{type = update_pdp_context_response} = Response, _RespRec, _Request,
		#{context := Context,
		  proxy_context := OldProxyContext} = State0) ->
    lager:warning("OK Proxy Response ~p", [lager:pr(Response, ?MODULE)]),

    ProxyContext = update_context_from_gtp_req(Response, OldProxyContext),
    State = apply_proxy_context_change(ProxyContext, OldProxyContext, State0),

    GtpResp0 = build_context_request(Context, undefined, Response),
    GtpResp = build_recovery(Context, NewPeer, GtpResp0),
    gtp_context:send_response(From, GtpResp#gtp{seq_no = SeqNo}),

    dp_update_pdp_context(Context, ProxyContext),

    {noreply, State};

handle_response(#request_info{from = From, seq_no = SeqNo},
		#gtp{type = delete_pdp_context_response} = Response, _RespRec, _Request,
		#{context := Context,
		  proxy_context := ProxyContext} = State) ->
    lager:warning("OK Proxy Response ~p", [lager:pr(Response, ?MODULE)]),

    GtpResp = build_context_request(Context, undefined, Response),
    gtp_context:send_response(From, GtpResp#gtp{seq_no = SeqNo}),

    dp_delete_pdp_context(Context, ProxyContext),
    {stop, State};

handle_response(_ReqInfo, Response, _RespRec, _Req, State) ->
    lager:warning("Unknown Proxy Response ~p", [lager:pr(Response, ?MODULE)]),
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_context_change(NewContext0, OldContext, State)
  when NewContext0 /= OldContext ->
    NewContext = gtp_path:bind(NewContext0),
    gtp_path:unbind(OldContext),
    State#{context => NewContext};
apply_context_change(_NewContext, _OldContext, State) ->
    State.

apply_proxy_context_change(NewContext0, OldContext, State)
  when NewContext0 /= OldContext ->
    NewContext = gtp_path:bind(NewContext0),
    gtp_path:unbind(OldContext),
    State#{proxy_context => NewContext};
apply_proxy_context_change(_NewContext, _OldContext, State) ->
    State.

init_session(IEs, Session) ->
    lists:foldr(fun copy_to_session/2, Session, IEs).

%% copy_to_session(#international_mobile_subscriber_identity{imsi = IMSI}, Session) ->
%%     Id = [{'Subscription-Id-Type' , 1}, {'Subscription-Id-Data', IMSI}],
%%     Session#{'Subscription-Id' => Id};

copy_to_session(#international_mobile_subscriber_identity{imsi = IMSI}, Session) ->
    Session#{'IMSI' => IMSI};
copy_to_session(#ms_international_pstn_isdn_number{
		   msisdn = {isdn_address, _, _, 1, MSISDN}}, Session) ->
    Session#{'MSISDN' => MSISDN};
copy_to_session(#gsn_address{instance = 0, address = IP}, Session) ->
    Session#{'SGSN-Address' => gtp_c_lib:ip2bin(IP)};
copy_to_session(#rat_type{rat_type = Type}, Session) ->
    Session#{'RAT-Type' => Type};
copy_to_session(#selection_mode{mode = Mode}, Session) ->
    Session#{'Selection-Mode' => Mode};

copy_to_session(_, Session) ->
    Session.

init_context(APN, CntlPort, CntlTEI, DataPort, DataTEI) ->
    #context{
       apn               = APN,
       version           = v1,
       control_interface = ?MODULE,
       control_port      = CntlPort,
       local_control_tei = CntlTEI,
       data_port         = DataPort,
       local_data_tei    = DataTEI,
       state             = #context_state{}
      }.

get_context_from_req(#gsn_address{instance = 0, address = CntlIP}, Context) ->
    Context#context{remote_control_ip = gtp_c_lib:bin2ip(CntlIP)};
get_context_from_req(#gsn_address{instance = 1, address = DataIP}, Context) ->
    Context#context{remote_data_ip = gtp_c_lib:bin2ip(DataIP)};
get_context_from_req(#tunnel_endpoint_identifier_data_i{instance = 0, tei = DataTEI}, Context) ->
    Context#context{remote_data_tei = DataTEI};
get_context_from_req(#tunnel_endpoint_identifier_control_plane{instance = 0, tei = CntlTEI}, Context) ->
    Context#context{remote_control_tei = CntlTEI};
get_context_from_req(#nsapi{instance = 0, nsapi = NSAPI}, #context{state = State} = Context) ->
    Context#context{state = State#context_state{nsapi = NSAPI}};
get_context_from_req(_, Context) ->
    Context.

update_context_from_gtp_req(#gtp{ie = IEs}, Context) ->
    lists:foldl(fun get_context_from_req/2, Context, IEs).

set_context_from_req(#context{control_port = #gtp_port{ip = CntlIP}},
		     #gsn_address{instance = 0} = IE) ->
    IE#gsn_address{address = gtp_c_lib:ip2bin(CntlIP)};
set_context_from_req(#context{data_port = #gtp_port{ip = DataIP}},
		     #gsn_address{instance = 1} = IE) ->
    IE#gsn_address{address = gtp_c_lib:ip2bin(DataIP)};
set_context_from_req(#context{local_data_tei = DataTEI},
		     #tunnel_endpoint_identifier_data_i{instance = 0} = IE) ->
    IE#tunnel_endpoint_identifier_data_i{tei = DataTEI};
set_context_from_req(#context{local_control_tei = CntlTEI},
		     #tunnel_endpoint_identifier_control_plane{instance = 0} = IE) ->
    IE#tunnel_endpoint_identifier_control_plane{tei = CntlTEI};
set_context_from_req(_, IE) ->
    IE.

update_gtp_req_from_context(Context, GtpReqIEs) ->
    lists:map(set_context_from_req(Context, _), GtpReqIEs).

proxy_request_nat(#proxy_info{apn = APN},
		  #access_point_name{instance = 0} = IE)
  when is_list(APN) ->
    IE#access_point_name{apn = APN};

proxy_request_nat(#proxy_info{imsi = IMSI},
		  #international_mobile_subscriber_identity{instance = 0} = IE)
  when is_binary(IMSI) ->
    IE#international_mobile_subscriber_identity{imsi = IMSI};

proxy_request_nat(#proxy_info{msisdn = MSISDN},
		  #ms_international_pstn_isdn_number{instance = 0} = IE)
  when is_binary(MSISDN) ->
    IE#ms_international_pstn_isdn_number{msisdn = {isdn_address, 1, 1, 1, MSISDN}};

proxy_request_nat(_ProxyInfo, IE) ->
    IE.

apply_proxy_request_nat(ProxyInfo, GtpReqIEs) ->
    lists:map(proxy_request_nat(ProxyInfo, _), GtpReqIEs).

build_context_request(#context{remote_control_tei = TEI} = Context,
		      ProxyInfo, #gtp{ie = RequestIEs} = Request) ->
    ProxyIEs0 = lists:keydelete(recovery, 1, RequestIEs),
    ProxyIEs1 = apply_proxy_request_nat(ProxyInfo, ProxyIEs0),
    ProxyIEs = update_gtp_req_from_context(Context, ProxyIEs1),
    Request#gtp{tei = TEI, ie = ProxyIEs}.

send_request(#context{control_port = GtpPort,
		      remote_control_tei = RemoteCntlTEI,
		      remote_control_ip = RemoteCntlIP},
	     T3, N3, RequestIEs) ->
    Msg = #gtp{version = v1, tei = RemoteCntlTEI, ie = RequestIEs},
    gtp_context:send_request(GtpPort, RemoteCntlIP, T3, N3, Msg, undefined).

forward_request(#context{control_port = GtpPort, remote_control_ip = RemoteCntlIP},
	       Request, From, SeqNo, NewPeer) ->
    ReqInfo = #request_info{from = From, seq_no = SeqNo, new_peer = NewPeer},
    lager:debug("Invoking Context Send Request: ~p", [Request]),
    gtp_context:send_request(GtpPort, RemoteCntlIP, Request, ReqInfo).

proxy_dp_args(#context{data_port = #gtp_port{name = Name},
		       local_data_tei = LocalTEI,
		       remote_data_tei = RemoteTEI,
		       remote_data_ip = RemoteIP}) ->
    {forward, [Name, RemoteIP, LocalTEI, RemoteTEI]}.

dp_create_pdp_context(GrxContext, FwdContext) ->
    Args = proxy_dp_args(FwdContext),
    gtp_dp:create_pdp_context(GrxContext, Args).

dp_update_pdp_context(GrxContext, FwdContext) ->
    Args = proxy_dp_args(FwdContext),
    gtp_dp:update_pdp_context(GrxContext, Args).

dp_delete_pdp_context(GrxContext, FwdContext) ->
    Args = proxy_dp_args(FwdContext),
    gtp_dp:delete_pdp_context(GrxContext, Args).

optional_apn_value(#access_point_name{apn = APN}, _) ->
    APN;
optional_apn_value(_, Default) ->
    Default.

optional_imsi_value(#international_mobile_subscriber_identity{imsi = IMSI}, _) ->
    IMSI;
optional_imsi_value(_, Default) ->
    Default.

build_recovery(Context, NewPeer, #gtp{ie = IEs} = Request) ->
    Request#gtp{ie = gtp_v1_c:build_recovery(Context, NewPeer, IEs)}.
