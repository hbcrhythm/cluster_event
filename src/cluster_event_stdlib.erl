%%%-------------------------------------------------------------------
%% @doc cluster event stdlib
%% @author labihbc@gamil.com
%% @end
%%%-------------------------------------------------------------------
-module(cluster_event_stdlib).

-export([event_add/3, event_add/4, event_del/2, event_del/3, event_trigger/2, event_trigger/3]).
-export([init/1, event2_add/3, event2_add/4, event2_del/2, event2_del/3, event2_trigger/2, event2_trigger/3]).

%% @doc Interface of event process 
event_add(Name, Id, CallBackMfa) ->
	cluster_event:add(Name, Id, CallBackMfa).
event_add(Name, Id, CallBackMfa, IsOnce) ->
	cluster_event:add(Name, Id, CallBackMfa, IsOnce).

event_del(Name, Id) ->
	cluster_event:del(Name, Id).
event_del(Name, Id, CallBackMfa) ->
	cluster_event:del(Name, Id, CallBackMfa).

event_trigger(Name, Id) ->
	cluster_event:trigger(Name, Id).
event_trigger(Name, Id, ExtraParams) ->
	cluster_event:trigger(Name, Id, ExtraParams).

init(Name) ->
	cluster_event2:init(Name).
%% @doc Interface of event dict
event2_add(Name, Id, CallBackMfa) ->
	cluster_event2:add(Name, Id, CallBackMfa).
event2_add(Name, Id, CallBackMfa, IsOnce) ->
	cluster_event2:add(Name, Id, CallBackMfa, IsOnce).

event2_del(Name, Id) ->
	cluster_event2:del(Name, Id).
event2_del(Name, Id, CallBackMfa) ->
	cluster_event2:del(Name, Id, CallBackMfa).

event2_trigger(Name, Id) ->
	cluster_event2:trigger(Name, Id).
event2_trigger(Name, Id, ExtraParams) ->
	cluster_event2:trigger(Name, Id, ExtraParams).
