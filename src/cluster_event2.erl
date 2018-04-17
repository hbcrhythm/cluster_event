%%%-------------------------------------------------------------------
%% @doc cluster event system2, use dict to implement
%% @author labihbc@gamil.com
%% @end
%%%-------------------------------------------------------------------
-module(cluster_event2).
-export([init/1, add/3, add/4, del/2, del/3, trigger/2, trigger/3, trigger/4]).

-include("cluster_event.hrl").

-record(state, {
		events = gb_trees:empty()
	}).

init(DictName) when is_atom(DictName) ->
	put(DictName, #state{});
init(_) ->
	{error, dictname_is_not_atom}.

add(DictName, Id, {Callback_F, Callback_A}) ->
	do(DictName, {add, Id, #cluster_event_callback{m = undefined, f = Callback_F, a = Callback_A, is_once = false} });
add(DictName, Id, {CallBack_M, Callback_F, Callback_A}) ->
	do(DictName, {add, Id, #cluster_event_callback{m = CallBack_M, f = Callback_F, a = Callback_A, is_once = false} }).
add(DictName, Id, {Callback_F, Callback_A}, IsOnce) ->
	add(DictName, Id, {undefined, Callback_F, Callback_A}, IsOnce);
add(DictName, Id, {CallBack_M, Callback_F, Callback_A}, IsOnce) ->
	do(DictName, {add, Id, #cluster_event_callback{m = CallBack_M, f = Callback_F, a = Callback_A, is_once = IsOnce} });
add(_, _Id, _, _) ->
	{error, id_is_not_atom}.


del(DictName, Id) ->
	do(DictName, {del, Id}).

del(DictName, Id, {Callback_F, Callback_A}) ) ->
	del(DictName, Id, {undefined, Callback_F, Callback_A});
del(DictName, Id, {CallBack_M, Callback_F, Callback_A}) ->
	do(DictName, {del, Id, #cluster_event_callback{m = CallBack_M, f = Callback_F, a = Callback_A} });
del(_DictName, _Id, _) ->
	{error, id_is_not_atom}.

trigger(DictName, Id) ->
	trigger(DictName, Id, []).
trigger(DictName, Id, ExtraParams) ->
	do(DictName, {trigger, Id, ExtraParams}).
trigger(DictName, Id, ExtraParams, ExtState) ->
	do(DictName, {trigger2, Id, ExtraParams, ExtState}).

do(DictName, {add, Id, CallBack}) ->
	case get(DictName) of
		undefined ->
			{error, not_init_dict};
		State = #state{events = Events} ->	
			case gb_trees:lookup(Id, Events) of
				none ->
					CallBackList = [CallBack],
					NewEvents = gb_trees:enter(Id, CallBackList, Events),
					NewState = State#state{events = NewEvents},
					put(DictName, NewState),
					ok;
				{value, CallBackList} ->
					case lists:member(CallBack, CallBackList) of
						true ->
							ok;
						false ->
							NewCallBackList = [CallBack | CallBackList],
							NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
							NewState = State#state{events = NewEvents},
							put(DictName, NewState),
							ok
					end
			end
	end;

do(DictName, {del, Id, CallBack})->
	case get(DictName) of
		undefined ->
			{error, not_init_dict};
		State = #state{events = Events} ->
			case gb_trees:lookup(Id, Events) of
				none ->
					ok;
				{value, CallBackList} ->
					NewCallBackList = lists:delete(CallBack, CallBackList),
					NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
					NewState = State#state{events = NewEvents},
					put(DictName, NewState),
					ok
			end
	end;

do(DictName, {del, Id}) ->
	case get(DictName) of
		undefined ->
			{error, not_init_dict};
		State = #state{events = Events} ->
			case gb_trees:lookup(Id, Events) of
				none ->
					ok;
				{value, _} ->
					NewEvents = gb_trees:delete(Id, Events),
					NewState = State#state{events = NewEvents},
					put(DictName, NewState),
					ok
			end
	end;

do(DictName, {trigger, Id, ExtraParams}) ->
	case get(DictName) of
		undefined ->
			{error, not_init_dict};
		State = #state{events = Events} ->		
			case gb_trees:lookup(Id, Events) of
				none ->
					ok;
				{value, CallBackList} ->
					F = fun(#cluster_event_callback{m = M, f = F, a = A, is_once = IsOnce}) ->
						ApplyResult = case M of
							undefined ->
								catch erlang:apply(F, ExtraParams ++ A);
							_ ->
								catch erlang:apply(M, F, ExtraParams ++ A)
						end,
						case ApplyResult of
							{'EXIT', Reason} -> 
								lager:info("cluster event callback [M]:~p, [F]:~p, [A]:~p fail, reason ~p",[M, F, A, Reason]);
							_ ->
								ignore
						end,
						IsOnce =:= false
					end,
					NewCallBackList = lists:filtermap(F, CallBackList),
					NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
					NewState = State#state{events = NewEvents},
					put(DictName, NewState),
					ok
			end
	end;

%% @doc 保留状态触发。ApplyResult 必须返回新状态。
do(DictName, {trigger2, Id, ExtraParams, ExtState}) ->
	case get(DictName) of
		undefined ->
			{error, not_init_dict};
		State = #state{events = Events} ->		
			case gb_trees:lookup(Id, Events) of
				none ->
					{ok, ExtState};
				{value, CallBackList} ->
					F = fun(CallBack = #cluster_event_callback{m = M, f = F, a = A, is_once = IsOnce}, {CallbackListAcc, ExtStateAcc}) ->
						ApplyResult = case M of
							undefined ->
								catch erlang:apply(F, ExtraParams ++ A ++ ExtStateAcc);
							_ ->
								catch erlang:apply(M, F, ExtraParams ++ A ++ ExtStateAcc)
						end,
						ExtStateAcc2 = case ApplyResult of
							{'EXIT', Reason} -> 
								lager:info("cluster event callback [M]:~p, [F]:~p, [A]:~p fail, reason ~p erlang:get_stacktrace() ~p",[M, F, A, Reason, erlang:get_stacktrace()]),
								ExtStateAcc;
							_ ->
								ApplyResult
						end,
						case IsOnce of
							false ->
								{[CallBack | CallbackListAcc], ExtStateAcc2};
							true ->
								{CallbackListAcc, ExtStateAcc2}
						end
					end,
					{NewCallBackList, ExtState2} = lists:foldl(F, {[], ExtState}, CallBackList),
					NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
					NewState = State#state{events = NewEvents},
					put(DictName, NewState),
					ExtState2
			end
	end.