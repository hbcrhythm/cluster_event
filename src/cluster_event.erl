%%%-------------------------------------------------------------------
%% @doc cluster event system, use process to implement
%% @author labihbc@gamil.com
%% @end
%%%-------------------------------------------------------------------
-module(cluster_event).

-behaviour(gen_server).

-export([start_link/0, start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add/3, add/4, del/2, del/3, trigger/2, trigger/3]).

-include("cluster_event.hrl").

-record(state, {
		events = gb_trees:empty()
	}).

add(Name, Id, {Callback_F, Callback_A}) when is_atom(Id) ->
	add(Name, Id, {Callback_F, Callback_A}, false);
add(Name, Id, {CallBack_M, Callback_F, Callback_A}) when is_atom(Id) ->
	add(Name, Id, {CallBack_M, Callback_F, Callback_A}, false).
add(Name, Id, {Callback_F, Callback_A}, IsOnce) when is_atom(Id) ->
	add(Name, Id, {undefined, Callback_F, Callback_A}, IsOnce);
add(Name, Id, {CallBack_M, Callback_F, Callback_A}, IsOnce) when is_atom(Id) ->
	Name ! {add, Id, #cluster_event_callback{m = CallBack_M, f = Callback_F, a = Callback_A, is_once = IsOnce} };
add(_, _Id, _, _) ->
	{error, id_is_not_atom}.


del(Name, Id) when is_atom(Id) ->
	Name ! {del, Id}.

del(Name, Id, {Callback_F, Callback_A}) when is_atom(Id) ->
	del(Name, Id, {undefined, Callback_F, Callback_A});
del(Name, Id, {CallBack_M, Callback_F, Callback_A}) when is_atom(Id) ->
	Name ! {del, Id, #cluster_event_callback{m = CallBack_M, f = Callback_F, a = Callback_A} };
del(_, _Id, _) ->
	{error, id_is_not_atom}.

trigger(Name, Id) ->
	trigger(Name, Id, []).
trigger(Name, Id, ExtraParams) ->
	Name ! {trigger, Id, ExtraParams}.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add, Id, CallBack}, State = #state{events = Events}) ->
	case gb_trees:lookup(Id, Events) of
		none ->
			CallBackList = [CallBack],
			NewEvents = gb_trees:enter(Id, CallBackList, Events),
			{noreply, State#state{events = NewEvents}};
		{value, CallBackList} ->
			case lists:member(CallBack, CallBackList) of
				true ->
					{noreply, State};
				false ->
					NewCallBackList = [CallBack | CallBackList],
					NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
					{noreply, State#state{events = NewEvents}}
			end
	end;

handle_info({del, Id, CallBack}, State = #state{events = Events}) ->
	case gb_trees:lookup(Id, Events) of
		none ->
			{noreply, State};
		{value, CallBackList} ->
			NewCallBackList = lists:delete(CallBack, CallBackList),
			NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
			{noreply, State#state{events = NewEvents}}
	end;

handle_info({del, Id}, State = #state{events = Events}) ->
	case gb_trees:lookup(Id, Events) of
		none ->
			{noreply, State};
		{value, _} ->
			NewEvents = gb_trees:delete(Id, Events),
			{noreply, State#state{events = NewEvents}}
	end;

handle_info({trigger, Id, ExtraParams}, State = #state{events = Events}) ->
	case gb_trees:lookup(Id, Events) of
		none ->
			{noreply, State};
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
						lager:info("cluster event callback [M]:~w, [F]:~w, [A]:~w fail, reason ~w",[M, F, A, Reason]);
					_ ->
						ignore
				end,
				IsOnce =:= false
			end,
			NewCallBackList = lists:filtermap(F, CallBackList),
			NewEvents = gb_trees:enter(Id, NewCallBackList, Events),
			{noreply, State#state{events = NewEvents}}
	end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

