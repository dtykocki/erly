-module(erly).
-behaviour(gen_server).

-export([start_link/0, stop/0, create_url/1, lookup_url/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("url.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).

create_url(Url) ->
  gen_server:call(?SERVER, {create_url, Url}).

lookup_url(Id) ->
  gen_server:call(?SERVER, {lookup_url, Id}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  error_logger:info_report("initializing erly"),
  erly_db:start(),
  error_logger:info_report("erl_db started"),
  {ok, Args}.

handle_call({create_url, Url}, _From, State) ->
  Reply = erly_db:create_url(Url),
  {reply, Reply, State};
handle_call({lookup_url, Id}, _From, State) ->
  Reply = erly_db:lookup_url(Id),
  {reply, Reply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

