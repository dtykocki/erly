-module(erly_db).

-export([create_schema/0, create_tables/0, ensure_loaded/0]).
-export([create_url/1, lookup_url/1]).

-record(url, {id, url}).
-record(counter, {id, type}).

%% ------------------------------------------------------------------
%% Mnesia Setup Function Definitions
%% ------------------------------------------------------------------

create_schema() ->
  mnesia:create_schema([node()|nodes()]).

create_tables() ->
  mnesia:create_table(url, [{disc_copies, [node()]},
                            {ram_copies, nodes()},
                            {attributes, record_info(fields, url)}]),
  mnesia:create_table(counter, [{disc_copies, [node()]},
                                {ram_copies, nodes()},
                                {attributes, record_info(fields, counter)}]).

ensure_loaded() ->
  ok = mnesia:wait_for_tables([url], 60000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

create_url(LongUrl) ->
  Rec = #url{id=next_id(), url=LongUrl},
  Fun = fun() -> mnesia:write(Rec) end,
  {atomic, Res} = mnesia:transaction(Fun),
  {Res, Rec}.

lookup_url(Id) ->
  Fun = fun() ->
      case mnesia:read(url, Id) of
        [Address] -> {ok, Address};
        []        -> {error, not_found}
      end
  end,
  {atomic, Res} = mnesia:transaction(Fun),
  Res.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

next_id() ->
  mnesia:dirty_update_counter(counter, id, 1).
