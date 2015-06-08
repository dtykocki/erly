-module(erly_db).

-export([start/0, start/1, create_schema/1, create_tables/1, ensure_loaded/0]).
-export([create_url/1, lookup_url/1]).

-include("url.hrl").

%% ------------------------------------------------------------------
%% Mnesia Setup Function Definitions
%% ------------------------------------------------------------------

create_schema(Nodes) ->
  mnesia:create_schema(Nodes).

create_tables([Node|Nodes]) ->
  mnesia:create_table(url, [{disc_copies, [Node]},
                            {ram_copies, Nodes},
                            {attributes, record_info(fields, url)},
                            {index, [hash]}]),
  mnesia:create_table(counter, [{disc_copies, [Node]},
                                {ram_copies, Nodes},
                                {attributes, record_info(fields, counter)}]).

ensure_loaded() ->
  ok = mnesia:wait_for_tables([url, counter], 60000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------


start() ->
  start([node()|nodes()]).

start(Nodes) ->
  case is_fresh_startup() of
    true ->
      case mnesia:system_info(is_running) of
        yes ->
          error_logger:info_report("stopping mnesia"),
          mnesia:stop();
        _   -> pass
      end,
      create_schema(Nodes),
      error_logger:info_report("mnesia schema created"),
      error_logger:info_report("starting mnesia"),
      mnesia:start(),
      create_tables(Nodes),
      error_logger:info_report("mnesia tables created");
    {exists, _} ->
      ok = ensure_loaded()
  end.

create_url(LongUrl) ->
  Ctx = hashids:new([{salt, binary_to_list(crypto:rand_bytes(16))},
                     {min_hash_length, 8}]),
  Id = next_id(),
  Encoded = hashids:encode(Ctx, Id),
  Rec = #url{id=Id, url=LongUrl, hash=Encoded},
  Fun = fun() -> mnesia:write(Rec) end,
  {atomic, Res} = mnesia:transaction(Fun),
  {Res, Rec}.

lookup_url(Hash) ->
  Fun = fun() ->
      case mnesia:index_read(url, Hash, hash) of
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

is_fresh_startup() ->
  Node = node(),
  case mnesia:system_info(tables) of
    [schema]  -> true;
    Tables    ->
      case mnesia:table_info(schema, cookie) of
        {_, Node} -> {exists, Tables};
        _         -> true
      end
  end.
