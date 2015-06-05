-module(erly_db).

-export([create_schema/0, create_tables/0, ensure_loaded/0]).
-export([create_url/1, lookup_url/1]).

-include("url.hrl").

%% ------------------------------------------------------------------
%% Mnesia Setup Function Definitions
%% ------------------------------------------------------------------

create_schema() ->
  mnesia:create_schema([node()|nodes()]).

create_tables() ->
  mnesia:create_table(url, [{disc_copies, [node()]},
                            {ram_copies, nodes()},
                            {attributes, record_info(fields, url)},
                            {index, [hash]}]),
  mnesia:create_table(counter, [{disc_copies, [node()]},
                                {ram_copies, nodes()},
                                {attributes, record_info(fields, counter)}]).

ensure_loaded() ->
  ok = mnesia:wait_for_tables([url, counter], 60000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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
