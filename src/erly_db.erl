-module(erly_db).

-export([create_schema/0, create_tables/0, ensure_loaded/0]).
-export([create_url/1, lookup_url/1]).

-record(url, {id, short_url, long_url}).

%% ------------------------------------------------------------------
%% Mnesia Setup Function Definitions
%% ------------------------------------------------------------------

create_schema() ->
  mnesia:create_schema([node()|nodes()]).

create_tables() ->
  mnesia:create_table(url, [{disc_copies, [node()]},
                            {ram_copies, nodes()},
                            {attributes, record_info(fields, url)}]).

ensure_loaded() ->
  ok = mnesia:wait_for_tables([url], 60000).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

create_url(LongUrl) ->
  {Id, ShortUrl} = create_short_url(),
  Rec = #url{id=Id,
             short_url=ShortUrl,
             long_url=LongUrl},
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

create_short_url() ->
  Id = guid(),
  ShortUrl = "er.ly/" ++ Id,
  {Id, ShortUrl}.

guid() ->
  B = crypto:rand_bytes(16),
  lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(B)]).
