%%% @author Pierre Matri  <pierre@matri.me>
%%%
%%% @copyright 2013 Pierre Matri, All rights reserved. Open source, MIT License.
%%% @version 0.1.0

-module(xxhash32_ffi).
-version(0.1).

-define(DEFAULT_SEED, 0).

-export([hash32/1, hash32/2,
         hash32_init/0, hash32_init/1,
         hash32_update/2, hash32_digest/1,
         hash64/1, hash64/2,
         hash64_init/0, hash64_init/1,
         hash64_update/2, hash64_digest/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).

-type hash_input() :: binary() | atom() | number() | list().
-type hash_handle() :: binary().
-type hash_digest() :: non_neg_integer().
-type hash_seed() :: non_neg_integer().


nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded, module, ?MODULE, line, Line}).


%%%=============================================================================
%%% Initializer
%%%=============================================================================


-spec init() -> ok | {error, any()}.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,

    erlang:load_nif(filename:join(PrivDir, xxhash), 0).


%%%=============================================================================
%%% Nif functions
%%%=============================================================================

hash32_impl(_Data, _Seed) ->
  ?nif_stub.

hash32_init_impl(_Seed) ->
  ?nif_stub.

hash32_update_impl(_Handle, _Data) ->
  ?nif_stub.

hash32_digest_impl(_Handle) ->
  ?nif_stub.

hash64_impl(_Data, _Seed) ->
  ?nif_stub.

hash64_init_impl(_Seed) ->
  ?nif_stub.

hash64_update_impl(_Handle, _Data) ->
  ?nif_stub.

hash64_digest_impl(_Handle) ->
  ?nif_stub.


%%%=============================================================================
%%% Exports
%%%=============================================================================

%% @doc Hash `Data' with a default seed value, and return the digest.
%% @see hash32/2

-spec hash32(Data::hash_input()) -> hash_digest().

hash32(Data) ->
  hash32(Data, ?DEFAULT_SEED).


%% @doc Hash `Data' with `Seed' value, and return the digest.
%% @see hash32/1

-spec hash32(Data::hash_input(), Seed::hash_seed()) -> hash_digest().

hash32(Data, Seed) when is_integer(Seed) ->
  hash32_impl(supported_to_binary(Data), Seed).


%% @doc Initialize a hasher with a default seed value, and return an handle.
%% @see hash32_init/1

-spec hash32_init() -> hash_handle().

hash32_init() ->
  hash32_init(?DEFAULT_SEED).


%% @doc Initialize a hasher with `Seed' value, and return an handle.
%% @see hash32_init/0

-spec hash32_init(Seed::hash_seed()) -> hash_handle().

hash32_init(Seed) when is_integer(Seed) ->
  hash32_init_impl(Seed).


%% @doc Update the `Handle' hasher content with the given `Data'. This can be
%%      called many times with new data as it is streamed.

-spec hash32_update(Handle::hash_handle(), Data::hash_input()) -> ok.

hash32_update(Handle, Data) when is_binary(Handle) ->
  hash32_update_impl(Handle, supported_to_binary(Data)).


%% @doc Calculates a digest of all the passed data to the `Handle' hasher.

-spec hash32_digest(Handle::hash_handle()) -> hash_digest().

hash32_digest(Handle) when is_binary(Handle) ->
  hash32_digest_impl(Handle).


%% @doc Hash `Data' with a default seed value, and return the digest.
%% @see hash64/2

-spec hash64(Data::hash_input()) -> hash_digest().

hash64(Data) ->
  hash64(Data, ?DEFAULT_SEED).


%% @doc Hash `Data' with `Seed' value, and return the digest.
%% @see hash64/1

-spec hash64(Data::hash_input(), Seed::hash_seed()) -> hash_digest().

hash64(Data, Seed) when is_integer(Seed) ->
  hash64_impl(supported_to_binary(Data), Seed).


%% @doc Initialize a hasher with a default seed value, and return an handle.
%% @see hash64_init/1

-spec hash64_init() -> hash_handle().

hash64_init() ->
  hash64_init(?DEFAULT_SEED).


%% @doc Initialize a hasher with `Seed' value, and return an handle.
%% @see hash64_init/0

-spec hash64_init(Seed::hash_seed()) -> hash_handle().

hash64_init(Seed) when is_integer(Seed) ->
  hash64_init_impl(Seed).


%% @doc Update the `Handle' hasher content with the given `Data'. This can be
%%      called many times with new data as it is streamed.

-spec hash64_update(Handle::hash_handle(), Data::hash_input()) -> ok.

hash64_update(Handle, Data) when is_binary(Handle) ->
  hash64_update_impl(Handle, supported_to_binary(Data)).


%% @doc Calculates a digest of all the passed data to the `Handle' hasher.

-spec hash64_digest(Handle::hash_handle()) -> hash_digest().

hash64_digest(Handle) when is_binary(Handle) ->
  hash64_digest_impl(Handle).


%%%=============================================================================
%%% Helpers
%%%=============================================================================

supported_to_binary(Data) when is_binary(Data) ->
  Data;
supported_to_binary(Data) when is_list(Data) ->
  list_to_binary(Data);
supported_to_binary(Data) when is_atom(Data) ->
  term_to_binary(Data);
supported_to_binary(Data) when is_integer(Data) ->
  list_to_binary(integer_to_list(Data));
supported_to_binary(Data) when is_float(Data) ->
  <<Data/float>>.
