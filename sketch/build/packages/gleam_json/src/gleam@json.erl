-module(gleam@json).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([decode_bits/2, decode/2, to_string/1, to_string_builder/1, string/1, bool/1, int/1, float/1, null/0, nullable/2, object/1, preprocessed_array/1, array/2]).
-export_type([json/0, decode_error/0]).

-type json() :: any().

-type decode_error() :: unexpected_end_of_input |
    {unexpected_byte, binary(), integer()} |
    {unexpected_sequence, binary(), integer()} |
    {unexpected_format, list(gleam@dynamic:decode_error())}.

-spec decode_bits(
    bitstring(),
    fun((gleam@dynamic:dynamic_()) -> {ok, FON} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, FON} | {error, decode_error()}.
decode_bits(Json, Decoder) ->
    gleam@result:then(
        gleam_json_ffi:decode(Json),
        fun(Dynamic_value) -> _pipe = Decoder(Dynamic_value),
            gleam@result:map_error(
                _pipe,
                fun(Field@0) -> {unexpected_format, Field@0} end
            ) end
    ).

-spec do_decode(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, FOJ} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, FOJ} | {error, decode_error()}.
do_decode(Json, Decoder) ->
    Bits = gleam_stdlib:identity(Json),
    decode_bits(Bits, Decoder).

-spec decode(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, FOF} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, FOF} | {error, decode_error()}.
decode(Json, Decoder) ->
    do_decode(Json, Decoder).

-spec to_string(json()) -> binary().
to_string(Json) ->
    gleam_json_ffi:json_to_string(Json).

-spec to_string_builder(json()) -> gleam@string_builder:string_builder().
to_string_builder(Json) ->
    gleam_json_ffi:json_to_iodata(Json).

-spec string(binary()) -> json().
string(Input) ->
    gleam_json_ffi:string(Input).

-spec bool(boolean()) -> json().
bool(Input) ->
    gleam_json_ffi:bool(Input).

-spec int(integer()) -> json().
int(Input) ->
    gleam_json_ffi:int(Input).

-spec float(float()) -> json().
float(Input) ->
    gleam_json_ffi:float(Input).

-spec null() -> json().
null() ->
    gleam_json_ffi:null().

-spec nullable(gleam@option:option(FOT), fun((FOT) -> json())) -> json().
nullable(Input, Inner_type) ->
    case Input of
        {some, Value} ->
            Inner_type(Value);

        none ->
            null()
    end.

-spec object(list({binary(), json()})) -> json().
object(Entries) ->
    gleam_json_ffi:object(Entries).

-spec preprocessed_array(list(json())) -> json().
preprocessed_array(From) ->
    gleam_json_ffi:array(From).

-spec array(list(FOX), fun((FOX) -> json())) -> json().
array(Entries, Inner_type) ->
    _pipe = Entries,
    _pipe@1 = gleam@list:map(_pipe, Inner_type),
    preprocessed_array(_pipe@1).
