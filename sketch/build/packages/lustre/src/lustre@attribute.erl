-module(lustre@attribute).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([attribute/2, property/2, on/2, map/2, style/1, class/1, none/0, classes/1, id/1, role/1, type_/1, value/1, checked/1, placeholder/1, selected/1, accept/1, accept_charset/1, msg/1, autocomplete/1, autofocus/1, disabled/1, name/1, pattern/1, readonly/1, required/1, for/1, max/1, min/1, step/1, cols/1, rows/1, wrap/1, href/1, target/1, download/1, rel/1, src/1, height/1, width/1, alt/1, autoplay/1, controls/1, loop/1, action/1, enctype/1, method/1, novalidate/1, form_action/1, form_enctype/1, form_method/1, form_novalidate/1, form_target/1]).

-spec attribute(binary(), binary()) -> lustre@internals@vdom:attribute(any()).
attribute(Name, Value) ->
    {attribute, Name, gleam@dynamic:from(Value), false}.

-spec property(binary(), any()) -> lustre@internals@vdom:attribute(any()).
property(Name, Value) ->
    {attribute, Name, gleam@dynamic:from(Value), true}.

-spec on(
    binary(),
    fun((gleam@dynamic:dynamic_()) -> {ok, OIY} |
        {error, list(gleam@dynamic:decode_error())})
) -> lustre@internals@vdom:attribute(OIY).
on(Name, Handler) ->
    {event, <<"on"/utf8, Name/binary>>, Handler}.

-spec map(lustre@internals@vdom:attribute(OJD), fun((OJD) -> OJF)) -> lustre@internals@vdom:attribute(OJF).
map(Attr, F) ->
    case Attr of
        {attribute, Name, Value, As_property} ->
            {attribute, Name, Value, As_property};

        {event, On, Handler} ->
            {event, On, fun(E) -> gleam@result:map(Handler(E), F) end}
    end.

-spec style(list({binary(), binary()})) -> lustre@internals@vdom:attribute(any()).
style(Properties) ->
    attribute(
        <<"style"/utf8>>,
        (gleam@list:fold(
            Properties,
            <<""/utf8>>,
            fun(Styles, _use1) ->
                {Name, Value} = _use1,
                <<<<<<<<Styles/binary, Name/binary>>/binary, ":"/utf8>>/binary,
                        Value/binary>>/binary,
                    ";"/utf8>>
            end
        ))
    ).

-spec class(binary()) -> lustre@internals@vdom:attribute(any()).
class(Name) ->
    attribute(<<"class"/utf8>>, Name).

-spec none() -> lustre@internals@vdom:attribute(any()).
none() ->
    class(<<""/utf8>>).

-spec classes(list({binary(), boolean()})) -> lustre@internals@vdom:attribute(any()).
classes(Names) ->
    attribute(
        <<"class"/utf8>>,
        begin
            _pipe = Names,
            _pipe@1 = gleam@list:filter_map(
                _pipe,
                fun(Class) -> case erlang:element(2, Class) of
                        true ->
                            {ok, erlang:element(1, Class)};

                        false ->
                            {error, nil}
                    end end
            ),
            gleam@string:join(_pipe@1, <<" "/utf8>>)
        end
    ).

-spec id(binary()) -> lustre@internals@vdom:attribute(any()).
id(Name) ->
    attribute(<<"id"/utf8>>, Name).

-spec role(binary()) -> lustre@internals@vdom:attribute(any()).
role(Name) ->
    attribute(<<"role"/utf8>>, Name).

-spec type_(binary()) -> lustre@internals@vdom:attribute(any()).
type_(Name) ->
    attribute(<<"type"/utf8>>, Name).

-spec value(binary()) -> lustre@internals@vdom:attribute(any()).
value(Val) ->
    property(<<"value"/utf8>>, gleam@dynamic:from(Val)).

-spec checked(boolean()) -> lustre@internals@vdom:attribute(any()).
checked(Is_checked) ->
    property(<<"checked"/utf8>>, Is_checked).

-spec placeholder(binary()) -> lustre@internals@vdom:attribute(any()).
placeholder(Text) ->
    attribute(<<"placeholder"/utf8>>, Text).

-spec selected(boolean()) -> lustre@internals@vdom:attribute(any()).
selected(Is_selected) ->
    property(<<"selected"/utf8>>, Is_selected).

-spec accept(list(binary())) -> lustre@internals@vdom:attribute(any()).
accept(Types) ->
    attribute(<<"accept"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-spec accept_charset(list(binary())) -> lustre@internals@vdom:attribute(any()).
accept_charset(Types) ->
    attribute(<<"acceptCharset"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-spec msg(binary()) -> lustre@internals@vdom:attribute(any()).
msg(Uri) ->
    attribute(<<"msg"/utf8>>, Uri).

-spec autocomplete(binary()) -> lustre@internals@vdom:attribute(any()).
autocomplete(Name) ->
    attribute(<<"autocomplete"/utf8>>, Name).

-spec autofocus(boolean()) -> lustre@internals@vdom:attribute(any()).
autofocus(Should_autofocus) ->
    property(<<"autofocus"/utf8>>, Should_autofocus).

-spec disabled(boolean()) -> lustre@internals@vdom:attribute(any()).
disabled(Is_disabled) ->
    property(<<"disabled"/utf8>>, Is_disabled).

-spec name(binary()) -> lustre@internals@vdom:attribute(any()).
name(Name) ->
    attribute(<<"name"/utf8>>, Name).

-spec pattern(binary()) -> lustre@internals@vdom:attribute(any()).
pattern(Regex) ->
    attribute(<<"pattern"/utf8>>, Regex).

-spec readonly(boolean()) -> lustre@internals@vdom:attribute(any()).
readonly(Is_readonly) ->
    property(<<"readonly"/utf8>>, Is_readonly).

-spec required(boolean()) -> lustre@internals@vdom:attribute(any()).
required(Is_required) ->
    property(<<"required"/utf8>>, Is_required).

-spec for(binary()) -> lustre@internals@vdom:attribute(any()).
for(Id) ->
    attribute(<<"for"/utf8>>, Id).

-spec max(binary()) -> lustre@internals@vdom:attribute(any()).
max(Val) ->
    attribute(<<"max"/utf8>>, Val).

-spec min(binary()) -> lustre@internals@vdom:attribute(any()).
min(Val) ->
    attribute(<<"min"/utf8>>, Val).

-spec step(binary()) -> lustre@internals@vdom:attribute(any()).
step(Val) ->
    attribute(<<"step"/utf8>>, Val).

-spec cols(integer()) -> lustre@internals@vdom:attribute(any()).
cols(Val) ->
    attribute(<<"cols"/utf8>>, gleam@int:to_string(Val)).

-spec rows(integer()) -> lustre@internals@vdom:attribute(any()).
rows(Val) ->
    attribute(<<"rows"/utf8>>, gleam@int:to_string(Val)).

-spec wrap(binary()) -> lustre@internals@vdom:attribute(any()).
wrap(Mode) ->
    attribute(<<"wrap"/utf8>>, Mode).

-spec href(binary()) -> lustre@internals@vdom:attribute(any()).
href(Uri) ->
    attribute(<<"href"/utf8>>, Uri).

-spec target(binary()) -> lustre@internals@vdom:attribute(any()).
target(Target) ->
    attribute(<<"target"/utf8>>, Target).

-spec download(binary()) -> lustre@internals@vdom:attribute(any()).
download(Filename) ->
    attribute(<<"download"/utf8>>, Filename).

-spec rel(binary()) -> lustre@internals@vdom:attribute(any()).
rel(Relationship) ->
    attribute(<<"rel"/utf8>>, Relationship).

-spec src(binary()) -> lustre@internals@vdom:attribute(any()).
src(Uri) ->
    attribute(<<"src"/utf8>>, Uri).

-spec height(integer()) -> lustre@internals@vdom:attribute(any()).
height(Val) ->
    property(<<"height"/utf8>>, gleam@int:to_string(Val)).

-spec width(integer()) -> lustre@internals@vdom:attribute(any()).
width(Val) ->
    property(<<"width"/utf8>>, gleam@int:to_string(Val)).

-spec alt(binary()) -> lustre@internals@vdom:attribute(any()).
alt(Text) ->
    attribute(<<"alt"/utf8>>, Text).

-spec autoplay(boolean()) -> lustre@internals@vdom:attribute(any()).
autoplay(Should_autoplay) ->
    property(<<"autoplay"/utf8>>, Should_autoplay).

-spec controls(boolean()) -> lustre@internals@vdom:attribute(any()).
controls(Visible) ->
    property(<<"controls"/utf8>>, Visible).

-spec loop(boolean()) -> lustre@internals@vdom:attribute(any()).
loop(Should_loop) ->
    property(<<"loop"/utf8>>, Should_loop).

-spec action(binary()) -> lustre@internals@vdom:attribute(any()).
action(Url) ->
    attribute(<<"action"/utf8>>, Url).

-spec enctype(binary()) -> lustre@internals@vdom:attribute(any()).
enctype(Value) ->
    attribute(<<"enctype"/utf8>>, Value).

-spec method(binary()) -> lustre@internals@vdom:attribute(any()).
method(Method) ->
    attribute(<<"method"/utf8>>, Method).

-spec novalidate(boolean()) -> lustre@internals@vdom:attribute(any()).
novalidate(Value) ->
    property(<<"novalidate"/utf8>>, Value).

-spec form_action(binary()) -> lustre@internals@vdom:attribute(any()).
form_action(Action) ->
    attribute(<<"formaction"/utf8>>, Action).

-spec form_enctype(binary()) -> lustre@internals@vdom:attribute(any()).
form_enctype(Value) ->
    attribute(<<"formenctype"/utf8>>, Value).

-spec form_method(binary()) -> lustre@internals@vdom:attribute(any()).
form_method(Method) ->
    attribute(<<"formmethod"/utf8>>, Method).

-spec form_novalidate(boolean()) -> lustre@internals@vdom:attribute(any()).
form_novalidate(Value) ->
    property(<<"formnovalidate"/utf8>>, Value).

-spec form_target(binary()) -> lustre@internals@vdom:attribute(any()).
form_target(Target) ->
    attribute(<<"formtarget"/utf8>>, Target).
