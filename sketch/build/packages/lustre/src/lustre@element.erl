-module(lustre@element).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([element/3, keyed/2, namespaced/4, advanced/6, text/1, none/0, map/2, to_string/1, to_document_string/1, to_string_builder/1, to_document_string_builder/1]).

-spec element(
    binary(),
    list(lustre@internals@vdom:attribute(OQQ)),
    list(lustre@internals@vdom:element(OQQ))
) -> lustre@internals@vdom:element(OQQ).
element(Tag, Attrs, Children) ->
    case Tag of
        <<"area"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"base"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"br"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"col"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"embed"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"hr"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"img"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"input"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"link"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"meta"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"param"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"source"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"track"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        <<"wbr"/utf8>> ->
            {element, <<""/utf8>>, <<""/utf8>>, Tag, Attrs, [], false, true};

        _ ->
            {element,
                <<""/utf8>>,
                <<""/utf8>>,
                Tag,
                Attrs,
                Children,
                false,
                false}
    end.

-spec do_keyed(lustre@internals@vdom:element(ORD), binary()) -> lustre@internals@vdom:element(ORD).
do_keyed(El, Key) ->
    case El of
        {element, _, Namespace, Tag, Attrs, Children, Self_closing, Void} ->
            {element, Key, Namespace, Tag, Attrs, Children, Self_closing, Void};

        {map, Subtree} ->
            {map, fun() -> do_keyed(Subtree(), Key) end};

        _ ->
            El
    end.

-spec keyed(
    fun((list(lustre@internals@vdom:element(OQW))) -> lustre@internals@vdom:element(OQW)),
    list({binary(), lustre@internals@vdom:element(OQW)})
) -> lustre@internals@vdom:element(OQW).
keyed(El, Children) ->
    El(
        (gleam@list:map(
            Children,
            fun(_use0) ->
                {Key, Child} = _use0,
                do_keyed(Child, Key)
            end
        ))
    ).

-spec namespaced(
    binary(),
    binary(),
    list(lustre@internals@vdom:attribute(ORG)),
    list(lustre@internals@vdom:element(ORG))
) -> lustre@internals@vdom:element(ORG).
namespaced(Namespace, Tag, Attrs, Children) ->
    {element, <<""/utf8>>, Namespace, Tag, Attrs, Children, false, false}.

-spec advanced(
    binary(),
    binary(),
    list(lustre@internals@vdom:attribute(ORM)),
    list(lustre@internals@vdom:element(ORM)),
    boolean(),
    boolean()
) -> lustre@internals@vdom:element(ORM).
advanced(Namespace, Tag, Attrs, Children, Self_closing, Void) ->
    {element, <<""/utf8>>, Namespace, Tag, Attrs, Children, Self_closing, Void}.

-spec text(binary()) -> lustre@internals@vdom:element(any()).
text(Content) ->
    {text, Content}.

-spec none() -> lustre@internals@vdom:element(any()).
none() ->
    {text, <<""/utf8>>}.

-spec escape(binary(), binary()) -> binary().
escape(Escaped, Content) ->
    case Content of
        <<"<"/utf8, Rest/binary>> ->
            escape(<<Escaped/binary, "&lt;"/utf8>>, Rest);

        <<">"/utf8, Rest@1/binary>> ->
            escape(<<Escaped/binary, "&gt;"/utf8>>, Rest@1);

        <<"&"/utf8, Rest@2/binary>> ->
            escape(<<Escaped/binary, "&amp;"/utf8>>, Rest@2);

        <<"\""/utf8, Rest@3/binary>> ->
            escape(<<Escaped/binary, "&quot;"/utf8>>, Rest@3);

        <<"'"/utf8, Rest@4/binary>> ->
            escape(<<Escaped/binary, "&#39;"/utf8>>, Rest@4);

        _ ->
            case gleam@string:pop_grapheme(Content) of
                {ok, {X, Xs}} ->
                    escape(<<Escaped/binary, X/binary>>, Xs);

                {error, _} ->
                    Escaped
            end
    end.

-spec map(lustre@internals@vdom:element(ORW), fun((ORW) -> ORY)) -> lustre@internals@vdom:element(ORY).
map(Element, F) ->
    case Element of
        {text, Content} ->
            {text, Content};

        {map, Subtree} ->
            {map, fun() -> map(Subtree(), F) end};

        {element, Key, Namespace, Tag, Attrs, Children, Self_closing, Void} ->
            {map,
                fun() ->
                    {element,
                        Key,
                        Namespace,
                        Tag,
                        gleam@list:map(
                            Attrs,
                            fun(_capture) ->
                                lustre@attribute:map(_capture, F)
                            end
                        ),
                        gleam@list:map(
                            Children,
                            fun(_capture@1) -> map(_capture@1, F) end
                        ),
                        Self_closing,
                        Void}
                end}
    end.

-spec to_string(lustre@internals@vdom:element(any())) -> binary().
to_string(Element) ->
    lustre@internals@vdom:element_to_string(Element).

-spec to_document_string(lustre@internals@vdom:element(any())) -> binary().
to_document_string(El) ->
    _pipe = lustre@internals@vdom:element_to_string(case El of
            {element, _, _, <<"html"/utf8>>, _, _, _, _} ->
                El;

            {element, _, _, <<"head"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {element, _, _, <<"body"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {map, Subtree} ->
                Subtree();

            _ ->
                element(
                    <<"html"/utf8>>,
                    [],
                    [element(<<"body"/utf8>>, [], [El])]
                )
        end),
    gleam@string:append(<<"<!doctype html>\n"/utf8>>, _pipe).

-spec to_string_builder(lustre@internals@vdom:element(any())) -> gleam@string_builder:string_builder().
to_string_builder(Element) ->
    lustre@internals@vdom:element_to_string_builder(Element).

-spec to_document_string_builder(lustre@internals@vdom:element(any())) -> gleam@string_builder:string_builder().
to_document_string_builder(El) ->
    _pipe = lustre@internals@vdom:element_to_string_builder(case El of
            {element, _, _, <<"html"/utf8>>, _, _, _, _} ->
                El;

            {element, _, _, <<"head"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {element, _, _, <<"body"/utf8>>, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {map, Subtree} ->
                Subtree();

            _ ->
                element(
                    <<"html"/utf8>>,
                    [],
                    [element(<<"body"/utf8>>, [], [El])]
                )
        end),
    gleam@string_builder:prepend(_pipe, <<"<!doctype html>\n"/utf8>>).
