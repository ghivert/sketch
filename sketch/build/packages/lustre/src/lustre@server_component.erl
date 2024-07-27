-module(lustre@server_component).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([component/1, script/0, route/1, data/1, include/1, subscribe/2, unsubscribe/1, emit/2, set_selector/1, decode_action/1, encode_patch/1]).

-spec component(list(lustre@internals@vdom:attribute(SKF))) -> lustre@internals@vdom:element(SKF).
component(Attrs) ->
    lustre@element:element(<<"lustre-server-component"/utf8>>, Attrs, []).

-spec script() -> lustre@internals@vdom:element(any()).
script() ->
    lustre@element:element(
        <<"script"/utf8>>,
        [lustre@attribute:attribute(<<"type"/utf8>>, <<"module"/utf8>>)],
        [lustre@element:text(
                <<"function k(l,n,i,s=!1){let t,o=[{prev:l,next:n,parent:l.parentNode}];for(;o.length;){let{prev:e,next:r,parent:u}=o.pop();if(r.subtree!==void 0&&(r=r.subtree()),r.content!==void 0)if(e)if(e.nodeType===Node.TEXT_NODE)e.textContent!==r.content&&(e.textContent=r.content),t??=e;else{let a=document.createTextNode(r.content);u.replaceChild(a,e),t??=a}else{let a=document.createTextNode(r.content);u.appendChild(a),t??=a}else if(r.tag!==void 0){let a=$({prev:e,next:r,dispatch:i,stack:o,isComponent:s});e?e!==a&&u.replaceChild(a,e):u.appendChild(a),t??=a}}return t}function L(l,n,i){let s=l.parentNode;for(let t of n[0]){let o=t[0].split(\"-\"),e=t[1],r=N(s,o),u;if(r!==null&&r!==s)u=k(r,e,i);else{let a=N(s,o.slice(0,-1)),f=document.createTextNode(\"\");a.appendChild(f),u=k(f,e,i)}o===\"0\"&&(l=u)}for(let t of n[1]){let o=t[0].split(\"-\");N(s,o).remove()}for(let t of n[2]){let o=t[0].split(\"-\"),e=t[1],r=N(s,o),u=v.get(r);for(let a of e[0]){let f=a[0],m=a[1];if(f.startsWith(\"data-lustre-on-\")){let b=f.slice(15),d=i(J);u.has(b)||el.addEventListener(b,y),u.set(b,d),el.setAttribute(f,m)}else r.setAttribute(f,m),r[f]=m}for(let a of e[1])if(a[0].startsWith(\"data-lustre-on-\")){let f=a[0].slice(15);r.removeEventListener(f,y),u.delete(f)}else r.removeAttribute(a[0])}return l}function $({prev:l,next:n,dispatch:i,stack:s}){let t=n.namespace||\"http://www.w3.org/1999/xhtml\",o=l&&l.nodeType===Node.ELEMENT_NODE&&l.localName===n.tag&&l.namespaceURI===(n.namespace||\"http://www.w3.org/1999/xhtml\"),e=o?l:t?document.createElementNS(t,n.tag):document.createElement(n.tag),r;if(v.has(e))r=v.get(e);else{let c=new Map;v.set(e,c),r=c}let u=o?new Set(r.keys()):null,a=o?new Set(Array.from(l.attributes,c=>c.name)):null,f=null,m=null,b=null;for(let c of n.attrs){let h=c[0],p=c[1];if(c[2])e[h]=p;else if(h.startsWith(\"on\")){let g=h.slice(2),A=i(p);r.has(g)||e.addEventListener(g,y),r.set(g,A),o&&u.delete(g)}else if(h.startsWith(\"data-lustre-on-\")){let g=h.slice(15),A=i(J);r.has(g)||e.addEventListener(g,y),r.set(g,A),e.setAttribute(h,p)}else h===\"class\"?f=f===null?p:f+\" \"+p:h===\"style\"?m=m===null?p:m+p:h===\"dangerous-unescaped-html\"?b=p:(e.setAttribute(h,p),h===\"value\"&&(e[h]=p),o&&a.delete(h))}if(f!==null&&(e.setAttribute(\"class\",f),o&&a.delete(\"class\")),m!==null&&(e.setAttribute(\"style\",m),o&&a.delete(\"style\")),o){for(let c of a)e.removeAttribute(c);for(let c of u)e.removeEventListener(c,y)}if(n.key!==void 0&&n.key!==\"\")e.setAttribute(\"data-lustre-key\",n.key);else if(b!==null)return e.innerHTML=b,e;let d=e.firstChild,C=null,w=null,O=null,E=n.children[Symbol.iterator]().next().value;E!==void 0&&E.key!==void 0&&E.key!==\"\"&&(C=new Set,w=T(l),O=T(n));for(let c of n.children)if(c.key!==void 0&&C!==null){for(;d&&!O.has(d.getAttribute(\"data-lustre-key\"));){let p=d.nextSibling;e.removeChild(d),d=p}if(w.size===0){s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;continue}if(C.has(c.key)){console.warn(`Duplicate key found in Lustre vnode: ${c.key}`),s.unshift({prev:null,next:c,parent:e});continue}C.add(c.key);let h=w.get(c.key);if(!h&&!d){s.unshift({prev:null,next:c,parent:e});continue}if(!h&&d!==null){let p=document.createTextNode(\"\");e.insertBefore(p,d),s.unshift({prev:p,next:c,parent:e});continue}if(!h||h===d){s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;continue}e.insertBefore(h,d),s.unshift({prev:h,next:c,parent:e})}else s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;for(;d;){let c=d.nextSibling;e.removeChild(d),d=c}return e}var v=new WeakMap;function y(l){let n=l.currentTarget;if(!v.has(n)){n.removeEventListener(l.type,y);return}let i=v.get(n);if(!i.has(l.type)){n.removeEventListener(l.type,y);return}i.get(l.type)(l)}function J(l){let n=l.target,i=n.getAttribute(`data-lustre-on-${l.type}`),s=JSON.parse(n.getAttribute(\"data-lustre-data\")||\"{}\"),t=JSON.parse(n.getAttribute(\"data-lustre-include\")||\"[]\");switch(l.type){case\"input\":case\"change\":t.push(\"target.value\");break}return{tag:i,data:t.reduce((o,e)=>{let r=e.split(\".\");for(let u=0,a=o,f=l;u<r.length;u++)u===r.length-1?a[r[u]]=f[r[u]]:(a[r[u]]??={},f=f[r[u]],a=a[r[u]]);return o},{data:s})}}function T(l){let n=new Map;if(l)for(let i of l.children){let s=i.key||i?.getAttribute(\"data-lustre-key\");s&&n.set(s,i)}return n}function N(l,n){let i,s,t=l;for(;[i,...s]=n,i!==void 0;)t=t.childNodes.item(i),n=s;return t}var S=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#n=null;#t=null;#e=null;constructor(){super(),this.#n=new MutationObserver(n=>{let i=[];for(let s of n)if(s.type===\"attributes\"){let{attributeName:t,oldValue:o}=s,e=this.getAttribute(t);if(o!==e)try{i.push([t,JSON.parse(e)])}catch{i.push([t,e])}}i.length&&this.#e?.send(JSON.stringify([5,i]))})}connectedCallback(){this.#t=document.createElement(\"div\"),this.appendChild(this.#t)}attributeChangedCallback(n,i,s){switch(n){case\"route\":if(!s)this.#e?.close(),this.#e=null;else if(i!==s){let t=this.getAttribute(\"id\"),o=s+(t?`?id=${t}`:\"\");this.#e?.close(),this.#e=new WebSocket(`ws://${window.location.host}${o}`),this.#e.addEventListener(\"message\",e=>this.messageReceivedCallback(e))}}}messageReceivedCallback({data:n}){let[i,...s]=JSON.parse(n);switch(i){case 0:return this.diff(s);case 1:return this.emit(s);case 2:return this.init(s)}}init([n,i]){let s=[];for(let t of n)t in this?s.push([t,this[t]]):this.hasAttribute(t)&&s.push([t,this.getAttribute(t)]),Object.defineProperty(this,t,{get(){return this[`_${t}`]??this.getAttribute(t)},set(o){let e=this[t];typeof o==\"string\"?this.setAttribute(t,o):this[`_${t}`]=o,e!==o&&this.#e?.send(JSON.stringify([5,[[t,o]]]))}});this.#n.observe(this,{attributeFilter:n,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(i),s.length&&this.#e?.send(JSON.stringify([5,s]))}morph(n){this.#t=k(this.#t,n,i=>s=>{let t=i(s);this.#e?.send(JSON.stringify([4,t.tag,t.data]))})}diff([n]){this.#t=L(this.#t,n,i=>s=>{let t=i(s);this.#e?.send(JSON.stringify([4,t.tag,t.data]))})}emit([n,i]){this.dispatchEvent(new CustomEvent(n,{detail:i}))}disconnectedCallback(){this.#e?.close()}};window.customElements.define(\"lustre-server-component\",S);export{S as LustreServerComponent};"/utf8>>
            )]
    ).

-spec route(binary()) -> lustre@internals@vdom:attribute(any()).
route(Path) ->
    lustre@attribute:attribute(<<"route"/utf8>>, Path).

-spec data(gleam@json:json()) -> lustre@internals@vdom:attribute(any()).
data(Json) ->
    _pipe = Json,
    _pipe@1 = gleam@json:to_string(_pipe),
    lustre@attribute:attribute(<<"data-lustre-data"/utf8>>, _pipe@1).

-spec include(list(binary())) -> lustre@internals@vdom:attribute(any()).
include(Properties) ->
    _pipe = Properties,
    _pipe@1 = gleam@json:array(_pipe, fun gleam@json:string/1),
    _pipe@2 = gleam@json:to_string(_pipe@1),
    lustre@attribute:attribute(<<"data-lustre-include"/utf8>>, _pipe@2).

-spec subscribe(binary(), fun((lustre@internals@patch:patch(SKS)) -> nil)) -> lustre@internals@runtime:action(SKS, lustre:server_component()).
subscribe(Id, Renderer) ->
    {subscribe, Id, Renderer}.

-spec unsubscribe(binary()) -> lustre@internals@runtime:action(any(), lustre:server_component()).
unsubscribe(Id) ->
    {unsubscribe, Id}.

-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-spec do_set_selector(
    gleam@erlang@process:selector(lustre@internals@runtime:action(any(), SLI))
) -> lustre@effect:effect(SLI).
do_set_selector(Sel) ->
    lustre@effect:from(
        fun(_) ->
            Self = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(Self, {set_selector, Sel})
        end
    ).

-spec set_selector(
    gleam@erlang@process:selector(lustre@internals@runtime:action(any(), SLC))
) -> lustre@effect:effect(SLC).
set_selector(Sel) ->
    do_set_selector(Sel).

-spec decode_event(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), any())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_event(Dyn) ->
    gleam@result:'try'(
        (gleam@dynamic:tuple3(
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:dynamic/1,
            fun gleam@dynamic:dynamic/1
        ))(Dyn),
        fun(_use0) ->
            {Kind, Name, Data} = _use0,
            gleam@bool:guard(
                Kind /= 4,
                {error,
                    [{decode_error,
                            gleam@int:to_string(4),
                            gleam@int:to_string(Kind),
                            [<<"0"/utf8>>]}]},
                fun() ->
                    gleam@result:'try'(
                        gleam@dynamic:string(Name),
                        fun(Name@1) -> {ok, {event, Name@1, Data}} end
                    )
                end
            )
        end
    ).

-spec decode_attr(gleam@dynamic:dynamic_()) -> {ok,
        {binary(), gleam@dynamic:dynamic_()}} |
    {error, list(gleam@dynamic:decode_error())}.
decode_attr(Dyn) ->
    (gleam@dynamic:tuple2(
        fun gleam@dynamic:string/1,
        fun gleam@dynamic:dynamic/1
    ))(Dyn).

-spec decode_attrs(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), any())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_attrs(Dyn) ->
    gleam@result:'try'(
        (gleam@dynamic:tuple2(
            fun gleam@dynamic:int/1,
            fun gleam@dynamic:dynamic/1
        ))(Dyn),
        fun(_use0) ->
            {Kind, Attrs} = _use0,
            gleam@bool:guard(
                Kind /= 5,
                {error,
                    [{decode_error,
                            gleam@int:to_string(5),
                            gleam@int:to_string(Kind),
                            [<<"0"/utf8>>]}]},
                fun() ->
                    gleam@result:'try'(
                        (gleam@dynamic:list(fun decode_attr/1))(Attrs),
                        fun(Attrs@1) -> {ok, {attrs, Attrs@1}} end
                    )
                end
            )
        end
    ).

-spec decode_action(gleam@dynamic:dynamic_()) -> {ok,
        lustre@internals@runtime:action(any(), lustre:server_component())} |
    {error, list(gleam@dynamic:decode_error())}.
decode_action(Dyn) ->
    (gleam@dynamic:any([fun decode_event/1, fun decode_attrs/1]))(Dyn).

-spec encode_patch(lustre@internals@patch:patch(any())) -> gleam@json:json().
encode_patch(Patch) ->
    lustre@internals@patch:patch_to_json(Patch).
