-module(epatom).

-export([parse/1, parse/2, from_file/1, from_file/2]).

-define(ATOM_XMLNS,"http://www.w3.org/2005/Atom"). 
-define(FORMAT, <<"%Y-%m-%dT%H:%M:%S">>).

-record(state, {filter      = undefined,
		skip_entry  = false,
		stack       = []}).
		

from_file(Fname) ->
    from_file(Fname, undefined).

from_file(Fname, Updated) ->
    case file:read_file(Fname) of
	{ok, Content} ->
	    parse(Content, Updated);
	Error ->
	    Error
    end.

parse(Xml) ->
    parse(Xml, undefined).

parse(Xml, Filter) ->
    parse2(Xml, Filter).

parse2(Xml, Filter)->
    State = #state{filter      = Filter,
		   skip_entry  = false,
		   stack       = []},
    try erlsom:parse_sax(Xml,State, fun do_parse/2) of
	{ok, #state{stack=Stack}, _} -> {ok, Stack};
	Other -> Other
    catch 
	_:Reason -> {error, Reason}
    end.


do_parse({startElement,Ns,"feed",_,_Attrs},_State) when Ns /= ?ATOM_XMLNS -> 
    throw(not_atom);
do_parse({startElement,_, Element,_,Attrs},State) ->    
    make_start(to_atom(Element), make_attrs(Attrs), State);
do_parse({endElement,_, Element,_},State) ->    
    make_end(to_atom(Element), State);
do_parse({characters, Ch}, State)->
    make_text(Ch, State);
do_parse(_, State)->
    State.

make_start(_Name, _Attrs, #state{skip_entry=true} = State) ->
    State;
make_start(Name, Attrs, #state{stack=Stack} = State) ->
    State#state{stack=[{Name, Attrs,[], undefined}|Stack]};
make_start(_Name, _Attrs, State) ->
    State.


make_end(entry, #state{skip_entry=true} = State) ->
    State#state{skip_entry=false};
make_end(ElName,#state{stack=[{ElName, Attrs, Childs, Value}]} = State) ->
    State#state{stack=[{ElName, Attrs, lists:reverse(Childs), Value}]};
make_end(ElName, #state{stack=[{ElName, Attrs, Childs, Value}|Stack]} = State) ->
    [{ElName1, Attrs1, Childs1, Value1} | TStack] = Stack,
    State#state{stack=[{ElName1, Attrs1, [{ElName, Attrs, lists:reverse(Childs), Value}|Childs1], Value1}|TStack]};
make_end(_, State) ->
    State.

make_text(_Text, #state{skip_entry=true} = State) ->
    State;
make_text(Text, #state{filter={Param, Criteria},
		       stack=[{Param, Attrs, Childs,_},
			      {entry, Attrs1, Childs1, Value1}|TStack]} = State) when Param == updated;
										      Param == published ->
   case is_date_more(Text, Criteria) of
       false -> State#state{skip_entry=true, stack=TStack};
       true  -> State#state{stack=[{Param, Attrs, Childs, Text}|[{entry, Attrs1, Childs1, Value1}|TStack]]}
   end;	   
make_text(Text, #state{stack=[{ElName, Attrs, Childs, _Value}|TStack]} = State) ->
    State#state{stack=[{ElName, Attrs, Childs, Text}|TStack]}.

make_attrs(AttrList) ->
    [{to_atom(Attr), Value} || {attribute,Attr,_,_,Value} <- AttrList].



to_atom("feed") ->        feed;
to_atom("author") ->      author;
to_atom("category") ->    category;
to_atom("contributor") -> contributor;
to_atom("generator") ->   generator;
to_atom("icon") ->        icon;
to_atom("id") ->          id;
to_atom("link") ->        link;
to_atom("logo") ->        logo;
to_atom("rights") ->      rights;
to_atom("subtitle") ->    subtitle;
to_atom("title") ->       title;
to_atom("updated") ->     updated;
to_atom("published") ->   published;
to_atom("rel") ->         rel;
to_atom("content") ->     content;
to_atom("source") ->      source;
to_atom("summary") ->     summary;
to_atom("entry") ->       entry;
to_atom("type") ->        type;
to_atom("src") ->         src;
to_atom("term") ->        term;
to_atom("scheme") ->      scheme;
to_atom("label") ->       label;
to_atom("uri") ->         uri;
to_atom("version") ->     version;
to_atom("href") ->        href;
to_atom("hreflang") ->    hreflang;
to_atom("length") ->      length;
to_atom("name") ->        name;
to_atom("email") ->       email;
to_atom("height") ->      height;
to_atom("width")  ->      widht;
to_atom("image") ->       image;
to_atom(String) ->        String.
    
    
is_date_more(FirstDate, SecondDate) ->
    FirstDateT  = to_erl_date(FirstDate),
    SecondDateT = to_erl_date(SecondDate),
    case calendar:time_difference(FirstDateT, SecondDateT) of
	{D, _} when D =< 0 -> true;
	_ -> false
    end.

to_erl_date(Date) when is_tuple(Date) ->
    Date;
to_erl_date(Date) when is_binary(Date) ->
    <<DateB:19/binary, _/binary>>  = Date,
    {ok, DateT} = tempo:parse(?FORMAT, {datetime, DateB}),
    DateT;
to_erl_date([A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19|_]) ->
    DateB = list_to_binary([A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19]),
    {ok, DateT} = tempo:parse(?FORMAT, {datetime, DateB}),
    DateT;
to_erl_date(Date) ->
    throw({error, {wrong_date_fromat, Date}}).
    

    














