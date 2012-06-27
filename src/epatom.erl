%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2012, Artem Golovinsky
%%% @doc Module for converting Atom XML to erlang-friendly format.
%%%      Filtering by published/updated name. It means, entry will be 
%%%      returned if entry published/updated date > filter entry 
%%%      published/updated date
%%% @end
%%%-------------------------------------------------------------------
-module(epatom).

-export([parse/1, parse/2]).

-include("epatom.hrl").

-define(ATOM_XMLNS,"http://www.w3.org/2005/Atom"). 
-define(FORMAT, <<"%Y-%m-%dT%H:%M:%S">>).

%%%-------------------------------------------------------------------
%%% Type specification
%%%-------------------------------------------------------------------

-type date()      :: binary()
		   | string()
		   | calendar:datetime()
		   | undefined.

-type date_crt()  :: published 
		   | updated.

-type filter()    :: {date_crt(), date()} 
		   | undefined.

-type atom_xml()  :: binary() 
		   | string().

-type error()     :: not_atom 
		   | wrong_date_format.

%%%------------------------------------------------------------------  

-record(state, {filter      = undefined :: filter(),
		skip_entry  = false     :: boolean(),
		stack       = []}).
%%%------------------------------------------------------------------

%% @doc Parse Atom Format string fully, without
%%      filtering by published/updated date.
%%      @equiv parse(Xml, undefined)
%% @end
-spec parse(atom_xml()) -> {ok, [#element{}]}
			 | {error, error()}.
parse(Xml) ->
    parse(Xml, undefined).

%% @doc Parse Atom Format string with filtering by 
%%      published/updated field. Entries are containing
%%      published/updated date MORE than in Filter 
%%      will be returned.
%%      See @see epatom_examples
%% @end
-spec parse(atom_xml(), filter()) -> {ok, [#element{}]}
				   | {error, error()}.
parse(Xml, Filter) ->
    parse2(Xml, Filter).

%% @private
%% @doc Helper function to execute parsing by erlsom
%%      and catch results. 
%% @end
-spec parse2(atom_xml(), filter()) -> {ok, [#element{}]}
				   | {error, error()}.
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

%% @private
%% @doc Helper function. Callback for erlsom sax parser.
%% @end
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

%% @private
%% @doc Helper function. Adding new element with attributes 
%%      to stack or skip entry if it does not meet to filter.
%% @end
make_start(_Name, _Attrs, #state{skip_entry=true} = State) ->
    State;
make_start(Name, Attrs, #state{stack=Stack} = State) ->
    State#state{stack=[#element{name  = Name, 
				attrs = Attrs}|Stack]};
make_start(_Name, _Attrs, State) ->
    State.

%% @private
%% @doc Helper function. Finishing element processing and
%%      put current finished record to child list of parent record.
%% @end
make_end(entry, #state{skip_entry=true} = State) ->
    State#state{skip_entry=false};
make_end(ElName,#state{stack=[#element{name=ElName,childs=Childs} = El]} = State) ->
    State#state{stack=[El#element{childs=lists:reverse(Childs)}]};
make_end(ElName, #state{stack=[#element{name=ElName,childs=Childs}=EndEl|Stack]} = State) ->
    [#element{childs=ParChilds} = ParentEl|TStack] = Stack,
    NewParChilds = [EndEl#element{childs=lists:reverse(Childs)}|ParChilds],
    State#state{stack=[ParentEl#element{childs=NewParChilds}|TStack]};
make_end(_, State) ->
    State.

%% @private
%% @doc Helper function. Putting text node of element to element record.
%% @end
make_text(_Text, #state{skip_entry=true} = State) ->
    State;
make_text(Text, #state{filter={Param, Criteria},
		       stack=[#element{name=Param} = UpdElem,
			      #element{name=entry} = EntElem|TStack]} = State) when Param == updated;
										    Param == published ->
   case is_date_more(Text, Criteria) of
       false -> State#state{skip_entry=true, stack=TStack};
       true  -> State#state{stack=[UpdElem#element{text=Text}|[EntElem|TStack]]}
   end;	   
make_text(Text, #state{stack=[Elem|TStack]} = State) ->
    State#state{stack=[Elem#element{text=Text}|TStack]}.

%% @private
%% @doc Helper function. Getting simple attribute tuple
%%      from erlsom attribute format.
%% @end
make_attrs(AttrList) ->
    [{to_atom(Attr), Value} || {attribute,Attr,_,_,Value} <- AttrList].

%% @private
%% @doc Helper function. Convert name of element/attribute to atom.
%%      Only names in http://www.w3.org/2005/Atom namespace will be 
%%      converted. String will be returned for others names.
%% @end
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
to_atom(String) ->        String.
    
%% @private
%% @doc Helper function. Comparing of two dates.
%%      'true' if FirstDate >= SecondDate.
%% @end    
is_date_more(FirstDate, SecondDate) ->
    FirstDateT  = to_erl_date(FirstDate),
    SecondDateT = to_erl_date(SecondDate),
    case calendar:time_difference(FirstDateT, SecondDateT) of
	{D, _} when D =< 0 -> true;
	_ -> false
    end.

%% @private
%% @doc Helper function. Converting date in binary/string
%%      format to calendar:datetime() format. 
%% @end  
to_erl_date(Date) when is_tuple(Date) ->
    Date;
to_erl_date(Date) when is_list(Date) ->
    to_erl_date(list_to_binary(Date));
to_erl_date(Date) when is_binary(Date) ->
    <<DateB:19/binary, _/binary>>  = Date,
    {ok, DateT} = tempo:parse(?FORMAT, {datetime, DateB}),
    DateT;
to_erl_date(Date) ->
    throw({wrong_date_format, Date}).


