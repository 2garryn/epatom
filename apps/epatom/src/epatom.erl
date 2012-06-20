-module(epatom).

-export([parse/1,parse/2]).


-record(fe_tree, { 
	  xmlns,
	  author=[],
	  category=[],
	  contributor=[],
	  icon,
	  logo,
	  link,
	  generator=[],
	  id,
	  rights,
	  title,
	  subtitle,
	  updated,
	  entry=[], 
	  content,
	  published,
	  source }).
	  

-record(state, { feed_rec,
		 temp_fe,
		 stop_date, 
		 prepare_elem,
		 ready_elem}).

parse(Xml) ->
    parse2(Xml, undefined).

parse(Xml, StrDate) ->
    parse2(Xml, StrDate).

parse2(Xml, StrDate)->
    State = {ok, #state{feed_rec=undefined,
			temp_fe=#fe_tree{},
			stop_date=StrDate,
			prepare_elem=undefined,	
			ready_elem=[]
		       }},
    case erlsom:parse_sax(Xml, State, fun do_parse/2) of
	{ok, #state{feed_rec=Feed}} ->
	    {ok, Feed};
	NotUpdated ->
	    NotUpdated
    end.


do_parse(startDocument, Acc) ->
    Acc;
do_parse(endDocument, Acc) ->
    Acc;
do_parse({startElement,_, Element,_,Attrs},State) ->    
    make_start(Element, make_attrs(Attrs), State);

do_parse({endElement,_, Element,_},State) ->    
    make_end(Element, State);

do_parse({characters, Ch}, State)->
    make_char(Ch, State).




%%% ------------------- FOR FEED ---------------------------
make_start("feed",Attrs, {ok, #state{temp_fe = Feed} = State}) -> 
    NewFeed = Feed#fe_tree{xmlns=gv("xmlns", Attrs)},
    {ok, State#state{temp_fe=NewFeed}};

make_start("author", _, {ok, State}) ->
    {ok, State};

make_start("name", _, {ok,State}) ->
    {ok, State#state{prepare_elem=name}};

make_start("uri", _, {ok, State}) ->
    {ok, State#state{prepare_elem=uri}};

make_start("email", _, {ok, State}) ->
    {ok, State#state{prepare_elem=email}};

make_start("category", Attr, {ok, #state{temp_fe=FeTree} = State}) ->
    NewAttrs = [{category_attr(AttrName),Value} || {AttrName, Value} <- make_attrs(Attr)],
    OldAttrs = FeTree#fe_tree.category, 
    NewFeed    = FeTree#fe_tree{category=[NewAttrs, OldAttrs]},
    {ok, State#state{temp_fe=NewFeed}};

make_start("contributor", _, {ok,  State}) ->
    {ok, State};

make_start("icon", _, {ok,  State}) ->
    {ok, State#state{prepare_elem=icon}}; 
 
make_start("generator", Attr, {ok, #state{temp_fe=FeTree} = State}) ->
    NewAttrs = [{generator_attr(AttrName),Value} || {AttrName, Value} <- make_attrs(Attr)], 
    NewFeed    = FeTree#fe_tree{generator=NewAttrs},
    {ok, State#state{temp_fe=NewFeed, prepare_elem=generator_text}};

make_start("id", _, {ok, State}) ->
    {ok, State#state{prepare_elem=id}};

make_start("link", Attr, {ok, #state{temp_fe=FeTree} = State}) ->
    NewAttrs = [{link_attr(AttrName),Value} || {AttrName, Value} <- make_attrs(Attr)],
    OldLink = FeTree#fe_tree.link,
    NewFeed  = FeTree#fe_tree{link=[NewAttrs,OldLink]},
    {ok, State#state{temp_fe=NewFeed}};

make_start("logo", _, {ok, State}) ->
    {ok, State#state{prepare_elem=logo}};

make_start("rights", Attr, {ok, #state{temp_fe=FeTree}  = State}) ->
    {PrElem, TempValue} = is_xhtml_supported(Attr),
    NewFeed = FeTree#fe_tree{rights=TempValue},
    {ok, State#state{temp_fe=NewFeed, prepare_elem=PrElem}};

make_start("subtitle", Attr, {ok, #state{temp_fe=Feed} = State}) ->
    {PrElem, TempValue} = is_xhtml_supported(Attr),
    NewFeed = Feed#fe_tree{subtitle=TempValue},
    {ok, State#state{temp_fe=NewFeed, prepare_elem=PrElem}};

 
make_start("title", Attr, {ok, #state{temp_fe=Feed} = State}) ->
    {PrElem, TempValue} = is_xhtml_supported(Attr),
    NewFeed = Feed#fe_tree{title=TempValue},
    {ok, State#state{temp_fe=NewFeed, prepare_elem=PrElem}};

make_start("updated", _, {ok, #state{temp_fe=_Feed} = State}) ->
    {ok, State#state{prepare_elem=updated}};

make_start("entry", _, {ok, #state{feed_rec=undefined,temp_fe=Feed} = State}) ->
    {ok, State#state{feed_rec=Feed,temp_fe=#fe_tree{}}};

make_start(_,_,State) ->
    State.








make_end("author", {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) ->
    NewAuthor = [ReElem,Feed#fe_tree.author],
    NewFeed = Feed#fe_tree{author=NewAuthor},
    NewState = State#state{temp_fe=NewFeed,prepare_elem=undefined, ready_elem=[]},
    {ok, NewState};  

make_end("contributor", {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) ->
    NewContributor = [ReElem,Feed#fe_tree.contributor],
    NewFeed = Feed#fe_tree{contributor=NewContributor},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState};    

make_end("icon",  {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) ->
    [{icon, Icon}|_] = ReElem,
    NewFeed = Feed#fe_tree{icon=Icon},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState};   

make_end("generator", {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) ->
    [{generator_text, Gener}|_] = ReElem,
    AttrGen = Feed#fe_tree.generator,
    NewFeed = Feed#fe_tree{generator=[{text,Gener}|AttrGen]},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined,ready_elem=[]},
    {ok, NewState}; 

make_end("id", {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) ->
    [{id, Id}|_] = ReElem,
    NewFeed = Feed#fe_tree{id=Id},
    NewState = State#state{temp_fe=NewFeed,prepare_elem=undefined, ready_elem=[]},
    {ok, NewState};  

make_end("logo", {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) -> 
    [{logo, Logo}|_] = ReElem,
    NewFeed = Feed#fe_tree{logo=Logo},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState};    
 
make_end("rights", {ok, #state{ready_elem=[T|_],temp_fe=Feed} = State}) ->
    NewFeed = Feed#fe_tree{rights=T},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState}; 

make_end("subtitle", {ok, #state{ready_elem=[T|_],temp_fe=Feed} = State}) ->
    NewFeed = Feed#fe_tree{subtitle=T},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState}; 

make_end("title", {ok, #state{ready_elem=[T|_],temp_fe=Feed} = State}) ->
    NewFeed = Feed#fe_tree{title=T},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState}; 

make_end("updated", {ok, #state{ready_elem=[{updated, Updated}|_], feed_rec=undefined, stop_date=Updated }= _State}) ->  
    {ok,not_updated};

make_end("updated", {ok, #state{ready_elem=ReElem,temp_fe=Feed} = State}) ->
    [{updated, Updated}|_] = ReElem,
    NewFeed = Feed#fe_tree{updated=Updated},
    NewState = State#state{temp_fe=NewFeed, prepare_elem=undefined, ready_elem=[]},
    {ok, NewState};     
 
make_end("entry", {ok, #state{feed_rec=Feed,temp_fe=Entr} = State}) ->
    OldEntr = Feed#fe_tree.entry,
    NewFeed = Feed#fe_tree{entry=[Entr|OldEntr]},
    {ok, State#state{feed_rec=NewFeed,temp_fe=#fe_tree{}}};

make_end(_,State) ->
    State.
    








make_char(Ch, {ok, #state{prepare_elem=PrElem, ready_elem=ReElem} = State}) when PrElem /= undefined ->
    NewState = State#state{prepare_elem=undefined,
			   ready_elem=[{PrElem,Ch}|ReElem]},
    {ok, NewState}; 
make_char(_, State) ->
    {ok, State}.

make_attrs(AttrList) ->
    [{Attr, Value} || {attribute,Attr,[],[],Value} <- AttrList].

gv(Attr, AttrList) ->
    proplists:get_value(Attr, AttrList, undefined).
    
category_attr("term") ->
    term;
category_attr("scheme") ->
    scheme;
category_attr("label") ->
    label;
category_attr(_) ->
    undefined.

generator_attr("uri") ->
    uri;
generator_attr("version") ->
    version.

link_attr("href") ->
    href;
link_attr("rel") ->
    rel;
link_attr("type") ->
    type;
link_attr("hreflang") ->
    hreflang;
link_attr("title") ->
    title;
link_attr("length") ->
    length.


is_xhtml_supported(Attr) ->
    case gv("type", Attr) of
	"text" ->
	    {text,undefined};
	"html" ->
	    {html,undefined};
	_ ->
	    {undefined, {xhtml,"not_supported"}}
    end.
    		     







