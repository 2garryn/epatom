%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2012, Artem Golovinsky
%%% @doc Header file for epatom.
%%%
%%% Name of element/attribute is element_name()/attr_name(), 
%%% when it is defined in RFC4287 and presented 
%%% in http://www.w3.org/2005/Atom namespace. 
%%% Otherwise, name of element/attribute is string()
%%%
%%% @end
%%%-------------------------------------------------------------------

-type element_name() :: feed | author | category | contributor | generator |
			icon | id | link | logo | rights | subtitle | title |
			updated | published | content | source | summary |
			entry | uri | version | name | email. 

-type attr_pair()    :: {{attr_name() | string()}, string()}.

-type attr_name()    :: rel | type | src | term | scheme | label | href | 
			hreflang | length. 

-type element() :: #element{name    :: element_name() | string(),
			    attrs   :: [attr_pair()],
			    childs  :: [element()],
			    text    :: undefined | string()}.

-record(element, {name,  attrs, childs=[], text}).

