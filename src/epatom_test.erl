%%%-------------------------------------------------------------------
%%% @author Artem Golovinsky <artemgolovinsky@gmail.com>
%%% @copyright (C) 2012, Artem Golovinsky
%%% @doc Tests for epatom
%%% @end
%%%-------------------------------------------------------------------
-module(epatom_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

entry_all_test() ->
    AllXML = atom_xml_all(),
    AllAtomParsed = atom_parsed_all(),
    {ok,AllAtomParsed} = epatom:parse(AllXML).

entry_published_test() ->
    AllXML = atom_xml_all(),
    PublishedAtomParsed = atom_parsed_published(),
    {ok,PublishedAtomParsed} = epatom:parse(AllXML, {published,"2003-12-14T18:30:02Z"}).

entry_updated_test() ->
    AllXML = atom_xml_all(),
    UpdatedAtomParsed = atom_parsed_updated(),
    {ok,UpdatedAtomParsed} = epatom:parse(AllXML, {updated,"2004-12-15T18:30:02Z"}).

not_atom_test() ->
    NotAtomXML = not_atom_xml(),
    {error, not_atom} = epatom:parse(NotAtomXML).
    
atom_xml_all() ->
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
     <feed xmlns=\"http://www.w3.org/2005/Atom\">
       <title>Test feed</title>
       <link href=\"http://example.org/\"/>
       <updated>2003-12-13T18:30:02Z</updated>
       <author>
          <name>John Doe</name>
          <email>example@mail.com</email>
       </author>
       <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
       <entry>
          <title>Test entry 1</title>
          <link href=\"http://example.org/test_entry_1\"/>
          <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
          <updated>2005-12-13T18:30:02Z</updated>
          <published>2003-12-13T18:30:02Z</published>
          <summary>Some text 1</summary>
       </entry>
       <entry>
          <title>Test entry 2</title>
          <link href=\"http://example.org/test_entry_1\"/>
          <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
          <updated>2005-12-13T18:30:02Z</updated>
          <published>2003-12-13T18:30:02Z</published>
          <summary>Some text 2</summary>
       </entry>
       <entry>
          <title>Test entry 3</title>
          <link href=\"http://example.org/test_entry_1\"/>
          <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
          <updated>2004-12-13T18:30:02Z</updated>
          <published>2004-12-13T18:30:02Z</published>
          <summary>Some text 3</summary>
       </entry>
       <entry>
          <title>Test entry 4</title>
          <link href=\"http://example.org/test_entry_1\"/>
          <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
          <updated>2004-12-13T18:30:02Z</updated>
          <published>2004-12-13T18:30:02Z</published>
          <summary>Some text 4</summary>
       </entry>
   </feed>".

not_atom_xml() ->
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>
     <feed xmlns=\"http://www.w3.org/2005/rss\">
       <title>Test feed</title>
       <link href=\"http://example.org/\"/>
       <updated>2003-12-13T18:30:02Z</updated>
       <author>
          <name>John Doe</name>
          <email>example@mail.com</email>
       </author>
       <id>urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6</id>
       <entry>
          <title>Test entry 1</title>
          <link href=\"http://example.org/test_entry_1\"/>
          <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
          <updated>2005-12-13T18:30:02Z</updated>
          <published>2003-12-13T18:30:02Z</published>
          <summary>Some text 1</summary>
       </entry>    
     </feed>".

atom_parsed_all() ->
        [{element,feed,[],
             [{element,title,[],[],"Test feed"},
              {element,link,[{href,"http://example.org/"}],[],undefined},
              {element,updated,[],[],"2003-12-13T18:30:02Z"},
              {element,author,[],
                  [{element,name,[],[],"John Doe"},
                   {element,email,[],[],"example@mail.com"}],
                  undefined},
              {element,id,[],[],
                  "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6"},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 1"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2005-12-13T18:30:02Z"},
                   {element,published,[],[],"2003-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 1"}],
                  undefined},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 2"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2005-12-13T18:30:02Z"},
                   {element,published,[],[],"2003-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 2"}],
                  undefined},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 3"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2004-12-13T18:30:02Z"},
                   {element,published,[],[],"2004-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 3"}],
                  undefined},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 4"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2004-12-13T18:30:02Z"},
                   {element,published,[],[],"2004-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 4"}],
                  undefined}],
             undefined}].

atom_parsed_published() ->
        [{element,feed,[],
             [{element,title,[],[],"Test feed"},
              {element,link,[{href,"http://example.org/"}],[],undefined},
              {element,updated,[],[],"2003-12-13T18:30:02Z"},
              {element,author,[],
                  [{element,name,[],[],"John Doe"},
                   {element,email,[],[],"example@mail.com"}],
                  undefined},
              {element,id,[],[],
                  "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6"},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 3"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2004-12-13T18:30:02Z"},
                   {element,published,[],[],"2004-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 3"}],
                  undefined},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 4"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2004-12-13T18:30:02Z"},
                   {element,published,[],[],"2004-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 4"}],
                  undefined}],
             undefined}].  

atom_parsed_updated() ->
        [{element,feed,[],
             [{element,title,[],[],"Test feed"},
              {element,link,[{href,"http://example.org/"}],[],undefined},
              {element,updated,[],[],"2003-12-13T18:30:02Z"},
              {element,author,[],
                  [{element,name,[],[],"John Doe"},
                   {element,email,[],[],"example@mail.com"}],
                  undefined},
              {element,id,[],[],
                  "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6"},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 1"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2005-12-13T18:30:02Z"},
                   {element,published,[],[],"2003-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 1"}],
                  undefined},
              {element,entry,[],
                  [{element,title,[],[],"Test entry 2"},
                   {element,link,
                       [{href,"http://example.org/test_entry_1"}],
                       [],undefined},
                   {element,id,[],[],
                       "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"},
                   {element,updated,[],[],"2005-12-13T18:30:02Z"},
                   {element,published,[],[],"2003-12-13T18:30:02Z"},
                   {element,summary,[],[],"Some text 2"}],
                  undefined}],
             undefined}].
