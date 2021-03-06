-module(atom).

-export([new/1,update/3, add_item/7, add_file/3, clear_items/1, dump/1]).

-export([human_filesize/1]).

-record(atom, {header, items}).
-record(atom_header, {name, subtitle=false, rights=false}).
-record(atom_item, {uri, title, link, timestamp, summary, categories=[]}).

new(Name) ->
    Header = #atom_header{name=Name},
    #atom{header=Header, items=[]}.
    
%%
% update(Key, Value, Atom) -> NewAtom
% Atom = NewAtom = atom:new()
% Key = subtitle�| rights
% Value = string()
update(Key, Value, Atom) ->
    Header = Atom#atom.header,
    
    NewHeader = case Key of
        subtitle -> Header#atom_header{subtitle=Value};
        rights -> Header#atom_header{rights=Value}
    end,
    
    Atom#atom{header=NewHeader}.
    
add_item(Uri, Title, Link, Timestamp, Summary, Categories, Atom = #atom{}) ->
    NewItem = #atom_item{
        uri = Uri,
        title = Title,
        link = Link,
        timestamp = Timestamp,
        summary = Summary,
        categories = Categories
    },
    Atom#atom{items=Atom#atom.items ++ [NewItem]}.
    
clear_items(Atom) ->
    Atom#atom{items=[]}.
    
%% ========================================================================== %%
%% Helper methods for dump & add_file:
category(File) ->
    case string:right(File,4) of
        ".avi" -> ["Video"];
        ".mpg" -> ["Video"];
        ".mkv" -> ["Video"];
        ".mp4" -> ["Audio", "Video"];
        ".jpg" -> ["Picture"];
        ".png" -> ["Picture"];
        
        ".app" -> ["Application"];
        _ -> ["Unknown"]
    end.

% Format a integer in a human readable way

human_filesize(Size) ->
    KiloByte = 1024,
    MegaByte = KiloByte * 1024,
    GigaByte = MegaByte * 1024,
    TeraByte = GigaByte * 1024,
    PetaByte = TeraByte * 1024,
    
    human_filesize(Size, [
        {PetaByte, "PB"},
        {TeraByte, "TB"},
        {GigaByte, "GB"},
        {MegaByte, "MB"},
        {KiloByte, "KB"}
    ]).
    

human_filesize(Size, []) ->
    integer_to_list(Size) ++ " Byte";
human_filesize(Size, [{Block, Postfix}|List]) ->
    case Size >= Block of
        true ->
            float_as_string(Size / Block) ++ " " ++ Postfix;
        false ->
            human_filesize(Size, List)
    end.
  
% Why do I have to do that? *cry*  
float_as_string(Float) ->
    Integer = trunc(Float), % Part before the .
    NewFloat = 1 + Float - Integer, % 1.<part behind>
    FloatString = float_to_list(NewFloat), % "1.<part behind>"
    integer_to_list(Integer) ++ string:sub_string(FloatString, 2, 4).
    
    
summary(File) ->
    Es = string:tokens(File, "/"),
    
    "<b>Datei</b>: " ++ lists:last(Es) ++ "<br />\n" ++
    "Pfad: /" ++ string:join(lists:sublist(Es, 1, length(Es)-1), "/") ++ "<br />\n" ++
    "Groesse: " ++ human_filesize(filelib:file_size(File)) ++ "<br />\n" ++
    "Kategory: " ++ string:join(category(File), " - ") ++ "<br />\n".
    
add_file(File, Url, Atom) ->
    add_item(
        Url, % uri
        lists:last(string:tokens(File, "/")) ++ " - " ++ string:join(category(File),", "), % title
        Url,
        filelib:last_modified(File),
        summary(File),
        category(File),
    Atom).
    
% ============================================================ %    

xml_escape(Text) ->
    xml_escape(Text, []).

xml_escape([], Result) ->
    Result;
xml_escape([Next|Rest], Result) ->
    Escaped = case Next of
        "<" -> "&lt;";
        ">" -> "&gt;";
        "&" -> "&amp;";
        _ -> Next
    end,
    xml_escape(Rest, Result ++ [Escaped]).

int_to_string(Integer) ->
    Prefix = case Integer < 10 of
        true -> "0";
        false -> ""
    end,
    Prefix ++ integer_to_list(Integer).

format_time({{Year, Month, Day}, {Hour, Minute, Second}})
    when Year > 0
    andalso Month > 0 andalso Month < 13
    andalso Day > 0 andalso Day < 32
    andalso Hour >= 0 andalso Hour < 25
    andalso Minute >= 0 andalso Minute < 60
    andalso Second >= 0 andalso Second < 62 % Yes, 62!
    ->
    int_to_string(Year) ++ "-" ++
    int_to_string(Month) ++ "-" ++
    int_to_string(Day) ++ "T" ++
    int_to_string(Hour) ++ ":" ++
    int_to_string(Minute) ++ ":" ++
    int_to_string(Second) ++ "Z".
    
dump(Atom) ->
    AtomHeader = Atom#atom.header,
    
    ContentHeader = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" ++
              "<feed xml:lang=\"en\" xmlns=\"http://www.w3.org/2005/Atom\">\n" ++
              " <id>" ++ integer_to_list(erlang:phash2(AtomHeader#atom_header.name)) ++ "</id>" ++
              " <updated>" ++ format_time(calendar:local_time()) ++ "</updated>\n" ++
              " <title type=\"text\">" ++ AtomHeader#atom_header.name ++ "</title>\n" ++
              case AtomHeader#atom_header.subtitle of
                false -> "";
                A -> " <subtitle type=\"html\">" ++ xml_escape(A) ++ "</subtitle>"
              end ++
              " <generator uri=\"http://www.moinz.de/\" version=\"1.0\">Moinz.de ewalker</generator>",
    
    ContentBody = lists:foldl(
        fun(Item, Body) ->
            Body ++ "<entry>\n" ++
                "<title>" ++ xml_escape(Item#atom_item.title) ++ "</title>\n" ++
                "<id>" ++ xml_escape(Item#atom_item.uri) ++ "</id>\n" ++
                "<updated>" ++ format_time(Item#atom_item.timestamp) ++ "</updated>\n" ++
                "<link href=\"" ++ xml_escape(Item#atom_item.link) ++ "\" />\n" ++
                "<category>" ++ lists:foldl(fun(Cat, Text) -> Text ++ "<term>" ++ Cat ++ "</term>" end, "", Item#atom_item.categories) ++ "</category>" ++
                "<summary type=\"html\">" ++ xml_escape(Item#atom_item.summary) ++ "</summary>" ++
            "</entry>\n\n"
        end,
        "",
        Atom#atom.items
    ),
    
    ContentFooter = "</feed>",
    
    Content = ContentHeader ++ ContentBody ++ ContentFooter,
    {ok, Content}.
    

%% 
%<?xml version="1.0" encoding="utf-8"?>
%<feed xml:lang="en" xmlns="http://www.w3.org/2005/Atom">
%  <title type="text">Dateien auf Moon</title>
%  <link href="http://moon/"/>
%  <link rel="self" type="application/atom+xml" href="http://moon.localdomain/~zeisss/newfiles.atom" />
%  <updated>2010-01-27T11:05:01Z</updated>
%  <id>http://moon/</id>
%
%  <entry>
%    <title>Chuck - 3x05 - Chuck vs First Class.mkv - Video</title>
%    <id>http://moon/dav/Videos/Serien/Chuck/Season_3/Chuck_-_3x05_-_Chuck_vs_First_Class.mkv</id>
%    <updated>2010-01-26T19:54:55Z</updated>
%    <link href="http://moon/dav/Videos/Serien/Chuck/Season 3/Chuck - 3x05 - Chuck vs First Class.mkv"/>
%    <content type="html">Kategory: Video&lt;br /&gt;Pfad: /public/Videos/Serien/Chuck/Season 3&lt;br /&gt;Name: Chuck - 3x05 - Chuck vs First Class.mkv&lt;br /&gt;</content>
%    <summary type="html">Kategory: Video&lt;br /&gt;Pfad: /public/Videos/Serien/Chuck/Season 3&lt;br /&gt;Name: Chuck - 3x05 - Chuck vs First Class.mkv&lt;br /&gt;</summary>
%    <category><term>Video</term></category>
%  </entry>
%</feed>