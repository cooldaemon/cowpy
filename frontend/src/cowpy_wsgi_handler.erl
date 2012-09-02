-module(cowpy_wsgi_handler).
-author('cooldaemon@gmail.com').

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).
-export([stream_fun/3]).

init({_Any, http}, Req, [MP, Context]) ->
  {ok, Socket} = erlzmq:socket(Context, [req, {active, false}]),
  ok = erlzmq:connect(Socket, "inproc://broker"),
  {ok, Req, {MP, Socket}}.

handle(Req, State) ->
  Header = get_header(Req, State),
  Req2 = request(Req, State, Header),
  Req3 = reply(Req2, State, recv_header(State)),
  {ok, Req3, State}.

terminate(_Req, {_MP, Socket}) ->
  erlzmq:close(Socket),
  ok.

get_header(Req, State) ->
  {get_http_headers(Req, State, get_wsgi_headers(Req))}.

get_wsgi_headers(Req) ->
  [
    {<<"SERVER_PROTOCOL">>, get_server_protocol(Req)},
    {<<"SERVER_NAME">>,     get_server_name(Req)},
    {<<"SERVER_PORT">>,     get_server_port(Req)},
    {<<"REMOTE_ADDR">>,     get_remote_addr(Req)},
    {<<"REQUEST_METHOD">>,  get_request_method(Req)},
    {<<"PATH_INFO">>,       get_path_info(Req)},
    {<<"QUERY_STRING">>,    get_query_string(Req)}
  ].

get_server_protocol(Req) ->
  case cowboy_http_req:version(Req) of
    {{1, 1}, _} -> <<"HTTP/1.1">>;
    _ -> <<"HTTP/1.0">>
  end.

get_server_name(Req) ->
  {Host, _} = cowboy_http_req:raw_host(Req),
  Host.

get_server_port(Req) ->
  {Port, _} = cowboy_http_req:port(Req),
  Port.

get_remote_addr(Req) ->
  {{A1, A2, A3, A4}, _} = cowboy_http_req:peer_addr(Req),
  list_to_binary(io_lib:format("~B.~B.~B.~B", [A1, A2, A3, A4])).

get_request_method(Req) ->
  {Method, _} = cowboy_http_req:method(Req),
  atom_to_binary(Method).

atom_to_binary(Atom) ->
  list_to_binary(atom_to_list(Atom)).

get_path_info(Req) ->
  {Path, _} = cowboy_http_req:raw_path(Req),
  Path.

get_query_string(Req) ->
  {QueryString, _} = cowboy_http_req:raw_qs(Req),
  QueryString.

get_http_headers(Req, State, Headers) ->
  {HTTPHeaders, _} = cowboy_http_req:headers(Req),
  add_headers(State, HTTPHeaders, Headers).

add_headers(_State, [], Headers) ->
  Headers;
add_headers(State, [HTTPHeader | HTTPHeaders], Headers) ->
  add_headers(
    State,
    HTTPHeaders,
    [convert_http_header(State, HTTPHeader) | Headers]
  ).

convert_http_header(State, {Name, Value}) ->
  {
    add_prefix(replace_separator(State, atom_to_upper(Name))),
    Value
  }.

atom_to_upper(Name) ->
  list_to_binary(string:to_upper(atom_to_list(Name))).

binary_to_upper(Name) ->
  list_to_binary(string:to_upper(binary_to_list(Name))).

replace_separator({MP, _Socket}, Name) ->
  re:replace(Name, MP, "_", [global, {return, binary}]).

add_prefix(<<"CONTENT_TYPE">>) ->
  <<"CONTENT_TYPE">>;
add_prefix(<<"CONTENT_LENGTH">>) ->
  <<"CONTENT_LENGTH">>;
add_prefix(Name) ->
  <<"HTTP_", Name/binary>>.

request(Req, {_MP, Socket}, Header) ->
  case cowboy_http_req:has_body(Req) of
    {false, _} ->
      request_header(Socket, Header, false),
      Req;
    _ ->
      request_header(Socket, Header, true),
      request_body(Req, Socket)
  end.

request_header(Socket, Header, false) ->
  ok = erlzmq:send(Socket, msgpack:pack(Header));
request_header(Socket, Header, true) ->
  ok = erlzmq:send(Socket, msgpack:pack(Header), [sndmore]).

request_body(Req, Socket) ->
  do_request_body(Req, Socket).

do_request_body(Req, Socket) ->
  case cowboy_http_req:stream_body(Req) of
    {ok, Data, Req2} ->
      ok = erlzmq:send(Socket, Data, [sndmore]),
      do_request_body(Req2, Socket);
    {done, Req2} ->
      ok = erlzmq:send(Socket, <<"">>),
      Req2%;
%   {error, Reason} ->
%     {error, Reason}
  end.

recv_header({_MP, Socket}) ->
  {ok, PackedHeader} = erlzmq:recv(Socket),
  {{ResponseHeader}, <<>>} = msgpack:unpack(PackedHeader),
  ResponseHeader.

reply(Req, {_MP, Socket}, ResponseHeader) ->
  Headers = get_headers(ResponseHeader),
  {ok, Req2} = cowboy_http_req:reply(
    get_status(ResponseHeader),
    Headers,
    get_body(Req, Socket, Headers),
    Req
  ),
  Req2.

get_status(ResponseHeader) ->
  binary_to_integer(proplists:get_value(<<"status">>, ResponseHeader)).

binary_to_integer(Bin) ->
  {Integer, _Tail} = string:to_integer(binary_to_list(Bin)),
  Integer.

get_headers(ResponseHeader) ->
  Headers = proplists:get_value(<<"headers">>, ResponseHeader),
  lists:map(
    fun ([Name, Value]) ->
      {convert_header_name(Name), Value}
    end,
    Headers
  ).

convert_header_name(Name) ->
  case binary_to_upper(Name) of
    <<"CONTENT-LENGTH">> -> <<"Content-Length">>;
    _ -> Name
  end.

get_body(Req, Socket, Headers) ->
  case search_content_length(Headers) of
    {found, Length} -> {Length, get_stream_fun(Req, Socket)};
    _ -> recv_body(Socket, <<"">>)
  end.

search_content_length([]) ->
  not_found;
search_content_length([{<<"Content-Length">>, Value} | _Headers]) ->
  case is_integer(Value) of
    true -> {found, Value};
    false -> {found, binary_to_integer(Value)}
  end;
search_content_length([_Header | Headers]) ->
  search_content_length(Headers).

recv_body(Socket, Body) ->
  case erlzmq:getsockopt(Socket, rcvmore) of
    {ok, 0} ->
      Body;
    {ok, 1} ->
      {ok, BodyPart} = erlzmq:recv(Socket),
      recv_body(Socket, <<Body/binary, BodyPart/binary>>)
  end.

get_stream_fun(Req, Socket) ->
  {ok, Transport, TSocket} = cowboy_http_req:transport(Req),
  fun () ->
    stream_fun(Socket, Transport, TSocket)
  end.

stream_fun(Socket, Transport, TSocket) ->
  case erlzmq:getsockopt(Socket, rcvmore) of
    {ok, 0} -> ok;
    {ok, 1} ->
      {ok, BodyPart} = erlzmq:recv(Socket),
      Transport:send(TSocket, BodyPart),
      stream_fun(Socket, Transport, TSocket)
  end.
