-module(xdaa).
%% -export([connect/2]).
-compile(export_all).

-define(TIMEOUT, 5000).
-define(XDAA_VERSION, 0).

%% Sizes are in octets
-define(XDAA_GID_LENGTH, 16).
-define(XDAA_NONCE_LENGTH, 32).
-define(XDAA_SERVER_KEY_EXCHANGE_HEADER_LENGTH, 9).

connect(Host, Port) ->
        tcp_connect(Host, Port).

tcp_connect(Host, Port) ->
        case gen_tcp:connect(Host,
                             Port, 
    		             [binary, {active, false}, {packet, 0}, {keepalive, true}, {nodelay, true}]) of
                {ok, TCPSocket} ->
                        lager:debug("TCP connection established with host:~p on port:~p", [Host, Port]),
                        xdaa_send_client_hello(TCPSocket);
                {error, Reason} ->
                        lager:warning("Error making TCP connection: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_send_client_hello(TCPSocket) ->
        %% TODO: Get the GID (and key-pair) from file.
        GID = <<"1234567898765432">>,
        ClientNonce = enacl:randombytes(?XDAA_NONCE_LENGTH),
        Packet = <<?XDAA_VERSION:8,
                   ?XDAA_GID_LENGTH:16/big,
                   ?XDAA_NONCE_LENGTH:16/big,
                   GID/binary,
                   ClientNonce/binary>>,
        lager:debug("Sending XDAA ClientHello (~p)...", [Packet]),
        case gen_tcp:send(TCPSocket, Packet) of
                ok ->
                        lager:debug("Sent XDAA ClientHello (~p) of length ~p octets", [Packet, bit_size(Packet)/8]),
                        xdaa_wait_on_server_key_exchange_header(TCPSocket, ClientNonce);
                {error, Reason} ->
                        lager:warning("XDAA: Error sending ClientHello: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_wait_on_server_key_exchange_header(TCPSocket, ClientNonce) ->
        case gen_tcp:recv(TCPSocket, ?XDAA_SERVER_KEY_EXCHANGE_HEADER_LENGTH, ?TIMEOUT) of
                {ok, <<?XDAA_VERSION:8,
                       ServerGIDLength:16/big,
                       ServerNonceLength:16/big,
                       ServerPubKeyLength:16/big,
                       ServerSigLength:16/big>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchangeHeader with GIDLength:~p, NonceLength:~p, PubKeyLength:~p, and SigLength:~p",
                                    [ServerGIDLength, ServerNonceLength, ServerPubKeyLength, ServerSigLength]),
                        xdaa_wait_on_server_key_exchange(TCPSocket, ServerGIDLength, ServerNonceLength, ServerPubKeyLength, ServerSigLength, ClientNonce);
                {ok, <<Version:8,
                       _/binary>>
                } ->
                        lager:warning("XDAA: Received ServerKeyExchange header with unsupported version: ~p", [Version]),
                        {error, "XDAA: Unsupported version in ServerKeyExchange header"};
                {ok, _} ->
                        lager:warning("XDAA: Received mal-formed ServerKeyExchange header", []),
                        {error, "XDAA: Mal-formed ServerKeyExchange header"};
                {error, Reason} ->
                        lager:warning("XDAA: Error receiving ServerKeyExchange header: ~p", [Reason]),
                        {error, Reason}
        end.

xdaa_wait_on_server_key_exchange(TCPSocket, ServerGIDLength, ServerNonceLength, ServerPubKeyLength, ServerSigLength, ClientNonce) ->
        case gen_tcp:recv(TCPSocket, ServerGIDLength+ServerNonceLength+ServerPubKeyLength+ServerSigLength, ?TIMEOUT) of
                {ok, <<ServerGID:ServerGIDLength/binary-unit:8,
                       ServerNonce:ServerNonceLength/binary-unit:8,
                       ServerPubKey:ServerPubKeyLength/binary-unit:8,
                       ServerSig:ServerSigLength/binary-unit:8>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchange with GID:~p, Nonce:~p, PubKey:~p, and Sig:~p",
                                    [ServerGID, ServerNonce, ServerPubKey, ServerSig]),
                        xdaa_validate_server_gid(TCPSocket, ServerGID, ServerNonce, ServerPubKey, ServerSig, ClientNonce);
                {ok, _} ->
                        lager:warning("XDAA: Received mal-formed ServerKeyExchange", []),
                        {error, "XDAA: Mal-formed ServerKeyExchange message"};
                {error, Reason} ->
                        {error, Reason}
        end.

xdaa_validate_server_gid(TCPSocket, ServerGID, ServerNonce, ServerPubKey, ServerSig, ClientNonce) ->
        %% TODO: Verify GID is in known list
        GIDVerify = ok,
        case GIDVerify of
                ok ->
                        lager:debug("Server GID (~p) accepted", [ServerGID]),
                        xdaa_validate_server_signature(TCPSocket, ServerGID, ServerNonce, ServerPubKey, ServerSig, ClientNonce);
                {error, Reason} ->
                        lager:info("XDAA: Server GID (~p) rejected", [ServerGID]),
                        {error, Reason}
        end.

xdaa_validate_server_signature(TCPSocket, ServerGID, ServerNonce, ServerPubKey, ServerSig, ClientNonce) ->
        SigStruct = <<ServerPubKey/binary, ClientNonce/binary>>,
        case crypto:verify(ecdsa,
                           sha256,
                           SigStruct,
                           ServerSig,
                           [ServerPubKey, secp256r1] ) of
                true ->
                        lager:debug("XDAA: Validation of server signature successful", []),
                        {ok, TCPSocket};
                false ->
                        lager:warning("XDAA: Validation of server signature failed", []),
                        {error, "XDAA: Validation of server signature failed"}
        end.

%% xdaa_send_client_key_exchange(TCPSocket, ServerNonce) ->
%%         %% TODO: Get the GID (and key-pair) from file.
%%         GID = <<"1234567898765432">>,
%%         Nonce = enacl:randombytes(?XDAA_NONCE_LENGTH),
%%         Packet = <<?XDAA_VERSION:8,
%%                    ?XDAA_GID_LENGTH:16/big,
%%                    ?XDAA_NONCE_LENGTH:16/big,
%%                    GID/binary,
%%                    Nonce/binary>>,
%%         lager:debug("Sending XDAA ClientHello (~p)...", [Packet]),
%%         case gen_tcp:send(TCPSocket, Packet) of
%%                 ok ->
%%                         lager:debug("Sent XDAA ClientHello (~p) of length ~p octets", [Packet, bit_size(Packet)/8]),
%%                         xdaa_wait_on_server_key_exchange_header(TCPSocket);
%%                 {error, Reason} ->
%%                         {error, Reason}
%%         end.
%% 
