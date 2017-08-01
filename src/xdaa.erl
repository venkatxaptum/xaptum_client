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
                        lager:debug("Sent XDAA ClientHello (~p) of length ~p octets", [Packet, bit_size(Packet) div 8]),
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
                       ServerECDHEPubKeyLength:16/big,
                       ServerSigLength:16/big>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchangeHeader with GIDLength:~p, NonceLength:~p, PubKeyLength:~p, and SigLength:~p",
                                    [ServerGIDLength, ServerNonceLength, ServerECDHEPubKeyLength, ServerSigLength]),
                        xdaa_wait_on_server_key_exchange(TCPSocket, ServerGIDLength, ServerNonceLength, ServerECDHEPubKeyLength, ServerSigLength, ClientNonce);
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

xdaa_wait_on_server_key_exchange(TCPSocket, ServerGIDLength, ServerNonceLength, ServerECDHEPubKeyLength, ServerSigLength, ClientNonce) ->
        case gen_tcp:recv(TCPSocket, ServerGIDLength+ServerNonceLength+ServerECDHEPubKeyLength+ServerSigLength, ?TIMEOUT) of
                {ok, <<ServerGID:ServerGIDLength/binary-unit:8,
                       ServerNonce:ServerNonceLength/binary-unit:8,
                       ServerECDHEPubKeySwapped:ServerECDHEPubKeyLength/binary-unit:8,
                       ServerSig:ServerSigLength/binary-unit:8>>
                } ->
                        lager:debug("Received XDAA ServerKeyExchange with GID:~p, Nonce:~p, SwappedPubKey:~p, and Sig:~p",
                                    [ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig]),
                        xdaa_validate_server_gid(TCPSocket, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce);
                {ok, _} ->
                        lager:warning("XDAA: Received mal-formed ServerKeyExchange", []),
                        {error, "XDAA: Mal-formed ServerKeyExchange message"};
                {error, Reason} ->
                        {error, Reason}
        end.

xdaa_validate_server_gid(TCPSocket, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce) ->
        %% TODO: Verify GID is in known list
        GIDVerify = ok,
        case GIDVerify of
                ok ->
                        lager:debug("Server GID (~p) accepted", [ServerGID]),
                        xdaa_validate_server_signature(TCPSocket, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce);
                {error, Reason} ->
                        lager:info("XDAA: Server GID (~p) rejected", [ServerGID]),
                        {error, Reason}
        end.

xdaa_validate_server_signature(TCPSocket, ServerGID, ServerNonce, ServerECDHEPubKeySwapped, ServerSig, ClientNonce) ->
        SigStruct = <<ServerECDHEPubKeySwapped/binary, ClientNonce/binary>>,
        %% TODO: Read this from file.
        ServerECDSAPubKey = 16#04D676C0F20309A1060A15ADBF20A28C27494908F13F34E3DA1F4973DD19B62DA281BBDEEB0D00354CD0A0E437958DE2BBD32BA70FE8FF805D9C395A1E069CDCEB,
        case crypto:verify(ecdsa,
                           sha256,
                           SigStruct,
                           ServerSig,
                           [ServerECDSAPubKey, secp256r1] ) of
                true ->
                        lager:debug("XDAA: Validation of server signature successful", []),
                        xdaa_send_client_key_exchange(TCPSocket, ServerNonce, ServerECDHEPubKeySwapped);
                false ->
                        lager:warning("XDAA: Validation of server signature failed", []),
                        {error, "XDAA: Validation of server signature failed"}
        end.

xdaa_send_client_key_exchange(TCPSocket, ServerNonce, ServerECDHEPubKeySwapped) ->
        % Generate ECDHE key pair
        #{public := ClientECDHEPubKey, secret := ClientECDHEPrivKey} = enacl:kx_keypair(),
        ClientECDHEPubKeySwapped = reverse_bytes(ClientECDHEPubKey),

        % Generate signature (and its length) from crypto
        SigStruct = <<ClientECDHEPubKeySwapped/binary, ServerNonce/binary>>,
        ClientECDSAPrivKey = 16#43F8F422782842DE799239644A49E359FDBFF81AB604AFD8BE0089B4F3686A0C,
        Signature = crypto:sign(ecdsa,
                                sha256,
                                SigStruct,
                                [ClientECDSAPrivKey, secp256r1]),
        ECDHEPubKeyLength = bit_size(ClientECDHEPubKeySwapped) div 8,
        SigLength = bit_size(Signature) div 8,

        Packet = <<?XDAA_VERSION:8,
                   ECDHEPubKeyLength:16/big,
                   SigLength:16/big,
                   ClientECDHEPubKeySwapped/binary,
                   Signature/binary>>,
        lager:debug("Sending XDAA ClientKeyExchange (~p)...", [Packet]),
        case gen_tcp:send(TCPSocket, Packet) of
                ok ->
                        lager:debug("Sent XDAA ClientKeyExchange (~p) of length ~p octets", [Packet, bit_size(Packet) div 8]),
                        xdaa_run_diffie_hellman(TCPSocket, ServerECDHEPubKeySwapped, ClientECDHEPrivKey);
                {error, Reason} ->
                        {error, Reason}
        end.

xdaa_run_diffie_hellman(TCPSocket, ServerECDHEPubKeySwapped, ClientECDHEPrivKey) ->
        ServerECDHEPubKey = reverse_bytes(ServerECDHEPubKeySwapped),
        DHSharedSecret = enacl:curve25519_scalarmult(ClientECDHEPrivKey, ServerECDHEPubKey),
        lager:debug("Got shared-secret:~p", [DHSharedSecret]),
        {ok, TCPSocket}.

reverse_bytes(Input) ->
        %% Interpret input as little-endian, then re-interpret as big-endian.
        Size = size(Input),
        <<AsLittle:Size/little-unit:8>> = Input,
        <<AsLittle:Size/big-unit:8>>.
