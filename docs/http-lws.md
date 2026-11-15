# HTTP Transport Support

CSPP uses dynamically loaded libwebsockets for HTTP. The live integration suite is verified
against LWS 4.5.8. `libcspp.so` has no direct LWS or nghttp2 dependency.

Use `http-client-version-set!` with `http/1.1` or `h2` to require a protocol, and inspect
`http-response-version` for the observed `h1` or `h2` result. Cleartext H2 uses prior knowledge.
`http-open` accepts a caller-owned TLS context, preserving its CA and verification policy.
Keep that context open until the client is closed. Tests cover trusted and untrusted HTTPS,
HTTP/1 ALPN, and H2 ALPN with a local certificate.

HTTP/1 client connections are intentionally not reused. LWS 4.5.8 closes completed client
transactions and offers no supported restart API. Pool idle settings do not imply physical reuse.
Fixed-length, EOF-delimited, and ordinary chunked responses are supported. LWS 4.5.8 rejects
chunk trailers; CSPP propagates that failure and does not parse residual wire bytes itself.

Redirects share one deadline. Cross-origin redirects strip explicit authorization and cookie
headers; target cookies are selected again. A consumed one-shot source cannot be replayed for
307/308. Exceeding ten redirects raises a `redirect-limit` network error. Response sinks receive
only the final response body and finish once, including cancellation and failure.

Servers support GET keep-alive and pipelining, fixed-length request bodies, bounded response
streaming with known lengths, and concurrent H2 requests over TLS. Installed LWS does not decode
chunked request bodies correctly; these requests are rejected. Pipelined POST data that exceeds
its declared length is also rejected. Unknown-length response sources are unsupported.

H2 POST requests require `Content-Length`, including `Content-Length: 0` for empty bodies.
LWS 4.5.8 can assert while draining buffered unknown-length POST DATA, or fail to report body
completion for an empty final DATA frame. CSPP rejects these requests at headers, before
publishing a request to Scheme. The listener remains usable; the rejected stream is closed.

LWS owns H2 SETTINGS and GOAWAY. Its public API exposes neither peer maximum streams
nor GOAWAY last-stream IDs. Live oversubscription of a peer limit of two causes LWS 4.5.8 to
schedule GOAWAY and close the connection; it does not queue excess client requests. The fixture
observes LWS's public log callback for that action and verifies prompt child failure. This proves
the observable shutdown behavior, not delivery of a GOAWAY frame before the TCP connection closes.
Use a local active-stream limit appropriate for the peer. No private H2 parser or synthetic peer
capacity is used.

Blocking helpers suspend when called inside fibers. The live stress suite mixes eight HTTP/1
and H2 operations across eight scheduler lifetimes and checks that waiter usage returns to zero.
Shutdown is synchronous; there is no bounded graceful-shutdown API.

Run the focused tests from the project root after a clean build:

```sh
timeout 300s sh -c 'make clean && make'
timeout 60s make -C tests test \
  net-http.ss net-http-contract.ss net-lws-reactor.ss net-lws-http2.ss net-lws-server.ss net-http-fiber.ss
```

The makefile prints build and suite labels. Successful MAT `.stdout` and `.stderr` files are empty.
Run test commands serially because the harness removes these output files at startup.
