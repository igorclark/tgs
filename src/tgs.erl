-module(tgs).
-export( [ start/0, stop/0 ] ).

start()	-> application:start( tgs ).

stop() -> application:stop( tgs ).
