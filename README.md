proof-of-concept project to help me understand a way to have dynamically supervised `gen_event` processes which are addressable by name.

basic idea is to use the `{via, Registry, Name}` variant of `gen_server:start_link/3` so that all the `gen_server` processes are findable via `Registry:registered_names()`/`Registry:whereis_name()`, and then provide wrappers round those in the `gen_server` module. we use a `simple_one_for_one` supervisor, spawning as many `gen_server`s as we want, and once the names are registered via `start_link/3` (as e.g. `{tgs_gs, server1}`), we can get `gen_server` to `cast()` or `call()` to it using the name (`server1`) directly, using `{via, Registry, {tgs_gs, server1}}`, and we don't have to muck about with PIDs.

this way we can do e.g.:

```
1> tgs:start().
module tgs_gs_sup process <0.92.0> init( [] )
=INFO REPORT==== 28-Mar-2016::21:52:52 ===
module tgs_sup process <0.91.0> init( [] )
ok
2> tgs_gs:start_server(s1).
module tgs_gs process <0.95.0> init(s1)
=INFO REPORT==== 28-Mar-2016::21:52:59 ===
tgs_eh adding server s1{ok,<0.95.0>}
3> tgs_gs:start_server(s2).
module tgs_gs process <0.97.0> init(s2){ok,<0.97.0>}
=INFO REPORT==== 28-Mar-2016::21:53:04 ===
tgs_eh adding server s2
4> tgs_gs:get_servers().
[s1,s2]
5> tgs_gs:send_message(s1,"what up s1").
ok
6> 
=INFO REPORT==== 28-Mar-2016::21:53:24 ===
module tgs_gs process <0.95.0> (s1) got message: "what up s1"
6> tgs_gs:send_message(s2,"what up s2"). 
ok
=INFO REPORT==== 28-Mar-2016::21:53:32 ===
module tgs_gs process <0.97.0> (s2) got message: "what up s2"
7> tgs_eh:time_sending_message_via_name_list("hi").
7
=INFO REPORT==== 28-Mar-2016::21:53:46 ===
module tgs_gs process <0.95.0> (s1) got message: "hi"
=INFO REPORT==== 28-Mar-2016::21:53:46 ===
module tgs_gs process <0.97.0> (s2) got message: "hi"
8> tgs_eh:time_sending_message_via_registry("hi").
30
=INFO REPORT==== 28-Mar-2016::21:53:58 ===
module tgs_gs process <0.95.0> (s1) got message: "hi"
9> tgs_gs:stop_server(s1).
ok
10> tgs_gs:stop_server(s1).
{error,{name_not_found,s1}}
```
obvious questions are:

1. why do this?

	- i'd like to make apps which run in erlang but which have a web UI. i don't want to have to do some kind of translation between terms used to represent elements on the client and processes on the server. if i can address processes that can receive messages using the same terms on both sides, it seems like it'll be saving me a fair bit of aggro.
	- i'm trying to build up a completely functional app in erlang which works entirely without UI, which has all the necessary calls to be able to use in the shell during development, and into which plugging a UI is basically a question of adding the comms infrastructure and then just sending messages back and forth. so having the above not only helps in the gluing stage, but also pre-glue, when it's just working up libraries of function calls just in the shell. it's a lot easier if you can just use labels for processes and not have to think about PIDs.

1. is this going to whack the performance?

	- not totally sure. i've done some testing, using the `tgs_eh:time_sending_message_via_*` functions. added 50 servers and ran the following to get the average of 250 sets of sending 10 messages to all 50 servers, getting the `gen_server` labels to send to from either a list stored in the `tgs_eh` event handler state, or the global registry:
	
		```
	1> [ tgs_gs:start_server(
			list_to_atom(lists:flatten(io_lib:format("server~p", [X])))
		) || X <- lists:seq(1,50)
	].
	[...]
	2> Message = "WHAT UP".
	3> [AverageViaNameList, AverageViaRegistry] = [
		lists:sum([
			lists:sum(
				[ tgs_eh:time_sending_message_via_name_list(Message) || X <- lists:seq(1,10) ]
			) / 10
			|| A <- lists:seq(1,250)
		]) / 250,
		lists:sum([
			lists:sum(
				[ tgs_eh:time_sending_message_via_registry(Message) || X <- lists:seq(1,10) ]
			) / 10
			|| A <- lists:seq(1,250)
		]) / 250
	].
	[238.5552000000001,282.3887999999999]
		```
	those times are in microseconds, so on average it was ~44µs slower to do the lookups against the registry than against an in-memory list. for 100 servers the times were `[482.91520000000014,513.2907999999998]`; for 250 servers it was `[1199.0048000000004,1345.8232000000012]`, i.e. ~31µs. according to this, if we have 250 servers, and we average out the times for (((looking up the gen_servers and sending the message to all of them) 10 times) 250 times), then it's ~146µs faster to use the list of `gen_server`s in the `tgs_eh` event handler's state than it is to get the list from the registry. which seems like a decent compromise against all the benefits above.
	
	- of course, i could easily be doing something really stupid with those numbers. would love to know about it if so.
