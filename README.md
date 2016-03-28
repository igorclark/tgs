proof-of-concept project to help me understand a way to have dynamically supervised `gen_event` processes which are addressable by name.

basic idea is to use the `{via, Registry, Name}` variant of `gen_server:start_link`, so that all the `gen_server` processes are findable via `Registry:registered_names()`/`Registry:whereis_name()`, and then provide wrappers round those in the `gen_server` module. this way we can do e.g.:

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