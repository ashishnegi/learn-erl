-module(records).
-compile(export_all).
-include("records.hrl").

new_robot(Name) ->
    #robot{
       name=Name
      }.

crusher() ->
    #robot{
       name="Crusher",
       killing_licence=true
      }.

revoke_kill_licence(Robot) ->
    Robot#robot{
      killing_licence=false
     }.
