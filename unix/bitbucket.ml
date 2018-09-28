module Time = struct
  let now = Unix.gettimeofday
  let sleep = Lwt_unix.sleep
end

module Env = struct
  (* let debug = try Unix.getenv "BITBUCKET_DEBUG" <> "0" with _ -> false *)
end

include Bitbucket_core.Make(Time)(Cohttp_lwt_unix.Client)
