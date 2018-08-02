open Printf

let user_name = "tmcgilchrist"

let user =
  Lwt_main.run (
    Bitbucket.(Monad.(run (
      User.info user_name () >|= Response.value
    )))
  )

let user_repo =
  Lwt_main.run (
    Bitbucket.(Monad.(run (
      User.repositories user_name () >|= Response.value
    )))
  )

(* let user_repo =
 *   Lwt_main.run (
 *     Bitbucket.(Monad.(run (
 *       let get_repos = User.repositories user_name () in
 *       Stream.next (get_repos) >>= function
 *       | None -> eprintf "no repositories for %s\n" user_name; exit 1
 *       | Some (first_repo,_) ->
 *          eprintf "repository %s\n!" first_repo.uuid;
 *          return ()
 *     )))
 *   ) *)

let _ =
  Printf.printf "current user: %s\n" (Bitbucket_j.string_of_account user);
  Printf.printf "user repos: %s\n" (Bitbucket_j.string_of_paginated_repositories user_repo)
