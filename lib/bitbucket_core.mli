(** Portable functor to the Bitbucket API.

    The [Cohttp_lwt.Client] interface can be satisfied by a normal
    [Cohttp_lwt_unix.Client], but also by the JavaScript Cohttp
    client for use in a browser. *)

module Make(Time : Bitbucket_s.Time)(CL : Cohttp_lwt.S.Client) : Bitbucket_s.Bitbucket
