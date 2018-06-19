(*
 * Copyright (c) 2012-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2016 David Sheets <sheets@alum.mit.edu>
 * Copyright (c) 2018 Tim McGilchrist <timmcgil@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(**
   {3 {{:https://developer.atlassian.com/bitbucket/api/2/}Bitbucket APIv2} client library}

   This library offers thin but natural bindings to Bitbucket's developer API.
*)

(** Modules of this type are returned from the {!Bitbucket_core.Make}
    functor which may be applied to Cohttp_lwt client libraries in
    order to run on Unix, in a browser in Javascript, or as a MirageOS
    unikernel. *)
module type Bitbucket = sig



  type rate = Core | Search (**)

  module Token : sig
    type t = string
  end

  (** Functions corresponding to direct API requests return
      {!Response.t} values inside of {!Monad.t} values so that more
      information about the request can be made
      available. {!Monad.(>>~)} is a convenience operator that lets
      you bind directly to the carried value. *)
  module Response : sig
    type redirect =
      | Temporary of Uri.t (** The redirection is temporary. *)
      | Permanent of Uri.t (** The redirection is permanent. *)
    (** [redirect] indicates whether the originally requested
        endpoint should continue to be used in the future. *)

    type 'a t = private < value : 'a; redirects : redirect list; .. >
    (** ['a t] is an API response containing a payload of type
        ['a]. {b Do not} refer to this type explicitly as its identity and
        representation are subject to change (e.g. a family of object
        types may replace it before 3.0). *)

    val value : < value : 'a; .. > -> 'a
    (** [value r] is the payload in response [r]. *)

    val redirects : < redirects : redirect list; .. > -> redirect list
    (** [redirects r] is the sequence of redirects prior to response [r]. *)

    val final_resource : redirect list -> redirect option
    (** [final_resource rs] is the single redirect, if any redirects
        occurred, that describes the overall redirect chain [rs]. If
        any redirect [rs] is temporary, [final_resource rs] will be a
        temporary redirect to the final URI. If all redirects [rs] are
        permanent, [final_resource rs] will be a permanent redirect to
        the final URI. *)
  end



  (** All API requests are bound through this monad which encapsulates
      an Lwt cooperative thread and includes some state which may be
      set via {!API} functions. *)
  module Monad : sig
    type 'a t
    (** ['a t] is an Lwt thread sensitive to GitHub API state. *)

    val return : 'a -> 'a t
    (** [return x] is the value [x] wrapped in a state-sensitive Lwt thread. *)

    val bind : ('a -> 'b t) -> 'a t -> 'b t
    (** [bind m f] is the eventual value of [f] applied to the
        contents of [m]. Its argument order is designed for currying. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f m] is {!bind} [m (fun x -> return (f x))]. Its argument
        order is designed for currying. *)

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** [m >>= f] is [{!bind} f m]. *)

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (** [m >|= f] is [{!map} f m]. *)

    val (>>~) : 'a Response.t t -> ('a -> 'b t) -> 'b t
    (** [m >>~ f] is [m >|= {!Response.value} >>= f]. *)

    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
    (** [catch try with] is the result of trying [try]. If [try]
        succeeds, its result is returned. If [try] raises an
        exception, [with] is applied to the exception and the result
        of [with] is returned. *)

    val fail : exn -> 'a t
    (** [fail exn] raises exception [exn] inside of the monad. *)

    val run : 'a t -> 'a Lwt.t
    (** [run m] is the Lwt thread corresponding to the sequence of API
        actions represented by [m]. Once a {!t} has been [run], any
        GitHub API state such as associated default security tokens or
        declared user agent string will not be available in
        subsequently bound functions. *)

    val embed : 'a Lwt.t -> 'a t
    (** [embed lwt] is an Lwt thread lifted into the GitHub API
        monad. Its monadic state will be inherited from any monadic
        values bound before it. *)
  end

  (** Each request to GitHub is made to a specific [Endpoint] in
      Bitbucket's REST-like API. *)
  module Endpoint : sig
    (** Some endpoints expose resources which change over time and
        responses from those endpoints may contain version metadata
        which can be used to make low-cost conditional requests
        (e.g. cache validation). *)
    module Version : sig
      type t =
        | Etag of string (** An entity tag identifier *)
        | Last_modified of string
          (** A timestamp conforming to the HTTP-date production *)
          (** [t] is a version of a resource representation. *)
    end
  end

  (** The [Stream] module provides an abstraction to Bitbucket's paginated
      endpoints. Stream creation does not initiate any network
      activity. When requests are made, results are buffered
      internally. Streams are not mutable. *)
  module Stream : sig
    type 'a t
    (** ['a t] is a stream consisting roughly of a buffer and a means
        to refill it. *)

    type 'a parse = string -> 'a list Lwt.t
    (** ['a parse] is the type of functions which extract elements
        from a paginated response. *)

    val next : 'a t -> ('a * 'a t) option Monad.t
    (** [next s] is the next element of the stream and a stream
        continuation if one exists. The input stream is not
        modified. This function offers an efficient, lazy, and uniform
        means to iterate over ordered API results which may be too
        numerous to fit into a single request/response pair. *)

    val map : ('a -> 'b list Monad.t) -> 'a t -> 'b t
    (** [map f s] is the lazy stream of [f] applied to elements of [s]
        as they are demanded. *)

    val fold : ('a -> 'b -> 'a Monad.t) -> 'a -> 'b t -> 'a Monad.t
    (** [fold f a s] is the left fold of [f] over the elements of [s]
        with a base value of [a]. {b Warning:} this function may
        result in {i many} successive API transactions. *)

    val find : ('a -> bool) -> 'a t -> ('a * 'a t) option Monad.t
    (** [find p s] is the first value in [s] satisfying [p] if one
        exists and a stream continuation for further ingestion. *)

    val iter : ('a -> unit Monad.t) -> 'a t -> unit Monad.t
    (** [iter f s] is activated after the application of [f] to each
        element of [s]. *)

    val to_list : 'a t -> 'a list Monad.t
    (** [to_list s] is a list with each element of [s]. {b Warning:}
        this function may result in {i many} successive API transactions. *)

    val of_list : 'a list -> 'a t
    (** [of_list l] is a stream with each element of [l].
        Occasionally, it is useful to write interfaces which operate
        generically on streams. [of_list] allows you to use list
        values with those interfaces. *)

    val poll : 'a t -> 'a t option Monad.t
    (** [poll stream] is a stream with items newer than [stream]'s
        items and will not resolve until any timeouts indicated by
        Bitbucket have elapsed. By default, Bitbucket throttles polling
        requests to once per minute per URL endpoint. *)

    val since : 'a t -> Endpoint.Version.t -> 'a t
    (** [since stream version] is [stream] with [version] but without
        any other change, i.e. the stream is not reset to its
        beginning. Used in conjunction with [poll], [since] enables
        low-cost conditional re-synchronization of local state with
        GitHub state. *)

    val version : 'a t -> Endpoint.Version.t option
    (** [version stream] is the version of [stream] if one is
        known. After any stream element is forced, the stream version
        will be available unless GitHub violates its API specification. *)
  end

  type +'a parse = string -> 'a Lwt.t
  (** ['a parse] is the type of functions which extract meaningful
      values from GitHub responses. *)

  type 'a handler = (Cohttp.Response.t * string -> bool) * 'a
  (** ['a handler] is the type of response handlers which consist of
      an activation predicate (fst) and a parse function (snd). *)

  module API : sig
    val code_handler : expected_code:Cohttp.Code.status_code -> 'a -> 'a handler
    (** [code_handler ~expected_code parse] is a response handler that
        fires for responses with status [expected_code] and applies
        [parse]. *)

    val get :
      ?rate:rate ->
      ?fail_handlers:'a parse handler list ->
      ?expected_code:Cohttp.Code.status_code ->
      ?media_type:string ->
      ?headers:Cohttp.Header.t ->
      ?token:Token.t ->
      ?params:(string * string) list ->
      uri:Uri.t ->
      'a parse -> 'a Response.t Monad.t
    (** [get ?rate ?fail_handlers ?expected_code ?headers ?token
        ?params uri p] is the [p]-parsed response to a GitHub API HTTP
        GET request to [uri] with extra query parameters [params] and
        extra headers [headers]. If [token] is supplied, it will be
        used instead of any token bound into the monad. [p] will only
        fire if the response status is [expected_code] which defaults
        to [200 OK]. If the response status is not [expected_code],
        [fail_handlers], if any, will be checked in the order
        supplied. The [rate] parameter determines which rate limit
        accounting regime will be used for caching rate limit values
        in response headers. *)
  end

  (** The [User] module provides basic user information query functions. *)
  module User : sig
    val info : ?token:Token.t ->
      user:string -> unit -> Bitbucket_t.account Response.t Monad.t
    (** [info ~user ()] is the user information for user [user]. *)

    (* val repositories :
     *   ?token:Token.t ->
     *   user:string -> unit -> Bitbucket_t.repository Stream.t *)
    val repositories :
      ?token:Token.t ->
      user:string -> unit -> Bitbucket_t.paginated_repositories Response.t Monad.t
    (** [repositories ~user ()] is a stream of user [user]'s repositories. *)
  end

  module Team : sig
    val info :
      ?token:Token.t ->
      team:string -> unit -> Bitbucket_t.team Response.t Monad.t
    (** [info ~team ()] is a description of team [team]*)

    val repositories :
      ?token:Token.t ->
      team:string -> unit -> Bitbucket_t.paginated_repositories Response.t Monad.t
  end

  (** The [Filter] module contains types used by search and
      enumeration interfaces which describe ways to perform result
      filtering directly in the Bitbucket API. *)
  module Filter : sig
  end

  (** The [Search] module exposes Bitbucket's search interfaces. *)
  module Search : sig
  end
end

(** A module of this type is required in order to construct a
    {!Bitbucket} module using {!Bitbucket_core.Make}. *)
module type Time = sig
  val now : unit -> float
  (** [now ()] is the current UNIX epoch time in seconds. *)

  val sleep : float -> unit Lwt.t
  (** [sleep sec] activates after [sec] seconds have elapsed. *)
end
