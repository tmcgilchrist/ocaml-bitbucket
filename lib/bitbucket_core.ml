let user_agent = "ocaml-bitbucket" (* TODO: add version from build system *)

module Make(Time: Bitbucket_s.Time)(CL : Cohttp_lwt.S.Client) = struct

  type rate = Core | Search

  let string_of_message message = "ERR"

  exception Message of Cohttp.Code.status_code * Bitbucket_t.error

  module Token = struct
    type t = string
  end

  module URI = struct
    let api = "https://api.bitbucket.org/2.0"

    let user ?user () =
      match user with
      | None -> Uri.of_string (Printf.sprintf "%s/user" api)
      | Some u -> Uri.of_string (Printf.sprintf "%s/users/%s" api u)

    let user_repositories ~user () =
      Uri.of_string (Printf.sprintf "%s/users/%s/repositories" api user)


    let team ?team () =
      match team with
      | None -> Uri.of_string (Printf.sprintf "%s/teams" api)
      | Some u -> Uri.of_string (Printf.sprintf "%s/teams/%s" api u)

    let team_repos ~team () =
        Uri.of_string (Printf.sprintf "%s/teams/%s/repositories" api team)
  end

  module Response = struct
    type redirect =
      | Temporary of Uri.t
      | Permanent of Uri.t
    type 'a t = < value : 'a; redirects : redirect list >

    let value r = r#value

    let redirects r = r#redirects

    let rec final_resource = function
      | [] -> None
      | (Permanent uri)::rest -> perm_resource uri rest
      | (Temporary uri)::rest -> temp_resource uri rest
    and perm_resource uri = function
      | [] -> Some (Permanent uri)
      | (Permanent uri)::rest -> perm_resource uri rest
      | (Temporary uri)::rest -> temp_resource uri rest
    and temp_resource uri = function
      | [] -> Some (Temporary uri)
      | (Temporary uri | Permanent uri)::rest -> temp_resource uri rest

    let wrap : ?redirects:redirect list -> 'a -> 'a t =
      fun ?(redirects=[]) v -> object
        method value = v
        method redirects = redirects
      end
  end



  module C = Cohttp
  module CLB = Cohttp_lwt.Body

  module Monad = struct
    open Printf
    open Lwt

    (* Each API call results in either a valid response or
    * an HTTP error. Depending on the error status code, it may
    * be retried within the monad, or a permanent failure returned *)
    type error =
      | Generic of (C.Response.t * string)
      | Semantic of C.Code.status_code * Bitbucket_t.error
      | Bad_response of exn * [ `None | `Json of Yojson.Basic.json | `Raw of string ]
    type request = {
      meth: C.Code.meth; uri: Uri.t;
      headers: C.Header.t; body: string;
    }

    type state = {
      user_agent: string option;
      token: string option
    }
    type 'a signal =
      | Request of request * (request -> 'a signal Lwt.t)
      | Response of 'a
      | Err of error
    type 'a t = state -> (state * 'a signal) Lwt.t

    let error_to_string = function
      | Generic (res, body) ->
        Lwt.return
          (sprintf "HTTP Error %s\nHeaders:\n%s\nBody:\n%s\n"
             (C.Code.string_of_status (C.Response.status res))
             (String.concat "" (C.Header.to_lines (C.Response.headers res)))
             body)
      | Semantic (_,message) ->
        Lwt.return ("Bitbucket API error: " ^ string_of_message message)
      | Bad_response (exn,j) ->
        Lwt.return (sprintf "Bad response: %s\n%s"
          (Printexc.to_string exn)
          (match j with
           |`None -> "<none>"
           |`Raw r -> sprintf "Raw body:\n%s" r
           |`Json j -> sprintf "JSON body:\n%s" (Yojson.Basic.pretty_to_string j)))

    let error err = Err err
    let response r = Response r
    let request ?token ?(params=[]) ({ uri; _ } as req) reqfn =
      let uri = Uri.add_query_params' uri begin match token with
        | None -> params
        | Some token -> ("access_token", token)::params
      end in Request ({req with uri}, reqfn)

    let add_ua hdrs ua =
      let hdrs = C.Header.prepend_user_agent hdrs (user_agent^" "^C.Header.user_agent) in
      match ua with
        | None -> hdrs
        | Some ua -> C.Header.prepend_user_agent hdrs ua

    let prepare_request state ({ headers; uri; _} as req) =
      { req with
        headers=add_ua headers state.user_agent;
        uri=if List.mem_assoc "access_token" (Uri.query req.uri)
            then uri
            else match state.token with
              | Some token -> Uri.add_query_param' uri ("access_token",token)
              | None -> uri
      }

    let rec bind fn x = fun state -> x state >>= function
      | state, Request (req, reqfn) ->
        reqfn (prepare_request state req)
        >>= fun r ->
        bind fn (fun state -> Lwt.return (state, r)) state
      | state, Response r -> fn r state
      | state, ((Err _) as x) -> Lwt.return (state, x)

    let return r = fun state -> Lwt.return (state, Response r)
    let map f m = bind (fun x -> return (f x)) m

    let with_error err = fun state -> Lwt.return (state, Err err)

    let initial_state = {user_agent=None; token=None}

    let run th = bind return th initial_state >>= function
      | _, Request (_,_) -> Lwt.fail (Failure "Impossible: can't run unapplied request")
      | _, Response r -> Lwt.return r
      | _, Err (Semantic (status,msg)) -> Lwt.(fail (Message (status,msg)))
      | _, Err e -> Lwt.(error_to_string e >>= fun err ->
                           Printf.eprintf "%s%!" err; fail (Failure err))

    let (>>=) m f = bind f m
    let (>|=) m f = map f m
    let (>>~) m f = m >|= Response.value >>= f

    let embed lw =
      Lwt.(fun state -> lw >>= (fun v -> return (state, Response v)))

    let fail exn _state = Lwt.fail exn

    let catch try_ with_ state =
      Lwt.catch (fun () -> try_ () state) (fun exn -> with_ exn state)
  end

  module Endpoint = struct
    module Version = struct
      type t = Etag of string | Last_modified of string

      let of_headers headers =
        match C.Header.get headers "etag" with
        | Some etag -> Some (Etag etag)
        | None -> match C.Header.get headers "last-modified" with
          | Some last -> Some (Last_modified last)
          | None -> None

      let add_conditional_headers headers = function
        | None -> headers
        | Some (Etag etag) ->
          C.Header.add headers "If-None-Match" etag
        | Some (Last_modified time) ->
          C.Header.add headers "If-Modified-Since" time

    end

    type t = {
      uri     : Uri.t;
      version : Version.t option;
    }

    let empty = { uri = Uri.empty; version = None; }

    let poll_after : (string, float) Hashtbl.t = Hashtbl.create 8

    let update_poll_after uri { C.Response.headers; _ } =
      let now = Time.now () in
      let poll_limit = match C.Header.get headers "x-poll-interval" with
        | Some interval -> now +. (float_of_string interval)
        | None -> now +. 60.
      in
      let uri_s = Uri.to_string uri in
      let t_0 = try Hashtbl.find poll_after uri_s with Not_found -> 0. in
      if t_0 < poll_limit then Hashtbl.replace poll_after uri_s poll_limit

    let poll_result uri ({ C.Response.headers; _ } as envelope) =
      let version = Version.of_headers headers in
      update_poll_after uri envelope;
      { uri; version; }

    (* TODO: multiple polling threads need to queue *)
    let wait_to_poll uri =
      let now = Time.now () in
      let uri_s = Uri.to_string uri in
      let t_1 = try Hashtbl.find poll_after uri_s with Not_found -> 0. in
      Monad.embed begin
        if now < t_1
        then Time.sleep (t_1 -. now)
        else Lwt.return_unit
      end
  end


  module Stream = struct
    type 'a t = {
      restart  : Endpoint.t -> 'a t option Monad.t;
      buffer   : 'a list;
      refill   : (unit -> 'a t Monad.t) option;
      endpoint : Endpoint.t;
    }
    type 'a parse = string -> 'a list Lwt.t

    let empty = {
      restart = (fun _endpoint -> Monad.return None);
      buffer = []; refill = None;
      endpoint = Endpoint.empty;
    }

    let rec next = Monad.(function
      | { buffer=[]; refill=None; _ } -> return None
      | { buffer=[]; refill=Some refill; _ } -> refill () >>= next
      | { buffer=h::buffer; _ } as s -> return (Some (h, { s with buffer }))
    )

    let map f s =
      let rec refill s () = Monad.(
        next s
        >>= function
        | None -> return empty
        | Some (v,s) ->
          f v
          >>= function
          | [] -> refill s ()
          | buffer ->
            return { s with restart; buffer; refill = Some (refill s) }
      )
      and restart endpoint = Monad.(
        s.restart endpoint
        >>= function
        | Some s -> return (Some {
          s with restart; buffer = []; refill = Some (refill s);
        })
        | None -> return None
      ) in
      {
        s with
        restart;
        buffer = [];
        refill = Some (refill s);
      }

    let rec fold f a s = Monad.(
      next s
      >>= function
      | None -> return a
      | Some (v,s) ->
        f a v
        >>= fun a ->
        fold f a s
    )

    let rec find p s = Monad.(
      next s
      >>= function
      | None -> return None
      | Some (n,s) as c -> if p n then return c else find p s
    )

    let rec iter f s = Monad.(
      next s
      >>= function
      | None -> return ()
      | Some (v,s) -> f v >>= fun () -> iter f s
    )

    let to_list s =
      let rec aux lst s = Monad.(
        next s
        >>= function
        | None -> return (List.rev lst)
        | Some (v,s) -> aux (v::lst) s
      ) in
      aux [] s

    let of_list buffer = { empty with buffer; refill=None; }

    let poll stream = stream.restart stream.endpoint

    let since stream version =
      { stream with endpoint = {
          stream.endpoint with Endpoint.version = Some version;
        };
      }

    let version stream = stream.endpoint.Endpoint.version
  end


  type +'a parse = string -> 'a Lwt.t
  type 'a handler = (Cohttp.Response.t * string -> bool) * 'a


  module API = struct
    (* Use the highest precedence handler that matches the response. *)
    let rec handle_response redirects (envelope,body as response) = Lwt.(
      function
      | (p, handler)::more ->
        if not (p response) then handle_response redirects response more
        else
          let bad_response exn body = return (Monad.(error (Bad_response (exn,body)))) in
          catch (fun () ->
            handler response
            >>= fun r ->
            return (Monad.response (Response.wrap ~redirects r))
          ) (fun exn ->
            catch (fun () ->
              catch (fun () ->
                let json = Yojson.Basic.from_string body in
                (* log "response body:\n%s" (Yojson.Basic.pretty_to_string json); *)
                bad_response exn (`Json json)
              ) (fun _exn -> bad_response exn (`Raw body))
            ) (fun _exn -> bad_response exn `None)
          )
      | [] ->
        let status = C.Response.status envelope in
        match status with
        | `Unprocessable_entity | `Gone | `Unauthorized | `Forbidden ->
          let message = Bitbucket_j.error_of_string body in
          return Monad.(error (Semantic (status,message)))
        | _ ->
          return Monad.(error (Generic (envelope, body)))
    )


    (* Force chunked-encoding
     * to be disabled (to satisfy Github, which returns 411 Length Required
     * to a chunked-encoding POST request). *)
    let lwt_req {Monad.uri; meth; headers; body} =
      let body = CLB.of_string body in
      CL.call ~headers ~body ~chunked:false meth uri

    let max_redirects = 64

    let make_redirect target = function
      | `Moved_permanently -> Response.Permanent target
      | _ -> Response.Temporary target

    let too_many_requests_error =
      let a = Bitbucket_t.{
        message = Printf.sprintf "ocaml-bitbucket exceeded max redirects %d" max_redirects;
        detail = None;
      } in
      Bitbucket_t.{error_type = "error"; error_detail = a}

    let rec request ?(redirects=[]) ~rate ~token resp_handlers req = Lwt.(
      if List.length redirects > max_redirects
      then Lwt.fail (Message (`Too_many_requests, too_many_requests_error))
      else
        lwt_req req
        >>= fun (resp, body) ->
        (* update_rate_table rate ?token resp; *)
        let response_code = C.Response.status resp in
        (* log "Response code %s\n%!" (C.Code.string_of_status response_code); *)
        match response_code with
        | `Found | `Temporary_redirect | `Moved_permanently -> begin
            match C.Header.get (C.Response.headers resp) "location" with
            | None -> Lwt.fail (Message (`Expectation_failed, too_many_requests_error))
            | Some location_s ->
              let location = Uri.of_string location_s in
              let target = Uri.resolve "" req.Monad.uri location in
              let redirect = make_redirect target response_code in
              let redirects = redirect::redirects in
              let req = { req with Monad.uri = target } in
              request ~redirects ~rate ~token resp_handlers req
          end
        | _ ->
          CLB.to_string body >>= fun body ->
          handle_response (List.rev redirects) (resp,body) resp_handlers
    )

    (* A simple response pattern that matches on HTTP code equivalence *)
    let code_handler ~expected_code handler =
      (fun (res,_) -> C.Response.status res = expected_code), handler

    (* Add the correct mime-type header *)
    let realize_headers ?(media_type="") headers =
      C.Header.add_opt headers "accept" media_type

    let idempotent meth
        ?(rate=Core) ?media_type ?headers ?token ?params ~fail_handlers ~expected_code ~uri
        fn =
      fun state -> Lwt.return
        (state,
         (Monad.(request ?token ?params
                   {meth; uri; headers=realize_headers ?media_type headers; body=""})
            (request ~rate ~token
               ((code_handler ~expected_code fn)::fail_handlers))))

    let just_body (_,(body:string)):string Lwt.t = Lwt.return body

    let map_fail_handlers f fhs = List.map (fun (p,fn) ->
      p, f fn;
    ) fhs

    let get ?(rate : rate option)
        ?(fail_handlers=[])
        ?(expected_code=`OK)
        ?(media_type :string option)
        ?(headers : Cohttp.Header.t option)
        ?(token : Token.t option)
        ?(params : (string * string) list option)
        ~(uri : Uri.t)
        (fn : ('a parse)) : 'a Response.t Monad.t =
      let fail_handlers =
        map_fail_handlers Lwt.(fun f x -> just_body x >>= f) fail_handlers
      in
      idempotent `GET ?rate ~fail_handlers ~expected_code
        ?media_type ?headers ?token ?params
        ~uri Lwt.(fun x -> just_body x >>= fn)

    let rec next_link base = Cohttp.Link.(function
    | { context; arc = { Arc.relation; _ }; target }::_
      when Uri.(equal context empty) && List.mem Rel.next relation ->
      Some (Uri.resolve "" base target)
    | _::rest -> next_link base rest
    | [] -> None
    )

    let stream_fail_handlers restart fhs =
      map_fail_handlers Lwt.(fun f (envelope, body) ->
        f body >>= fun buffer ->
        return {
          Stream.restart; buffer; refill=None; endpoint=Endpoint.empty;
        }
      ) fhs

    let rec stream_next restart request uri fn endpoint (envelope, body) = Lwt.(
      let endpoint = match endpoint.Endpoint.version with
        | None -> Endpoint.poll_result uri envelope
        | Some _ -> endpoint
      in
      let refill = Some (fun () ->
        let links = Cohttp.(Header.get_links envelope.Response.headers) in
        match next_link uri links with
        | None -> Monad.return Stream.empty
        | Some uri -> request ~uri (stream_next restart request uri fn endpoint)
      ) in
      fn body >>= fun buffer ->
      return { Stream.restart; buffer; refill; endpoint }
    )

    let rec restart_stream
        ?rate ~fail_handlers ~expected_code ?media_type ?headers ?token
        ?params fn endpoint =
      let restart = restart_stream
          ?rate ~fail_handlers ~expected_code ?headers ?token ?params fn
      in
      let first_request ~uri f =
        let not_mod_handler =
          code_handler ~expected_code:`Not_modified (fun (envelope,_) ->
            Endpoint.update_poll_after uri envelope;
            Lwt.return_none
          )
        in
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        let fail_handlers = map_fail_handlers Lwt.(fun f response ->
          f response >|= fun stream -> Some stream
        ) fail_handlers in
        let fail_handlers = not_mod_handler::fail_handlers in
        let f ((envelope, _) as response) = Lwt.(
          let endpoint = Endpoint.poll_result uri envelope in
          f response
          >|= fun stream ->
          Some { stream with Stream.endpoint }
        ) in
        let headers = match headers with
          | None -> C.Header.init ()
          | Some h -> h
        in
        let headers =
          Endpoint.(Version.add_conditional_headers headers endpoint.version)
        in
        Monad.(
          Endpoint.wait_to_poll uri
          >>= fun () ->
          idempotent ?rate
            `GET ?media_type ~headers ?token ?params ~fail_handlers
            ~expected_code ~uri f
        )
      in
      let request ~uri f =
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        Monad.map Response.value
          (idempotent ?rate
             `GET ?media_type ?headers ?token ?params ~fail_handlers
             ~expected_code ~uri f)
      in
      let uri = endpoint.Endpoint.uri in
      Monad.map Response.value
        (first_request ~uri (stream_next restart request uri fn endpoint))



    let get_stream (type a)
        ?rate
        ?(fail_handlers:a Stream.parse handler list=[])
        ?(expected_code:Cohttp.Code.status_code=`OK)
        ?media_type
        ?(headers:Cohttp.Header.t option) ?(token:string option)
        ?(params:(string * string) list option)
        ~(uri:Uri.t) (fn : a Stream.parse) =
      let restart = restart_stream
          ?rate ~fail_handlers ~expected_code ?headers ?token ?params fn
      in
      let request ~uri f =
        let fail_handlers = stream_fail_handlers restart fail_handlers in
        Monad.map Response.value
          (idempotent ?rate
             `GET ?media_type ?headers ?token ?params ~fail_handlers
             ~expected_code ~uri f)
      in
      let endpoint = Endpoint.({ empty with uri }) in
      let refill = Some (fun () ->
        request ~uri (stream_next restart request uri fn endpoint)
      ) in
      {
        Stream.restart;
        buffer = [];
        refill;
        endpoint;
      }



  end

  module User = struct
    open Lwt

    let info ?token ~user () =
      let uri = URI.user ~user () in
      Printf.printf "Requesting URI: %s\n" (Uri.to_string uri);
      API.get ?token ~uri (fun body -> return (Bitbucket_j.account_of_string body))

    let repositories ?token ~user () =
      let uri = URI.user_repositories ~user () in
      Printf.printf "Requesting URI: %s\n" (Uri.to_string uri);
      API.get ?token ~uri (fun body -> return (Bitbucket_j.paginated_repositories_of_string body))
      (* API.get_stream ?token ~uri (fun b ->
       *   return (Bitbucket_j.stream_repositories_of_string b)
       * ) *)

  end

  module Team = struct
    open Lwt

    let info ?token ~team () =
      let uri = URI.team ~team () in
      API.get ?token ~uri (fun body -> return (Bitbucket_j.team_of_string body))


    let repositories ?token ~team () =
      let uri = URI.team_repos ~team () in
      API.get ?token ~uri (fun body -> return (Bitbucket_j.paginated_repositories_of_string body))
      (* API.get_stream ?token ~uri (fun b ->
       *   return (Bitbucket_j.stream_repositories_of_string b)
       * ) *)
  end

  module Search = struct
  end

  module Filter = struct

  end
end
