open Printf;
open Lwt;
open Cohttp;
open Cohttp_lwt_unix;

type execution_status =
  | Executing
  | Not_Executing;

type http_operation_result =
  | Success(string)
  | Failure(int, string);

type host_info = {
  ip: string,
  user: string,
  password: string
};

type run_info = {
  start_at: option(string),
  stop_at: option(string),
  pause: float,
  timeouts: float,
  diff_command: option(string)
};

type execution_info = {
  token: string,
  executing: execution_status
};

type use_csv = bool;

module StringSet = Set.Make({
  type t = string;
  let compare = Pervasives.compare;
});

type config_info = {
  ignore_endpoints: StringSet.t,
  ignore_attributes: StringSet.t
};

type action_context = {
  config_info,
  host_info,
  run_info,
  execution_info,
  use_csv
};

let build_context = (~config_info, ~host_info, ~run_info, ~execution_info, ~use_csv) => {
  config_info,
  host_info,
  run_info,
  execution_info,
  use_csv
};

let ctx = WebContext.get_web_context();

/*
 If the application being tested sets a cookie containing a session token,
 we will memorize that token and re use it in future requests.
 */
let get_token_from_headers = headers => {
  let access_token_matcher =
    Str.regexp({|set-cookie: accessToken=\([a-f0-9\-]+\);.+|});
  switch (Str.search_forward(access_token_matcher, headers, 0)) {
  | exception Not_found => None
  | idx => Some(Str.matched_group(1, headers))
  };
};

let get_json = file_name => Yojson.Basic.from_file(file_name);

let perform_login = (no_login, host_info) => {
  switch(no_login) {
  | true => (Success("No Login"), Some(""))
  | false =>
    let login_uri = Uri.of_string("https://" ++ host_info.ip ++ "/api/login");
    let referer_url = "https://" ++ host_info.ip ++ "/api/login.html";
    let main_t = () =>
      Client.post(
        ~ctx,
        ~headers =
          Header.of_list([
            ("Content-Type", "application/json"),
            ("Referer", referer_url)
          ]),
        ~body =
          `Assoc([
            ("userName", `String(host_info.user)),
            ("password", `String(host_info.password))
          ])
          |> Yojson.Basic.pretty_to_string
          |> Cohttp_lwt.Body.of_string,
        login_uri
      )
      >>= (
        ((resp, body)) => {
          let code = resp |> Response.status |> Code.code_of_status;
          let headers = resp |> Response.headers |> Header.to_string;
          let token = get_token_from_headers(headers);
          body |> Cohttp_lwt.Body.to_string >|= (body => (
            switch(code) {
            | 200 => Success(body)
            | code_ => Failure(code_, body)
            }
            , token));
        }
      );
    Lwt_main.run(main_t());
  }
};

let execute_get = (url, query_string, context) => {
  open Yojson.Basic.Util;
  let full_query =
    switch query_string {
    | [] => url
    | [head, ...tail] =>
      List.fold_left(
        (accu, query) =>
          accu
          ++ "&"
          ++ (query |> member("name") |> to_string)
          ++ "="
          ++ (query |> member("value") |> to_string),
        url
        ++ "?"
        ++ (head |> member("name") |> to_string)
        ++ "="
        ++ (head |> member("value") |> to_string),
        tail
      )
    };
  let timeout =
    Lwt_unix.sleep(context.run_info.timeouts) >|= (() => (Failure(-1, "Timeout")));
  let main_t =
    Client.get(
      ~ctx,
      ~headers =
        Header.of_list([
          ("Content-Type", "application/json"),
          ("Cookie", "accessToken=" ++ context.execution_info.token)
        ]),
      Uri.of_string(full_query)
    )
    >>= (
      ((resp, body)) =>
        switch (Cohttp_lwt.Response.status(resp)) {
        | `OK =>
          body |> Cohttp_lwt.Body.to_string >|= (body => Success(body))
        | code =>
          body
          |> Cohttp_lwt.Body.to_string
          >|= (body => (Failure(code |> Code.code_of_status, body)))
        }
    );
  Lwt_main.run(Lwt.pick([timeout, main_t]));
};

let execute_post = (url, post_data, context) => {
  open Yojson.Basic.Util;
  let timeout =
    Lwt_unix.sleep(context.run_info.timeouts) >|= (() => (Failure(-1, "Timeout")));
  let main_t =
    Client.post(
      ~ctx,
      ~headers =
        Header.of_list([
          ("Content-Type", "application/json"),
          ("Cookie", "accessToken=" ++ context.execution_info.token)
        ]),
      ~body = post_data |> Cohttp_lwt.Body.of_string,
      Uri.of_string(url)
    )
    >>= (
      ((resp, body)) =>
        switch (Cohttp_lwt.Response.status(resp)) {
        | `OK =>
          body |> Cohttp_lwt.Body.to_string >|= (body => Success(body))
        | code =>
          body
          |> Cohttp_lwt.Body.to_string
          >|= (body => (Failure(code |> Code.code_of_status, body)))
        }
    );
  Lwt_main.run(Lwt.pick([timeout, main_t]));
};

let execute_put = (url, post_data, context) => {
  open Yojson.Basic.Util;
  let timeout =
    Lwt_unix.sleep(context.run_info.timeouts) >|= (() => (Failure(-1, "Timeout")));
  let main_t =
    Client.put(
      ~ctx,
      ~headers =
        Header.of_list([
          ("Content-Type", "application/json"),
          ("Cookie", "accessToken=" ++ context.execution_info.token)
        ]),
      ~body = post_data |> Cohttp_lwt.Body.of_string,
      Uri.of_string(url)
    )
    >>= (
      ((resp, body)) =>
        switch (Cohttp_lwt.Response.status(resp)) {
        | `OK =>
          body |> Cohttp_lwt.Body.to_string >|= (body => Success(body))
        | code =>
          body
          |> Cohttp_lwt.Body.to_string
          >|= (body => (Failure(code |> Code.code_of_status, body)))
        }
    );
  Lwt_main.run(Lwt.pick([timeout, main_t]));
};
