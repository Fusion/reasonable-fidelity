open Printf;
open Lwt;
open Cohttp;
open Cohttp_lwt_unix;
/*open ExtLib;*/
open CfrIO;

module StringSet = Set.Make({
  type t = string;
  let compare = Stdlib.compare;
});

module StringMap = Map.Make({
  type t = string;
  let compare = Stdlib.compare;
});

type execution_status =
  | Executing
  | Not_Executing;

type http_operation_result =
  | Success(string)
  | Failure(int, string);

type cookies_info = StringMap.t(string);

type host_info = {
  ip: string,
  user: string,
  password: string
};

type run_info = {
  debug_level: int,
  use_token_not_cookies: bool,
  start_at: option(string),
  stop_at: option(string),
  pause: float,
  timeouts: float,
  diff_command: option(string),
  plugins_path: option(string)
};

type execution_info = {
  cookies: cookies_info,
  token: string,
  executing: execution_status
};

type use_csv = bool;

type config_info = {
  ignore_endpoints: StringSet.t,
  ignore_attributes: StringSet.t
};

type plugin_info = Plugins.PluginMap.t(Plugins.plugin_info);

type action_context = {
  config_info,
  plugin_info,
  host_info,
  run_info,
  execution_info,
  use_csv
};

/*
 * Build a new context record based on data retrieved from an existing record,
 * merged with updated values
 */
let build_context = (~config_info, ~plugin_info, ~host_info, ~run_info, ~execution_info, ~use_csv) => {
  config_info,
  plugin_info,
  host_info,
  run_info,
  execution_info,
  use_csv
};

/*
 * Build patched context, where executing state is changed
 */
let build_context_patch_executing: (action_context, execution_status) => action_context
= (context, _) => {
  let execution_info = {
    cookies: context.execution_info.cookies,
    token: context.execution_info.token,
    executing: Executing
  };
  build_context(
    ~config_info=context.config_info,
    ~plugin_info=context.plugin_info,
    ~host_info=context.host_info,
    ~run_info=context.run_info,
    ~execution_info,
    ~use_csv=context.use_csv
  )
};

/*
 * Build patched context, where cookies list is updated
 */
let build_context_patch_cookies: (action_context, cookies_info) => action_context
= (context, cookies) => {
  let execution_info = {
    cookies: cookies,
    token: context.execution_info.token,
    executing: context.execution_info.executing
  };
  build_context(
    ~config_info=context.config_info,
    ~plugin_info=context.plugin_info,
    ~host_info=context.host_info,
    ~run_info=context.run_info,
    ~execution_info,
    ~use_csv=context.use_csv
  )
};

let ctx = WebContext.get_web_context();

let get_json = file_name => Yojson.Basic.from_file(file_name);

let put_json: (string, string) => unit
= (file_name, json_content) => {
  write_file(sprintf("%s.bak", file_name), read_file(file_name));
  write_file(file_name, json_content);
};

/*
 Generic case: we need to memorize and send cookies back
 */
let get_cookies_from_header_string: string => StringMap.t(string)
= header_string => {
  let cookie_matcher = Str.regexp({|set-cookie: \([^;, ]+\)=\([^;, ]*\)|});

  List.fold_left((accu, item) =>
    switch (Str.search_forward(cookie_matcher, item, 0)) {
    | exception Not_found => accu
    | _ => StringMap.add(Str.matched_group(1, item), Str.matched_group(2, item), accu)
    },
    StringMap.empty,
    Str.split(Str.regexp("\n"), header_string)
  )
};

let update_cookies_from_header_string: (action_context, string) => cookies_info
= (context, header_string) => {
  let new_cookies = get_cookies_from_header_string(header_string);
  let old_cookies = context.execution_info.cookies;
  StringMap.merge((_,o1,o2) => {
      switch(o1, o2) {
      | (Some(_), Some(v2)) => Some(v2)
      | (None, Some(v2)) => Some(v2)
      | (Some(v1), None) => Some(v1)
      | (None, None) => None
      }
    },
    old_cookies,
    new_cookies);
};

/*
 If the application being tested sets a cookie containing a session token,
 we will memorize that token and re use it in future requests.
 */
let get_token_from_header_string: string => option(string)
= header_string => {
  let access_token_matcher =
    Str.regexp({|set-cookie: accessToken=\([a-f0-9\-]+\);.+|});
  switch (Str.search_forward(access_token_matcher, header_string, 0)) {
  | exception Not_found => None
  | _ => Some(Str.matched_group(1, header_string))
  };
};

/*
 Based on settings, we will now return either a list of cookies
 as previously retrieved, or a token value
 */
let get_client_auth_headers: action_context => list((string, string))
= (context) => {
  switch(context.run_info.use_token_not_cookies) {
  | true =>
    [("Cookie", "accessToken=" ++ context.execution_info.token)]
  | false =>
    List.map(binding =>
      switch(binding) {
      | (k, v) => ("Cookie", sprintf("%s=%s", k, v))
      },
      StringMap.bindings(context.execution_info.cookies))
  }
};

/*
 * TODO Too many assumptions in this function.
 */
let perform_login: (bool, host_info) => (http_operation_result, cookies_info, option(string))
= (no_login, host_info) => {
  switch(no_login) {
  | true => (Success("No Login"), StringMap.empty, Some(""))
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
          let header_string = resp |> Response.headers |> Header.to_string;
          let cookies = get_cookies_from_header_string(header_string);
          let token = get_token_from_header_string(header_string);
          body |> Cohttp_lwt.Body.to_string >|= (body => (
            switch(code) {
            | 200 => Success(body)
            | code_ => Failure(code_, body)
            }
            , cookies
            , token));
        }
      );
    Lwt_main.run(main_t());
  }
};

let execute_get: (string, list(Yojson.Basic.t), action_context) => (http_operation_result, option(cookies_info))
= (url, query_string, context) => {
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
    Lwt_unix.sleep(context.run_info.timeouts) >|= (() => (Failure(-1, "Timeout"), None));
  let main_t =
    Client.get(
      ~ctx,
      ~headers =
        Header.of_list([
          ("Content-Type", "application/json"),
          ...get_client_auth_headers(context)
        ]),
      Uri.of_string(full_query)
    )
    >>= (
      ((resp, body)) => {
        let header_string = resp |> Response.headers |> Header.to_string;
        let cookies = update_cookies_from_header_string(context, header_string);
        switch (Cohttp_lwt.Response.status(resp)) {
        | `OK =>
          body |> Cohttp_lwt.Body.to_string >|= (body => (Success(body), Some(cookies)))
        | code =>
          body
          |> Cohttp_lwt.Body.to_string
          >|= (body => (Failure(code |> Code.code_of_status, body), Some(cookies)))
        }
      }
    );
  Lwt_main.run(Lwt.pick([timeout, main_t]));
};

let execute_post: (string, string, action_context) => (http_operation_result, option(cookies_info))
= (url, post_data, context) => {
  open Yojson.Basic.Util;
  let timeout =
    Lwt_unix.sleep(context.run_info.timeouts) >|= (() => (Failure(-1, "Timeout"), None));
  let main_t =
    Client.post(
      ~ctx,
      ~headers =
        Header.of_list([
          ("Content-Type", "application/json"),
          ...get_client_auth_headers(context)
        ]),
      ~body = post_data |> Cohttp_lwt.Body.of_string,
      Uri.of_string(url)
    )
    >>= (
      ((resp, body)) => {
        let header_string = resp |> Response.headers |> Header.to_string;
        let cookies = update_cookies_from_header_string(context, header_string);
        switch (Cohttp_lwt.Response.status(resp)) {
        | `OK =>
          body |> Cohttp_lwt.Body.to_string >|= (body => (Success(body), Some(cookies)))
        | code =>
          body
          |> Cohttp_lwt.Body.to_string
          >|= (body => (Failure(code |> Code.code_of_status, body), Some(cookies)))
        }
      }
    );
  Lwt_main.run(Lwt.pick([timeout, main_t]));
};

let execute_put: (string, string, action_context) => (http_operation_result, option(cookies_info))
= (url, post_data, context) => {
  open Yojson.Basic.Util;
  let timeout =
    Lwt_unix.sleep(context.run_info.timeouts) >|= (() => (Failure(-1, "Timeout"), None));
  let main_t =
    Client.put(
      ~ctx,
      ~headers =
        Header.of_list([
          ("Content-Type", "application/json"),
          ...get_client_auth_headers(context)
        ]),
      ~body = post_data |> Cohttp_lwt.Body.of_string,
      Uri.of_string(url)
    )
    >>= (
      ((resp, body)) => {
        let header_string = resp |> Response.headers |> Header.to_string;
        let cookies = update_cookies_from_header_string(context, header_string);
        switch (Cohttp_lwt.Response.status(resp)) {
        | `OK =>
          body |> Cohttp_lwt.Body.to_string >|= (body => (Success(body), Some(cookies)))
        | code =>
          body
          |> Cohttp_lwt.Body.to_string
          >|= (body => (Failure(code |> Code.code_of_status, body), Some(cookies)))
        }
      }
    );
  Lwt_main.run(Lwt.pick([timeout, main_t]));
};

// vim: syntax=reason
