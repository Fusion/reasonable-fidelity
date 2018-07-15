open Printf;
open Lwt;
open Cohttp;
open Cohttp_lwt_unix;
open CfrIO;
open ExtLib;

/*
 * Check whether a server response should not be verified against the
 * canonical, recorded response.
 * Return true if the mime-type is one considered unimportant, or if a plugin
 * instructs us to ignore the response
 */
let should_ignore_response = (context, url, mime_type) =>
  Web.(
    switch (
      StringSet.mem(url, context.config_info.ignore_endpoints),
      mime_type,
    ) {
    | (true, _) => true
    | (_, "application/javascript") => true
    | (_, "text/plain") => true
    | (_, "application/x-font-woff") => true
    | (_, "image/png") => true
    | (_, "image/jpg") => true
    | (_, _) =>
      Plugins.if_any(
        context.plugin_info,
        "should_ignore_response",
        [Lymp.Pystr(url), Lymp.Pystr(mime_type)],
      )
    }
  );

/*
 * The most recent diff output gets an official name
 */
let dump_diff = url =>
  rename_file(
    "diffs/diffed-left-right.txt",
    "diffs/diffed-" ++ encode_uri_to_path(url) ++ ".txt",
  );

/*
 * Build string to display to report success/failure
 * following the appropriate format (CSV, full text)
 */
let build_report_str = (context, url, methd, code, response) =>
  context.Web.use_csv ?
    switch (code) {
    | 200 => sprintf("\"%s\",\"%s\",%d,\"%s\"", url, methd, code, "")
    | 0 => sprintf("\"%s\",\"%s\",%d,\"%s\"", url, methd, code, response)
    | code_ when code_ < 1 =>
      sprintf("\"%s\",\"%s\",%d,\"%s\"", url, methd, code, response)
    | code_ => sprintf("\"%s\",\"%s\",%d,\"%s\"", url, methd, code, "")
    } :
    (
      switch (
        Plugins.if_option(
          context.plugin_info,
          "build_report_str",
          [
            Lymp.Pystr(url),
            Lymp.Pystr(methd),
            Lymp.Pyint(code),
            Lymp.Pystr(response),
          ],
        )
      ) {
      | Some(plugin_response) => plugin_response
      | None =>
        switch (code) {
        | 200 => response
        | 0 => response
        | code_ when code_ < 1 => sprintf("Error type: %s", response)
        | code_ => sprintf("Error code: %d", code)
        }
      }
    );

let process_response = (context, url, methd, mime_type, reference_text, text) =>
  if (should_ignore_response(context, url, mime_type)) {
    notify(build_report_str(context, url, methd, 0, "Ignoring"));
  } else {
    switch (Entities.compare_responses(context, reference_text, text)) {
    | false =>
      dump_diff(url);
      notify(
        build_report_str(
          context,
          url,
          methd,
          -1,
          "Different response than expected",
        ),
      );
    | true => notify(build_report_str(context, url, methd, 200, "OK"))
    };
  };

let execute_action = (action, context) => {
  open Yojson.Basic.Util;
  let request = action |> member("request");
  let method = request |> member("method") |> to_string;
  let url = request |> member("url") |> to_string;

  let response = action |> member("response");
  let content = response |> member("content");
  let text =
    switch (content |> member("size") |> to_int) {
    | 0 => ""
    | _ => content |> member("text") |> to_string
    };
  let mime_type = content |> member("mimeType") |> to_string;
  switch (method) {
  | "GET" =>
    let query_string = request |> member("queryString") |> to_list;
    if (! context.Web.use_csv) {
      notify_begin(sprintf("Querying: %s ... ", url));
    };
    Web.(
      switch (execute_get(url, query_string, context)) {
      | (Success(response), cookies_option) =>
        process_response(context, url, "get", mime_type, text, response);
        switch (cookies_option) {
        | Some(cookies) => build_context_patch_cookies(context, cookies)
        | None => context
        };
      | (Failure(code, response), cookies_option) =>
        notify(build_report_str(context, url, "get", code, response));
        switch (cookies_option) {
        | Some(cookies) => build_context_patch_cookies(context, cookies)
        | None => context
        };
      }
    );
  | "POST" =>
    let post_data =
      request |> member("postData") |> member("text") |> to_string;
    if (! context.Web.use_csv) {
      notify_begin(sprintf("Posting to: %s ... ", url));
    };
    Web.(
      switch (execute_post(url, post_data, context)) {
      | (Success(response), cookies_option) =>
        process_response(context, url, "post", mime_type, text, response);
        switch (cookies_option) {
        | Some(cookies) => build_context_patch_cookies(context, cookies)
        | None => context
        };
      | (Failure(code, response), cookies_option) =>
        notify(build_report_str(context, url, "post", code, response));
        switch (cookies_option) {
        | Some(cookies) => build_context_patch_cookies(context, cookies)
        | None => context
        };
      }
    );
  | "PUT" =>
    let post_data =
      request |> member("postData") |> member("text") |> to_string;
    if (! context.Web.use_csv) {
      notify_begin(sprintf("Putting to: %s ... ", url));
    };
    Web.(
      switch (execute_put(url, post_data, context)) {
      | (Success(response), cookies_option) =>
        process_response(context, url, "put", mime_type, text, response);
        switch (cookies_option) {
        | Some(cookies) => build_context_patch_cookies(context, cookies)
        | None => context
        };
      | (Failure(code, response), cookies_option) =>
        notify(build_report_str(context, url, "put", code, response));
        switch (cookies_option) {
        | Some(cookies) => build_context_patch_cookies(context, cookies)
        | None => context
        };
      }
    );
  | method_ =>
    notify(sprintf("Unsupported Method: %s", method_));
    context;
  };
};

let rec traverse_actions = (list_of_actions, context) => {
  open Web;
  Unix.sleepf(context.run_info.pause);
  switch (list_of_actions) {
  | [] => ()
  | [head, ...tail] =>
    open Yojson.Basic.Util;
    let ts =
      (head |> member("startedDateTime") |> to_string)
      ++ "/"
      ++ (head |> member("time") |> to_float |> string_of_float);
    switch (
      context.execution_info.executing,
      context.run_info.start_at,
      context.run_info.stop_at,
    ) {
    | (Not_Executing, Some(start_at), _) when start_at != ts =>
      traverse_actions(tail, context)
    | (Not_Executing, Some(start_at), _) =>
      let tmp_context = build_context_patch_executing(context, Executing);
      let new_context = execute_action(head, tmp_context);
      traverse_actions(tail, new_context);
    | (Executing, _, Some(stop_at)) when stop_at == ts =>
      let new_context = build_context_patch_executing(context, Not_Executing);
      traverse_actions(tail, new_context);
    | (Executing, _, _) =>
      let new_context = execute_action(head, context);
      traverse_actions(tail, new_context);
    | _ =>
      raise(Failure("Unsupported state mix while traversing action list."))
    };
  };
};

let execute_actions =
    (json, host_info, run_info, config_info, plugin_info, use_csv, no_login) =>
  Yojson.Basic.Util.(
    Web.(
      switch (perform_login(no_login, host_info)) {
      | (Success(response), cookies, Some(token)) =>
        let will_be_executing =
          switch (run_info.start_at) {
          | Some(start_at) => Not_Executing
          | None => Executing
          };
        let execution_info = {cookies, token, executing: will_be_executing};
        traverse_actions(
          json |> member("log") |> member("entries") |> to_list,
          build_context(
            ~config_info,
            ~plugin_info,
            ~host_info,
            ~run_info,
            ~execution_info,
            ~use_csv,
          ),
        );
      | _ => notify("I was not able to log in or retrieve a token.")
      }
    )
  );
