open Printf;
open Lwt;
open Cohttp;
open Cohttp_lwt_unix;
open CfrIO;

type cmdlineargs =
  | Source_file(string)
  | Config_dir(string)
  | Csv;

let should_ignore_response = (context, url, mime_type) =>
  Web.(
    switch (StringSet.mem(url, context.config_info.ignore_endpoints), mime_type) {
    | (true, _) => true
    | (_, "application/javascript") => true
    | (_, "text/plain") => true
    | (_, "application/x-font-woff") => true
    | (_, "image/png") => true
    | (_, "image/jpg") => true
    | (_, _) => false
    });

let dump_diff = (url) => {
  rename_file(
    "diffs/diffed-left-right.txt",
    "diffs/diffed-" ++ encode_uri_to_path(url) ++ ".txt"
  );
};

let report = (context, url, code, response) => {
  switch(context.Web.use_csv) {
  | true => ()
  | false =>
    switch(code) {
    | 200 => notify(response)
    | 0 => notify(response)
    | code_ when code_< 1 => notify(sprintf("Error type: %s", response))
    | code_ => notify(sprintf("Error code: %d", code))
    }
  };
};

let process_response = (context, url, mime_type, reference_text, text) => {
  if (should_ignore_response(context, url, mime_type)) {
    report(context, url, 0, "Ignoring");
  }
  else {
    switch(Entities.compare_responses(context, reference_text, text)) {
    | false =>
        dump_diff(url);
        report(context, url, -1, "Different response than expected");
    | true =>
        report(context, url, 200, "OK");
    }
  };
};

let execute_action = (action, context) => {
  open Yojson.Basic.Util;
  let request = action |> member("request");
  let method = request |> member("method") |> to_string;
  let url = request |> member("url") |> to_string;
  /* let request_headers = request |> member("headers"); */
  let response = action |> member("response");
  /* let status = response |> member("status") |> to_int; */
  /* let response_headers = response |> member("headers"); */
  /* let redirect_url = response |> member("redirect_url"); */
  let content = response |> member("content");
  /* let size = content |> member("size") |> to_int; */
  let text = content |> member("text") |> to_string;
  let mime_type = content |> member("mimeType") |> to_string;
  switch method {
  | "GET" =>
    let query_string = request |> member("queryString") |> to_list;
    if (! context.Web.use_csv) notify_begin(sprintf("Querying: %s ... ", url));
    Web.(
      switch (execute_get(url, query_string, context)) {
      | Success(response) => process_response(context, url, mime_type, text, response)
      | Failure(code, response) => report(context, url, code, response)
      }
    );
  | "POST" =>
    let post_data = request |> member("postData") |> member("text") |> to_string;
    if (! context.Web.use_csv) notify_begin(sprintf("Posting to: %s ... ", url));
    Web.(
      switch (execute_post(url, post_data, context)) {
      | Success(response) => process_response(context, url, mime_type, text, response)
      | Failure(code, response) => report(context, url, code, response)
      }
    );
  | "PUT" =>
    let post_data = request |> member("postData") |> member("text") |> to_string;
    if (! context.Web.use_csv) notify_begin(sprintf("Putting to: %s ... ", url));
    Web.(
      switch (execute_put(url, post_data, context)) {
      | Success(response) => process_response(context, url, mime_type, text, response)
      | Failure(code, response) => report(context, url, code, response)
      }
    );
  | method_ => notify(sprintf("Unsupported Method: %s", method_))
  };
};

let rec traverse_actions = (list_of_actions, context) => {
  open Web;
  Unix.sleepf(context.run_info.pause);
  switch list_of_actions {
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
      context.run_info.stop_at
    ) {
    | (Not_Executing, Some(start_at), _) when start_at != ts =>
      traverse_actions(tail, context)
    | (Not_Executing, Some(start_at), _) =>
      let execution_info = {
        token: context.execution_info.token,
        executing: Executing
      };
      let new_context =
        build_context(
          ~config_info=context.config_info,
          ~host_info=context.host_info,
          ~run_info=context.run_info,
          ~execution_info,
          ~use_csv=context.use_csv
        );
      execute_action(head, new_context);
      traverse_actions(tail, new_context);
    | (Executing, _, Some(stop_at)) when stop_at == ts =>
      let execution_info = {
        token: context.execution_info.token,
        executing: Not_Executing
      };
      let new_context =
        build_context(
          ~config_info=context.config_info,
          ~host_info=context.host_info,
          ~run_info=context.run_info,
          ~execution_info,
          ~use_csv=context.use_csv
        );
      traverse_actions(tail, new_context);
    | (Executing, _, _) =>
      execute_action(head, context);
      traverse_actions(tail, context);
    | _ =>
      raise(Failure("Unsupported state mix while traversing action list."))
    };
  };
  ();
};

let execute_actions = (json, host_info, run_info, config_info, use_csv) =>
  Yojson.Basic.Util.(
    Web.(
      switch (perform_login(host_info)) {
      | (Success(response), Some(token)) =>
        let will_be_executing =
          switch run_info.start_at {
          | Some(start_at) => Not_Executing
          | None => Executing
          };
        let execution_info = {token, executing: will_be_executing};
        traverse_actions(
          json |> member("log") |> member("entries") |> to_list,
          build_context(~config_info, ~host_info, ~run_info, ~execution_info, ~use_csv)
        );
      | _ => notify("I was not able to log in or retrieve a token.")
      }
    )
  );

let check_requisites = () => {
  List.for_all(util => {
    switch(
      run_command(
        sprintf("which %s", util),
        None)) {
    | 0 => true
    | _ => notify(sprintf("Missing binary. Please install '%s'", util)); false
    }
  },
  ["diff", "grep"])
};

let prepare = () => {
  empty_directory("diffs");
  check_requisites()
};

let cleanup = () => {
  cleanup_directory("diffs");
};

let display_timestamp = (ts, te) => {
  switch(float_of_string(te)) {
  | exception Failure(_) => notify("Duration is not a float value.")
  | duration => notify(sprintf("%s/%f", ts, duration));
  };
};

let display_modifications = () => {
  switch(Entities.run_modifications_diff()) {
  | true => notify(read_file("diffs/modifications.tmp"))
  | false => notify("Error running command")
  }
};

let display_help = () => {
  notify("Valid Arguments:\n");
  notify("help:\n    display help\n");
  notify("run:\n    run tests");
  notify("    options:");
  notify("        --csv: output csv data rather than plain text");
  notify("         -source <file_name>: specify alternate .har file");
  notify("         -config <dir_name>: specify alternate config directory\n");
  notify("modified:\n    show which lines were changed from the reference capture\n");
  notify("timestamp <startedDateTime> <time>:\n    return a timestamp for 'start_at'/'stop_at' configuration\n");
  notify("reset:\n    re-create default configuration file\n");
};

module ArgSet = Set.Make({
  type t = cmdlineargs;
  let compare = Pervasives.compare;
});

exception MissingArgument(string);
exception InvalidArgument(string);

let rec process_args = (args, ret) => {
  switch(args) {
  | [] => ret
  | [head, ...tail] =>
    switch(head) {
    | "--csv" => ArgSet.add(Csv, process_args(tail, ret))
    | "--source" =>
      switch(tail) {
      | [] => raise(MissingArgument("--source"))
      | [arg1, ...tail] => ArgSet.add(Source_file(arg1), process_args(tail, ret))
      }
    | "--config" =>
      switch(tail) {
      | [] => raise(MissingArgument("--config"))
      | [arg1, ...tail] => ArgSet.add(Config_dir(arg1), process_args(tail, ret))
      }
    | arg_ => raise(InvalidArgument(arg_))
    }
  };
};

let () = {
  switch(Sys.argv) {
  | [|_|] => display_help()
  | [|_, "help"|] => display_help()
  | [|_, "timestamp", ts, te|] => display_timestamp(ts, te)
  | [|_, "reset"|] => Config.write_default_config_file()
  | arguments when arguments[1] == "run" =>
    switch(prepare()) {
    | true =>
      let args = process_args(
        Array.to_list(Array.sub(arguments, 2, Array.length(arguments) - 2)),
        ArgSet.empty);
      let arg_use_csv = ref(false);
      let arg_source_file = ref("example.har");
      let arg_dir_path = ref("config");
      ArgSet.iter(
        k => {
          switch(k) {
          | Csv => arg_use_csv := true
          | Source_file(file_name) => arg_source_file := file_name
          | Config_dir(dir_path) => arg_dir_path := dir_path
          }
        },
        args
      );
      let (host_info, run_info, config_info) = Config.read_info(arg_dir_path^);
      execute_actions(
        Web.get_json(arg_source_file^),
        host_info,
        run_info,
        config_info,
        arg_use_csv^
      );
      cleanup();
    | false => ()
    }
  | [|_, "modified"|] => display_modifications()
  | _ => display_help()
  };
};