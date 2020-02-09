open Printf;
open ReasonableFidelity;
open ReasonableFidelity.CfrIO;

type cmdlineargs =
  | Source_file(string)
  | Config_dir(string)
  | ForceLogin
  | Csv;

/*
 * The most recent diff output gets an official name
 */
let dump_diff = url =>
  rename_file(
    "diffs/diffed-left-right.txt",
    "diffs/diffed-" ++ encode_uri_to_path(url) ++ ".txt",
  );

let check_requisites = () =>
  List.for_all(
    util =>
      switch (run_command(sprintf("which %s", util), None)) {
      | 0 => true
      | _ =>
        notify(sprintf("Missing binary. Please install '%s'", util));
        false;
      },
    ["diff", "grep"],
  );

let check_plugins = () =>
  Plugins.check_requisites(Plugins.get_interpreter()) ?
    () :
    notify(
      "You may be missing the python bson library: please install pymongo.\nPlugins will be disabled until you do.",
    );

let prepare = () => {
  empty_directory("diffs");
  check_requisites();
};

let cleanup = () => cleanup_directory("diffs");

/*
 * Display a manufactured timestamp, based on a json entry's
 * timestamp and duration.
 */
let display_timestamp = (ts, te) =>
  switch (float_of_string(te)) {
  | exception (Failure(_)) => notify("Duration is not a float value.")
  | duration => notify(sprintf("%s/%f", ts, duration))
  };

let display_modifications = () =>
  Entities.run_modifications_diff() ?
    notify(read_file("diffs/modifications.tmp")) :
    notify("Error running command");

let display_help = () => {
  notify("Valid Arguments:\n");
  notify("help:\n    display help\n");
  notify("sanity:\n    check requirements & help explain issues\n");
  notify("run:\n    run tests");
  notify("    options:");
  notify("        --csv: output csv data rather than plain text");
  notify("        --source <file_name>: specify alternate .har file");
  notify("        --config <dir_name>: specify alternate config directory");
  notify(
    "        --forcelogin: attempt log in to service rather than trust cookies/token (experimental!)\n",
  );
  notify(
    "modified:\n    show which lines were changed from the reference capture\n",
  );
  notify(
    "timestamp <startedDateTime> <time>:\n    return a timestamp for 'start_at'/'stop_at' configuration\n",
  );
  notify(
    "edit <filename>:\n    edit har file -- for now, mark actions for deletion\n",
  );
  notify("reset:\n    re-create default configuration file\n");
};

let display_sanity = () => {
  ignore(check_requisites());
  ignore(check_plugins());
};

module ArgSet =
  Set.Make({
    type t = cmdlineargs;
    let compare = Stdlib.compare;
  });

exception MissingArgument(string);
exception InvalidArgument(string);

/*
 * Return a set of symbolic representations of
 * command line arguments
 */
let rec process_args = (args, ret) =>
  switch (args) {
  | [] => ret
  | [head, ...tail] =>
    switch (head) {
    | "--csv" => ArgSet.add(Csv, process_args(tail, ret))
    | "--forcelogin" => ArgSet.add(ForceLogin, process_args(tail, ret))
    | "--source" =>
      switch (tail) {
      | [] => raise(MissingArgument("--source"))
      | [arg1, ...tail] =>
        ArgSet.add(Source_file(arg1), process_args(tail, ret))
      }
    | "--config" =>
      switch (tail) {
      | [] => raise(MissingArgument("--config"))
      | [arg1, ...tail] =>
        ArgSet.add(Config_dir(arg1), process_args(tail, ret))
      }
    | arg_ => raise(InvalidArgument(arg_))
    }
  };

/*
 * This is not meant for production.
 * Only stuff being tested is to be found here.
 */
let run_test = () => ();

/*
 * Let's get started
 */
let () =
  switch (Sys.argv) {
  | [|_|] => display_help()
  | [|_, "test"|] => run_test()
  | [|_, "help"|] => display_help()
  | [|_, "sanity"|] => display_sanity()
  | [|_, "timestamp", ts, te|] => display_timestamp(ts, te)
  | [|_, "reset"|] => Config.write_default_config_file()
  | arguments when arguments[1] == "run" =>
    prepare() ?
      {
        let args =
          process_args(
            Array.to_list(
              Array.sub(arguments, 2, Array.length(arguments) - 2),
            ),
            ArgSet.empty,
          );
        let arg_use_csv = ref(false);
        let arg_no_login = ref(true);
        let arg_source_file = ref("example.har");
        let arg_dir_path = ref("config");
        ArgSet.iter(
          k =>
            switch (k) {
            | Csv => arg_use_csv := true
            | ForceLogin => arg_no_login := false
            | Source_file(file_name) => arg_source_file := file_name
            | Config_dir(dir_path) => arg_dir_path := dir_path
            },
          args,
        );

        let (host_info, run_info, config_info) =
          Config.read_info(arg_dir_path^);

        let plugin_info = Plugins.load_plugins(run_info.plugins_path);

        Run.execute_actions(
          Web.get_json(arg_source_file^),
          host_info,
          run_info,
          config_info,
          plugin_info,
          arg_use_csv^,
          arg_no_login^,
        );

        cleanup();
      } :
      ()
  | [|_, "modified"|] => display_modifications()
  | [|_, "edit", fn|] => Editor.edit_source(fn)
  | _ => display_help()
  };

// vim: syntax=reason
