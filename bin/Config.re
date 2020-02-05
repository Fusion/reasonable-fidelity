open Printf;
open Toml;
open Web;

/* All preserved configuration-related information */
/*
let group = new group;
let _ip = (new string_cp)(
  ~group,
  ["host_info", "ip"],
  "127.0.0.1",
  "Server ip address or host name");
let _user = (new string_cp)(
  ~group,
  ["host_info", "user"],
  "user",
  "User name (or none if no login is required)");
let _password = (new string_cp)(
  ~group,
  ["host_info", "password"],
  "password",
  "Password (or none if no login is required)");
let _debug_level = (new int_cp)(
  ~group,
  ["run_info", "debug_level"],
  0,
  "For more verbose output.");
let _use_token_not_cookies = (new bool_cp)(
  ~group,
  ["run_info", "use_token_not_cookies"],
  false,
  "Set to true for specific cases");
let _start_at = (new option_cp)(
  string_wrappers,
  ~group,
  ["run_info", "start_at"],
  None,
  "Only start test when matching this record timestamp/duration");
let _stop_at = (new option_cp)(
  string_wrappers,
  ~group, ["run_info", "stop_at"],
  None,
  "Stop testing when matching this record timestamp/duration.");
let _pause = (new float_cp)(
  ~group,
  ["run_info", "pause"],
  0.5,
  "Pause for this duration (in seconds) between steps.");
let _timeouts = (new float_cp)(
  ~group,
  ["run_info", "timeouts"],
  5.0,
  "Each step can take up to this time (in seconds) before timeing out.");
let _diff_command = (new option_cp)(
  string_wrappers,
  ~group,
  ["run_info", "diff_command"],
  Some("diff -y"),
  "System command to execute to generate diff bits.");
*/
/* */

/*
let write_default_config_file = () => {
  group#write("config/default.cfg");
};

let read_config_file = (dir_path) => {
  group#read(
    ~on_type_error=
      (groupable_cp, raw_cp, output, filename, in_channel) =>
        {
          CfrIO.notify(
            sprintf(
              "Type error while loading configuration parameter %s from file %s.\n%!",
              String.concat(".", groupable_cp#get_name),
              filename
            )
          );
        },
    sprintf("%s/default.cfg", dir_path));
};
*/

let write_default_config_file = () => {
  ()
}

let read_config_file = (dir_path) => {
  let _ = CfrIO.read_file(sprintf("%s/default.toml", dir_path)) |> Parser.from_string |> Parser.unsafe;
  /* TomlTypes.Table.find(Toml.key("key"), tt); */
}

/*
 * Read an ignore list from a file.
 * Each line contains an ignored entity (URL, attribute...)
 */
let read_ignore_list = (dir_path, name) =>
  List.fold_left((accu, item) =>
    StringSet.add(item, accu),
    StringSet.empty,
    Str.split(Str.regexp("\n"), CfrIO.read_file(sprintf("%s/ignored.%s", dir_path, name)))
  );

/*
 * Read current configuration, either from default
 * configuration files or an alternate directory
 */
let read_info = (dir_path) => {
  let ignore_endpoints = read_ignore_list(dir_path, "endpoints");
  let ignore_attributes = read_ignore_list(dir_path, "attributes");

  read_config_file(dir_path);

  (
    {
      ip: "127.0.0.1",
      user: "user",
      password: "password"
    },
    {
      debug_level: 0,
      use_token_not_cookies: false,
      start_at: None,
      stop_at: None,
      pause: 0.5,
      timeouts: 5.0,
      diff_command: Some("diff -y")
    },
    {
      ignore_endpoints: ignore_endpoints,
      ignore_attributes: ignore_attributes
    }
  )
};

// vim: syntax=reason
