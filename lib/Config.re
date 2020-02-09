open Printf;
open Toml;
open Web;

/* All preserved configuration-related information */

/*
let write_default_config_file = () => {
  group#write("config/default.cfg");
};
*/

/* TODO */
let write_default_config_file = () => {
  ()
}

let read_config_file = (dir_path, ignore_endpoints, ignore_attributes) => {
  let toml_data = CfrIO.read_file(sprintf("%s/default.toml", dir_path)) |> Parser.from_string |> Parser.unsafe;
  let ip = switch(TomlLenses.(get(toml_data, key("host_info") |-- table |-- key("ip") |-- string))) {
  | Some(x) => x
  | None => "127.0.0.1"
  };
  let user = switch(TomlLenses.(get(toml_data, key("host_info") |-- table |-- key("user") |-- string))) {
  | Some(x) => x
  | None => "user"
  };
  let password = switch(TomlLenses.(get(toml_data, key("host_info") |-- table |-- key("password") |-- string))) {
  | Some(x) => x
  | None => "password"
  };
  let debug_level = switch(TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("debug_level") |-- int))) {
  | Some(x) => x
  | None => 0
  };
  let use_token_not_cookies = switch(TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("use_token_not_cookies") |-- bool))) {
  | Some(x) => x
  | None => false
  };
  let start_at = TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("start_at") |-- string));
  let stop_at = TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("stop_at") |-- string));
  let pause = switch(TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("pause") |-- float))) {
  | Some(x) => x
  | None => 0.5
  };
  let timeouts = switch(TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("timeouts") |-- float))) {
  | Some(x) => x
  | None => 5.0
  };
  let diff_command = TomlLenses.(get(toml_data, key("run_info") |-- table |-- key("diff_command") |-- string));

  (
    {
      ip: ip,
      user: user,
      password: password
    },
    {
      debug_level: debug_level,
      use_token_not_cookies: use_token_not_cookies,
      start_at: start_at,
      stop_at: stop_at,
      pause: pause,
      timeouts: timeouts,
      diff_command: diff_command
    },
    {
      ignore_endpoints: ignore_endpoints,
      ignore_attributes: ignore_attributes
    }
  )
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

  read_config_file(dir_path, ignore_endpoints, ignore_attributes); /* Yuck */
};

// vim: syntax=reason
