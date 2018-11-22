open Printf;
open CfrIO;
open Lymp;
open ExtLib;

exception UnexpectedCapabilityType(pyobj);
exception UnexpectedReturnType;

module CapabilitySet = Set.Make({
  type t = string;
  let compare = Pervasives.compare;
});

type plugin_info = {
  module_ref: Lymp.pycallable,
  capabilities: CapabilitySet.t
};

module PluginMap = Map.Make({
  type t = string;
  let compare = Pervasives.compare;
});

let plugin_name_matcher = Str.regexp({|.+\.py|});
let plugin_name_cleaner = Str.regexp({|\.py|});

/*
 * Retrieve list of plugin modules found in `plugins`
 */
let get_plugins_list: unit => list(string)
= () => {
  open Unix;

  let cur_dir = getcwd();
  chdir("plugins");
  let all_files = List.map(name =>
    Str.replace_first(plugin_name_cleaner, "", name),
    List.filter(name =>
      Str.string_match(plugin_name_matcher, name, 0),
      Array.to_list(Sys.readdir("."))));
  chdir(cur_dir);
  all_files
};

let get_interpreter: unit => string
= () => {
    switch(
      run_command(
        "which python3",
        None)) {
    | 0 => "python3"
    | _ => "python"
    }
};

let check_requisites: string => bool
= interpreter => {
  switch(
    run_command(
      sprintf("%s -c \"import pymongo\"", interpreter),
      None)) {
  | 0 => true
  | _ => false
  }
};

/*
 * Build a reference table for all declared plugins alongside their
 * invocation reference and stated capabilities.
 */
let load_plugins: unit => PluginMap.t(plugin_info)
= () => {
  let interpreter = get_interpreter();
  
  if (!check_requisites(interpreter)) {
    PluginMap.empty
  }
  else {
    let py = init(~exec=interpreter, "plugins");

    List.fold_left((accu, name) => {
      let module_ref = get_module(py, name);
      let capabilities = CapabilitySet.of_list(
        List.map(capability =>
          switch(capability) {
          | Pystr(s) => s
          | invalid_ => raise(UnexpectedCapabilityType(invalid_))
          },
          get_list(module_ref, "get_capabilities", [])));
      PluginMap.add(name, {
        module_ref: module_ref,
        capabilities: capabilities
        }, accu)
      },
      PluginMap.empty,
      get_plugins_list())
  }
};

/*
 * Plugin condition:
 * Return true if any plugin matches the capability
 */
let if_any: (PluginMap.t(plugin_info), string, 'a) => bool
= (plugin_map, capability_name, args) => {
  ! PluginMap.for_all((k, v) => {
    switch(CapabilitySet.mem(capability_name, v.capabilities)) {
    | false => true
    | true => switch(get(v.module_ref, capability_name, args)) {
      | Pybool(b) => !b
      | _ => raise(UnexpectedReturnType)
      }
    }
  },
  plugin_map)
};

/*
 * Plugin condition:
 * TBD
 */
let if_option: (PluginMap.t(plugin_info), string, 'a) => option(string)
= (plugin_map, capability_name, args) => {
/* TODO */
  None
};
