open Printf;

/*
 * You Are Entering The Land Of Side Effects
 */

let notify = arg => printf("%s\n%!", arg);

let notify_begin = arg => printf("%s%!", arg);

let notify_end = () => printf("\n%!");

let encode_uri_to_path = uri => Uri.pct_encode(uri);

let lf_regex = Str.regexp("\n");

let encode_string_to_one_line: string => string = str =>
  Str.global_replace(lf_regex, "\\n", str);

let read_file: string => string
= name => {
  let ch = open_in(name);
  let content = really_input_string(ch, in_channel_length(ch));
  close_in(ch);
  content;
};

let write_file: (string, string) => unit
= (name, content) => {
  let ch = open_out(name);
  output_string(ch, content);
  close_out(ch);
};

let write_file_from_list: (string, list(string)) => unit
= (name, content) => {
  let ch = open_out(name);
  List.iter(
    item => output_string(ch, Printf.sprintf("%s\n", item)),
    content
  );
  close_out(ch);
};

let rename_file: (string, string) => unit = (old_name, new_name) => Unix.rename(old_name, new_name);

let ensure_dir_exists: string => unit
= dir_name =>
  if (! Sys.file_exists(dir_name)) {
    Unix.mkdir(dir_name, 0o755);
  };

let empty_directory: string => unit
= dir_name => {
  open Unix;
  ensure_dir_exists(dir_name);
  let cur_dir = getcwd();
  chdir(dir_name);
  Array.iter(unlink, Sys.readdir("."));
  chdir(cur_dir);
};

let cleanup_directory: string => unit
= dir_name => {
  open Unix;

  let cur_dir = getcwd();
  chdir(dir_name);
  let dh = opendir(".");
  while (switch (readdir(dh)) {
         | exception End_of_file => false
         | file_name
             when ! Str.string_match(Str.regexp(".+\\.tmp"), file_name, 0) =>
           true
         | file_name =>
           unlink(file_name);
           true;
         }) {
    ();
  };
  closedir(dh);
  chdir(cur_dir);
};

let run_command: (string, option(string)) => int
= (command, out_path) =>
  switch (out_path) {
  | Some(path) => Sys.command(command ++ " > " ++ path)
  | None => Sys.command(command ++ " > /dev/null 2>&1")
  };

// vim: syntax=reason
