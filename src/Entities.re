open CfrIO;

let run_modifications_diff = () => {
  switch(
    run_command(
      "for f in diffs/*.txt; do printf \"\\n%s\\n\\n\" \"$f\"; cat \"$f\" | grep '|'; done",
      Some("diffs/modifications.tmp"))) {
  | 0 => true
  | _ => false
  }
};

/*
 * Store the content of our original response, and the content of our current response
 */
let create_comparison_files = (left, right) => {
  ensure_dir_exists("diffs");
  write_file("diffs/diff.left.tmp", Str.global_replace(Str.regexp(","), "\n", left));
  write_file("diffs/diff.right.tmp", Str.global_replace(Str.regexp(","), "\n", right));
};

/*
 * Run command-line diff command and store result
 */
let create_diff_file = (context) => {
  let diff_command = switch(context.Web.run_info.diff_command) {
  | Some(command) => command
  | None => "diff -y"
  };
  switch(
    run_command(
      diff_command ++ " diffs/diff.left.tmp diffs/diff.right.tmp",
      Some("diffs/diffed-left-right.txt"))) {
  | 0 => ()
  | _ => ()
  }
};

/*
 * Return attribute found in current json line
 */
let extracted_attribute_name = (index_name) => {
  Str.replace_first(
    Str.regexp({|.*"\(.+\)".*|}),
    "\\1",
    index_name
  )
};

/*
 * Compare attribute retrieved from current json line.
 * Return false if we have renamed our attribute.
 * Return true if our attributes are equal, or in the ignore list
 * Return true if a plugin tells us to ignore it
 * Finally, return false if, after these checks, the attribute's values differ
 */
let compare_attributes = (context, left, right) => {
  let left_bits = Str.bounded_split(Str.regexp(":"), left, 2);
  let right_bits = Str.bounded_split(Str.regexp(":"), right, 2);
  switch(List.length(left_bits), List.length(right_bits)) {
  | (2, 2) =>
    let left_index_name = extracted_attribute_name(List.hd(left_bits));
    let right_index_name = extracted_attribute_name(List.hd(right_bits));
    if (left_index_name != right_index_name) {
      false
    }
    else {
      /* Check if this is an attribute found in the ignore list */
      if(Web.StringSet.mem(left_index_name, context.Web.config_info.ignore_attributes)) {
        true
      }
      else if(Plugins.if_any(context.plugin_info, "should_ignore_attribute", [Lymp.Pystr(left_index_name)])) {
        true
      }
      else {
        false
      }
    }
  | (_, 2) => false
  | (2, _) => false
  | (_, _) => false
  }
};

let compare_diff_file = (context) => {
  let lines = Str.split(Str.regexp("\n"), read_file("diffs/diffed-left-right.txt"));
  List.for_all(line => {
    let bits = Str.split(Str.regexp(" *\t+ *"), line);
    switch(List.length(bits), bits) {
    | (2, [">", b2]) => false
    | (2, [b1, "<"]) => false
    | (3, [b1, "|", b3]) => compare_attributes(context, b1, b3)
    | (3, [b1, ">", b3]) => false
    | (3, [b1, "<", b3]) => false
    | (2, _) => true
    | (_, _) => false
    }
  },
  lines)
};

let compare_responses = (context, left, right) => {
  if (left == right) {
    true
  }
  else {
    create_comparison_files(left, right);
    create_diff_file(context);
    compare_diff_file(context);
  };
};