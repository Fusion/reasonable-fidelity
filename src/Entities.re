open CfrIO;

let run_modifications_diff = () =>
  switch (
    run_command(
      "for f in diffs/*.txt; do printf \"\\n%s\\n\\n\" \"$f\"; cat \"$f\" | grep '|'; done",
      Some("diffs/modifications.tmp"),
    )
  ) {
  | 0 => true
  | _ => false
  };

/*
 * Store the content of our original response, and the content of our current response
 */
let create_comparison_files = (left, right) => {
  ensure_dir_exists("diffs");
  write_file(
    "diffs/diff.left.tmp",
    Str.global_replace(Str.regexp(","), "\n", left),
  );
  write_file(
    "diffs/diff.right.tmp",
    Str.global_replace(Str.regexp(","), "\n", right),
  );
};

/*
 * Run command-line diff command and store result
 */
let create_diff_file = context => {
  let diff_command =
    switch (context.Web.run_info.diff_command) {
    | Some(command) => command
    | None => "diff -y"
    };
  switch (
    run_command(
      diff_command ++ " diffs/diff.left.tmp diffs/diff.right.tmp",
      Some("diffs/diffed-left-right.txt"),
    )
  ) {
  | 0 => ()
  | _ => ()
  };
};

let test_create_diff_file = context => {
  open Printf;
  open Odiff;

  let diffs = files_diffs("diffs/diff.left.tmp", "diffs/diff.right.tmp");
  let _ = print_endline(sprintf("\n\n%d differences:", List.length(diffs)));
  List.iter(
    item =>
      switch (item) {
      | Add(idx1, idx2, text) =>
        notify(sprintf("Added: %s", text |> encode_string_to_one_line))
      | Delete(idx1, idx2, text) =>
        notify(sprintf("Deleted: %s", text |> encode_string_to_one_line))
      | Change(idx1, text1, idx2, text2) =>
        notify(
          sprintf(
            "Changed:\n    %s\nto:\n    %s",
            text1 |> encode_string_to_one_line,
            text2 |> encode_string_to_one_line,
          ),
        )
      },
    diffs,
  );
  ();
};

/*
 * Return attribute found in current json line
 */
let extracted_attribute_name = index_name =>
  Str.replace_first(Str.regexp({|.*"\(.+\)".*|}), "\\1", index_name);

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
  switch (List.length(left_bits), List.length(right_bits)) {
  | (2, 2) =>
    let left_index_name = extracted_attribute_name(List.hd(left_bits));
    let right_index_name = extracted_attribute_name(List.hd(right_bits));
    if (left_index_name != right_index_name) {
      false;
    } else if
      /* Check if this is an attribute found in the ignore list */
      (Web.StringSet.mem(
         left_index_name,
         context.Web.config_info.ignore_attributes,
       )) {
      true;
    } else if (Plugins.if_any(
                 context.plugin_info,
                 "should_ignore_attribute",
                 [Lymp.Pystr(left_index_name)],
               )) {
      true;
    } else {
      false;
    };
  | (_, 2) => false
  | (2, _) => false
  | (_, _) => false
  };
};

let compare_single_line = (context, line) => {
  let bits = Str.split(Str.regexp(" *\t+ *"), line);
  switch (List.length(bits), bits) {
  | (2, [">", b2]) => false
  | (2, [b1, "<"]) => false
  | (3, [b1, "|", b3]) => compare_attributes(context, b1, b3)
  | (3, [b1, ">", b3]) => false
  | (3, [b1, "<", b3]) => false
  | (2, _) => true
  | (_, _) => false
  };
};

let compare_diff_file_the_naive_way = context => {
  let lines =
    Str.split(Str.regexp("\n"), read_file("diffs/diffed-left-right.txt"));
  List.for_all(compare_single_line(context), lines);
};

let should_ignore_attribute: (Web.action_context, string) => bool
= (context, attribute_name) => {
  /* Check if this is an attribute found in the ignore list */
  if(Web.StringSet.mem(
       attribute_name,
       context.Web.config_info.ignore_attributes)) {
    true
  } else if (Plugins.if_any(
               context.plugin_info,
               "should_ignore_attribute",
               [Lymp.Pystr(attribute_name)],
  )) {
    true
  } else {
    false
  };
};

let smart_compare: (Web.action_context, string, string) => bool
= (context, left, right) => {
  open Yojson.Basic;

  ensure_dir_exists("diffs");

  let rec expander = (item1, item2) => {
    let is_same = (name1, name2, v1, v2) => {
      if(should_ignore_attribute(context, name1)) {
        true
      }
      else {
        (name1 == name2 && v1 == v2)
      }
    };

    switch((item1, item2)) {
    | ((name1, json1), (name2, json2)) => {
        switch ((json1, json2)) {
        | (`Assoc(a1), `Assoc(a2)) =>
            List.fold_left2(
              (accu, query1, query2) => List.append(accu, expander(query1, query2)),
              [],
              a1,
              a2
            )
        | (`List(l1), `List(l2)) => {
            /* TODO "Serialize" similarly to:
             * List.for_all2(List.for_all2(expander), List.map(Util.to_assoc, jl1), List.map(Util.to_assoc, jl2));
             */
            switch(List.fold_left2(
              (accu, item1, item2) => {
                List.append(
                  accu,
                  switch((item1, item2)) {
                  | (`Assoc(a1), `Assoc(a2)) => {
                      switch(List.fold_left2(
                        (accu, query1, query2) => List.append(accu, expander(query1, query2)),
                        [],
                        a1,
                        a2
                      )) {
                      | result => result
                      | exception Invalid_argument(arg) => Printf.printf("WTF"); []
                      | exception _ => Printf.printf("WTF"); []
                      }
                    }
                  | _ => [(false, "[ERROR] Only assoc supported in list for now")]
                  }
                )
              },
              [],
              l1, l2    
            )){
              | result => result
              | exception e => {
                let c1 = Helpers.list_size(l1);
                let c2 = Helpers.list_size(l2);
                if (c1 != c2) {
                  [(false, Printf.sprintf("[ERROR] List sizes differ: %d vs %d (parent: %s)", c1, c2, name1))]
                }
                else {
                  [(false, Printf.sprintf("[ERROR] Unknown error comparing lists (parent: %s): %s", name1, Printexc.to_string(e)))]
                }
              }
            }
        }
        | (`Bool(b1), `Bool(b2))     => [(is_same(name1, name2, b1, b2), Printf.sprintf("%s: %b -> %s: %b\n", name1, b1, name2, b2))]
        | (`Float(f1), `Float(f2))   => [(is_same(name1, name2, f1, f2), Printf.sprintf("%s: %f -> %s: %f", name1, f1, name2, f2))]
        | (`Int(i1), `Int(i2))       => [(is_same(name1, name2, i1, i2), Printf.sprintf("%s: %d -> %s: %d", name1, i1, name2, i2))]
        | (`Null, `Null)             => [(is_same(name1, name2,  0,  0), Printf.sprintf("%s -> %s", name1, name2))]
        | (`String(s1), `String(s2)) => [(is_same(name1, name2, s1, s2), Printf.sprintf("%s: \"%s\" -> %s:\"%s\"", name1, s1, name2, s2))]
        | _                          => [(false, Printf.sprintf("[ERROR] Mismatch: %s -> %s\n", name1, name2))]
        }
      }
    };
  };

  let res = List.fold_left2(
      (accu, query1, query2) => List.append(accu, expander(query1, query2)),
      [],
      left |> from_string |> Util.to_assoc,
      right |> from_string |> Util.to_assoc
  );
  /* Debug
  List.iter(item => { switch(item) { | (b, s) => Printf.printf("%b: %s\n", b, s) } }, res);
  */
  let issue_details = List.map(
    item =>
      switch(item) {
      | (_, detail) => detail
      },
    List.filter(
      row => {
        switch(row) {
        | (false, _) => true
        | _ => false
        }
      },
      res
    )
  );
  /* If at least one row isn't a match, write a diff file to be later
     picked up and saved as evidence.
     This is an uglier contract than needs to be,
     for purely historical reasons, and it will need fixing. */
  switch(issue_details) {
  | [] => true
  | l => {
      write_file_from_list("diffs/diffed-left-right.txt", issue_details);
      false
    }
  }
};

let compare_responses: (Web.action_context, string, string) => bool
= (context, left, right) =>
  if (left == right) {
    true;
  } else {
    if(context.run_info.debug_level > 5) { Printf.printf("compare_responses::start\n");};
    /*
      DEAD CODE
    create_comparison_files(left, right);
    create_diff_file(context);

    compare_diff_file_the_naive_way(context);
    */
    switch(smart_compare(context, left, right)) {
    | response => response
    | exception _ => {
       Printf.printf("Oh an exception!!!");
       false
    }
    }
  };

// vim: syntax=reason
