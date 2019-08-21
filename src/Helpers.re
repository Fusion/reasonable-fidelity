
let list_size = lst => List.fold_left((accu, item) => accu + 1, 0, lst);

let print_list = lst => List.iter(item => Printf.printf(item), lst);

let dump_json_list = json => {
  open Yojson.Basic;

  let rec f = ((nm, j)) => {
    Printf.printf("\"%s\" -> ", nm);
    switch (j) {
    | `Assoc(a) => {
        Printf.printf("assoc\n");
        List.iter(f, a);
      }
    | `Bool(b) => {
        Printf.printf("bool: %b\n", b)
      }
    | `Float(f) => {
        Printf.printf("float: %f\n", f)
      }
    | `Int(i) => {
        Printf.printf("int: %d\n", i)
      }
    | `List(jl) => {
        Printf.printf("list\n");
        List.iter(List.iter(f), List.map(Util.to_assoc, jl));
      }
    | `Null => {
        Printf.printf("null\n")
      }
    | `String(s) => {
        Printf.printf("string: \"%s\"\n", s)
      }
    };
  };

  List.iter(List.iter(f), List.map(Util.to_assoc, json));
};

// vim: syntax=reason
