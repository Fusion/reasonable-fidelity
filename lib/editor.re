open Curses;

/***********************************
 * Data structures and conversions *
 ***********************************/

type screen_info = {
  width: int,
  height: int
};

type size = {
  height: int,
  width: int
};

type offsets = {
  y: int,
  x: int
};

type actiontype =
  | ActionDelete
  | ActionEdit
  | ActionNoop;

type item_info = {
  action: actiontype,
  data: Yojson.Basic.t
};

type displayattributes =
  | DisplayAttrHighlighted
  | DisplayAttrNone;

type actions_elements = {
  wrapper: window,
  pad: window,
  size: size,
  offsets: offsets
};

type info_elements = {
  status: window,
  size: size
};

type screen_elements = {
  screen: window,
  actions_elements: actions_elements,
  info_elements: info_elements
};

let json_to_entries = (json_content) => {
  open Yojson.Basic.Util;
  json_content |> member("log") |> member("entries") |> to_list
};

let from_json_list_to_info_array = json_list => {
  Array.of_list(
    List.map(item =>
      {action: ActionNoop, data: item},
      json_list)
  )
};

let get_entry_content = entry => {
  open Yojson.Basic.Util;
  let (request, action) = (entry.data |> member("request"), entry.action);
  let url = request |> member("url") |> to_string;
  (url, action)
};

let build_actions_elements = (wrapper, pad, height, width, offset_y, offset_x) => {
 {
    wrapper: wrapper,
    pad: pad,
    size: {height: height, width: width},
    offsets: {y:offset_y, x:offset_x}
  }
};

let build_info_elements = (status, size) => {
  {
    status: status,
    size: size
  }
};

let build_screen_elements = (screen, actions_elements, info_elements) => {
  {
    screen: screen,
    actions_elements: actions_elements,
    info_elements: info_elements
  }
};

/*******************
 * Display helpers *
 *******************/

let initialize_colors = () => {
  start_color() |> ignore;
  /* Regular standout */
  init_pair(1, Color.black, Color.white) |> ignore;
  /* Delete */
  init_pair(2, Color.red, 0) |> ignore;
  init_pair(3, Color.red, Color.white) |> ignore;
  init_pair(4, Color.black, Color.blue) |> ignore;
};

let get_standard_color = () => A.color_pair(0);

let get_crazy_color = () => A.color_pair(4);

let get_highlighted_color = () => A.color_pair(1);

let get_delete_color = () => A.color_pair(2);

let get_highlighted_delete_color = () => A.color_pair(3);

let get_display_attribute = (attribute_name, arg) => {
  attribute_name == arg
};

let draw_borders = (win) => {
  let acs = get_acs_codes();
  wborder(
    win,
    acs.Acs.vline,
    acs.Acs.vline,
    acs.Acs.hline,
    acs.Acs.hline,
    acs.Acs.ulcorner,
    acs.Acs.urcorner,
    acs.Acs.llcorner,
    acs.Acs.lrcorner
  );
};

let display_actions_area_row = (screen_elements, row, text, action, display_attributes) => {
  let row_color_scheme = switch(get_display_attribute(DisplayAttrHighlighted, display_attributes)) {
  | true => get_highlighted_color()
  | false => get_standard_color()
  };
  let delete_color_scheme = switch(get_display_attribute(DisplayAttrHighlighted, display_attributes)) {
  | true => get_highlighted_delete_color()
  | false => get_delete_color()
  };

  wattron(
    screen_elements.actions_elements.pad,
    row_color_scheme);
  mvwaddstr(
    screen_elements.actions_elements.pad,
    row, 0,
    String.make(screen_elements.actions_elements.size.width, ' ')
  ) |> ignore;
  mvwaddstr(
    screen_elements.actions_elements.pad,
    row, 2,
    text
  ) |> ignore;
  wattroff(
    screen_elements.actions_elements.pad,
    row_color_scheme);

  switch(action) {
  | ActionDelete =>
    wattron(
      screen_elements.actions_elements.pad,
      delete_color_scheme);
    mvwaddstr(
      screen_elements.actions_elements.pad,
      row, 0,
      "-"
    ) |> ignore;
    wattroff(
      screen_elements.actions_elements.pad,
      delete_color_scheme);
  | ActionEdit => ()
  | ActionNoop => ()
  };
};

let add_actions_area = (parent, screen_info, rows_count) => {
  let (height, width) = (screen_info.height - 2, screen_info.width - 2);
  let actions_wrapper = derwin(parent, screen_info.height - 2, screen_info.width - 2, 1, 1);
  let actions_pad = newpad(rows_count, screen_info.width);
  (actions_wrapper, actions_pad, height, width)
};

let refresh_actions_area = (screen_elements) => {
  wnoutrefresh(screen_elements.screen) |> ignore;

  touchwin(screen_elements.actions_elements.wrapper) |> ignore;
  wnoutrefresh(screen_elements.actions_elements.wrapper) |> ignore;
  pnoutrefresh(
    screen_elements.actions_elements.pad,
    screen_elements.actions_elements.offsets.y,
    screen_elements.actions_elements.offsets.x,
    1, 1,
    screen_elements.actions_elements.size.height,
    screen_elements.actions_elements.size.width
  ) |> ignore;

  wnoutrefresh(screen_elements.screen) |> ignore;
  touchwin(screen_elements.info_elements.status) |> ignore;
  wnoutrefresh(screen_elements.info_elements.status) |> ignore;
  doupdate() |> ignore;
};

let rec fill_actions_area = (screen_elements, entries, row) => {
  switch(entries) {
    | [] => ()
    | [head, ...tail] => {
        let (text, _) = get_entry_content(head);
        display_actions_area_row(
          screen_elements,
          row,
          text,
          ActionNoop,
          DisplayAttrNone
        );
        fill_actions_area(screen_elements, tail, row + 1);
      }
  };
};

/* Scrolling */

let new_offset = (cur_offset, direction, rows_count) => {
  switch(direction > 0, cur_offset + direction) {
  | (true, offset) when offset >= rows_count => rows_count - 1
  | (true, offset) => offset
  | (false, offset) when offset < 0 => 0
  | (false, offset) => offset
  }
};

let scroll_actions_area = (screen_elements, entries_array, direction, rows_count) => {
  let updated_screen_elements = 
    build_screen_elements(
      screen_elements.screen,
      build_actions_elements(
        screen_elements.actions_elements.wrapper,
        screen_elements.actions_elements.pad,
        screen_elements.actions_elements.size.height,
        screen_elements.actions_elements.size.width,
        new_offset(screen_elements.actions_elements.offsets.y, direction, rows_count),
        screen_elements.actions_elements.offsets.x
      ),
      build_info_elements(
        screen_elements.info_elements.status,
        screen_elements.info_elements.size,
      )
    );

    let (text, action) = get_entry_content(Array.get(entries_array, screen_elements.actions_elements.offsets.y));
    let (updated_text, updated_action) = get_entry_content(Array.get(entries_array, updated_screen_elements.actions_elements.offsets.y));

    display_actions_area_row(
      screen_elements,
      screen_elements.actions_elements.offsets.y,
      text,
      action,
      DisplayAttrNone
    );
    display_actions_area_row(
      updated_screen_elements,
      updated_screen_elements.actions_elements.offsets.y,
      updated_text,
      updated_action,
      DisplayAttrHighlighted
    );

  (updated_screen_elements, entries_array)
};

/* */

let update_entry_status = (screen_elements, entries_array, row, action_toggle) => {
  /* WARNING! Array side effect */
  let entry = Array.get(entries_array, row);
  let (data, action) = (entry.data, entry.action);
  let new_action = switch(action) {
  | ActionDelete when action_toggle == ActionDelete => ActionNoop
  | ActionNoop => action_toggle
  | _ => action_toggle
  };
  Array.set(
    entries_array,
    row,
    {data: data, action: new_action}
  );
  /* */

  let (updated_text, updated_action) = get_entry_content(
    Array.get(entries_array, row)
  );
  display_actions_area_row(
    screen_elements,
    row,
    updated_text,
    updated_action,
    DisplayAttrHighlighted
  );

  (screen_elements, entries_array)
};

let centered = (text, width) => {
  (width - String.length(text)) / 2
};

let add_info_area = (parent, height, width) => {
  let msg1 = "Reasonable Fidelity Editor v0.0.1";
  let msg2 = "Use 'up' and 'down' arrows to navigate";
  let msg3 = "Hit '-' to mark an action for deletion";
  let msg4 = "Hit 's' to save current state";
  let msg5 = "Hit 'q' to exit editor without saving";

  let info_area = derwin(parent, height - 2, width - 2, 1, 1);
  mvwaddstr(info_area, 0, centered(msg1, width), msg1) |> ignore;
  mvwaddstr(info_area, 2, 1, msg2) |> ignore;
  mvwaddstr(info_area, 3, 1, msg3) |> ignore;
  mvwaddstr(info_area, 4, 1, msg4) |> ignore;
  mvwaddstr(info_area, 5, 1, msg5) |> ignore;

  (info_area)
};

let update_status = (screen_elements, text) => {
  wattron(
    screen_elements.info_elements.status,
    get_crazy_color()) |> ignore;
  mvwaddstr(
    screen_elements.info_elements.status,
    screen_elements.info_elements.size.height - 3,
    0,
    String.make(screen_elements.actions_elements.size.width, ' ')
  ) |> ignore;
  mvwaddstr(
    screen_elements.info_elements.status,
    screen_elements.info_elements.size.height - 3,
    1,
    text) |> ignore;
  wattroff(
    screen_elements.info_elements.status,
    get_crazy_color()) |> ignore;
};

let save_changes = (file_name, screen_elements, entries_array) => {
  /*
         List.filter(name =>
      Str.string_match(plugin_name_matcher, name, 0),
      Array.to_list(Sys.readdir("."))));
      */
   let json_entries = `List(List.map(item =>
      item.data,
      List.filter(item =>
        item.action != ActionDelete,
        Array.to_list(entries_array))));
  let entries = `Assoc([
    ("entries", json_entries)
  ]);
  let top = `Assoc([
    ("log", entries)
  ]);
  Web.put_json(file_name, top |> Yojson.Basic.pretty_to_string);
  update_status(
    screen_elements,
    Printf.sprintf("Written file '%s' and backup.", file_name)
  );

  (screen_elements, entries_array)
};

let rec main_loop = ((screen_elements, entries_array), rows_count, file_name) => {
  refresh_actions_area(screen_elements);

  /*
   * 45:  '-'
   * 113: 'q'
   * 115: 's'
   * 258: down arrow
   * 259: up arrow
   * 339: down arrow
   * 338: up arrow
   */
  switch(getch()) {
  | 113 => ()
  | 115 =>
    update_status(screen_elements, "Saving Changes...");
    main_loop(save_changes(file_name, screen_elements, entries_array), rows_count, file_name)
  | 258 =>
    update_status(screen_elements, "Ready.");
    main_loop(scroll_actions_area(screen_elements, entries_array, 1, rows_count), rows_count, file_name)
  | 338 =>
    update_status(screen_elements, "Ready.");
    main_loop(scroll_actions_area(screen_elements, entries_array, 10, rows_count), rows_count, file_name)
  | 259 =>
    update_status(screen_elements, "Ready.");
    main_loop(scroll_actions_area(screen_elements, entries_array, -1, rows_count), rows_count, file_name)
  | 339 =>
    update_status(screen_elements, "Ready.");
    main_loop(scroll_actions_area(screen_elements, entries_array, -10, rows_count), rows_count, file_name)
  | 45 =>
    update_status(screen_elements, "Toggled deletion flag.");
    main_loop(
      update_entry_status(
        screen_elements,
        entries_array,
        screen_elements.actions_elements.offsets.y,
        ActionDelete),
      rows_count,
      file_name)
  | _ =>
    update_status(screen_elements, "Unknown key.");
    main_loop((screen_elements, entries_array), rows_count, file_name)
  }
};

/********
 * main *
 ********/

let edit_source = (file_name) => {
  let entries_array = Web.get_json(file_name) |> json_to_entries |> from_json_list_to_info_array;
  let rows_count = Array.length(entries_array);

  let info_window_height = 10;

  let screen = initscr();
  initialize_colors() |> ignore;
  let (screen_height, screen_width) = getmaxyx(screen);
  let cur_screen_info = {
    width: screen_width,
    height: screen_height - info_window_height
  };
  cbreak() |> ignore;
  noecho() |> ignore;
  curs_set(0) |> ignore;
  keypad(screen, true) |> ignore;

  let interactive_window = derwin(screen, cur_screen_info.height, cur_screen_info.width , 0, 0);
  draw_borders(interactive_window);
  let (actions_wrapper, actions_pad, actions_height, actions_width) = add_actions_area(interactive_window, cur_screen_info, rows_count);

  let info_window = derwin(screen, info_window_height, cur_screen_info.width , cur_screen_info.height, 0);
  draw_borders(info_window);
  mvwhline(info_window, info_window_height - 3, 1, 0, cur_screen_info.width - 2);
  let info_area = add_info_area(info_window, info_window_height, cur_screen_info.width);

  let cur_screen_elements =
    build_screen_elements(
      screen,
      build_actions_elements(actions_wrapper, actions_pad, actions_height, actions_width, 0, 0),
      build_info_elements(info_area, {height: info_window_height, width: cur_screen_info.width})
    );

  fill_actions_area(cur_screen_elements, Array.to_list(entries_array), 0);

  update_status(cur_screen_elements, "Ready.");

  main_loop(scroll_actions_area(cur_screen_elements, entries_array, 0, rows_count), rows_count, file_name);

  nocbreak() |> ignore;
  echo() |> ignore;
  endwin();
  ()
};

// vim: syntax=reason
