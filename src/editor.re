open Printf;
open Lwt;
open Cohttp;
open Cohttp_lwt_unix;
open CfrIO;
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
  data: Yojson.Basic.json
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

type screen_elements = {
  screen: window,
  actions_elements: actions_elements
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

let build_screen_elements = (screen, actions_elements) => {
  {
    screen: screen,
    actions_elements: actions_elements
  }
};

/*******************
 * Display helpers *
 *******************/

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

let initialize_colors = () => {
  start_color() |> ignore;
  /* Regular standout */
  init_pair(1, Color.black, Color.white) |> ignore;
  /* Delete */
  init_pair(2, Color.red, 0) |> ignore;
  init_pair(3, Color.red, Color.white);
};

let get_standard_color = () => A.color_pair(0);

let get_highlighted_color = () => A.color_pair(1);

let get_delete_color = () => A.color_pair(2);

let get_highlighted_delete_color = () => A.color_pair(3);

let get_display_attribute = (attribute_name, arg) => {
  attribute_name == arg
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
  doupdate();
};

let rec fill_actions_area = (screen_elements, entries, row) => {
  switch(entries) {
    | [] => ()
    | [head, ...tail] => {
        let (text, action) = get_entry_content(head);
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
  | (true, offset) when offset > rows_count => rows_count
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
    DisplayAttrNone
  );

  (screen_elements, entries_array)
};

let rec main_loop = ((screen_elements, entries_array), rows_count) => {
  refresh_actions_area(screen_elements) |> ignore;

  switch(getch()) {
  | 27 => ()
  | 258 =>
    main_loop(scroll_actions_area(screen_elements, entries_array, 1, rows_count), rows_count)
  | 259 =>
    main_loop(scroll_actions_area(screen_elements, entries_array, -1, rows_count), rows_count)
  | 45 =>
    main_loop(
      update_entry_status(
        screen_elements,
        entries_array,
        screen_elements.actions_elements.offsets.y,
        ActionDelete),
      rows_count)
  | code_ =>
    Printf.printf("%d\n", code_); main_loop((screen_elements, entries_array), rows_count)
  }
};

/********
 * main *
 ********/

let edit_source = (json_content) => {
  let entries_array = json_to_entries(json_content) |> from_json_list_to_info_array;
  let rows_count = Array.length(entries_array);

  let screen = initscr();
  initialize_colors() |> ignore;
  let (screen_height, screen_width) = getmaxyx(screen);
  let cur_screen_info = {
    width: screen_width,
    height: screen_height
  };
  cbreak() |> ignore;
  noecho() |> ignore;
  keypad(screen, true) |> ignore;

  let (actions_wrapper, actions_pad, actions_height, actions_width) = add_actions_area(screen, cur_screen_info, rows_count);
  let cur_screen_elements =
    build_screen_elements(
      screen,
      build_actions_elements(actions_wrapper, actions_pad, actions_height, actions_width, 0, 0)
    );
  draw_borders(screen);

  fill_actions_area(cur_screen_elements, Array.to_list(entries_array), 0);

  main_loop((cur_screen_elements, entries_array), rows_count);

  nocbreak() |> ignore;
  echo() |> ignore;
  endwin();
  ()
};
