/*
 * A quarantine area for code that is not currently needed
 */

/* main.re */

  /* let request_headers = request |> member("headers"); */
  /*
  let cookies = request |> member("cookies") |> to_list;
  List.iter(item => {
    let cookie_name = item |> member("name") |> to_string;
    let cookie_value = item |> member("value") |> to_string;
    let cookie_http_only = item |> member("httpOnly") |> to_bool;
    let cookie_secure = item |> member("secure") |> to_bool;
    let cookie_expires = switch(item |> member("expires") |> to_string_option) {
    | Some(exp_date) => `Session
    | None => `Session
    };
    ignore(Cookie.Set_cookie_hdr.make(
      ~expiration = cookie_expires,
      ~secure = cookie_secure,
      ~http_only = cookie_http_only,
      (cookie_name, cookie_value)
    ));
  }, cookies);
  */
  /* let status = response |> member("status") |> to_int; */
  /* let response_headers = response |> member("headers"); */
  /* let redirect_url = response |> member("redirect_url"); */
  /* let size = content |> member("size") |> to_int; */


