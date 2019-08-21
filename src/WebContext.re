open Printf;
open Cohttp_lwt_unix;

/*
 * This web context relies on a dynamic resolver that will memoize host resolutions.
 * This is good to work around wonky resolvers. If we were to use a statid resolver,
 * it could fail now and then, which is too bad since we are actually testing our app,
 * not the network it is built on.
 */
let get_web_context = () => {
  let resolver = {
    let resolver_table = Hashtbl.create(16);
    let blocking_resolver = (service, uri) => {
      let host =
        switch (Uri.host(uri)) {
        | Some(h) => h
        | None => "localhost"
        };
      let port =
        switch (Uri.port(uri)) {
        | Some(p) => p
        | None => service.Resolver.port
        };
      let endpoint_key = sprintf("%s-%d", host, port);
      switch (Hashtbl.find(resolver_table, endpoint_key)) {
      | Some(res) => Lwt.return(res)
      | _ => Lwt.return(`Unknown("name resolution failed"))
      | exception Not_found =>
        switch (
          Unix.getaddrinfo(
            host,
            string_of_int(port),
            [AI_SOCKTYPE(SOCK_STREAM)]
          )
        ) {
        | [] => Lwt.return(`Unknown("name resolution failed"))
        | [{ai_addr: [@implicit_arity] ADDR_INET(addr, port), _}, ..._] =>
          let resolved = `TCP((Ipaddr_unix.of_inet_addr(addr), port));
          Hashtbl.add(resolver_table, endpoint_key, Some(resolved));
          Lwt.return(resolved);
        | [{ai_addr: ADDR_UNIX(file), _}, ..._] =>
          let resolved = `Unix_domain_socket(file);
          Hashtbl.add(resolver_table, endpoint_key, Some(resolved));
          Lwt.return(resolved);
        }
      };
    };
    Resolver_lwt.init(
      ~service=Resolver_lwt_unix.system_service,
      ~rewrites=[("", blocking_resolver)],
      ()
    );
  };
  Client.custom_ctx(~resolver, ());
};

// vim: syntax=reason
